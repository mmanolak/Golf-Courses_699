# Purpose: Standalone Julia implementation of the Phase 5 Hawaii micro-case
#          study pipeline. Replicates Steps 1–6 (excluding Step 4) end-to-end
#          in a single script with no calls to the Bulk Tests step scripts.
# Inputs:  Phase 1 Parsing/Data/Julia/Jl_Phase1_Baseline_Golf_Valuation.csv
#          Phase 2 Spatial Polygons and True Acreage/Data/Julia/Jl_Phase2_OSM_Golf_Polygons.gpkg
#          00 - Data Sources/Honolulu/All_Parcels_6378200148342636690.gpkg
#          00 - Data Sources/Honolulu/All_Parcels_-4613852522541990741.csv
#          00 - Data Sources/Honolulu/Zoning_-2205419429161838665.gpkg
#          Phase 3 Economic Merge and MICE Imputation/Data/Julia/Jl_Imputed_Dataset_{1..100}.csv
# Outputs: Data/Julia/Jl_Phase5_Oahu_Comparison.csv
#          Data/Julia/Jl_Phase5_Geographic_Breakdown.csv
#          Data/Julia/Jl_Phase5_Step6_Zoning_Percentages.csv
#          Data/Julia/Jl_Phase5_Step6_Zone_Golf_Penetration.csv
# Note:    Run the R version first to generate the Geopackage File
#
#          for running the script:
#          julia --threads=auto .\Phase_5.jl


# === 1. LIBRARIES ===

# X-08/Decision 1 (2026-07-27): pinned environment, not the machine's global one.
import Pkg
Pkg.activate(normpath(joinpath(@__DIR__, "..")); io = devnull)

using GeoDataFrames
using ArchGDAL
using DataFrames
using CSV
using Statistics
using Printf


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR        = @__DIR__
const PROV_START = time()
include(joinpath(SCRIPT_DIR, "..", "provenance.jl"))

@printf("    Julia threads available: %d\n", Threads.nthreads())
if Threads.nthreads() == 1
    @warn "Running on 1 thread - parallel speedup disabled. " *
            "Relaunch with: julia --threads=auto .\\Phase_5.jl"
end

const WORK_DIR          = normpath(joinpath(@__DIR__, ".."))
const HONOLULU_DATA_DIR = joinpath(WORK_DIR, "00 - Data Sources", "Honolulu")
const OUT_DIR           = joinpath(SCRIPT_DIR, "Data", "Julia")

const PHASE1_IN = joinpath(
    WORK_DIR, "Phase 1 Parsing", "Data", "Julia",
    "Jl_Phase1_Baseline_Golf_Valuation.csv",
)
const OSM_IN = joinpath(
    WORK_DIR, "Phase 2 Spatial Polygons and True Acreage", "Data", "Julia",
    "Jl_Phase2_OSM_Golf_Polygons.gpkg",
)
const PARCELS_IN = joinpath(HONOLULU_DATA_DIR, "All_Parcels_6378200148342636690.gpkg")
const TAX_CSV_IN = joinpath(HONOLULU_DATA_DIR, "All_Parcels_-4613852522541990741.csv")

const PHASE3_DATA_DIR = joinpath(
    WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "Julia",
)
const IMPUTED_PATHS = [
    joinpath(PHASE3_DATA_DIR, "Jl_Imputed_Dataset_$i.csv") for i in 1:100
]
const ZONING_GPKG        = joinpath(HONOLULU_DATA_DIR, "Zoning_-2205419429161838665.gpkg")
# Vendored 2026-07-27 (X-08/Gate-3 policy: no master script performs a network fetch at
# run time). Same TIGER/Line file Phase 1 uses -- source
# https://www2.census.gov/geo/tiger/TIGER2022/COUNTY/tl_2022_us_county.zip
const COUNTY_SHP = joinpath(WORK_DIR, "00 - Data Sources", "Original Data", "tl_2022_us_county.shp")

const COMPARISON_OUT     = joinpath(OUT_DIR, "Jl_Phase5_Oahu_Comparison.csv")
const GEO_BREAKDOWN_OUT  = joinpath(OUT_DIR, "Jl_Phase5_Geographic_Breakdown.csv")
const ZONING_PCT_OUT     = joinpath(OUT_DIR, "Jl_Phase5_Step6_Zoning_Percentages.csv")
const ZONE_PENETRATION_OUT = joinpath(OUT_DIR, "Jl_Phase5_Step6_Zone_Golf_Penetration.csv")

const M2_PER_ACRE = 4046.856422

# Coarse pre-filter only (Step 5, narrows the M=100 national imputed datasets before
# exact polygon matching) -- NOT the Step 1 Oahu-membership test, which now uses the
# real vendored county boundary polygon (see COUNTY_SHP / P5-08).
const OAHU_LON_MIN = -158.5
const OAHU_LON_MAX = -157.6
const OAHU_LAT_MIN =  21.2
const OAHU_LAT_MAX =  21.9

const M = 100

const DISTRICT_MAP = Dict(
    "1" => "Honolulu (Urban Core)",
    "2" => "Honolulu (East/Anomalies)",
    "3" => "Honolulu (Anomalies)",
    "4" => "Koolaupoko (Kailua/Kaneohe)",
    "5" => "Koolauloa (North/East)",
    "6" => "Waialua (North Shore)",
    "7" => "Wahiawa (Central)",
    "8" => "Waianae (West)",
    "9" => "Ewa (Kapolei/Pearl City)",
)


# === 3. FUNCTIONS ===

# [METHODOLOGY] createcoordtrans + transform! - in-place reproject using ArchGDAL.jl API;
# ArchGDAL.reproject(geom, ISpatialRef, ISpatialRef) is not defined in this version.
function reproject_geom(geom, src_crs, tgt_crs)
    ArchGDAL.createcoordtrans(src_crs, tgt_crs) do t
        ArchGDAL.transform!(geom, t)
        geom
    end
end

function find_tmk_column(df::DataFrame)
    for candidate in ["TMK", "PARCEL_ID", "Parcel_ID", "parcel_id",
                       "TAX_MAP_KEY", "Tax_Map_Key", "tax_map_key",
                       "MAPKEY", "mapkey", "tmk"]
        candidate in names(df) && return candidate
    end
    return nothing
end

function find_nearest_polygon(pt_osm, polys_geo::DataFrame)
    min_dist   = Inf
    nearest_id = 0
    for j in 1:nrow(polys_geo)
        # [METHODOLOGY] ArchGDAL.distance - nearest OSM polygon to a course point
        d = ArchGDAL.distance(pt_osm, polys_geo.geometry[j])
        if d < min_dist
            min_dist   = d
            nearest_id = j
        end
    end
    return nearest_id, min_dist
end

function add_row!(rows::Vector, metric::AbstractString, value)
    push!(rows, (Metric = String(metric), Value = string(value)))
end


# === 4. EXECUTION ===

function main()
    println("\n" * "=" ^ 70)
    println("PHASE 5 - HAWAII MICRO-CASE STUDY (STANDALONE)")
    println("=" ^ 70)

    # ── input validation ──────────────────────────────────────────────────────
    for path in [PHASE1_IN, OSM_IN, PARCELS_IN, TAX_CSV_IN]
        isfile(path) || error("[FATAL] Input file not found:\n  $path")
    end
    missing_imp = filter(!isfile, IMPUTED_PATHS)
    isempty(missing_imp) || error("[FATAL] Phase 3 imputed datasets not found:\n  " *
                                   join(missing_imp, "\n  "))
    mkpath(OUT_DIR)


    # ── STEP 1: Data Acquisition ──────────────────────────────────────────────
    println("\n" * "─" ^ 70)
    println("STEP 1 - Data Acquisition")
    println("─" ^ 70)
    println("\nLoading datasets...")

    baseline_df  = CSV.read(PHASE1_IN, DataFrame)
    # [METHODOLOGY] GeoDataFrames.read - spatial read of Phase 2 Julia OSM golf polygons
    osm_golf_geo = GeoDataFrames.read(OSM_IN)
    # [METHODOLOGY] GeoDataFrames.read - spatial read of Honolulu cadastral parcel layer
    parcels_geo  = GeoDataFrames.read(PARCELS_IN)
    # Honolulu cadastral GPKG stores geometry as "SHAPE"; normalize to "geometry"
    "SHAPE" in names(parcels_geo) && rename!(parcels_geo, :SHAPE => :geometry)

    osm_crs     = ArchGDAL.getspatialref(osm_golf_geo.geometry[1])
    # importPROJ4 guarantees traditional lon/lat (x=lon, y=lat) axis order;
    # importEPSG(4326) in GDAL 3.x uses official lat/lon which silently swaps axes.
    wgs84       = ArchGDAL.importPROJ4("+proj=longlat +datum=WGS84 +no_defs")
    parcels_crs = ArchGDAL.getspatialref(parcels_geo.geometry[1])

    println("Reading Oahu boundary (vendored TIGER/Line)...")
    isfile(COUNTY_SHP) || error("Input file not found: $COUNTY_SHP")
    county_geo = GeoDataFrames.read(COUNTY_SHP)
    oahu_rows  = filter(row -> row.STATEFP == "15" && row.NAME == "Honolulu", county_geo)
    nrow(oahu_rows) == 1 || error("[FATAL] Expected exactly 1 Honolulu County row in $COUNTY_SHP, found $(nrow(oahu_rows)).")
    county_crs = ArchGDAL.importPROJ4("+proj=longlat +datum=NAD83 +no_defs")  # TIGER/Line native CRS (EPSG:4269)
    # [METHODOLOGY] reproject_geom - align county boundary to OSM CRS for polygon matching
    oahu_boundary_geom = reproject_geom(oahu_rows.geometry[1], county_crs, osm_crs)

    println("Filtering OSM polygons to Oahu (real polygon intersects, matching R/Python)...")
    # [METHODOLOGY] polygon-vs-polygon intersects against the real Honolulu County boundary -
    # matches R's st_filter(..., .predicate = st_intersects) and Python's .intersects(boundary_union)
    oahu_mask = [ArchGDAL.intersects(g, oahu_boundary_geom) for g in osm_golf_geo.geometry]
    oahu_golf_geo = osm_golf_geo[oahu_mask, :]
    nrow(oahu_golf_geo) > 0 || error("[FATAL] No OSM polygons found on Oahu.")

    oahu_baseline = filter(baseline_df) do row
        (!ismissing(row.County_Name) && row.County_Name == "Honolulu") ||
        (!ismissing(row.FIPS)        && row.FIPS        == 15003)
    end
    n_total     = nrow(oahu_baseline)
    hit_results = fill(false, n_total)

    # [METHODOLOGY] WGS84 → OSM CRS - align Phase 1 lat/lon points to OSM CRS for
    #               point-in-polygon check; mismatch rate quantifies Phase 1-to-Phase 2
    #               representational error
    ArchGDAL.createcoordtrans(wgs84, osm_crs) do t
        for i in 1:n_total
            pt = ArchGDAL.createpoint(oahu_baseline.Longitude[i], oahu_baseline.Latitude[i])
            ArchGDAL.transform!(pt, t)
            hit_results[i] = any(j -> ArchGDAL.intersects(pt, oahu_golf_geo.geometry[j]),
                                 1:nrow(oahu_golf_geo))
        end
    end
    hits = count(hit_results)

    println("  Phase 1 Baseline Total (Points) : $n_total courses")
    println("  Phase 2 OSM Total (Polygons)    : $(nrow(oahu_golf_geo)) courses")
    println("  " * "─" ^ 46)
    println("  Points hitting a polygon        : $hits")
    println("  Points missing a polygon        : $(n_total - hits)")
    @printf("  Direct Point Match Rate         : %.1f%%\n", hits / n_total * 100)

    # Step 2 only needs geometry + tmk; dropping other columns avoids all-Missing
    # columns that GeoDataFrames.write can't convert to OGR field types.
    select!(parcels_geo, [:geometry, :tmk])
    println("\nReprojecting parcels to OSM CRS...")
    # [METHODOLOGY] createcoordtrans + transform! - reproject parcels from native CRS to OSM CRS (EPSG:5070)
    ArchGDAL.createcoordtrans(parcels_crs, osm_crs) do t
        for g in parcels_geo.geometry
            ArchGDAL.transform!(g, t)
        end
    end
    println("[+] Step 1 complete.")


    # ── STEP 2: Parcel Intersection ───────────────────────────────────────────
    println("\n" * "─" ^ 70)
    println("STEP 2 - Parcel Intersection")
    println("─" ^ 70)
    println("  $(nrow(oahu_golf_geo)) golf polygons  ×  $(nrow(parcels_geo)) parcel features")
    println("  Performing spatial intersection (this may take a moment)...")

    result_tmks  = String[]
    result_geoms = ArchGDAL.IGeometry[]

    tmk_col = find_tmk_column(parcels_geo)
    isnothing(tmk_col) && error("[FATAL] No TMK column found in parcel data.")

    # [METHODOLOGY] ArchGDAL.intersection - cookie-cutter of Phase 2 OSM polygons
    #               over the Phase 5 legal cadastre to isolate golf-course parcel fragments
    for i in 1:nrow(oahu_golf_geo)
        g_geom = oahu_golf_geo.geometry[i]
        for j in 1:nrow(parcels_geo)
            p_geom = parcels_geo.geometry[j]
            ArchGDAL.intersects(g_geom, p_geom) || continue
            isect = ArchGDAL.intersection(g_geom, p_geom)
            ArchGDAL.isempty(isect)        && continue
            ArchGDAL.geomarea(isect) ≈ 0.0 && continue
            push!(result_tmks,  string(parcels_geo[j, tmk_col]))
            push!(result_geoms, isect)
        end
    end
    isempty(result_tmks) && error("[FATAL] No parcel fragments identified.")

    unique_tmks   = sort(unique(result_tmks))
    total_area_m2 = sum(ArchGDAL.geomarea(g) for g in result_geoms)
    osm_acres     = total_area_m2 / 4046.86

    println("  Intersection complete: $(length(result_geoms)) fragments, $(length(unique_tmks)) unique TMKs.")
    @printf("  OSM-derived legal footprint: %s acres\n",
            replace(@sprintf("%.2f", osm_acres), r"(?<=\d)(?=(\d{3})+\.)" => ","))
    println("[+] Step 2 complete.")


    # ── STEP 3: Economic Validation ───────────────────────────────────────────
    println("\n" * "─" ^ 70)
    println("STEP 3 - Economic Validation")
    println("─" ^ 70)

    # parcel attribute join against in-memory reprojected cadastre (geometry dropped)
    parcels_attr      = select(parcels_geo, Not(:geometry))
    parcels_attr.tmk  = string.(parcels_attr.tmk)
    tmk_join          = DataFrame(tmk = unique_tmks)
    matched_parcels   = innerjoin(tmk_join, parcels_attr, on = :tmk)
    println("  TMKs from Step 2:    $(length(unique_tmks))")
    println("  Matched in cadastre: $(nrow(matched_parcels))")

    area_candidates     = ["dpp_approved_area_acres", "dpp_stated_area", "rpa_stated_area"]
    area_col            = findfirst(c -> c in names(matched_parcels) &&
                                    any(!ismissing, matched_parcels[!, c]), area_candidates)
    official_area_acres = NaN
    if !isnothing(area_col)
        col = area_candidates[area_col]
        official_area_acres = sum(skipmissing(matched_parcels[!, col]))
        println("  Official area column : $col")
        @printf("  Total official area  : %s acres\n",
                replace(@sprintf("%.2f", official_area_acres), r"(?<=\d)(?=(\d{3})+\.)" => ","))
    end
    @printf("  OSM-derived legal footprint (Step 2): %s acres\n",
            replace(@sprintf("%.2f", osm_acres), r"(?<=\d)(?=(\d{3})+\.)" => ","))

    println("\n  Loading Phase 3 imputations & applying spatial deduplication...")
    oahu_estimates = Vector{DataFrame}(undef, M)
    for i in 1:M
        df_i  = CSV.read(IMPUTED_PATHS[i], DataFrame)
        # [METHODOLOGY] lat/lon bounding box - Oahu extents to pre-filter national dataset
        mask  = .!ismissing.(df_i.Longitude) .& .!ismissing.(df_i.Latitude) .&
                (df_i.Latitude  .>= OAHU_LAT_MIN) .& (df_i.Latitude  .<= OAHU_LAT_MAX) .&
                (df_i.Longitude .>= OAHU_LON_MIN) .& (df_i.Longitude .<= OAHU_LON_MAX)
        df_oahu = df_i[mask, :]
        df_oahu.Total_Opportunity_Cost = df_oahu.osm_acreage .* df_oahu.Baseline_Value_Per_Acre
        df_oahu.imputation = fill(i, nrow(df_oahu))
        oahu_estimates[i]  = df_oahu
        df_i = nothing; GC.gc()
    end
    oahu_all = vcat(oahu_estimates...)
    println("  Oahu courses before dedup (per imputation): $(join(string.(nrow.(oahu_estimates)), ", "))")

    # reuse oahu_golf_geo as the OSM polygon reference; tag with poly_id for dedup
    oahu_golf_geo.poly_id = 1:nrow(oahu_golf_geo)
    unique_courses = combine(groupby(oahu_all, [:Longitude, :Latitude]),
                                :Holes => maximum => :Holes)

    group_ids = Vector{String}(undef, nrow(unique_courses))
    for i in 1:nrow(unique_courses)
        # [METHODOLOGY] ArchGDAL.createpoint - convert course lat/lon to spatial point
        pt_wgs84 = ArchGDAL.createpoint(unique_courses.Longitude[i], unique_courses.Latitude[i])
        # [METHODOLOGY] reproject_geom - align course point to OSM CRS for polygon matching
        pt_osm   = reproject_geom(pt_wgs84, wgs84, osm_crs)
        # [METHODOLOGY] find_nearest_polygon + 500 m cap - mirrors Phase 2 fallback matching
        nearest_id, dist = find_nearest_polygon(pt_osm, oahu_golf_geo)
        group_ids[i] = dist <= 500 ? string(oahu_golf_geo.poly_id[nearest_id]) : "orphan_$i"
    end
    unique_courses.group_id = group_ids
    sort!(unique_courses, [:group_id, :Holes], rev = [false, true])
    master_keep = unique(unique_courses, :group_id)
    select!(master_keep, [:Longitude, :Latitude, :Holes])
    println("  Unique Oahu courses after spatial dedup: $(nrow(master_keep))")

    oahu_deduped_list = Vector{DataFrame}(undef, M)
    for i in 1:M
        df_i = filter(r -> r.imputation == i, oahu_all)
        oahu_deduped_list[i] = innerjoin(df_i, master_keep, on = [:Longitude, :Latitude, :Holes])
    end

    all_deduped = vcat(oahu_deduped_list...)
    agg_spec    = [
        :imputation              => length => :n_imputations,
        :osm_acreage             => mean   => :mean_final_acreage,
        :Baseline_Value_Per_Acre => mean   => :mean_baseline_val,
        :Total_Opportunity_Cost  => mean   => :mean_opportunity_cost,
        :Holes                   => first  => :Holes,
    ]
    "county_type" in names(all_deduped) && push!(agg_spec, :county_type => first => :county_type)
    oahu_per_course = combine(groupby(all_deduped, [:Longitude, :Latitude]), agg_spec...)
    sort!(oahu_per_course, :Longitude)

    # [METHODOLOGY] Rubin's Rules - pooling across M imputations; simplified formula
    #               using total-level aggregates (see Phase 4 for full coefficient pooling)
    oahu_agg_dedup = [sum(d.Total_Opportunity_Cost) for d in oahu_deduped_list]
    q_bar = mean(oahu_agg_dedup)
    v_w   = mean([var(d.Total_Opportunity_Cost) for d in oahu_deduped_list])
    v_b   = var(oahu_agg_dedup)
    v_t   = v_w + v_b + v_b / M
    se    = sqrt(v_t)
    ci_lo = q_bar - 2.576 * se
    ci_hi = q_bar + 2.576 * se

    rows = NamedTuple{(:Metric, :Value), Tuple{String, String}}[]
    add_row!(rows, "Total Golf Courses (Oahu, OSM polygons)",      nrow(oahu_golf_geo))
    add_row!(rows, "Total Unique TMKs (Step 2)",                   replace(@sprintf("%d", length(unique_tmks)), r"(?<=\d)(?=(\d{3})+$)" => ","))
    add_row!(rows, "TMKs Matched in Cadastre",                     replace(@sprintf("%d", nrow(matched_parcels)), r"(?<=\d)(?=(\d{3})+$)" => ","))
    add_row!(rows, "OSM-Derived Legal Footprint (acres)",          @sprintf("%.2f", osm_acres))
    for (i, val) in enumerate(oahu_agg_dedup)
        add_row!(rows, "Oahu Opportunity Cost - Imputation $i (\$B)", @sprintf("%.3f", val / 1e9))
    end
    add_row!(rows, "Pooled Oahu Opportunity Cost - q_bar (\$B)",   @sprintf("%.3f", q_bar / 1e9))
    add_row!(rows, "Standard Error (\$B)",                         @sprintf("%.3f", se    / 1e9))
    add_row!(rows, "95% CI Lower (\$B)",                           @sprintf("%.3f", ci_lo / 1e9))
    add_row!(rows, "95% CI Upper (\$B)",                           @sprintf("%.3f", ci_hi / 1e9))
    !isnan(official_area_acres) &&
        add_row!(rows, "Total Official Area (acres)", @sprintf("%.2f", official_area_acres))

    comparison_df = DataFrame(rows)
    println("\n" * "=" ^ 70)
    println("PHASE 5 ECONOMIC VALIDATION - RESULTS")
    println("=" ^ 70)
    for row in eachrow(comparison_df)
        @printf("  %-55s %s\n", row.Metric, row.Value)
    end
    println("=" ^ 70)

    @printf("\n  Per-Course Summary (%d courses, averaged across %d imputations):\n",
            nrow(oahu_per_course), M)
    @printf("  %-12s %-12s %-10s %-18s %s\n",
            "Latitude", "Longitude", "Holes", "Mean Acreage", "Mean Opp. Cost (\$M)")
    println("  " * "─" ^ 66)
    for row in eachrow(oahu_per_course)
        @printf("  %-12.4f %-12.4f %-10s %-18.1f \$%.2fM\n",
                row.Latitude, row.Longitude,
                string(row.Holes), row.mean_final_acreage,
                row.mean_opportunity_cost / 1e6)
    end

    CSV.write(COMPARISON_OUT, comparison_df)
    println("\n[+] Comparison table saved -> $(basename(COMPARISON_OUT))")
    println("[+] Step 3 complete.")


    # ── STEP 5: Geographic Concentration Breakdown ────────────────────────────
    println("\n" * "─" ^ 70)
    println("STEP 5 - Geographic Concentration Breakdown")
    println("─" ^ 70)

    tax_data    = CSV.read(TAX_CSV_IN, DataFrame)
    tmk_col_idx = findfirst(c -> occursin(r"^tmk$"i, c), names(tax_data))
    isnothing(tmk_col_idx) &&
        error("[FATAL] No TMK column in cadastral CSV. Columns: $(names(tax_data))")
    tmk_col5 = names(tax_data)[tmk_col_idx]

    tmk_clean_step     = replace.(unique_tmks, r"[^0-9]" => "")
    tax_data.TMK_clean = replace.(string.(tax_data[!, tmk_col5]), r"[^0-9]" => "")
    csv_lens  = length.(skipmissing(tax_data.TMK_clean))
    step_lens = length.(tmk_clean_step)

    # 8-digit format = Z S PPP QQQ  (3-digit parcel field)
    # 9-digit format = Z S PPP QQQQ (4-digit parcel field, trailing 0 for non-CPR parcels)
    if all(==(8), step_lens) && all(==(9), csv_lens)
        tmk_clean_step = tmk_clean_step .* "0"
    elseif all(==(9), step_lens) && all(==(8), csv_lens)
        tax_data.TMK_clean = tax_data.TMK_clean .* "0"
    end

    tmk5_df    = DataFrame(TMK_clean = tmk_clean_step)
    geo_merged = innerjoin(tmk5_df, tax_data, on = :TMK_clean; makeunique = true)
    # CPR sub-parcel records share a TMK but have null Zone; drop them so only
    # parent parcel records (which carry zone info) are counted.
    dropmissing!(geo_merged, :Zone)

    geo_merged.Zone_Code     = string.(geo_merged.Zone)
    geo_merged.District_Name = map(z -> get(DISTRICT_MAP, z, "Zone $z"), geo_merged.Zone_Code)

    geo_summary = combine(groupby(geo_merged, [:Zone_Code, :District_Name]),
                          nrow => :Parcel_Count)
    total_parcels = sum(geo_summary.Parcel_Count)
    geo_summary.Pct_of_Total_Parcels = geo_summary.Parcel_Count ./ total_parcels .* 100
    sort!(geo_summary, :Parcel_Count, rev = true)

    @printf("%-5s %-35s %-15s %-15s\n",
            "Zone", "Geographic District", "Parcel Count", "% of Parcels")
    println("─" ^ 70)
    for row in eachrow(geo_summary)
        @printf("%-5s %-35s %-15d %.1f%%\n",
                row.Zone_Code, row.District_Name, row.Parcel_Count, row.Pct_of_Total_Parcels)
    end
    println("─" ^ 70)
    @printf("%-5s %-35s %-15d 100.0%%\n", "", "TOTAL", total_parcels)

    CSV.write(GEO_BREAKDOWN_OUT, geo_summary)
    println("\n[+] Geographic breakdown saved -> $(basename(GEO_BREAKDOWN_OUT))")
    println("[+] Step 5 complete.")


    # ── STEP 6: Zoning Intersection Analysis ──────────────────────────────────
    println("\n" * "─" ^ 70)
    println("STEP 6 - Zoning Intersection Analysis")
    println("─" ^ 70)

    isfile(ZONING_GPKG) || error("[FATAL] Zoning layer not found:\n  $ZONING_GPKG")

    # [METHODOLOGY] GeoDataFrames.read - spatial read of Honolulu zoning layer
    zoning_gdf = GeoDataFrames.read(ZONING_GPKG)
    println("  Loaded zoning layer: $(nrow(zoning_gdf)) features")

    # [METHODOLOGY] Zoning is in EPSG 3760 (ftUS); reprojected to match golf CRS (EPSG 5070,
    #               metres) so ArchGDAL.geomarea() returns m², convertible to acres.
    zoning_crs = ArchGDAL.getspatialref(zoning_gdf.SHAPE[1])
    ArchGDAL.createcoordtrans(zoning_crs, osm_crs) do t
        for g in zoning_gdf.SHAPE
            ArchGDAL.transform!(g, t)
        end
    end
    println("  Reprojection complete.")

    county_zone_acres_z6 = combine(
        groupby(
            DataFrame(
                zone_class       = string.(zoning_gdf.zone_class),
                zone_total_acres = ArchGDAL.geomarea.(zoning_gdf.SHAPE) ./ M2_PER_ACRE,
            ),
            :zone_class
        ),
        :zone_total_acres => sum => :county_total_acres,
    )

    # [METHODOLOGY] ArchGDAL.intersection - clips the zoning polygons to the exact
    #               boundary of each golf course polygon, producing fragment geometries
    #               whose combined area quantifies which zoning classes overlap the
    #               golf course footprint (Pebesma 2018).
    println("  Performing spatial intersection (golf courses ∩ zoning)...")

    frag_zone_class_z6 = String[]
    frag_zone_desc_z6  = String[]
    frag_area_acres_z6 = Float64[]

    for i in 1:nrow(oahu_golf_geo)
        g_geom = oahu_golf_geo.geometry[i]
        for j in 1:nrow(zoning_gdf)
            z_geom = zoning_gdf.SHAPE[j]
            ArchGDAL.intersects(g_geom, z_geom) || continue
            isect   = ArchGDAL.intersection(g_geom, z_geom)
            ArchGDAL.isempty(isect) && continue
            area_m2 = ArchGDAL.geomarea(isect)
            area_m2 ≈ 0.0 && continue
            push!(frag_zone_class_z6, string(zoning_gdf.zone_class[j]))
            push!(frag_zone_desc_z6,  string(coalesce(zoning_gdf.zoning_description[j], "")))
            push!(frag_area_acres_z6, area_m2 / M2_PER_ACRE)
        end
    end

    println("  Intersection produced $(length(frag_area_acres_z6)) fragments.")
    total_golf_acres_z6 = sum(frag_area_acres_z6)
    @printf("  Total intersected golf footprint: %.1f acres\n", total_golf_acres_z6)

    frag_df_z6 = DataFrame(
        zone_class         = frag_zone_class_z6,
        zoning_description = frag_zone_desc_z6,
        area_acres         = frag_area_acres_z6,
    )

    zone_summary_z6 = combine(
        groupby(frag_df_z6, [:zone_class, :zoning_description]),
        :area_acres => sum    => :acres,
        :area_acres => length => :fragments,
    )
    zone_summary_z6.pct_of_total = zone_summary_z6.acres ./ total_golf_acres_z6 .* 100
    sort!(zone_summary_z6, :acres, rev = true)

    zone_penetration_z6 = leftjoin(
        rename(zone_summary_z6[:, [:zone_class, :zoning_description, :acres]], :acres => :golf_acres),
        county_zone_acres_z6,
        on = :zone_class,
    )
    zone_penetration_z6.pct_zone_as_golf = (
        zone_penetration_z6.golf_acres ./ zone_penetration_z6.county_total_acres .* 100
    )
    sort!(zone_penetration_z6, :pct_zone_as_golf, rev = true)

    CSV.write(ZONING_PCT_OUT, zone_summary_z6)
    println("\n[+] Zoning percentages saved -> $(basename(ZONING_PCT_OUT))")
    CSV.write(ZONE_PENETRATION_OUT, zone_penetration_z6)
    println("[+] Zone penetration saved   -> $(basename(ZONE_PENETRATION_OUT))")
    println("[+] Step 6 complete.")


    # ── DONE ──────────────────────────────────────────────────────────────────
    println("\n" * "=" ^ 70)
    println("PHASE 5 COMPLETE")
    println("  Outputs written to: $OUT_DIR")
    println("=" ^ 70)
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
    record_provenance("Phase 5", "Phase_5.jl", SCRIPT_DIR, PROV_START)
end
