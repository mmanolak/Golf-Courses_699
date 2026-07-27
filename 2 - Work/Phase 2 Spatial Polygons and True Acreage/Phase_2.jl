# Purpose: Phase 2 master - OSM polygon parse + spatial matching.
#          Step 1 reads the pyosmium GeoPackage, recalculates acreage in
#          parallel via ArchGDAL, filters by plausibility bounds, and saves
#          a canonical Julia GPKG.
#          Step 2 matches those polygons to Phase 1 baseline points via
#          spatial intersect + 500 m nearest-neighbour fallback, then saves
#          a flat CSV.
#
# Fully self-contained - no bulk scripts required.
#
# NOTE: JULIA_NUM_THREADS must be set via the -t flag or environment variable
#       before Julia starts (e.g. julia -t 24 Phase_2.jl). Setting
#       ENV["JULIA_NUM_THREADS"] inside a running script has no effect.
#
# Inputs:  Data/Python/Py_Phase2_OSM_Golf_Polygons.gpkg
#          Phase 1 Parsing/Data/Julia/Jl_Phase1_Baseline_Golf_Valuation.csv
# Outputs: Data/Julia/Jl_Phase2_OSM_Golf_Polygons.gpkg
#          Data/Julia/Jl_Phase2_Acreage_Matched.csv


# === 1. LIBRARIES ===

# X-08/Decision 1 (2026-07-27): pinned environment, not the machine's global one.
import Pkg
Pkg.activate(normpath(joinpath(@__DIR__, "..")); io = devnull)

using CSV, DataFrames, GeoDataFrames, ArchGDAL, Statistics


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR   = @__DIR__
const PY_GPKG      = joinpath(SCRIPT_DIR, "Data", "python", "Py_Phase2_OSM_Golf_Polygons.gpkg")
const OSM_GPKG_OUT = joinpath(SCRIPT_DIR, "Data", "Julia",  "Jl_Phase2_OSM_Golf_Polygons.gpkg")
const PHASE1_CSV   = joinpath(SCRIPT_DIR, "..", "Phase 1 Parsing", "Data", "Julia",
                                "Jl_Phase1_Baseline_Golf_Valuation.csv")
const OUT_CSV      = joinpath(SCRIPT_DIR, "Data", "Julia", "Jl_Phase2_Acreage_Matched.csv")

const MIN_ACRES     = 5.0
const MAX_ACRES     = 1500.0
const SQ_M_PER_ACRE = 4046.8564224
const MAX_NEAREST_M = 500.0


# === 3. FUNCTIONS ===

function format_number(n::Integer)
    s   = string(n)
    length(s) <= 3 && return s
    result = ""
    for (i, c) in enumerate(reverse(s))
        if i > 1 && (i - 1) % 3 == 0
            result *= ","
        end
        result *= c
    end
    return reverse(result)
end

function format_decimal(n::Real, digits::Int = 1)
    (ismissing(n) || isnan(n)) && return "NaN"
    s = string(round(n; digits = digits))
    occursin('.', s) || (s *= ".0")
    parts    = split(s, '.')
    int_part = format_number(parse(Int, parts[1]))
    return int_part * "." * parts[2]
end


# === 4. EXECUTION ===

function main()
    global_start = time()

    println("=" ^ 80)
    println("PHASE 2 PIPELINE: OSM Polygon Extraction & Spatial Matching")
    println("Script: Phase_2.jl (master)")
    println("=" ^ 80)
    println("  Python GPKG input : $PY_GPKG")
    println("  Julia GPKG output : $OSM_GPKG_OUT")
    println("  Phase 1 CSV       : $PHASE1_CSV")
    println("  Output CSV        : $OUT_CSV")
    println("  Threads           : $(Threads.nthreads())")
    println()


    #  STEP 1: Parse OSM Polygons 

    println("=" ^ 80)
    println("STEP 1: Parse OSM Polygons and Calculate True Acreage")
    println("=" ^ 80)

    isfile(PY_GPKG) || error("Python GPKG not found: $PY_GPKG\nRun Phase_2.py first.")

    println("\n1  Loading golf-course polygons from Python GPKG")
    t0           = time()
    osm_golf_geo = GeoDataFrames.read(PY_GPKG)  # [METHODOLOGY] read from pyosmium GPKG
    raw_count    = nrow(osm_golf_geo)
    println("    Loaded: $(format_number(raw_count)) polygons in $(round(time() - t0, digits = 2))s")

    println("\n2 & 3  Calculating acreage (parallel, $(Threads.nthreads()) threads)")
    area_results = Vector{Float64}(undef, raw_count)
    Threads.@threads for i in 1:raw_count  # [METHODOLOGY] planar area in EPSG:5070
        area_results[i] = ArchGDAL.geomarea(osm_golf_geo.geometry[i]) / SQ_M_PER_ACRE
    end
    osm_golf_geo.osm_acreage = area_results

    println("\n4  Applying plausibility filter ($(MIN_ACRES)–$(MAX_ACRES) acres)")
    osm_golf_geo   = filter(
        row -> row.osm_acreage >= MIN_ACRES && row.osm_acreage <= MAX_ACRES,
        osm_golf_geo
    )
    filtered_count = nrow(osm_golf_geo)
    dropped        = raw_count - filtered_count

    println("\n=== STEP 1 OUTPUT STATISTICS ===")
    println("  Raw polygons before filter:   $(format_number(raw_count))")
    println("  Dropped (< $MIN_ACRES or > $MAX_ACRES acres): $(format_number(dropped))")
    println("  Final polygon count:          $(format_number(filtered_count))")

    ac = osm_golf_geo.osm_acreage
    println("\n  osm_acreage summary:")
    println("    Min:    $(format_decimal(minimum(ac))) acres")
    println("    Median: $(format_decimal(median(ac))) acres")
    println("    Mean:   $(format_decimal(mean(ac))) acres")
    println("    Max:    $(format_decimal(maximum(ac))) acres")

    println("\n5  Saving to $OSM_GPKG_OUT")
    mkpath(dirname(OSM_GPKG_OUT))
    isfile(OSM_GPKG_OUT) && rm(OSM_GPKG_OUT)
    GeoDataFrames.write(OSM_GPKG_OUT, osm_golf_geo; layer_name = "golf_courses")  # [METHODOLOGY]
    println("  [OK] Saved -> $OSM_GPKG_OUT")

    step1_elapsed = round(time() - global_start; digits = 2)
    println("\n[OK] Step 1 completed in $(step1_elapsed)s")


    #  STEP 2: Match OSM to Phase 1 Courses

    println("\n" * "=" ^ 80)
    println("STEP 2: Match OSM Polygons to Golf Courses")
    println("=" ^ 80)
    step2_start = time()

    isfile(PHASE1_CSV) || error("Phase 1 CSV not found: $PHASE1_CSV")

    println("\n 1  Loading Phase 1 baseline dataset")
    courses_df = CSV.read(PHASE1_CSV, DataFrame)
    courses_df = dropmissing(courses_df, [:Longitude, :Latitude])
    original_n = nrow(courses_df)
    println("    Phase 1 rows: $(format_number(original_n))")

    println("    Projecting coordinates to EPSG:5070...")
    source_crs = ArchGDAL.importPROJ4("+proj=longlat +datum=WGS84 +no_defs")
    target_crs = ArchGDAL.importPROJ4(
        "+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 " *
        "+x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
    )

    pt_geoms = Vector{ArchGDAL.IGeometry}(undef, nrow(courses_df))
    ArchGDAL.createcoordtrans(source_crs, target_crs) do transform  # [METHODOLOGY] EPSG:5070 - equal-area CRS
        for i in 1:nrow(courses_df)
            pt = ArchGDAL.createpoint(courses_df.Longitude[i], courses_df.Latitude[i])
            ArchGDAL.transform!(pt, transform)
            pt_geoms[i] = pt
        end
    end

    println("\n 2  Using OSM polygons from Step 1 ($(format_number(nrow(osm_golf_geo))) features)")
    poly_geoms     = osm_golf_geo.geometry
    poly_acres     = osm_golf_geo.osm_acreage
    poly_envelopes = ArchGDAL.envelope.(poly_geoms)

    println("\n 3a Spatial join (intersects) with 500 m fallback")
    println("    Using $(Threads.nthreads()) threads")

    acreage_results = Vector{Union{Float64, Missing}}(missing, nrow(courses_df))
    fallback_flags  = fill(false, nrow(courses_df))

    Threads.@threads for i in 1:nrow(courses_df)  # [METHODOLOGY] point-in-polygon primary match
        pt          = pt_geoms[i]
        px          = ArchGDAL.getx(pt, 0)
        py          = ArchGDAL.gety(pt, 0)
        match_found = false
        best_area   = -Inf

        # [METHODOLOGY] tie-break: when a point falls inside >1 candidate polygon,
        # keep the largest-area match (matches R's explicit largest-area-wins rule)
        for j in 1:nrow(osm_golf_geo)
            env = poly_envelopes[j]
            if px >= env.MinX && px <= env.MaxX && py >= env.MinY && py <= env.MaxY
                if ArchGDAL.intersects(pt, poly_geoms[j])
                    if poly_acres[j] > best_area
                        acreage_results[i] = poly_acres[j]
                        best_area          = poly_acres[j]
                    end
                    match_found = true
                end
            end
        end

        if !match_found  # [METHODOLOGY] nearest-feature fallback for unmatched courses
            min_dist = Inf
            best_idx = 0
            for j in 1:nrow(osm_golf_geo)
                env = poly_envelopes[j]
                if px >= (env.MinX - MAX_NEAREST_M) && px <= (env.MaxX + MAX_NEAREST_M) &&
                   py >= (env.MinY - MAX_NEAREST_M) && py <= (env.MaxY + MAX_NEAREST_M)
                    dist = ArchGDAL.distance(pt, poly_geoms[j])
                    # [METHODOLOGY] tie-break: among equidistant candidates, keep the largest-area match
                    if dist <= MAX_NEAREST_M &&
                       (dist < min_dist || (dist == min_dist && best_idx > 0 && poly_acres[j] > poly_acres[best_idx]))
                        min_dist = dist
                        best_idx = j
                    end
                end
            end
            if best_idx > 0
                acreage_results[i] = poly_acres[best_idx]
                fallback_flags[i]  = true
            end
        end
    end

    courses_df.osm_acreage    = acreage_results
    courses_df.acreage_source = ifelse.(ismissing.(courses_df.osm_acreage), "MICE_Target", "OSM")

    has_acre_mask = .!ismissing.(courses_df.osm_acreage)
    has_acre      = sum(has_acre_mask)
    miss_acre     = sum(ismissing.(courses_df.osm_acreage))
    recovered     = sum(fallback_flags)

    println("\n=== STEP 2 OUTPUT STATISTICS ===")
    println("  Total courses:               $(format_number(original_n))")
    println("  Direct intersect hits:       $(format_number(has_acre - recovered))")
    println("  Recovered via nearest:       $(format_number(recovered))")
    println("  Matched with osm_acreage:    $(format_number(has_acre)) ($(round(100 * has_acre / original_n, digits = 1))%)")
    println("  Missing osm_acreage:         $(format_number(miss_acre))  (MICE target)")

    ac = courses_df.osm_acreage[has_acre_mask]
    if length(ac) > 0
        println("\n  osm_acreage summary (matched only):")
        println("    Min:    $(format_decimal(minimum(ac))) acres")
        println("    Median: $(format_decimal(median(ac))) acres")
        println("    Mean:   $(format_decimal(mean(ac))) acres")
        println("    Max:    $(format_decimal(maximum(ac))) acres")
    end

    mkpath(dirname(OUT_CSV))
    CSV.write(OUT_CSV, courses_df)
    println("\n  [OK] Saved -> $OUT_CSV")

    step2_elapsed = round(time() - step2_start; digits = 2)
    println("\n[OK] Step 2 completed in $(step2_elapsed)s")


    #  Final summary 

    total_elapsed = round(time() - global_start; digits = 2)
    println("\n" * "=" ^ 80)
    println("PHASE 2 PIPELINE COMPLETE")
    println("=" ^ 80)
    println("Total execution time: $(total_elapsed)s")
    println("\nOutput files:")
    println("  Intermediate GPKG : $(basename(OSM_GPKG_OUT))")
    println("  Final Matched CSV : $(basename(OUT_CSV))")
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
