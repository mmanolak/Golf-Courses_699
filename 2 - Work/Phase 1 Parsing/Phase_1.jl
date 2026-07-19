# Purpose: Master pipeline - parse, spatial-join, proxy-merge, and classify
#          baseline land values for all US golf courses (Phase 1).
# Inputs:  00 - Data Sources/Original Data/Golf Courses-USA.csv
#          00 - Data Sources/Original Data/2022 - USDA County Data - Ag Use.csv
#          00 - Data Sources/Original Data/2024 - FHFA June 20 Land Prices.xlsx
#          00 - Data Sources/Secondary/2023-rural-urban-continuum-codes.csv
# Outputs: Phase 1 Parsing/Data/Julia/Jl_Phase1_Parsed_Golf_Courses.csv
#          Phase 1 Parsing/Data/Julia/Jl_Phase1_Spatial_Joined_Golf_Courses.csv
#          Phase 1 Parsing/Data/Julia/Jl_Phase1_Baseline_Golf_Valuation.csv
#
#          for running the script:
#          julia --threads=auto .\Phase_1.jl


# === 1. LIBRARIES ===

    using CSV, DataFrames, GeoDataFrames, ArchGDAL, Downloads, XLSX, Printf, Statistics


    # === 2. GLOBALS & PATHS ===

    ENV["JULIA_NUM_THREADS"] = "24"

SCRIPT_DIR = @__DIR__
ROOT_DIR   = joinpath(SCRIPT_DIR, "..")
DATA_DIR   = joinpath(ROOT_DIR, "00 - Data Sources", "Original Data")
OUTPUT_DIR = joinpath(SCRIPT_DIR, "Data", "Julia")

RAW_CSV    = joinpath(DATA_DIR, "Golf Courses-USA.csv")
USDA_IN    = joinpath(DATA_DIR, "2022 - USDA County Data - Ag Use.csv")
FHFA_IN    = joinpath(DATA_DIR, "2024 - FHFA June 20 Land Prices.xlsx")
RUCC_CSV   = joinpath(ROOT_DIR, "00 - Data Sources", "Secondary", "2023-rural-urban-continuum-codes.csv")

COUNTY_DIR = DATA_DIR
COUNTY_SHP = joinpath(COUNTY_DIR, "tl_2022_us_county.shp")
COUNTY_ZIP = joinpath(COUNTY_DIR, "tl_2022_us_county.zip")
COUNTY_URL = "https://www2.census.gov/geo/tiger/TIGER2022/COUNTY/tl_2022_us_county.zip"

OUT_PARSED   = joinpath(OUTPUT_DIR, "Jl_Phase1_Parsed_Golf_Courses.csv")
OUT_SPATIAL  = joinpath(OUTPUT_DIR, "Jl_Phase1_Spatial_Joined_Golf_Courses.csv")
OUT_BASELINE = joinpath(OUTPUT_DIR, "Jl_Phase1_Baseline_Golf_Valuation.csv")


# === 3. FUNCTIONS ===

const STATE_FIPS_TO_ABBR = Dict(
    "01" => "AL", "02" => "AK", "04" => "AZ", "05" => "AR", "06" => "CA",
    "08" => "CO", "09" => "CT", "10" => "DE", "11" => "DC", "12" => "FL",
    "13" => "GA", "15" => "HI", "16" => "ID", "17" => "IL", "18" => "IN",
    "19" => "IA", "20" => "KS", "21" => "KY", "22" => "LA", "23" => "ME",
    "24" => "MD", "25" => "MA", "26" => "MI", "27" => "MN", "28" => "MS",
    "29" => "MO", "30" => "MT", "31" => "NE", "32" => "NV", "33" => "NH",
    "34" => "NJ", "35" => "NM", "36" => "NY", "37" => "NC", "38" => "ND",
    "39" => "OH", "40" => "OK", "41" => "OR", "42" => "PA", "44" => "RI",
    "45" => "SC", "46" => "SD", "47" => "TN", "48" => "TX", "49" => "UT",
    "50" => "VT", "51" => "VA", "53" => "WA", "54" => "WV", "55" => "WI",
    "56" => "WY", "60" => "AS", "66" => "GU", "69" => "MP", "72" => "PR",
    "78" => "VI"
)

function name_state_to_name(name_state::String)::String
    m = match(r"^(.+?)\s*-\s*[A-Z]{2}$", name_state)
    return isnothing(m) ? name_state : m.captures[1]
end

function name_state_to_abbr(name_state::String)::String
    m = match(r"([A-Z]{2})$", name_state)
    return isnothing(m) ? "" : m.captures[1]
end

function extract_ownership(details::String)::String
    m = match(r"^\(([^)]+)\)", details)
    return isnothing(m) ? "" : m.captures[1]
end

function extract_holes(details::String)::Int64
    s = lowercase(details)
    m = match(r"\((\d+)\s*hole[s]?\)", s)
    return isnothing(m) ? 18 : parse(Int64, m.captures[1])
end

function extract_zip(details::String)::String
    m = match(r"([A-Z]{2})\s+(\d{5})", details)
    return isnothing(m) ? "" : m.captures[2]
end

function extract_city(details::String)::String
    m = match(r",([^,]+),\s*[A-Z]{2}", details)
    if isnothing(m)
        return ""
    end
    city = m.captures[1]
    city = lstrip(city, [' ', ','])
    city = rstrip(city, [',', ' '])
    return city
end

function extract_address(details::String)::String
    m = match(r"\),\s*(.*?),(?=\s*[^,]+,[A-Z]{2})", details)
    if isnothing(m)
        return ""
    end
    addr = m.captures[1]
    addr = lstrip(addr, [',', ' ', ')'])
    addr = rstrip(addr, [',', ' '])
    return addr
end

function format_number(n::Integer)
    s = string(n)
    len = length(s)
    if len <= 3
        return s
    end
    result = ""
    for (i, c) in enumerate(reverse(s))
        if i > 1 && (i-1) % 3 == 0
            result *= ","
        end
        result *= c
    end
    return reverse(result)
end

function format_fips(x, pad_len::Int)
    if ismissing(x)
        return ""
    end
    s = replace(string(x), r"\.0$" => "")
    return lpad(s, pad_len, '0')
end


# === 4. EXECUTION ===

function main()
    for path in (RAW_CSV, USDA_IN, FHFA_IN, RUCC_CSV)
        if !isfile(path)
            error("Input file not found: $path")
        end
    end

    mkpath(OUTPUT_DIR)

    println("="^80)
    println("PHASE 1 PIPELINE: Parse, Spatial Join, Economic Proxy Merge & Baseline Valuation")
    println("="^80)
    global_start = time()

    # Step 1: Parse & Deduplicate
    println("\n" * "="^80)
    println("STEP 1: Parsing and Deduplicating Raw Golf Course Data")
    println("="^80)
    step_start = time()

    courses_df = CSV.read(RAW_CSV, DataFrame; header=0)
    rename!(courses_df, "Column1" => :Longitude, "Column2" => :Latitude,
            "Column3" => :Name_State, "Column4" => :Details)
    original_n = nrow(courses_df)

    courses_df = transform(courses_df,
        :Name_State => ByRow(name_state_to_name) => :Course_Name,
        :Name_State => ByRow(name_state_to_abbr) => :State_Abbr,
        :Details    => ByRow(extract_ownership)  => :Ownership_Type,
        :Details    => ByRow(extract_holes)      => :Holes,
        :Details    => ByRow(extract_zip)        => :Zip_Code,
        :Details    => ByRow(extract_city)       => :City,
        :Details    => ByRow(extract_address)    => :Address
    )

    courses_df.Latitude_rounded  = round.(courses_df.Latitude,  digits=4)
    courses_df.Longitude_rounded = round.(courses_df.Longitude, digits=4)

    courses_df = combine(groupby(courses_df, [:Latitude_rounded, :Longitude_rounded, :Course_Name])) do g
        sort!(g, :Holes, rev=true)
        first(g)
    end

    courses_df.course_id = 1:nrow(courses_df)
    courses_df = select(courses_df,
        :course_id, :Course_Name, :Ownership_Type, :Holes,
        :Address, :City, :State_Abbr, :Zip_Code, :Longitude, :Latitude
    )

    final_n = nrow(courses_df)
    println("  Original raw rows:      $original_n")
    println("  Duplicates removed:     $(original_n - final_n)")
    println("  Final cleaned rows:     $final_n")

    CSV.write(OUT_PARSED, courses_df)
    println("\n  [OK] Parsed data saved -> $OUT_PARSED")
    println("\n[OK] Step 1 completed in $(round(time() - step_start; digits=2))s")

    # Step 2: Spatial Join
    println("\n" * "="^80)
    println("STEP 2: Spatially Joining to US Counties")
    println("="^80)
    step_start = time()

    courses_df = dropmissing(courses_df, [:Longitude, :Latitude])
    courses_df.geometry = [ArchGDAL.createpoint(row.Longitude, row.Latitude) for row in eachrow(courses_df)]
    courses_geo = courses_df

    if !isfile(COUNTY_SHP)
        if !isfile(COUNTY_ZIP)
            Downloads.download(COUNTY_URL, COUNTY_ZIP)
        end
        run(`7z x -y -o"$COUNTY_DIR" "$COUNTY_ZIP"`)
    end

    # [METHODOLOGY] Spatial read - county boundaries in EPSG 4326 (WGS 84), matching golf course point CRS
    county_geo = GeoDataFrames.read(COUNTY_SHP)
    county_geo = transform(county_geo,
        :GEOID   => ByRow(string)                                         => :FIPS,
        :STATEFP => ByRow(fp -> get(STATE_FIPS_TO_ABBR, string(fp), "")) => :Tigris_State_Abbr
    )
    county_geo = select(county_geo, :FIPS, :NAME => :County_Name, :Tigris_State_Abbr, :geometry)

    county_envelopes = ArchGDAL.envelope.(county_geo.geometry)

    fips_results        = Vector{Union{String, Missing}}(missing, nrow(courses_geo))
    county_name_results = Vector{Union{String, Missing}}(missing, nrow(courses_geo))
    state_abbr_results  = Vector{Union{String, Missing}}(missing, nrow(courses_geo))

    Threads.@threads for i in 1:nrow(courses_geo)  # [METHODOLOGY]
        pt_geom = courses_geo.geometry[i]
        pt_env  = ArchGDAL.envelope(pt_geom)
        px_min, py_min, px_max, py_max = pt_env.MinX, pt_env.MinY, pt_env.MaxX, pt_env.MaxY

        for j in 1:length(county_envelopes)
            env = county_envelopes[j]
            if px_min >= env.MinX && px_max <= env.MaxX &&
               py_min >= env.MinY && py_max <= env.MaxY
                county_geom = county_geo[!, :geometry][j]
                if ArchGDAL.intersects(pt_geom, county_geom)  # [METHODOLOGY] exact point-in-polygon after bbox pre-filter
                    fips_results[i]        = county_geo[!, :FIPS][j]
                    county_name_results[i] = county_geo[!, :County_Name][j]
                    state_abbr_results[i]  = county_geo[!, :Tigris_State_Abbr][j]
                    break
                end
            end
        end
    end

    courses_geo.FIPS              = fips_results
    courses_geo.County_Name       = county_name_results
    courses_geo.Tigris_State_Abbr = state_abbr_results

    courses_df = select(courses_geo, Not(:geometry))
    courses_df.FIPS = [ismissing(val) ? "" : lpad(string(val), 5, '0') for val in courses_df.FIPS]

    missing_fips = sum(ismissing.(courses_df.FIPS))
    println("  Total golf courses:       $(format_number(final_n))")
    println("  Missing FIPS (no county): $missing_fips")

    CSV.write(OUT_SPATIAL, courses_df)
    println("\n  [OK] Spatial data saved -> $OUT_SPATIAL")
    println("\n[OK] Step 2 completed in $(round(time() - step_start; digits=2))s")

    # Step 3: Economic Proxy Merge & RUCC Classification
    println("\n" * "="^80)
    println("STEP 3: Merging Economic Proxies and Classifying Baseline Values")
    println("="^80)
    step_start = time()

    courses_df = CSV.read(OUT_SPATIAL, DataFrame)
    courses_df.FIPS = format_fips.(courses_df.FIPS, 5)

    usda_df = CSV.read(USDA_IN, DataFrame)
    usda_df = filter(row -> row."Data Item" == "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN \$ / ACRE", usda_df)
    usda_df.State_ANSI  = format_fips.(usda_df[!, "State ANSI"],  2)
    usda_df.County_ANSI = format_fips.(usda_df[!, "County ANSI"], 3)
    usda_df.FIPS = usda_df.State_ANSI .* usda_df.County_ANSI
    usda_df.USDA_Ag_Value_Per_Acre = map(usda_df.Value) do x
        ismissing(x) && return missing
        val = tryparse(Float64, replace(string(x), "," => ""))
        isnothing(val) ? missing : val
    end
    usda_df = dropmissing(select(usda_df, [:FIPS, :USDA_Ag_Value_Per_Acre]), :USDA_Ag_Value_Per_Acre)
    usda_df = unique!(usda_df, :FIPS)

    fhfa_workbook = XLSX.readxlsx(FHFA_IN)
    sheet_names   = XLSX.sheetnames(fhfa_workbook)
    target_sheet  = nothing
    for name in sheet_names
        if lowercase(strip(name)) == "panel counties"
            target_sheet = name
            break
        end
    end
    if isnothing(target_sheet)
        error("Could not find 'Panel Counties' sheet in FHFA file. Available sheets: $sheet_names")
    end

    sheet_data   = fhfa_workbook[target_sheet]
    detected_row = 1
    for r in 1:15
        row_data = sheet_data[r, :]
        if any(x -> !ismissing(x) && occursin("FIPS", string(x)), row_data)
            detected_row = r
            break
        end
    end

    fhfa_df = DataFrame(XLSX.readtable(FHFA_IN, target_sheet; first_row=detected_row))
    if "Year" in names(fhfa_df)
        fhfa_df = filter(row -> row.Year == 2022, fhfa_df)
    end
    fhfa_df.FIPS = format_fips.(fhfa_df.FIPS, 5)

    as_is_col = nothing
    for col in names(fhfa_df)
        if occursin("Per Acre, As-Is", col)
            as_is_col = col
            break
        end
    end
    if isnothing(as_is_col)
        error("Could not find 'Per Acre, As-Is' column in FHFA data")
    end

    fhfa_df.FHFA_Res_Value_Per_Acre = map(fhfa_df[!, as_is_col]) do x
        ismissing(x) && return missing
        val = tryparse(Float64, replace(string(x), "," => ""))
        isnothing(val) ? missing : val
    end
    fhfa_df = dropmissing(select(fhfa_df, [:FIPS, :FHFA_Res_Value_Per_Acre]), :FHFA_Res_Value_Per_Acre)
    fhfa_df = unique!(fhfa_df, :FIPS)

    courses_df = leftjoin(courses_df, usda_df, on=:FIPS)
    courses_df = leftjoin(courses_df, fhfa_df, on=:FIPS)

    rucc_df = CSV.read(RUCC_CSV, DataFrame)
    # RUCC source is long-format with multiple attributes per FIPS - isolate RUCC_2023 to get one code per county
    rucc_df = filter(row -> row.Attribute == "RUCC_2023", rucc_df)
    rucc_df.FIPS = format_fips.(rucc_df.FIPS, 5)
    rucc_df.RUCC_2023 = map(rucc_df.Value) do x
        ismissing(x) && return missing
        val = tryparse(Int64, replace(string(x), "," => ""))
        isnothing(val) ? missing : val
    end
    rucc_df = dropmissing(select(rucc_df, [:FIPS, :RUCC_2023]), :RUCC_2023)
    rucc_df = unique!(rucc_df, :FIPS)

    courses_df = leftjoin(courses_df, rucc_df, on=:FIPS)

    courses_df.county_type = map(courses_df.RUCC_2023) do rucc_val
        if ismissing(rucc_val)
            return missing
        end
        if rucc_val in 1:3
            return "Urban"
        elseif rucc_val in 4:9
            return "Rural"
        else
            return missing
        end
    end

    courses_df.Baseline_Value_Per_Acre = map(
        courses_df.county_type, courses_df.FHFA_Res_Value_Per_Acre, courses_df.USDA_Ag_Value_Per_Acre
    ) do ct, fhfa_val, usda_val
        if ismissing(ct)
            return missing
        elseif ct == "Urban"
            return fhfa_val
        elseif ct == "Rural"
            return usda_val
        else
            return missing
        end
    end

    urban        = count(==("Urban"), skipmissing(courses_df.county_type))
    rural        = count(==("Rural"), skipmissing(courses_df.county_type))
    unclassified = count(ismissing, courses_df.county_type)
    missing_base = count(ismissing, courses_df.Baseline_Value_Per_Acre)
    bv           = courses_df.Baseline_Value_Per_Acre[.!ismissing.(courses_df.Baseline_Value_Per_Acre)]

    println("\n=== OUTPUT STATISTICS ===")
    println("  Total golf courses:       $(lpad(string(final_n), 14, ' '))")
    println("  Urban courses:            $(lpad(string(urban), 14, ' '))")
    println("  Rural courses:            $(lpad(string(rural), 14, ' '))")
    println("  Unclassified (no RUCC):   $unclassified")
    println("  Missing Baseline value:   $(lpad(string(missing_base), 14, ' '))  (MICE imputation target)")

    if !isempty(bv)
        println("\n  Baseline_Value_Per_Acre summary:")
        println("    Min:    \$$(lpad(@sprintf("%.2f", minimum(bv)), 14, ' '))")
        println("    Median: \$$(lpad(@sprintf("%.2f", median(bv)), 14, ' '))")
        println("    Mean:   \$$(lpad(@sprintf("%.2f", mean(bv)), 14, ' '))")
        println("    Max:    \$$(lpad(@sprintf("%.2f", maximum(bv)), 14, ' '))")
    end

    CSV.write(OUT_BASELINE, courses_df)
    println("\n  [OK] Final Baseline saved -> $OUT_BASELINE")
    println("\n[OK] Step 3 completed in $(round(time() - step_start; digits=2))s")

    total_elapsed = round(time() - global_start; digits=2)
    println("\n" * "="^80)
    println("PIPELINE COMPLETE - Total execution time: $(total_elapsed)s")
    println("="^80)
    println("Output files:")
    println("  - Parsed data:    $OUT_PARSED")
    println("  - Spatial join:   $OUT_SPATIAL")
    println("  - Final baseline: $OUT_BASELINE")
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
end
