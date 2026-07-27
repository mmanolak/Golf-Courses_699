# Purpose: Complete Phase 3 pipeline - MICE imputation (m=5) then Rubin's
#          Rules pooling to produce a national land-value estimate with 95%/99% CI.
# Inputs:  Phase 2 Spatial Polygons and True Acreage/Data/Julia/
#            Jl_Phase2_Acreage_Matched.csv
# Outputs: Data/Julia/Jl_Imputed_Dataset_{1..100}.csv
#          Data/Julia/Jl_Rubins_Rules_Summary.csv
#          Data/Julia/Jl_National_Acreage_Summary.csv
#
# Rubin's Rules formula summary (m = 100 imputations):
#   q_bar = mean(Q_i)           -- pooled point estimate
#   v_w   = mean(Var_i)         -- within-imputation variance
#   v_b   = var(Q_i, ddof=1)    -- between-imputation variance
#   v_t   = v_w + v_b + v_b/m  -- total variance
#   se    = sqrt(v_t)
#   99%CI = q_bar +/- 2.576 * se
#   95%CI = q_bar +/- 1.960 * se
#
#          for running the script:
#          julia --threads=auto .\Phase_3.jl


# === 1. LIBRARIES ===

# X-08/Decision 1 (2026-07-27): pinned environment, not the machine's global one.
import Pkg
Pkg.activate(normpath(joinpath(@__DIR__, "..")); io = devnull)

using CategoricalArrays, CSV, DataFrames, Mice, Printf, Random, Statistics


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR = @__DIR__
const PROV_START = time()
include(joinpath(SCRIPT_DIR, "..", "provenance.jl"))

@printf("    Julia threads available: %d\n", Threads.nthreads())
if Threads.nthreads() == 1
    @warn "Running on 1 thread - parallel speedup disabled. " *
            "Relaunch with: julia --threads=auto .\\Phase_3.jl"
end

const INPUT_CSV  = joinpath(
    SCRIPT_DIR, "..",
    "Phase 2 Spatial Polygons and True Acreage",
    "Data", "Julia",
    "Jl_Phase2_Acreage_Matched.csv"
)
const OUT_DIR = joinpath(SCRIPT_DIR, "Data", "Julia")
const OUT_CSV         = joinpath(OUT_DIR, "Jl_Rubins_Rules_Summary.csv")
const OUT_ACREAGE_CSV = joinpath(OUT_DIR, "Jl_National_Acreage_Summary.csv")
const M             = 100
const IMPUTE_COLS   = [:osm_acreage, :Baseline_Value_Per_Acre]
const PREDICTOR_COLS = [:Holes, :Course_Type, :county_type, :Longitude, :Latitude]


# === 3. FUNCTIONS ===

function run_imputation(input_csv::String, out_dir::String; m_datasets::Int = M)
    # 1. Load ----------------------------------------------------------------
    println("--- 1  Loading Phase 2 acreage-matched dataset ---")

    isfile(input_csv) || error("Input file not found: $input_csv")
    mkpath(out_dir)

    acreage_df = CSV.read(input_csv, DataFrame)
    println("    Rows: $(size(acreage_df, 1))")
    println("    Missing osm_acreage:            $(sum(ismissing, acreage_df.osm_acreage))")
    println("    Missing Baseline_Value_Per_Acre: $(sum(ismissing, acreage_df.Baseline_Value_Per_Acre))")

    # 2. Prepare -------------------------------------------------------------
    println("--- 2  Preparing imputation frame ---")

    acreage_df.Holes = [
        ismissing(x) || string(x) == "" ? missing :
        parse(Float64, string(x))
        for x in acreage_df.Holes
    ]
    acreage_df.Course_Type = categorical(acreage_df.Ownership_Type)
    acreage_df.county_type = categorical(acreage_df.county_type)

    model_cols = vcat(PREDICTOR_COLS, IMPUTE_COLS)
    imp_df     = acreage_df[!, model_cols]

    # 3. Run MICE ------------------------------------------------------------
    println("--- 3  Running MICE imputation (m=$m_datasets) ---")
    Random.seed!(42)  # [METHODOLOGY] reproducibility seed for stochastic MICE imputation
    # [METHODOLOGY] Mice.jl MICE - m=100 multiply-imputed datasets, iter=10;
    #               see Van Buuren (2018) for methodology
    # [METHODOLOGY] explicit visitsequence (P1-01/D-2) - Mice.jl imputes every missing value
    # by default; restricting the visit sequence to IMPUTE_COLS keeps predictors as
    # predictors regardless of what Phase 1 does or doesn't leave missing upstream.
    # mice()'s visitsequence::Vector{String} signature requires String, not Symbol -
    # IMPUTE_COLS is declared as Symbols (used elsewhere for DataFrame column selection).
    imputed_list = mice(imp_df, m = m_datasets, visitsequence = string.(IMPUTE_COLS), iter = 10)

    # 4. Save each imputed dataset -------------------------------------------
    println("\n--- 4  Saving $m_datasets imputed datasets ---")

    for i in 1:m_datasets
        completed = complete(imputed_list, i)

        completed.osm_acreage            = clamp.(completed.osm_acreage, 0, Inf)
        completed.Baseline_Value_Per_Acre = clamp.(
            completed.Baseline_Value_Per_Acre, 0, Inf
        )

        out = copy(acreage_df)
        out.osm_acreage            = completed.osm_acreage
        out.Baseline_Value_Per_Acre = completed.Baseline_Value_Per_Acre

        # Mice.jl's complete() returns drawn values for ALL rows, not just missing
        # ones. Restore original observed (non-missing) values so MICE never
        # overwrites anchored inputs like Hawaii Kai's BVPA = $4,952,600.
        for col in IMPUTE_COLS
            orig = acreage_df[!, col]
            obs  = .!ismissing.(orig)
            out[obs, col] = orig[obs]
        end

        fname = joinpath(out_dir, "Jl_Imputed_Dataset_$i.csv")
        CSV.write(fname, out; header = true, quote_empty_string = false)
        println("    [OK] $fname")
    end

    # 5. Verification report (Dataset 1) -------------------------------------
    ds1 = CSV.read(joinpath(out_dir, "Jl_Imputed_Dataset_1.csv"), DataFrame)

    println("\n=== IMPUTATION VERIFICATION (Dataset 1) ===")
    println("  Method: Mice.jl")
    println("  Datasets generated:     $m_datasets")
    println("  Iterations per dataset: 10")

    for col in IMPUTE_COLS
        s = ds1[!, col]
        println("\n  $col:")
        println("    Missing:  $(sum(ismissing, s))")
        @printf "    Min:      %14.2f\n" minimum(s)
        @printf "    Median:   %14.2f\n" median(s)
        @printf "    Mean:     %14.2f\n" mean(s)
        @printf "    Max:      %14.2f\n" maximum(s)
        println("    Negative: $(sum(<(0), s))")
    end
end

function run_pooling(in_dir::String, out_csv::String; m_datasets::Int = M)
    aggregates  = Float64[]
    within_vars = Float64[]

    println("--- 1  Loading imputed datasets and computing aggregates ---\n")

    for i in 1:m_datasets
        fpath = joinpath(in_dir, "Jl_Imputed_Dataset_$i.csv")
        isfile(fpath) || error("Imputed dataset not found: $fpath")

        df = CSV.read(fpath, DataFrame)
        df.Total_Opportunity_Cost = df.osm_acreage .* df.Baseline_Value_Per_Acre

        q_i   = sum(df.Total_Opportunity_Cost)
        var_i = var(df.Total_Opportunity_Cost)

        push!(aggregates,  q_i)
        push!(within_vars, var_i)

        df = nothing; GC.gc()
        @printf("  Dataset %d:  \$%10.3f B\n", i, q_i / 1e9)
    end

    # [METHODOLOGY] Rubin's Rules pooling - q_bar is the pooled national estimate;
    #               v_t combines within- and between-imputation variance (Rubin 1987)
    println("\n--- 2  Applying Rubin's Rules ---")

    q_bar = mean(aggregates)
    v_w   = mean(within_vars)
    v_b   = var(aggregates; corrected = true)
    v_t   = v_w + v_b + v_b / m_datasets
    se    = sqrt(v_t)
    ci95_lo = q_bar - 1.960 * se
    ci95_hi = q_bar + 1.960 * se
    ci99_lo = q_bar - 2.576 * se
    ci99_hi = q_bar + 2.576 * se

    println("\n=== RUBIN'S RULES RESULTS ===")
    @printf("  Pooled Aggregate National Value:  \$%10.3f B\n", q_bar / 1e9)
    @printf("  Within-Imputation Variance (v_w): %.4e\n", v_w)
    @printf("  Between-Imputation Variance (v_b):%.4e\n", v_b)
    @printf("  Total Variance (v_t):             %.4e\n", v_t)
    @printf("  Standard Error:                   \$%10.3f B\n", se / 1e9)
    @printf(
        "  99%% Confidence Interval:          \$%10.3f B - \$%10.3f B\n",
        ci99_lo / 1e9, ci99_hi / 1e9
    )
    @printf(
        "  95%% Confidence Interval:          \$%10.3f B - \$%10.3f B\n",
        ci95_lo / 1e9, ci95_hi / 1e9
    )

    pooled_df = DataFrame(
        Metric = vcat(
            [
                "Pooled Aggregate National Value (\$)",
                "Pooled Aggregate National Value (\$B)",
                "Within-Imputation Variance (v_w)",
                "Between-Imputation Variance (v_b)",
                "Total Variance (v_t)",
                "Standard Error (\$)",
                "99% CI Lower (\$B)",
                "99% CI Upper (\$B)",
                "95% CI Lower (\$B)",
                "95% CI Upper (\$B)",
            ],
            ["Dataset $i Aggregate (\$B)" for i in 1:m_datasets]
        ),
        Value = vcat(
            [
                @sprintf("%.2f",  q_bar),
                @sprintf("%.3f",  q_bar / 1e9),
                @sprintf("%.4e",  v_w),
                @sprintf("%.4e",  v_b),
                @sprintf("%.4e",  v_t),
                @sprintf("%.2f",  se),
                @sprintf("%.3f",  ci99_lo / 1e9),
                @sprintf("%.3f",  ci99_hi / 1e9),
                @sprintf("%.3f",  ci95_lo / 1e9),
                @sprintf("%.3f",  ci95_hi / 1e9),
            ],
            [@sprintf("%.3f", aggregates[i] / 1e9) for i in 1:m_datasets]
        )
    )

    CSV.write(out_csv, pooled_df; header = true)
    println("\n  [OK] Saved -> $out_csv")

    return q_bar, se, ci95_lo, ci95_hi, ci99_lo, ci99_hi
end


function pool_acreage(x::AbstractVector{<:Real})
    q_bar = mean(x)
    v_b   = var(x; corrected = true)
    se    = sqrt(v_b + v_b / length(x))
    return (
        mean    = q_bar,
        sd_b    = sqrt(v_b),
        ci95_lo = q_bar - 1.960 * se,
        ci95_hi = q_bar + 1.960 * se,
        ci99_lo = q_bar - 2.576 * se,
        ci99_hi = q_bar + 2.576 * se,
    )
end

function run_acreage_summary(in_dir::String, out_csv::String; m_datasets::Int = M)
    fmt(x) = replace(@sprintf("%d", round(Int, x)), r"(?<=\d)(?=(\d{3})+$)" => ",")

    println("--- 1  Loading imputed datasets and computing acreage totals ---\n")

    national_totals = zeros(Float64, m_datasets)
    by_type_list    = Vector{DataFrame}(undef, m_datasets)

    for i in 1:m_datasets
        path = joinpath(in_dir, "Jl_Imputed_Dataset_$i.csv")
        isfile(path) || error("File not found: $path")

        df = CSV.read(path, DataFrame)

        national_totals[i] = sum(skipmissing(df.osm_acreage))

        type_sums = combine(
            groupby(df, :county_type),
            :osm_acreage => (x -> sum(skipmissing(x))) => :acreage,
        )
        type_sums.imputation = fill(i, nrow(type_sums))
        by_type_list[i] = type_sums

        df = nothing; GC.gc()
        urban_acres = only(filter(r -> isequal(r.county_type, "Urban"), type_sums).acreage)
        rural_acres = only(filter(r -> isequal(r.county_type, "Rural"), type_sums).acreage)
        @printf("  Dataset %d:  %s acres  (%s Urban / %s Rural)\n",
            i, fmt(national_totals[i]), fmt(urban_acres), fmt(rural_acres))
    end

    println("\n--- 2  Pooling across imputations ---\n")

    # [METHODOLOGY] Rubin's Rules (acreage) - between-imputation variance only;
    #               within-variance is zero for a spatially fixed attribute
    nat_pool    = pool_acreage(national_totals)
    all_by_type = vcat(by_type_list...)
    type_groups = groupby(all_by_type, :county_type)
    type_pool   = combine(type_groups) do grp
        p = pool_acreage(grp.acreage)
        DataFrame(
            pooled_acres = p.mean,
            sd_b         = p.sd_b,
            ci95_lo      = p.ci95_lo,
            ci95_hi      = p.ci95_hi,
            ci99_lo      = p.ci99_lo,
            ci99_hi      = p.ci99_hi,
        )
    end
    sort!(type_pool, :pooled_acres, rev = true)

    println("=== NATIONAL ACREAGE RESULTS ===")
    @printf("  Total U.S. Golf Acreage:  %s acres\n",          fmt(nat_pool.mean))
    @printf("  Between-Imputation SD:    %.2f\n",               nat_pool.sd_b)
    @printf("  99%% CI:                   %s - %s acres\n",     fmt(nat_pool.ci99_lo), fmt(nat_pool.ci99_hi))
    @printf("  95%% CI:                   %s - %s acres\n",     fmt(nat_pool.ci95_lo), fmt(nat_pool.ci95_hi))
    for row in eachrow(type_pool)
        @printf("  %-20s %s acres\n", row.county_type, fmt(row.pooled_acres))
    end

    national_row = DataFrame(
        Category          = "National Total",
        County_Type       = "All",
        Pooled_Acres      = round(nat_pool.mean, digits = 2),
        SD_Between        = round(nat_pool.sd_b,  digits = 4),
        CI_95_Lower_Acres = round(nat_pool.ci95_lo, digits = 2),
        CI_95_Upper_Acres = round(nat_pool.ci95_hi, digits = 2),
        CI_99_Lower_Acres = round(nat_pool.ci99_lo, digits = 2),
        CI_99_Upper_Acres = round(nat_pool.ci99_hi, digits = 2),
    )
    type_rows = DataFrame(
        Category          = fill("By County Type", nrow(type_pool)),
        County_Type       = type_pool.county_type,
        Pooled_Acres      = round.(type_pool.pooled_acres, digits = 2),
        SD_Between        = round.(type_pool.sd_b,         digits = 4),
        CI_95_Lower_Acres = round.(type_pool.ci95_lo,      digits = 2),
        CI_95_Upper_Acres = round.(type_pool.ci95_hi,      digits = 2),
        CI_99_Lower_Acres = round.(type_pool.ci99_lo,      digits = 2),
        CI_99_Upper_Acres = round.(type_pool.ci99_hi,      digits = 2),
    )
    summary_df = vcat(national_row, type_rows)

    CSV.write(out_csv, summary_df)
    println("\n  [OK] Saved -> $out_csv")
end


# === 4. EXECUTION ===

function main()
    println("PHASE 3 ANALYSIS PIPELINE (Julia)")
    println("Output directory: $OUT_DIR\n")

    run_imputation(INPUT_CSV, OUT_DIR; m_datasets = M)

    println("\n=== STEP 2: RUBIN'S RULES POOLING ===")
    run_pooling(OUT_DIR, OUT_CSV; m_datasets = M)

    println("\n=== STEP 3: NATIONAL ACREAGE SUMMARY ===")
    run_acreage_summary(OUT_DIR, OUT_ACREAGE_CSV; m_datasets = M)

    println("\n=== PHASE 3 ANALYSIS COMPLETE ===")
    println("Output files saved to: $OUT_DIR")
    for i in 1:M
        println("  - Jl_Imputed_Dataset_$i.csv")
    end
    println("  - Jl_Rubins_Rules_Summary.csv")
    println("  - Jl_National_Acreage_Summary.csv")
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
    record_provenance("Phase 3", "Phase_3.jl", SCRIPT_DIR, PROV_START;
                       M = M, maxit = 10, seed = 42)
end
