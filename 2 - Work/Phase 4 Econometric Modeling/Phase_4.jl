# Purpose: Phase 4 master - fit OLS with HC1 robust SEs on each of M Julia-generated
#          MICE imputed datasets, then pool via Rubin's Rules and save results.
# Inputs:  Phase 3 Economic Merge and MICE Imputation/Data/Julia/Jl_Imputed_Dataset_{1..M}.csv
# Outputs: Data/Julia/Jl_model_results.jls
#          Data/Julia/Jl_Regression_Results.csv
#
#          for running the script:
#          julia --threads=auto .\Phase_4.jl


# === 1. LIBRARIES ===

# X-08/Decision 1 (2026-07-27): pinned environment, not the machine's global one.
import Pkg
Pkg.activate(normpath(joinpath(@__DIR__, "..")); io = devnull)

using DataFrames
using CSV
using GLM
using Serialization
using Statistics
using LinearAlgebra
using Printf
using Distributions


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR    = @__DIR__
const PROV_START = time()
include(joinpath(SCRIPT_DIR, "..", "provenance.jl"))

@printf("    Julia threads available: %d\n", Threads.nthreads())
if Threads.nthreads() == 1
    @warn "Running on 1 thread - parallel speedup disabled. " *
            "Relaunch with: julia --threads=auto .\\Phase_4.jl"
end

const PHASE3_DIR    = joinpath(
    SCRIPT_DIR, "..", "Phase 3 Economic Merge and MICE Imputation", "Data", "Julia"
)
const OUT_DIR       = joinpath(SCRIPT_DIR, "Data", "Julia")
const JLS_PATH      = joinpath(OUT_DIR, "Jl_model_results.jls")
const OUT_CSV       = joinpath(OUT_DIR, "Jl_Regression_Results.csv")

const FORMULA_STR   = "Log_Opportunity_Cost ~ Holes + county_type"
const M             = 100
const IMPUTED_PATHS = [
    joinpath(PHASE3_DIR, "Jl_Imputed_Dataset_$i.csv") for i in 1:M
]


# === 3. FUNCTIONS ===

"""
    get_stars(p) -> String

Return significance stars for a single p-value.
"""
function get_stars(p)
    if isnan(p) return "" end
    if p < 0.001 return "***" end
    if p < 0.01  return "**" end
    if p < 0.05  return "*" end
    if p < 0.1   return "." end
    return ""
end


# === 4. EXECUTION ===

function main()
    mkpath(OUT_DIR)

    missing_files = filter(p -> !isfile(p), IMPUTED_PATHS)
    if !isempty(missing_files)
        println("[FATAL] The following imputed dataset(s) were not found:")
        for p in missing_files
            println("  $p")
        end
        exit(1)
    end

    println("\n=== PHASE 4 - STEP 1: MODEL FITTING ===")
    println("Phase 3 inputs : $PHASE3_DIR")
    println("Output folder  : $OUT_DIR")
    println("Formula        : $FORMULA_STR")

    model_results       = []
    first_model_summary = ""

    for (i, path) in enumerate(IMPUTED_PATHS)
        fname = basename(path)
        println("[$i/$M] Loading $fname...")

        acreage_df = CSV.read(path, DataFrame)

        if !("osm_acreage" in names(acreage_df))
            error("Column 'osm_acreage' not found in $fname.")
        end
        if !("Baseline_Value_Per_Acre" in names(acreage_df))
            error("Column 'Baseline_Value_Per_Acre' not found in $fname.")
        end

        acreage_df.Total_Opportunity_Cost = (
            acreage_df.osm_acreage .* acreage_df.Baseline_Value_Per_Acre
        )
        acreage_df.Log_Opportunity_Cost = log1p.(acreage_df.Total_Opportunity_Cost)

        cols_needed = [
            "Log_Opportunity_Cost", "Holes", "Baseline_Value_Per_Acre", "county_type"
        ]
        n_before = nrow(acreage_df)
        dropmissing!(acreage_df, cols_needed)
        n_dropped = n_before - nrow(acreage_df)

        if n_dropped > 0
            println("       Dropped $n_dropped rows with missing values in model columns.")
        end

        acreage_df.county_type = string.(acreage_df.county_type)

        model = lm(@formula(Log_Opportunity_Cost ~ Holes + county_type), acreage_df)  # [METHODOLOGY] OLS - log-linear model for opportunity cost

        # [METHODOLOGY] HC1 robust standard errors - heteroskedasticity-consistent;
        #               HC1 applies n/(n-k) finite-sample correction (manual sandwich)
        X    = modelmatrix(model)
        e    = residuals(model)
        n, k = size(X)
        bread    = inv(X' * X)
        meat     = X' * (X .* (e .^ 2))
        vcov_hc1 = (n / (n - k)) .* (bread * meat * bread)
        bse      = sqrt.(diag(vcov_hc1))

        rsquared_val     = r2(model)
        rsquared_adj_val = adjr2(model)
        nobs_val         = nobs(model)
        df_resid_val     = dof_residual(model)
        params_names     = coefnames(model)
        params_vals      = coef(model)

        param_dict = Dict{String, Float64}(
            params_names[j] => params_vals[j] for j in 1:length(params_names)
        )
        bse_dict = Dict{String, Float64}(
            params_names[j] => bse[j] for j in 1:length(params_names)
        )

        model_data = Dict(
            "params"       => param_dict,
            "bse"          => bse_dict,
            "rsquared"     => rsquared_val,
            "rsquared_adj" => rsquared_adj_val,
            "nobs"         => nobs_val,
            "df_resid"     => df_resid_val,
        )

        push!(model_results, model_data)

        @printf("       Done - R²=%.4f, N=%d, df_resid=%d\n",
                rsquared_val, nobs_val, df_resid_val)

        if i == 1
            io = IOBuffer()
            show(io, MIME("text/plain"), coeftable(model))
            first_model_summary = String(take!(io))
        end

        acreage_df = nothing
        model = nothing
        GC.gc()
    end

    serialize(JLS_PATH, model_results)

    println("\n[+] Saved $(length(model_results)) model data dicts to:\n    $JLS_PATH")

    println("\n============================================================")
    println("Model 1 Summary (Jl_Imputed_Dataset_1.csv) [Default SEs shown]")
    println("============================================================")
    println(first_model_summary)

    # ---- Step 2: Parameter Pooling ----

    println("\n=== PHASE 4 - STEP 2: PARAMETER POOLING (RUBIN'S RULES) ===")
    println("============================================================\n")

    num_imp = length(model_results)

    all_params_set = Set{String}()
    for r in model_results
        union!(all_params_set, keys(r["params"]))
    end
    all_params = collect(all_params_set)
    sort!(all_params)
    if "(Intercept)" in all_params
        filter!(x -> x != "(Intercept)", all_params)
        insert!(all_params, 1, "(Intercept)")
    end

    num_params = length(all_params)
    coef_mat   = fill(NaN, num_imp, num_params)
    var_mat    = fill(NaN, num_imp, num_params)

    for i in 1:num_imp
        for (j, p) in enumerate(all_params)
            if haskey(model_results[i]["params"], p)
                coef_mat[i, j] = model_results[i]["params"][p]
                var_mat[i, j]  = (model_results[i]["bse"][p])^2
            end
        end
    end

    missing_mask = any(isnan.(coef_mat), dims=1)[1, :]
    if any(missing_mask)
        println("[!] The following parameters were absent in at least one model:")
        for (j, p) in enumerate(all_params)
            if missing_mask[j]
                present_in = sum(.!isnan.(coef_mat[:, j]))
                println("      $p  (present in $present_in/$num_imp models)")
            end
        end
        println()
    end

    # [METHODOLOGY] Rubin's Rules - Barnard & Rubin (1999) df approximation
    m_i = Float64.(vec(sum(.!isnan.(coef_mat), dims=1)))
    m_i .= max.(m_i, 2.0)

    q_bar = zeros(num_params)
    v_w   = zeros(num_params)
    v_b   = zeros(num_params)

    for j in 1:num_params
        valid_idx = .!isnan.(coef_mat[:, j])
        q_bar[j]  = mean(coef_mat[valid_idx, j])
        v_w[j]    = mean(var_mat[valid_idx, j])
        v_b[j]    = var(coef_mat[valid_idx, j], corrected=true)
    end

    v_t    = v_w .+ (1.0 .+ 1.0 ./ m_i) .* v_b
    se     = sqrt.(v_t)

    t_stat  = q_bar ./ se
    lambda_ = (1.0 .+ 1.0 ./ m_i) .* v_b ./ v_t
    df_old  = (m_i .- 1.0) ./ (lambda_ .^ 2)
    df_com  = model_results[1]["df_resid"]
    df_obs  = (df_com + 1.0) / (df_com + 3.0) * df_com .* (1.0 .- lambda_)
    df_adj  = 1.0 ./ (1.0 ./ df_old .+ 1.0 ./ df_obs)

    p_val = 2.0 .* ccdf.(TDist.(df_adj), abs.(t_stat))

    sig_stars = get_stars.(p_val)

    pooled_df = DataFrame(
        Parameter = all_params,
        Coef      = q_bar,
        Std_Error = se,
        t_stat    = t_stat,
        df_adj    = df_adj,
        p_value   = p_val,
        Sig       = sig_stars,
        V_within  = v_w,
        V_between = v_b,
        V_total   = v_t,
        FMI       = lambda_,
    )

    @printf("Pooled OLS Regression Results  (M=%d imputations, Rubin's Rules)\n",
            num_imp)
    println("Formula: Log_Opportunity_Cost ~ Holes + county_type")
    println("Robust variance: HC1 | Sig: *** p<.001  ** p<.01  * p<.05  . p<.1")
    println("-" ^ 70)
    header = @sprintf("%-45s %10s %10s %8s %8s  %s",
                      "Parameter", "Coef", "SE", "t", "p", "Sig")
    println(header)
    println("-" ^ 70)

    for i in 1:nrow(pooled_df)
        @printf("%-45s %10.4f %10.4f %8.3f %8.4f  %s\n",
                pooled_df.Parameter[i],
                pooled_df.Coef[i],
                pooled_df.Std_Error[i],
                pooled_df.t_stat[i],
                pooled_df.p_value[i],
                pooled_df.Sig[i])
    end
    println("-" ^ 70)

    r2_vals  = [r["rsquared"]     for r in model_results]
    r2a_vals = [r["rsquared_adj"] for r in model_results]
    n_vals   = [r["nobs"]         for r in model_results]

    @printf("\nModel diagnostics across %d imputations:\n", num_imp)
    @printf("  R²         : mean=%.4f  min=%.4f  max=%.4f\n",
            mean(r2_vals), minimum(r2_vals), maximum(r2_vals))
    @printf("  Adj. R²    : mean=%.4f  min=%.4f  max=%.4f\n",
            mean(r2a_vals), minimum(r2a_vals), maximum(r2a_vals))
    println("  N per model: [$(join(n_vals, ", "))]")

    CSV.write(OUT_CSV, pooled_df)
    @printf("\n[+] Jl_Regression_Results.csv saved to:\n    %s\n", OUT_CSV)

    println("\n============================================================")
    println("OUTPUT FILES")
    println("============================================================")
    @printf("[+] Model results (JLS) : %s\n", JLS_PATH)
    @printf("[+] Regression table (CSV) : %s\n", OUT_CSV)
    println("\n[DONE] Phase 4 Julia version complete.")
end

if abspath(PROGRAM_FILE) == @__FILE__
    main()
    record_provenance("Phase 4", "Phase_4.jl", SCRIPT_DIR, PROV_START)
end
