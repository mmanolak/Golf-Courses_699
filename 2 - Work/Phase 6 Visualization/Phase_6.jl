# X-08/Decision 1 (2026-07-27): pinned environment, not the machine's global one.
import Pkg
Pkg.activate(normpath(joinpath(@__DIR__, "..")); io = devnull)

using Printf

# Purpose: Generate all Julia-based Phase 6 statistical visualization outputs.
#          Runs the complete pipeline end-to-end for scripts 5, 6, and 10-14.
# Inputs:  Phase 4 Econometric Modeling/Data/[python|R|Julia]/[Py|R|Jl]_Regression_Results.csv
#          Phase 2 Spatial Polygons and True Acreage/Data/Julia/Jl_Phase2_Acreage_Matched.csv
#          Phase 3 Economic Merge and MICE Imputation/Data/python/Py_Imputed_Dataset_{1..100}.csv
#          Phase 3 Economic Merge and MICE Imputation/Data/R/R_Imputed_Dataset_{1..100}.csv
#          Phase 3 Economic Merge and MICE Imputation/Data/Julia/Jl_Imputed_Dataset_{1..100}.csv
#          Phase 1 Parsing/Data/Julia/Jl_Phase1_Baseline_Golf_Valuation.csv
# Outputs: output/Final_Thesis_Figures/5.141_Forest_Plot_Combined.png
#          output/Final_Thesis_Figures/5.241-5.245_MICE_Density_Combined_n{020..100}.png  (5 files)
#          output/QA_Verification/5.211-5.215_MICE_Density_Jl_n{020..100}.png             (5 files)
#          output/QA_Verification/5.221-5.225_MICE_Density_Py_n{020..100}.png             (5 files)
#          output/QA_Verification/5.231-5.235_MICE_Density_R_n{020..100}.png              (5 files)
#          output/Final_Thesis_Figures/6.141_Marginal_Effects_Dollar_Value_Combined.png
#          output/Final_Thesis_Figures/6.241_MICE_Raincloud_Diagnostic_Combined.png
#          output/Final_Thesis_Figures/10.141_Hawaii_Gap_Dumbbell_TriLanguage.png
#          output/Final_Thesis_Figures/11.141_Lorenz_Curve_TriLanguage.png
#          output/Final_Thesis_Figures/14.141_Urban_Rural_Scatter_TriLanguage.png
#          output/Final_Thesis_Figures/14.111_Urban_Rural_Scatter_JlVsGrandMean.png
#          output/Final_Thesis_Figures/14.121_Urban_Rural_Scatter_PyVsGrandMean.png
#          output/Final_Thesis_Figures/14.131_Urban_Rural_Scatter_RVsGrandMean.png
#
#          for running the script:
#          julia --threads=auto .\Phase_6.jl


# ---------- Module 5: Econometric Plots ----------
module Mod_5_Econometric_Plots


# === 1. LIBRARIES ===

using CSV, CairoMakie, DataFrames, Printf, Colors


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR  = @__DIR__
const WORK_DIR    = normpath(joinpath(SCRIPT_DIR, ".."))
const PHASE2_CSV  = joinpath(
    WORK_DIR, "Phase 2 Spatial Polygons and True Acreage", "Data", "Julia",
    "Jl_Phase2_Acreage_Matched.csv"
)
const M           = 100
const CHECKPOINTS = [20, 40, 60, 80, 100]
const OUTPUT_DIR  = joinpath(SCRIPT_DIR, "output")
const THESIS_DIR  = joinpath(OUTPUT_DIR, "Final_Thesis_Figures")
const QA_DIR      = joinpath(OUTPUT_DIR, "QA_Verification")
mkpath(THESIS_DIR)
mkpath(QA_DIR)
const OUT_FOREST  = joinpath(THESIS_DIR, "5.141_Forest_Plot_Combined.png")

const PARAM_LABELS = Dict(
    "(Intercept)"        => "Intercept",
    "Holes"              => "Holes \n (per 18-hole unit)",
    "county_type: Urban" => "Urban County"
)

# Colors for UH Manoa
UHM_GREEN = colorant"#024731"   # Green
UHM_GOLD = colorant"#B3995D"    # Gold
UHM_SILVER = colorant"#B2B2B2"  # Silver/Grey
UHM_BLACK = colorant"#000000"   # Black
UHM_WHITE = colorant"#FFFFFF"   # White
OCEAN = colorant"#00758D"       # Darker Cyan
SKY = colorant"#00A4E2"         # Lighter Blue
LEHUA = colorant"#E3002C"       # Red
ILIMA = colorant"#F2A900"       # Dark Yellow
PUA_KENIKENI = colorant"#FAD561"# Dark Gold
KUKUI = colorant"#D6CBAE"       # Beige
AKALA = colorant"#E06E8C"       # Dark Pink
MAO = colorant"#82B53F"         # Dark Lime Green
LAI = colorant"#00846B"         # Royal Green
J_COLOR = colorant"#800080"     # Julia Base Color
R_COLOR = colorant"#008000"     # R Base Color
P_COLOR = colorant"#0000FF"     # Python Base Color

UHM_Palette = (green = UHM_GREEN, gold = UHM_GOLD, silver = UHM_SILVER, ocean = OCEAN, sky = SKY, lehua = LEHUA)

# === 3. FUNCTIONS ===

function plot_forest(py_reg::DataFrame, r_reg::DataFrame, jl_reg::DataFrame, out_path::String)
    labels = map(eachrow(r_reg)) do row
        get(PARAM_LABELS, String(row.Parameter), String(row.Parameter))
    end

    order         = sortperm(r_reg.Coef)
    sorted_labels = labels[order]
    n             = length(order)

    fig = Figure(size = (1050, 450))
    ax  = Axis(fig[1, 1];
        yticks        = (1:n, sorted_labels),
        xlabel        = "Coefficient  [Dependent variable: log(1 + Opportunity_Cost)]",
        title         = "Regression Coefficients - Tri-Language MICE-Pooled Models",
        subtitle      = "Each dot = Rubin-pooled OLS estimate (M=100 imputations). Outer bar = 99% CI · Inner bar = 95% CI.\n" *
                        "Three estimates per predictor, dodged vertically: \n Python (green) · R (blue) · Julia (purple).",
        titlesize = 14, subtitlesize = 11, subtitlecolor = "#024731", xgridvisible = true, ygridvisible = false, tellwidth = false
    )

    vlines!(ax, 0.0; color = "#888888", linestyle = :dash, linewidth = 0.5)

    jl_coef    = jl_reg.Coef[order]
    jl_ci99_lo = (jl_reg.Coef .- 2.576 .* jl_reg.Std_Error)[order]
    jl_ci99_hi = (jl_reg.Coef .+ 2.576 .* jl_reg.Std_Error)[order]
    jl_ci95_lo = (jl_reg.Coef .- 1.960 .* jl_reg.Std_Error)[order]
    jl_ci95_hi = (jl_reg.Coef .+ 1.960 .* jl_reg.Std_Error)[order]
    rangebars!(ax, collect(Float64.(1:n)) .- 0.2, jl_ci99_lo, jl_ci99_hi;
        direction = :x, color = :purple, linewidth = 1.5, whiskerwidth = 0.2)
    rangebars!(ax, collect(Float64.(1:n)) .- 0.2, jl_ci95_lo, jl_ci95_hi;
        direction = :x, color = :purple, linewidth = 0.75, whiskerwidth = 0.15)
    scatter!(ax, jl_coef, collect(Float64.(1:n)) .- 0.2; color = :purple, markersize = 12)

    r_coef    = r_reg.Coef[order]
    r_ci99_lo = (r_reg.Coef .- 2.576 .* r_reg.Std_Error)[order]
    r_ci99_hi = (r_reg.Coef .+ 2.576 .* r_reg.Std_Error)[order]
    r_ci95_lo = (r_reg.Coef .- 1.960 .* r_reg.Std_Error)[order]
    r_ci95_hi = (r_reg.Coef .+ 1.960 .* r_reg.Std_Error)[order]
    rangebars!(ax, collect(Float64.(1:n)), r_ci99_lo, r_ci99_hi;
        direction = :x, color = :blue, linewidth = 1.5, whiskerwidth = 0.2)
    rangebars!(ax, collect(Float64.(1:n)), r_ci95_lo, r_ci95_hi;
        direction = :x, color = :blue, linewidth = 0.75, whiskerwidth = 0.15)
    scatter!(ax, r_coef, collect(Float64.(1:n)); color = :blue, markersize = 12)

    py_coef    = py_reg.Coef[order]
    py_ci99_lo = (py_reg.Coef .- 2.576 .* py_reg.Std_Error)[order]
    py_ci99_hi = (py_reg.Coef .+ 2.576 .* py_reg.Std_Error)[order]
    py_ci95_lo = (py_reg.Coef .- 1.960 .* py_reg.Std_Error)[order]
    py_ci95_hi = (py_reg.Coef .+ 1.960 .* py_reg.Std_Error)[order]
    rangebars!(ax, collect(Float64.(1:n)) .+ 0.2, py_ci99_lo, py_ci99_hi;
        direction = :x, color = :green, linewidth = 1.5, whiskerwidth = 0.2)
    rangebars!(ax, collect(Float64.(1:n)) .+ 0.2, py_ci95_lo, py_ci95_hi;
        direction = :x, color = :green, linewidth = 0.75, whiskerwidth = 0.15)
    scatter!(ax, py_coef, collect(Float64.(1:n)) .+ 0.2; color = :green, markersize = 12)

    Legend(fig[1, 2],
        [
            [MarkerElement(color = :green,  marker = :circle)],
            [MarkerElement(color = :blue,   marker = :circle)],
            [MarkerElement(color = :purple, marker = :circle)],
            [LineElement(color = :gray40, linewidth = 1.5)],
            [LineElement(color = :gray60, linewidth = 0.75)],
        ],
        ["Python", "R", "Julia", "99% CI (outer)", "95% CI (inner)"];
        framevisible = false, labelsize = 11
    )

    Label(fig[2, 1:2],
        "Dependent variable: log(1 + Opportunity_Cost). Each dot is a Rubin-pooled OLS coefficient estimated " *
        "from M = 100 MICE imputations; outer bar = 99% CI, inner bar = 95% CI. " *
        "The three languages use independent MICE backends (LightGBM, Random Forest, Mice.jl); " *
        "near-identical estimates across all three languages demonstrate robustness to imputation backend choice.";
        fontsize = 10, color = "#024731", halign = :left, tellwidth = false, word_wrap = true
    )
    rowsize!(fig.layout, 2, Fixed(40))

    save(out_path, fig; px_per_unit = 3)
    println("    Saved: $(out_path)")
    return fig
end


function plot_density(phase2_df::DataFrame, thesis_dir::String, qa_dir::String)
    round_coords(lon, lat) = round(lon, digits = 4), round(lat, digits = 4)
    missing_mask = ismissing.(phase2_df.osm_acreage)
    missing_locs = Set(round_coords.(phase2_df[missing_mask, :Longitude],
                                    phase2_df[missing_mask, :Latitude]))

    obs_raw   = [x for x in phase2_df.osm_acreage if !ismissing(x) && x > 0]
    obs_log   = log10.(Float64.(obs_raw))
    tick_vals = log10.([1.0, 10.0, 50.0, 200.0, 1000.0, 5000.0])
    tick_strs = ["1 ac", "10 ac", "50 ac", "200 ac", "1,000 ac", "5,000 ac"]

    # [METHODOLOGY] Intentional exception to the Jl_-only input rule: this diagnostic
    # overlay reads all three language MICE sets for visual comparison only - no
    # statistical pooling or inference is performed across languages here.
    # lang_id follows CLAUDE.md convention: 1=Julia, 2=Python, 3=R.
    langs = [("Jl", :purple, "Julia",  1),
            ("Py", :green,  "python", 2),
            ("R",  :blue,   "R",      3)]

    # Pre-collect extracted log-acreage values per language, indexed by dataset number.
    # Raw DataFrames are dropped immediately after metric extraction.
    lang_log_vals = Dict{String, Vector{Vector{Float64}}}()
    for (prefix, _, dir_name, _) in langs
        vals_by_i = Vector{Float64}[]
        base_path = joinpath(WORK_DIR,
            "Phase 3 Economic Merge and MICE Imputation", "Data", dir_name)
        for i in 1:M
            ipath = joinpath(base_path, "$(prefix)_Imputed_Dataset_$(i).csv")
            if isfile(ipath)
                imp_df      = CSV.read(ipath, DataFrame)
                locs        = round_coords.(imp_df.Longitude, imp_df.Latitude)
                target_rows = imp_df[in.(locs, Ref(missing_locs)), :]
                acre_col    = hasproperty(target_rows, :final_acreage) ? :final_acreage : :osm_acreage
                vals        = [x for x in target_rows[!, acre_col] if !ismissing(x) && x > 0]
                push!(vals_by_i, isempty(vals) ? Float64[] : log10.(Float64.(vals)))
                imp_df = nothing
                GC.gc()
            else
                push!(vals_by_i, Float64[])
            end
        end
        lang_log_vals[prefix] = vals_by_i
        @printf("    [%s] Extracted from %d datasets.\n", prefix, length(vals_by_i))
    end

    # Per-language checkpoint plots (QA) - 5 files per language, 15 total → qa_dir
    for (prefix, col, lang_name, lang_id) in langs
        for (ci, n) in enumerate(CHECKPOINTS)
            fig = Figure(size = (1100, 600))
            ax  = Axis(fig[1, 1];
                xticks        = (tick_vals, tick_strs),
                xlabel        = "Final Acreage (log₁₀ scale)",
                ylabel        = "Density",
                title         = "Acreage Distribution - $(lang_name) MICE (Cumulative n = $(n))",
                subtitle      = "Log₁₀ scale  |  Observed = measured parcels  |  Draws 1–$(n)",
                titlesize = 14, subtitlesize = 12, subtitlecolor = "#024731", xgridvisible  = false,
                ygridvisible  = false,
            )
            for i in 1:n
                v = lang_log_vals[prefix][i]
                isempty(v) && continue
                density!(ax, v;
                    color = (col, 0.02), strokecolor = :transparent, strokewidth = 0.0)
            end
            density!(ax, obs_log;
                color = (:black, 0.0), strokecolor = :black, strokewidth = 1.3)
            Legend(fig[1, 2],
                [
                    [LineElement(color = :black, linewidth = 1.3)],
                    [PolyElement(color = (col, 0.5), strokecolor = :transparent)],
                ],
                ["Observed", "$(lang_name) MICE (n = $(n))"];
                framevisible = false, labelsize = 11
            )
            n_str = lpad(n, 3, '0')
            out   = joinpath(qa_dir,
                "5.2$(lang_id)$(ci)_MICE_Density_$(prefix)_n$(n_str).png")
            save(out, fig; px_per_unit = 3)
            println("    Saved: $(out)")
        end
    end

    # Combined checkpoint plots - 5 files → thesis_dir
    for (ci, n) in enumerate(CHECKPOINTS)
        fig = Figure(size = (1100, 600))
        ax  = Axis(fig[1, 1];
            xticks        = (tick_vals, tick_strs),
            xlabel        = "Final Acreage (log₁₀ scale)",
            ylabel        = "Density",
            title         = "Acreage Distribution - Tri-Language MICE (Cumulative n = $(n) per language)",
            subtitle      = "Log₁₀ scale  |  Observed = measured parcels  |  Py (Green), R (Blue), Jl (Purple)",
            titlesize = 14, subtitlesize = 12, subtitlecolor = "#024731",
            xgridvisible  = false,
            ygridvisible  = false,
        )
        for (prefix, col, _, _) in langs
            for i in 1:n
                v = lang_log_vals[prefix][i]
                isempty(v) && continue
                density!(ax, v;
                    color = (col, 0.02), strokecolor = :transparent, strokewidth = 0.0)
            end
        end
        density!(ax, obs_log;
            color = (:black, 0.0), strokecolor = :black, strokewidth = 1.3)
        Legend(fig[1, 2],
            [
                [LineElement(color = :black,                       linewidth = 1.3)],
                [PolyElement(color = (:green,  0.5), strokecolor = :transparent)],
                [PolyElement(color = (:blue,   0.5), strokecolor = :transparent)],
                [PolyElement(color = (:purple, 0.5), strokecolor = :transparent)],
            ],
            ["Observed",
            "Python MICE (n = $(n))",
            "R MICE (n = $(n))",
            "Julia MICE (n = $(n))"];
            framevisible = false, labelsize = 11
        )
        n_str = lpad(n, 3, '0')
        out   = joinpath(thesis_dir,
            "5.24$(ci)_MICE_Density_Combined_n$(n_str).png")
        save(out, fig; px_per_unit = 3)
        println("    Saved: $(out)")
    end
end


# === 4. EXECUTION ===

function main()
    println("\n--- Script 5: Econometric Plots ---\n")

    py_reg_path = joinpath(WORK_DIR, "Phase 4 Econometric Modeling", "Data", "python",
        "Py_Regression_Results.csv")
    r_reg_path  = joinpath(WORK_DIR, "Phase 4 Econometric Modeling", "Data", "R",
        "R_Regression_Results.csv")
    jl_reg_path = joinpath(WORK_DIR, "Phase 4 Econometric Modeling", "Data", "Julia",
        "Jl_Regression_Results.csv")

    for f in [py_reg_path, r_reg_path, jl_reg_path, PHASE2_CSV]
        isfile(f) || error("Input file not found: $f")
    end

    py_reg    = CSV.read(py_reg_path, DataFrame)
    r_reg     = CSV.read(r_reg_path, DataFrame)
    jl_reg    = CSV.read(jl_reg_path, DataFrame)
    phase2_df = CSV.read(PHASE2_CSV, DataFrame)

    println("\n--- Step 1: Rendering Combined Forest Plot ---")
    plot_forest(py_reg, r_reg, jl_reg, OUT_FOREST)

    println("\n--- Step 2: Rendering MICE Density Checkpoint Plots ---")
    plot_density(phase2_df, THESIS_DIR, QA_DIR)

    println("--- Done ---")
end

end # Mod_5_Econometric_Plots


# ---------- Module 6: Advanced Econometric Plots ----------
module Mod_6_Advanced_Econometric_Plots


# === 1. LIBRARIES ===

using CSV, CairoMakie, Colors, DataFrames, Printf, Random, Statistics


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR     = @__DIR__
const WORK_DIR       = normpath(joinpath(SCRIPT_DIR, ".."))
const REGRESSION_CSV_PY = joinpath(
    WORK_DIR, "Phase 4 Econometric Modeling", "Data", "python",
    "Py_Regression_Results.csv"
)
const REGRESSION_CSV_R  = joinpath(
    WORK_DIR, "Phase 4 Econometric Modeling", "Data", "R",
    "R_Regression_Results.csv"
)
const REGRESSION_CSV_JL = joinpath(
    WORK_DIR, "Phase 4 Econometric Modeling", "Data", "Julia",
    "Jl_Regression_Results.csv"
)
const PHASE2_CSV        = joinpath(
    WORK_DIR, "Phase 2 Spatial Polygons and True Acreage", "Data", "Julia",
    "Jl_Phase2_Acreage_Matched.csv"
)
const PHASE3_DIR_PY     = joinpath(
    WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "python"
)
const PHASE3_DIR_R      = joinpath(
    WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "R"
)
const PHASE3_DIR_JL     = joinpath(
    WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "Julia"
)
const M                 = 100
const RAINCLOUD_SEED    = 42
const IMPUTED_PATHS_PY  = [joinpath(PHASE3_DIR_PY, "Py_Imputed_Dataset_$(i).csv") for i in 1:M]
const IMPUTED_PATHS_R   = [joinpath(PHASE3_DIR_R,  "R_Imputed_Dataset_$(i).csv")  for i in 1:M]
const IMPUTED_PATHS_JL  = [joinpath(PHASE3_DIR_JL, "Jl_Imputed_Dataset_$(i).csv") for i in 1:M]
const OUTPUT_DIR        = joinpath(SCRIPT_DIR, "output")
const THESIS_DIR        = joinpath(OUTPUT_DIR, "Final_Thesis_Figures")
mkpath(THESIS_DIR)
const OUT_MARGINAL      = joinpath(THESIS_DIR, "6.141_Marginal_Effects_Dollar_Value_Combined.png")
const OUT_RAINCLOUD     = joinpath(THESIS_DIR, "6.241_MICE_Raincloud_Diagnostic_Combined.png")

const COL_RURAL = "#5a9e51"
const COL_URBAN = "#1a6faf"

# Colors for UH Manoa
UHM_GREEN = colorant"#024731"   # Green
UHM_GOLD = colorant"#B3995D"    # Gold
UHM_SILVER = colorant"#B2B2B2"  # Silver/Grey
UHM_BLACK = colorant"#000000"   # Black
UHM_WHITE = colorant"#FFFFFF"   # White
OCEAN = colorant"#00758D"       # Darker Cyan
SKY = colorant"#00A4E2"         # Lighter Blue
LEHUA = colorant"#E3002C"       # Red
ILIMA = colorant"#F2A900"       # Dark Yellow
PUA_KENIKENI = colorant"#FAD561"# Dark Gold
KUKUI = colorant"#D6CBAE"       # Beige
AKALA = colorant"#E06E8C"       # Dark Pink
MAO = colorant"#82B53F"         # Dark Lime Green
LAI = colorant"#00846B"         # Royal Green
J_COLOR = colorant"#800080"     # Julia Base Color
R_COLOR = colorant"#008000"     # R Base Color
P_COLOR = colorant"#0000FF"     # Python Base Color

UHM_Palette = (green = UHM_GREEN, gold = UHM_GOLD, silver = UHM_SILVER, ocean = OCEAN, sky = SKY, lehua = LEHUA)


# === 3. FUNCTIONS ===

function plot_marginal(marginal_df::DataFrame, med_holes::Float64, out_path::String)
    type_order = ["Rural", "Urban"]
    xs         = [1.2, 1.8]
    colors     = [COL_RURAL, COL_URBAN]

    get_val(type, col) = only(marginal_df[marginal_df.type .== type, col])
    est_vals = [get_val(t, :est_M) for t in type_order]
    lo_vals  = [get_val(t, :lo_M)  for t in type_order]
    hi_vals  = [get_val(t, :hi_M)  for t in type_order]

    fig = Figure(size = (700, 600))
    ax  = Axis(fig[1, 1];
        xticks        = (xs, type_order),
        ylabel        = "Expected Opportunity Cost (USD Millions)",
        title         = "Expected Opportunity Cost by County Type",
        subtitle      = "Grand Mean of Py/R/Jl Rubin-pooled estimates  │  " *
                        "Holes fixed at median = $(Int(round(med_holes)))",
        titlesize     = 14,
        subtitlesize  = 12,
        subtitlecolor = "#024731",
        xgridvisible  = false,
        ygridvisible  = false,
    )

    errorbars!(ax, xs, est_vals, est_vals .- lo_vals, hi_vals .- est_vals;
        direction    = :y,
        color        = colors,
        linewidth    = 1.0,
        whiskerwidth = 18
    )

    # White backing then colored fill replicates a double-outlined-circle point effect.
    scatter!(ax, xs, est_vals; color = :white, markersize = 16, strokewidth = 0)
    scatter!(ax, xs, est_vals; color = colors, markersize = 16,
            strokecolor = colors, strokewidth = 1.8)

    text!(ax, xs, hi_vals;
        text     = [@sprintf("\$%.2fM", v) for v in est_vals],
        offset   = (0, 10),
        align    = (:center, :bottom),
        fontsize = 14,
        font     = :bold,
        color    = colors
    )

    xlims!(ax, 0.7, 2.3)
    ylims!(ax, 0.0, maximum(hi_vals) * 1.25)

    Label(fig[2, 1],
        "Model: log(1 + Opportunity_Cost) = β₀ + β₁·Holes + β₂·I(Urban). " *
        "OC (USD) = exp(ŷ); converted to millions. Grand Mean of Py/R/Jl Rubin-pooled estimates. " *
        "Error bars: 99% CI (delta method; covariance terms omitted).";
        fontsize = 10, color = "#024731", halign = :left, tellwidth = false, word_wrap = true
    )
    rowsize!(fig.layout, 2, Auto())

    save(out_path, fig; px_per_unit = 3)
    println("    Saved: $(basename(out_path))")
    return fig
end


function plot_raincloud(cloud_df::DataFrame, imp_indices::NamedTuple, out_path::String)
    group_levels = [
        "Observed",
        "Python (Imp. $(imp_indices.py))",
        "R (Imp. $(imp_indices.r))",
        "Julia (Imp. $(imp_indices.jl))",
    ]
    group_colors = ["#404040", "#2ca02c", "#1f77b4", "#9467bd"]
    n_groups     = length(group_levels)

    log10_breaks = log10.([1.0, 10.0, 50.0, 200.0, 1000.0, 5000.0])
    log10_labels = ["1", "10", "50", "200", "1,000", "5,000"]

    fig = Figure(size = (1200, 700))
    ax  = Axis(fig[1, 1];
        xticks        = (1:n_groups, group_levels),
        yticks        = (log10_breaks, log10_labels),
        ylabel        = "log₁₀(Acreage) - input to log(1 + Opportunity_Cost)",
        title         = "MICE Imputation Diagnostic - log(1 + Opportunity_Cost) Acreage Inputs",
        subtitle      = "1 imputation per language drawn at seed $(RAINCLOUD_SEED)  │  " *
                        "Py (green), R (blue), Jl (purple)  │  Imputed parcels only",
        titlesize = 14, subtitlesize = 12, subtitlecolor = "#024731", xgridvisible = false, ygridvisible = true,
    )

    # [METHODOLOGY] Fixed seed for reproducible jitter scatter positions.
    Random.seed!(RAINCLOUD_SEED)
    for (gidx, (label, color)) in enumerate(zip(group_levels, group_colors))
        vals = cloud_df[cloud_df.group .== label, :log10_acreage]
        n    = length(vals)
        n == 0 && continue

        # [METHODOLOGY] Half-violin (side=:right) approximates ggdist::stat_halfeye;
        # the flat edge sits at the group x-position, density extends rightward.
        violin!(ax, fill(gidx, n), vals;
            side        = :right,
            width       = 0.55,
            color       = (color, 0.60),
            strokecolor = color,
            strokewidth = 0.5
        )

        boxplot!(ax, fill(gidx, n), vals;
            width        = 0.12,
            color        = (color, 0.55),
            strokecolor  = "#333333",
            strokewidth  = 0.5,
            whiskerwidth = 0.5,
            outliercolor = :transparent
        )

        jitter_x = fill(Float64(gidx), n) .+ randn(n) .* 0.055
        scatter!(ax, jitter_x, vals;
            color      = (color, 0.20),
            markersize = 3
        )
    end

    Label(fig[2, 1],
        "Seed = $(RAINCLOUD_SEED). One imputation randomly selected per language " *
        "(Python imp. $(imp_indices.py), R imp. $(imp_indices.r), Julia imp. $(imp_indices.jl)). " *
        "Observed = measured parcels; imputed rows = parcels with missing osm_acreage in Phase 2.";
        fontsize = 10, color = "#024731", halign = :left, tellwidth = false
    )
    rowsize!(fig.layout, 2, Fixed(36))

    save(out_path, fig; px_per_unit = 3)
    println("    Saved: $(basename(out_path))")
    return fig
end


# === 4. EXECUTION ===

function main()
    println("\n--- Script 6: Advanced Econometric Plots ---\n")

    for f in [REGRESSION_CSV_PY, REGRESSION_CSV_R, REGRESSION_CSV_JL, PHASE2_CSV]
        isfile(f) || error("Input file not found: $f")
    end

    println("\n--- Step 1: Marginal Effects Plot ---")

    # [METHODOLOGY] Rubin-pooled regression results read independently per language;
    # Grand Mean β̂ = arithmetic mean of the three Rubin-pooled β̂ vectors (M=100 each).
    reg_py    = CSV.read(REGRESSION_CSV_PY, DataFrame)
    reg_r     = CSV.read(REGRESSION_CSV_R,  DataFrame)
    reg_jl    = CSV.read(REGRESSION_CSV_JL, DataFrame)
    phase2_df = CSV.read(PHASE2_CSV, DataFrame)

    lookup_coef(df, p) = only(df[df.Parameter .== p, :Coef])
    lookup_se(df, p)   = only(df[df.Parameter .== p, :Std_Error])

    # Parameter names differ per language due to formula encoding in each software.
    # Python: "Intercept", "C(county_type)[T.Urban]"
    # R:      "(Intercept)", "factor(county_type)Urban"
    # Julia:  "(Intercept)", "county_type: Urban"
    # "Holes" is identical across all three.
    grand_mean_coef(p_py, p_r, p_jl) = mean([lookup_coef(reg_py, p_py), lookup_coef(reg_r, p_r), lookup_coef(reg_jl, p_jl)])
    grand_mean_se(p_py, p_r, p_jl)   = mean([lookup_se(reg_py, p_py),   lookup_se(reg_r, p_r),   lookup_se(reg_jl, p_jl)])

    b0       = grand_mean_coef("Intercept",               "(Intercept)",            "(Intercept)")
    b_holes  = grand_mean_coef("Holes",                   "Holes",                  "Holes")
    b_urban  = grand_mean_coef("C(county_type)[T.Urban]", "factor(county_type)Urban","county_type: Urban")
    se_b0    = grand_mean_se("Intercept",               "(Intercept)",            "(Intercept)")
    se_holes = grand_mean_se("Holes",                   "Holes",                  "Holes")
    se_urban = grand_mean_se("C(county_type)[T.Urban]", "factor(county_type)Urban","county_type: Urban")

    med_holes = median([Float64(x) for x in phase2_df.Holes if !ismissing(x)])

    # Predicted log(1 + Opportunity_Cost) at median holes (Rural = no Urban premium).
    # The regression DV is log(1 + Opportunity_Cost) in dollars, so exp(ŷ) is already
    # the predicted OC in dollars (the "+1" is negligible at these magnitudes) -
    # no bvpa multiplication needed or correct.
    log_hat_rural = b0 + b_holes * med_holes
    log_hat_urban = b0 + b_holes * med_holes + b_urban

    # [METHODOLOGY] Prediction SE via delta method (diagonal variance only;
    # off-diagonal covariance terms omitted, matching the R implementation).
    se_pred_rural = sqrt(se_b0^2 + (med_holes * se_holes)^2)
    se_pred_urban = sqrt(se_b0^2 + (med_holes * se_holes)^2 + se_urban^2)

    make_row(log_hat, se_pred, type; z = 2.576) = (
        type  = type,
        est_M = exp(log_hat)               / 1e6,
        lo_M  = exp(log_hat - z * se_pred) / 1e6,
        hi_M  = exp(log_hat + z * se_pred) / 1e6,
    )
    marginal_df = DataFrame([
        make_row(log_hat_rural, se_pred_rural, "Rural"),
        make_row(log_hat_urban, se_pred_urban, "Urban"),
    ])

    @printf("    Median holes = %g\n", med_holes)

    rural_row = only(eachrow(marginal_df[marginal_df.type .== "Rural", :]))
    urban_row = only(eachrow(marginal_df[marginal_df.type .== "Urban", :]))
    @printf("    Predicted OC - Rural: \$%.2fM  [%.2fM, %.2fM]\n",
        rural_row.est_M, rural_row.lo_M, rural_row.hi_M)
    @printf("    Predicted OC - Urban: \$%.2fM  [%.2fM, %.2fM]\n",
        urban_row.est_M, urban_row.lo_M, urban_row.hi_M)

    plot_marginal(marginal_df, med_holes, OUT_MARGINAL)

    println("\n--- Step 2: MICE Raincloud Diagnostic ---")

    # Py and R imputed datasets lack course_id; match by rounded (Lon, Lat) for all three.
    # R uses final_acreage; Py and Jl use osm_acreage.
    round_loc(lon, lat) = (round(Float64(lon), digits = 4), round(Float64(lat), digits = 4))
    missing_locs = Set(
        round_loc(row.Longitude, row.Latitude)
        for row in eachrow(phase2_df[ismissing.(phase2_df.osm_acreage), :])
    )

    rows = Tuple{Float64, String}[]
    for x in phase2_df.osm_acreage
        !ismissing(x) && x > 0 && push!(rows, (log10(Float64(x)), "Observed"))
    end

    # [METHODOLOGY] Each MICE draw provides imputed acreage for courses lacking an OSM
    # polygon (identified by missing osm_acreage in Phase 2). Rows are matched by
    # rounded (Longitude, Latitude) since Py and R datasets have no course_id column.
    # [METHODOLOGY] Fixed seed (RAINCLOUD_SEED) selects exactly 1 imputation per language
    # for reproducibility; jitter positions inside plot_raincloud are also seeded.
    Random.seed!(RAINCLOUD_SEED)
    imp_py = rand(1:M)
    imp_r  = rand(1:M)
    imp_jl = rand(1:M)
    imp_indices = (py = imp_py, r = imp_r, jl = imp_jl)
    @printf("    Raincloud seed = %d  |  Py Imp. %d  |  R Imp. %d  |  Jl Imp. %d\n",
        RAINCLOUD_SEED, imp_py, imp_r, imp_jl)

    for (label, paths, idx, acre_col) in [
        ("Python (Imp. $(imp_py))", IMPUTED_PATHS_PY, imp_py, :osm_acreage),
        ("R (Imp. $(imp_r))",       IMPUTED_PATHS_R,  imp_r,  :final_acreage),
        ("Julia (Imp. $(imp_jl))",  IMPUTED_PATHS_JL, imp_jl, :osm_acreage),
    ]
        isfile(paths[idx]) || error("Input file not found: $(paths[idx])")
        imp_df   = CSV.read(paths[idx], DataFrame)
        locs     = [round_loc(row.Longitude, row.Latitude) for row in eachrow(imp_df)]
        filtered = imp_df[in.(locs, Ref(missing_locs)), :]
        for row in eachrow(filtered)
            v = getproperty(row, acre_col)
            !ismissing(v) && v > 0 && push!(rows, (log10(Float64(v)), label))
        end
        imp_df = nothing
        GC.gc()
    end

    cloud_df = DataFrame(rows, [:log10_acreage, :group])
    n_obs    = count(==("Observed"), cloud_df.group)
    @printf("    Observed: %d rows  |  Py: %d  |  R: %d  |  Jl: %d rows\n",
        n_obs,
        count(==("Python (Imp. $(imp_py))"), cloud_df.group),
        count(==("R (Imp. $(imp_r))"),       cloud_df.group),
        count(==("Julia (Imp. $(imp_jl))"),  cloud_df.group))

    plot_raincloud(cloud_df, imp_indices, OUT_RAINCLOUD)

    println("--- Done ---")
end

end # Mod_6_Advanced_Econometric_Plots


# ---------- Module 10: Hawaii Gap Dumbbell ----------
module Mod_10_Hawaii_Gap_Dumbbell


# === 1. LIBRARIES ===

using CSV, CairoMakie, Colors, DataFrames, Printf, Statistics


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR    = @__DIR__
const WORK_DIR      = normpath(joinpath(SCRIPT_DIR, ".."))
const PHASE1_JL_CSV = joinpath(WORK_DIR, "Phase 1 Parsing", "Data", "Julia",
                                "Jl_Phase1_Baseline_Golf_Valuation.csv")
const PHASE2_JL_CSV = joinpath(WORK_DIR, "Phase 2 Spatial Polygons and True Acreage",
                                "Data", "Julia", "Jl_Phase2_Acreage_Matched.csv")
const PHASE3_DIR_PY = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
                                "Data", "python")
const PHASE3_DIR_R  = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
                                "Data", "R")
const PHASE3_DIR_JL = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
                                "Data", "Julia")
const M            = 100
const OAHU_FIPS    = 15003
const OUTPUT_DIR   = joinpath(SCRIPT_DIR, "output")
const THESIS_DIR   = joinpath(OUTPUT_DIR, "Final_Thesis_Figures")
mkpath(THESIS_DIR)
const OUT_DUMBBELL_LEFT   = joinpath(THESIS_DIR, "10.141_Hawaii_Gap_Dumbbell_Left_TriLanguage.png")
const OUT_DUMBBELL_RIGHT  = joinpath(THESIS_DIR, "10.142_Hawaii_Gap_Dumbbell_Right_TriLanguage.png")
const OUT_DUMBBELL_LEGEND = joinpath(THESIS_DIR, "10.143_Hawaii_Gap_Dumbbell_Legend_TriLanguage.png")

const COL_PY  = "#2ca02c"
const COL_R   = "#1f77b4"
const COL_JL  = "#9467bd"
const COL_OBS = "#404040"

# Colors for UH Manoa
UHM_GREEN = colorant"#024731"   # Green
UHM_GOLD = colorant"#B3995D"    # Gold
UHM_SILVER = colorant"#B2B2B2"  # Silver/Grey
UHM_BLACK = colorant"#000000"   # Black
UHM_WHITE = colorant"#FFFFFF"   # White
OCEAN = colorant"#00758D"       # Darker Cyan
SKY = colorant"#00A4E2"         # Lighter Blue
LEHUA = colorant"#E3002C"       # Red
ILIMA = colorant"#F2A900"       # Dark Yellow
PUA_KENIKENI = colorant"#FAD561"# Dark Gold
KUKUI = colorant"#D6CBAE"       # Beige
AKALA = colorant"#E06E8C"       # Dark Pink
MAO = colorant"#82B53F"         # Dark Lime Green
LAI = colorant"#00846B"         # Royal Green
J_COLOR = colorant"#800080"     # Julia Base Color
R_COLOR = colorant"#008000"     # R Base Color
P_COLOR = colorant"#0000FF"     # Python Base Color

UHM_Palette = (green = UHM_GREEN, gold = UHM_GOLD, silver = UHM_SILVER, ocean = OCEAN, sky = SKY, lehua = LEHUA)


# === 3. FUNCTIONS ===

round_loc(lon, lat) = (round(Float64(lon); digits = 4), round(Float64(lat); digits = 4))

function rubin_pool(vals::Vector{Float64})
    M_local = length(vals)
    M_local == 0 && return (est = NaN, se = NaN)
    q_bar = mean(vals)
    B     = M_local > 1 ? var(vals) : 0.0
    T     = B * (1.0 + 1.0 / M_local)
    (est = q_bar, se = sqrt(max(T, 0.0)))
end


function plot_dumbbell(
    course_names::Vector{String},
    ph2_missing::Vector{Bool},
    left_vals::Vector{Float64},
    right_py::Vector{Float64},  right_py_se::Vector{Float64},
    right_r::Vector{Float64},   right_r_se::Vector{Float64},
    right_jl::Vector{Float64},  right_jl_se::Vector{Float64},
    grand_mean::Vector{Float64},
    out_path::String;
    panel_title::String = ""
)
    n  = length(course_names)
    ys = collect(Float64, 1:n)

    dy_py = +0.20;  dy_r = 0.0;  dy_jl = -0.20

    fig_h = max(600, 26 * n + 220)
    fig   = Figure(size = (1200, fig_h), backgroundcolor = :white)
    ytick_labels = [ph2_missing[i] ? course_names[i] * " †" : course_names[i]
                    for i in 1:n]
    ax    = Axis(fig[1, 1];
        yticks        = (ys, ytick_labels),
        xlabel        = "Estimated Land Value (USD, log₁₀ scale)",
        title         = panel_title,
        titlesize     = 13,
        titlecolor    = "#024731",
        xgridvisible  = true,
        ygridvisible  = false,
    )

    tick_pos  = log10.([1e6, 1e7, 1e8, 1e9, 1e10])
    tick_lbls = ["\$1M", "\$10M", "\$100M", "\$1B", "\$10B"]
    ax.xticks = (tick_pos, tick_lbls)

    # Axis bounds: half-decade below $1M on the left, $20B on the right.
    # CI bars that would extend beyond these limits are clamped (see loop below).
    x_min_ax = log10(5e5)
    x_max_ax = log10(2e10)
    xlims!(ax, x_min_ax, x_max_ax)

    for i in 1:n
        x_left = log10(left_vals[i])
        x_gm   = log10(grand_mean[i])
        y      = ys[i]

        lines!(ax, [x_left, x_gm], [y, y]; color = :gray80, linewidth = 1.5)

        scatter!(ax, [x_left], [y]; color = COL_OBS, marker = :diamond, markersize = 10)

        for (est, se, color, dy) in [
            (right_py[i], right_py_se[i], COL_PY, dy_py),
            (right_r[i],  right_r_se[i],  COL_R,  dy_r),
            (right_jl[i], right_jl_se[i], COL_JL, dy_jl),
        ]
            # Skip languages with no valid estimate (e.g. coord-collision courses).
            isfinite(est) && est > 0 || continue
            x_ctr  = log10(max(est, 1.0))
            # [METHODOLOGY] Delta-method CI propagation from linear to log₁₀ space.
            # CI ends are clamped to [x_min_ax, x_max_ax] before computing half-widths
            # so that courses with high imputation variance (wide Rubin SE) do not
            # produce error bars spanning the entire axis or beyond.
            q_lo   = est - 2.576 * se
            q_hi   = est + 2.576 * se
            log_lo = q_lo > 0.0 ? log10(q_lo) : x_min_ax
            log_hi = q_hi > 0.0 ? log10(q_hi) : x_ctr
            bar_lo = max(x_ctr - max(log_lo, x_min_ax), 0.0)
            bar_hi = max(min(log_hi, x_max_ax) - x_ctr,  0.0)
            errorbars!(ax, [x_ctr], [y + dy], [bar_lo], [bar_hi];
                direction    = :x,
                color        = color,
                linewidth    = 0.9,
                whiskerwidth = 4
            )
            scatter!(ax, [x_ctr], [y + dy]; color = color, markersize = 8)
        end

        scatter!(ax, [x_gm], [y];
            color = :white, markersize = 12,
            strokecolor = :black, strokewidth = 1.5
        )
    end


    save(out_path, fig; px_per_unit = 3)
    println("    Saved: $(basename(out_path))")
end


function plot_legend_card(out_path::String)
    fig = Figure(size = (800, 275), backgroundcolor = :white)

    Label(fig[1, 1:2],
        "The Hawaii Gap: HBU Model Value vs. Agricultural Floor - Oahu Golf Courses";
        fontsize = 20, font = :bold, halign = :center, tellwidth = false
    )
    Label(fig[2, 1:2],
        "Three Rubin-pooled estimates per course (Py/R/Jl, M=100 each)  │  " *
        "Grand Mean = arithmetic mean of three language estimates";
        fontsize = 13, color = "#024731", halign = :center, valign = :bottom, tellwidth = false
    )

    Label(fig[3, 1],
        "Left endpoint: parcel acreage × USDA agricultural value per acre (restricted-use floor). " *
        "Right endpoints: parcel acreage × FHFA residential value per acre (HBU estimate, Rubin-pooled). " *
        "Horizontal gap = Zoning Tax / Deadweight Loss. " *
        "Error bars: 99% CI via Rubin's Rules (delta method, log₁₀ scale); bars capped at axis limits for courses with high imputation variance. " *
        "† No OSM polygon in Phase 2 (military/federal installation or unmapped course); " *
        "acreage fully imputed by MICE - wide CI reflects genuine uncertainty. " *
        "Courses where Py/R coordinate matching is ambiguous show Julia estimate only.";
        fontsize = 11, color = "#024731", halign = :left, valign = :bottom, tellwidth = false, word_wrap = true
    )

    elem_py   = MarkerElement(color = COL_PY,  marker = :circle,  markersize = 9)
    elem_r    = MarkerElement(color = COL_R,   marker = :circle,  markersize = 9)
    elem_jl   = MarkerElement(color = COL_JL,  marker = :circle,  markersize = 9)
    elem_gm   = MarkerElement(color = :white,  marker = :circle,  markersize = 12,
                                strokecolor = :black, strokewidth = 1.5)
    elem_left = MarkerElement(color = COL_OBS, marker = :diamond, markersize = 10)
    Legend(fig[3, 2],
        [elem_py, elem_r, elem_jl, elem_gm, elem_left],
        ["Python  (Rubin M=100)", "R  (Rubin M=100)", "Julia  (Rubin M=100)",
            "Grand Mean", "Agricultural Floor (USDA × acreage)"];
        framevisible = true, halign = :right, valign = :top
    )

    colsize!(fig.layout, 1, Relative(0.65))
    colsize!(fig.layout, 2, Relative(0.35))
    rowsize!(fig.layout, 1, Fixed(30))
    rowsize!(fig.layout, 2, Fixed(24))

    save(out_path, fig; px_per_unit = 3)
    println("    Saved: $(basename(out_path))")
end


# === 4. EXECUTION ===

function main()
    println()
    mkpath(THESIS_DIR)

    isfile(PHASE1_JL_CSV) || error("Input file not found: $PHASE1_JL_CSV")

    println("--- [1/5] Loading Phase 1 baseline for Oahu courses")
    baseline  = CSV.read(PHASE1_JL_CSV, DataFrame)
    oahu      = baseline[coalesce.(baseline.FIPS .== OAHU_FIPS, false), :]
    n_courses = nrow(oahu)
    @printf("    Oahu courses found: %d\n", n_courses)

    course_ids   = Int.(oahu.course_id)
    course_names = String.(oahu.Course_Name)
    usda_vals    = Float64.(oahu.USDA_Ag_Value_Per_Acre)
    fhfa_vals    = Float64.(oahu.FHFA_Res_Value_Per_Acre)
    lons         = Float64.(oahu.Longitude)
    lats         = Float64.(oahu.Latitude)

    id_to_idx    = Dict(course_ids[i] => i for i in 1:n_courses)
    # Collision-safe coordinate lookup: if two courses share the same rounded
    # (Lon, Lat), exclude both from Py/R matching - ambiguity cannot be resolved
    # without a course_id (which only Julia datasets carry).
    _coord_count = Dict{Tuple{Float64,Float64}, Int}()
    for i in 1:n_courses
        key = round_loc(lons[i], lats[i])
        _coord_count[key] = get(_coord_count, key, 0) + 1
    end
    coord_to_idx = Dict{Tuple{Float64,Float64}, Int}(
        round_loc(lons[i], lats[i]) => i
        for i in 1:n_courses
        if _coord_count[round_loc(lons[i], lats[i])] == 1
    )
    let n_coll = count(v -> v > 1, values(_coord_count))
        n_coll > 0 && @printf("    Coordinate collisions (excluded from Py/R): %d location(s)\n", n_coll)
    end

    println("--- [2/5] Reading observed acreage from Jl imputation 1")
    jl1_path = joinpath(PHASE3_DIR_JL, "Jl_Imputed_Dataset_1.csv")
    isfile(jl1_path) || error("Input file not found: $jl1_path")
    jl1_df   = CSV.read(jl1_path, DataFrame)
    obs_acre = zeros(Float64, n_courses)
    for row in eachrow(jl1_df[coalesce.(jl1_df.FIPS .== OAHU_FIPS, false), :])
        idx = get(id_to_idx, Int(row.course_id), 0)
        idx == 0 && continue
        v = row.osm_acreage
        !ismissing(v) && v > 0 && (obs_acre[idx] = Float64(v))
    end
    jl1_df = nothing; GC.gc()

    # Read Phase 2 to flag courses whose acreage was fully MICE-imputed
    # (no OSM polygon - military bases, unmapped clubs, etc.).
    isfile(PHASE2_JL_CSV) || error("Input file not found: $PHASE2_JL_CSV")
    ph2_df       = CSV.read(PHASE2_JL_CSV, DataFrame)
    ph2_oahu     = ph2_df[coalesce.(ph2_df.FIPS .== OAHU_FIPS, false), :]
    ph2_acre_map = Dict{Int, Union{Float64, Missing}}(
        Int(row.course_id) =>
            (ismissing(row.osm_acreage) ? missing : Float64(row.osm_acreage))
        for row in eachrow(ph2_oahu)
    )
    ph2_missing = Bool[ismissing(get(ph2_acre_map, course_ids[i], missing))
                        for i in 1:n_courses]
    ph2_df = nothing; GC.gc()
    @printf("    Courses with missing Phase 2 acreage (MICE-imputed): %d\n", sum(ph2_missing))

    left_vals = obs_acre .* usda_vals

    oc_jl = [Float64[] for _ in 1:n_courses]
    oc_py = [Float64[] for _ in 1:n_courses]
    oc_r  = [Float64[] for _ in 1:n_courses]

    println("--- [3a/5] Pooling Julia (M=$(M))")
    for i in 1:M
        df = CSV.read(joinpath(PHASE3_DIR_JL, "Jl_Imputed_Dataset_$(i).csv"), DataFrame)
        for row in eachrow(df[coalesce.(df.FIPS .== OAHU_FIPS, false), :])
            idx = get(id_to_idx, Int(row.course_id), 0)
            idx == 0 && continue
            v = row.osm_acreage
            !ismissing(v) && v > 0 &&
                push!(oc_jl[idx], Float64(v) * fhfa_vals[idx])
        end
        df = nothing; GC.gc()
    end

    println("--- [3b/5] Pooling Python (M=$(M))")
    for i in 1:M
        df = CSV.read(joinpath(PHASE3_DIR_PY, "Py_Imputed_Dataset_$(i).csv"), DataFrame)
        for row in eachrow(df)
            ismissing(row.FIPS) && continue
            row.FIPS != Float64(OAHU_FIPS) && continue
            loc = round_loc(row.Longitude, row.Latitude)
            idx = get(coord_to_idx, loc, 0)
            idx == 0 && continue
            v = row.osm_acreage
            !ismissing(v) && v > 0 &&
                push!(oc_py[idx], Float64(v) * fhfa_vals[idx])
        end
        df = nothing; GC.gc()
    end

    println("--- [3c/5] Pooling R (M=$(M))")
    for i in 1:M
        df = CSV.read(joinpath(PHASE3_DIR_R, "R_Imputed_Dataset_$(i).csv"), DataFrame)
        for row in eachrow(df)
            loc = round_loc(row.Longitude, row.Latitude)
            idx = get(coord_to_idx, loc, 0)
            idx == 0 && continue
            v = row.final_acreage
            !ismissing(v) && v > 0 &&
                push!(oc_r[idx], Float64(v) * fhfa_vals[idx])
        end
        df = nothing; GC.gc()
    end

    # [METHODOLOGY] Rubin's Rules applied independently per language (M=100 each).
    # Per-course pooled estimate: q̄ = mean(OC_m), T = B(1 + 1/M), SE = √T.
    println("--- [4/5] Applying Rubin's Rules per course per language")
    right_py = zeros(n_courses); right_py_se = zeros(n_courses)
    right_r  = zeros(n_courses); right_r_se  = zeros(n_courses)
    right_jl = zeros(n_courses); right_jl_se = zeros(n_courses)
    grand    = zeros(n_courses)

    for i in 1:n_courses
        rpy = rubin_pool(oc_py[i]);  right_py[i] = rpy.est;  right_py_se[i] = rpy.se
        rr  = rubin_pool(oc_r[i]);   right_r[i]  = rr.est;   right_r_se[i]  = rr.se
        rjl = rubin_pool(oc_jl[i]);  right_jl[i] = rjl.est;  right_jl_se[i] = rjl.se
        valid_ests = filter(x -> !isnan(x) && x > 0, [rpy.est, rr.est, rjl.est])
        grand[i] = isempty(valid_ests) ? NaN : mean(valid_ests)
    end

    # Keep any course with a finite Julia estimate and a positive agricultural
    # floor. Py/R may be NaN for coord-collision courses; those bars are omitted.
    valid = [i for i in 1:n_courses if
        left_vals[i] > 0 && isfinite(right_jl[i]) && isfinite(grand[i])]
    @printf("    Courses plotted: %d / %d\n", length(valid), n_courses)

    ord = valid[sortperm(grand[valid])]   # ascending → largest at top of chart

    short_name(s) = String(first(split(String(s), "-"; limit = 2)))

    half      = length(ord) ÷ 2
    ord_left  = ord[1:half]
    ord_right = ord[half+1:end]

    println("--- [5/7] Building dumbbell panel 1 (lower-value courses)")
    plot_dumbbell(
        [short_name(course_names[i]) for i in ord_left],
        ph2_missing[ord_left],
        left_vals[ord_left],
        right_py[ord_left],   right_py_se[ord_left],
        right_r[ord_left],    right_r_se[ord_left],
        right_jl[ord_left],   right_jl_se[ord_left],
        grand[ord_left],
        OUT_DUMBBELL_LEFT;
        panel_title = "Lower-value courses"
    )

    println("--- [6/7] Building dumbbell panel 2 (higher-value courses)")
    plot_dumbbell(
        [short_name(course_names[i]) for i in ord_right],
        ph2_missing[ord_right],
        left_vals[ord_right],
        right_py[ord_right],   right_py_se[ord_right],
        right_r[ord_right],    right_r_se[ord_right],
        right_jl[ord_right],   right_jl_se[ord_right],
        grand[ord_right],
        OUT_DUMBBELL_RIGHT;
        panel_title = "Higher-value courses"
    )

    println("--- [7/7] Building legend card")
    plot_legend_card(OUT_DUMBBELL_LEGEND)

    println()
    println("=== Mod_10 Hawaii Gap Dumbbell complete ===")
    println()
end

end # Mod_10_Hawaii_Gap_Dumbbell


# ---------- Module 11: Lorenz Curve ----------
module Mod_11_Lorenz_Curve


# === 1. LIBRARIES ===

using CSV, CairoMakie, Colors, DataFrames, Printf, Statistics


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR    = @__DIR__
const WORK_DIR      = normpath(joinpath(SCRIPT_DIR, ".."))
const PHASE1_JL_CSV = joinpath(WORK_DIR, "Phase 1 Parsing", "Data", "Julia",
                                "Jl_Phase1_Baseline_Golf_Valuation.csv")
const PHASE2_JL_CSV = joinpath(WORK_DIR, "Phase 2 Spatial Polygons and True Acreage",
                                "Data", "Julia", "Jl_Phase2_Acreage_Matched.csv")
const PHASE3_DIR_PY = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
                                "Data", "python")
const PHASE3_DIR_R  = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
                                "Data", "R")
const PHASE3_DIR_JL = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
                                "Data", "Julia")
const M             = 100
const GRID_N        = 1001          # Common x-grid for CI interpolation: 0.000, 0.001, …, 1.000
const OUTPUT_DIR    = joinpath(SCRIPT_DIR, "output")
const THESIS_DIR    = joinpath(OUTPUT_DIR, "Final_Thesis_Figures")
const OUT_LORENZ    = joinpath(THESIS_DIR, "11.141_Lorenz_Curve_TriLanguage.png")

const COL_PY = :green
const COL_R  = :blue
const COL_JL = :purple

# Colors for UH Manoa
UHM_GREEN = colorant"#024731"   # Green
UHM_GOLD = colorant"#B3995D"    # Gold
UHM_SILVER = colorant"#B2B2B2"  # Silver/Grey
UHM_BLACK = colorant"#000000"   # Black
UHM_WHITE = colorant"#FFFFFF"   # White
OCEAN = colorant"#00758D"       # Darker Cyan
SKY = colorant"#00A4E2"         # Lighter Blue
LEHUA = colorant"#E3002C"       # Red
ILIMA = colorant"#F2A900"       # Dark Yellow
PUA_KENIKENI = colorant"#FAD561"# Dark Gold
KUKUI = colorant"#D6CBAE"       # Beige
AKALA = colorant"#E06E8C"       # Dark Pink
MAO = colorant"#82B53F"         # Dark Lime Green
LAI = colorant"#00846B"         # Royal Green
J_COLOR = colorant"#800080"     # Julia Base Color
R_COLOR = colorant"#008000"     # R Base Color
P_COLOR = colorant"#0000FF"     # Python Base Color

UHM_Palette = (green = UHM_GREEN, gold = UHM_GOLD, silver = UHM_SILVER, ocean = OCEAN, sky = SKY, lehua = LEHUA)


# === 3. FUNCTIONS ===

round_loc(lon, lat) = (round(Float64(lon); digits = 4), round(Float64(lat); digits = 4))


function interp1(xs::Vector{Float64}, ys::Vector{Float64}, xq::Float64)
    xq <= xs[1]   && return ys[1]
    xq >= xs[end] && return ys[end]
    i = searchsortedlast(xs, xq)
    i >= length(xs) && return ys[end]
    t = (xq - xs[i]) / (xs[i + 1] - xs[i])
    return ys[i] + t * (ys[i + 1] - ys[i])
end


# Build one Lorenz curve interpolated onto `grid` from a vector of per-course OC values.
# Returns a NaN-filled grid if insufficient data.
function lorenz_on_grid(oc_vals::Vector{Float64}, grid::Vector{Float64})
    vals = sort([v for v in oc_vals if isfinite(v) && v > 0.0])
    n    = length(vals)
    n == 0          && return fill(NaN, length(grid))
    total = sum(vals)
    total == 0.0    && return fill(NaN, length(grid))
    cx = vcat(0.0, collect(1:n) ./ n)
    cy = vcat(0.0, cumsum(vals) ./ total)
    return [interp1(cx, cy, gx) for gx in grid]
end


# Accumulate M imputed Lorenz curves onto `grid` (one language at a time, memory-safe).
# Returns (mean_y, lo_y, hi_y) where lo/hi are pointwise 2.5th / 97.5th percentiles
# across the M imputation-specific Lorenz curves.
function lorenz_ci(
    lang_prefix::String,
    phase3_dir::String,
    acreage_col::Symbol,
    use_course_id::Bool,
    id_to_idx::Dict,
    coord_to_idx::Dict,
    fhfa::Vector{Float64},
    n_courses::Int,
    grid::Vector{Float64}
)
    G     = length(grid)
    # Store M rows × G cols; each row is one imputation's Lorenz curve on the grid.
    mat   = Matrix{Float64}(undef, M, G)

    for i in 1:M
        path = joinpath(phase3_dir, "$(lang_prefix)_Imputed_Dataset_$(i).csv")
        isfile(path) || error("Input file not found: $path")
        df   = CSV.read(path, DataFrame)

        oc_i = fill(NaN, n_courses)
        for row in eachrow(df)
            if use_course_id
                idx = get(id_to_idx, Int(row.course_id), 0)
            else
                loc = round_loc(row.Longitude, row.Latitude)
                idx = get(coord_to_idx, loc, 0)
            end
            idx == 0 && continue
            v = getproperty(row, acreage_col)
            !ismissing(v) && Float64(v) > 0.0 || continue
            oc_i[idx] = Float64(v) * fhfa[idx]
        end

        mat[i, :] = lorenz_on_grid(oc_i, grid)
        # [METHODOLOGY] Memory-safe: drop raw DataFrame after metric extraction.
        df = nothing
        GC.gc()
    end

    # Pointwise statistics across M imputation-specific curves.
    # [METHODOLOGY] Rubin's Rules point estimate: q̄ = mean of M per-imputation estimates.
    mean_y = Float64[mean(filter(isfinite, mat[:, j])) for j in 1:G]
    lo_y   = Float64[let col = filter(isfinite, mat[:, j]);
                        isempty(col) ? NaN : quantile(col, 0.025) end for j in 1:G]
    hi_y   = Float64[let col = filter(isfinite, mat[:, j]);
                        isempty(col) ? NaN : quantile(col, 0.975) end for j in 1:G]

    return mean_y, lo_y, hi_y
end


function plot_lorenz(
    grid::Vector{Float64},
    mean_py::Vector{Float64}, lo_py::Vector{Float64}, hi_py::Vector{Float64},
    mean_r::Vector{Float64},  lo_r::Vector{Float64},  hi_r::Vector{Float64},
    mean_jl::Vector{Float64}, lo_jl::Vector{Float64}, hi_jl::Vector{Float64},
    mean_gm::Vector{Float64},
    out_path::String
)
    fig = Figure(size = (880, 880), backgroundcolor = :white)
    ax  = Axis(fig[1, 1];
        xlabel        = "Cumulative Share of Golf Courses (Sorted by Opportunity Cost, Ascending)",
        ylabel        = "Cumulative Share of Total Opportunity Cost",
        title         = "Lorenz Curve of Spatial Misallocation - Opportunity Cost of Golf Courses",
        subtitle      = "Rubin-pooled estimates (M=100 per language) with 95% CI ribbons  │  Grand Mean = black dashed",
        titlesize     = 14,
        subtitlesize  = 12,
        subtitlecolor = "#024731",
        xgridvisible  = true,
        ygridvisible  = true,
        aspect        = 1.0,
        limits        = (0.0, 1.0, 0.0, 1.0),
    )

    pct_ticks = (collect(0.0:0.2:1.0), ["0%", "20%", "40%", "60%", "80%", "100%"])
    ax.xticks = pct_ticks
    ax.yticks = pct_ticks

    # Line of equality
    lines!(ax, [0.0, 1.0], [0.0, 1.0];
        color     = :gray50,
        linestyle = :dot,
        linewidth = 1.5,
        label     = "Line of Equality"
    )

    # 95% CI ribbons (semi-transparent) then mean lines on top
    band!(ax, grid, lo_py, hi_py; color = (COL_PY, 0.15))
    band!(ax, grid, lo_r,  hi_r;  color = (COL_R,  0.15))
    band!(ax, grid, lo_jl, hi_jl; color = (COL_JL, 0.15))

    lines!(ax, grid, mean_py; color = COL_PY, linewidth = 2.0, label = "Python (M=100, 95% CI)")
    lines!(ax, grid, mean_r;  color = COL_R,  linewidth = 2.0, label = "R (M=100, 95% CI)")
    lines!(ax, grid, mean_jl; color = COL_JL, linewidth = 2.0, label = "Julia (M=100, 95% CI)")

    # Grand Mean Lorenz curve - black dashed, no ribbon
    lines!(ax, grid, mean_gm;
        color     = :black,
        linestyle = :dash,
        linewidth = 2.5,
        label     = "Grand Mean"
    )

    axislegend(ax; position = :lt, framevisible = true, labelsize = 11)

    Label(fig[2, 1],
        "OC per course = imputed acreage × FHFA residential value per acre (HBU estimate). " *
        "Rubin's Rules applied independently per language (M=100 each). " *
        "95% CI ribbons = pointwise 2.5th–97.5th percentile across M imputation-specific Lorenz curves. " *
        "Grand Mean = arithmetic mean of three Rubin-pooled per-course OC estimates.";
        fontsize = 10, color = "#024731", halign = :left, tellwidth = false, word_wrap = true
    )
    rowsize!(fig.layout, 2, Fixed(36))

    mkpath(dirname(out_path))
    save(out_path, fig; px_per_unit = 3)
    println("    Saved: $(basename(out_path))")
    return fig
end


# === 4. EXECUTION ===

function main()
    println("\n--- Script 11: Lorenz Curve ---\n")

    isfile(PHASE1_JL_CSV) || error("Input file not found: $PHASE1_JL_CSV")

    println("--- [1/5] Loading Phase 1 baseline")
    baseline  = CSV.read(PHASE1_JL_CSV, DataFrame)
    baseline  = dropmissing(baseline, [:course_id, :FHFA_Res_Value_Per_Acre, :Longitude, :Latitude])
    n_courses = nrow(baseline)
    @printf("    Total courses: %d\n", n_courses)

    course_ids   = Int.(baseline.course_id)
    fhfa         = Float64.(baseline.FHFA_Res_Value_Per_Acre)
    lons         = Float64.(baseline.Longitude)
    lats         = Float64.(baseline.Latitude)
    id_to_idx    = Dict(course_ids[i] => i for i in 1:n_courses)
    # Collision-safe coordinate lookup: if two courses share the same rounded
    # (Lon, Lat), exclude both from Py/R matching - ambiguity cannot be resolved
    # without a course_id (which only Julia datasets carry).
    _coord_count = Dict{Tuple{Float64,Float64}, Int}()
    for i in 1:n_courses
        key = round_loc(lons[i], lats[i])
        _coord_count[key] = get(_coord_count, key, 0) + 1
    end
    coord_to_idx = Dict{Tuple{Float64,Float64}, Int}(
        round_loc(lons[i], lats[i]) => i
        for i in 1:n_courses
        if _coord_count[round_loc(lons[i], lats[i])] == 1
    )
    let n_coll = count(v -> v > 1, values(_coord_count))
        n_coll > 0 && @printf("    Coordinate collisions (excluded from Py/R): %d location(s)\n", n_coll)
    end

    grid = collect(range(0.0, 1.0; length = GRID_N))

    println("--- [2/5] Julia imputations (M=$(M)) - building per-imputation Lorenz curves")
    mean_jl, lo_jl, hi_jl = lorenz_ci(
        "Jl", PHASE3_DIR_JL, :osm_acreage, true,
        id_to_idx, coord_to_idx, fhfa, n_courses, grid
    )

    println("--- [3/5] Python imputations (M=$(M)) - building per-imputation Lorenz curves")
    mean_py, lo_py, hi_py = lorenz_ci(
        "Py", PHASE3_DIR_PY, :osm_acreage, false,
        id_to_idx, coord_to_idx, fhfa, n_courses, grid
    )

    println("--- [4/5] R imputations (M=$(M)) - building per-imputation Lorenz curves")
    mean_r, lo_r, hi_r = lorenz_ci(
        "R", PHASE3_DIR_R, :final_acreage, false,
        id_to_idx, coord_to_idx, fhfa, n_courses, grid
    )

    # [METHODOLOGY] Grand Mean = arithmetic mean of three Rubin-pooled per-course OC
    # Lorenz curves. Computed pointwise on the common grid.
    mean_gm = (mean_py .+ mean_r .+ mean_jl) ./ 3.0

    n_valid = count(isfinite, mean_gm)
    @printf("    Grand Mean grid points with valid data: %d / %d\n", n_valid, GRID_N)

    println("--- [5/5] Rendering Lorenz Curve plot")
    plot_lorenz(
        grid,
        mean_py, lo_py, hi_py,
        mean_r,  lo_r,  hi_r,
        mean_jl, lo_jl, hi_jl,
        mean_gm,
        OUT_LORENZ
    )

    println()
    println("=== Mod_11 Lorenz Curve complete ===")
    println()
end

end # Mod_11_Lorenz_Curve


# ---------- Module 12: Zoning Waffle Chart ----------
module Mod_12_Zoning_Waffle


# === 1. LIBRARIES ===

using CSV, CairoMakie, Colors, DataFrames, Printf


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR     = @__DIR__
const WORK_DIR       = normpath(joinpath(SCRIPT_DIR, ".."))
# Phase 5b canonical Oahu OC (TMK-parcel-intersected; authoritative for the thesis).
const PHASE5B_JL_CSV = joinpath(WORK_DIR, "Phase 5 The Hawaii Micro-Case Study",
                                 "Data", "Julia",  "Jl_Phase5_Oahu_Comparison.csv")
const PHASE5B_PY_CSV = joinpath(WORK_DIR, "Phase 5 The Hawaii Micro-Case Study",
                                 "Data", "python", "Py_Phase5_Oahu_Comparison.csv")
const PHASE5B_R_CSV  = joinpath(WORK_DIR, "Phase 5 The Hawaii Micro-Case Study",
                                 "Data", "R",      "Phase5_Oahu_Comparison.csv")
const ZONING_CSV     = joinpath(WORK_DIR, "Phase 5 The Hawaii Micro-Case Study",
                                 "Data", "Julia",  "Jl_Phase5_Step6_Zoning_Percentages.csv")
const OUTPUT_DIR     = joinpath(SCRIPT_DIR, "output")
const THESIS_DIR     = joinpath(OUTPUT_DIR, "Final_Thesis_Figures")
mkpath(THESIS_DIR)
const OUT_WAFFLE     = joinpath(THESIS_DIR, "12.141_Zoning_Waffle_Chart_TriLanguage.png")
# Diagnostic-pathway constants (national model applied to Oahu, no TMK intersection).
# Produces $30.07B grand mean — ~12.5% above Phase 5b canonical.
# See QA/data/Oahu_OC_Direct_vs_Canonical_Note.md.
# const PHASE1_JL_CSV = joinpath(WORK_DIR, "Phase 1 Parsing", "Data", "Julia",
#                                 "Jl_Phase1_Baseline_Golf_Valuation.csv")
# const PHASE3_DIR_JL = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
#                                 "Data", "Julia")
# const PHASE3_DIR_PY = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
#                                 "Data", "python")
# const PHASE3_DIR_R  = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
#                                 "Data", "R")
# const M = 100
# const OAHU_FIPS = 15003

# Zone-to-group mapping; codes absent from this dict → "Resort / Residential / Other"
const ZONE_GROUP = Dict(
    "P-1"  => "Preservation / Federal",
    "P-2"  => "Preservation / Federal",
    "F-1"  => "Preservation / Federal",
    "AG-1" => "Agriculture",
    "AG-2" => "Agriculture",
)

const ZONE_LABELS = ["Preservation / Federal", "Agriculture", "Resort / Residential / Other"]
const ZONE_COLORS = [:forestgreen, :saddlebrown, :slategray]

# Colors for UH Manoa
UHM_GREEN = colorant"#024731"   # Green
UHM_GOLD = colorant"#B3995D"    # Gold
UHM_SILVER = colorant"#B2B2B2"  # Silver/Grey
UHM_BLACK = colorant"#000000"   # Black
UHM_WHITE = colorant"#FFFFFF"   # White
OCEAN = colorant"#00758D"       # Darker Cyan
SKY = colorant"#00A4E2"         # Lighter Blue
LEHUA = colorant"#E3002C"       # Red
ILIMA = colorant"#F2A900"       # Dark Yellow
PUA_KENIKENI = colorant"#FAD561"# Dark Gold
KUKUI = colorant"#D6CBAE"       # Beige
AKALA = colorant"#E06E8C"       # Dark Pink
MAO = colorant"#82B53F"         # Dark Lime Green
LAI = colorant"#00846B"         # Royal Green
J_COLOR = colorant"#800080"     # Julia Base Color
R_COLOR = colorant"#008000"     # R Base Color
P_COLOR = colorant"#0000FF"     # Python Base Color

UHM_Palette = (green = UHM_GREEN, gold = UHM_GOLD, silver = UHM_SILVER, ocean = OCEAN, sky = SKY, lehua = LEHUA)


# === 3. FUNCTIONS ===

# Read Phase 5b Rubin-pooled q̄ from the language-specific Oahu comparison CSV.
function read_qbar(path::String)
    df  = CSV.read(path, DataFrame)
    row = findfirst(df.Metric .== "Pooled Oahu Opportunity Cost - q_bar (\$B)")
    row === nothing && error("q_bar row not found in $path")
    parse(Float64, replace(string(df[row, :Value]), "," => "")) * 1e9
end

# Diagnostic: national-model-applied-to-Oahu pathway (Phase 3 × FHFA, no TMK
# intersection). Produces $30.07B grand mean; ~12.5% above Phase 5b canonical.
# See QA/data/Oahu_OC_Direct_vs_Canonical_Note.md.
#
# round_loc(lon, lat) = (round(Float64(lon); digits = 4), round(Float64(lat); digits = 4))
#
# function rubin_pool(vals::Vector{Float64})
#     M_local = length(vals)
#     M_local == 0 && return (est = NaN, se = NaN)
#     q_bar = mean(vals)
#     B     = M_local > 1 ? var(vals) : 0.0
#     T     = B * (1.0 + 1.0 / M_local)
#     (est = q_bar, se = sqrt(max(T, 0.0)))
# end
#
# function oahu_oc_totals(lang_prefix, phase3_dir, acreage_col, use_course_id,
#                         id_to_idx, coord_to_idx, fhfa)
#     totals = Float64[]
#     for i in 1:M
#         path = joinpath(phase3_dir, "$(lang_prefix)_Imputed_Dataset_$(i).csv")
#         isfile(path) || error("Input file not found: $path")
#         df = CSV.read(path, DataFrame)
#         oc_m = 0.0
#         for row in eachrow(df)
#             ... (coord/id match to FHFA table; no TMK parcel intersection)
#             oc_m += Float64(v) * fhfa[idx]
#         end
#         push!(totals, oc_m)
#         df = nothing; GC.gc()
#     end
#     return totals
# end


# Group zoning CSV rows into ZONE_LABELS categories.
# Returns (pcts::Vector{Float64}, total_acres::Float64).
function group_zones(zdf::DataFrame)
    acres = Dict(l => 0.0 for l in ZONE_LABELS)
    for row in eachrow(zdf)
        grp = get(ZONE_GROUP, row.zone_class, "Resort / Residential / Other")
        acres[grp] += row.acres
    end
    total = sum(values(acres))
    pcts  = [acres[l] / total * 100.0 for l in ZONE_LABELS]
    return pcts, total
end


# Largest-remainder rounding so tile counts sum to exactly 100.
function pct_to_tiles(pcts::Vector{Float64})
    tiles = floor.(Int, pcts)
    rem   = 100 - sum(tiles)
    fracs = sort([(pcts[i] - tiles[i], i) for i in eachindex(tiles)]; rev = true)
    for k in 1:rem
        tiles[fracs[k][2]] += 1
    end
    return tiles
end


function plot_waffle(
    labels::Vector{String},
    tiles::Vector{Int},
    pcts::Vector{Float64},
    grand_mean_oc::Float64,
    out_path::String
)
    @assert sum(tiles) == 100 "Tile counts must sum to 100"
    categories = String[]
    for (lbl, cnt) in zip(labels, tiles)
        append!(categories, fill(lbl, cnt))
    end
    lbl_to_col = Dict(labels[i] => ZONE_COLORS[i] for i in eachindex(labels))

    fig = Figure(size = (1100, 820), backgroundcolor = :white)
    ax  = Axis(fig[1, 1];
        title         = "The Preservation Paradox - Zoning of Oahu Golf Land",
        subtitle      = "Grand Mean of Py / R / Jl Rubin-pooled estimates  │  M=100 per language",
        titlesize     = 16,
        subtitlesize  = 12,
        subtitlecolor = "#024731",
        aspect        = DataAspect()
    )
    hidedecorations!(ax)
    hidespines!(ax)

    for i in 1:100
        col = cld(i, 10)
        row = (i - 1) % 10 + 1
        poly!(ax, Rect2f(col - 0.45, row - 0.45, 0.9, 0.9);
            color       = lbl_to_col[categories[i]],
            strokecolor = :white,
            strokewidth = 1
        )
    end
    xlims!(ax, 0, 11)
    ylims!(ax, 0, 11)

    oc_B  = grand_mean_oc / 1e9
    elems = [PolyElement(color = c, strokecolor = :transparent) for c in ZONE_COLORS]
    lbl_annot = [
        @sprintf("%s  (%.1f%%  ≈  \$%.1fB OC)", labels[i], pcts[i], oc_B * pcts[i] / 100.0)
        for i in eachindex(labels)
    ]
    Legend(fig[1, 2], elems, lbl_annot;
        orientation  = :vertical,
        framevisible = false,
        labelsize    = 12,
    )

    Label(fig[2, 1],
        @sprintf(
            "Grand Mean total Oahu OC: \$%.2fB  (Phase 5b TMK-parcel-intersected Rubin-pooled estimates, M=100 per language; arithmetic mean of Jl, Py, R). Tile proportions = land-area share by zoning category from Phase 5 parcel data.",
            oc_B
        );
        fontsize = 10, color = "#024731", halign = :left, tellwidth = false, word_wrap = true
    )
    rowsize!(fig.layout, 2, Fixed(44))

    mkpath(dirname(out_path))
    save(out_path, fig; px_per_unit = 3)
    @printf("    Saved: %s\n", basename(out_path))
    return fig
end


# === 4. EXECUTION ===

function main()
    println("\n--- Script 12: Zoning Waffle Chart ---\n")

    isfile(PHASE5B_JL_CSV) || error("Input file not found: $PHASE5B_JL_CSV")
    isfile(PHASE5B_PY_CSV) || error("Input file not found: $PHASE5B_PY_CSV")
    isfile(PHASE5B_R_CSV)  || error("Input file not found: $PHASE5B_R_CSV")
    isfile(ZONING_CSV)     || error("Input file not found: $ZONING_CSV")

    # [METHODOLOGY] Read Phase 5b Rubin-pooled OC estimates (TMK-parcel-intersected).
    # Grand Mean = arithmetic mean of three independently pooled estimates (M=100 each).
    println("--- [1/3] Reading Phase 5b Rubin-pooled Oahu OC estimates")
    oc_jl = read_qbar(PHASE5B_JL_CSV)
    oc_py = read_qbar(PHASE5B_PY_CSV)
    oc_r  = read_qbar(PHASE5B_R_CSV)
    @printf("    Phase 5b Oahu OC - Jl: \$%.3fB  Py: \$%.3fB  R: \$%.3fB\n",
        oc_jl / 1e9, oc_py / 1e9, oc_r / 1e9)
    grand_mean_oc = (oc_jl + oc_py + oc_r) / 3.0
    @printf("    Grand Mean total Oahu OC: \$%.3fB\n", grand_mean_oc / 1e9)

    println("--- [2/3] Loading zoning percentages")
    zdf              = CSV.read(ZONING_CSV, DataFrame)
    pcts, total_acres = group_zones(zdf)
    tiles            = pct_to_tiles(pcts)
    @printf("    Zones - Preservation: %d%%  Agriculture: %d%%  Other: %d%%  (total acres: %.1f)\n",
        tiles[1], tiles[2], tiles[3], total_acres)

    println("--- [3/3] Rendering Zoning Waffle Chart")
    plot_waffle(ZONE_LABELS, tiles, pcts, grand_mean_oc, OUT_WAFFLE)

    println()
    println("=== Mod_12 Zoning Waffle Chart complete ===")
    println()
end

end # Mod_12_Zoning_Waffle


# ---------- Module 13: Counterfactual Area Comparison ----------
module Mod_13_Counterfactual_Area


# === 1. LIBRARIES ===

using CSV, CairoMakie, Colors, DataFrames, Printf, Statistics


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR = @__DIR__
const WORK_DIR   = normpath(joinpath(SCRIPT_DIR, ".."))
const PHASE3_DIR = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data")

const JL_ACREAGE_CSV = joinpath(PHASE3_DIR, "Julia",  "Jl_National_Acreage_Summary.csv")
const PY_ACREAGE_CSV = joinpath(PHASE3_DIR, "python", "Py_National_Acreage_Summary.csv")
const R_ACREAGE_CSV  = joinpath(PHASE3_DIR, "R",      "R_National_Acreage_Summary.csv")

const OUTPUT_DIR  = joinpath(SCRIPT_DIR, "output")
const THESIS_DIR  = joinpath(OUTPUT_DIR, "Final_Thesis_Figures")
mkpath(THESIS_DIR)
const OUT_COUNTER = joinpath(THESIS_DIR, "13.141_Counterfactual_Area_TriLanguage.png")

# Solar reference: EIA Preliminary Monthly Electric Generator Inventory, Aug 2024
# (107.4 GW utility-scale solar in the Lower 48 × 6 ac/MW per SEIA central land-use intensity).
# Source: Peterson & Peterson, "Utility-scale U.S. solar electricity generation continues to grow
# in 2024," Today in Energy, EIA, 2 Oct 2024 (BibTeX: EIA2024, SEIA).
const SOLAR_ACRES   =   644_000.0   # U.S. utility-scale solar capacity footprint, Aug 2024
const HOUSING_ACRES =    50_000.0   # 1M units at 20 units/acre (HUD high-density range 15–30)

# Colors for UH Manoa
UHM_GREEN = colorant"#024731"   # Green
UHM_GOLD = colorant"#B3995D"    # Gold
UHM_SILVER = colorant"#B2B2B2"  # Silver/Grey
UHM_BLACK = colorant"#000000"   # Black
UHM_WHITE = colorant"#FFFFFF"   # White
OCEAN = colorant"#00758D"       # Darker Cyan
SKY = colorant"#00A4E2"         # Lighter Blue
LEHUA = colorant"#E3002C"       # Red
ILIMA = colorant"#F2A900"       # Dark Yellow
PUA_KENIKENI = colorant"#FAD561"# Dark Gold
KUKUI = colorant"#D6CBAE"       # Beige
AKALA = colorant"#E06E8C"       # Dark Pink
MAO = colorant"#82B53F"         # Dark Lime Green
LAI = colorant"#00846B"         # Royal Green
J_COLOR = colorant"#800080"     # Julia Base Color
R_COLOR = colorant"#008000"     # R Base Color
P_COLOR = colorant"#0000FF"     # Python Base Color

UHM_Palette = (green = UHM_GREEN, gold = UHM_GOLD, silver = UHM_SILVER, ocean = OCEAN, sky = SKY, lehua = LEHUA)


# === 3. FUNCTIONS ===

function read_national_total(path::String)
    df   = CSV.read(path, DataFrame)
    mask = coalesce.(df.Category .== "National Total", false) .&
            coalesce.(df.County_Type .== "All", false)
    nrow(df[mask, :]) > 0 || error("National Total / All row not found in $path")
    row = df[mask, :][1, :]
    (
        pooled = Float64(row.Pooled_Acres),
        ci_lo  = Float64(row.CI_99_Lower_Acres),
        ci_hi  = Float64(row.CI_99_Upper_Acres)
    )
end


function plot_comparison(
    jl::NamedTuple, py::NamedTuple, r::NamedTuple,
    gm_pooled::Float64, gm_ci_lo::Float64, gm_ci_hi::Float64,
    out_path::String
)
    M_ACRES = 1_000_000.0   # display in millions of acres

    fig = Figure(size = (1200, 580), backgroundcolor = :white)
    ax  = Axis(fig[1, 1];
        title          = "Magnitude of U.S. Golf Land Against Reference Categories",
        subtitle       = "Grand Mean of Py / R / Jl Rubin-pooled estimates  │  M=100 per language",
        titlesize      = 16,
        subtitlesize   = 12,
        subtitlecolor  = "#024731",
        xlabel         = "Total Land Area (million acres)",
        xlabelsize     = 13,
        yticks         = (1:6, [
            "1M Homes at\n20 units/acre",
            "Golf - Julia (Rubin-pooled)",
            "Golf - Python (Rubin-pooled)",
            "Golf - R (Rubin-pooled)",
            "Golf - Grand Mean",
            "Utility-Scale Solar\n(EIA Aug 2024, Reference)"
        ]),
        yticklabelsize = 11,
        ygridvisible   = true,
        xgridvisible   = true,
    )

    # Dashed reference lines at solar and housing x-positions
    vlines!(ax, [SOLAR_ACRES / M_ACRES];   color = (:goldenrod, 0.35), linewidth = 1.5, linestyle = :dash)
    vlines!(ax, [HOUSING_ACRES / M_ACRES]; color = (:slategray, 0.35), linewidth = 1.5, linestyle = :dash)

    # External reference markers
    scatter!(ax, [SOLAR_ACRES   / M_ACRES], [6]; color = :goldenrod, markersize = 14, marker = :diamond)
    scatter!(ax, [HOUSING_ACRES / M_ACRES], [1]; color = :slategray, markersize = 14, marker = :diamond)
    text!(ax, SOLAR_ACRES   / M_ACRES + 0.08, 6.22;
            text = @sprintf("%.0fK ac", SOLAR_ACRES / 1000.0),
            color = :goldenrod, fontsize = 9, align = (:left, :center))
    text!(ax, HOUSING_ACRES / M_ACRES + 0.08, 1.22;
            text = @sprintf("%.0fk ac", HOUSING_ACRES / 1000.0),
            color = :slategray, fontsize = 9, align = (:left, :center))

    # Grand Mean CI ribbon (shaded rectangle spanning CI lower → upper at y ≈ 5)
    poly!(ax, Rect2f(gm_ci_lo / M_ACRES, 4.55, (gm_ci_hi - gm_ci_lo) / M_ACRES, 0.90);
            color = (:black, 0.12), strokecolor = :transparent)

    # Per-language horizontal CI curves: line segment = 95% CI, circle = point estimate
    for (y_pos, est, ci_lo, ci_hi, col, mk) in [
        (2, jl.pooled, jl.ci_lo, jl.ci_hi, :purple, :circle),
        (3, py.pooled, py.ci_lo, py.ci_hi, :green,  :circle),
        (4, r.pooled,  r.ci_lo,  r.ci_hi,  :blue,   :circle),
        (5, gm_pooled, gm_ci_lo, gm_ci_hi, :black,  :star5)
    ]
        # CI line (the counterfactual area curve for this language)
        lines!(ax, [ci_lo / M_ACRES, ci_hi / M_ACRES], [y_pos, y_pos];
                color = col, linewidth = 3.5)
        # Whisker end caps
        scatter!(ax, [ci_lo / M_ACRES, ci_hi / M_ACRES], [y_pos, y_pos];
                color = col, markersize = 8, marker = :vline)
        # Point estimate marker
        scatter!(ax, [est / M_ACRES], [y_pos];
                color = col, markersize = 14, marker = mk)
        # Value label
        text!(ax, ci_hi / M_ACRES + 0.003, y_pos + 0.16;
            text     = @sprintf("%.4fM", est / M_ACRES),
            color    = col,
            fontsize = 9,
            align    = (:left, :center))
    end

    ylims!(ax, 0.35, 6.65)
    xlims!(ax, -0.10, 3.0)

    Label(fig[2, 1],
        "Solar reference: EIA Aug 2024, 107.4 GW × 6 ac/MW per SEIA (Peterson & Peterson 2024; SEIA n.d.). Housing reference: 1M units at 20 units/acre = 50,000 ac (illustrative density assumption within HUD's 15–30 units/acre high-density range). Golf acreage from MICE-pooled OSM polygon data (Jl/Py: osm_acreage; R: final_acreage). Grand Mean = arithmetic mean of three independently Rubin-pooled national totals.";
        fontsize = 10, color = "#024731", halign = :left, tellwidth = false, word_wrap = true
    )
    rowsize!(fig.layout, 2, Fixed(42))

    mkpath(dirname(out_path))
    save(out_path, fig; px_per_unit = 3)
    @printf("    Saved: %s\n", basename(out_path))
    return fig
end


# === 4. EXECUTION ===

function main()
    println("\n--- Script 13: Counterfactual Area Comparison ---\n")

    isfile(JL_ACREAGE_CSV) || error("Input file not found: $JL_ACREAGE_CSV")
    isfile(PY_ACREAGE_CSV) || error("Input file not found: $PY_ACREAGE_CSV")
    isfile(R_ACREAGE_CSV)  || error("Input file not found: $R_ACREAGE_CSV")

    jl = read_national_total(JL_ACREAGE_CSV)
    py = read_national_total(PY_ACREAGE_CSV)
    r  = read_national_total(R_ACREAGE_CSV)

    @printf("    Pooled national golf acreage:\n")
    @printf("      Julia:      %.0f ac  [%.0f – %.0f]\n", jl.pooled, jl.ci_lo, jl.ci_hi)
    @printf("      Python:     %.0f ac  [%.0f – %.0f]\n", py.pooled, py.ci_lo, py.ci_hi)
    @printf("      R:          %.0f ac  [%.0f – %.0f]\n", r.pooled,  r.ci_lo,  r.ci_hi)

    # [METHODOLOGY] Grand Mean = arithmetic mean of three independently Rubin-pooled acreage estimates.
    gm_pooled = mean([jl.pooled, py.pooled, r.pooled])
    gm_ci_lo  = mean([jl.ci_lo,  py.ci_lo,  r.ci_lo])
    gm_ci_hi  = mean([jl.ci_hi,  py.ci_hi,  r.ci_hi])
    @printf("      Grand Mean: %.0f ac  [%.0f – %.0f]\n", gm_pooled, gm_ci_lo, gm_ci_hi)

    plot_comparison(jl, py, r, gm_pooled, gm_ci_lo, gm_ci_hi, OUT_COUNTER)

    println()
    println("=== Mod_13 Counterfactual Area complete ===")
    println()
end


end # Mod_13_Counterfactual_Area


# ---------- Module 14: Urban-Rural Scatter ----------
module Mod_14_Urban_Rural_Scatter


# === 1. LIBRARIES ===

using CSV, CairoMakie, Colors, DataFrames, Printf, Statistics


# === 2. GLOBALS & PATHS ===

const SCRIPT_DIR    = @__DIR__
const WORK_DIR      = normpath(joinpath(SCRIPT_DIR, ".."))
const PHASE3_DIR    = joinpath(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data")

const PHASE3_DIR_JL = joinpath(PHASE3_DIR, "Julia")
const PHASE3_DIR_PY = joinpath(PHASE3_DIR, "python")
const PHASE3_DIR_R  = joinpath(PHASE3_DIR, "R")

const JL_IMP_CSV    = joinpath(PHASE3_DIR_JL, "Jl_Imputed_Dataset_1.csv")
const PY_IMP_CSV    = joinpath(PHASE3_DIR_PY, "Py_Imputed_Dataset_1.csv")
const R_IMP_CSV     = joinpath(PHASE3_DIR_R,  "R_Imputed_Dataset_1.csv")

const M             = 100

const OUTPUT_DIR    = joinpath(SCRIPT_DIR, "output")
const THESIS_DIR    = joinpath(OUTPUT_DIR, "Final_Thesis_Figures")
mkpath(THESIS_DIR)
const OUT_SCATTER   = joinpath(THESIS_DIR, "14.141_Urban_Rural_Scatter_TriLanguage.png")
const OUT_JL_VS_GM  = joinpath(THESIS_DIR, "14.111_Urban_Rural_Scatter_JlVsGrandMean.png")
const OUT_PY_VS_GM  = joinpath(THESIS_DIR, "14.121_Urban_Rural_Scatter_PyVsGrandMean.png")
const OUT_R_VS_GM   = joinpath(THESIS_DIR, "14.131_Urban_Rural_Scatter_RVsGrandMean.png")

# Colors for UH Manoa
UHM_GREEN = colorant"#024731"   # Green
UHM_GOLD = colorant"#B3995D"    # Gold
UHM_SILVER = colorant"#B2B2B2"  # Silver/Grey
UHM_BLACK = colorant"#000000"   # Black
UHM_WHITE = colorant"#FFFFFF"   # White
OCEAN = colorant"#00758D"       # Darker Cyan
SKY = colorant"#00A4E2"         # Lighter Blue
LEHUA = colorant"#E3002C"       # Red
ILIMA = colorant"#F2A900"       # Dark Yellow
PUA_KENIKENI = colorant"#FAD561"# Dark Gold
KUKUI = colorant"#D6CBAE"       # Beige
AKALA = colorant"#E06E8C"       # Dark Pink
MAO = colorant"#82B53F"         # Dark Lime Green
LAI = colorant"#00846B"         # Royal Green
J_COLOR = colorant"#800080"     # Julia Base Color
R_COLOR = colorant"#008000"     # R Base Color
P_COLOR = colorant"#0000FF"     # Python Base Color

UHM_Palette = (green = UHM_GREEN, gold = UHM_GOLD, silver = UHM_SILVER, ocean = OCEAN, sky = SKY, lehua = LEHUA)


# === 3. FUNCTIONS ===

function load_scatter_data(path::String, acreage_col::Symbol)
    df   = CSV.read(path, DataFrame)
    df   = dropmissing(df, [acreage_col, :Baseline_Value_Per_Acre, :county_type])
    mask = (Float64.(df[!, acreage_col]) .> 0.0) .& (Float64.(df.Baseline_Value_Per_Acre) .> 0.0)
    df   = df[mask, :]
    ac   = Float64.(df[!, acreage_col])
    bv   = Float64.(df.Baseline_Value_Per_Acre)
    (
        log_ac   = log.(ac),
        log_oc   = log.(ac .* bv),
        is_urban = df.county_type .== "Urban"
    )
end


function ols(x::Vector{Float64}, y::Vector{Float64})
    length(x) < 2 && return (intercept = 0.0, slope = 0.0)
    x̄    = mean(x);  ȳ = mean(y)
    Sxx  = sum((x .- x̄).^2)
    slope     = Sxx > 0.0 ? sum((x .- x̄) .* (y .- ȳ)) / Sxx : 0.0
    intercept = ȳ - slope * x̄
    (intercept = intercept, slope = slope)
end


# Compute M=100 OLS envelopes for one language group on the provided xs evaluation grid.
# Returns mean_int, mean_slope (for Grand Mean computation), plus mean_y, lo_y, hi_y where
# lo/hi are pointwise 2.5th–97.5th percentile of predicted log(OC) across M imputations.
function ols_envelope(
    lang_prefix::String,
    phase3_dir::String,
    acreage_col::Symbol,
    xs::Vector{Float64},
    m::Int = M
)
    n          = length(xs)
    pred_mat   = Matrix{Float64}(undef, m, n)
    slopes     = Vector{Float64}(undef, m)
    intercepts = Vector{Float64}(undef, m)

    for i in 1:m
        path = joinpath(phase3_dir, "$(lang_prefix)_Imputed_Dataset_$(i).csv")
        isfile(path) || error("Input file not found: $path")
        df   = CSV.read(path, DataFrame)
        df   = dropmissing(df, [acreage_col, :Baseline_Value_Per_Acre])
        mask = (Float64.(df[!, acreage_col]) .> 0.0) .& (Float64.(df.Baseline_Value_Per_Acre) .> 0.0)
        df   = df[mask, :]
        ac   = Float64.(df[!, acreage_col])
        bv   = Float64.(df.Baseline_Value_Per_Acre)
        fit  = ols(log.(ac), log.(ac .* bv))
        slopes[i]      = fit.slope
        intercepts[i]  = fit.intercept
        pred_mat[i, :] = fit.intercept .+ fit.slope .* xs
        # [METHODOLOGY] Memory-safe: drop raw DataFrame after metric extraction.
        df = nothing
        GC.gc()
    end

    mean_int   = mean(intercepts)
    mean_slope = mean(slopes)
    mean_y     = vec(mean(pred_mat; dims = 1))
    lo_y       = [quantile(pred_mat[:, j], 0.025) for j in 1:n]
    hi_y       = [quantile(pred_mat[:, j], 0.975) for j in 1:n]

    return (mean_int = mean_int, mean_slope = mean_slope, mean_y = mean_y, lo_y = lo_y, hi_y = hi_y)
end


function plot_scatter(
    jl::NamedTuple, py::NamedTuple, r::NamedTuple,
    env_jl::NamedTuple, env_py::NamedTuple, env_r::NamedTuple,
    xs::Vector{Float64},
    out_path::String
)
    fig = Figure(size = (1100, 680), backgroundcolor = :white)
    ax  = Axis(fig[1, 1];
        title         = "The Urban / Rural Bifurcation - log(Opportunity Cost) vs. log(Acreage)",
        subtitle      = "Scatter: imputation #1 per language  │  OLS lines and 95% CI bands from M=100 imputations  │  Grand Mean = black dashed",
        titlesize     = 15,
        subtitlesize  = 12,
        subtitlecolor = "#024731",
        xlabel        = "log(Acreage)",
        ylabel        = "log(Opportunity Cost)",
        xlabelsize    = 13,
        ylabelsize    = 13,
    )

    # [METHODOLOGY] Grand Mean OLS = arithmetic mean of three independently fitted per-language OLS mean estimates.
    gm_int   = mean([env_jl.mean_int,   env_py.mean_int,   env_r.mean_int])
    gm_slope = mean([env_jl.mean_slope, env_py.mean_slope, env_r.mean_slope])
    ys_gm    = gm_int .+ gm_slope .* xs

    for (d, env, col) in [(jl, env_jl, :purple), (py, env_py, :green), (r, env_r, :blue)]
        u = d.is_urban
        scatter!(ax, d.log_ac[u],   d.log_oc[u];
                color = (col, 0.14), markersize = 3.5, marker = :circle)
        scatter!(ax, d.log_ac[.!u], d.log_oc[.!u];
                color = (col, 0.14), markersize = 3.5, marker = :utriangle)
        band!(ax, xs, env.lo_y, env.hi_y; color = (col, 0.18))
        lines!(ax, xs, env.mean_y; color = col, linewidth = 2.5)
    end

    lines!(ax, xs, ys_gm; color = :black, linewidth = 3.2, linestyle = :dash)

    elem_urban = MarkerElement(color = :gray40, marker = :circle,    markersize = 9)
    elem_rural = MarkerElement(color = :gray40, marker = :utriangle, markersize = 9)
    elem_jl    = LineElement(color = :purple, linewidth = 2.5)
    elem_py    = LineElement(color = :green,  linewidth = 2.5)
    elem_r_ln  = LineElement(color = :blue,   linewidth = 2.5)
    elem_gm    = LineElement(color = :black,  linewidth = 3.2, linestyle = :dash)
    Legend(fig[1, 2],
        [elem_urban, elem_rural, elem_jl, elem_py, elem_r_ln, elem_gm],
        ["Urban (RUCC 1–3)", "Rural (RUCC 4–9)",
         "Julia OLS (M=100)", "Python OLS (M=100)", "R OLS (M=100)", "Grand Mean OLS"];
        framevisible = true, labelsize = 11, rowgap = 5
    )

    Label(fig[2, 1:2],
        "Scatter: imputation #1 per language. OLS line = mean slope/intercept across M=100 imputations; " *
        "CI band = pointwise 2.5th–97.5th percentile of predicted log(OC) across M imputations. " *
        "Jl/Py: osm_acreage; R: final_acreage. Urban/Rural from county_type. " *
        "Grand Mean = arithmetic mean of three per-language OLS mean fits.";
        fontsize = 10, color = "#024731", halign = :left, tellwidth = false, word_wrap = true
    )
    rowsize!(fig.layout, 2, Fixed(42))

    mkpath(dirname(out_path))
    save(out_path, fig; px_per_unit = 3)
    @printf("    Saved: %s\n", basename(out_path))
    return fig
end


function plot_lang_vs_gm(
    d::NamedTuple,
    env::NamedTuple,
    gm_int::Float64,
    gm_slope::Float64,
    xs::Vector{Float64},
    lang_label::String,
    lang_color::Symbol,
    out_path::String
)
    fig = Figure(size = (1100, 680), backgroundcolor = :white)
    ax  = Axis(fig[1, 1];
        title         = "Urban/Rural Bifurcation - $(lang_label) vs. Grand Mean OLS",
        subtitle      = "$(lang_label) OLS mean line and 95% CI band (M=100)  │  Grand Mean OLS = black dashed",
        titlesize     = 15,
        subtitlesize  = 12,
        subtitlecolor = "#024731",
        xlabel        = "log(Acreage)",
        ylabel        = "log(Opportunity Cost)",
        xlabelsize    = 13,
        ylabelsize    = 13,
    )

    u = d.is_urban
    scatter!(ax, d.log_ac[u],   d.log_oc[u];
            color = (lang_color, 0.14), markersize = 3.5, marker = :circle)
    scatter!(ax, d.log_ac[.!u], d.log_oc[.!u];
            color = (lang_color, 0.14), markersize = 3.5, marker = :utriangle)

    band!(ax, xs, env.lo_y, env.hi_y; color = (lang_color, 0.25))
    lines!(ax, xs, env.mean_y; color = lang_color, linewidth = 2.5)

    ys_gm = gm_int .+ gm_slope .* xs
    lines!(ax, xs, ys_gm; color = :black, linewidth = 3.2, linestyle = :dash)

    elem_urban = MarkerElement(color = :gray40,    marker = :circle,    markersize = 9)
    elem_rural = MarkerElement(color = :gray40,    marker = :utriangle, markersize = 9)
    elem_lang  = LineElement(color = lang_color,   linewidth = 2.5)
    elem_ci    = PolyElement(color = (lang_color, 0.25))
    elem_gm    = LineElement(color = :black,       linewidth = 3.2, linestyle = :dash)
    Legend(fig[1, 2],
        [elem_urban, elem_rural, elem_lang, elem_ci, elem_gm],
        ["Urban (RUCC 1–3)", "Rural (RUCC 4–9)",
         "$(lang_label) OLS (M=100 mean)", "95% CI band", "Grand Mean OLS"];
        framevisible = true, labelsize = 11, rowgap = 5
    )

    Label(fig[2, 1:2],
        "Scatter: imputation #1. OLS line = $(lang_label) mean slope/intercept across M=100 imputations; " *
        "CI band = pointwise 2.5th–97.5th percentile of predicted log(OC). " *
        "Grand Mean OLS = arithmetic mean of Julia, Python, and R per-language mean slopes and intercepts.";
        fontsize = 10, color = "#024731", halign = :left, tellwidth = false, word_wrap = true
    )
    rowsize!(fig.layout, 2, Fixed(42))

    mkpath(dirname(out_path))
    save(out_path, fig; px_per_unit = 3)
    @printf("    Saved: %s\n", basename(out_path))
    return fig
end


# === 4. EXECUTION ===

function main()
    println("\n--- Script 14: Urban-Rural Scatter ---\n")

    isfile(JL_IMP_CSV) || error("Input file not found: $JL_IMP_CSV")
    isfile(PY_IMP_CSV) || error("Input file not found: $PY_IMP_CSV")
    isfile(R_IMP_CSV)  || error("Input file not found: $R_IMP_CSV")

    println("--- [1/7] Loading imputation #1 scatter data")
    jl = load_scatter_data(JL_IMP_CSV, :osm_acreage)
    py = load_scatter_data(PY_IMP_CSV, :osm_acreage)
    r  = load_scatter_data(R_IMP_CSV,  :final_acreage)

    @printf("    Courses loaded - Jl: %d  Py: %d  R: %d\n",
            length(jl.log_ac), length(py.log_ac), length(r.log_ac))

    x_min = min(minimum(jl.log_ac), minimum(py.log_ac), minimum(r.log_ac))
    x_max = max(maximum(jl.log_ac), maximum(py.log_ac), maximum(r.log_ac))
    xs    = collect(range(x_min, x_max; length = 300))

    println("--- [2/7] Julia M=100 OLS envelopes")
    env_jl = ols_envelope("Jl", PHASE3_DIR_JL, :osm_acreage,   xs)

    println("--- [3/7] Python M=100 OLS envelopes")
    env_py = ols_envelope("Py", PHASE3_DIR_PY, :osm_acreage,   xs)

    println("--- [4/7] R M=100 OLS envelopes")
    env_r  = ols_envelope("R",  PHASE3_DIR_R,  :final_acreage, xs)

    # [METHODOLOGY] Grand Mean OLS = arithmetic mean of three independently fitted per-language OLS mean estimates.
    gm_int   = mean([env_jl.mean_int,   env_py.mean_int,   env_r.mean_int])
    gm_slope = mean([env_jl.mean_slope, env_py.mean_slope, env_r.mean_slope])
    @printf("    Grand Mean OLS - intercept: %.4f  slope: %.4f\n", gm_int, gm_slope)

    println("--- [5/7] Rendering 14.141 tri-language scatter with CI bands")
    plot_scatter(jl, py, r, env_jl, env_py, env_r, xs, OUT_SCATTER)

    println("--- [6/7] Rendering per-language vs Grand Mean scatter plots")
    plot_lang_vs_gm(jl, env_jl, gm_int, gm_slope, xs, "Julia",  :purple, OUT_JL_VS_GM)
    plot_lang_vs_gm(py, env_py, gm_int, gm_slope, xs, "Python", :green,  OUT_PY_VS_GM)
    plot_lang_vs_gm(r,  env_r,  gm_int, gm_slope, xs, "R",      :blue,   OUT_R_VS_GM)

    println("--- [7/7] Done")
    println()
    println("=== Mod_14 Urban-Rural Scatter complete ===")
    println()
end


end # Mod_14_Urban_Rural_Scatter


# === EXECUTION ===
# All five modules are independent (separate inputs, separate outputs).
# Threads.@spawn launches each on its own thread; fetch() blocks until all finish.
# Launch with: julia --threads=auto .\Phase_6.jl
# On a 12-core 3900XT, --threads=auto will use all available logical threads.

function main()
    println("\n=== Phase 6 Visualization ===")
    @printf("    Julia threads available: %d\n", Threads.nthreads())
    if Threads.nthreads() == 1
        @warn "Running on 1 thread - parallel speedup disabled. " *
                "Relaunch with: julia --threads=auto .\\Phase_6.jl"
    end
    println()

    t5  = Threads.@spawn Mod_5_Econometric_Plots.main()
    t6  = Threads.@spawn Mod_6_Advanced_Econometric_Plots.main()
    t10 = Threads.@spawn Mod_10_Hawaii_Gap_Dumbbell.main()
    t11 = Threads.@spawn Mod_11_Lorenz_Curve.main()
    t12 = Threads.@spawn Mod_12_Zoning_Waffle.main()
    t13 = Threads.@spawn Mod_13_Counterfactual_Area.main()
    t14 = Threads.@spawn Mod_14_Urban_Rural_Scatter.main()

    fetch(t5)
    fetch(t6)
    fetch(t10)
    fetch(t11)
    fetch(t12)
    fetch(t13)
    fetch(t14)

    GC.gc()
    println("\n=== All Phase 6 modules complete ===\n")
end

main()
