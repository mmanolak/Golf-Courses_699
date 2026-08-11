# Purpose: Generate publication-ready LaTeX tables for the thesis.
#          Three booktabs-styled tables are written as standalone .tex files
#          suitable for \input{} inclusion in a LaTeX document.
#          Table 1: national acreage summary (Urban/Rural/Total).
#          Table 2: MICE-pooled OLS regression results (Rubin's Rules).
#          Table 3: Hawaii golf course parcel distribution by geographic zone.
# Inputs:  Phase 3 Economic Merge and MICE Imputation/Bulk Tests/R/National_Acreage_Summary.csv
#          Phase 4 Econometric Modeling/Data/R/R_Regression_Results.csv
#          Phase 5 The Hawaii Micro-Case Study/Data/R/Phase5_Geographic_Breakdown.csv
# Outputs: Bulk/R/output/8.1_Table1_Acreage.tex
#          Bulk/R/output/8.2_Table2_Regression.tex
#          Bulk/R/output/8.3_Table3_Hawaii_Geo.tex
# LaTeX preamble requirements: booktabs, threeparttable, float


# === 1. LIBRARIES ===

suppressPackageStartupMessages({
    library(tidyverse)
    library(knitr)
    library(kableExtra)
    library(this.path)
})


# === 2. GLOBALS & PATHS ===

SCRIPT_DIR     <- this.path::this.dir()
WORK_DIR       <- normalizePath(
    file.path(SCRIPT_DIR, "..", "..", ".."), mustWork = FALSE
)
ACREAGE_CSV    <- file.path(
    WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
    "Bulk Tests", "R", "National_Acreage_Summary.csv"
)
REGRESSION_CSV <- file.path(
    WORK_DIR, "Phase 4 Econometric Modeling", "Data", "R",
    "R_Regression_Results.csv"
)
HAWAII_CSV     <- file.path(
    WORK_DIR, "Phase 5 The Hawaii Micro-Case Study", "Data", "R",
    "Phase5_Geographic_Breakdown.csv"
)
OUTPUT_DIR     <- file.path(SCRIPT_DIR, "output")
OUT_TEX1       <- file.path(OUTPUT_DIR, "8.1_Table1_Acreage.tex")
OUT_TEX2       <- file.path(OUTPUT_DIR, "8.2_Table2_Regression.tex")
OUT_TEX3       <- file.path(OUTPUT_DIR, "8.3_Table3_Hawaii_Geo.tex")


# === 3. FUNCTIONS ===

# Escape LaTeX special characters in a character vector.
# Backslash is processed first to avoid double-escaping characters introduced
# by the subsequent substitutions.
latex_escape <- function(x) {
    x <- gsub("\\\\",  "\\\\textbackslash{}", x)
    x <- gsub("%",     "\\\\%",               x, fixed = TRUE)
    x <- gsub("\\$",   "\\\\$",               x)
    x <- gsub("_",     "\\\\_",               x, fixed = TRUE)
    x <- gsub("&",     "\\\\&",               x, fixed = TRUE)
    x <- gsub("#",     "\\\\#",               x, fixed = TRUE)
    x <- gsub("\\^",   "\\\\^{}",             x)
    x <- gsub("~",     "\\\\~{}",             x, fixed = TRUE)
    x
}

# Format p-values: values below 0.001 rendered as "$<$ 0.001" (math-mode <),
# all others rounded to 3 decimal places.
fmt_pval <- function(p) {
    ifelse(p < 0.001, "$<$ 0.001", sprintf("%.3f", p))
}


# === 4. EXECUTION ===

cat("\n")
cat(strrep("=", 70), "\n")
cat("Phase 6 - Script 8: LaTeX Table Generation\n")
cat(strrep("=", 70), "\n\n")

dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

# -- Guard inputs
for (f in c(ACREAGE_CSV, REGRESSION_CSV, HAWAII_CSV)) {
    if (!file.exists(f)) {
        stop(sprintf("[FATAL] Input not found:\n  %s", f))
    }
}


# ── 4.1  Table 1: National Acreage Summary ────────────────────────────────────

cat("[Table 1] Formatting national acreage summary...\n")

acreage_raw <- read_csv(ACREAGE_CSV, show_col_types = FALSE)
cat(sprintf("  %d rows loaded.\n", nrow(acreage_raw)))

acreage_tbl <- acreage_raw |>
    select(Category, County_Type, Pooled_Acres, SD_Between,
           CI_99_Lower_Acres, CI_99_Upper_Acres) |>
    mutate(
        Category    = latex_escape(Category),
        County_Type = latex_escape(County_Type),
        # Round to 1 decimal place then apply comma formatting
        across(where(is.numeric), ~ format(
            round(.x, 1), big.mark = ",", nsmall = 1, trim = TRUE
        ))
    )

tbl1 <- kable(
    acreage_tbl,
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    caption   = "National Golf Course Acreage Summary (MICE-Pooled, $M = 100$)",
    label     = "acreage_summary",
    col.names = c(
        "Category", "County Type",
        "Pooled Acres", "SD (Between)",
        "99\\% CI Lower", "99\\% CI Upper"
    )
) |>
    kable_styling(latex_options = c("hold_position")) |>
    footnote(
        general = paste0(
            "99\\% confidence intervals reported; ",
            "95\\% intervals available in the replication package."
        ),
        general_title     = "",
        footnote_as_chunk = TRUE,
        escape            = FALSE
    )

writeLines(as.character(tbl1), OUT_TEX1)
cat(sprintf("  Saved: %s\n\n", basename(OUT_TEX1)))


# ── 4.2  Table 2: Rubin's Rules Regression Results ────────────────────────────

cat("[Table 2] Formatting regression results...\n")

reg_raw <- read_csv(REGRESSION_CSV, show_col_types = FALSE)
cat(sprintf("  %d parameters loaded.\n", nrow(reg_raw)))

reg_tbl <- reg_raw |>
    mutate(
        # Clean up R-style parameter names for academic display
        Parameter = case_when(
            Parameter == "(Intercept)"              ~ "Intercept",
            Parameter == "Holes"                    ~ "Holes",
            Parameter == "factor(county_type)Urban" ~ "Urban County",
            TRUE                                    ~ latex_escape(Parameter)
        ),
        Coef      = sprintf("%.3f", Coef),
        Std_Error = sprintf("%.3f", Std_Error),
        t_stat    = sprintf("%.3f", t_stat),
        df_adj    = sprintf("%.1f", df_adj),
        p_value   = fmt_pval(p_value),
        FMI       = sprintf("%.3f", FMI)
    ) |>
    select(Parameter, Coef, Std_Error, t_stat, df_adj, p_value, Sig, FMI)

cat("  Parameter labels:\n")
for (p in reg_tbl$Parameter) cat(sprintf("    %s\n", p))

tbl2 <- kable(
    reg_tbl,
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    caption   = paste0(
        "MICE-Pooled OLS Regression Results (Rubin's Rules, $M = 100$). ",
        "Dep.\\ var.: $\\log(\\text{Opportunity Cost})$."
    ),
    label     = "regression_results",
    col.names = c(
        "Parameter", "Coef.", "Std.\\ Error",
        "$t$-stat", "$df_{adj}$", "$p$-value", "Sig.", "FMI"
    )
) |>
    kable_styling(latex_options = c("hold_position"), font_size = 10) |>
    footnote(
        general = paste0(
            "Sig.\\ codes: *** $p < 0.001$. ",
            "FMI = Fraction of Missing Information from MICE imputation. ",
            "Covariance terms omitted from delta-method prediction CIs. ",
            "Holes is also a MICE predictor for the imputed acreage/value columns ",
            "that this model's dependent variable is built from (28.8\\% of courses ",
            "imputed for acreage, a further subset for value); the Holes coefficient ",
            "above is estimated on the full sample and is not free of that dependency ",
            "for the imputed rows. See Appendix A.4 for the estimate restricted to ",
            "courses with fully observed (non-imputed) acreage and value."
        ),
        general_title  = "Note: ",
        escape         = FALSE,
        threeparttable = TRUE
    )

writeLines(as.character(tbl2), OUT_TEX2)
cat(sprintf("  Saved: %s\n\n", basename(OUT_TEX2)))


# ── 4.3  Table 3: Hawaii Geographic Breakdown ─────────────────────────────────

cat("[Table 3] Formatting Hawaii geographic breakdown...\n")

hawaii_raw <- read_csv(HAWAII_CSV, show_col_types = FALSE)
cat(sprintf("  %d districts loaded.\n", nrow(hawaii_raw)))

hawaii_tbl <- hawaii_raw |>
    mutate(
        District_Name        = latex_escape(District_Name),
        # Embed \% directly so the percent sign renders correctly in LaTeX
        Pct_of_Total_Parcels = sprintf("%.1f\\%%", Pct_of_Total_Parcels)
    ) |>
    select(Zone_Code, District_Name, Parcel_Count, Pct_of_Total_Parcels)

tbl3 <- kable(
    hawaii_tbl,
    format    = "latex",
    booktabs  = TRUE,
    escape    = FALSE,
    caption   = "Hawaii Golf Course Parcel Distribution by Geographic Zone",
    label     = "hawaii_geo",
    col.names = c("Zone", "District", "Parcels", "Share (\\%)")
) |>
    kable_styling(latex_options = c("hold_position"))

writeLines(as.character(tbl3), OUT_TEX3)
cat(sprintf("  Saved: %s\n\n", basename(OUT_TEX3)))


cat(strrep("=", 70), "\n")
cat("[DONE] Phase 6 - Script 8: All 3 LaTeX tables written.\n\n")
