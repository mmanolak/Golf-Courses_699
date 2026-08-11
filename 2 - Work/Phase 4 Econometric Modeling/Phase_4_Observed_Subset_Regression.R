# Purpose: Reproduce the "observed-only" Holes coefficient cited in the manuscript
#          (Appendix A.4) as the estimate free of Phase 3's Holes/Ownership_Type
#          MICE-predictor circularity (see Issue_Register.md P1-11). Restricts the
#          Phase 4 regression to courses where BOTH final_acreage and
#          Baseline_Value_Per_Acre were directly observed pre-imputation, so no
#          row in this regression's sample was ever touched by MICE.
#
# Row filter (explicit, applied to the pre-imputation Phase 2 output, not a
# post-hoc flag): acreage_source == "OSM" (final_acreage was matched to a real
# OSM polygon, not MICE-imputed) AND !is.na(Baseline_Value_Per_Acre) (BVPA was
# never missing to begin with). Both conditions are read directly off
# R_Phase2_Acreage_Matched_v2.csv, Phase 3's own input, before any imputation runs.
#
# Model and DV construction intentionally mirror Phase_4.R exactly:
#   Total_Opportunity_Cost = final_acreage * Baseline_Value_Per_Acre
#   Log_Opportunity_Cost   = log1p(Total_Opportunity_Cost)
#   Formula: Log_Opportunity_Cost ~ Holes + factor(county_type)
# The only difference from Phase_4.R is the input: this runs OLS once on the
# doubly-observed subset of the raw Phase 2 output, not pooled across M = 100
# MICE-imputed datasets, because this subset was never imputed to begin with.
#
# Reads:  Phase 2 Spatial Polygons and True Acreage/Data/R/R_Phase2_Acreage_Matched_v2.csv
# Writes: Data/R/R_Observed_Subset_Regression.csv (this script's own output dir)

suppressPackageStartupMessages({
  library(sandwich)
  library(lmtest)
  library(this.path)
})

SCRIPT_DIR <- this.path::this.dir()
INPUT_CSV  <- file.path(
  SCRIPT_DIR, "..", "Phase 2 Spatial Polygons and True Acreage",
  "Data", "R", "R_Phase2_Acreage_Matched_v2.csv"
)
OUT_DIR <- file.path(SCRIPT_DIR, "Data", "R")
OUT_CSV <- file.path(OUT_DIR, "R_Observed_Subset_Regression.csv")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

df <- read.csv(INPUT_CSV, stringsAsFactors = FALSE)
cat(sprintf("Loaded %s: %d rows\n", basename(INPUT_CSV), nrow(df)))
cat("acreage_source counts:\n"); print(table(df$acreage_source, useNA = "always"))
cat(sprintf("Baseline_Value_Per_Acre NA count: %d\n", sum(is.na(df$Baseline_Value_Per_Acre))))

# === Explicit row filter: doubly-observed subset, no MICE imputation involved ===
observed <- df[df$acreage_source == "OSM" & !is.na(df$Baseline_Value_Per_Acre), ]
cat(sprintf(
  "\nDoubly-observed subset (acreage_source == 'OSM' & Baseline_Value_Per_Acre observed): %d rows\n",
  nrow(observed)
))

# === DV construction, identical to Phase_4.R ===
observed$Total_Opportunity_Cost <- observed$final_acreage * observed$Baseline_Value_Per_Acre
observed$Log_Opportunity_Cost   <- log1p(observed$Total_Opportunity_Cost)

cols_needed <- c("Log_Opportunity_Cost", "Holes", "Baseline_Value_Per_Acre", "county_type")
before_n <- nrow(observed)
observed <- observed[complete.cases(observed[, cols_needed]), ]
cat(sprintf(
  "complete.cases on %s dropped %d rows (%d -> %d)\n",
  paste(cols_needed, collapse = ", "), before_n - nrow(observed), before_n, nrow(observed)
))

# === Fit, identical formula to Phase_4.R, HC1 robust SEs for consistency ===
model <- lm(Log_Opportunity_Cost ~ Holes + factor(county_type), data = observed)
robust_se <- coeftest(model, vcov = vcovHC(model, type = "HC1"))

cat("\n=== OBSERVED-SUBSET REGRESSION RESULT ===\n")
cat(sprintf("N = %d\n", nrow(observed)))
print(robust_se)

holes_coef <- coef(model)["Holes"]
holes_se   <- robust_se["Holes", "Std. Error"]
cat(sprintf("\nHoles: beta = %.4f, HC1 SE = %.4f, N = %d\n", holes_coef, holes_se, nrow(observed)))

result_df <- data.frame(
  N = nrow(observed),
  Holes_Coef = holes_coef,
  Holes_SE_HC1 = holes_se,
  Holes_t = holes_coef / holes_se,
  Run_Date = format(Sys.Date())
)
write.csv(result_df, OUT_CSV, row.names = FALSE)
cat(sprintf("\nSaved: %s\n", OUT_CSV))
