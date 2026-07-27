# Purpose: Complete Phase 3 pipeline - MICE imputation (m=100, Random Forest)
#          followed by Rubin's Rules pooling to produce a national land-value
#          point estimate with 95% and 99% CI.
# Inputs:  Phase 2 Spatial Polygons and True Acreage/Data/R/
#            R_Phase2_Acreage_Matched_v2.csv
# Outputs: Data/R/R_Imputed_Dataset_{1..100}.csv
#          Data/R/R_Rubins_Rules_Summary.csv
#          Data/R/R_National_Acreage_Summary.csv
#
# Rubin's Rules formula summary (m = 100 imputations):
#   q_bar = mean(Q_i)           -- pooled point estimate
#   v_w   = mean(var_i)         -- within-imputation variance
#   v_b   = var(Q_i, ddof=1)    -- between-imputation variance
#   v_t   = v_w + v_b + v_b/m  -- total variance
#   se    = sqrt(v_t)
#   99%CI = q_bar +/- 2.576 * se
#   95%CI = q_bar +/- 1.960 * se


# === 1. LIBRARIES ===

# X-08/Decision 1 (2026-07-27): activate the pinned renv project library before
# loading any packages, so this script runs against the versions in renv.lock,
# not whatever happens to be in this machine's personal R library.
local({
  cmd_args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", cmd_args)
  if (length(m) == 0) return(invisible(NULL))
  script_path <- normalizePath(sub("^--file=", "", cmd_args[m]))
  proj_dir <- dirname(dirname(script_path))
  activate_r <- file.path(proj_dir, "renv", "activate.R")
  if (file.exists(activate_r)) {
    Sys.setenv(RENV_PROJECT = proj_dir)
    source(activate_r)
  }
})

suppressPackageStartupMessages({
  library(wooldridge)   # pre-existing dependency - do not remove
  library(tidyverse)
  library(mice)
  library(future)
  library(furrr)
  library(parallelly)
  library(this.path)
})


# === 2. GLOBALS & PATHS ===

SCRIPT_DIR <- this.path::this.dir()

PROV_START <- Sys.time()
source(file.path(SCRIPT_DIR, "..", "provenance.R"))

INPUT_CSV  <- file.path(
  SCRIPT_DIR, "..",
  "Phase 2 Spatial Polygons and True Acreage",
  "Data", "R",
  "R_Phase2_Acreage_Matched_v2.csv"
)
OUT_DIR <- file.path(SCRIPT_DIR, "Data", "R")
OUT_CSV         <- file.path(OUT_DIR, "R_Rubins_Rules_Summary.csv")
OUT_ACREAGE_CSV <- file.path(OUT_DIR, "R_National_Acreage_Summary.csv")

M             <- 100
IMPUTE_COLS   <- c("final_acreage", "Baseline_Value_Per_Acre")

SAFE_WORKERS <- min(availableCores() - 8, 20)
SAFE_WORKERS <- max(SAFE_WORKERS, 1L)
options(future.globals.maxSize = 20 * 1024^3)
plan(multisession, workers = SAFE_WORKERS)

set.seed(42)  # [METHODOLOGY] reproducibility seed for stochastic MICE imputation


# === 3. FUNCTIONS ===

pool_acreage <- function(x) {
  q_bar <- mean(x)
  v_b   <- var(x)
  se    <- sqrt(v_b + v_b / length(x))
  list(
    mean    = q_bar,
    sd_b    = sqrt(v_b),
    ci95_lo = q_bar - 1.960 * se,
    ci95_hi = q_bar + 1.960 * se,
    ci99_lo = q_bar - 2.576 * se,
    ci99_hi = q_bar + 2.576 * se
  )
}


# === 4. EXECUTION ===

cat("\n=== STEP 1: MICE IMPUTATION ===\n")
cat("Loading data from:", INPUT_CSV, "\n")

if (!file.exists(INPUT_CSV)) {
  stop(sprintf(
    "\n[FATAL ERROR] Input file not found:\n  %s\nCheck Phase 2 pipeline.",
    INPUT_CSV
  ))
}

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

acreage_df <- read.csv(INPUT_CSV, stringsAsFactors = FALSE)
cat(
  "Data loaded successfully. Dimensions:",
  nrow(acreage_df), "rows,", ncol(acreage_df), "columns\n"
)

cat("\nColumn names in dataset:\n")
print(names(acreage_df))

cat("\nMissing values check:\n")
cat("final_acreage - Missing:", sum(is.na(acreage_df$final_acreage)), "\n")
cat(
  "Baseline_Value_Per_Acre - Missing:",
  sum(is.na(acreage_df$Baseline_Value_Per_Acre)), "\n"
)

course_col <- if ("Course_Type" %in% names(acreage_df)) {
  "Course_Type"
} else {
  "Ownership_Type"
}

predictors <- c("Holes", course_col, "county_type", "Longitude", "Latitude")

imp_df <- acreage_df[, c(predictors, IMPUTE_COLS)]

# [METHODOLOGY] explicit per-column method vector (P1-01/D-2) - a bare method="rf" string
# applies to every column mice() finds with missing data, which would silently promote
# Holes/Ownership_Type (or any future predictor with a stray NA) to imputation targets
# alongside the two intended ones. Declaring the schema explicitly makes predictors stay
# predictors regardless of what Phase 1 does or doesn't leave missing upstream.
method_vec <- setNames(rep("", ncol(imp_df)), names(imp_df))
method_vec[IMPUTE_COLS] <- "rf"

cat("\nVariables to be imputed:", paste(IMPUTE_COLS, collapse = ", "), "\n")
cat("Predictor variables:", paste(predictors, collapse = ", "), "\n")

cat("\nRunning MICE imputation in PARALLEL...\n")
cat(sprintf(
  "Parameters: m=%d (datasets), method='rf' (Random Forests), seed=42, workers=%d\n",
  M, SAFE_WORKERS
))

# [METHODOLOGY] futuremice - parallel MICE with Random Forest; tree-based to avoid
#               negative predictions and handle mixed predictor types natively
imputed_list <- futuremice(
  data         = imp_df,
  m            = M,
  method       = method_vec,
  parallelseed = 42,
  maxit        = 10
)

cat("\nMICE imputation completed successfully!\n")

cat("\nSaving imputed datasets to:", OUT_DIR, "\n")
for (i in 1:M) {
  complete_data <- complete(imputed_list, i)

  # [METHODOLOGY] floor imputed values at 0 (P3-07) - matches the explicit
  # clip/clamp Python and Julia both apply after complete(); currently a no-op
  # against this RF backend's draws, kept for defensive parity across languages
  complete_data$final_acreage <- pmax(complete_data$final_acreage, 0)
  complete_data$Baseline_Value_Per_Acre <- pmax(complete_data$Baseline_Value_Per_Acre, 0)

  dataset_file  <- file.path(OUT_DIR, sprintf("R_Imputed_Dataset_%d.csv", i))
  write.csv(complete_data, dataset_file, row.names = FALSE)
  cat(sprintf("  Saved: %s\n", dataset_file))
}

cat("\n=== MICE Imputation Complete ===\n")
cat("All", M, "imputed datasets have been saved.\n")

# --- Rubin's Rules Pooling ---

aggregates  <- numeric(M)
within_vars <- numeric(M)

cat("\n=== STEP 2: RUBIN'S RULES POOLING ===\n")
cat("Loading imputed datasets and computing aggregates\n\n")

for (i in 1:M) {
  filepath <- file.path(OUT_DIR, sprintf("R_Imputed_Dataset_%d.csv", i))

  if (!file.exists(filepath)) {
    stop(sprintf("File not found: %s", filepath))
  }

  df <- read_csv(filepath, show_col_types = FALSE)
  df$Total_Opportunity_Cost <-
    df[["final_acreage"]] * df[["Baseline_Value_Per_Acre"]]

  aggregates[i]  <- sum(df$Total_Opportunity_Cost, na.rm = TRUE)
  within_vars[i] <- var(df$Total_Opportunity_Cost, na.rm = TRUE)

  cat(sprintf("  Dataset %d:  $%10.3f B\n", i, aggregates[i] / 1e9))
  rm(df); gc()
}

# [METHODOLOGY] Rubin's Rules pooling - q_bar is the pooled national estimate;
#               v_t combines within- and between-imputation variance (Rubin 1987)
cat("\nApplying Rubin's Rules\n")

q_bar <- mean(aggregates)
v_w   <- mean(within_vars)
v_b   <- var(aggregates)
v_t   <- v_w + v_b + v_b / M
se    <- sqrt(v_t)
ci95_lo <- q_bar - 1.960 * se
ci95_hi <- q_bar + 1.960 * se
ci99_lo <- q_bar - 2.576 * se
ci99_hi <- q_bar + 2.576 * se

cat("\n=== RUBIN'S RULES RESULTS ===\n")
cat(sprintf("  Pooled Aggregate National Value:  $%10.3f B\n", q_bar / 1e9))
cat(sprintf("  Within-Imputation Variance (v_w): %.4e\n",      v_w))
cat(sprintf("  Between-Imputation Variance (v_b):%.4e\n",      v_b))
cat(sprintf("  Total Variance (v_t):             %.4e\n",      v_t))
cat(sprintf("  Standard Error:                   $%10.3f B\n", se / 1e9))
cat(sprintf(
  "  99%% Confidence Interval:         $%10.3f B - $%10.3f B\n",
  ci99_lo / 1e9, ci99_hi / 1e9
))
cat(sprintf(
  "  95%% Confidence Interval:         $%10.3f B - $%10.3f B\n",
  ci95_lo / 1e9, ci95_hi / 1e9
))

pooled_df <- data.frame(
  Metric = c(
    "Pooled Aggregate National Value ($)",
    "Pooled Aggregate National Value ($B)",
    "Within-Imputation Variance (v_w)",
    "Between-Imputation Variance (v_b)",
    "Total Variance (v_t)",
    "Standard Error ($)",
    "99% CI Lower ($B)",
    "99% CI Upper ($B)",
    "95% CI Lower ($B)",
    "95% CI Upper ($B)",
    paste0("Dataset ", 1:M, " Aggregate ($B)")
  ),
  Value = c(
    format(q_bar, big.mark = ",", scientific = FALSE),
    sprintf("%.3f", q_bar / 1e9),
    sprintf("%.4e", v_w),
    sprintf("%.4e", v_b),
    sprintf("%.4e", v_t),
    format(se, big.mark = ",", scientific = FALSE),
    sprintf("%.3f", ci99_lo / 1e9),
    sprintf("%.3f", ci99_hi / 1e9),
    sprintf("%.3f", ci95_lo / 1e9),
    sprintf("%.3f", ci95_hi / 1e9),
    sprintf("%.3f", aggregates / 1e9)
  )
)

write_csv(pooled_df, OUT_CSV)
cat(sprintf("\n[OK] Saved -> %s\n", OUT_CSV))

# --- National Acreage Summary ---

cat("\n=== STEP 3: NATIONAL ACREAGE SUMMARY ===\n")
cat("Computing total U.S. golf course footprint (pooled across imputations)\n\n")

acreage_totals <- numeric(M)
acreage_by_type <- vector("list", M)

for (i in seq_len(M)) {
  filepath <- file.path(OUT_DIR, sprintf("R_Imputed_Dataset_%d.csv", i))
  df_ac    <- read_csv(filepath, show_col_types = FALSE)

  acreage_totals[i] <- sum(df_ac$final_acreage, na.rm = TRUE)

  acreage_by_type[[i]] <- df_ac |>
    group_by(county_type) |>
    summarise(acreage = sum(final_acreage, na.rm = TRUE), .groups = "drop") |>
    mutate(imputation = i)

  cat(sprintf("  Dataset %d:  %s acres  (%s Urban / %s Rural)\n",
    i,
    format(round(acreage_totals[i]), big.mark = ","),
    format(round(filter(acreage_by_type[[i]], county_type == "Urban")$acreage[1]), big.mark = ","),
    format(round(filter(acreage_by_type[[i]], county_type == "Rural")$acreage[1]), big.mark = ",")
  ))
  rm(df_ac); gc()
}

# [METHODOLOGY] Rubin's Rules (acreage) - between-imputation variance only;
#               within-variance is zero for a spatially fixed attribute
nat_pool_ac   <- pool_acreage(acreage_totals)
all_by_type_ac <- bind_rows(acreage_by_type)
type_pool_ac  <- all_by_type_ac |>
  group_by(county_type) |>
  summarise(
    pooled_acres = mean(acreage),
    sd_b         = sd(acreage),
    ci95_lo      = pool_acreage(acreage)$ci95_lo,
    ci95_hi      = pool_acreage(acreage)$ci95_hi,
    ci99_lo      = pool_acreage(acreage)$ci99_lo,
    ci99_hi      = pool_acreage(acreage)$ci99_hi,
    .groups = "drop"
  ) |>
  arrange(desc(pooled_acres))

cat(sprintf("\n  Total U.S. Golf Acreage:  %s acres\n",
  format(round(nat_pool_ac$mean), big.mark = ",")))
cat(sprintf("  Between-Imputation SD:    %.2f\n", nat_pool_ac$sd_b))
cat(sprintf("  99%% CI:                   %s - %s acres\n",
  format(round(nat_pool_ac$ci99_lo), big.mark = ","),
  format(round(nat_pool_ac$ci99_hi), big.mark = ",")
))
cat(sprintf("  95%% CI:                   %s - %s acres\n",
  format(round(nat_pool_ac$ci95_lo), big.mark = ","),
  format(round(nat_pool_ac$ci95_hi), big.mark = ",")
))
for (i in seq_len(nrow(type_pool_ac))) {
  row <- type_pool_ac[i, ]
  cat(sprintf("  %-20s %s acres\n",
    row$county_type,
    format(round(row$pooled_acres), big.mark = ",")
  ))
}

acreage_summary_df <- bind_rows(
  tibble(
    Category          = "National Total",
    County_Type       = "All",
    Pooled_Acres      = round(nat_pool_ac$mean, 2),
    SD_Between        = round(nat_pool_ac$sd_b, 4),
    CI_95_Lower_Acres = round(nat_pool_ac$ci95_lo, 2),
    CI_95_Upper_Acres = round(nat_pool_ac$ci95_hi, 2),
    CI_99_Lower_Acres = round(nat_pool_ac$ci99_lo, 2),
    CI_99_Upper_Acres = round(nat_pool_ac$ci99_hi, 2)
  ),
  type_pool_ac |>
    transmute(
      Category          = "By County Type",
      County_Type       = county_type,
      Pooled_Acres      = round(pooled_acres, 2),
      SD_Between        = round(sd_b, 4),
      CI_95_Lower_Acres = round(ci95_lo, 2),
      CI_95_Upper_Acres = round(ci95_hi, 2),
      CI_99_Lower_Acres = round(ci99_lo, 2),
      CI_99_Upper_Acres = round(ci99_hi, 2)
    )
)

write_csv(acreage_summary_df, OUT_ACREAGE_CSV)
cat(sprintf("  [OK] Saved -> %s\n", basename(OUT_ACREAGE_CSV)))

cat("\n=== PHASE 3 ANALYSIS COMPLETE ===\n")
cat("Output files saved to:", OUT_DIR, "\n")
for (i in 1:M) {
  cat(sprintf("  - R_Imputed_Dataset_%d.csv\n", i))
}
cat("  - R_Rubins_Rules_Summary.csv\n")
cat("  - R_National_Acreage_Summary.csv\n")

record_provenance("Phase 3", "Phase_3.R", SCRIPT_DIR, PROV_START,
                   M = M, maxit = 10, n_workers = SAFE_WORKERS, seed = 42)
