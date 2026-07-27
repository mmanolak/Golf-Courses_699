# Purpose: Phase 4 master - fit OLS with HC1 robust SEs on each of M R-generated
#          MICE imputed datasets, then pool via Rubin's Rules and save results.
# Inputs:  Phase 3 Economic Merge and MICE Imputation/Data/R/R_Imputed_Dataset_{1..M}.csv
# Outputs: Data/R/R_model_results.rds
#          Data/R/R_Regression_Results.csv


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
  library(wooldridge)  # pre-existing dependency - do not remove
  library(tidyverse)
  library(lmtest)
  library(sandwich)
  library(broom)       # pre-existing dependency - do not remove
  library(this.path)
})


# === 2. GLOBALS & PATHS ===

SCRIPT_DIR  <- this.path::this.dir()

PROV_START <- Sys.time()
source(file.path(SCRIPT_DIR, "..", "provenance.R"))

PHASE3_DIR  <- file.path(
  SCRIPT_DIR, "..", "Phase 3 Economic Merge and MICE Imputation", "Data", "R"
)
OUT_DIR     <- file.path(SCRIPT_DIR, "Data", "R")
MODEL_RDS   <- file.path(OUT_DIR, "R_model_results.rds")
OUT_CSV     <- file.path(OUT_DIR, "R_Regression_Results.csv")

FORMULA_STR   <- "Log_Opportunity_Cost ~ Holes + factor(county_type)"
M             <- 100
IMPUTED_PATHS <- file.path(
  PHASE3_DIR,
  paste0("R_Imputed_Dataset_", seq_len(M), ".csv")
)


# === 3. FUNCTIONS ===

#' Return significance stars for a vector of p-values.
#'
#' @param p Numeric vector of p-values.
#' @return Character vector of star strings.
stars <- function(p) {
  sapply(p, function(x) {
    if (is.na(x))  return("")
    if (x < 0.001) return("***")
    if (x < 0.01)  return("**")
    if (x < 0.05)  return("*")
    if (x < 0.1)   return(".")
    ""
  })
}


# === 4. EXECUTION ===

if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

missing_files <- IMPUTED_PATHS[!file.exists(IMPUTED_PATHS)]
if (length(missing_files) > 0) {
  stop(paste(
    c("[FATAL] Imputed datasets not found in Phase 3 directory:", missing_files),
    collapse = "\n  "
  ))
}

cat(sprintf(
  "Phase 4 initialized. Ready to process %d datasets.\n",
  length(IMPUTED_PATHS)
))

# ---- Step 1: Model Fitting ----

cat("============================================================\n")
cat("STEP 1: MODEL FITTING (OLS with HC1 robust SE)\n")
cat("============================================================\n")

model_results       <- list()
first_model_summary <- NULL

for (i in seq_along(IMPUTED_PATHS)) {
  path  <- IMPUTED_PATHS[i]
  fname <- basename(path)
  cat(sprintf("[%d/%d] Loading %s...\n", i, length(IMPUTED_PATHS), fname))

  acreage_df <- read.csv(path, stringsAsFactors = FALSE)

  if (!"final_acreage" %in% names(acreage_df)) {
    stop(sprintf("Column 'final_acreage' not found in %s.", fname))
  }
  if (!"Baseline_Value_Per_Acre" %in% names(acreage_df)) {
    stop(sprintf("Column 'Baseline_Value_Per_Acre' not found in %s.", fname))
  }

  acreage_df$Total_Opportunity_Cost <- (
    acreage_df$final_acreage * acreage_df$Baseline_Value_Per_Acre
  )
  acreage_df$Log_Opportunity_Cost <- log1p(acreage_df$Total_Opportunity_Cost)

  cols_needed <- c(
    "Log_Opportunity_Cost", "Holes", "Baseline_Value_Per_Acre", "county_type"
  )
  n_before   <- nrow(acreage_df)
  acreage_df <- acreage_df[complete.cases(acreage_df[, cols_needed]), ]
  n_dropped  <- n_before - nrow(acreage_df)

  if (n_dropped > 0) {
    cat(sprintf(
      "       Dropped %d rows with missing values in model columns.\n",
      n_dropped
    ))
  }

  model <- lm(as.formula(FORMULA_STR), data = acreage_df)  # [METHODOLOGY] OLS - log-linear model for opportunity cost

  # [METHODOLOGY] HC1 robust standard errors - heteroskedasticity-consistent;
  #               HC1 applies n/(n-k) finite-sample correction
  vcov_hc1 <- vcovHC(model, type = "HC1")
  bse      <- sqrt(diag(vcov_hc1))

  summ         <- summary(model)
  rsquared     <- summ$r.squared
  rsquared_adj <- summ$adj.r.squared
  nobs_val     <- nobs(model)
  df_resid     <- df.residual(model)

  model_data <- list(
    params       = coef(model),
    bse          = bse,
    rsquared     = rsquared,
    rsquared_adj = rsquared_adj,
    nobs         = nobs_val,
    df_resid     = df_resid
  )

  model_results[[i]] <- model_data

  cat(sprintf(
    "       Done - R²=%.4f, N=%d, df_resid=%d\n",
    rsquared, nobs_val, df_resid
  ))

  if (i == 1) {
    summ_robust         <- coeftest(model, vcov. = vcov_hc1)
    first_model_summary <- capture.output(print(summ_robust))
  }

  rm(acreage_df, model, vcov_hc1); gc()
}

saveRDS(model_results, file = MODEL_RDS)

cat(sprintf(
  "\n[+] Saved %d model data lists to:\n    %s\n",
  length(model_results), MODEL_RDS
))
cat("Model 1 Summary (R_Imputed_Dataset_1.csv)\n")
cat(paste(first_model_summary, collapse = "\n"), "\n")

# ---- Step 2: Parameter Pooling ----

cat("============================================================\n")
cat("STEP 2: PARAMETER POOLING (Rubin's Rules)\n")
cat("============================================================\n")

num_imp <- length(model_results)

cat(sprintf(
  "Loaded %d model data lists from:\n  %s\n\n",
  num_imp, MODEL_RDS
))

params_list <- lapply(model_results, function(r) r$params)
bse_list    <- lapply(model_results, function(r) r$bse)

all_params <- unique(unlist(lapply(params_list, names)))

coef_mat <- matrix(NA, nrow = num_imp, ncol = length(all_params))
colnames(coef_mat) <- all_params
var_mat  <- matrix(NA, nrow = num_imp, ncol = length(all_params))
colnames(var_mat)  <- all_params

for (i in seq_along(model_results)) {
  p_names              <- names(params_list[[i]])
  coef_mat[i, p_names] <- params_list[[i]]
  var_mat[i, p_names]  <- (bse_list[[i]])^2
}

missing_mask <- apply(is.na(coef_mat), 2, any)
if (any(missing_mask)) {
  cat("[!] The following parameters were absent in at least one model\n")
  cat("    and will be pooled only over models where they appeared:\n")
  for (p in names(which(missing_mask))) {
    present_in <- sum(!is.na(coef_mat[, p]))
    cat(sprintf(
      "      %s  (present in %d/%d models)\n",
      p, present_in, num_imp
    ))
  }
  cat("\n")
}

# [METHODOLOGY] Rubin's Rules - Barnard & Rubin (1999) df approximation
m_i <- apply(!is.na(coef_mat), 2, sum)
m_i[m_i < 2] <- 2

q_bar  <- colMeans(coef_mat, na.rm = TRUE)
v_w    <- colMeans(var_mat, na.rm = TRUE)
v_b    <- apply(coef_mat, 2, var, na.rm = TRUE)
v_t    <- v_w + (1 + 1 / m_i) * v_b
se     <- sqrt(v_t)

t_stat  <- q_bar / se
lambda_ <- (1 + 1 / m_i) * v_b / v_t
df_old  <- (m_i - 1) / (lambda_^2)
df_com  <- model_results[[1]]$df_resid
df_obs  <- (df_com + 1) / (df_com + 3) * df_com * (1 - lambda_)
df_adj  <- 1 / (1 / df_old + 1 / df_obs)

p_val <- 2 * pt(abs(t_stat), df = df_adj, lower.tail = FALSE)

pooled_df <- data.frame(
  Parameter = names(q_bar),
  Coef      = q_bar,
  Std_Error = se,
  t_stat    = t_stat,
  df_adj    = df_adj,
  p_value   = p_val,
  Sig       = stars(p_val),
  V_within  = v_w,
  V_between = v_b,
  V_total   = v_t,
  FMI       = lambda_,
  stringsAsFactors = FALSE
)
rownames(pooled_df) <- NULL

cat(sprintf(
  "Pooled OLS Regression Results  (M=%d imputations, Rubin's Rules)\n",
  num_imp
))
cat("Formula: Log_Opportunity_Cost ~ Holes + factor(county_type)\n")
cat("Robust variance: HC1 | Sig: *** p<.001  ** p<.01  * p<.05  . p<.1\n")
cat("----------------------------------------------------------------------\n")

header <- sprintf(
  "%-45s %12s %10s %8s %10s  %s",
  "Parameter", "Coef", "SE", "t", "p", "Sig"
)
cat(header, "\n")
cat("----------------------------------------------------------------------\n")

for (i in seq_len(nrow(pooled_df))) {
  cat(sprintf(
    "%-45s %12.4f %10.4f %8.3f %10.4f  %s\n",
    pooled_df$Parameter[i],
    pooled_df$Coef[i],
    pooled_df$Std_Error[i],
    pooled_df$t_stat[i],
    pooled_df$p_value[i],
    pooled_df$Sig[i]
  ))
}

cat("----------------------------------------------------------------------\n")

r2_vals  <- sapply(model_results, function(r) r$rsquared)
r2a_vals <- sapply(model_results, function(r) r$rsquared_adj)
n_vals   <- sapply(model_results, function(r) r$nobs)

cat(sprintf("\nModel diagnostics across %d imputations:\n", num_imp))
cat(sprintf(
  "  R²         : mean=%.4f  min=%.4f  max=%.4f\n",
  mean(r2_vals), min(r2_vals), max(r2_vals)
))
cat(sprintf(
  "  Adj. R²    : mean=%.4f  min=%.4f  max=%.4f\n",
  mean(r2a_vals), min(r2a_vals), max(r2a_vals)
))
cat(sprintf("  N per model: [%s]\n", paste(n_vals, collapse = ", ")))

write.csv(pooled_df, OUT_CSV, row.names = FALSE)

cat("\n============================================================\n")
cat("OUTPUT FILES\n")
cat("============================================================\n")
cat(sprintf("[+] Model results (RDS) : %s\n", MODEL_RDS))
cat(sprintf("[+] Regression table (CSV) : %s\n", OUT_CSV))
cat("\n[DONE] Phase 4 R version complete.\n")

record_provenance("Phase 4", "Phase_4.R", SCRIPT_DIR, PROV_START)
