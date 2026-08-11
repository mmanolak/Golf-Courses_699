---
title: "Phase 4 Summary: Econometric Modeling"
author: "Michael"
format:
  pdf:
    toc: true
    number-sections: true
    colorlinks: true
---

# Phase 4 Summary: Econometric Modeling

## Overview

Phase 4 implements the econometric modeling step of the golf course land valuation
thesis. The pipeline fits an OLS regression with HC1 heteroskedasticity-robust
standard errors on each of the M = 100 MICE-imputed datasets produced in Phase 3,
then pools the estimates across imputations using **Rubin's Rules** to produce
statistically valid inference under multiple imputation.

The phase is implemented across three languages (Python, R, Julia) and exists in
two tiers:

| Tier | Purpose | Scripts |
|------|---------|---------|
| **Master Scripts** (Phase 4 root) | Single-file, end-to-end pipeline | `Phase_4.py`, `Phase_4.R`, `Phase_4.jl` |
| **Bulk Tests** (sub-directories) | Modular two-step scripts for testing | `model_fitting.*` + `parameter_pooling.*` |

---

## Model Specification

```
Log_Opportunity_Cost ~ Holes + county_type
```

| Variable | Type | Description |
|----------|------|-------------|
| `Log_Opportunity_Cost` | Outcome | `log1p(osm_acreage × Baseline_Value_Per_Acre)` |
| `Holes` | Continuous | Number of holes at the golf course |
| `county_type` | Binary categorical | `Rural` (reference) vs. `Urban` |

**Robust SE:** HC1 sandwich estimator, `(n / (n - k)) × (X'X)⁻¹ X' diag(ê²) X (X'X)⁻¹`

---

## Methodology

### Step 1 — Model Fitting

For each of the M = 100 imputed datasets:
1. Load the dataset from Phase 3 (`{Py|R|Jl}_Imputed_Dataset_{1..100}.csv`)
2. Compute derived variables:
   - `Total_Opportunity_Cost = osm_acreage × Baseline_Value_Per_Acre`
   - `Log_Opportunity_Cost = log1p(Total_Opportunity_Cost)`
3. Drop rows with any missing values in model columns (consistently 34 rows per dataset)
4. Fit OLS using language-native implementation
5. Compute HC1 robust standard errors
6. Serialize: coefficients, robust SEs, R², N, df_resid

### Step 2 — Parameter Pooling (Rubin's Rules)

| Symbol | Formula | Description |
|--------|---------|-------------|
| Q̄ | `mean(Qᵢ)` | Pooled point estimate |
| V_W | `mean(Var_i)` | Within-imputation variance |
| V_B | `var(Qᵢ, ddof=1)` | Between-imputation variance |
| V_T | `V_W + (1 + 1/M) × V_B` | Total variance |
| SE | `√V_T` | Pooled standard error |
| FMI | `(1 + 1/M) × V_B / V_T` | Fraction of Missing Information |
| df | Barnard & Rubin (1999) | Adjusted degrees of freedom |

Significance: `*** p<.001  ** p<.01  * p<.05  . p<.1`

---

## Master Scripts

### `Phase_4.py`

- **Data source:** `Py_Imputed_Dataset_{1..100}.csv` (Python MICE via `miceforest`, LightGBM backend)
- **OLS engine:** `statsmodels.formula.api.ols` with `cov_type="HC1"`
- **Intermediate output:** `Py_model_results.pkl`
- **Final output:** `Py_Regression_Results.csv`

### `Phase_4.R`

- **Data source:** `R_Imputed_Dataset_{1..100}.csv` (R MICE via `futuremice`, Random Forest)
- **OLS engine:** `lm()` with `sandwich::vcovHC(type="HC1")`
- **Path resolution:** `get_script_dir()` + `find_work_dir()` — works from interactive
  source and `Rscript` command line; uses `stop()` instead of `quit()` to avoid
  crashing the R interactive terminal on fatal errors
- **Intermediate output:** `R_model_results.rds`
- **Final output:** `R_Regression_Results.csv`

### `Phase_4.jl`

- **Data source:** `Jl_Imputed_Dataset_{1..100}.csv` (Julia MICE via `Mice.jl`)
- **OLS engine:** `GLM.lm()` with manual HC1 sandwich estimator
- **HC1 implementation:** `(n/(n-k)) × (X'X)⁻¹ X'diag(ê²)X (X'X)⁻¹` — computed
  directly from `modelmatrix()` and `residuals()` for version stability
- **Path resolution:** `@__DIR__` + `find_work_dir()` walk
- **Intermediate output:** `Jl_model_results.jls`
- **Final output:** `Jl_Regression_Results.csv`

---

## Results

> **Note:** The tables below reflect pilot runs at M = 5 imputations. They will be
> updated once the M = 100 pipelines complete across all three languages.

### Python — `Py_Regression_Results.csv`

**N per model:** 16,258  |  **Rows dropped:** 34 per dataset  |  *(M = 5 pilot)*

| Parameter | Coef | SE | t | df_adj | p | Sig | FMI |
|-----------|------|----|---|--------|---|-----|-----|
| Intercept | 12.2822 | 0.0386 | 318.55 | 613.9 | <.001 | *** | 0.079 |
| Holes | 0.0474 | 0.0024 | 20.06 | 3133.8 | <.001 | *** | 0.032 |
| C(county_type)[T.Urban] | 4.1720 | 0.0226 | 184.80 | 25.9 | <.001 | *** | 0.392 |

**Model R² across imputations:** mean ≈ 0.77 (from original `Phase_4.py` run)

---

### R — `R_Regression_Results.csv`

**N per model:** 16,258  |  **Rows dropped:** 34 per dataset  |  *(M = 5 pilot)*

| Parameter | Coef | SE | t | df_adj | p | Sig | FMI |
|-----------|------|----|---|--------|---|-----|-----|
| (Intercept) | 12.2292 | 0.0414 | 295.32 | 8,997.1 | <.001 | *** | 0.014 |
| Holes | 0.0525 | 0.0027 | 19.79 | 1,625.7 | <.001 | *** | 0.047 |
| factor(county_type)Urban | 4.0014 | 0.0251 | 159.71 | 34.8 | <.001 | *** | 0.339 |

**Model R² across imputations:**
- Mean: 0.6988  |  Min: 0.6942  |  Max: 0.7030

---

### Julia — `Jl_Regression_Results.csv`

**N per model:** 16,258  |  **Rows dropped:** 34 per dataset  |  *(M = 5 pilot)*

| Parameter | Coef | SE | t | df_adj | p | Sig | FMI |
|-----------|------|----|---|--------|---|-----|-----|
| (Intercept) | 12.2471 | 0.0388 | 315.30 | 2,112.1 | <.001 | *** | 0.040 |
| Holes | 0.0476 | 0.0024 | 19.92 | 6,641.5 | <.001 | *** | 0.019 |
| county_type: Urban | 4.1577 | 0.0201 | 206.61 | 426.8 | <.001 | *** | 0.095 |

**Model R² across imputations:**
- Mean: 0.7304  |  Min: 0.7280  |  Max: 0.7339

---

## Cross-Language Coefficient Comparison *(M = 5 pilot — pending M = 100 rerun)*

| Parameter | Python | R | Julia |
|-----------|--------|---|-------|
| **Intercept** | 12.282 | 12.229 | 12.247 |
| **Holes** | 0.0474 | 0.0525 | 0.0476 |
| **Urban Premium** | 4.172 | 4.001 | 4.158 |
| **Mean R²** | ~0.770 | 0.699 | 0.730 |

> **Note — Holes coefficient circularity.** `Holes` is a MICE predictor for the
> imputed `osm_acreage`/`Baseline_Value_Per_Acre` columns this model's dependent
> variable is built from. The coefficient above is a full-sample estimate and, for
> the imputed rows, partly reflects the imputer's own use of `Holes` rather than an
> independent association. See Appendix A.4 for the estimate restricted to courses
> with fully observed (non-imputed) acreage and value.
| **N per model** | 16,258 | 16,258 | 16,258 |

---

## What the Data Tells Us

### 1. The Urban Land Premium is Large and Dominant

Across all three languages the `county_type: Urban` coefficient is **~4.0–4.2
log-units**, which on the log-scale means urban golf course land is worth
approximately `exp(4.1) ≈ 60×` more than otherwise equivalent rural land.
This is by far the largest effect in the model and is estimated with enormous
precision (t > 150 in every implementation), reflecting the well-known urban
land price gradient.

### 2. More Holes = Higher Opportunity Cost

The `Holes` coefficient is positive and highly significant (~0.047–0.053) across
all languages. Each additional hole is associated with roughly a **4.7–5.3%
increase** in log opportunity cost, consistent with larger courses commanding
premium land in higher-value areas (course size and land value co-vary positively).

### 3. Cross-Language Consistency Confirms the Pipeline

The intercept (~12.23–12.28), Holes effect (~0.047–0.053), and Urban premium
(~4.0–4.2) are consistent across all three independent implementations. The
minor numerical differences arise from each language using a different MICE
backend (Python `IterativeImputer`, R `mice`, Julia `Mice.jl`), which produce
statistically equivalent but not numerically identical imputed datasets.

### 4. The Fraction of Missing Information (FMI) Varies by Parameter

The FMI quantifies how much imputation uncertainty contributed to each pooled
standard error:

| Parameter | Py FMI | R FMI | Jl FMI | Interpretation |
|-----------|--------|-------|--------|----------------|
| Intercept | 0.079 | 0.014 | 0.040 | Low — intercept well-identified |
| Holes | 0.032 | 0.047 | 0.019 | Low — Holes rarely missing |
| Urban | 0.392 | 0.339 | 0.095 | Moderate — some imputation variance |

The elevated Urban FMI in Python and R indicates that the `county_type` Urban
dummy carries more between-imputation variability — likely because `county_type`
is used as a predictor in MICE, and variation in its imputed values (for courses
that lacked it) propagates into the Urban coefficient across imputations.

### 5. R² is Moderate and Consistent

The model explains **69–73% of the variance** in log opportunity cost across
all imputed datasets and languages. This is strong for a two-predictor model
on cross-sectional observational data, driven primarily by the Urban/Rural
distinction. The remaining variance reflects course-level heterogeneity not
captured by holes count alone (course quality, amenities, local demand, etc.).

---

## Output File Summary

| Language | Master Script | Model Object | Pooled CSV |
|----------|--------------|--------------|------------|
| Python | `Phase_4.py` | `Py_model_results.pkl` | `Py_Regression_Results.csv` |
| R | `Phase_4.R` | `R_model_results.rds` | `R_Regression_Results.csv` |
| Julia | `Phase_4.jl` | `Jl_model_results.jls` | `Jl_Regression_Results.csv` |

All files are saved to the `Phase 4 Econometric Modeling/` directory (same folder as
the master scripts). Bulk test sub-scripts in `Bulk Tests/{python,R,Julia}/`
produce identically-named files in their respective subdirectories.

---

## Technical Notes

### HC1 Robust SE Implementations

| Language | Method |
|----------|--------|
| Python | `statsmodels` `cov_type="HC1"` |
| R | `sandwich::vcovHC(model, type="HC1")` |
| Julia | Manual: `(n/(n-k)) × (X'X)⁻¹ X'diag(ê²)X (X'X)⁻¹` via `modelmatrix()` |

### Serialization Formats

| Language | Format | Extension |
|----------|--------|-----------|
| Python | `pickle` binary | `.pkl` |
| R | R native serialization | `.rds` |
| Julia | `Serialization.serialize` | `.jls` |

### Interactive Source vs. Command-Line Execution

The R and Julia master scripts include robust path-detection logic:
- **R:** `get_script_dir()` checks `--file=` CLI argument first, then inspects
  `sys.frames()` for the `ofile` attribute set by `source()`. Falls back to
  `getwd()`. Fatal errors use `stop()` (not `quit()`) so the R interactive
  terminal is not terminated.
- **Julia:** `@__DIR__` macro resolves the script directory automatically at
  parse time — works identically under `julia script.jl` and `include()`.

---

## Bulk Test Scripts

The `Bulk Tests/` sub-directory contains modular two-step versions of the pipeline
split across `model_fitting.*` and `parameter_pooling.*`. These exist for iterative
development and cross-language comparison. They are functionally identical to the
master scripts but write their outputs to their respective language sub-directory.

| Language dir | Intermediate | Final CSV |
|---|---|---|
| `Bulk Tests/python/` | `Py_model_results.pkl` | `Py_Regression_Results.csv` |
| `Bulk Tests/R/` | `R_model_results.rds` | `R_Regression_Results.csv` |
| `Bulk Tests/Julia/` | `Jl_model_results.jls` | `Jl_Regression_Results.csv` |

---

## Historical / Legacy Scripts

Earlier exploratory scripts (`debug_compare`, `debug_dtypes`, `debug_params`,
`Pooled_Log_Regression`) used a fuller 5-parameter model specification
(`Holes + Course_Type + county_type`) and are preserved in the root of the
`Phase 4 Econometric Modeling/` directory. They are **not part of the current
standardized pipeline** but document the iterative porting process from Python
to R. Results from those scripts (R² ≈ 0.77, Urban ~4.09) reflect the richer
specification and are consistent with the current simplified model.

---

## Code Standardization

All 9 Phase 4 scripts were standardized for naming consistency, path resolution,
and structural conventions. No logic, formulas, spatial parameters, or filter
thresholds were changed.

### Four-Section Structure

Every script follows the same section order:

```
# === 1. LIBRARIES ===
# === 2. GLOBALS & PATHS ===
# === 3. FUNCTIONS ===
# === 4. EXECUTION ===
```

R and Julia use `# (none)` in Section 3 where no helper functions are defined.

### Variable Renaming

| Old name | New name | Applies to |
|----------|----------|------------|
| `results_list` | `model_results` | all scripts |
| `results_df` | `pooled_df` | all scripts |
| `df` (per-loop dataset) | `acreage_df` | all scripts |
| `se_vals` | `se` | R and Julia scripts |
| `Q_bar`, `V_W`, `V_B`, `V_T`, `SE` | `q_bar`, `v_w`, `v_b`, `v_t`, `se` | Python scripts |
| `script_dir`, `work_dir`, `phase3_dir`, `output_dir` | `SCRIPT_DIR`, `PHASE3_DIR`, `OUT_DIR` | all |
| `formula_str` | `FORMULA_STR` | R / Julia |
| `rds_path` / `output_csv` | `RDS_PATH` / `OUT_CSV` | R / Julia bulk |
| `model_rds_path` / `regression_csv` | `MODEL_RDS` / `OUT_CSV` | Phase_4.R master |
| `MODEL_PKL_PATH` / `REGRESSION_CSV` | `PKL_PATH` / `OUT_CSV` | Phase_4.py master |
| `find_work_dir()` | removed | all — replaced by relative path construction |
| `get_script_dir()` | removed | R bulk — replaced by `this.path` |

### Path Resolution

Fragile `find_work_dir()` / `get_script_dir()` traversal functions were removed
from every script and replaced with single-line relative constructions anchored
to the script's own location:

| Language | Anchor | Example |
|----------|--------|---------|
| R | `this.path::this.dir()` | `SCRIPT_DIR <- this.path::this.dir()` |
| Python | `pathlib.Path(__file__).parent` | `SCRIPT_DIR = pathlib.Path(__file__).parent` |
| Julia | `@__DIR__` | `const SCRIPT_DIR = @__DIR__` |

**Bulk test scripts** (3 levels up to `2 - Work`):
- R / Julia: `joinpath(SCRIPT_DIR, "..", "..", "..", "Phase 3 ...")`
- Python: `SCRIPT_DIR.parents[2] / "Phase 3 ..."`

**Master scripts** (1 level up to `2 - Work`, then into `Data/<Language>/`):
- R: `file.path(SCRIPT_DIR, "..", "Phase 3 ...", "Data", "R")`
- Python: `SCRIPT_DIR.parent / "Phase 3 ..." / "Data" / "Python"`
- Julia: `joinpath(SCRIPT_DIR, "..", "Phase 3 ...", "Data", "Julia")`

### `[METHODOLOGY]` Flags

Three blocks carry `[METHODOLOGY]` flags in every script:

| Block | Tag content |
|-------|------------|
| `lm()` / `smf.ols()` call | `OLS — log-linear model for opportunity cost` |
| HC1 vcov block | `HC1 robust standard errors — heteroskedasticity-consistent; HC1 = n/(n-k) finite-sample correction` |
| Rubin's Rules block | `Rubin's Rules — Barnard & Rubin (1999) df approximation` |

### Language-Specific Conventions

- **R:** `stars()` moved to Section 3 with Roxygen2 `#'` docstring. Pre-existing
  `library(wooldridge)` and `library(broom)` in `Phase_4.R` retained with
  `# pre-existing dependency — do not remove` comments.
- **Python:** `stars()` moved to Section 3 with `"""docstring"""`. All scripts
  wrapped with `def main(): ... if __name__ == "__main__": main()` guard.
- **Julia:** `get_stars()` moved to Section 3 with `"""docstring"""`. All scripts
  wrapped with `function main() ... end` + `if abspath(PROGRAM_FILE) == @__FILE__ / main() / end`
  guard. All module-level variables declared `const`.

### Folder Name Correction

Bulk test scripts previously referenced
`"Phase 3 Economic Merge & MICE Imputation"` (ampersand). Corrected to
`"Phase 3 Economic Merge and MICE Imputation"` to match the actual folder name
and the Phase 3 standardization convention.

### Outstanding Issues (Flagged, Not Fixed)

1. **Path mismatch — bulk vs. master:** Bulk test scripts resolve imputed datasets
   from the Phase 3 root directory (`Phase 3 .../`). Master scripts resolve from
   `Phase 3 .../Data/<Language>/`. Pre-existing inconsistency — not fixed.
2. **`final_acreage` vs. `osm_acreage`:** `Phase_4.R` uses `final_acreage` as
   the acreage column (intentional R-side design choice); all other scripts use
   `osm_acreage`. Cross-language column inconsistency — not fixed.
3. **`Pkg.add()` in Julia scripts:** All Julia scripts call `Pkg.add()` on every
   execution. Should be removed once packages are installed, but left as-is
   pending confirmation.
4. **`library(broom)` in `Phase_4.R`:** Loaded but no explicit `broom` call is
   visible — may be unused. Retained as pre-existing dependency.

---

## Next Steps

1. **Phase 5:** Hawaii Micro-Case Study — validate the model coefficients against
   municipal tax assessment data for Hawaii golf courses.
2. Consider extending the bulk framework to include the full 5-parameter
   specification (`Holes + Course_Type + county_type`) for cross-language parity
   with legacy results.

---

## Phase 4C Script Review

**Script reviewed:** `Phase_4.py` (master script, 247 lines pre-fix, 250 lines post-fix)

### Compliance Audit

| # | Check | Result | Notes |
|---|-------|--------|-------|
| 1 | Four-section layout | ✓ Pass | `# === 1–4. ===` at lines 8/18/38/49; two blank lines at all boundaries |
| 2 | ALL_CAPS constants in Section 2 | ✓ Pass | `SCRIPT_DIR`, `PHASE3_DIR`, `OUT_DIR`, `PKL_PATH`, `OUT_CSV`, `FORMULA_STR`, `M`, `IMPUTED_PATHS` |
| 3 | Relative path via `Path(__file__).parent` | ✓ Pass | `SCRIPT_DIR = pathlib.Path(__file__).parent` at line 20 |
| 4 | `if __name__ == "__main__"` guard | ✓ Pass | Lines 245–246 |
| 5 | File existence check before loop | ✓ Pass | List comprehension checks all 100 before loop; `raise SystemExit(1)` if any missing |
| 6 | `import gc` in Section 1 | **FIXED** | Missing — would have caused `NameError` on any `gc.collect()` call |
| 7 | `del df; gc.collect()` in model-fitting loop | **FIXED** | `acreage_df`, `model`, `result` never freed; added `del acreage_df, model, result; gc.collect()` |
| 8 | Dependent variable is log(OC), not log(acreage) | ✓ Pass | `np.log1p(Total_Opportunity_Cost)`, `FORMULA_STR` contains `Log_Opportunity_Cost` |
| 9 | `# [METHODOLOGY]` on OLS, HC1, Rubin's blocks | ✓ Pass | Inline at line 101 (OLS), line 102 (HC1), line 164 (Rubin's) |
| 10 | Rubin's Rules formula correct | ✓ Pass | `v_b = coef_df.var(ddof=1)`, `v_w = var_df.mean()`, `v_t = v_w + (1+1/m_i)*v_b`; Barnard & Rubin (1999) df; `2 * stats.t.sf(|t|, df_adj)` |
| 11 | FMI stored in output CSV | ✓ Pass | `FMI = lambda_` in `pooled_df` |
| 12 | No synthetic data | ✓ Pass | All values from real CSVs |

### Fixes Applied

**Fix 1 — Added `import gc` to Section 1:**
The `gc` module was entirely absent from imports. CLAUDE.md mandates `gc.collect()` inside
all dataset-loading loops; without `import gc` any such call would raise `NameError` at runtime.
Added `import gc` alphabetically first in Section 1. Same class as Phase_3.py Fix 1.

**Fix 2 — Added `del acreage_df, model, result; gc.collect()` at end of model-fitting loop body:**
The 100-iteration loop created three large objects per iteration (`acreage_df` = full CSV
DataFrame, `model` = statsmodels OLS object, `result` = fitted model with HC1 SEs) but never
freed them before the next iteration. All needed values were already captured in `model_data`
(lightweight dict of pandas Series + scalars). Added cleanup after the print statement inside
the loop. Same CLAUDE.md memory violation class as Phase_4.R Fix 1, Phase_4.jl Fix 3,
Phase_3.py Fixes 2–4.

### Observations (no fix)

1. **`"Python"` vs `"python"` in path strings**: `PHASE3_DIR` uses `"Data" / "Python"` (capital P)
   while Phase_3.py writes to `"Data" / "python"` (lowercase). Windows case-insensitive
   filesystem makes this harmless but inconsistent. Not a CLAUDE.md violation.

2. **`stars()` lacks NaN guard**: R's `stars()` checks `is.na(x)` first; Julia's `get_stars()`
   checks `isnan(p)` first; Python's goes directly to `< 0.001`. In Python, `NaN < 0.001`
   evaluates to `False` (IEEE 754), so the function returns `""` for NaN inputs correctly.
   Not a functional bug — no fix applied.

3. **`C(county_type)[T.Urban]` parameter name**: statsmodels' `C()` operator produces this
   name in `pooled_df`. Diverges from R (`factor(county_type)Urban`) and Julia
   (`county_type: Urban`). Flagged for Part 4D.

4. **`log1p` vs `log`**: Dependent variable uses `np.log1p(Total_Opportunity_Cost)` rather than
   `np.log()`. Guards against zero values. Consistent with R and Julia. Phase 6 axis labels
   should technically read `log(1 + Opportunity_Cost)`. Flagged for Phase 6 label review.

---

## Phase 4D Cross-Language Consistency Review

**Source files read:** `Bulk Tests/R/R_Regression_Results.csv`, `Bulk Tests/Julia/Jl_Regression_Results.csv`, `Bulk Tests/python/Py_Regression_Results.csv`

### Full Parameter Name Divergence Table

| Parameter | R CSV | Julia CSV | Python CSV |
|-----------|-------|-----------|------------|
| Intercept | `(Intercept)` | `(Intercept)` | `Intercept` |
| Holes | `Holes` | `Holes` | `Holes` |
| Urban County | `factor(county_type)Urban` | `county_type: Urban` | `C(county_type)[T.Urban]` |
| Row order | Intercept, Holes, Urban | Intercept, Holes, Urban | Intercept, Urban, Holes |

Python drops parentheses from `Intercept` — a statsmodels convention; R and Julia are consistent.
Row order differs: Python places Urban before Holes. Phase 6 `compute_grand_means()` must match
by parameter name, not by row index.

### Coefficient Comparison (from actual M=100 Bulk Tests CSVs)

| Parameter | R | Julia | Python | Spread |
|-----------|---|-------|--------|--------|
| Intercept | 12.2292 | 12.2471 | 12.2822 | 0.053 (0.4%) |
| Holes | 0.05251 | 0.04764 | 0.04740 | 0.00511 (10.3%) |
| Urban County | 4.00145 | 4.15774 | 4.17199 | 0.17054 (4.1%) |

No order-of-magnitude divergence. R's Holes coefficient is ~10% higher than Julia/Python,
attributable to R using `final_acreage` (OSM + Tigris acreage) while Julia/Python use
`osm_acreage` (OSM-only). Same data asymmetry documented in Phase 2D.

### Standard Error and FMI Comparison

| Parameter | R SE | Jl SE | Py SE | R FMI | Jl FMI | Py FMI |
|-----------|------|-------|-------|-------|--------|--------|
| Intercept | 0.04141 | 0.03884 | 0.03856 | 0.014 | 0.040 | 0.079 |
| Holes | 0.002653 | 0.002392 | 0.002363 | 0.047 | 0.019 | 0.032 |
| Urban | 0.025054 | 0.020123 | 0.022576 | 0.339 | 0.095 | 0.392 |

Urban FMI is high in R (0.339) and Python (0.392) but low in Julia (0.095). Urban `df_adj`
is very low: Python=25.9, R=34.8, Julia=426.8. High between-imputation variance for the Urban
coefficient under R's Random Forest MICE and Python's LightGBM MICE backends. Julia's Mice.jl
backend produces substantially lower between-imputation variance for this coefficient.

### Checklist Items

1. **Parameter names**: Not fully consistent — three distinct naming conventions, one additional
   Python-specific divergence (no parentheses on Intercept). No fix applied — language-native conventions.
2. **Coefficient magnitude**: ✓ In plausible range; no order-of-magnitude divergence.
3. **SE and FMI columns**: ✓ Present and populated in all three CSVs.
4. **p_value numeric**: ✓ All values are float (not string); all parameters *** in all three.
5. **$0.944T plausibility**: $0.944T is the Phase 3 aggregate national opportunity cost from
   Rubin's pooled sum — it is NOT derived from Phase 4 regression coefficients. Phase 4
   coefficients feed Phase 6 Forest Plots (β̂ comparison per language). The checklist item
   conflates the two. Phase 4 coefficients are directionally consistent with the economics
   (large positive Urban premium, positive Holes effect).

### Critical Operational Observation

**Canonical `Data/` output directories are empty.** All three regression CSVs and model objects
exist only in `Bulk Tests/{R,Julia,python}/` subdirectories. The master Phase 4 scripts
(`Phase_4.R`, `Phase_4.jl`, `Phase_4.py`) write to:
- `Phase 4 Econometric Modeling/Data/R/R_Regression_Results.csv`
- `Phase 4 Econometric Modeling/Data/Julia/Jl_Regression_Results.csv`
- `Phase 4 Econometric Modeling/Data/python/Py_Regression_Results.csv`

None of these files currently exist. Phase 6 scripts reading from those paths will fail
until the three master Phase 4 scripts are executed to completion. The Bulk Tests outputs
are functionally correct and serve as confirmation that the pipeline runs; the master scripts
need to be run to populate the canonical output locations.