# Purpose: Phase 4 master - fit OLS with HC1 robust SEs on each of M Python-generated
#          MICE imputed datasets, then pool via Rubin's Rules and save results.
# Inputs:  Phase 3 Economic Merge and MICE Imputation/Data/Python/Py_Imputed_Dataset_{1..M}.csv
# Outputs: Data/Python/Py_model_results.pkl
#          Data/Python/Py_Regression_Results.csv


# === 1. LIBRARIES ===

import gc
import pathlib
import pickle
import sys
import time
import numpy as np
import pandas as pd
from scipy import stats
import statsmodels.formula.api as smf


# === 2. GLOBALS & PATHS ===

SCRIPT_DIR    = pathlib.Path(__file__).parent

PROV_START = time.time()
sys.path.insert(0, str(SCRIPT_DIR.parent))
import provenance

PHASE3_DIR    = (
    SCRIPT_DIR.parent
    / "Phase 3 Economic Merge and MICE Imputation"
    / "Data"
    / "python"
)
OUT_DIR       = SCRIPT_DIR / "Data" / "python"
PKL_PATH      = OUT_DIR / "Py_model_results.pkl"
OUT_CSV       = OUT_DIR / "Py_Regression_Results.csv"

FORMULA_STR   = "Log_Opportunity_Cost ~ Holes + C(county_type)"
M             = 100
IMPUTED_PATHS = [
    PHASE3_DIR / f"Py_Imputed_Dataset_{i}.csv" for i in range(1, M + 1)
]


# === 3. FUNCTIONS ===

def stars(p: float) -> str:
    """Return significance stars for a single p-value."""
    if p < 0.001: return "***"
    if p < 0.01:  return "**"
    if p < 0.05:  return "*"
    if p < 0.1:   return "."
    return ""


# === 4. EXECUTION ===

def main():
    OUT_DIR.mkdir(parents=True, exist_ok=True)

    missing = [p for p in IMPUTED_PATHS if not p.is_file()]
    if missing:
        print("[FATAL] The following imputed dataset(s) were not found:")
        for p in missing:
            print(f"  {p}")
        raise SystemExit(1)

    print("=" * 70)
    print("PHASE 4 - ECONOMETRIC MODELLING")
    print("=" * 70)
    print()
    print(f"Phase 3 inputs : {PHASE3_DIR}")
    print(f"Output folder  : {OUT_DIR}")
    print(f"Formula        : {FORMULA_STR}")
    print()

    # ---- Step 1: Model Fitting ----

    model_results = []

    for i, path in enumerate(IMPUTED_PATHS, start=1):
        fname = path.name
        print(f"[{i}/{M}] Loading {fname}...")

        acreage_df = pd.read_csv(path, low_memory=False)

        if "osm_acreage" not in acreage_df.columns:
            raise KeyError(f"Column 'osm_acreage' not found in {fname}.")
        if "Baseline_Value_Per_Acre" not in acreage_df.columns:
            raise KeyError(f"Column 'Baseline_Value_Per_Acre' not found in {fname}.")

        acreage_df["Total_Opportunity_Cost"] = (
            acreage_df["osm_acreage"] * acreage_df["Baseline_Value_Per_Acre"]
        )
        acreage_df["Log_Opportunity_Cost"] = np.log1p(
            acreage_df["Total_Opportunity_Cost"]
        )

        cols_needed = [
            "Log_Opportunity_Cost", "Holes", "Baseline_Value_Per_Acre", "county_type"
        ]
        n_before   = len(acreage_df)
        acreage_df = acreage_df.dropna(subset=cols_needed)
        n_dropped  = n_before - len(acreage_df)
        if n_dropped:
            print(f"       Dropped {n_dropped:,} rows with missing values in model columns.")

        model  = smf.ols(FORMULA_STR, data=acreage_df)  # [METHODOLOGY] OLS - log-linear model for opportunity cost
        result = model.fit(cov_type="HC1")               # [METHODOLOGY] HC1 robust SEs - heteroskedasticity-consistent; HC1 = n/(n-k) correction

        model_data = {
            "params":       result.params,
            "bse":          result.bse,
            "rsquared":     result.rsquared,
            "rsquared_adj": result.rsquared_adj,
            "nobs":         result.nobs,
            "df_resid":     result.df_resid,
        }

        model_results.append(model_data)
        print(
            f"       Done - R²={result.rsquared:.4f}, N={int(result.nobs):,}, "
            f"df_resid={int(result.df_resid):,}"
        )

        del acreage_df, model, result
        gc.collect()

    with open(PKL_PATH, "wb") as f:
        pickle.dump(model_results, f)

    print()
    print(f"[+] Saved {len(model_results)} model data dictionaries to:")
    print(f"    {PKL_PATH}")
    print()

    first = model_results[0]
    model1_df = pd.DataFrame({
        "Parameter": first["params"].index,
        "Coef":      first["params"].values,
        "Std_Error": first["bse"].values,
        "t_stat":    first["params"].values / first["bse"].values,
    })
    print("Model 1 Summary (Py_Imputed_Dataset_1.csv)")
    print("-" * 70)
    print(model1_df.to_string(index=False))
    print()

    # ---- Step 2: Parameter Pooling ----

    print("=" * 70)
    print("STEP 2: PARAMETER POOLING (Rubin's Rules)")
    print("=" * 70)
    print()

    M_loaded = len(model_results)

    coef_df = pd.DataFrame([r["params"] for r in model_results])
    var_df  = pd.DataFrame([r["bse"] ** 2 for r in model_results])

    all_params = coef_df.columns.tolist()
    coef_df    = coef_df.reindex(columns=all_params)
    var_df     = var_df.reindex(columns=all_params)

    missing_mask = coef_df.isna().any(axis=0)
    if missing_mask.any():
        print("[!] The following parameters were absent in at least one model and will")
        print("    be pooled only over models where they appeared:")
        for p in missing_mask[missing_mask].index:
            present_in = coef_df[p].notna().sum()
            print(f"      {p}  (present in {present_in}/{M_loaded} models)")
        print()

    # [METHODOLOGY] Rubin's Rules - Barnard & Rubin (1999) df approximation
    m_i = coef_df.notna().sum(axis=0).clip(lower=2)

    q_bar   = coef_df.mean(axis=0)
    v_w     = var_df.mean(axis=0)
    v_b     = coef_df.var(axis=0, ddof=1)
    v_t     = v_w + (1 + 1 / m_i) * v_b
    se      = np.sqrt(v_t)

    t_stat  = q_bar / se
    lambda_ = (1 + 1 / m_i) * v_b / v_t
    df_old  = (m_i - 1) / (lambda_ ** 2)
    df_com  = model_results[0]["df_resid"]
    df_obs  = (df_com + 1) / (df_com + 3) * df_com * (1 - lambda_)
    df_adj  = 1 / (1 / df_old + 1 / df_obs)

    p_val = 2 * stats.t.sf(np.abs(t_stat), df=df_adj)

    pooled_df = pd.DataFrame({
        "Parameter": q_bar.index,
        "Coef":      q_bar.values,
        "Std_Error": se.values,
        "t_stat":    t_stat.values,
        "df_adj":    df_adj.values,
        "p_value":   p_val,
        "Sig":       [stars(p) for p in p_val],
        "V_within":  v_w.values,
        "V_between": v_b.values,
        "V_total":   v_t.values,
        "FMI":       lambda_.values,
    }).reset_index(drop=True)

    print(f"Pooled OLS Regression Results  (M={M_loaded} imputations, Rubin's Rules)")
    print(f"Formula: Log_Opportunity_Cost ~ Holes + C(county_type)")
    print("Robust variance: HC1")
    print("Significance codes: *** p<.001  ** p<.01  * p<.05  . p<.1")
    print("-" * 90)

    header = f"{'Parameter':<45} {'Coef':>12} {'SE':>10} {'t':>8} {'p':>10}  {'Sig'}"
    print(header)
    print("-" * 90)

    for _, row in pooled_df.iterrows():
        print(
            f"{str(row['Parameter']):<45} "
            f"{row['Coef']:>12.4f} "
            f"{row['Std_Error']:>10.4f} "
            f"{row['t_stat']:>8.3f} "
            f"{row['p_value']:>10.4f}  "
            f"{row['Sig']}"
        )

    print("-" * 90)

    r2_vals  = [r["rsquared"]     for r in model_results]
    r2a_vals = [r["rsquared_adj"] for r in model_results]
    n_vals   = [int(r["nobs"])    for r in model_results]

    print()
    print(f"Model diagnostics across {M_loaded} imputations:")
    print(
        f"  R²         : mean={np.mean(r2_vals):.4f}  "
        f"min={np.min(r2_vals):.4f}  max={np.max(r2_vals):.4f}"
    )
    print(
        f"  Adj. R²    : mean={np.mean(r2a_vals):.4f}  "
        f"min={np.min(r2a_vals):.4f}  max={np.max(r2a_vals):.4f}"
    )
    print(f"  N per model: {n_vals}")

    pooled_df.to_csv(OUT_CSV, index=False)
    print()
    print("=" * 70)
    print("OUTPUT FILES")
    print("=" * 70)
    print(f"[+] Model results (pickle) : {PKL_PATH}")
    print(f"[+] Regression table (CSV) : {OUT_CSV}")
    print()
    print("[DONE] Phase 4 complete.")


if __name__ == "__main__":
    main()
    provenance.record_provenance("Phase 4", "Phase_4.py", SCRIPT_DIR, PROV_START)
