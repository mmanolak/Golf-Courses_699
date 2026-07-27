# Purpose: Complete Phase 3 pipeline - MICE imputation (m=100, LightGBM Random
#          Forest) on the Phase 2 acreage-matched dataset to fill missing
#          osm_acreage and Baseline_Value_Per_Acre values.
# Inputs:  Phase 2 Spatial Polygons and True Acreage/Data/python/
#            Py_Phase2_Acreage_Matched.csv
# Outputs: Data/python/Py_Imputed_Dataset_{1..100}.csv
#          Data/python/Py_Rubins_Rules_Summary.csv
#          Data/python/Py_National_Acreage_Summary.csv


# === 1. LIBRARIES ===

import gc
import multiprocessing
import pathlib

import miceforest as mf
import numpy as np
import pandas as pd

import warnings
warnings.filterwarnings("ignore", category=pd.errors.PerformanceWarning)


# === 2. GLOBALS & PATHS ===

SCRIPT_DIR = pathlib.Path(__file__).parent
INPUT_CSV  = (
    SCRIPT_DIR.parent
    / "Phase 2 Spatial Polygons and True Acreage"
    / "Data" / "python"
    / "Py_Phase2_Acreage_Matched.csv"
)
OUT_DIR         = SCRIPT_DIR / "Data" / "python"
OUT_RUBINS_CSV  = OUT_DIR / "Py_Rubins_Rules_Summary.csv"
OUT_ACREAGE_CSV = OUT_DIR / "Py_National_Acreage_Summary.csv"

M              = 100
IMPUTE_COLS    = ["osm_acreage", "Baseline_Value_Per_Acre"]
PREDICTOR_COLS = ["Holes", "Ownership_Type", "county_type", "Longitude", "Latitude"]

# Reserve one core for the OS; caps at 23 on a 24-thread 3900XT
N_CORES = max(1, multiprocessing.cpu_count() - 1)


# === 3. FUNCTIONS ===

def run_imputation(input_csv, out_dir, m_datasets=M):
    # 1. Load ----------------------------------------------------------------
    print("--- 1  Loading Phase 2 acreage-matched dataset ---")

    if not input_csv.exists():
        raise FileNotFoundError(f"Input file not found: {input_csv}")

    out_dir.mkdir(parents=True, exist_ok=True)

    acreage_df = pd.read_csv(input_csv)
    print(f"    Rows: {len(acreage_df):,}")
    print(
        f"    Missing osm_acreage:            "
        f"{acreage_df['osm_acreage'].isna().sum():,}"
    )
    print(
        f"    Missing Baseline_Value_Per_Acre: "
        f"{acreage_df['Baseline_Value_Per_Acre'].isna().sum():,}"
    )

    # 2. Prepare -------------------------------------------------------------
    print("--- 2  Preparing imputation frame ---")

    acreage_df["Holes"]        = pd.to_numeric(acreage_df["Holes"], errors="coerce")
    acreage_df["Ownership_Type"] = acreage_df["Ownership_Type"].astype("category")
    acreage_df["county_type"]  = acreage_df["county_type"].astype("category")

    model_cols = PREDICTOR_COLS + IMPUTE_COLS
    imp_df = acreage_df[model_cols].copy()

    # 3. Run miceforest -------------------------------------------------------
    print(
        f"--- 3  Running miceforest "
        f"(m={m_datasets}, LightGBM backend, n_jobs={N_CORES}) ---"
    )
    # [METHODOLOGY] miceforest MICE - LightGBM Random Forest, m=100 datasets,
    #               random_state=42 for reproducibility (Van Buuren 2018)
    # [METHODOLOGY] explicit variable_schema (P1-01/D-2) - without it, miceforest imputes
    # every column with missing data; declaring the schema keeps predictors as predictors
    # regardless of what Phase 1 does or doesn't leave missing upstream
    imputed_list = mf.ImputationKernel(
        data=imp_df,
        variable_schema=IMPUTE_COLS,
        num_datasets=m_datasets,
        random_state=42,
    )
    # [METHODOLOGY] 10 iterations per dataset; n_jobs parallelises LightGBM trees
    imputed_list.mice(iterations=10, verbose=True, n_jobs=N_CORES)

    # 4. Extract & save each imputed dataset ---------------------------------
    print(f"\n--- 4  Saving {m_datasets} imputed datasets ---")
    for i in range(m_datasets):
        completed = imputed_list.complete_data(dataset=i)

        completed["osm_acreage"] = completed["osm_acreage"].clip(lower=0)
        completed["Baseline_Value_Per_Acre"] = (
            completed["Baseline_Value_Per_Acre"].clip(lower=0)
        )

        out = acreage_df.copy()
        out["osm_acreage"] = completed["osm_acreage"].values
        out["Baseline_Value_Per_Acre"] = completed["Baseline_Value_Per_Acre"].values

        fname = out_dir / f"Py_Imputed_Dataset_{i + 1}.csv"
        out.to_csv(fname, index=False)
        del completed, out; gc.collect()
        print(f"    [OK] {fname}")

    # 5. Verification report (Dataset 1) -------------------------------------
    ds1 = pd.read_csv(out_dir / "Py_Imputed_Dataset_1.csv")

    print("\n=== IMPUTATION VERIFICATION (Dataset 1) ===")
    print(f"  Method: miceforest v{mf.__version__} (LightGBM Random Forest)")
    print(f"  Datasets generated:     {m_datasets}")
    print(f"  Iterations per dataset: 10")

    for col in IMPUTE_COLS:
        s = ds1[col]
        print(f"\n  {col}:")
        print(f"    Missing:  {s.isna().sum()}")
        print(f"    Min:      {s.min():>14,.2f}")
        print(f"    Median:   {s.median():>14,.2f}")
        print(f"    Mean:     {s.mean():>14,.2f}")
        print(f"    Max:      {s.max():>14,.2f}")
        print(f"    Negative: {(s < 0).sum()}")


def run_pooling(in_dir, out_csv, m_datasets=M):
    aggregates  = []
    within_vars = []

    print("--- 1  Loading imputed datasets and computing aggregates ---\n")

    for i in range(1, m_datasets + 1):
        path = in_dir / f"Py_Imputed_Dataset_{i}.csv"
        if not path.exists():
            raise FileNotFoundError(f"File not found: {path}")

        df = pd.read_csv(path)
        df["Total_Opportunity_Cost"] = df["osm_acreage"] * df["Baseline_Value_Per_Acre"]

        q_i   = df["Total_Opportunity_Cost"].sum()
        var_i = df["Total_Opportunity_Cost"].var(ddof=1)

        aggregates.append(q_i)
        within_vars.append(var_i)

        del df; gc.collect()
        print(f"  Dataset {i}:  ${q_i / 1e9:>10.3f} B")

    print("\n--- 2  Applying Rubin's Rules ---")

    aggregates  = np.array(aggregates)
    within_vars = np.array(within_vars)

    # [METHODOLOGY] Rubin's Rules pooling - q_bar is the pooled national estimate;
    #               v_t combines within- and between-imputation variance (Rubin 1987)
    q_bar = aggregates.mean()
    v_w   = within_vars.mean()
    v_b   = aggregates.var(ddof=1)
    v_t   = v_w + v_b + v_b / m_datasets
    se    = np.sqrt(v_t)
    ci95_lo = q_bar - 1.960 * se
    ci95_hi = q_bar + 1.960 * se
    ci99_lo = q_bar - 2.576 * se
    ci99_hi = q_bar + 2.576 * se

    print("\n=== RUBIN'S RULES RESULTS ===")
    print(f"  Pooled Aggregate National Value:  ${q_bar / 1e9:>10.3f} B")
    print(f"  Within-Imputation Variance (v_w): {v_w:.4e}")
    print(f"  Between-Imputation Variance (v_b):{v_b:.4e}")
    print(f"  Total Variance (v_t):             {v_t:.4e}")
    print(f"  Standard Error:                   ${se / 1e9:>10.3f} B")
    print(
        f"  99% Confidence Interval:          "
        f"${ci99_lo / 1e9:>10.3f} B - ${ci99_hi / 1e9:>10.3f} B"
    )
    print(
        f"  95% Confidence Interval:          "
        f"${ci95_lo / 1e9:>10.3f} B - ${ci95_hi / 1e9:>10.3f} B"
    )

    metrics = [
        ("Pooled Aggregate National Value ($)",  f"{q_bar:.2f}"),
        ("Pooled Aggregate National Value ($B)", f"{q_bar / 1e9:.3f}"),
        ("Within-Imputation Variance (v_w)",     f"{v_w:.4e}"),
        ("Between-Imputation Variance (v_b)",    f"{v_b:.4e}"),
        ("Total Variance (v_t)",                 f"{v_t:.4e}"),
        ("Standard Error ($)",                   f"{se:.2f}"),
        ("99% CI Lower ($B)",                    f"{ci99_lo / 1e9:.3f}"),
        ("99% CI Upper ($B)",                    f"{ci99_hi / 1e9:.3f}"),
        ("95% CI Lower ($B)",                    f"{ci95_lo / 1e9:.3f}"),
        ("95% CI Upper ($B)",                    f"{ci95_hi / 1e9:.3f}"),
    ] + [
        (f"Dataset {i} Aggregate ($B)", f"{aggregates[i - 1] / 1e9:.3f}")
        for i in range(1, m_datasets + 1)
    ]

    pooled_df = pd.DataFrame(metrics, columns=["Metric", "Value"])
    pooled_df.to_csv(out_csv, index=False)
    print(f"\n  [OK] Saved -> {out_csv.name}")

    return q_bar, se, ci95_lo, ci95_hi, ci99_lo, ci99_hi


def pool_acreage(x: np.ndarray) -> dict:
    q_bar = x.mean()
    v_b   = x.var(ddof=1)
    se    = np.sqrt(v_b + v_b / len(x))
    return {
        "mean":  q_bar,
        "sd_b":  np.sqrt(v_b),
        "ci95_lo": q_bar - 1.960 * se,
        "ci95_hi": q_bar + 1.960 * se,
        "ci99_lo": q_bar - 2.576 * se,
        "ci99_hi": q_bar + 2.576 * se,
    }


def run_acreage_summary(in_dir, out_csv, m_datasets=M):
    print("Computing total U.S. golf course footprint (pooled across imputations)\n")

    print("--- 1  Loading imputed datasets and computing acreage totals ---\n")

    national_totals = np.zeros(m_datasets)
    by_type_frames  = []

    for i in range(1, m_datasets + 1):
        path = in_dir / f"Py_Imputed_Dataset_{i}.csv"
        if not path.exists():
            raise FileNotFoundError(f"File not found: {path}")

        df = pd.read_csv(path)
        national_totals[i - 1] = df["osm_acreage"].sum()

        type_sums = (
            df.groupby("county_type", as_index=False)["osm_acreage"]
            .sum()
            .rename(columns={"osm_acreage": "acreage"})
        )
        type_sums["imputation"] = i
        by_type_frames.append(type_sums)

        del df; gc.collect()
        urban = type_sums.loc[type_sums["county_type"] == "Urban", "acreage"].iat[0]
        rural = type_sums.loc[type_sums["county_type"] == "Rural", "acreage"].iat[0]
        print(
            f"  Dataset {i}:  {round(national_totals[i - 1]):>12,} acres"
            f"  ({round(urban):,} Urban / {round(rural):,} Rural)"
        )

    print("\n--- 2  Pooling across imputations ---\n")

    # [METHODOLOGY] Rubin's Rules (acreage) - between-imputation variance only;
    #               within-variance is zero for a spatially fixed attribute
    nat_pool = pool_acreage(national_totals)

    all_by_type = pd.concat(by_type_frames, ignore_index=True)
    type_pool = (
        all_by_type.groupby("county_type")["acreage"]
        .apply(lambda x: pd.Series(pool_acreage(x.to_numpy())))
        .unstack()
        .reset_index()
        .rename(columns={"mean": "pooled_acres"})
        .sort_values("pooled_acres", ascending=False)
        .reset_index(drop=True)
    )

    print("=== NATIONAL ACREAGE RESULTS ===")
    print(f"  Total U.S. Golf Acreage:  {round(nat_pool['mean']):,} acres")
    print(f"  Between-Imputation SD:    {nat_pool['sd_b']:,.2f}")
    print(
        f"  99% CI:                   "
        f"{round(nat_pool['ci99_lo']):,} - {round(nat_pool['ci99_hi']):,} acres"
    )
    print(
        f"  95% CI:                   "
        f"{round(nat_pool['ci95_lo']):,} - {round(nat_pool['ci95_hi']):,} acres"
    )
    for _, row in type_pool.iterrows():
        print(f"  {row['county_type']:<20} {round(row['pooled_acres']):,} acres")

    national_row = pd.DataFrame([{
        "Category":          "National Total",
        "County_Type":       "All",
        "Pooled_Acres":      round(nat_pool["mean"], 2),
        "SD_Between":        round(nat_pool["sd_b"], 4),
        "CI_95_Lower_Acres": round(nat_pool["ci95_lo"], 2),
        "CI_95_Upper_Acres": round(nat_pool["ci95_hi"], 2),
        "CI_99_Lower_Acres": round(nat_pool["ci99_lo"], 2),
        "CI_99_Upper_Acres": round(nat_pool["ci99_hi"], 2),
    }])
    type_rows = pd.DataFrame({
        "Category":          "By County Type",
        "County_Type":       type_pool["county_type"],
        "Pooled_Acres":      type_pool["pooled_acres"].round(2),
        "SD_Between":        type_pool["sd_b"].round(4),
        "CI_95_Lower_Acres": type_pool["ci95_lo"].round(2),
        "CI_95_Upper_Acres": type_pool["ci95_hi"].round(2),
        "CI_99_Lower_Acres": type_pool["ci99_lo"].round(2),
        "CI_99_Upper_Acres": type_pool["ci99_hi"].round(2),
    })
    summary_df = pd.concat([national_row, type_rows], ignore_index=True)
    summary_df.to_csv(out_csv, index=False)
    print(f"  [OK] Saved -> {out_csv.name}")


# === 4. EXECUTION ===

if __name__ == "__main__":
    run_imputation(INPUT_CSV, OUT_DIR, M)
    print("\n=== STEP 2: RUBIN'S RULES POOLING ===")
    run_pooling(OUT_DIR, OUT_RUBINS_CSV, M)
    print("\n=== STEP 3: NATIONAL ACREAGE SUMMARY ===")
    run_acreage_summary(OUT_DIR, OUT_ACREAGE_CSV, M)
