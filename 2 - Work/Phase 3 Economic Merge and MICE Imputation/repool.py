# Purpose: CORRECTED Rubin's Rules pooling for a census total.
#
#   Fixes a specification error in the original run_pooling():
#     OLD:  v_w = mean( var(individual course values) )   <-- cross-sectional
#                                                              dispersion, NOT
#                                                              estimator variance
#     NEW:  v_w = 0                                       <-- correct for a
#                                                              census total
#
#   Justification: the estimand Q is the TOTAL over all N courses, not a
#   sample mean. Given a completed dataset, the total is known exactly, so the
#   within-imputation sampling variance U_m = 0. All uncertainty is imputation
#   uncertainty, captured by the between-imputation variance B.
#
#       q_bar = mean(q_m)
#       v_w   = 0
#       v_b   = var(q_m, ddof=1)
#       v_t   = v_b + v_b/M = v_b (1 + 1/M)
#       se    = sqrt(v_t)
#       df    = M - 1          (lambda = 1 when v_w = 0)
#       CI    = q_bar +/- t(df) * se        <-- t, not z
#
#   This is the SAME logic the original script already applied correctly in
#   pool_acreage() ("within-variance is zero for a spatially fixed attribute").
#
#   NOTE: Requires NO rerun of the imputations. It re-reads the existing
#   Phase 3 imputed datasets and re-pools them. The POINT ESTIMATE q_bar is
#   unchanged by this correction; only the SE / CI change.

import pathlib
import sys

import numpy as np
import pandas as pd
from scipy import stats


def pool_total(q: np.ndarray) -> dict:
    """Rubin pooling for a census total (within-imputation variance = 0)."""
    M = len(q)
    if M < 2:
        raise ValueError("Need M >= 2 imputations to estimate between-variance.")
    q_bar = q.mean()
    v_b = q.var(ddof=1)
    v_w = 0.0
    v_t = v_w + v_b + v_b / M
    se = np.sqrt(v_t)
    df = M - 1                     # lambda = 1 exactly when v_w = 0
    t95 = stats.t.ppf(0.975, df)
    t99 = stats.t.ppf(0.995, df)
    return {
        "M": M, "q_bar": q_bar, "v_w": v_w, "v_b": v_b, "v_t": v_t,
        "se": se, "df": df, "t95": t95, "t99": t99,
        "ci95_lo": q_bar - t95 * se, "ci95_hi": q_bar + t95 * se,
        "ci99_lo": q_bar - t99 * se, "ci99_hi": q_bar + t99 * se,
    }


def repool(in_dir: pathlib.Path, prefix: str, out_csv: pathlib.Path, m_datasets: int):
    aggregates = []
    old_within = []

    for i in range(1, m_datasets + 1):
        path = in_dir / f"{prefix}_Imputed_Dataset_{i}.csv"
        if not path.exists():
            raise FileNotFoundError(path)
        # R's imputed datasets use "final_acreage"; Python/Julia use "osm_acreage"
        # (same convention as Phase_6.R's get_acreage() / Phase_6.jl's get_acreage()).
        header_cols = pd.read_csv(path, nrows=0).columns
        acre_col = "osm_acreage" if "osm_acreage" in header_cols else "final_acreage"
        df = pd.read_csv(path, usecols=[acre_col, "Baseline_Value_Per_Acre"])
        toc = df[acre_col].to_numpy() * df["Baseline_Value_Per_Acre"].to_numpy()
        aggregates.append(toc.sum())
        old_within.append(toc.var(ddof=1))   # reproduce OLD v_w for comparison
        del df

    q = np.array(aggregates)
    r = pool_total(q)

    # --- what the ORIGINAL code would have reported, for side-by-side ---
    v_w_old = float(np.mean(old_within))
    se_old = np.sqrt(v_w_old + r["v_b"] + r["v_b"] / r["M"])

    print("=== CORRECTED RUBIN'S RULES (census total) ===")
    print(f"  M                         : {r['M']}")
    print(f"  Pooled point estimate     : ${r['q_bar']/1e9:12.3f} B   (UNCHANGED)")
    print(f"  v_b (between-imputation)  : {r['v_b']:.4e}")
    print(f"  v_w  OLD (misspecified)   : {v_w_old:.4e}")
    print(f"  v_w  NEW (correct)        : {r['v_w']:.4e}")
    print(f"  SE   OLD                  : ${se_old/1e9:12.4f} B")
    print(f"  SE   NEW                  : ${r['se']/1e9:12.4f} B")
    print(f"  CI width ratio (old/new)  : {se_old/r['se']:.3f}x")
    print(f"  df                        : {r['df']}  (t95={r['t95']:.4f}, t99={r['t99']:.4f})")
    print(f"  95% CI  : ${r['ci95_lo']/1e9:10.3f} B  -  ${r['ci95_hi']/1e9:10.3f} B")
    print(f"  99% CI  : ${r['ci99_lo']/1e9:10.3f} B  -  ${r['ci99_hi']/1e9:10.3f} B")

    rows = [
        ("Pooled Aggregate National Value ($)",  f"{r['q_bar']:.2f}"),
        ("Pooled Aggregate National Value ($B)", f"{r['q_bar']/1e9:.3f}"),
        ("Within-Imputation Variance (v_w)",     f"{r['v_w']:.4e}"),
        ("Between-Imputation Variance (v_b)",    f"{r['v_b']:.4e}"),
        ("Total Variance (v_t)",                 f"{r['v_t']:.4e}"),
        ("Standard Error ($)",                   f"{r['se']:.2f}"),
        ("Degrees of Freedom",                   f"{r['df']}"),
        ("t critical (95%)",                     f"{r['t95']:.4f}"),
        ("t critical (99%)",                     f"{r['t99']:.4f}"),
        ("99% CI Lower ($B)",                    f"{r['ci99_lo']/1e9:.3f}"),
        ("99% CI Upper ($B)",                    f"{r['ci99_hi']/1e9:.3f}"),
        ("95% CI Lower ($B)",                    f"{r['ci95_lo']/1e9:.3f}"),
        ("95% CI Upper ($B)",                    f"{r['ci95_hi']/1e9:.3f}"),
        ("[superseded] v_w as originally coded", f"{v_w_old:.4e}"),
        ("[superseded] SE as originally coded",  f"{se_old:.2f}"),
    ] + [(f"Dataset {i} Aggregate ($B)", f"{q[i-1]/1e9:.3f}") for i in range(1, r["M"] + 1)]

    pd.DataFrame(rows, columns=["Metric", "Value"]).to_csv(out_csv, index=False)
    print(f"\n  [OK] Saved -> {out_csv}")
    return r


if __name__ == "__main__":
    if len(sys.argv) > 1:
        # CLI mode: repool.py <in_dir> [prefix] [m]
        in_dir = pathlib.Path(sys.argv[1])
        prefix = sys.argv[2] if len(sys.argv) > 2 else "Py"
        m = int(sys.argv[3]) if len(sys.argv) > 3 else 100
        repool(in_dir, prefix, in_dir / f"{prefix}_Rubins_Rules_Summary_CORRECTED.csv", m)
    else:
        # No-args mode (e.g. IDE "Run"): repool all three languages in place,
        # same convention as Phase_3.py/R/jl (paths resolved off this script's
        # own location, no CLI args required).
        SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
        LANGUAGES = [
            (SCRIPT_DIR / "Data" / "R", "R"),
            (SCRIPT_DIR / "Data" / "python", "Py"),
            (SCRIPT_DIR / "Data" / "Julia", "Jl"),
        ]
        for in_dir, prefix in LANGUAGES:
            print(f"\n{'=' * 60}\n{prefix}  ({in_dir})\n{'=' * 60}")
            if not in_dir.exists():
                print(f"  [SKIP] Directory not found: {in_dir}")
                continue
            repool(in_dir, prefix, in_dir / f"{prefix}_Rubins_Rules_Summary_CORRECTED.csv", 100)