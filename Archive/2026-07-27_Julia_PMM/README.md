# Archived run: Julia Phase 3-6 under Mice.jl's default PMM method (pre-RF switch)

**What this is.** A complete copy of Julia's Phase 3, 4, 5 outputs and the full tri-language
Phase 6 output (`Final_Thesis_Figures/`, `QA_Verification/`, `Diagnostic_Results.csv`), plus
`Run_Provenance_Julia.csv`, as they stood immediately after the 2026-07-28 frozen cascade and
the subsequent R Phase 6 contamination fix — before Julia was switched to explicit
random-forest imputation.

**Why it's archived here.** Per **Issue_Register.md X-10**: Julia's `Phase_3.jl` called `mice()`
without a `methods` argument, so every imputed column used `Mice.jl`'s package default,
predictive mean matching (PMM) — confirmed against `Mice.jl` source
(`src/makefunctions.jl:26-31`, `makemethods()` docstring: "The default method is predictive mean
matching (pmm)"). R (`method="rf"` via `ranger`) and Python (`miceforest`, LightGBM-based, RF by
construction) both used random forest. On author review, this was reframed: PMM is `Mice.jl`'s
own canonical default, not a bug — the actual defect is that a methodology choice made
explicitly in two languages was left implicit (and therefore silently different) in the third.
Decision: run Julia both ways and keep both results, so PMM-vs-RF becomes a quantified
methodological sensitivity result rather than a silently-absorbed discrepancy. This archive
preserves the PMM run before `Phase_3.jl` is changed to specify `method="rf"` explicitly
(matching R/Python) and the chain is re-run.

**This is a legitimate, defensible run on its own terms** — PMM is `mice`-family software's
standard default and is a reasonable choice — retained for direct comparison against the RF
re-run, not because it is wrong.

## Contents

| Path | Contents |
|---|---|
| `2 - Work/Phase 3 Economic Merge and MICE Imputation/Data/Julia/` | 100 PMM-imputed datasets + Rubin's Rules / acreage summaries. |
| `2 - Work/Phase 4 Econometric Modeling/Data/Julia/` | Pooled regression results fit on the PMM-imputed datasets. |
| `2 - Work/Phase 5 The Hawaii Micro-Case Study/Data/Julia/` | Oahu micro-case outputs derived from the PMM run. |
| `2 - Work/Phase 6 Visualization/output/` | Full tri-language Phase 6 output (Grand Mean pools this PMM-Julia run with fresh R/Python) — this is the corrected, uncontaminated version produced by the targeted R Phase 6 re-run, not the earlier stale-Julia-contaminated one. |
| `Run_Provenance_Julia.csv` | Provenance ledger as of the PMM run (Phase 1/2 rows have the known blank `git_sha`/`git_dirty` — see `Issue_Register.md` B-8 follow-up). |

## Verification (2026-07-28)

Copied via `cp -r` from the live `Data/`/`output/` directories immediately before switching
`Phase_3.jl` to explicit `method="rf"`. File counts confirmed to match source exactly in every
directory listed above (102/2/4/60 files respectively), and an MD5 checksum on
`Jl_Imputed_Dataset_50.csv` matched the source exactly.

## Provenance

- Archived by: Cross-Language Function Parity Audit (Sonnet), 2026-07-28, immediately before the
  RF sensitivity re-run.
- Source paths (as of archiving): `2 - Work/Phase {3,4,5} .../Data/Julia/`,
  `2 - Work/Phase 6 Visualization/output/`, `2 - Work/Run_Provenance_Julia.csv`.
- Full analytic writeup: `Issue_Register.md` **X-10**.
