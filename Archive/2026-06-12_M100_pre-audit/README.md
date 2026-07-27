# Archived run: 2026-06-12 tri-language M=100 cascade

**What this is.** A complete copy of the Phase 2 → 3 → 4 → 6 tri-language (R/Python/Julia)
M=100 pipeline output, generated 2026-06-12 (Phase 3/4/6 outputs; Phase 1 inputs are mixed
vintage — see caveat below). First identified and inventoried during the Cross-Language
Function Parity Audit (`Issue_Register.md` **X-04**).

**Why it's archived here, not left in `Data/`.** Per **Decision 5** (2026-07-27), the upcoming
frozen cascade re-run uses this run as a **diff baseline** — after the cascade completes, its
outputs get compared against this archive to confirm every delta is explainable by a known,
already-logged audit fix (e.g. **P2-04**'s tie-break correction, quantified at +$1.79B for
Python / +$959M for Julia). But the frozen cascade writes to the exact same `Data/{R,python,Julia}`
paths this run occupies — running it in place would destroy the only copy of the thing it's
supposed to be diffed against. This archive exists so the cascade has something to diff against
*after* it overwrites `Data/`.

**This is NOT a target to reproduce or reconcile toward.** It predates every fix made during this
audit: fabricated `Holes` values (**P1-01**), the collapsed `Ownership_Type` category and missing
Python dedup (**P1-05**/**P1-06**), the arbitrary Python/Julia polygon tie-break (**P2-04**), R's
Tigris Tier-2 enrichment (since removed, **P2-03**/**X-02**), and the unpinned environments
(**X-08**/**X-09**). Every number in this archive is stale by design and retained solely for
audit-trail/diff purposes.

## Contents

| Path | Contents |
|---|---|
| `Phase 1 Parsing/Data/{R,python,Julia}/` | Phase 1 baseline valuation outputs. **Mixed vintage** — R is 2026-06-12, Python is 2026-05-18, Julia is 2026-05-14 (see `X-04` caveat 1: only R's Phase 1 was rerun on the 12th; Python/Julia's Phase 2 ran against their pre-existing older Phase 1 outputs). |
| `Phase 2 Spatial Polygons and True Acreage/Data/{R,python,Julia}/` | Phase 2 OSM polygon + acreage-matched outputs, 2026-06-12. |
| `Phase 3 Economic Merge and MICE Imputation/Data/{R,python,Julia}/` | 100 imputed datasets per language + Rubin's Rules / acreage summaries, 2026-06-12. |
| `Phase 4 Econometric Modeling/Data/{R,python,Julia}/` | Pooled regression results, 2026-06-12. |
| `Phase 5 The Hawaii Micro-Case Study/Data/{R,python,Julia}/` | Oahu micro-case outputs. |
| `Phase 6 Visualization/output/Final_Thesis_Figures/` | 35 files — the curated final figure/table set. |
| `Phase 6 Visualization/output/QA_Verification/` | 24 files — per-language QA renders. |

## Verification (2026-07-27)

Copied via `cp -r` from the live `Data/` and `output/` directories immediately before the frozen
cascade. Verified post-copy: file counts match source exactly in every directory listed above
(1.1 GB total, matching source total to the byte), a CSV spot-check parsed cleanly, a PNG
spot-check passed file-signature validation, and an MD5 checksum on one 100-dataset file
(`R_Imputed_Dataset_50.csv`) matched the source exactly.

## Provenance

- Archived by: Cross-Language Function Parity Audit (Sonnet), 2026-07-27, immediately before the
  first authorized frozen cascade run.
- Original run date: 2026-06-12 (Phase 2 onward); see mixed-vintage caveat above for Phase 1.
- Source paths (as of archiving): `2 - Work/Phase N .../Data/{R,python,Julia}/` and
  `2 - Work/Phase 6 Visualization/output/{Final_Thesis_Figures,QA_Verification}/`.
- Full analytic writeup of this run: `Issue_Register.md` **X-04**, **P3-01**.
