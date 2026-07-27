# Expected Deltas — Frozen Cascade vs. 2026-06-12 Baseline

**Purpose.** Pre-registers the predicted direction (and magnitude, where known) of every
number the frozen cascade is expected to move, relative to the archived
`Archive/2026-06-12_M100_pre-audit/` run, with the responsible fix named for each. Written
and committed *before* the cascade runs, per Decision 5: the post-cascade diff check is only
a real test if the predictions exist first. An unexplained delta after the run is a finding;
a delta explained only after seeing it is not.

**Baseline reference (Jun-12 raw run, pre any audit fix):**

| | R | Python | Julia | Mean | Spread |
|---|---|---|---|---|---|
| Pooled national OC ($B) | 935.521 | 938.309 | 950.637 | 941.489 | **1.61%** |
| Holes coefficient | 0.053 | 0.048 | 0.048 | — | — |

This is the actual on-disk artifact the cascade output gets diffed against (not the
P2-04-arithmetic-adjusted 1.71% figure quoted elsewhere in `Issue_Register.md` — that figure
already bakes in one of the deltas predicted below and is reproduced here as a line item, not
as the baseline).

---

## Per-fix predictions

### P2-04 — polygon tie-break standardization (Python, Julia)
**Status:** already quantified via diagnostic (not a fresh prediction).
- Python: **+$1.79B** (938.309 → 940.099)
- Julia: **+$0.959B** (950.637 → 951.596)
- R: unchanged (already implemented the rule being standardized on)
- Mechanism: Python and Julia previously broke ties among overlapping OSM polygon matches
  arbitrarily; standardizing onto R's largest-area rule moves both totals up. Already measured
  against the 41 affected rows — this is the one entry in this document with a verified
  magnitude, not a prediction.

### P1-01 — `extract_holes()` fabrication fix + D-2 schema fix (all three, R dominant)
**Direction: R's total should move, magnitude unsigned; cross-language spread should narrow.**
- R: the 9 previously-`NA` `Holes` rows no longer exist (regex fix parses them for real), *and*
  D-2's explicit `method_vec`/`variable_schema`/`visitsequence` schema means even a future stray
  NA in `Holes` or `Ownership_Type` can no longer become an accidental second/third `futuremice`
  imputation target. This closes the mechanism identified in `Issue_Register.md` P3-01 as the
  **leading candidate** for the cross-language spread — R's Jun-12 run jointly imputed 4
  variables (`final_acreage`, `Baseline_Value_Per_Acre`, `Holes`, `Ownership_Type`) where
  Python/Julia imputed 2. Removing this should pull R's Holes coefficient down from 0.053 toward
  Python/Julia's 0.048, and should move R's pooled total — **direction not signable from the
  regex fix alone** (depends on which way the spurious joint imputation was biasing the affected
  rows), but the mechanism is real and this document predicts a **non-trivial** move, not a
  no-op.
- Python/Julia: 2 of 16,292 rows change value (Streamsong FL 18→36, Roseburg OR combo 18→27).
  Holes is a predictor, not an imputed target, in both — expect a **negligible** direct effect
  on the point estimate (2 rows out of ~16,300), though Holes feeds the Phase 4 regression
  directly as a regressor, so the Holes coefficient itself could shift by a hair.
- **Spread:** since this closes the audit's own leading candidate for R's outlier position,
  predict the cross-language spread **narrows** from the 1.61% Jun-12 baseline. Not quantifiable
  in advance — candidates #4 (MICE backend differences) and #5 (Phase 1 vintage skew) in
  `Issue_Register.md` P3-01 remain open and untested, so some residual spread should persist.

### P1-05 — `extract_ownership()` Semi Private collapse fix (Python)
**Direction: unknown; magnitude plausibly non-trivial.**
- Python's `Ownership_Type` gains a 4th category (`Semi Private`, 1,663 rows, 10.2% of the
  dataset) previously folded into `Private`. `Ownership_Type` is a Phase 3 `PREDICTOR_COLS`
  member (random-forest split variable) for the ~28.8% of courses needing acreage imputation and
  ~6.7% needing BVPA imputation (`Issue_Register.md` P1-05) — a materially different category
  structure for one in ten rows, feeding a random-forest split, is a real mechanism, not a
  rounding change. **No sign prediction is possible from inspection alone** — RF split behavior
  under a finer categorical predictor doesn't have a knowable direction without running it. Flag
  this as the least-confidently-signed prediction in this document; if Python's total moves by
  more than the P1-06 dedup effect below can explain, P1-05 is the first place to look.

### P1-06 — Python course-level dedup (Python)
**Direction: down. Magnitude: small, order of a few hundred million dollars.**
- Python loses exactly 5 rows (16,297 → 16,292, matching R/Julia), all 5 in `county_type =
  Urban` counties (`Issue_Register.md` P1-06). Fewer valued courses, all else equal, means a
  lower pooled total. Rough order-of-magnitude: mean per-course contribution to the $940B total
  is ~$57.7M (940B / 16,297); 5 Urban courses (higher BVPA than the national mean) removed
  suggests a reduction plausibly in the **low hundreds of millions**, not billions — this is a
  back-of-envelope bound, not a modeled estimate. Direction is the confident part; the exact
  figure is not.

### X-02 / P2-03 — R Tigris Tier 2 removed
**Direction: unchanged. This is a predicted no-op.**
- The diagnostic already run (Decision 3, 7-state sample) confirmed Tigris Tier 2 recovered 0
  rows in the current on-disk data before removal — R's `final_acreage` was already functionally
  identical to `osm_acres` in the Jun-12 run. Removing the tier makes this permanent/structural
  but should not change a single value versus Jun-12. If R's acreage numbers move at all, that
  is itself an unexplained delta worth investigating, not an expected one.

### D-2 — explicit MICE schema (`predictorMatrix`/`variable_schema`/`visitsequence`), all three
**Direction: bundled into P1-01 above for R; no-op for Python/Julia on current data.**
- Covered jointly with P1-01 above since the two fixes were applied together and their effects
  aren't separable from the Jun-12 diff alone. For Python and Julia, D-2 is a belt-and-braces
  guard against a *future* stray NA — on the current, already-clean predictor columns, it
  changes nothing. For R, see P1-01.

---

## Other Decision 1–7 items with no predicted numeric effect

- **X-09 (vendoring RUCC + county boundaries, all live fetches removed):** the live-vs-vendored
  files were verified byte-identical (MD5 match) during the audit. Predicted effect: **none.**
- **Decision 6 / P5-07 (R Oahu TMK candidate list extended):** the original shorter list already
  matched every TMK in the current data (no crash occurred); the extension is defensive, not
  corrective. Predicted effect on Oahu output: **none.**
- **Environment pinning (X-08, Decision 1):** `renv::hydrate()` and the Julia/Python pins were
  built from already-installed package versions, not fresh installs — predicted effect:
  **none**, but flagged as a residual uncertainty. If the machine that runs the frozen cascade
  has ever had a package silently upgrade since Jun-12 (independent of any code change here),
  that would show up as an unexplained delta with no audit fix to attribute it to. Worth checking
  the new `Run_Provenance_*.csv` `key_packages` column against expectation if anything doesn't
  reconcile.

## The one item predicted to move for a reason unrelated to a "fix"

- **Decision 7 / P5-08 (Julia Oahu boundary test — bbox+centroid → real polygon intersection):**
  R and Python already used real polygon tests; only Julia's course-inclusion test changes
  mechanism. **Direction unknown, magnitude likely small but not zero** — any golf course whose
  centroid fell on the wrong side of the old bounding-box heuristic near Oahu's coastline could
  flip in or out of Julia's Oahu course set, moving Julia's Phase 5 course count, footprint
  acreage, and pooled Oahu OC by whatever that course's own contribution is. This is Julia-only
  and Phase-5/Oahu-only — no effect on the national estimate.

---

## Explicitly unchanged

Predicted **no effect** versus the Jun-12 baseline, for the reasons stated above: R's national
acreage variable (X-02/P2-03), the RUCC/county-boundary source data (X-09), R's Oahu TMK match
count (P5-07), and — baseline case, no fix applied — Python's raw `Holes` fallback mechanism for
the 7 of 9 rows that already read `18` under both the old and new logic.

---

## Cross-language spread — net prediction

**Predict: narrows below 1.61% (the Jun-12 raw spread), not holds, not widens.**

Reasoning: the fixes bearing directly on the *structural* divergence identified in
`Issue_Register.md` P3-01 (R's 4-variable vs. Python/Julia's 2-variable imputation model — the
audit's own leading candidate for the spread) are closed by P1-01 + D-2 this cycle, on top of
P2-04 already being ruled out as a spread driver (it suppressed, not caused, the pre-fix
1.61%). Independently, P1-05 and P1-06 both pull Python's Phase 1 output structurally closer to
R/Julia's (matching category count, matching row count) rather than further away. Every fix in
this cycle points the same direction — narrowing — which is different from the P2-04 episode,
where the fix widened the spread by removing noise that had been *masking* a real divergence.
This cycle removes mechanism, not noise.

This is a directional prediction, not a point estimate. If the spread does not narrow — or
narrows by only a token amount — that is itself informative: it would mean candidates #4 (MICE
backend differences: `ranger` vs LightGBM vs `Mice.jl`) or #5 (Phase 1 vintage skew) are carrying
more of the divergence than P3-01 currently credits them for, and should be promoted ahead of
"leading candidate: R's imputation model" in the next revision of that entry.
