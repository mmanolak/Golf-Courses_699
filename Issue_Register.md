# Gone Golfing — Issue Register

**Purpose.** A living record of defects, inconsistencies, and open questions found while
reviewing the Phase 1–6 pipeline (summaries, documentation, and the 17 master scripts).
Updated phase by phase as each is worked through.

**Scope of review as of this revision.** All six `00_-_PhaseN_Summary.md`, all six
`01_-_PhaseN_Documentation.md`, `Meta_Summary.qmd`, `Meta_Documentation.qmd`, and the
master scripts `Phase_1..5.{py,R,jl}`, `Phase_6.{R,jl}`. Bulk-test sub-scripts have **not**
been read — they are referenced in the docs but were not supplied.

---

## How to use this file

Every issue carries a stable ID (`P3-01`, `X-02`). IDs are never reused or renumbered, so
they can be cited in commit messages, thesis notes, and conversation.

| Field | Meaning |
|---|---|
| **Severity** | `Critical` = affects a published number or a defence claim · `Major` = affects interpretation or reproducibility · `Minor` = internal inconsistency, low blast radius · `Cosmetic` = wording/formatting |
| **Status** | `Open` · `Confirmed` (root cause established) · `Fixed` · `Won't fix` (accepted, documented) · `Question` (needs the author's intent before it can be classified) |
| **Locus** | `Code` = the script is wrong · `Docs` = the script is right, the write-up is stale/wrong · `Both` |

**Standing caveat.** A `Docs` issue is not automatically the lesser problem. Everything the
committee reads is the write-up; a correct script with a stale table published beside it is
still a wrong published number.

---

## Running tally

| Phase | Critical | Major | Minor | Cosmetic | Total |
|---|---|---|---|---|---|
| Cross-cutting | 3 | 5 | 1 | 0 | 9 |
| Phase 1 | 0 | 2 | 6 | 2 | 10 |
| Phase 2 | 0 | 0 | 4 | 0 | 4 |
| Phase 3 | 2 | 2 | 4 | 0 | 8 |
| Phase 4 | 0 | 2 | 1 | 1 | 4 |
| Phase 5 | 0 | 3 | 6 | 1 | 10 |
| Phase 6 | 0 | 2 | 1 | 2 | 5 |
| **Total** | **5** | **16** | **23** | **6** | **50** |

*(2026-07-27: +2 net — new **X-09** (vendoring policy, Major, Fixed) and **P1-10** (`extract_holes`
fallback residual, Minor, Open). P6-05/P6-07 remain excluded from this count as N/A/verified-sound,
same convention as before.)*

---

## Cross-cutting

### X-01 — Published Phase 3 results were produced at M = 5, not M = 100
**Severity:** Critical · **Status:** Confirmed · **Locus:** Docs (code is correct)

The single most consequential finding. Full detail under **P3-01**; recorded here because it
propagates into `00_-_Phase3_Summary.md`, `Meta_Summary.pdf`, and any thesis prose quoting the
$943B confidence interval.

**Update (2026-07-25):** see **X-04** — a complete M=100 tri-language run, through Phase 6, exists
on disk dated 2026-06-12. Whether this M=5-vs-M=100 framing still describes what's *currently
published* needs rechecking against that run, not assumed unchanged.

### X-02 — R runs a different acreage variable than Python and Julia, end to end
**Severity:** Major · **Status:** Resolved 2026-07-27 (author decision, Gate 3) · **Locus:** Code (by design) / Docs (framing)

R imputes and regresses `final_acreage` (OSM + Tigris landmarks, coalesced); Python and Julia
use `osm_acreage` (OSM only). This persists from Phase 2 through Phase 4.

- `Phase_3.R:47` — `IMPUTE_COLS <- c("final_acreage", "Baseline_Value_Per_Acre")`
- `Phase_4.R:89-97` — hard-stops unless `final_acreage` is present
- `Phase_4.py:81-87`, `Phase_4.jl:82-90` — both use `osm_acreage`

The Phase 4 documentation already attributes R's ~10% higher `Holes` coefficient to this
(§4D). The problem is what the framing then claims: the tri-language design is presented
throughout as a **language/backend** robustness check, but one of the three arms is fed a
different input variable. The R–vs–Py/Jl spread therefore confounds two effects (MICE backend
*and* acreage definition) that the write-up treats as one.

**Not necessarily a bug.** Tigris Tier 2 is defensible as R-only enrichment. But the claim
needs to be stated as what it is, or R needs an `osm_acreage`-only variant for the parity
comparison. See also **P2-01**.

**Update (2026-07-25):** the specific mechanism the doc names —
`01_-_Phase4_Documentation.md:485-487`, "R's Holes coefficient is ~10% higher... attributable to
R using final_acreage (OSM+Tigris) while Julia/Python use osm_acreage" — is weakened by **P2-03**:
in the current on-disk data, R's Tigris tier recovered **zero** courses, so `final_acreage` and
`osm_acreage`-with-fallback are the same set of observed values for this snapshot. The stated
explanation doesn't obviously hold for the data it's describing. Two more mechanistic, verified
candidates now exist for the same coefficient spread: **P1-01** (R alone silently imputes 9
`Holes` values via random forest, using `final_acreage`/`BVPA` as predictors — directly touches
the `Holes` coefficient specifically) and **P1-05** (Python's `Ownership_Type` collapse skewed
~10% of Python's own MICE predictor matrix, pre-fix). Neither is confirmed as *the* cause — that
needs a re-run with fixes applied, which is out of scope now — but the doc's current explanation
should not be treated as settled.

**Confirmed live at Phase 4, `VERIFIED` by reading (Parity Audit E-1, 2026-07-25):** this isn't a
Phase 2/3 curiosity that fades before the regression — it's the literal multiplicand of the
regression's dependent variable. `Phase_4.R:96-99`: `Total_Opportunity_Cost <-
final_acreage * Baseline_Value_Per_Acre`. `Phase_4.py:86-91` / `Phase_4.jl:89-92`: same
computation, `osm_acreage * Baseline_Value_Per_Acre`. `log1p()` is applied identically, in the
same order (multiply-then-log, not log-then-something-else), in all three — the only actual
divergence at this step is which acreage column feeds it, which is exactly the standing **X-02**
question, not a new one. Formula order itself checked clean.

**Cross-reference (2026-07-26):** the acreage-*variable*-choice question this entry tracks
(`final_acreage` vs `osm_acreage`) is distinct from **P2-04**'s acreage-*matching*-tie-break fix,
but both bear on the same national total. P2-04's fix moved Python/Julia's pooled totals up and
**widened** the cross-language spread (1.61%→1.71% — see **P3-01**), which rules P2-04 out as a
suppressor-turned-cause of the spread and leaves this entry's four-variable-imputation-model
candidate (via **P1-01**) as the standing lead explanation, not weakened by anything found here.

**Resolved (author decision, Gate 3/Decision 3, 2026-07-27): Option B — R's Tigris Tier 2
removed, `final_acreage` = `osm_acres` unconditionally.** Rationale, from the author: Tier 2
recovered zero courses in every run to date, its live fetch was non-deterministic (**P2-03**'s
diagnostic confirmed real golf landmarks exist and would have been recovered on a different run
day), and **X-06** already established the polygon *set* is shared across all three languages —
so the enrichment was costing determinism and parity while contributing nothing observed. R now
runs the identical acreage variable, by identical construction, as Python and Julia. This closes
the standing "which acreage variable is canonical" author-call question (**CLAUDE.md** §5) by
removing the second variable rather than choosing between them. **Does not resolve or affect**
the leading spread-cause candidate (R's four-variable imputation model, **P1-01**) — that
divergence is at Phase 3, not Phase 2, and is untouched by this fix. Not executed beyond the
diagnostic (§2.1); effect will be visible in the next cascade.

### X-03 — Bulk-test sub-scripts are undocumented dependencies of the review
**Severity:** Major · **Status:** Question, partially superseded by X-04 · **Locus:** —

The documentation repeatedly cites results, fixes, and canonical outputs living in
`Bulk Tests/{R,Julia,python}/`. Phase 4 §4D states outright that the canonical `Data/`
output directories are **empty** and that all regression CSVs exist only under `Bulk Tests/`.
None of these scripts or CSVs were supplied, so every claim sourced from them is currently
unverifiable. Several numeric conflicts below (**P3-05**, **P4-01**) may resolve trivially
once it's clear which tier produced which table.

**Update (2026-07-25, see X-04):** the claim that `Data/` is empty is now false, and — per
`Phase_6.R:74,107,137,220,461,1141,1421-1433,1662-1664,2072-2074,2575-2579` — was never true of
the *code*: `Phase_6.R` reads directly from the canonical `Phase 3.../Data/{R,python,Julia}` and
`Phase 4.../Data/{R,python,Julia}` paths, never from `Bulk Tests/`. Whatever `Bulk Tests/`
contains, it is not what `Phase_6.R` consumes. Doesn't resolve which numbers are *currently
published*, but narrows the question considerably — see X-04.

**Open, deliberately not resolved by rounding (2026-07-25):** there may be a **third**
run-generation, distinct from both the M=5 pilot and the Jun-12 `Data/` run. Python's `Holes`
coefficient is **0.048** in `Final_Thesis_Figures/8.241_Table2_Regression.tex` (the Jun-12 run)
but **0.04740** as currently published in `01_-_Phase4_Documentation.md` (labeled "from actual
M=100 Bulk Tests CSVs"). `0.0474` does not round to `0.048` — that's a 0.0006 gap at the reported
precision, larger than rounding noise. Two explanations remain open, not one favored: (a) the
published table is a genuinely different `Bulk Tests/` M=100 run, predating Jun-12, or (b) same
run, transcription/precision-loss error somewhere in the doc. **Not closing this by assumption
either way** — if a third generation exists, its provenance (which script, which day, which
inputs) needs establishing before the freeze, not discovered after. Needs the author's input on
whether `Bulk Tests/{R,Julia,python}/` scripts were ever run standalone, and when.

### X-04 — A complete, synchronized tri-language M=100 run already exists on disk (2026-06-12)
**Severity:** Critical · **Status:** Confirmed, not yet reconciled with published docs · **Locus:** Data/Docs

Directly relevant to **P3-01/P4-01**'s open question ("do M=100 outputs exist anywhere?") — yes.
`VERIFIED` by reading file listings and mtimes only (no execution, §2.1):

- `Data/{R,python,Julia}/*_Imputed_Dataset_{1..100}.csv` — exactly 100 files per language, all
  three languages, mtimes clustered 2026-06-12 13:23–13:35
- `Data/{R,python,Julia}/*_Regression_Results.csv` (Phase 4) — all three, mtimes 2026-06-12
  13:53–13:54, ~20 minutes after the Phase 3 M=100 run completed
- `Phase 6 Visualization/output/Final_Thesis_Figures/` and `.../QA_Verification/` — both
  directories' contents dated 2026-06-12, 15:12–23:15, including `n020/n040/n060/n080/n100`
  MICE-density diagnostics (an M=100 naming pattern, distinct from the older
  `Bulk/{R,Julia}/output/` files' `n005/n025/n050/n075/n100` pattern and May 5–18 mtimes)

This looks like a complete Phase 2 → 3 → 4 → 6 cascade, all three languages, run same-day,
finishing with what appear to be curated "final" and "QA" figure sets — i.e., substantially
further along than **X-01**/**X-03** assumed when they described the M=5 pilot as the only
completed run and `Data/` as empty.

**Caveat 1 — the cascade is synchronized from Phase 2 onward, not from Phase 1.** Phase 1
outputs are **not** same-day: `R_Phase1_Baseline_Golf_Valuation.csv` is 2026-06-12, but
`Py_Phase1_Baseline_Golf_Valuation.csv` is 2026-05-18 and
`Jl_Phase1_Baseline_Golf_Valuation.csv` is 2026-05-14. Only R's Phase 1 was rerun on the 12th
(consistent with the FIPS fix timing, see **P1-07**); Python and Julia's Phase 2 (2026-06-12) ran
against their pre-existing, older Phase 1 outputs. Concretely: **B-6**'s +5 duplicate rows and
**P1-05**'s collapsed `Ownership_Type` (both Python-side, both pre-existing in Python's Phase 1
output since before this run) are already baked into this Jun-12 Phase 2/3/4/6 cascade, not
introduced by it. "Synchronized cascade" describes Phase 2 downward; it does not mean
"synchronized inputs."

**What this does NOT mean:** these are not necessarily *correct* or ready to publish. This
2026-06-12 run predates today's fixes and still carries every defect found in this audit that
existed in the code at the time: fabricated `Holes` in Python/Julia (**P1-01**), the
`Ownership_Type` collapse and missing dedup in Python (**P1-05**, **P1-06**, fixed today, not yet
rerun), and R's silent `futuremice` imputation of `Holes`/`Ownership_Type` (**P1-01**/**P1-05**).
It is evidence that a full cascade *can* complete (roughly 10 hours, Phase 2 start to Phase 6
finish, from the timestamps above) — useful for planning the frozen re-run — not evidence that
any number in it is final.

**QA_Verification/ and Final_Thesis_Figures/ inventoried (2026-07-25).** `QA_Verification/`
(26 files) is entirely per-language PNG renders — Scripts 1, 2, 5 (MICE density), 7, each split
into `_Julia`/`_Python`/`_R` suffixed images. **It does not contain cross-language numeric
comparison artifacts** (no CSVs, no diff tables) — it's a visual per-language sanity check, not
something that can answer **H-4** (filter boundaries) or **H-6** (join semantics), which are
code/data questions a rendered map can't settle. `Final_Thesis_Figures/` (32 files) is more
useful: alongside the "TriLanguage"/"GrandMean"/"Combined" figures, it contains three actual
`.tex` tables — `8.141_Table1_Acreage.tex`, `8.241_Table2_Regression.tex`,
`8.301_Table3_Hawaii_Geo.tex` — real M=100 numbers, read directly rather than re-derived:

- `8.241_Table2_Regression.tex`: Holes coefficient — Python 0.048, R 0.053, Julia 0.048.
  Close to, but not obviously identical at full precision to, the coefficients already published
  in `01_-_Phase4_Documentation.md` (R 0.05251, Python 0.04740, Julia 0.04764) — which that doc
  itself already labels as "from actual M=100 Bulk Tests CSVs." Two different M=100 sources
  (`Bulk Tests/` vs this Jun-12 `Data/` run) producing close-but-not-provably-identical numbers
  is exactly what **X-03** is asking about, now with concrete numbers on both sides instead of an
  abstract question. Not conclusively resolved — would need full-precision digits from both
  sources to prove same-run-vs-different-run — but it's evidence, not a re-derivation.
- `Data/{R,python,Julia}/*_Rubins_Rules_Summary.csv` (the Jun-12 run): pooled national OC R
  $935.521B / Python $938.309B / Julia $950.637B, Grand Mean ≈ **$941.5B**. The currently
  published Phase 3 Summary figures are $936.0B/$943.0B/$951.4B, Grand Mean $943.5B
  (`Notes.md`) — close but **measurably different**, not a rounding artifact. This is strong
  evidence for the open question below.

**Escalating** (§5), now sharper with the above: does the author know this run exists? Two
sub-questions, not one:
1. If `Final_Thesis_Figures`/`QA_Verification` are what's currently cited, **X-03**'s "`Data/` is
   empty" premise is simply wrong and needs correcting at the source, not just here.
2. Given the National OC numbers above don't match, the manuscript most likely still cites the
   M=5 pilot for that figure specifically — see the new open question logged at the bottom of
   this file. If confirmed, the practical implication is good news: **a better version of the
   headline number may already exist on disk**, pending the **P1-01**/**P1-05** fixes and a
   reconciliation re-run, rather than requiring a from-scratch M=100 run.

**Decided (author decision, Decision 5, 2026-07-27): the upcoming frozen cascade is a fresh run,
not a reconciliation with the Jun-12 data.** The question posed above (is Jun-12 "good enough to
reconcile toward" instead of re-running) is answered no: the Jun-12 outputs predate every fix this
audit has made — **P1-01** (Holes), **P1-05**/**P1-06** (Ownership/dedup), **P2-04** (tie-break),
**P2-03**/**X-02** (Tigris Tier 2 removed), **X-08**/**X-09** (environment + live-fetch pinning) —
so treating it as a target to converge on would mean judging the new, corrected pipeline against
an uncorrected baseline. **Its role going forward is a diff baseline, not a target:** after the
frozen cascade completes, compare its outputs against the Jun-12 run and confirm each delta is
explainable by a known, already-logged fix — e.g. Python's pooled total should move by
approximately **P2-04**'s already-quantified **+$1.79B** from the tie-break fix alone, before any
other change is accounted for. **A delta that isn't explainable by a logged fix is a red flag
worth investigating before publication, not a number to accept because it moved.** This framing
applies to every open provenance question in this entry (X-03's Bulk Tests tier, the two-M=100
figures question) — none of them are being resolved *by* matching Jun-12; they're resolved by the
frozen cascade being correct on its own terms, with Jun-12 serving only as a sanity-checkable
diff.

### X-06 — R and Julia's OSM golf-course polygons are not independently extracted — both consume Python's extraction
**Severity:** Critical · **Status:** Confirmed · **Locus:** Code (by design, documented) / Docs (framing)

Surfaced while checking **C-4** (reprojection order). This bears directly on the thesis's central
claim (`CLAUDE.md` §1): "three independent implementations converge... evidence the result is
not an artefact of one toolchain." For the OSM polygon geometry specifically, **it is not three
independent extractions.**

**Precise scope (2026-07-25) — this is not a blanket claim about the pipeline.** The project is
a controlled comparison from Phase 2 onward, not an independent replication end to end, and the
independence boundary is exact, not fuzzy:
- **Phase 1 (parsing):** genuinely independent — three separate parsers run on the same raw
  `Golf Courses-USA.csv`, with real algorithmic differences (**P1-01**, **P1-05**, **P1-06**).
- **Phase 2, polygon *extraction*:** shared, not independent — Python's `pyosmium` pass only,
  inherited by R and Julia (below).
- **Phase 2, area computation / reprojection / plausibility filtering / point-matching:**
  independent per language (different libraries, own code paths) — verified in **C-3**/**C-4**.
- **Phases 3–6:** independent per language (each language's own MICE call, own regression, own
  visualization code).
So: independent inputs into a shared Phase-2 geometry step, independent processing on both sides
of it. Correcting the framing wherever the thesis claims blanket independence is a post-freeze
documentation task, same as the `Phase_2.R:4`/`Phase_2.jl:9` "fully self-contained" header
claims below — logging both here, not editing either now (`CLAUDE.md` §2.2 applies to these
headers the same as to `.md`/`.qmd` docs, per author instruction).

`READ`, all three headers and Step-0/Step-1 code:
- **Python** (`Phase_2.py:1-83`) is the only language that independently streams the 11 GB
  `us-260413.osm.pbf` file, via `pyosmium`'s C++ streaming handler, filtering `leisure=golf_course`
  areas and building multipolygon WKB geometries. This produces
  `Data/Python/Py_Phase2_OSM_Golf_Polygons.gpkg`.
- **Julia** (`Phase_2.jl:1-18,29,89`): header states inputs are
  `Data/Python/Py_Phase2_OSM_Golf_Polygons.gpkg` — Julia has **no PBF-parsing code at all**. Step 1
  is `isfile(PY_GPKG) || error("Python GPKG not found... Run Phase_2.py first.")` followed by
  `GeoDataFrames.read(PY_GPKG)`. Julia recomputes area (`ArchGDAL.geomarea`), reprojects, filters,
  and does its own point-matching against its own Phase 1 data — but the polygon geometries
  themselves are Python's.
- **R** (`Phase_2.R:1-29,111-163`): documented and coded as GPKG-first. `Phase_2.R:122-123`:
  "Priority: use the Python GPKG if it exists (fast, clean); only attempt the PBF as a last resort
  when the GPKG is absent." The code checks `file.exists(PY_GPKG)` **first** and only attempts its
  own `st_read(PBF_FILE, ...)` if that's missing — and the comment at `Phase_2.R:119-121` explains
  why: "GDAL's OGR driver cannot reliably parse this particular 11 GB PBF file (crashes at ~byte
  3,049,247,581 due to data corruption). The Python pipeline used pyosmium... which tolerates the
  corruption." **R's own independent PBF-parsing path is known-broken on this file** and exists
  only as an unreachable fallback, since `Py_Phase2_OSM_Golf_Polygons.gpkg` is already present on
  disk — the `file.exists(PY_GPKG)` branch fires every time, in practice, today.

**What is and isn't independent, precisely:** the *set of polygons* — which real-world features
count as a golf course, their exact boundary shapes, and (per **C-6**) how multipolygon relations
get assembled into single areas — is decided **once**, by Python's `pyosmium` pass, and inherited
identically by R and Julia. What R and Julia *do* independently: recompute area from that shared
geometry (via `sf`/GDAL vs `ArchGDAL` — different libraries, could still numerically diverge),
reproject, apply the plausibility filter, and run their own point-to-polygon matching against
their own Phase 1 baseline data. So the acreage **numbers** downstream of the shared geometry are
still independently computed — but any omission, mis-tagged feature, or corruption-handling
choice in Python's extraction is silently present in all three arms, not caught by cross-language
comparison, because there's only one extraction to disagree with.

**Why this matters more than a typical finding:** the whole audit is framed around catching
places where the three languages *aren't actually independent* and a bug masquerades as
convergence. This is that pattern, but upstream and structural rather than a coding accident —
and it's the one variable (acreage) most implicated in candidate #1 of **P3-01**'s spread
analysis and in **X-02**. Also worth noting: this is honestly documented in both scripts' own
headers and comments — it was a deliberate engineering decision (a real, reproducible GDAL crash
on this PBF), not something hidden. But "fully self-contained" (`Phase_2.R:4`, `Phase_2.jl:9`) is
the wrong description for what's actually a documented hard dependency on Python's Phase 2
running first.

**Analytic consequence — this makes the origin of the P3-01 spread decisively testable.** Because
all three languages match the *same* polygon set, the same points against it, and independently
compute area from *identical* geometry, observed (non-imputed) acreage should be numerically
identical across languages — any divergence there would have to come from area computation,
reprojection, or matching, not from the geometry itself. See **D-0**.

**D-0 result (2026-07-25):** `VERIFIED` — largely confirms the prediction, with one real
exception. Joined the three Phase 2 outputs on (round(lat,4), round(lon,4), Course_Name); 16,290
keys common to all three (2 short of the 16,292 in each language individually — a small join
gap, not investigated further, noted as a caveat on the count). Of 11,604 keys with acreage
observed in all three languages: **11,563 (99.6%) match to within 1e-6 relative tolerance** (2
bit-exact, 11,561 float-tolerance-exact) — strong confirmation that shared geometry produces
shared area for the overwhelming majority of courses, exactly as predicted. But **41 rows (0.35%)
differ substantially** — not floating-point noise: relative differences from 2.7% to 85%, median
37%. Logged as **P2-04**: the pattern in nearly every one of the 41 is *two* languages agreeing
exactly while the third differs by a large, non-trivial amount (odd-language-out tally over all
41: Julia 15, Python 14, R 12 — roughly even, no single language is the systematic outlier).
That signature points to independent **point-to-polygon matching** picking a *different candidate
polygon* from the shared pool for a small number of ambiguous courses (e.g. multi-course
complexes), not an area-formula or CRS bug — C-3/C-4 already verified those clean. Rough dollar
impact: summed max pairwise acreage difference across the 41 rows ≈ 4,324 acres; at the ~$414k/
acre national mean BVPA, order-of-magnitude **~$1.8B** — real, but well short of the ~$15B
(1.61%) cross-language spread in **P3-01**, so this does not replace R's four-variable imputation
model as the leading candidate, it adds a small, independently-verified, second contributor.
**Interpretation per the two branches posed:** acreage is identical for the overwhelming majority
of rows (supports **candidate 3**: the spread mostly originates at Phase 3+), but a genuine,
verified minority-case divergence exists in matching (a real, if small, second finding) — the
result is not cleanly one branch or the other.

**Escalating** (§5): this doesn't necessarily need a code fix — R's fallback exists for a real
reason (a genuine GDAL crash). But it changes how the "three independent implementations" claim
should be *worded* wherever it discusses acreage specifically, and it's worth the author knowing
explicitly rather than finding out from a reviewer. Recommend: correct "fully self-contained" in
both headers, and add a sentence to whichever documentation makes the independence claim, scoping
it correctly (independent area computation and matching, shared polygon extraction).

### X-05 — RETRACTED as originally written: Julia is not clean on imputation-model shape either
**Severity:** Major (upgraded from Cosmetic — this was a wrong conclusion, not just a record) · **Status:** Corrected 2026-07-25 · **Locus:** —

**Original claim (2026-07-25, same day): "Julia's `""` sentinel... is a valid non-missing
category level... gets R's correct parsing behavior without R's accidental imputation
behavior." This is wrong, or at best unverified overclaiming, and is corrected here rather than
silently edited — per the evidence standard (§4), the record should show the correction, not
hide that it was needed.**

**What actually happened (D-2, 2026-07-25):** direct NA/blank counts on the on-disk Phase 2
outputs that fed the Jun-12 run:

| | Holes NA | Ownership_Type/Course_Type NA | Acreage NA | BVPA NA | Columns with missingness |
|---|---|---|---|---|---|
| R | 9 | 1 | 4,687 | 1,064 | **4** |
| Python (pre-fix) | 0 | 0 | 4,687 | 1,064 | **2** |
| Julia | 0 | **1** | 4,687 | 1,064 | **3** |

Julia's `Ownership_Type` **does** have a real missing value — 1 row (the same
`Turtle Creek Golf Club, FL` "CLOSED|" row as R and, before the fix, as R alone was thought to
have). `READ`, `Jl_Phase2_Acreage_Matched.csv`: the field is written as a **fully empty,
unquoted CSV field** (`,,`), not the literal string `""`.

**Promoted to `VERIFIED` (author confirmation, 2026-07-25):** `Phase_3.jl:50`
(`CSV.read(input_csv, DataFrame)`) passes no `missingstring` argument. `CSV.jl`'s documented
default is `missingstring=""` — an empty field is read as `missing` by default, with no override
in this call. So the empty-field evidence above is conclusive, not merely suggestive:
`Phase_3.jl:63` (`categorical(acreage_df.Ownership_Type)`) preserves the `missing`
(`CategoricalArrays.jl` supports `missing` natively) and `Phase_3.jl:74`
(`mice(imp_df, m=m_datasets, iter=10)`) encounters it as real missingness in `Course_Type` —
confirmed as a third imputation target in Julia, by the same general "MICE imputes every column
with missingness" default shared by `mice`, `miceforest`, and `Mice.jl` alike.

**Revised picture:** R and Julia share the *same* root cause (both use the raw
leading-parenthetical regex, both fail identically on the one `CLOSED|`-prefixed row) and, most
likely, the *same* consequence (an accidental extra imputation target) — Julia isn't structurally
clean, it's structurally identical to R on this axis, just with one fewer accidental target
(`Holes` fabricates cleanly in Julia; it doesn't in R). Python (pre-fix) was the only language
with a *fully* 2-variable imputation model for the Jun-12 run — but only because its Ownership
extraction was *wrong* (**P1-05**) in a way that happened to never leave a missing value. Once
**P1-05**'s fix is carried through a Python re-run, Python's `Ownership_Type` will *also* pick up
this same 1-row `NA` (correctly, this time) — and `Phase_3.py`'s `miceforest.ImputationKernel`
call (`Phase_3.py:85-89`) has no explicit `variable_schema`, so by the same shared MICE-library
default, Python would then **also** inherit a 3rd accidental imputation target, ironically as a
direct consequence of fixing the parsing bug. **No language is a clean reference on this axis
without an explicit `predictorMatrix`/`variable_schema` fix in Phase 3** — the fix belongs at the
Phase 3 MICE-call level (constrain which columns are imputed), not at the Phase 1 parsing level,
and applies to R, Julia, and (once P1-05 is carried forward) Python alike.

### X-07 — `Data/Python` vs `Data/python` path casing, live in 5 master scripts, fixed
**Severity:** Minor (Windows-masked; would break on a case-sensitive filesystem) · **Status:** Fixed 2026-07-26 · **Locus:** Code

Parity Audit **G-8**. `VERIFIED`: every phase's actual on-disk Python output directory is
lowercase `python` (`ls -d Data/*/` confirmed for Phase 1, 2, 4, 5). `Phase_3.py` and
`Phase_6.jl`'s real code already used lowercase correctly. But `Phase_1.py:28`, `Phase_2.py:
28-30`, `Phase_2.jl:29`, `Phase_4.py:26,28`, and `Phase_5.py:35,42,49,63` all constructed the
path string with capitalized `"Python"` — silently masked on this Windows machine only because
NTFS/Windows path lookups are case-insensitive; the exact same code would fail to find (or would
create a second, divergent) directory on Linux/macOS/most CI runners. Notably this included
`Phase_2.jl:29` — Julia's own hardcoded path to **Python's** Phase 2 polygon output
(`Py_Phase2_OSM_Golf_Polygons.gpkg`), the cross-language dependency file **X-06** already
established Julia hard-depends on — so this wasn't only a Python-internal-consistency issue.

**Fixed:** all 8 occurrences across the 6 files above changed to lowercase `"python"`, matching
the actual directory and `Phase_3.py`/`Phase_6.jl`'s existing correct convention. Also corrected
`Phase_6.jl:4,7` (**G-7**): the header comment said "scripts 5, 6, and 10" (missing 11-14) and
referenced `Data/Python` — now reads "scripts 5, 6, and 10-14" and `Data/python`. Purely
mechanical, output-path-only changes — no computed value, seed, or algorithm touched.
`(Legend/display strings like Phase_6.jl:146,2078` and `Phase_6.R`'s `"Python"` plot-label text
were left alone — those are human-readable labels, not directory paths.)

### X-08 — No pinned environment in any of the three languages; `Phase_1.jl`'s `XLSX` dependency was never installed on this machine
**Severity:** Major (blocks Phase 1 entirely, currently) · **Status:** Fixed 2026-07-27 (all three languages, freeze blocker per Gate 3) · **Locus:** Environment

Found during the dress-rehearsal cascade, `VERIFIED` by execution. `Phase_1.jl:17` does
`using CSV, DataFrames, GeoDataFrames, ArchGDAL, Downloads, XLSX, Printf, Statistics` — `XLSX` is
needed to read `2024 - FHFA June 20 Land Prices.xlsx`. Running `Phase_1.jl` from a clean
invocation failed immediately: `ArgumentError: Package XLSX not found in current path.` **This
project has no `Project.toml`/`Manifest.toml` anywhere in the repository** — grepped, none found
— so every Julia master script runs against this machine's single global environment
(`~/.julia/environments/v1.12`), and that environment's package list (checked earlier this audit:
`ArchGDAL, CSV, CairoMakie, CategoricalArrays, ..., Mice, ...` — 20 packages) never included
`XLSX`. Every *other* Julia master script (`Phase_2.jl` through `Phase_6.jl`) uses only packages
already present, so this was invisible until Phase 1 specifically was executed.

**Fixed (environment-level, not a code change):** ran `Pkg.add("XLSX")` against the global
environment. `Phase_1.jl` then ran to completion. **Not fixed: the underlying reproducibility
gap.** Without a committed `Project.toml`/`Manifest.toml`, there's no record of which Julia
package versions the pipeline was validated against, and no way for a different machine (or a
future environment reset on this one) to reproduce the same dependency set automatically — the
exact failure mode just hit here. Recommend `Pkg.activate(".")` + a committed
`Project.toml`/`Manifest.toml` for the whole project as a post-freeze infrastructure item; out of
this audit's scope to add unilaterally (touches every Julia master script's invocation
convention, a structural change beyond a parity fix).

**Reclassified as a freeze blocker (author decision, Gate 3, 2026-07-27) — fixed, all three
languages, same day.** Unpinned environments make the frozen cascade non-reproducible on any
other machine or after any environment reset on this one; the `XLSX.jl` gap was the symptom, not
the whole problem — R and Python had exactly the same exposure, just no missing-package crash
yet to reveal it.

- **Julia:** `Project.toml`/`Manifest.toml` generated at `2 - Work/` (repo Julia-project root),
  pinning the 16 packages actually used across `Phase_1..6.jl` (`ArchGDAL 0.10.11`,
  `CSV 0.10.16`, `CairoMakie 0.15.11`, `CategoricalArrays 1.1.1`, `Colors 0.13.1`,
  `DataFrames 1.8.2`, `Distributions 0.25.126`, `GLM 1.9.5`, `GeoDataFrames 0.4.2`,
  `Mice 0.4.1`, `XLSX 0.12.0`, plus stdlibs) to the exact versions already validated by the
  2026-07-26 dress-rehearsal cascade — not whatever the registry currently resolves to. Wired in:
  every master script now runs `Pkg.activate(normpath(joinpath(@__DIR__, "..")); io = devnull)`
  as the first line of its `LIBRARIES` section, before any `using`. `VERIFIED` by execution: a
  standalone activation+load test against `Phase_2.jl`'s dependency set succeeded, confirming
  `Base.active_project()` resolves to `2 - Work/Project.toml` when run as a real file (this
  matters — an inline `julia -e` test initially gave a false pass by resolving `@__DIR__` to the
  shell's `pwd()` instead of the script's own location; caught and re-verified against a real
  file before trusting it).
- **R:** `renv::init(bare = TRUE)` + `renv::hydrate()` (linked 200 already-installed packages
  into a project-local library, auto-installed 9 more that were used but not yet present:
  `janitor`, `osmdata`, `pacman`, `viridis`, + transitive deps) + `renv::snapshot()` at `2 - Work/`
  produced `renv.lock` pinning **209 packages** (every `library()` call across all six master
  scripts and their real dependency trees, via static analysis of the `.R` files). Wired in: each
  master script now runs a small `local({...})` bootstrap as the first thing in its `LIBRARIES`
  section — locates its own file path via `commandArgs()` (no package dependency, so it works
  before any `library()` call), derives the project root two directories up, sets
  `Sys.setenv(RENV_PROJECT = proj_dir)`, then `source()`s `renv/activate.R`. **The explicit
  `RENV_PROJECT` env var is load-bearing, not optional** — `VERIFIED` by execution: the first
  attempt (without it) silently bootstrapped a *second*, wrong, empty renv project rooted at
  whatever directory `Rscript` happened to be invoked from, rather than the real one at `2 - Work`
  — caught by inspecting `Sys.getenv("RENV_PROJECT")` and `.libPaths()` after activation, not
  assumed correct. Re-verified after the fix: `.libPaths()` correctly resolves to
  `2 - Work/renv/library/...` and a full Phase 1 `library()` load succeeds against packages
  installed there, not the machine's personal library. `renv`'s own `renv/.gitignore`
  (auto-generated) already excludes `library/` (574 MB) from version control; `renv.lock`,
  `.Rprofile`, and `renv/activate.R` are small and trackable.
- **Python:** `pip list --format=freeze` against the current environment (already minimal and
  project-dedicated, 34 packages, no unrelated system pollution) written to
  `2 - Work/requirements.txt`, pinning `geopandas 1.1.3`, `lightgbm 4.6.0`, `miceforest 6.0.5`,
  `numpy 2.4.6`, `osmium 4.3.1`, `pandas 3.0.3`, `pyogrio 0.12.1`, `pyproj 3.7.2`,
  `scipy 1.17.1`, `shapely 2.1.2`, `statsmodels 0.14.6`, and all transitive deps, Python 3.13.13.
  Not wired into the scripts at runtime — Python has no per-script self-activation idiom
  equivalent to Julia's `Pkg.activate`/R's `renv/activate.R`; `requirements.txt` is the
  install-time contract (`pip install -r requirements.txt`), consistent with standard Python
  practice.
- **No `Pkg.add()` calls exist in any Julia master script** (`Phase_1..6.jl`) — grepped before
  touching anything. The `XLSX.jl` install earlier this audit was run interactively against the
  global environment, never written into a script, so there was nothing to remove per the
  original instruction. The only `Pkg.add()` calls in the repo are in
  `Bulk Tests/Julia/{parameter_pooling,model_fitting}.jl`, each already self-flagged in-code as
  `# [OUTSTANDING ISSUE] runs on every execution -- remove once packages installed` by whoever
  wrote them — pre-existing, out of master-script scope, left alone.

### X-09 — Every live network fetch in the master pipeline vendored; no master script performs a network call at run time
**Severity:** Major · **Status:** Fixed 2026-07-27 · **Locus:** Code / Environment

Gate 3 (2026-07-26) asked for confirmation that RUCC (**A-3**/**P1-04**) was the only live fetch in
the pipeline. It was not — a full grep of every master script for URL literals, `Downloads`/
`tigris`/`pygris` calls found **five** distinct live-fetch sites across three languages:

1. **RUCC 2023 CSV** (`Phase_1.R`, `Phase_1.py`) — live USDA ERS URL. Julia already read a local
   mirror (**P1-04**).
2. **County boundary, full-resolution TIGER/Line** (`Phase_1.R`, `Phase_1.py`, `Phase_1.jl`) — the
   FIPS-join boundary source, the same one **P1-07**'s fix depends on.
3. **County boundary, cartographic (cb=TRUE, Oahu test)** (`Phase_5.R`, `Phase_5.py`) — a
   *different* Tigris/pygris call from #2, used only for the Honolulu County intersects test.
4. **County + state boundary, cartographic 500k (GENZ2022)** (`Phase_6.R`, 5 call sites: 3 county,
   2 state) — used for the national/state choropleth maps. The **state** boundary fetch
   (`tigris::states(cb=TRUE)`, `Phase_6.R:335,1311`) was not on the author's original list and is
   recorded here as the "found one you hadn't listed" item Gate 3 asked for.
5. **Tigris Area Landmarks** (`Phase_2.R` Tier 2) — removed entirely, not vendored; see **P2-03**.

**Correction to the original risk framing (author correction, 2026-07-27):** the initial write-up
described #2/#3's risk as the underlying *content* shifting between runs (Census republishing a
different county-boundary vintage). That overstates it — `tl_2022_us_county.zip` and
`cb=FALSE, year=2022` both pin the **2022 TIGER vintage explicitly**, and Census publishes new
vintage years alongside old ones rather than rewriting a published year's file in place. The
realistic failure mode for #2–#4 is **availability** (a renamed URL, a Census outage, or a network
hiccup on cascade day causing a hard crash, same failure class as **A-3**'s RUCC risk) — not
silent content drift. Corrected here rather than left overstated in the register.

**Fixed — vendored, one policy, no per-fetch triage (author decision, Gate 3, 2026-07-27):**
- **RUCC:** R and Python repointed from `RUCC_URL` (live) to `RUCC_CSV`
  (`00 - Data Sources/Secondary/2023-rural-urban-continuum-codes.csv`) — the same file Julia
  already used, byte-identical per **A-3**'s earlier MD5 check.
- **County boundary (#2):** `VERIFIED` this file was *already vendored* on disk
  (`00 - Data Sources/Original Data/tl_2022_us_county.{shp,shx,dbf,prj,cpg}`) — Julia's Phase 1
  already checked-then-downloaded-only-if-missing, so it was never actually re-fetching on a
  cache hit; R and Python were the two languages genuinely hitting the network every run. Both
  repointed to `sf::st_read()`/`gpd.read_file()` on the local shapefile; schema confirmed
  identical to what `tigris::counties()`/`pygris.counties()` return (`STATEFP`, `GEOID`, `NAME`,
  3,235 rows), `VERIFIED` by reading it directly. Julia's dead `Downloads`-fallback branch removed
  (now hard-errors like R/Python if the vendored file is missing, rather than silently attempting
  a live download) — closes the file-existence check into the same `for path in (...)` guard
  Julia's other inputs already use.
- **County boundary (#3, Phase 5 Oahu test):** repointed R and Python from the live `cb=TRUE`
  fetch onto the **same** vendored `tl_2022_us_county.shp` as #2 (filtered to
  `STATEFP=="15" & NAME=="Honolulu"`), rather than vendoring a second, different file — this also
  unifies Phase 5's boundary source with Phase 1's, and is the file Julia's fix (below) now reads
  too. `VERIFIED` via a standalone Julia diagnostic read: `STATEFP`/`NAME` are `String`-typed as
  expected, exactly one Honolulu County row present.
- **County + state boundary (#4, Phase 6):** downloaded fresh (the on-disk `cb_2022_us_county_20m`
  files were a *different, coarser* resolution left over from Phase 1's pre-**P1-07** boundary
  method — confirmed by checking `tigris`'s actual default resolution for a bare `cb=TRUE` call is
  `500k`, not `20m` — vendoring the wrong-resolution file on disk would have been a silent,
  wrong fix). Fetched `cb_2022_us_county_500k.zip`/`cb_2022_us_state_500k.zip` from Census GENZ2022
  (`VERIFIED` HTTP 200, schema-checked: `STATEFP`/`GEOID`/`NAME`/`STUSPS` present, 3,235
  county / 56 state rows), stored under `00 - Data Sources/Secondary/`, all 5 `Phase_6.R` call
  sites repointed to `st_read()` on the local files.
- **Provenance recorded in-repo** at each repointed constant, as source URL + retrieval date in a
  code comment (the master scripts are the tracked, non-gitignored files; the vendored data files
  themselves fall under the repo's existing `*.csv`/`*.zip` gitignore patterns, same as the
  pre-existing RUCC mirror already did).

**Result: after this fix, no master script (`Phase_1..6.{R,py,jl}`) performs a network fetch at
run time.** All spatial/administrative reference data is read from `00 - Data Sources/` on disk.

---

## Phase 1 — Spatial Parsing & Economic Baseline Valuation

### P1-01 — `extract_holes()` fabricates `Holes = 18` on regex failure (Python AND Julia)
**Severity:** Major · **Status:** Fixed at source (2026-07-25) + belt-and-braces schema fix · **Locus:** Code

`Phase_1.py:69` and `Phase_1.jl:79` both return `18` as the default when the holes regex fails
to match; R (`Phase_1.R:76`) returns `NA` for the identical input. **Correction to the original
framing:** this is not Python-only — Julia does the same thing. It is R alone against both its
brothers, and R is the one that's right. The Phase 1 documentation logs this as a "minor
cross-language inconsistency" — it is more than that:

1. `Holes` is a **regressor** in the Phase 4 model, not a passive field.
2. `Holes` is a **predictor** in the Phase 3 MICE model, so a fabricated value propagates into
   imputed acreage and BVPA for other courses.
3. R's `NA` rows are visible to MICE and get imputed; Python's and Julia's silently become a
   hard 18. The languages aren't handling missingness differently — Py/Jl aren't recording it.

**Resolved (Parity Audit A-2, 2026-07-24):** `VERIFIED` — counted against the raw 16,297-row
CSV: Py/Jl fail on **9 rows (0.055%)**, R fails on the *same* 9 rows (0 asymmetric failures).
Inspected all 9: 7 have true value 18 (fabricated default happens to be correct), 1 is a 36-hole
course (Streamsong, FL — fabricated 18 understates by half), 1 is an ambiguous two-course combo.
None are driving ranges/par-3s/practice facilities — the hypothesized directional bias toward
undercounting small-format courses does not occur in this dataset. **The row count alone is a
footnote, not a finding.**

**But a deeper mechanism was found underneath it.** `Holes` is a `PREDICTOR_COLS` member in all
three (never `IMPUTE_COLS`), but because Py/Jl's `extract_holes()` never returns missing,
`Holes` reaches their Phase 3 `mice` calls with zero NAs — a purely-observed covariate. R's 9
real NAs, combined with `Phase_3.R:125-131`'s `futuremice(method = "rf", ...)` being called with
`method` as a **bare string** (no `predictorMatrix` override), mean `mice`'s documented default
behavior applies `"rf"` to *every* column with missingness — so `Holes` becomes a **second
imputation target in R alone**, jointly modeled with (and predicted by) `final_acreage` and
`Baseline_Value_Per_Acre` across `maxit = 10` FCS iterations. Python/Julia never do this. This
is a genuine methodological divergence in the imputation model structure, not just a value
difference, on top of the original NA-vs-18 finding.

**Resolved at source (2026-07-25, author call: fix both layers, not just one).**

**(1) Regex fix, all three languages.** `extract_holes()` now tries, in order: the existing
strict `"(N Holes)"` pattern; a new combo pattern for two-course facilities,
`"(N Holes & M Holes)"` → sum to `N+M`; a new bare-digit fallback `"(N)"` with no "Holes" text,
restricted to the substring before the first comma (so a phone area code or zip can never
match). Validated against all 16,297 raw rows (scratchpad `validate_p1_fixes.py`, read+count
only) before touching any source file:
- **0 regressions** — no row that previously parsed correctly changes value, in either the
  Py/Jl-style or R-style engine.
- Py/Jl: 2 rows visibly change value (Streamsong FL: `18`→`36`, correcting a wrong fabrication;
  Roseburg OR combo: `18`→`27`, correcting an ambiguous fabrication). The other 7 originally-
  fabricated rows already read `18` under the old fallback and still read `18` now — but for the
  right reason (actually parsed) instead of guessed. **Fabricated-18-fallback count: 9 → 0.**
- R: all 9 `NA` rows become real parsed values (`18`×8, `36`×1 via bare-digit;
  `27`×1 via combo). **`NA` count: 9 → 0.**
Applied to `Phase_1.py:66-79`, `Phase_1.jl:76-89`, `Phase_1.R:63-76` (new named helper function,
added to the previously-empty `# === 3. FUNCTIONS ===` section, called via `map_dbl()` in the
`mutate()` — R's existing inline-mutate convention preserved for `Course_Name`/`State_Abbr`,
diverged from only because this logic no longer fits a one-line `str_extract`).

**(2) Schema declared explicitly, all three, belt-and-braces.** Even with `Holes` (and
`Ownership_Type`, **P1-05**) no longer carrying stray NAs, an explicit schema was added so a
*future* stray NA anywhere in the predictor set can never silently become an accidental
imputation target again:
- `Phase_3.R:110-131` — `futuremice()`'s bare `method="rf"` replaced with a named per-column
  vector (`method_vec`), `""` for every predictor, `"rf"` only for `IMPUTE_COLS`.
- `Phase_3.py:85-90` — `mf.ImputationKernel(...)` now takes `variable_schema=IMPUTE_COLS`.
- `Phase_3.jl:77` — `mice(...)` now takes `visitsequence=string.(IMPUTE_COLS)` (per `Mice.jl`'s
  documented API — "you can skip the imputation of a column by removing it from the
  `visitsequence`" — confirmed against the package's own README, `tom-metherell/Mice.jl`).
  **Correction (2026-07-25, caught during D-5's execution):** the original edit passed
  `visitsequence=IMPUTE_COLS` directly; `IMPUTE_COLS` is declared as `Vector{Symbol}`
  (`Phase_3.jl:37`, used elsewhere for DataFrame column selection), but `Mice.jl`'s `mice()`
  signature requires `visitsequence::Vector{String}` (`READ`, confirmed against
  `Mice.jl/src/Mice.jl:57,92,132`) — the original fix would have thrown a `TypeError` on the
  first line of any real run. Caught only because D-5 actually executed the scoped-exception
  reproducibility test; would not have been caught by reading alone. Fixed to `string.(...)`.
- `Phase_3.R:113-115`'s "Variables to be imputed: {IMPUTE_COLS}" print statement, previously a
  claim the code didn't honor, is now accurate as a direct consequence of the method-vector fix.
No behavior change expected on data that's already clean (which, after (1), is all of it) — this
closes the trap for any future Phase 1 change that leaves a stray NA in a predictor column,
in all three languages, not just R.
**Not regenerated:** per `CLAUDE.md` §2.1, none of the on-disk outputs (any language, any phase)
were rerun. All existing data remains from before these fixes.

**Downstream verification, `VERIFIED` (Parity Audit E-2, 2026-07-25):** checked whether this
bug's fingerprint is still visible at the Phase 4 boundary, against the current (pre-fix, June-12
run) on-disk `*_Imputed_Dataset_1.csv` files. Replicated Phase 4's exact drop logic
(`dropna`/`complete.cases` on `Log_Opportunity_Cost`, `Holes`, `Baseline_Value_Per_Acre`,
`county_type`): **0 rows dropped in all three languages** — every language reaches Phase 4 with
zero missingness in the model columns. On its face this looks like clean agreement. It isn't, for
the same reason **P1-01** exists: **R's zero-missingness is an artifact of the very bug this item
tracks** — the 9 originally-`NA` `Holes` values in R's raw Phase 1 output were silently filled in
by `mice()`'s pre-fix default full-column imputation (this is `A-2`'s original finding), so by the
time this June-12 run reached Phase 3's output, R's `Holes` column was already complete, not
missing. Python and Julia's `Holes` is complete for an unrelated reason — their old
`extract_holes()` fabricated `18` rather than ever emitting a null. **Same outcome (0 drops, N
match on model columns), three different mechanisms, one of them a bug this audit exists to
catch** — exactly the trap `CLAUDE.md` §1 describes. Going forward, after today's **P1-01** source
fix (Holes now parses correctly with `NA` as R's honest fallback, confirmed 0 raw `Holes` `NA`s in
the current 16,297-row dataset) and the **D-2** schema fix (no more accidental full-column
imputation), the *next* real run should reach the same 0-drop outcome for the right reason in all
three — not verified, since that requires the prohibited full cascade re-run.

Row-count note (separate, already-tracked mechanism): R and Julia's `Dataset_1` both have 16,292
rows; Python's has 16,297 — the same 5-row gap **B-6**/**P1-06** already traced to Python's Phase
2 having no course-level dedup. Confirmed here that the gap survives unchanged all the way through
to the actual Phase 4 regression sample size, not just Phase 2's intermediate output.

### P1-02 — `course_id` and address fields absent from Python's Phase 1 output
**Severity:** Minor · **Status:** Won't fix (verified harmless) · **Locus:** Code

Python's output lacks `course_id`, `Address`, `City`, `State_Abbr`, `Zip_Code`. Phase 1 docs
flag this as the "highest-risk gap"; Phase 2 §2C then resolves it — `Phase_2.py` joins spatially
only and never touches `course_id`. Confirmed harmless **for the current pipeline**, but it's a
tripwire for any future Python-side join.

### P1-03 — `Course_Name` content differs between R and Py/Jl
**Severity:** Minor · **Status:** Won't fix (documented) · **Locus:** Code

R applies `str_remove(Name_State, "-.*$")`; Julia and Python carry the raw suffixed string
(`"Seamountain Golf Course"` vs `"Seamountain Golf Course-HI"`). No downstream join uses
`Course_Name`, so it's inert — but it means any human-readable cross-language spot-check on
course names will appear to fail.

### P1-04 — RUCC source split: live URL (R, Python) vs local mirror (Julia)
**Severity:** Minor · **Status:** Fixed 2026-07-27 — see X-09 · **Locus:** Both

`Phase_1.R:45` and `Phase_1.py:35` fetch RUCC 2023 live from a USDA ERS URL; `Phase_1.jl:32`
reads a local mirror (`00 - Data Sources/Secondary/2023-rural-urban-continuum-codes.csv`).

**Resolved (Parity Audit A-3, 2026-07-24):** `VERIFIED` — fetched the live URL today (HTTP 200,
9,704 lines) and diffed it against the local mirror: byte-identical, same MD5. No content
divergence exists right now. Neither live fetch (`Phase_1.R:165-169`, `Phase_1.py:160`) is
wrapped in error handling — a fetch failure crashes the script rather than degrading silently.
Read (not run) the three already-on-disk `*_Phase1_Baseline_Golf_Valuation.csv` outputs:
`FIPS`/`RUCC_2023`/`county_type` all show **0 blanks** in all three languages — full join
coverage currently, nothing for a silent-imputation mechanism to act on today.

**Residual risk, not a current bug:** `county_type` sits in `Phase_3.R`'s `predictors` vector
(`Phase_3.R:110`) exactly like `Holes` (**P1-01**) and `Ownership_Type` (**P1-05**) do. If a
future live fetch (R or Python) ever degrades partially — ERS renames/moves the file, or a proxy
returns a truncated-but-parseable response — R would silently promote the resulting
`county_type` NAs to a `futuremice` imputation target by the same scalar-`method` mechanism as
**P1-01**, while Julia (mirror) is unaffected. **Recommendation (escalated, §5):** vendor the
RUCC CSV for all three languages with retrieval date and provenance recorded in-repo before the
frozen re-run — removes both the hard-crash risk and the latent silent-imputation risk in one
move.

**Fixed 2026-07-27 (Gate 3/X-09):** R and Python repointed to the local `2023-rural-urban-
continuum-codes.csv` mirror Julia already used. No live fetch remains in any language.

### P1-05 — `extract_ownership()`: Python collapses `"Semi Private"` into `"Private"` for ~10% of courses
**Severity:** Major · **Status:** Fixed (2026-07-25) · **Locus:** Code

`Phase_1.py:60-65` lowercases the whole `Details` string and substring-matches, in fixed
priority order, against `("public","private","municipal","military","resort")`. Julia
(`Phase_1.jl:71-74`) and R (`Phase_1.R:75`) both extract the raw leading parenthetical verbatim.

**Found (Parity Audit B-1, 2026-07-24):** `VERIFIED` against the raw 16,297-row CSV: **1,663
rows (10.2%) disagree**, almost entirely because `"private"` is a substring of `"semi private"`
— Python silently discards the "Semi" qualifier on every such course. Cross-checked the on-disk
`Ownership_Type` columns (2026-06-12 run): R and Julia both show a `Semi Private` category of
1,661 courses; **Python's output has no such category at all** — those 1,661 courses (plus 1
more from the B-6 dedup set) are folded into Python's `Private` bucket, which reconciles exactly
(`2716 + 1661 + 1 = 4378`, Python's actual count). This is 0 rows of failure-sentinel divergence
(Python's `"Unknown"` never fires here) — it's pure miscategorization, a different failure mode
from **P1-01**.

Also carries the same second-imputation-target mechanism as **P1-01**: R/Julia's leading-paren
regex fails on exactly 1 row (`Turtle Creek Golf Club, FL` — a `CLOSED|` prefix breaks the `^\(`
anchor; Python's substring scan still recovers `"Public"` for it). That 1 `NA` in R's
`Ownership_Type` makes it a third silent `futuremice` imputation target in R alone, same root
cause as **P1-01**.

`Ownership_Type` is a Phase 3 `PREDICTOR_COLS` member in all three (not a Phase 4 regression
term), but that undersells the impact: MICE imputes `osm_acreage`/`final_acreage` for 28.8% of
courses and `Baseline_Value_Per_Acre` for a further ~6.7% (per **P2-02**). Every one of Python's
M=100 random-forest imputations was splitting on a 3-category `Ownership_Type` where R and Julia
split on 4. This doesn't move a published *coefficient* so much as it can move the pooled
**national total** — the thesis's headline number.

Also a live candidate for part of the Holes-coefficient spread that
`01_-_Phase4_Documentation.md:485-487` currently attributes entirely to R's `final_acreage`
(OSM+Tigris) vs Python/Julia's `osm_acreage` (OSM-only) — see the new cross-cutting note below;
**P2-03** found R's Tigris tier currently contributes zero rows, which weakens that explanation
for the current on-disk data.

**Fixed 2026-07-25 (pass 1):** the completed Python M=100 run already on disk (2026-06-12, see
**X-04**) was invalid regardless — it also carries **P1-01** (fabricated `Holes`). Per roadmap
§0.1, a correctness fix is not held hostage to output already scheduled for deletion by the
frozen re-run. Changed `Phase_1.py:60-64` from substring-keyword matching to the same
leading-parenthetical regex R/Julia used (`^\(([^)]+)\)`), verbatim, no case normalization —
structurally removes the substring-collision hazard rather than reordering the keyword list.
Verified 0/16,297 disagreements against R/Julia's extraction after the fix.

**Fixed 2026-07-25 (pass 2, same day — all three languages this time):** D-2 then found R and
Julia both still had the 1-row `CLOSED|`-prefix `NA`/`missing` this entry originally described as
R-only-adjacent (**X-05**'s correction). Root cause: the leading-paren regex was anchored to
position 0 (`^\(`), which the `"CLOSED|(Public)..."` row defeats in all three. Fixed by removing
the anchor in all three — `Phase_1.py`, `Phase_1.R:55-61` (new helper, previously inline),
`Phase_1.jl:71-74` — now searching for the first parenthetical *anywhere* in the string instead
of requiring it at position 0. Validated against all 16,297 rows: **1 row changes (Turtle Creek,
FL), 0 regressions.** `Ownership_Type` `NA`/blank count: R 1→0, Julia 1→0, Python already 0.
Combined with **P1-01**'s belt-and-braces MICE-schema fix, `Ownership_Type` is now (a) parsed
correctly in all three and (b) structurally guaranteed to stay a predictor even if some future
row does leave it missing. **The on-disk outputs were not regenerated** (§2.1) in either pass —
all three languages' data remains stale pending the frozen cascade re-run.

### P1-06 — Python has no course-level deduplication; explains B-6's "+5 rows" (not a `geopandas` default)
**Severity:** Minor · **Status:** Fixed 2026-07-27 (Decision 4) · **Locus:** Code

`Phase_1.R:83-89` and `Phase_1.jl:171-176` both deduplicate `courses_df` on
`(round(lat,4), round(lon,4), Course_Name)` before any spatial join. `Phase_1.py` has **no
course-row dedup step at all** — its only `drop_duplicates` calls (`:137,147,165`) are on the
USDA/FHFA/RUCC lookup tables. The "+5 Python rows" previously logged as "documented as a
`geopandas` dedup default, plausible, not verified" is neither: there is no dedup default at
play, there's an absent dedup step.

**Resolved (Parity Audit B-6, 2026-07-24):** `VERIFIED` — found exactly 5 duplicate groups in
the raw CSV by that key, all genuine same-address, same-course double-listings (e.g. The
Boulders Resort Golf Club, Carefree AZ, listed once as `(Private)` and once as `(Public)` at
identical coordinates) — real source duplicates, not 5 distinct courses. R and Julia use
*different* tie-break rules (`arrange(desc(Holes))` vs first-encountered) but happen to select
the same winner in all 5 current cases — a latent, currently-harmless divergence (H-5 class).
Full row-count ledger closes exactly to the row once **P1-05**'s mislabeling is folded in (see
Parity Audit B-6 for the arithmetic). All 5 "extra" Python rows are in `county_type = Urban`
counties. **Escalated:** the fix (add the same dedup to Python) is straightforward, but R and
Julia's tie-break rules aren't actually the same rule, they just haven't collided yet — pick one
documented rule for all three rather than porting either incidentally.

**Correction (Decision 4 review, 2026-07-27) — the "different tie-break rules" premise above was
wrong; R and Julia already agreed.** Re-reading `Phase_1.jl:189-192` directly (not from memory of
the earlier B-6 characterization) shows `sort!(g, :Holes, rev=true)` immediately before
`first(g)` — this is the *same* Holes-descending rule as R's `arrange(desc(Holes)) |> slice(1)`,
not "first-encountered, original row order" as originally logged. `VERIFIED` via `git show HEAD`
that this line predates this entire audit session — it was never a genuine R/Julia divergence,
just a mischaracterization in the original B-6 write-up that went uncaught until this review.
Only Python needed a fix.

**Fixed (Decision 4, 2026-07-27):** added the same `(round(lat,4), round(lon,4), Course_Name)`
dedup to `Phase_1.py`, sorted by `Holes` descending (stable `mergesort`, matching R's/Julia's
stable-sort tie handling) before `drop_duplicates(keep="first")` — the same rule R and Julia
already implemented, not a new 3-way standardization. `VERIFIED` by a standalone diagnostic
against the raw 16,297-row CSV: **16,292 rows after dedup, 5 duplicate groups removed**, exactly
matching R/Julia's row count and the same 5 groups B-6 originally identified. Not executed beyond
the diagnostic (§2.1) — documents the source fix, not a re-verified pipeline output.

**Bonus, logged in passing:** one row in the raw CSV (`Turtle Creek Golf Club, Rockledge FL`) has
`Details = "CLOSED|(Public) (18 Holes), ..."` — a course marked closed in the source data that is
still counted in the active 16,297/16,292-course valuation total in all three languages. Single
row, `Won't fix (verified low-impact)` unless the author wants closed courses excluded on
principle — flagging for awareness, not proposing a filter unilaterally.

### P1-07 — FIPS boundary fix: confirmed now applied in all three languages, not R-only
**Severity:** Cosmetic (corrects a stale record, not a code defect) · **Status:** Confirmed · **Locus:** Docs (Notes.md)

The session log in `999 - Late Stage/Notes.md` (2026-05-14 entry) states the county-boundary
FIPS fix (`cb=FALSE`, full-resolution TIGER/Line instead of the coarse 20m cartographic
boundary) "was applied to `Phase_1.R` only," and that Python/Julia "remain FIPS-NA: 34 →34
(unchanged)."

**Checked (Parity Audit B-5, 2026-07-25):** `READ` — this is no longer true of the code, and
per the on-disk data, has not been true for a while:
- `Phase_1.R:105` `counties(cb = FALSE, year = 2022, ...)`
- `Phase_1.py:97` `counties(cb=False, year=2022)`
- `Phase_1.jl:35,37,206,212` downloads `tl_2022_us_county.zip` directly from
  `www2.census.gov/geo/tiger/TIGER2022/COUNTY/` — TIGER/Line full resolution, not the coarse
  `cb_2022_us_county_20m` cartographic file
All three use equivalent fine-resolution boundary sources. Cross-checked the on-disk
`*_Phase1_Baseline_Golf_Valuation.csv` for all three languages (Jun 12 / May 18 / May 14 —
mismatched vintages, see caveat below): **0 blank `FIPS` in all three**, not 34/0/0 as the stale
note implies. FIPS zero-padding is also consistently applied (`str_pad(...,5,"0")` R,
`.zfill(5)` Python, `lpad(s,5,'0')` Julia).

**Caveat:** the three baseline files checked are not a synchronized same-day triple (R
2026-06-12, Python 2026-05-18, Julia 2026-05-14) — same limitation as **A-3**'s check. Confirms
FIPS coverage was complete as of each language's most recent available snapshot, not necessarily
simultaneously. `Notes.md` is scratch/session-log per its own header, not a tracked source of
truth — flagging here rather than editing it directly (out of scope, and it isn't one of the two
files this audit is permitted to edit). Recommend the author prune or update that stale entry.

### P1-08 — Dual-proxy RUCC boundary (1–3 → FHFA/Urban, 4–9 → USDA/Rural): verified clean, one dormant edge case
**Severity:** Minor (dormant, not currently reachable) · **Status:** Confirmed · **Locus:** Code

**Checked (Parity Audit B-7, 2026-07-25):** `READ`, character-by-character, all three:
`Phase_1.R:182-185` (`RUCC_2023 %in% 1:3` / `%in% 4:9` / else `NA`), `Phase_1.py:168-171`
(`.isin([1,2,3])` / `.between(4,9)` [pandas default `inclusive="both"`] / else `None`),
`Phase_1.jl:347-358` (`rucc_val in 1:3` / `in 4:9` / else `missing`). The 3-vs-4 boundary (where
FHFA flips to USDA, a ~60× per-acre swing) is inclusive on both ends in all three, covers the
full 1–9 RUCC range with no gap or overlap, and is implemented identically. Missing/unclassified
`RUCC_2023` correctly falls through to a missing `county_type` and a missing
`Baseline_Value_Per_Acre` in all three (verified the R `%in%`/`case_when` NA-handling
specifically, since `NA %in% x` in R is `FALSE` rather than `NA` — a common R gotcha — but
`case_when`'s fall-through still lands on the correct `NA_character_` result either way).

**One dormant divergence found, not currently live:** on a non-integer `RUCC_2023` value, R's
`as.integer(Value)` (`Phase_1.R:173`) truncates (e.g. `3.5` → `3` → classified `Urban`), while
Python's `pd.to_numeric` (`Phase_1.py:161`) would preserve `3.5` and have it match neither
`isin([1,2,3])` nor `.between(4,9)` (unclassified), and Julia's `tryparse(Int64, ...)`
(`Phase_1.jl:339`) would fail outright on a non-integer string (also unclassified). **Checked the
actual RUCC source data** (`00 - Data Sources/Secondary/2023-rural-urban-continuum-codes.csv`):
`RUCC_2023` values are the 9 clean integers `1`–`9`, nothing else — `VERIFIED` this divergence
cannot currently fire. Logged for completeness per the audit's evidence standard, not because
it's live.

### P1-09 — `Phase_1.py`'s hardcoded FHFA column name (fixed); two dead-code claims checked
**Severity:** Cosmetic · **Status:** Fixed (2026-07-25) · **Locus:** Code

**B-9, fixed:** `Phase_1.py:143` hardcoded `as_is_col = "Land Value\n(Per Acre, As-Is)"` — a
literal embedded newline, fragile to any FHFA source header reformatting. `Phase_1.R:148`
(`grep(...)`) and `Phase_1.jl:311-319` (`occursin(...)` loop, errors if absent) both already used
a dynamic match; Python was the 1-of-3 outlier. Changed to a dynamic substring match over
`fhfa_df.columns`, raising if not found — identical behavior on current data, robust going
forward. R and Julia untouched.

**B-8, checked, mixed result:** of three claimed dead-code items, two confirmed
(`Phase_1.jl:22`'s `ENV["JULIA_NUM_THREADS"]="24"` mid-script assignment is a genuine runtime
no-op; `Phase_1.R`'s loaded-but-unused `future`/`furrr`/`plan(multisession)` is genuine dead
code) and recorded only, per the original instruction, not fixed. The third — "`Downloads.
download(COUNTY_CB, COUNTY_ZIP)` passes a local path where a URL is expected" — **does not match
current code**: `COUNTY_CB` doesn't exist in `Phase_1.jl`; the actual call uses `COUNTY_URL`, a
correct, genuine URL. No fix needed; the original claim was stale or mistaken.

### P1-10 — `extract_holes()`'s ultimate fallback still returns fabricated `18` in Python/Julia, `NA` in R (dormant — P1-01's fix means it's currently unreachable)
**Severity:** Minor (dormant — 0/16,297 rows currently reach this branch) · **Status:** Open · **Locus:** Code

Surfaced while reading `Phase_1.py`/`Phase_1.jl` for **Decision 4**'s dedup fix — not part of that
task, logged per `CLAUDE.md` §6 rather than fixed inline. `READ`: after **P1-01**'s three-pattern
fix (strict `"(N Holes)"`, combo `"(N Holes & M Holes)"`, bare `"(N)"` before the first comma),
all three languages fall through to a final catch-all if none match. `Phase_1.py:88` and
`Phase_1.jl:94` both `return 18` (fabricated); `Phase_1.R:82` returns `NA_real_` (honest) — the
exact same **P1-01** pattern, just one layer deeper, at the true last-resort fallback rather than
the primary regex.

**Currently dormant:** **P1-01** already validated 0/16,297 raw rows reach this fallback after the
three-pattern fix — every row matches one of the three real patterns. So today this is inert,
same shape as **P1-08**'s dormant RUCC edge case. **Not currently live, but not structurally
impossible either** — any future row whose `Details` field doesn't match any of the three
patterns (a new source-formatting variant, e.g.) would silently diverge exactly like **P1-01**'s
original finding: Python/Julia fabricate a plausible-looking `18`, R honestly returns `NA` and
lets Phase 3's MICE schema handle it. Not fixed here — this is a "does the fallback value itself
need to match, not just the patterns before it" question, and per **P1-01**'s own precedent
(R's `NA` is the correct behavior, the two-language majority was wrong), the fix would be
changing Python/Julia's final fallback to `None`/`missing` rather than harmonizing on `18`. Flagging
for the author rather than deciding unilaterally, since it's a small but real behavioral change,
not a mechanical parity gap like **P5-07**.

### Phase 1 — noted, no issue raised

- Cross-language mean BVPA converges to within $5 ($413,695.90 / $413,699.57 / $413,700.97).
- The FIPS zero-padding fix is real, well-explained, and the highest-value catch in the project.

---

## Phase 2 — OSM Polygon Extraction & Acreage Matching

### P2-01 — `acreage_source` / primary column asymmetry is structural, not cosmetic
**Severity:** Minor (as documented) · **Status:** Fixed 2026-07-27 — see P2-03/X-02 · **Locus:** Code
**Escalates to:** **X-02**

**Resolved:** Tier 2 removed (**P2-03**), so R's `acreage_source` is now two-valued (`OSM`/
`MICE_Target`) like Python/Julia's, and `final_acreage` = `osm_acres` unconditionally. The
asymmetry this entry tracked no longer exists.

R carries a three-tier schema (`OSM` / `Tigris` / `MICE_Target`) and coalesces to
`final_acreage`; Python and Julia carry two tiers and `osm_acreage`. The mitigation in the
docs — "Phase 3 scripts filter on `acreage_source != "MICE_Target"` rather than on a positive
value" — is correct and works. The issue isn't the filter, it's that R's MICE target set is
strictly smaller and its acreage values come from a partly different source. Tracked at the
Phase 2 level as schema; tracked at **X-02** as the thing that actually bites.

**Correction (Parity Audit C-5, 2026-07-25):** the doc's *mechanism* description is wrong, even
though its conclusion is right. Grepped all three `Phase_3.{R,py,jl}` for `acreage_source` /
`MICE_Target` — **0 matches in any of them.** Phase 3 never filters on the label string at all;
it relies purely on whether `final_acreage`/`osm_acreage` is `NA`/missing, which `coalesce()`
(R) and a failed OSM match (Python/Julia) already produce directly, with no read of
`acreage_source` needed. Practical behavior is unaffected (NA-detection is equally consistent
across languages) but "Phase 3 scripts filter on acreage_source" should not be repeated as a
description of the code.

### P2-02 — "Final data profile heading into Phase 3" table is pre-fallback and contradicts everything else
**Severity:** Minor · **Status:** Confirmed · **Locus:** Docs (code is correct)

`01_-_Phase2_Documentation.md` §"Phase 2 Refinement" reports:

| Source | Count | % |
|---|---|---|
| MICE_Target | 10,834 | 66.5% |
| OSM | 5,458 | 33.5% |

Those are the **Step 1 intersect-only** numbers (5,458 direct hits, 10,834 misses). Every other
statement in the corpus — the summary, §Step 2, Phase 3's missing-data profile — uses
**11,605 matched (71.2%) / 4,687 missing (28.8%)** after the 500 m nearest-neighbour recovery
of 6,147 courses. The `03_Finalize_Acreage.R` output being described here appears to skip the
Tier-1b fallback entirely.

**Resolved (Parity Audit C-2, 2026-07-24):** `VERIFIED` by reading the actual on-disk
`Phase 2 .../Data/R/R_Phase2_Acreage_Matched_v2.csv` (mtime 2026-06-12, the file
`Phase_3.R:36-40` actually reads): `acreage_source` counts are `OSM 11,605 / MICE_Target 4,687`
— matching the post-fallback figure exactly, not the 5,458/10,834 pre-fallback one. Cross-checked
same-day Python and Julia `Data/{python,Julia}/*_Phase2_Acreage_Matched*.csv` outputs (same
2026-06-12 run): Python `OSM 11,610 / MICE_Target 4,687`, Julia `OSM 11,605 / MICE_Target 4,687`
— all three in close parity, confirming the 500 m nearest-neighbour fallback (`Phase_2.R:291-313`,
`Phase_2.py:169-185`, `Phase_2.jl:170-220`) is present and functioning identically across all
three languages, and that **R's live Phase 3 input does include the fallback tier.**

The `5,458/10,834` table was never wrong about the code — it genuinely describes
`Bulk Tests/R/03_Finalize_Acreage.R`, a separate legacy script that writes its own
`R_Phase2_Acreage_Matched_v2.csv` to `Bulk Tests/R/`, a different path from the one
`Phase_3.R` reads (`Phase 2 .../Data/R/`). The documentation table was transcribed from the
legacy script's console output, not the consolidated `Phase_2.R`'s. Root cause is `Docs`, not
`Code`: no fix needed in `Phase_2.R`; the doc table should be corrected in the post-freeze
documentation pass (**out of scope now** — do not edit `01_-_Phase2_Documentation.md` per
`CLAUDE.md` §2.2). See new **P2-03** for a related but distinct finding surfaced during this
check.

### P2-03 — R's Tigris fallback tier (Tier 2) recovered zero courses in the current on-disk data
**Severity:** Minor · **Status:** Fixed 2026-07-27 — Tier 2 removed (author decision, Gate 3, Decision 3) · **Locus:** Code (was environmental, now moot)
**Relates to:** **P2-01**, **X-02**

Surfaced while resolving P2-02 above. In the current `Data/R/R_Phase2_Acreage_Matched_v2.csv`
(2026-06-12 run), `acreage_source` has exactly two observed values — `OSM` (11,605) and
`MICE_Target` (4,687) — and zero rows are labeled `Tigris`. `final_acreage` is `NA` for all
4,687 `MICE_Target` rows and, since Tigris contributed nothing, is otherwise identical to
`osm_acreage` for every row in this snapshot.

`READ`, `Phase_2.R:391-395`: Tier 2 live-downloads Census `tigris::landmarks(type="area")` for
every state and filters to golf-related `FULLNAME`s; if the combined download yields zero
polygons, it hits `warning("No Tigris golf landmarks downloaded -- check internet / tigris
version. Skipping Tier 2.")` and Tier 2 is skipped entirely — which is consistent with what the
data shows. `INFERRED`: this looks like exactly that branch firing on the day this file was
generated (network hiccup, `tigris` cache state, or a Census landmarks API change), not a logic
bug in the fallback code itself — but this wasn't run today to confirm, per `CLAUDE.md` §2.1.

**Why it matters:** `final_acreage` vs `osm_acreage` (**X-02**) is already an escalated,
author-decides question. This finding means that *as things currently stand*, the two are
identical in practice for R (Tigris tier is contributing nothing), so the "3-tier vs 2-tier"
structural difference (**P2-01**) is currently latent, not live. But it is fragile: a future
run of `Phase_2.R` on a day when the live Tigris fetch succeeds would silently repopulate the
`Tigris` tier and change `final_acreage` for some subset of the 4,687 currently-`MICE_Target`
rows — meaning the R arm's Phase 3 input could shift between runs for reasons having nothing to
do with any code change, purely from Census landmark-service availability on run day. Same
non-reproducibility shape as **A-3**'s RUCC live-fetch risk. Recommend: before the frozen
re-run, either vendor the Tigris landmarks extract too, or deliberately decide (author call) that
Tier 2 is disabled/removed so R's acreage pipeline is not weather-dependent.

**Corroboration (2026-07-25, see P3-01):** the Phase 4 documentation's own explanation for R's
~10%-high `Holes` coefficient (`01_-_Phase4_Documentation.md:485-487`, "attributable to R using
final_acreage... while Julia/Python use osm_acreage") is undermined twice over now. First, by
this entry — Tigris contributed 0 rows, so the acreage variables share the same observed values.
Second, by the M=100 Jun-12 run: the same ~10% gap is still there (R 0.053 vs Python/Julia 0.048,
`Final_Thesis_Figures/8.241_Table2_Regression.tex`) at 20× the imputations, in a run where Tigris
still contributed nothing. An acreage-variable explanation predicts the gap should track Tigris
recovery; it hasn't moved while Tigris stayed at zero across two separate runs. See **P3-01**'s
candidate-ranking table — R's structurally different imputation model (**P1-01**) is now the
leading explanation instead.

**One-time offline diagnostic (author-directed, 2026-07-27, not committed as pipeline code) —
network miss, not structural non-match.** Before removing Tier 2, ran the exact
`landmarks(st_abbr, type="area") |> filter(str_detect(FULLNAME, "(?i)Golf|Country Club"))` call
from `Phase_2.R` standalone against 7 states (FL, CA, AZ, SC, NC, HI, GA), read-only, nothing
written to `Data/`. `VERIFIED`: every state returned real golf/country-club area landmarks — FL
109, CA 504, AZ 67, HI 26, GA 15, NC 3, SC 2. The zero-recovery in the 2026-06-12 run was a
network/environment miss on that specific run day, not a structural absence of golf-tagged
landmarks in the dataset or a join-key mismatch — golf landmarks genuinely exist in this Census
layer and the existing filter finds them correctly. This *confirms* the non-determinism concern
rather than undercutting it: a fallback tier that silently succeeds on some run days and silently
fails on others, changing R's `final_acreage` distribution for reasons unrelated to any code
change, is exactly the reproducibility risk described below — now with a diagnosed cause instead
of an unexplained zero.

**Fixed (author decision, Gate 3/Decision 3, 2026-07-27): Tier 2 removed entirely, not vendored.**
`final_acreage` is now `osm_acres` unconditionally in R — identical in construction to Python and
Julia's `osm_acreage` (no `coalesce()`, no live fetch). This closes **P2-01**'s three-tier-vs-
two-tier structural asymmetry and **X-02**'s "which acreage variable is canonical" question by
making the answer moot: there is no longer a second candidate variable to choose between. Removed
from `Phase_2.R`: the `ALL_STATES` constant, the `library(tigris)` import (no longer used
elsewhere in the file), `options(tigris_use_cache = TRUE)`, and the entire Tier 2 block (~100
lines). `acreage_source` is now two-valued (`OSM` / `MICE_Target`) instead of three-valued.
Header comment and step numbering updated to match. Not executed beyond the diagnostic above
(§2.1) — this documents the source fix; the corrected `R_Phase2_Acreage_Matched_v2.csv` will only
exist once Phase 2 is next run (the upcoming rehearsal, then the frozen cascade).

### P2-04 — 41 courses (0.35% of comparable rows) have genuinely different observed acreage across languages, despite sharing OSM geometry
**Severity:** Minor · **Status:** Fixed 2026-07-25 (Python + Julia standardized onto R's tie-break rule) · **Locus:** Code
**Relates to:** **X-06**, **D-0**

Surfaced by **D-0** (author-directed cross-language acreage identity check). Joined the three
Phase 2 outputs on `(round(lat,4), round(lon,4), Course_Name)`; of 11,604 rows with acreage
observed in all three, 11,563 (99.6%) match to float tolerance — expected, given **X-06**'s
shared-geometry finding. **41 rows (0.35%) differ by 2.7%–85% (median 37%)** — far beyond
floating-point or library-precision noise.

**Characterization (2026-07-25, author-directed, before any fix):**

- **Candidate-polygon count:** ran a real spatial join (not inferred) of the 41 course points
  against the shared Python OSM polygon set (`Py_Phase2_OSM_Golf_Polygons.gpkg`, 15,166
  polygons, EPSG:5070). **27/41 (66%) sit directly inside exactly 2 candidate polygons** —
  genuine spatial ambiguity, always exactly 2, never 3+. **14/41 (34%) don't intersect any
  polygon directly and were matched via the 500 m nearest-neighbour fallback**, at distances of
  4.5–282.7 m — all comfortably inside the 500 m threshold, so the boundary itself is not in
  question. **0/41** have their nearest polygon beyond 500 m.
- **Tie-break rule per language, `READ` from the actual matching code — `VERIFIED`, not
  inferred:**
  - **R** (`Phase_2.R:267-276`): `st_join(..., join = st_intersects)` fans out to one row per
    matching polygon, then `arrange(row_idx, desc(area_sqft)) |> filter(!duplicated(row_idx))`
    — **explicit, principled: the largest polygon by area wins.**
  - **Python** (`Phase_2.py:154-161,187-188`): `gpd.sjoin(..., predicate="intersects")` also
    fans out, then `courses_geo[~courses_geo.index.duplicated(keep="first")]` — keeps whichever
    row `gpd.sjoin`'s internal spatial-index traversal happened to return first. **Not
    size-based, not otherwise principled — an accident of library internals.**
  - **Julia** (`Phase_2.jl:176-191`): a manual loop over `osm_golf_geo` in raw table order,
    `if ArchGDAL.intersects(pt, poly_geoms[j]) ... break` — **keeps whichever polygon appears
    first in the table's on-disk order. Also not size-based, also an accident of ordering,** and
    by a different mechanism than Python's, so there's no reason to expect the two even agree
    with each other.
  - **Conclusion: only R's tie-break reflects a deliberate decision.** Python's and Julia's are
    both "first-encountered," via unrelated implementation-specific orderings, with no golf-
    course-relevant justification and no reason to coincide. This is the direct mechanism behind
    the 27 direct-intersect divergences. The 14 NN-fallback divergences are a distinct, smaller
    puzzle — `sjoin_nearest`/manual-nearest should generally agree since true-nearest is well-
    defined; residual disagreement there most likely traces to CRS/precision differences between
    matchers rather than a tie-break choice, not separately traced further today.
- **Urban/rural skew:** confirmed. 37/41 (90%) of divergent rows are `Urban` vs. a 76.5% Urban
  base rate across all 11,604 comparable rows — courses in denser development are more likely to
  have a second nearby golf-tagged OSM feature to be ambiguous against.
- **Geographic clustering:** spread nationally (lat 26°–46°, lon -124° to -74°), no single tight
  cluster; roughly 23/41 in a central-US longitude band, the rest spread across the west, east,
  and mountain regions.

**Tag-disambiguation check (2026-07-25, author-directed decisive query) — `VERIFIED` by reading
the extraction filter, not inferred:** `Phase_2.py:50`, the OSM-extraction handler, keeps a
polygon *only if* `a.tags.get("leisure") == "golf_course"`; every other tag combination is
discarded before the polygon ever reaches the GeoPackage. Confirmed against the actual file:
`Py_Phase2_OSM_Golf_Polygons.gpkg` carries only `osm_id`, `name`, `osm_acreage`, `geometry` — no
tag columns survive extraction, and there is no broader-feature (resort/park/`landuse=*`) polygon
anywhere in the shared polygon set to disambiguate against. **All 27 direct-multi pairs are two
`leisure=golf_course` polygons overlapping each other** — tag-based disambiguation is not just
absent from the code, it's categorically impossible given how the polygon set was built.

**Applying the author's stated decision rule (both-golf-tagged branch): standardized on R's
largest-area rule in all three languages, 2026-07-25.** Sanity-checked first: for a 5-row sample
of the 27, the point's largest *directly-containing* polygon by `osm_acreage` matched R's current
`final_acreage` to 8+ significant figures in 4/5 (the 5th's rounded-coordinate spot-check
produced a false negative from lat/lon rounding in the verification script, not a real
mismatch) — confirms R's `arrange(row_idx, desc(area_sqft))` is already exactly "largest polygon
wins," so Python and Julia were changed to match it rather than R being re-derived:
- **Python** (`Phase_2.py:154-166`): after the `sjoin`, added `courses_geo =
  courses_geo.sort_values("osm_acreage", ascending=False)` before the existing
  `duplicated(keep="first")` dedup, so "first" is now "largest" by construction. Same fix applied
  to the NN-fallback dedup (`Phase_2.py:181-183`), sorting by `["_dist", "osm_acreage"]` so
  equidistant ties (not observed in the 41, but possible in principle) also resolve by area.
- **Julia** (`Phase_2.jl:176-212`): the manual intersect loop no longer `break`s on the first hit;
  it now scans every candidate and keeps the one with the highest `poly_acres[j]`. The NN-fallback
  loop's `dist < min_dist` condition gained an explicit `dist == min_dist &&
  poly_acres[j] > poly_acres[best_idx]` tie-break branch for the same reason.
- **R unchanged** — it already implements the rule being standardized on.

This is a `§3` three-brothers-compliant change: behavior changes in Python and Julia only, in the
same commit, converging all three onto one documented, principled rule rather than three
different (two of them arbitrary) ones. Not yet executed (`§2.1`) — the corrected CSVs will only
exist once Phase 2 is next run; this entry documents the source fix and its expected effect, not
a re-verified output.

**Dollar impact of the corrected rule vs. the current (pre-fix) mixed behavior, on the 27
direct-multi rows, using each row's own `Baseline_Value_Per_Acre` (not a national average) —
`VERIFIED`, computed from the actual on-disk Phase 2 + Phase 3 data:**
- **Python:** 22/27 rows actually change (5 already coincidentally matched R's pick).
  Acreage delta sums to **+1,997.0 acres**; dollar delta sums to **+$1,794,979,303** —
  **0.191% of Python's own $938.309B pooled national total.**
- **Julia:** 15/27 rows actually change (12 already coincidentally matched R's pick, one row,
  Stonebridge Golf & Country Club, has no `Baseline_Value_Per_Acre` and can't be dollarized).
  Acreage delta sums to **+1,122.6 acres**; dollar delta sums to **+$958,706,994** —
  **0.101% of Julia's own $950.637B pooled national total.**
- Single-largest-row effect: Industry Hills At Pacific Palms Conference Resort, Python
  213.5→420.4 acres, +$855.5M alone (Los Angeles County BVPA ≈ $4.13M/acre) — one course in one
  language accounts for nearly half of Python's total correction, underscoring that this is a
  small-N/high-leverage effect (25/27 affected rows are Urban, where BVPA is 1-2 orders of
  magnitude above the Rural USDA rate) rather than a broad-based one.
- The 41-row set (27 fixed here + the still-unexplained 14 NN-fallback rows) is retained as a
  **sensitivity set** per instruction, so the alternative (leaving the mixed tie-break) can still
  be quantified later if needed.

**Consequence for the national spread — recorded explicitly under P3-01, not buried here.**
Applying this fix moves Python's pooled total **938.309B → 940.099B** and Julia's
**950.637B → 951.596B** (R unaffected — it already implemented the standardized rule); the
cross-language spread **widens**, 1.61% → 1.71%. This means the pre-fix 1.61% was partly two
independent errors (Python's and Julia's arbitrary tie-breaks) partially cancelling against each
other and against R, not a clean read of the real divergence. See **P3-01** for the full table and
the standing-candidate ranking this changes — P2-04 is now *ruled out* as a driver of the spread,
having demonstrably suppressed rather than caused it.

---

## Phase 3 — MICE Imputation & Rubin's Rules

### P3-01 — Every published Phase 3 figure was computed at M = 5
**Severity:** Critical · **Status:** Confirmed · **Locus:** Docs

**The code is correct.** `Phase_3.py:164`, `Phase_3.R:179`, `Phase_3.jl:155` all implement
`v_t = v_w + v_b + v_b/m` — algebraically `V_W + (1 + 1/M)·V_B`, exactly Rubin. All three
`main()` blocks pass `M = 100`.

**The published numbers cannot have come from that code at M = 100.** Solving the documented
tables for the implied M returns **exactly 5.00** in all three languages:

| Lang | Doc `V_W` | Doc `V_B` | Doc `V_T` | `V_T` if M=100 | `V_T` if M=5 | Implied M |
|---|---|---|---|---|---|---|
| Python | 2.1215e16 | 9.7765e18 | **1.1753e19** | 9.8955e18 | **1.1753e19** | 5.00 |
| R | 2.1111e16 | 2.0232e19 | **2.4300e19** | 2.0455e19 | **2.4300e19** | 5.00 |
| Julia | 2.1339e16 | 1.2354e19 | **1.4846e19** | 1.2499e19 | **1.4846e19** | 5.00 |

Reproduces to every printed digit. `V_T / V_B ≈ 1.2020` in all three — that is `1 + 1/5`.

**Corroborating evidence, independent of the arithmetic:**
- Each results table in `01_-_Phase3_Documentation.md` lists exactly **five** per-dataset
  aggregates (Dataset 1–5), matching a summary CSV built from an M=5 run.
- `01_-_Phase5_Documentation.md` Julia data-flow diagram: `Phase 3 CSVs (×5)`.
- `01_-_Phase4_Documentation.md` labels its results `(M = 5 pilot)` and states they "will be
  updated once the M = 100 pipelines complete."

**Consequences:**
1. Published SEs are inflated by `√(1.2/1.01) ≈ 1.09` — roughly 9% too wide.
2. `V_B` is estimated from 5 draws. Its own sampling error is enormous; this is precisely the
   instability the M=100 choice was made to eliminate. The Phase 3 Summary's argument for
   M=100 over M=5 is sound — it just wasn't the run that got published.
3. The $943.0B / $936.0B / $951.4B point estimates and every CI in the Phase 3 Summary,
   Phase 3 Documentation, and `Meta_Summary.pdf` are stale.
4. ~~The "cross-language spread of 1.6%" claim is a spread between three 5-draw estimates. It may
   tighten or widen at M=100; it is currently unknown.~~ **Resolved 2026-07-25 — see below: it
   does not tighten.**

**Re-characterized (2026-07-25, per X-04).** Original framing: "the M=100 run never happened,
action = re-run at M=100." **That's now wrong.** A complete, synchronized, tri-language M=100
run exists on disk, dated 2026-06-12 (see **X-04**) — Phase 3 through Phase 6, including
LaTeX tables and figures. The algebra above still stands exactly as computed: the *currently
published* tables were built from an M=5 run. What changes is the diagnosis: this is not a
missing-run problem, it's a **documentation-lag problem** — the write-up was never updated after
the M=100 run completed. Root cause moves from `Locus: Docs (code correct, run needed)` to
`Locus: Docs (code correct, run exists, write-up stale)`.

**Read the Jun-12 run directly** (`Data/{R,python,Julia}/*_Rubins_Rules_Summary.csv`):
Pooled national OC — R $935.521B, Python $938.309B, Julia $950.637B (Grand Mean ≈ **$941.5B**).
This is close to but **not identical** to the published $936.0B/$943.0B/$951.4B (Grand Mean
$943.5B, per `Notes.md`) — confirming these are genuinely two different runs, not the same
numbers presented two ways. The M=5-vs-M=100 algebra in this entry used the *published* numbers
and is unaffected by this; it's independent confirmation that the Jun-12 run is not what's
currently cited anywhere.

**Action, revised:** No re-run needed to get *an* M=100 result — one already exists. What's
needed is (a) the author's confirmation of whether the Jun-12 run is fit to publish as-is (it
predates **P1-01**/**P1-05**, so it isn't — see **X-04**), and (b) once fixes land and the frozen
cascade re-run happens, updating the write-up from *that* output, not treating "get an M=100 run"
as the open task. Nothing needs fixing in the pooling code — unchanged from the original finding.

**Finding: the cross-language spread does not narrow at M=100 — Monte Carlo noise is eliminated
as an explanation (2026-07-25).**

| Run | R | Python | Julia | Mean | Spread |
|---|---|---|---|---|---|
| M=5 (published) | 936.0 | 943.0 | 951.4 | 943.47 | 1.63% |
| M=100 (Jun-12) | 935.521 | 938.309 | 950.637 | 941.489 | 1.61% |
| M=100, **P2-04-corrected** | 935.521 | **940.099** | **951.596** | 942.405 | **1.71%** |

A twentyfold increase in imputations (M=5 → M=100) moved the spread by **0.02 percentage
points** — statistically indistinguishable from unchanged — and the **ordering is identical in
both runs**: R lowest, Python middle, Julia highest, in both. If the spread were an artifact of
M=5's known instability (**consequence #2 above**), M=100 should have visibly tightened it, and
it did not. This rules out "not enough imputations" as the explanation for the cross-language
divergence.

**The spread widens under correction (2026-07-26) — recorded explicitly, not buried in P2-04's
entry.** Applying **P2-04**'s fix (standardizing Python and Julia's polygon tie-break onto R's
largest-area rule) moves Python's total **938.309 → 940.099** (+$1.79B, matching P2-04's computed
dollar impact exactly) and Julia's **950.637 → 951.596** (+$0.959B, ditto). R is unaffected — it
already implemented the rule being standardized on. **Range widens from $15.116B to $16.075B;
spread widens from 1.61% to 1.71%.** The direction matters as much as the number: the *pre-fix*
1.61% was not a clean measurement of the structural divergence — it was **two independent errors
partially cancelling** (Python's arbitrary tie-break happened, on net, to pull its total closer to
R's; Julia's happened to pull less far away than Python's, but still short of the honest gap).
Removing the tie-break noise doesn't shrink the spread, it **reveals a wider one that noise had
been partially masking.** The corrected 1.71% is the honest figure — this is now the number to
carry forward, not 1.61%.

This is the **second** time the spread has resisted the expected direction of travel: it did not
narrow at 20× the imputations (M=5→M=100), and it does not narrow under a genuine bug fix
(P2-04) — it widens. A stochastic or noise-driven explanation predicts shrinkage under both
interventions; neither happened. Both results point the same way: toward a **structural**
cause, not a Monte Carlo artifact — and further isolate that structural cause away from Phase 2
(P2-04's own contribution, now measured and removed, turns out to have been suppressing the
spread, not producing it) and back onto Phase 3, keeping **R's four-variable imputation model
(candidate 3 below) the leading explanation**, now on slightly stronger footing than before this
correction. Recording this as a **finding about the pipeline**, exactly as the M=100 result above
was — a spread stable-or-widening across two independent interventions is a stronger,
more characterizable result than an unresolved discrepancy would be.

**Candidate explanations for the (real, stable) cross-language spread, ranked as of today:**
1. ~~Acreage variable (`final_acreage` vs `osm_acreage`)~~ — weakened by **P2-03** (R's Tigris
   tier recovered 0 rows; the two variables share the same observed values in current data).
2. ~~M=5 Monte Carlo noise~~ — **eliminated by the finding above.**
3. **R's structurally different imputation model — now the leading candidate.** Per **P1-01**/
   **P1-05**, R's `futuremice(method="rf")` jointly imputes **4** variables (`final_acreage`,
   `Baseline_Value_Per_Acre`, `Holes`, `Ownership_Type`) via a single scalar `method` argument
   with no `predictorMatrix` override; Python and Julia impute **2** (acreage, BVPA only —
   `Holes`/`Ownership_Type` are always fully observed in their pipelines, so `mice` never touches
   them). Same *observed* values entering MICE, different *imputed* values coming out, feeding
   directly into `log1p(acreage × BVPA)`. See **C-1** (elevated to high-value — this is now the
   same structural question as **A-2**, applied to the headline number instead of a single
   coefficient).
4. MICE backend differences (R's `ranger`/RF vs Python's LightGBM vs Julia's `Mice.jl`) — still
   open, not yet isolated from #3 above (both act through the same imputation step).
5. Phase 1 date/vintage skew (R Jun-12 vs Python May-18 vs Julia May-14, per **X-04** caveat 1) —
   still open, and now independently testable since Part B fixed the known Phase 1-level
   divergences (**P1-05**, **P1-06**, **P1-09**) that would otherwise have confounded it.
6. ~~Phase 2 polygon-matching tie-break (**P2-04**)~~ — **quantified and fixed, 2026-07-26; it
   was suppressing the spread, not producing it.** Standardizing Python/Julia onto R's tie-break
   rule *widened* the spread (1.61%→1.71%, see above), the opposite of what a contributing-cause
   candidate would do once corrected. Ruled out as a driver of the spread; kept as its own tracked
   fix under **P2-04**.

**Corroborating evidence, added to P2-03:** R's `Holes` coefficient is ~10% above Python's and
Julia's in the M=100 run too (0.053 vs 0.048 vs 0.048, `Final_Thesis_Figures/8.241_Table2_Regression.tex`)
— the identical gap the M=5-era `01_-_Phase4_Documentation.md:485-487` attributes to the Tigris
acreage difference, which **P2-03** already showed contributed zero rows. The gap surviving a
20× change in M, in a variable (`Holes`) that **P1-01** already proved R silently imputes and
Python/Julia never do, is a second independent line of evidence for candidate #3 above.

### P3-02 — `Phase_3.jl` retained `m_datasets = 5` as the default in all three function signatures
**Severity:** Critical (as a latent trap) · **Status:** Fixed 2026-07-25 (Parity Audit D-1) · **Locus:** Code

```julia
Phase_3.jl:36   const M = 100
Phase_3.jl:43   function run_imputation(input_csv::String, out_dir::String; m_datasets::Int = 5)
Phase_3.jl:125  function run_pooling(in_dir::String, out_csv::String; m_datasets::Int = 5)
Phase_3.jl:231  function run_acreage_summary(in_dir::String, out_csv::String; m_datasets::Int = 5)
```

Harmless **at the time this was found**: `main()` passes `m_datasets = M` to all three. But any
call that omits the keyword — an interactive `include()`, a REPL invocation, a future bulk
script — would have silently run at M=5 and written a file whose name and header claim M=100.
Given **P3-01**, this is very likely the mechanism by which the M=5 results were generated in
the first place.

Python (`Phase_3.py:48,131,223`) has the same shape but defaults to `m_datasets=100`, so it
fails safe.

**Fixed (Parity Audit D-1, 2026-07-25):** `Phase_3.jl:43,125,231` changed to `m_datasets::Int =
M` (not a literal `100` — that would have reintroduced the same desync risk against a future
edit to `const M`, an error caught and corrected same-day, see `PARITY_AUDIT.md` D-1). Same
belt-and-braces fix applied to `Phase_3.py:48,131,223` even though its literal `100` default
wasn't broken today. **This entry's status was stale until 2026-07-26** — the fix landed under
D-1 the same day this issue was logged, but the ID's own status field was never updated to
match; caught and corrected during the Gate 2 status audit.

### P3-03 — MICE-free complete-case value is bit-identical to Python's pooled estimate
**Severity:** Major · **Status:** Open · **Locus:** Docs

`01_-_Phase3_Documentation.md` §"Complete Case Analysis (MICE-Free)" reports the MICE-free
national value as **$943.025 B**. Python's pooled MICE Q̄ is **$943.025 B** — identical to three
decimals.

A complete-case sum over 5,115 courses landing on exactly the multiply-imputed pooled sum over
~16,292 courses is not a coincidence; it's a transcription error. And the doc then reasons *from*
the coincidence:

> "The MICE-free national value ($943.025 B) is remarkably close to the pooled MICE estimates…
> This suggests that: 1. The 28.8% of courses missing `osm_acreage` are not systematically
> different in value…"

That conclusion is currently unsupported. If a genuine complete-case analysis exists it should
be quoted; if not, the robustness claim must be withdrawn. Note the direction of the risk: a
complete-case estimate that *matches* the imputed one is the single most attractive result for
the thesis, which is exactly why it needs to be right.

Minor arithmetic in the same section: "excluding 68.4% of the sample (5,115 vs. ~16,292)" —
5,115/16,292 = 31.4%, so the exclusion is 68.6%, consistent with the 11,177 removed stated two
lines above.

### P3-04 — Acreage-pooling rationale contradicts itself
**Severity:** Minor · **Status:** Open · **Locus:** Docs

§Step 3 states:

> "Acreage is a **fixed spatial measurement** … it does not vary across imputed datasets
> because MICE imputes acreage itself, not geography."

Acreage demonstrably *does* vary across imputed datasets — 28.8% of it is imputed, and the same
section proceeds to compute its between-imputation variance `V_B` and build a CI from it. The
*conclusion* (drop `V_W`, since a completed dataset's total is a deterministic sum with no
within-dataset sampling variance) is defensible. The stated reason is not, and a committee
member reading it closely will notice.

### P3-05 — Two different sets of national acreage totals are published
**Severity:** Minor · **Status:** Open · **Locus:** Docs

| Source | R | Julia | Python |
|---|---|---|---|
| §"Results — National Acreage" | — | 2,293,146 | 2,305,904 |
| §3D "Confirmed from Output CSVs" | 2,303,152 | 2,291,064 | 2,306,485 |

Two runs, both published in the same document. §3D claims to read the actual CSVs, so it is
presumably authoritative — but the earlier section isn't marked stale. Likely the same M=5/M=100
split as **P3-01**; needs confirming rather than assuming.

Also in §"Results": the R acreage table is a stub — the header exists, the numbers were never
filled in.

### P3-06 — Julia `Pooled_Acres` written in scientific notation
**Severity:** Minor · **Status:** Won't fix (documented) · **Locus:** Code

`Jl_National_Acreage_Summary.csv` writes the national total as `2.29106386e6`. Parses fine
everywhere; noted only because it will look like a defect to anyone opening the CSV by hand.

**Re-confirmed (Parity Audit D-7, 2026-07-25):** still present on the current Jun-12 output, and
it's inconsistent even within the same file — `National Total` and `Urban` rows are scientific
notation (`2.29090437e6`, `1.70409671e6`), `Rural` is plain decimal (`586807.66`), same file, same
column. Likely just Julia's default float-formatting magnitude threshold. Status unchanged.

### P3-07 — R has no explicit floor at 0 on imputed acreage/BVPA; Python and Julia both do
**Severity:** Minor (currently dormant — no negative values observed) · **Status:** Fixed 2026-07-25 · **Locus:** Code

**Checked (Parity Audit D-4, 2026-07-25):** `Phase_3.py:98-101`
(`.clip(lower=0)` on both `osm_acreage` and `Baseline_Value_Per_Acre`) and `Phase_3.jl:82-85`
(`clamp.(..., 0, Inf)` on both) explicitly floor imputed values at 0 before writing. `Phase_3.R`'s
save loop (`Phase_3.R:136-141`, `complete_data <- complete(imputed_list, i); write.csv(...)`) has
**no equivalent clip anywhere** — grepped for `clip|clamp|pmax`, 0 matches in `Phase_3.R`.

**Checked whether this currently matters:** read `Dataset 1` for all three languages —
**0 negative values in any of `final_acreage`/`osm_acreage`/`Baseline_Value_Per_Acre`, all three,
min values identical (5.05 acres, $325) across all three**, matching the plausibility-filtered
floor, not a clipped artifact. So R's `ranger`-backed `"rf"` method isn't currently producing
negative draws in practice — the missing clip is dormant, not live. Only checked Dataset 1 of
100 per language, not all M=100 × 3.

**Why it's still worth recording:** Python/Julia's authors evidently found it *necessary* to add
this guard, implying their RF backends (LightGBM, Julia's implementation) can produce
negative regression-tree predictions near a zero boundary. Whether R's `mice`+`ranger` "rf"
method structurally can't (e.g. because it draws literal observed donor values rather than
predicted means, which would mathematically preclude negatives if no observed value is negative)
or simply hasn't yet in the checked samples is not established either way — that open question
is unaffected by the fix below and is not resolved here.

**Fixed 2026-07-25:** added `complete_data$osm_acreage <- pmax(complete_data$osm_acreage, 0)` and
the equivalent for `Baseline_Value_Per_Acre`, in `Phase_3.R`'s per-dataset save loop (right after
`complete(imputed_list, i)`, before `write.csv`) — mirrors Python's `.clip(lower=0)` and Julia's
`clamp.(..., 0, Inf)` at the same point in their save loops. Currently a no-op against this RF
backend's draws (Dataset 1 check above still holds), added purely for defensive parity so all
three behave identically **by construction** rather than by the current backend's luck. Not
executed (`§2.1`) — this documents the source fix, not a re-verified output.

**Correction (2026-07-26, caught during the dress-rehearsal cascade):** the fix above used the
wrong column name — `complete_data$osm_acreage`, copied from Python/Julia's convention, but R's
imputed column is `final_acreage` (`IMPUTE_COLS <- c("final_acreage", ...)`, `Phase_3.R:44`).
`complete_data$osm_acreage` doesn't exist in R's frame, so `pmax(NULL, 0)` returned a zero-length
vector and the assignment crashed: `Error in $<-.data.frame(...) : replacement has 0 rows, data
has 16292`. **`VERIFIED` by execution** — this was not caught by re-reading the diff, only by
actually running `Phase_3.R` end-to-end during the rehearsal. Fixed to
`complete_data$final_acreage <- pmax(complete_data$final_acreage, 0)`; re-ran, clean. See the
rehearsal writeup in `PARITY_AUDIT.md` for the other two crashes it caught.

### P3-08 — Julia's `mice()` hard-crashes if any predictor categorical column contains a missing value
**Severity:** Major (was live against pre-P1-05 on-disk data; confirmed non-recurring post-fix) · **Status:** Verified resolved 2026-07-26 (dress-rehearsal cascade) · **Locus:** Code / cross-cutting

**Surfaced incidentally during D-5's execution** (author-authorized scoped exception to `CLAUDE.md`
§2.1 — see `PARITY_AUDIT.md` D-5). Running a minimal standalone reproduction of `Phase_3.jl`'s
imputation step against the current, unmodified on-disk `Jl_Phase2_Acreage_Matched.csv` produced
an immediate crash — `VERIFIED` by execution, not inferred:

```
ERROR: MethodError: Cannot `convert` an object of type Missing to an object of type
CategoricalValue{String15, UInt32}
```

Traced into `Mice.jl`'s `initialiseworkingdata` (`Mice/src/makefunctions.jl:131`). Root cause:
`Course_Type` (`= categorical(acreage_df.Ownership_Type)`) is a **predictor**, not an imputation
target (it's not in `IMPUTE_COLS`/`visitsequence`) — but the current on-disk data still carries
the single pre-**P1-05** missing `Ownership_Type` value (Turtle Creek, the same row **X-05**
traced to `CSV.jl`'s `missingstring=""` default). `Mice.jl` does not skip or otherwise tolerate a
`missing` in a non-imputed categorical predictor column — it refuses to build its internal working
data structures at all, and the failure is total (no partial output, no imputed columns, nothing).

**Practical consequence:** `Phase_3.jl`, run today exactly as it sits on disk, against today's
on-disk Phase 2 output, **would not complete** — not a silent divergence, a hard stop. R's `mice`
and Python's `miceforest` were not observed to have an equivalent failure mode (both completed
against the same underlying missingness pattern in their respective per-language predictor
columns without incident).

**Why this is (probably) already fixed, but not verified as fixed:** **P1-05**'s source-level
regex fix removes the underlying missing `Ownership_Type` value at Phase 1, before it can reach
Phase 2 or Phase 3 — once the post-freeze cascade re-run regenerates Phase 2's output from the
fixed Phase 1, this crash should no longer trigger, because the predictor column it depends on
will have zero missing values. **Not verified**, because verifying it means running Phase 1 → 2 →
3 in sequence, which is out of scope beyond the single D-5 exception already granted.

**Recommend:** treat this as a standing risk, not a closed item — if any *other* predictor column
(not just `Ownership_Type`) ever picks up a stray missing value in a future data refresh, Julia's
Phase 3 will hard-crash where R and Python would (per **A-2**'s original finding) silently
mis-impute it instead. Worth a defensive `dropmissing`/assertion on `PREDICTOR_COLS` in
`Phase_3.jl` before the `mice()` call, so the failure mode is an explicit, informative check
rather than an opaque library `MethodError` — not implemented here, as it's a robustness
improvement beyond what D-5 was scoped to fix, and touches only Julia (no equivalent R/Python
divergence to correct in parallel under **§3**).

---

## Phase 4 — Econometric Modeling

### P4-01 — Documentation contradicts itself on whether the results are M=5 or M=100
**Severity:** Major · **Status:** Open · **Locus:** Docs

Same numbers, two incompatible labels, one document:

- §Results: *"the tables below reflect pilot runs at M = 5 imputations. They will be updated
  once the M = 100 pipelines complete"* — Python 12.2822 / 0.0474 / 4.1720.
- §4D: *"Coefficient Comparison (from actual M=100 Bulk Tests CSVs)"* — Python 12.2822 /
  0.04740 / 4.17199.

Identical to five significant figures. Both labels cannot be true. Given **P3-01**, M=5 is the
more likely provenance — and if so, §4D is mislabelled and the Phase 4 Summary's coefficient
table inherits the error. Resolution depends on **X-03** (which tier's CSVs are which).

### P4-02 — Python's MICE backend is described three different ways
**Severity:** Minor · **Status:** Open · **Locus:** Docs

Phase 4 §"What the Data Tells Us" ¶3 attributes Python's imputation to `IterativeImputer`.
Everywhere else — Phase 3 docs, Phase 3 Summary, Phase 4 §Master Scripts — it is
`miceforest` v6.0.5 with a LightGBM backend. `IterativeImputer` is a **scikit-learn** class
and a materially different algorithm (typically Bayesian ridge). One sentence, but it appears
in the paragraph that explains away the cross-language spread, so it undercuts the explanation
it's offered in support of.

### P4-03 — The 60× urban premium is largely mechanical, and the caveat is load-bearing
**Severity:** Major · **Status:** Open (author is aware) · **Locus:** Docs

Recorded not as an error but as the interpretive exposure most likely to be pressed at defence.
The Phase 4 Summary already concedes it:

> "the hybrid valuation algorithm in Phase 1 assigns urban courses the FHFA residential price
> and rural courses the USDA agricultural price, and the FHFA–USDA per-acre ratio is itself
> approximately 60× in many counties."

Which means `β_urban ≈ 4.1` substantially recovers the ratio that Phase 1 *assigned by
construction*, and `R² ≈ 0.70–0.77` is high largely because the dummy reproduces a
deterministic branch in the data-generating process. The Summary handles this well — it calls
the regression a *decomposition* rather than a causal estimate and carries the caveat to §5.3.

The exposure is that Phase 4 §"What the Data Tells Us" ¶1 does **not** carry the caveat, and
instead reads the coefficient straight: *"reflecting the well-known urban land price gradient."*
Any figure or table generated from that framing (Forest Plot, Table 2) inherits the stronger
claim without the qualification. The two documents need to agree.

### P4-04 — Phase 4 function parity sweep (E-1, E-3–E-6): clean, one unused-import conflict flagged
**Severity:** Cosmetic · **Status:** Verified clean except one flagged item, not acted on · **Locus:** Code

Parity Audit Part E, batch result. E-2 is its own entry (folded into **P1-01** above, since it's a
direct downstream consequence of that bug). The rest:

- **E-1 (dependent variable): clean.** Covered above under **X-02** — `log1p(acreage × BVPA)`,
  multiply-then-log, identical order in all three; the only divergence is *which* acreage column,
  already tracked as **X-02**.
- **E-3 (HC1 SEs): `VERIFIED` numerically identical, not just formula-identical.** Built one common
  dataset (Python's `Dataset_1`, `Log_Opportunity_Cost`/`Holes`/`county_type`, N=16,297, no
  missingness) and fit `Log_Opportunity_Cost ~ Holes + county_type` through each language's own
  HC1 code path against the *same* data — `sandvich::vcovHC(type="HC1")` (R), `cov_type="HC1"` via
  `statsmodels` (Python), and the hand-rolled sandwich estimator in `Phase_4.jl:111-117`
  (`(n/(n-k)) .* bread * meat * bread`). Coefficients and SEs agree to 6+ significant figures
  across all three (e.g. `Holes` SE: `0.002318615` R / `0.002319` Py / `0.002318615126867545`
  Jl) — the formula is genuinely the same estimator in all three, not just described the same way.
- **E-4 (significance stars): clean.** `stars()` (R, Python) / `get_stars()` (Julia) all use the
  same four strict `<` thresholds (0.001/0.01/0.05/0.1) with no `<=` anywhere, so boundary
  behavior at exactly `p = 0.05` (etc.) is identical (no star) in all three. `NA`/`NaN` handling:
  R and Julia guard explicitly (`is.na`/`isnan`); Python has no explicit guard but relies on
  IEEE-754 `NaN < x` always evaluating `False`, which falls through to the same `""` result —
  stylistically different, functionally identical.
- **E-5 (Barnard–Rubin df): clean.** `df_old`, `df_obs`, `df_adj` formulas are algebraically
  identical in all three (`Phase_4.R:214-219`, `Phase_4.py:178-182`, `Phase_4.jl:229-233`),
  including the same choice of `df_com` = the **first** imputed dataset's residual df in all
  three, not an average or a recomputation per-dataset.
- **E-6 (unused `broom` import): confirmed unused, NOT removed — flagging the conflict rather
  than resolving it.** `Phase_4.R:15`: `library(broom)`, 0 calls to `tidy()`/`glance()`/
  `augment()`/any broom function anywhere in the file (`VERIFIED`, grepped). The checklist item
  asks to "verify and remove," but the import line itself carries an explicit
  `# pre-existing dependency - do not remove` comment — the identical comment also guards
  `library(wooldridge)` in the same file and in `Phase_3.R`. Given `CLAUDE.md` §6 (preserve
  existing conventions, don't opportunistically clean up) and that this looks like a deliberate
  prior instruction rather than an oversight, **not removed** — surfaced here for the author to
  decide, rather than silently complying with either the checklist item or the in-code comment.

---

## Phase 5 — Hawaii Micro-Case Study

### P5-01 — Three different Oahu opportunity-cost totals are in circulation
**Severity:** Major · **Status:** Open · **Locus:** Docs

| Value | Source | Basis |
|---|---|---|
| **$25.400B** | Phase 5 Summary + Phase 5 Doc §Step 1–3 | Rubin-pooled, 33 dedup courses, CI $22.663–$28.137B |
| **$28.6B** | Phase 5 Summary §Key result; Phase 6 waffle chart | "gross HBU estimate" — decomposed 23.4 / 3.9 / 1.3 |
| **$31.197B** | Phase 6 Doc §Last Verified Run | "M=100 R draws, 37 courses" |

The $28.6B figure has no derivation anywhere in the corpus. It appears fully formed in the
Preservation Paradox decomposition, and the three sub-figures ($23.4B / $3.9B / $1.3B) are
simply the **acreage** shares (81.7% / 13.8% / 4.5%) multiplied through it — so the dollar
decomposition carries no independent information beyond the zoning acreage split, and inherits
whichever total is chosen.

This matters because **$1.3B "directly unlockable" is the single most quotable number in the
thesis.** At $25.4B the same 4.5% share gives ~$1.14B; at $31.2B it gives ~$1.40B. The
headline moves by ±20% depending on which total is used, and the Summary uses $25.4B and
$28.6B **four paragraphs apart**.

**Needed:** one canonical Oahu total, a stated derivation, and consistent use. The 37 → 33 → 29
course-count ladder is well explained in the Summary and is *not* the problem — the problem is
that the dollar totals attached to those stages aren't reconciled.

### P5-02 — Summary and Documentation disagree on whether the P-1 discrepancy was resolved
**Severity:** Major, downgraded from live discrepancy · **Status:** Resolved — Summary is correct, Documentation is stale · **Locus:** Docs

- `00_-_Phase5_Summary.md`: *"A subsequent re-run with updated `sf` geometry handling resolved
  the divergence; all three languages now report P-1 = 744.6 acres, with all zoning classes
  agreeing across implementations to within 0.01 acres."*
- `01_-_Phase5_Documentation.md`: R reports **523.5** acres (total ~5,845); Python/Julia report
  744.6 (total ~6,066); *"root cause unconfirmed"*; listed under **Limitations §6**.

Directly contradictory. If the Summary is right, the Documentation's limitation and cross-language
note must be struck. If the Documentation is right, the Summary is claiming a fix that didn't
happen — and 6,066.2 acres, the canonical denominator for every zoning share in the thesis,
is a Python/Julia-only figure that R does not reproduce.

**Resolved, `VERIFIED` (Parity Audit F-1, 2026-07-26):** read the actual current on-disk output
of all three languages' Step 6 zoning tables (`Data/{R,python,Julia}/*Phase5_Step6_Zoning_
Percentages.csv`). **P-1 = 744.6255827951873 (R) / 744.625582790703 (Python) /
744.6255827939525 (Julia)** — identical to 8+ significant figures. All 19 zone classes across
all three agree to well within the Summary's claimed 0.01-acre tolerance, not just P-1. **The
Summary is the accurate statement; the Documentation's "root cause unconfirmed" limitation and
cross-language note describe a state that no longer matches current data** — the same
documentation-lag pattern already established for **P3-01**/**X-04** (a real fix landed, the
narrative describing the *problem* was never updated to describe the *fix*). Not a code defect;
nothing to change in either master script. `Issue_Register.md`/`PARITY_AUDIT.md` are the only
files this audit may edit, so the actual doc correction (striking the stale limitation) is left
for the post-freeze documentation pass, per `CLAUDE.md` §2.2.

### P5-03 — The six pilot courses do not span all four counties
**Severity:** Minor · **Status:** Open · **Locus:** Docs

`00_-_Phase5_Summary.md` claims the pilot covers *"six high-profile Hawaii golf courses spanning
all four counties (Honolulu, Maui, Hawaii, Kauai)."* The table in the Documentation:

| County | Courses in pilot |
|---|---|
| Honolulu | 2 (Turtle Bay, Waialae) |
| Maui | 2 (Kaanapali, Wailea) |
| Hawaii | 2 (Hualalai, Kohala) |
| **Kauai** | **0** |

Three counties, not four. Easy fix in the prose — but note the Summary also leans on the
urban/rural gradient across the pilot (1.16× Honolulu → 1.69× Big Island), and Kauai's absence
removes one of the two rural counties from a six-point trend.

### P5-04 — The $456.8M "average opportunity cost" is a mean over 61 courses, not 74
**Severity:** Minor · **Status:** Open · **Locus:** Docs

Phase 5 Doc §"Hawaii Course Summary (All Islands)":

| Metric | Published |
|---|---|
| Total Courses (Hawaii state) | 74 |
| Average Opportunity Cost | $456,829,248 |
| Total Opportunity Cost | $27,866,584,127 |

$27,866,584,127 / $456,829,248 = **61.0**, not 74. The mean silently excludes 13 courses —
presumably those with missing BVPA or acreage. $27,866,584,127 / 74 = **$376.6M**. Either
number can be published; the label must match the denominator. (The per-county table is
internally consistent: counts sum to 74, totals sum to ~$27.85B.)

### P5-05 — Stale hardcoded acreage constant survives in Bulk Tests daughter scripts (all three languages)
**Severity:** Minor · **Status:** Confirmed, out of master-pipeline scope · **Locus:** Code (Bulk Tests only)

`OSM_DERIVED_ACRES = 8342.28` was hardcoded in both `Phase_5.py` and
`Bulk Tests/Julia/Step3_Final_Comparison.jl`. The Python master was corrected to compute the
value live (**8,564.23 acres** authoritative). The Julia daughter script still carries the stale
constant. The docs argue it's out of the master pipeline's path and therefore inert — true, but
it's a 2.7% error sitting in a script named "Final_Comparison" that someone will eventually run.

**Confirmed and widened, `VERIFIED` (Parity Audit F-2, 2026-07-26):** grepped for the literal
constant across the whole Phase 5 directory. **All three `Bulk Tests/{R,python,Julia}/
Step3_Final_Comparison.{R,py,jl}` scripts carry the identical stale `8342.28`** — not a
Julia-only issue as originally scoped, it's all three Bulk Tests daughter scripts equally. The
master pipeline is clean in all three languages: `Data/Julia/Jl_Phase5_Oahu_Comparison.csv` and
the QA file `Data/QA/Phase5b_Acreage_QA_Results.csv` both show the live-computed **8,564.23**
agreeing across R/Python/Julia. `01_-_Phase5_Documentation.md` already states this correctly
("that script is not part of the master pipeline") — confirming the doc's own claim rather than
contradicting it, unlike **P5-02**. No master-script fix needed; Bulk Tests scripts are outside
`CLAUDE.md` §8's "Master scripts" list and this audit's scope (see **X-03**), so not edited here.

### P5-06 — Parenthesis bug is real, but only in a Bulk Tests daughter script — master `Phase_5.R` is already clean
**Severity:** Cosmetic · **Status:** Confirmed, out of master-pipeline scope · **Locus:** Code (Bulk Tests only)

Parity Audit **F-3**. `01_-_Phase5_Documentation.md:328` flags: *"Step4 `else if` parenthesis
error — `all(nchar(tmk_df$TMK_clean)) == 9` should be `all(nchar(tmk_df$TMK_clean) == 9)`."*
`VERIFIED` by reading both locations:
- **`Bulk Tests/R/Step4_Offical_Tax_Merge.R:88-90`: the bug is real.**
  `all(nchar(tmk_df$TMK_clean)) == 9` — `all()` wraps only `nchar(...)`, coercing a vector of
  positive integers to `TRUE` (all nonzero), then compares `TRUE == 9` (always `FALSE`). This
  `else if` branch can never fire, exactly as the doc says, and the file even carries its own
  `[REVIEW NEEDED]` comment flagging it.
- **`Phase_5.R:313-315` (the master script): already correct.**
  `all(nchar(tmk_df$TMK_clean) == 9) && all(nchar(na.omit(tax_data$TMK_clean)) == 8)` — properly
  parenthesized, `nchar(...) == 9` evaluated per-element before `all()`. The master pipeline does
  not have this bug; it only exists in the (out-of-scope) Bulk Tests sibling.

No fix applied — the affected file is a Bulk Tests daughter script, outside `CLAUDE.md` §8's
master-script list and this audit's scope, consistent with **P5-05**/**X-03**.

### P5-07 — R's TMK column-name candidate list is a strict subset of Python/Julia's
**Severity:** Minor (dormant — current data matches on all three) · **Status:** Fixed 2026-07-27 (Decision 6) · **Locus:** Code

Parity Audit **F-4**. `READ`, then `VERIFIED` against the actual cadastre schema:
- **R** (`Phase_5.R:160`): `tmk_columns <- c("TMK", "PARCEL_ID", "Parcel_ID", "parcel_id",
  "TAX_MAP_KEY", "tmk")` — **6 candidates**.
- **Python** (`Phase_5.py:95-98`) / **Julia** (`Phase_5.jl:96-98`): identical 10-item list —
  `TMK, PARCEL_ID, Parcel_ID, parcel_id, TAX_MAP_KEY, Tax_Map_Key, tax_map_key, MAPKEY, mapkey,
  tmk` — R is missing `Tax_Map_Key`, `tax_map_key`, `MAPKEY`, `mapkey`. All three use the same
  priority-order-first selection logic (`intersect(...)[1]` in R; `next(... for col in
  TMK_CANDIDATES ...)` in Python; a `for candidate in [...]` loop in Julia) — the divergence is
  purely in candidate *set size*, not selection order or tie-break.
- **Currently dormant:** `VERIFIED` — read the actual parcel GPKG schema
  (`pyogrio.read_info(...)['fields']`); the real column is lowercase `tmk`, which is the *last*
  candidate in all three lists (R's shorter list still contains it), so all three converge on the
  same column today. If the county ever republishes this cadastre with one of the 4 R-missing
  names, R hard-stops (`[FATAL] No TMK column identified`) while Python/Julia silently succeed —
  a latent three-brothers gap, not a live one.

**Fixed (Decision 6, 2026-07-27):** extended `Phase_5.R:160`'s `tmk_columns` to the identical
10-candidate list Python/Julia already use (`TMK, PARCEL_ID, Parcel_ID, parcel_id, TAX_MAP_KEY,
Tax_Map_Key, tax_map_key, MAPKEY, mapkey, tmk`), same priority order. Four-line change, no
behavior change on current data (still resolves to `tmk`, last in the list either way) — removes
the latent hard-crash risk cheaply rather than continuing to track it. Only R touched.

### P5-08 — Julia's Oahu-boundary test uses a different method entirely, not just a different predicate
**Severity:** Minor (dormant — current data agrees) · **Status:** Fixed 2026-07-27 (Decision 7 — reclassified from author-call to code-affecting, resolved by the Decision 2 vendoring) · **Locus:** Code

Parity Audit **F-5**. The checklist item asked to verify predicate/CRS agreement; the actual
divergence is a level deeper — the three languages don't test the same *thing*:
- **R** (`Phase_5.R:99-114`): downloads the real Honolulu County polygon via `tigris::counties()`,
  reprojects it to the OSM CRS, then `st_filter(osm_golf_sf, oahu_boundary_sf, .predicate =
  st_intersects)` — full golf-polygon-vs-full-county-polygon intersection test.
- **Python** (`Phase_5.py:126-137`): `pygris.counties(state="HI", cb=True).query("NAME ==
  'Honolulu'")`, then `osm_golf_geo.geometry.intersects(boundary_union)` — same method as R
  (real county polygon, full-geometry intersects), via Python's Tigris equivalent.
- **Julia** (`Phase_5.jl:82-83,160-169`): **no Tigris/Census call anywhere in the file.** Instead,
  a hardcoded rectangular lat/lon box (`in_oahu(lon,lat) = -158.5<=lon<=-157.6 && 21.2<=lat<=
  21.9`), tested against each golf polygon's **centroid** (`ArchGDAL.centroid(g)`), not its full
  geometry. This is a materially different methodology, not a predicate/CRS variant of the same
  one — no Census boundary source, no polygon-vs-polygon test, a proxy rectangle standing in for
  the county shape (which, notably, legally also includes the remote Northwestern Hawaiian
  Islands — Julia's box would exclude any golf course there; R/Python's real county polygon would
  not, though none currently exist to matter).
- **Currently dormant, `VERIFIED`:** all three languages' on-disk `*_Phase5_Oahu_Comparison.csv`
  report **identical Total Golf Courses = 39** and identical OSM-derived footprint (8,564.23
  acres) — no golf course centroid currently falls near enough to the Oahu coastline/county
  boundary for the two methods to disagree. A real divergence in method, producing no current
  divergence in outcome. Originally flagged for the author as needing a Julia Tigris-equivalent
  dependency (`Tigris.jl`/manual Census API call) — a larger lift than this audit's scope.

**Reclassified and fixed (author correction + Decision 7, 2026-07-27): cheaper than logged — no
new dependency needed.** Once **X-09** vendors `tl_2022_us_county.shp` locally, Julia can read the
exact same file R and Python now use via `GeoDataFrames.read`/`ArchGDAL`, with no `Tigris.jl`
equivalent required. `Phase_5.jl`'s hardcoded lat/lon bounding box + centroid test (the
`in_oahu()` function, `OAHU_LON/LAT_MIN/MAX` constants used for the Step-1 boundary test) replaced
with a real polygon-vs-polygon `ArchGDAL.intersects()` test against the vendored Honolulu County
boundary (`STATEFP=="15" & NAME=="Honolulu"`, reprojected to the OSM CRS) — matching R's
`st_filter(..., st_intersects)` and Python's `.intersects(boundary_union)` exactly, not just in
outcome. `VERIFIED` via a standalone diagnostic: the vendored shapefile read through
`GeoDataFrames.jl` returns `STATEFP`/`NAME` as `String` (not some other type that would silently
fail the `==` comparison) and finds exactly 1 Honolulu County row. **Note:** a separate, unrelated
use of the same bounding-box constants survives in Step 5 (`Phase_5.jl:281-284`, a coarse
pre-filter over the M=100 national imputed datasets before exact polygon matching) — left alone,
since it's a performance shortcut ahead of the real precision check, not the boundary-membership
test this entry is about; removing it would be a `CLAUDE.md` §2.4 performance change, out of
scope. Not executed beyond the diagnostic (§2.1).

### P5-09 — Julia's Step 3 "official tax-assessor area" cross-check is silently dead; also finds 1 more TMK than R/Python
**Severity:** Minor · **Status:** Confirmed · **Locus:** Code

Parity Audit **F-6**/**F-7**. Two related, `VERIFIED` findings from the actual on-disk comparison
tables:
- **R doesn't attempt this diagnostic at all** — `Phase_5.R`'s Step 3 (lines 182-295) has no
  cadastre-attribute join or "official area" computation; its `Phase5_Oahu_Comparison.csv` has no
  such row.
- **Python attempts it and succeeds:** `Phase_5.py:304-318` joins the Step-2 TMK list against the
  full reprojected parcel attribute table on `tmk`, finds `dpp_approved_area_acres`, and reports
  **"Total Official Area (acres)" = 664.77**, with **"TMKs Matched in Cadastre" = 1,072** — an
  exact 1:1 match against "Total Unique TMKs (Step 2)" = 1,072.
- **Julia attempts it and cannot succeed, silently:** `Phase_5.jl:201` (`select!(parcels_geo,
  [:geometry, :tmk])`, in Step 1) drops every attribute column — including
  `dpp_approved_area_acres`/`dpp_stated_area`/`rpa_stated_area` — before Step 3 ever runs, so
  `area_col` (line 265-266) can never be found and no "Total Official Area" row is ever produced;
  confirmed absent from `Jl_Phase5_Oahu_Comparison.csv`. Additionally, Julia's "TMKs Matched in
  Cadastre" = **6,556** against only **1,073** unique TMKs from Step 2 — a real fan-out, not a
  typo: `VERIFIED` by reading the raw cadastre (`All_Parcels_....gpkg` via `pyogrio.read_info`),
  `tmk` is **not** a unique key at the raw-parcel-feature level (177,392 rows, only 171,900
  distinct `tmk` values, 5,491 duplicate rows — consistent with golf-course parcels being
  disproportionately subdivided into CPR/leasehold sub-records). Because Julia's Step 1 never
  deduplicates by `tmk` before the Step 3 join, and golf-adjacent TMKs are apparently
  over-represented among the duplicated keys, the inner join fans out 6× rather than the ~3%
  county-wide average would predict. Python's matching join does not fan out (1,072 = 1,072
  exactly), suggesting Python's parcel attribute table effectively behaves as tmk-unique for this
  specific TMK subset — not fully root-caused beyond what's stated here.
- **Also found: Julia's Step 2 finds 1,073 unique TMKs vs R/Python's 1,072** — a genuine off-by-one
  at the TMK-extraction stage, despite all three computing the *same* OSM-derived acreage to two
  decimal places (8,564.23). Likely traceable to Julia's manual double-loop
  `ArchGDAL.intersects`/`ArchGDAL.intersection` (lines 227-238) behaving slightly differently at a
  polygon-edge case than R's `st_intersection`/Python's `gpd.overlay` — not traced further; the
  acreage identity suggests it's a near-zero-area sliver fragment, not a missed golf course.
- **Impact:** all of the above is confined to a **diagnostic/QA row that feeds no published
  number** — `official_area_acres` and `matched_parcels` are never used in `q_bar`/opportunity-cost
  pooling in any language. Logged as a function-parity gap, not a headline-number risk. **F-7**
  (the historical "37→33→29 course-count ladder") was already reviewed as "explained clearly and
  correctly... not a defect" in an earlier pass; current on-disk data shows all three at **39**
  Oahu courses today, confirming the dedup/matching *algorithm* (`Longitude`/`Latitude` group,
  nearest-polygon + 500 m cap, max-`Holes` tie-break) reads as identical across all three even
  though the final funnel counts aren't independently persisted anywhere to directly re-verify
  past Step 1.

### P5-10 — `Phase_5.py` crashes unconditionally on this machine's default console encoding
**Severity:** Major (crashes every run, not an edge case) · **Status:** Fixed 2026-07-26 · **Locus:** Code

Found during the dress-rehearsal cascade, `VERIFIED` by execution. `Phase_5.py`'s `run_step6()`
prints `"Performing spatial intersection (golf courses ∩ zoning)..."` — the Unicode "∩"
(intersection) character. Python's `print()` encodes to the console's active codepage; this
machine's default Windows console codepage is `cp1252`, which has no mapping for `∩`, so the
print raises `UnicodeEncodeError: 'charmap' codec can't encode character '∩'` and the whole
script halts. **Not a rehearsal artifact** — this is unconditional on any normal invocation of
`Phase_5.py` on this machine (`python Phase_5.py` from a plain terminal), not something the
scratch-redirect setup caused or masked. R's equivalent print (`Phase_5.R`'s `cat()`, identical
"∩" character) does **not** crash — `Rscript.exe`'s console output handling differs from Python's
`print()`/`cp1252` interaction on this setup. Grepped all Python master scripts for other
non-ASCII characters inside `print()` calls — this was the only instance.

**Fixed:** replaced "∩" with ASCII "x" (`"...golf courses x zoning)..."`). Purely a print-string
change, zero effect on any computed value. Re-ran Phase 5 end-to-end after the fix — completed
clean, matching R's and Julia's 6,066.2-acre total exactly.

---

## Phase 6 — Visualization

### P6-01 — Two different national Grand Means inside the same document
**Severity:** Major · **Status:** Open · **Locus:** Docs

`01_-_Phase6_Documentation.md` line 33: **$0.944T**, in the "Last Verified Run" table.
Line 420 (§Scripts 10–14): *"actively routing and plotting the global Tri-Language Grand Mean
(**$0.938T**)"*.

Scripts 10–14 produce the Lorenz curve, the waffle chart, and the counterfactual area chart —
i.e. the figures that go in the thesis. If they are rendering $0.938T while the maps render
$0.944T, two thesis figures disagree by $6B. Also note neither matches the Phase 3 Summary's
Grand Mean of "approximately $943 billion", which is the mean of three M=5 estimates
(**P3-01**) and will move on re-run.

### P6-02 — `grand_mean_se` averages standard errors arithmetically
**Severity:** Major · **Status:** Open · **Locus:** Code

`Phase_6.jl:561`:
```julia
grand_mean_se(p_py, p_r, p_jl) = mean([lookup_se(reg_py, p_py), lookup_se(reg_r, p_r), lookup_se(reg_jl, p_jl)])
```

The Grand Mean **point estimate** is the arithmetic mean of three Rubin-pooled estimates —
which is a deliberate, well-argued choice (Phase 6 Summary §What was solved defends it
correctly against pooling all 300 draws). But the Grand Mean **SE** is then the arithmetic
mean of three SEs, which is not the uncertainty of that quantity in any framework:

- It ignores the **between-implementation** spread of the three point estimates entirely.
- It is neither the SE of a mean of three estimates (which would shrink) nor a conservative
  envelope (which would widen).
- It quietly asserts the three implementations agree perfectly, which is the very thing the
  Grand Mean exists to demonstrate rather than assume.

Feeds `6.141_Marginal_Effects` (delta-method CIs built from `se_b0`, `se_holes`, `se_urban`) and
any Grand Mean interval on the Forest Plot. If the Grand Mean is framed as descriptive — "here
are three estimates, here is their centre" — the honest move is to plot the three intervals and
the spread, and not attach an interval to the mean at all.

### P6-03 — Meta `.qmd` wrappers include a Phase 7 file that isn't in the project
**Severity:** Minor · **Status:** Question · **Locus:** Code

`Meta_Summary.qmd:42` and `Meta_Documentation.qmd:42` both `include` from
`../Phase 7 Documentation, Discussion and Write Up/other/Phase7_{Summary,Documentation}.md`.
No Phase 7 files were supplied, and `00_-_Phase6_Summary.md` states that Phase 6 output feeds
the manuscript *"bypassing Phase 7's traditional role as a separate 'documentation' stage."*

If Phase 7 was folded into the thesis proper, both `.qmd` files will fail to render. If it still
exists, it's outside the review scope and should be added.

### P6-04 — Phase 6's "function parity" question doesn't apply the way it does in Phases 1-5: R and Julia implement disjoint script sets, by design
**Severity:** Cosmetic (clarifying, not a defect) · **Status:** Confirmed, documented already · **Locus:** —

Surfaced while working Parity Audit Part G. Unlike Phases 1-5, where each language independently
implements the *same* pipeline stage, **master `Phase_6.R` and master `Phase_6.jl` implement
completely non-overlapping sets of visualizations.** `VERIFIED` by reading every `run_*`/`module`
definition in both master files:
- **R** (`Phase_6.R`): Scripts 1, 2, 3, 4, 7, 8, 9, 9b, 15, 16 — spatial maps, LaTeX tables,
  Oahu/zoning figures, residual maps.
- **Julia** (`Phase_6.jl`): Modules 5, 6, 10, 11, 12, 13, 14 — econometric/statistical plots
  (Forest Plot, Marginal Effects, MICE diagnostics, Hawaii Gap Dumbbell, Lorenz Curve, Zoning
  Waffle, Urban/Rural Bifurcation).
- **Zero overlap** in script/module numbers between the two master files.

This is deliberate, not a silent gap: `01_-_Phase6_Documentation.md:370` states outright
*"Spatial map scripts are not translated to Julia — those outputs are consolidated into
`Phase_6.R`. Script 8 (LaTeX tables) and Advanced Plots (10-14) are fully executed by
`Phase_6.jl`."* Recorded here only to make explicit, for whoever reads **G-3**/**G-4** next, that
"does R do the same thing as Julia" in Phase 6 is a question about *shared methodology*
(Grand Mean = arithmetic mean of independently-pooled per-language estimates, used in both
files' respective disjoint scripts) rather than *shared figures* — the two master scripts were
never meant to be checked against each other function-by-function the way Phases 1-5 are.

### P6-05 — `rubin_pool` (Phase_6.jl) verified clean: matches Phase 3's simple-total pooling formula, not a drifted copy
**Severity:** N/A (verified clean) · **Status:** Verified sound · **Locus:** Code

Parity Audit **G-4**. `Phase_6.jl:729-736`:
```julia
q_bar = mean(vals); B = var(vals); T = B * (1 + 1/M); se = sqrt(max(T, 0))
```
This omits the within-imputation variance term (`V_W`) that Phase 3/4's full Rubin's Rules
formula (`V_T = V_W + V_B·(1+1/M)`) carries — at first read this looks like exactly the "second,
drifted copy of a pooling formula" the checklist item warns about. **It isn't.** `rubin_pool`
pools **scalar per-course/per-imputation totals** (e.g. one dollar total per imputation, no
individual model SE attached to each draw) — for that quantity, there is no within-imputation
variance component to add, because each pooled value isn't itself an estimate-with-a-SE. This is
exactly the same simplified formula Phase 3's own `pool_acreage()` (`Phase_3.R` etc.) uses for
the national acreage total — `se = sqrt(v_b·(1+1/m))`, algebraically identical to `rubin_pool`.
**Verified as a correct reuse of the appropriate formula for this quantity, not a drift from the
regression-coefficient formula** (which legitimately needs `V_W`, since each per-imputation
coefficient has its own model SE). No action needed.

### P6-06 — `log(Opportunity_Cost)` labels/comments corrected to `log(1 + Opportunity_Cost)`; one residual-calculation `log`/`log1p` mismatch found and left as-is
**Severity:** Cosmetic · **Status:** Fixed (labels), noted (residual calc) · **Locus:** Code

Parity Audit **G-6**. Phase 4's actual DV is `log1p(Total_Opportunity_Cost)` in all three
languages (confirmed under **X-02**/**E-1**), but several Phase 6 figure labels/captions in both
R and Julia said plain `log(Opportunity_Cost)`. **Fixed, text-only, zero numeric impact** — all
instances describing Phase 4's fitted model specifically:
- `Phase_6.jl:96,151,450,480,481,572,573` (Marginal Effects axis/caption, MICE Imputation
  Diagnostic axis/title, code comments).
- `Phase_6.R:2894` (Script 15 Residual Map caption) and the adjacent `2888` residual-definition
  caption line.
- **Not changed:** `Phase_6.jl`'s Urban/Rural Bifurcation scatter (module 14, lines
  1919/1925/1962/1994/2025) — checked the underlying fit (`Phase_6.jl:1892`,
  `ols(log.(ac), log.(ac .* bv))`) and confirmed it's a genuinely separate, strictly-positive-
  filtered log-log OLS fit, unrelated to Phase 4's model — its "log(Acreage)"/"log(Opportunity
  Cost)" labels were already accurate and left alone.
- **Found, not fixed — a real (if numerically dead) inconsistency in `Phase_6.R`'s Script 15
  residual calculation** (`Phase_6.R:2738`): `log_residual = log(acreage * Baseline_Value_Per_Acre)
  - predicted_log`, where `predicted_log`'s coefficients were fit on `log1p(OC)`. The "actual"
  side uses plain `log()`, the "predicted" side implicitly represents `log1p`-scale fitted values
  — a genuine "+1" mismatch between the two terms being differenced. Not fixed, since it changes
  a computed value (residuals feeding a published figure) rather than just a label — flagged in
  a code comment at the site instead, per `CLAUDE.md` §2.3/§5 (don't silently change a number
  that feeds a published figure). Negligible in practice: real OC values are $10K+, so `log(x)`
  vs `log(1+x)` differ by <1e-4 — far below any visible residual-map color-scale threshold.

### P6-07 — Script 9's `get_acreage()`/`pick(everything())` column-agnostic fix verified: genuinely produces tri-language output
**Severity:** N/A (verified clean) · **Status:** Verified sound · **Locus:** Code

Parity Audit **G-5**. `Phase_6.R`'s `get_acreage(df)` helper (three instances: lines 1701, 2116,
2707 — Scripts 9, 9b, 15) returns `osm_acreage` if present, else `final_acreage`, called via
`pick(everything())` inside `mutate()` so the whole current data frame is visible to the closure
— the correct modern-`dplyr` idiom for this (`cur_data()`'s replacement). `VERIFIED` each call
site actually invokes the enclosing pooling function **three times, once per language**
(`pool_oahu_oc(R_IMPUTED_PATHS,"R")` / `(...,"Py")` / `(...,"Jl")` at `Phase_6.R:1849-1851`; same
pattern at `2827-2829` for Script 15). Confirms the fix does what the checklist expected — the
bug it replaced (a bare `df$final_acreage` reference, which would silently return `NULL`/error
for Python/Julia's `osm_acreage`-named frames and degrade the Grand Mean to R-only) is fully
closed, not just patched in one call site.

---

## Verified sound — no issue raised

Recorded so the register isn't read as a list of everything that was looked at.

- **Rubin's Rules implementation.** Correct in all six places it appears
  (`Phase_3.{py,R,jl}`, `Phase_4.{py,R,jl}`). `V_T = V_W + (1+1/M)·V_B`, FMI, and
  Barnard–Rubin adjusted df all match the literature. **P3-01 is a provenance problem, not a
  formula problem** — this distinction is worth keeping crisp at defence.
- **Dependent variable.** `log1p(acreage × BVPA)` consistently in all three languages
  (`Phase_4.R:99`, `Phase_4.py:89`, `Phase_4.jl:92`). The Phase 6 audit's grep for `log(acreage)`
  confirms the Script 15 offset bug did not survive anywhere else.
- **Grand Mean point estimate.** Arithmetic mean of three independently pooled estimates rather
  than a 300-draw single pool. Correct, and the Phase 6 Summary's defence of it is the sharpest
  methodological writing in the corpus. (The **SE** is the problem — **P6-02**.)
- **EPSG:5070 reprojection before area computation**, in all three languages, with the reasoning
  explained. Correct and well justified.
- **FIPS zero-padding fix.** The highest-value catch in the project.
- **Script 15 residual fixes.** Both the log-unit offset (`log(acreage)` → `log(acreage × BVPA)`)
  and the dollars-minus-acres units error were caught and corrected; the `Holes ∈ [9,72]` guard
  against the 252-hole aggregate record is a good catch.
- **37 → 33 → 29 course-count ladder.** Explained clearly and correctly in the Phase 5 Summary.
  Not a defect — the dollar totals attached to it are (**P5-01**).
- **M = 100 justification.** The Phase 3 Summary's argument for M=100 over M=5 — Monte Carlo
  error scaling as 1/√M, and the M vs. chain-iteration distinction — is correct and unusually
  well written. It just doesn't describe the run that got published.

---

## Open questions for the author

1. **P3-01 / P4-01** — Do M=100 Phase 3 and Phase 4 outputs exist anywhere, or is the M=5 pilot
   the only completed run? This determines whether the fix is "regenerate the tables" or
   "re-run the pipeline."
2. **P5-01** — Where does $28.6B come from? It is the denominator for the thesis's headline
   $1.3B.
3. **P5-02** — Was the `sf` P-1 re-run actually performed? The Summary says yes; the
   Documentation says no.
4. ~~**P2-02** — Does R's Phase 3 input include the 500 m nearest-neighbour tier?~~ **Answered
   2026-07-24: yes**, confirmed against the live on-disk data. See P2-02.
5. ~~**X-02** — Is R's `final_acreage` (OSM+Tigris) intended as the R arm's permanent input, or
   should the parity comparison run on `osm_acreage` across all three?~~ **Answered 2026-07-27:**
   Tigris Tier 2 removed (Decision 3); R now runs `osm_acreage` unconditionally, identical
   construction to Python/Julia. See **X-02**/**P2-03**.
6. **X-03** — Which tier (`Bulk Tests/` vs `Data/`) produced the currently published tables?
7. **P1-01 / P1-05** — Should R's `futuremice(method="rf")` call explicitly exclude `Holes` and
   `Ownership_Type` from imputation (via `predictorMatrix`), so R's MICE predictor set is
   structurally identical to Python/Julia's instead of incidentally different because R's regexes
   happen to leave a few real NAs where Py/Jl fabricate values? Small row counts (9 and 1) but a
   real structural difference in the imputation model, not just a value.
8. **P1-05** — Should `Phase_1.py`'s `extract_ownership()` be fixed to recognize `"Semi Private"`
   as its own category (currently collapsed into `"Private"` for ~1,662 of 16,297 courses, 10.2%)?
   If yes, Python's Phase 3 (already run at M=100 per **P3-01**'s resolution) would need
   re-running with the corrected predictor.
9. **P1-06** — Should Python gain the same (lat, lon, Course_Name) row-level dedup that R and
   Julia already have? And separately: R and Julia currently use different tie-break rules on
   ties (Holes-descending vs first-encountered) that happen not to have collided yet — worth
   picking one explicit rule for all three before that changes.
10. **X-04** — Does the thesis manuscript currently cite the M=5 Phase 3 Summary tables, or the
    2026-06-12 M=100 output files (`Final_Thesis_Figures/`, `Data/*_Rubins_Rules_Summary.csv`)?
    Evidence points to the former: the Jun-12 run's actual pooled National OC
    (R $935.521B / Python $938.309B / Julia $950.637B, Grand Mean ≈$941.5B) doesn't match the
    published $936.0B/$943.0B/$951.4B (Grand Mean $943.5B) closely enough to be the same run,
    read to the same precision. If the manuscript does cite the M=5 pilot, **a closer-to-final
    version of the headline number is already sitting on disk** — worth knowing before deciding
    whether the next step is "wait for the frozen re-run" or "reconcile this existing run once
    P1-01/P1-05/P1-06 are addressed."