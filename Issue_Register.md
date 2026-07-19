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
| Cross-cutting | 1 | 2 | 0 | 0 | 3 |
| Phase 1 | 0 | 1 | 2 | 0 | 3 |
| Phase 2 | 0 | 0 | 2 | 0 | 2 |
| Phase 3 | 2 | 1 | 3 | 0 | 6 |
| Phase 4 | 0 | 2 | 1 | 0 | 3 |
| Phase 5 | 0 | 2 | 3 | 0 | 5 |
| Phase 6 | 0 | 2 | 1 | 0 | 3 |
| **Total** | **3** | **10** | **12** | **0** | **25** |

---

## Cross-cutting

### X-01 — Published Phase 3 results were produced at M = 5, not M = 100
**Severity:** Critical · **Status:** Confirmed · **Locus:** Docs (code is correct)

The single most consequential finding. Full detail under **P3-01**; recorded here because it
propagates into `00_-_Phase3_Summary.md`, `Meta_Summary.pdf`, and any thesis prose quoting the
$943B confidence interval.

### X-02 — R runs a different acreage variable than Python and Julia, end to end
**Severity:** Major · **Status:** Confirmed · **Locus:** Code (by design) / Docs (framing)

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

### X-03 — Bulk-test sub-scripts are undocumented dependencies of the review
**Severity:** Major · **Status:** Question · **Locus:** —

The documentation repeatedly cites results, fixes, and canonical outputs living in
`Bulk Tests/{R,Julia,python}/`. Phase 4 §4D states outright that the canonical `Data/`
output directories are **empty** and that all regression CSVs exist only under `Bulk Tests/`.
None of these scripts or CSVs were supplied, so every claim sourced from them is currently
unverifiable. Several numeric conflicts below (**P3-05**, **P4-01**) may resolve trivially
once it's clear which tier produced which table.

---

## Phase 1 — Spatial Parsing & Economic Baseline Valuation

### P1-01 — `extract_holes()` fabricates `Holes = 18` on regex failure (Python only)
**Severity:** Major · **Status:** Open · **Locus:** Code

`Phase_1.py:53` returns `18` as the default when the holes regex fails to match; R returns
`NA` for the identical input. The Phase 1 documentation logs this as a "minor cross-language
inconsistency" — it is more than that:

1. `Holes` is a **regressor** in the Phase 4 model, not a passive field.
2. `Holes` is a **predictor** in the Phase 3 MICE model, so a fabricated value propagates into
   imputed acreage and BVPA for other courses.
3. R's `NA` rows are visible to MICE and get imputed; Python's silently become a hard 18.
   The two languages aren't handling missingness differently — Python isn't recording it at all.

This is a plausible contributor to the Python/R coefficient spread that is currently
attributed wholesale to MICE backend differences.

**To determine:** how many rows actually hit the default. If it's a handful, this is a
footnote; if it's hundreds, it's a finding.

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

### Phase 1 — noted, no issue raised

- Cross-language mean BVPA converges to within $5 ($413,695.90 / $413,699.57 / $413,700.97).
- The FIPS zero-padding fix is real, well-explained, and the highest-value catch in the project.
- The +5 Python row count is documented as a `geopandas` dedup default. Plausible, not verified.

---

## Phase 2 — OSM Polygon Extraction & Acreage Matching

### P2-01 — `acreage_source` / primary column asymmetry is structural, not cosmetic
**Severity:** Minor (as documented) · **Status:** Open · **Locus:** Code
**Escalates to:** **X-02**

R carries a three-tier schema (`OSM` / `Tigris` / `MICE_Target`) and coalesces to
`final_acreage`; Python and Julia carry two tiers and `osm_acreage`. The mitigation in the
docs — "Phase 3 scripts filter on `acreage_source != "MICE_Target"` rather than on a positive
value" — is correct and works. The issue isn't the filter, it's that R's MICE target set is
strictly smaller and its acreage values come from a partly different source. Tracked at the
Phase 2 level as schema; tracked at **X-02** as the thing that actually bites.

### P2-02 — "Final data profile heading into Phase 3" table is pre-fallback and contradicts everything else
**Severity:** Minor · **Status:** Open · **Locus:** Docs

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

**Question:** is `R_Phase2_Acreage_Matched_v2.csv` genuinely missing the nearest-neighbour tier,
or is the table simply transcribed from the wrong console block? If the former, R's Phase 3
input is wrong and the whole R arm shifts.

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
4. The "cross-language spread of 1.6%" claim is a spread between three 5-draw estimates. It may
   tighten or widen at M=100; it is currently unknown.

**Action:** re-run all three Phase 3 pipelines at M=100 and regenerate every downstream table.
Nothing needs fixing in the pooling code.

### P3-02 — `Phase_3.jl` retains `m_datasets = 5` as the default in all three function signatures
**Severity:** Critical (as a latent trap) · **Status:** Open · **Locus:** Code

```julia
Phase_3.jl:36   const M = 100
Phase_3.jl:43   function run_imputation(input_csv::String, out_dir::String; m_datasets::Int = 5)
Phase_3.jl:125  function run_pooling(in_dir::String, out_csv::String; m_datasets::Int = 5)
Phase_3.jl:231  function run_acreage_summary(in_dir::String, out_csv::String; m_datasets::Int = 5)
```

Harmless **today**: `main()` passes `m_datasets = M` to all three. But any call that omits the
keyword — an interactive `include()`, a REPL invocation, a future bulk script — silently runs
at M=5 and writes a file whose name and header claim M=100. Given **P3-01**, this is very
likely the mechanism by which the M=5 results were generated in the first place, and it is
still armed.

Python (`Phase_3.py:48,131,223`) has the same shape but defaults to `m_datasets=100`, so it
fails safe. The fix is to change the Julia defaults to `M` or make the keyword mandatory.

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
**Severity:** Major · **Status:** Open · **Locus:** Docs

- `00_-_Phase5_Summary.md`: *"A subsequent re-run with updated `sf` geometry handling resolved
  the divergence; all three languages now report P-1 = 744.6 acres, with all zoning classes
  agreeing across implementations to within 0.01 acres."*
- `01_-_Phase5_Documentation.md`: R reports **523.5** acres (total ~5,845); Python/Julia report
  744.6 (total ~6,066); *"root cause unconfirmed"*; listed under **Limitations §6**.

Directly contradictory. If the Summary is right, the Documentation's limitation and cross-language
note must be struck. If the Documentation is right, the Summary is claiming a fix that didn't
happen — and 6,066.2 acres, the canonical denominator for every zoning share in the thesis,
is a Python/Julia-only figure that R does not reproduce.

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

### P5-05 — Stale hardcoded acreage constant survives in a Julia daughter script
**Severity:** Minor · **Status:** Open (partially fixed) · **Locus:** Code

`OSM_DERIVED_ACRES = 8342.28` was hardcoded in both `Phase_5.py` and
`Bulk Tests/Julia/Step3_Final_Comparison.jl`. The Python master was corrected to compute the
value live (**8,564.23 acres** authoritative). The Julia daughter script still carries the stale
constant. The docs argue it's out of the master pipeline's path and therefore inert — true, but
it's a 2.7% error sitting in a script named "Final_Comparison" that someone will eventually run.

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
4. **P2-02** — Does R's Phase 3 input include the 500 m nearest-neighbour tier?
5. **X-02** — Is R's `final_acreage` (OSM+Tigris) intended as the R arm's permanent input, or
   should the parity comparison run on `osm_acreage` across all three?
6. **X-03** — Which tier (`Bulk Tests/` vs `Data/`) produced the currently published tables?