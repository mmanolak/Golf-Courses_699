# Gone Golfing — Project Roadmap & Checklist

**Companion to:** `Issue_Register.md` (what is wrong) — this file is what we do about it, and in
what order.

**Terminology.** "Master scripts" = `Phase_N.{R,py,jl}`. "Meta files" = `Meta_Summary.qmd` /
`Meta_Documentation.qmd`, the include-wrappers. Below reads "master scripts" throughout; flag if
that's not what was meant.

---

## Part 0 — Working agreement

### 0.1 The one rule that governs everything else

> **Diagnose everything → fix all code → re-run once → then document.**

**Why this matters more than it looks.** `P1-01` (`extract_holes()` defaulting to 18) is a Phase 1
code fix. Phase 1 feeds Phase 2 feeds Phase 3 feeds Phases 4, 5, 6. So fixing it invalidates
**every downstream number in the project.** So does `P3-01` (the M=5 re-run). So might `P2-02`.

If we fix P3-01 first and re-run Phase 3 (the expensive step), then fix P1-01 afterwards, we
re-run Phase 3 **again**. Given MICE is already the slow part, that's a mistake we only get to
make once before it hurts.

Therefore: no re-runs until every code fix from Phase 1 through Phase 6 is in. One cascade,
bottom-up, and the numbers are frozen after it.

### 0.2 The second rule

> **Never change correctness and compute substrate in the same step.**

Every acceleration item in Part 6 (RAPIDS, Cython, GPU, added parallelism) changes *how* a number
is computed. If we do that while numbers are still moving, we lose the ability to tell a bug fix
from a numerical artefact. Freeze first, accelerate second, and the acceptance test for any
acceleration is *"does it reproduce the frozen number to tolerance."*

This is also the honest answer to the tension in the plan: the re-run that fixes P3-01 is exactly
the workload that motivates wanting RAPIDS. Do it slow once, correctly. See §6.0 for why the
speed problem is smaller than it currently looks.

### 0.3 Per-phase loop

For each phase, in order 1 → 6:

- [ ] **Diagnose** — read the three master scripts side by side; confirm/refute the register's
      open items; add anything new
- [ ] **Decide** — for each issue, pick: fix / won't-fix-and-document / needs-author-call
- [ ] **Fix** — apply code changes in all three languages together, not one at a time
- [ ] **Log** — update `Issue_Register.md` status + running tally
- [ ] **Defer** — do *not* re-run; do *not* rewrite the phase docs yet (see §0.1, §4.0)

Documentation rewrite happens in Part 4, after the cascade re-run, because the docs must quote
final numbers.

---

## Part 1 — Stage 1: Diagnose (no code changes)

- [ ] **1.1** Resolve `X-03` — establish which tier (`Bulk Tests/` vs `Data/`) produced every
      currently-published table. Until this is known, `P3-01`, `P3-05`, and `P4-01` can't be
      closed, only suspected.
- [ ] **1.2** Answer the six open questions at the foot of `Issue_Register.md`.
- [ ] **1.3** Confirm whether any M=100 output exists anywhere, for any phase, in any language.
      This is the single highest-value fact in the project right now: it decides whether Part 3
      is "regenerate tables" (hours) or "re-run the pipeline" (days).
- [ ] **1.4** Inventory the bulk sub-scripts. They're cited as authoritative throughout the docs
      and none were supplied.
- [ ] **1.5** Decide the fate of Phase 7 (`P6-03`) — folded into the thesis, or still a stage?
      Both `.qmd` wrappers currently fail to render without it.

---

## Part 2 — Stage 2: Fix code, bottom-up

### 2.1 Phase 1 — Parsing & Baseline Valuation

**Diagnostics first (these are counts, not fixes):**
- [ ] Count rows hitting `extract_holes()`'s `18` default in Python (`P1-01`). Compare against
      R's `NA` count on the same input. **If the counts differ by more than a handful, this is a
      finding, not a footnote** — it means Python has been silently fabricating a regressor.
- [ ] Confirm the 34 FIPS-NA courses are the same 34 rows in all three languages, not just the
      same *count*. The Phase 6 audit confirmed the count; identity is the stronger claim.
- [ ] Confirm the +5 Python row delta is the `geopandas` dedup default and not 5 real courses.

**Fixes:**
- [ ] `P1-01` — standardise the unparseable-holes contract. **Recommend `NA` in all three**: it's
      honest, and MICE already imputes `Holes` as a predictor, so NA is *handled*, not lost.
- [ ] `P1-04` **(new — see §2.1.1)** — standardise the RUCC source.
- [ ] `P1-02` — add `course_id` + address fields to Python's output. Cheap, and removes a tripwire.
- [ ] `P1-03` — standardise `Course_Name` (strip suffix in all three, or none).
- [ ] Remove `Phase_1.jl` dead code: `Downloads.download(COUNTY_CB, COUNTY_ZIP)` passes a local
      path where a URL is expected.
- [ ] Remove `ENV["JULIA_NUM_THREADS"] = "24"` from `Phase_1.jl:19` — runtime no-op; thread count
      must be set at launch (`julia -t 24`). Carry the requirement into a run script instead.
- [ ] Remove unused `future`/`furrr`/`parallelly` + `plan(multisession)` from `Phase_1.R`, **or**
      actually use it (see §6.2 — Phase 1 is a good parallelism candidate).
- [ ] `Phase_1.py` — replace hardcoded `as_is_col = "Land Value\n(Per Acre, As-Is)"` with dynamic
      column detection, matching R's `grep()`. A hardcoded column name containing a literal
      newline is a silent-breakage risk on any source refresh.

#### 2.1.1 New issue for the register — `P1-04`

**RUCC is read from different sources in different languages, and one path is a live URL.**
**Severity: Major · Locus: Code**

Per `01_-_Phase1_Documentation.md` §"RUCC data source split":
- R master + Python master: **fetch live from a USDA ERS URL**
- Julia master + Julia bulk: read a **local mirror**, because *"original USDA URL was dead,
  mirrored from WeitzGroup/SciMap-Methods on GitHub"*

Two distinct problems:
1. **Parity break.** RUCC drives the urban/rural branch, which drives which proxy each course
   gets, which drives `Baseline_Value_Per_Acre` — the most consequential variable in the project.
   Three languages reading it from two sources is not a language-independence check on that branch.
2. **Reproducibility.** A live URL fetch means the pipeline's output depends on a third party's
   uptime and versioning. The documentation *already records that a USDA URL died once.* For a
   thesis whose central methodological claim is tri-language reproducibility, a live-fetch
   dependency in two of three arms is a soft spot worth closing before someone else finds it.

**Recommend:** all three read the vendored local mirror; document the provenance and retrieval
date of the mirror in the phase docs. Reproducibility beats freshness for a fixed-vintage study.

---

### 2.2 Phase 2 — OSM Polygons & Acreage

- [ ] `P2-02` — **resolve first, it's a fork in the road.** Determine whether
      `R_Phase2_Acreage_Matched_v2.csv` actually skips the 500 m nearest-neighbour tier. If it
      does, R's Phase 3 input is wrong and the entire R arm shifts — which would also partly
      explain `X-02`'s coefficient gap.
- [ ] `X-02` / `P2-01` — **author call required.** Options:
      - **(a)** Keep Tigris as R-only enrichment; reframe the tri-language claim honestly as
        "three implementations, two acreage definitions."
      - **(b)** Produce an `osm_acreage`-only R variant for the parity comparison, keep
        `final_acreage` as a sensitivity arm. **Recommended** — it gives a clean language
        comparison *and* an extra robustness result, at the cost of one column.
      - **(c)** Extend Tigris to Py/Jl. Expensive (50-state iteration ×2), already rejected on
        cost grounds, and the rejection is well argued.
- [ ] Add `gc()` after the `rm()` at `Phase_2.R:337` — several GB of spatial objects freed
      without a GC hint.
- [ ] Reconcile `PBF_FILE` path against documented location.
- [ ] Document the methodological basis for `MAX_NEAREST_M = 500` inline. It's currently a bare
      constant, and it's a defensible-but-arbitrary threshold that determines 6,147 matches — an
      obvious question at defence.
- [ ] Consider recording the 5–1,500 acre filter's 1,281 drops to a file rather than only a count.

---

### 2.3 Phase 3 — MICE & Rubin's Rules

- [ ] `P3-02` — **do this before any re-run.** Change `Phase_3.jl` defaults
      `m_datasets::Int = 5` → `= M` at lines 43, 125, 231, or make the keyword mandatory. This is
      most likely how the M=5 results were produced, and it's still armed.
- [ ] `P3-01` — no code fix needed. Pooling is correct. This is a re-run + republish (Part 3).
- [ ] `P3-03` — locate the real complete-case number or withdraw the robustness claim. **Do not
      let this one slide**: an unimputed estimate matching the imputed one is the most favourable
      possible result, which is exactly why a transcription error here is dangerous.
- [ ] Verify `Random.seed!(42)` / `random_state=42` / R's seed actually produce reproducible
      output across runs in each language. Seeds are documented; reproducibility isn't verified.
- [ ] Confirm the predictor set is genuinely identical across languages — R detects
      `Course_Type`/`Ownership_Type` dynamically, Julia aliases, Python names it directly. Docs
      say the underlying data is the same; confirm on the data, not the docs.
- [ ] `P3-06` — force fixed-point formatting on Julia's `Pooled_Acres` output.

---

### 2.4 Phase 4 — Econometrics

- [ ] `P4-01` — resolve M labelling once `X-03` is known.
- [ ] `P4-02` — one-word doc fix: `IterativeImputer` → `miceforest`/LightGBM.
- [ ] `P4-03` — **align the two documents.** The Summary's *decomposition-not-causal* caveat is
      correct and well argued; the Documentation's ¶1 reads the coefficient straight as a land
      price gradient. Propagate the caveat, and propagate it into any figure caption built from
      Phase 4 output (Forest Plot, Table 2).
- [ ] Confirm the 34 dropped rows per model are the FIPS-NA courses and are dropped *identically*
      across languages — N=16,258 in all three is consistent with that but doesn't prove it.
- [ ] Decide `log1p` axis labelling for Phase 6 (flagged in Phase 4 §4C obs 4): labels should read
      `log(1 + OC)`, not `log(OC)`.
- [ ] Remove or justify `library(broom)` in `Phase_4.R` (no visible call).
- [ ] Remove `Pkg.add()` from Julia scripts once environments are pinned — see §5.3.

---

### 2.5 Phase 5 — Hawaii

- [ ] `P5-01` — **highest priority in this phase.** Establish the canonical Oahu total and its
      derivation. $1.3B "directly unlockable" is the most quotable number in the thesis and it
      currently swings ±20% depending on which of $25.4B / $28.6B / $31.2B is used. The
      decomposition also inherits no information beyond the zoning acreage split — worth stating
      plainly rather than presenting three dollar figures as if independently derived.
- [ ] `P5-02` — determine whether the `sf` P-1 re-run happened. Summary says resolved (744.6
      everywhere); Documentation says unresolved (R=523.5) and lists it as a limitation. 6,066.2
      acres is the denominator for **every zoning share in the thesis** — if R can't reproduce it,
      that needs saying out loud.
- [ ] `P5-03` — fix the "all four counties" claim (Kauai has no pilot course), and note the
      effect on the 6-point urban/rural gradient.
- [ ] `P5-04` — fix the $456.8M average: it's a mean over 61 courses, not 74. Publish either
      $456.8M/61 or $376.6M/74 — with a matching label.
- [ ] `P5-05` — remove the stale `OSM_DERIVED_ACRES = 8342.28` from the Julia daughter script.
      2.7% error in a file named `Final_Comparison`.
- [ ] Fix the flagged `Step4` parenthesis bug: `all(nchar(tmk_df$TMK_clean)) == 9` →
      `all(nchar(tmk_df$TMK_clean) == 9)`. Currently marked `[REVIEW NEEDED]` and not fixed. As
      written this evaluates `all()` on a numeric vector then compares to 9 — it is always
      `FALSE`, so the diagnostic silently never fires.

---

### 2.6 Phase 6 — Visualization

- [ ] `P6-01` — reconcile $0.944T vs $0.938T. Will move again after the Part 3 re-run, so
      **defer the final value** but fix the mechanism that let two values coexist.
- [ ] `P6-02` — **decide what the Grand Mean interval means.** Averaging three SEs
      (`Phase_6.jl:561`) isn't the uncertainty of the Grand Mean under any framework. Options:
      - **(a)** Plot the three per-language intervals and drop the Grand Mean interval entirely.
        **Recommended** — the Grand Mean is a descriptive centre of three implementations, and
        the honest visual is the spread itself. This is also the *stronger* thesis claim: three
        independent stacks landing close together is more persuasive than an averaged bar.
      - **(b)** Add the between-implementation variance term, and state the framework.
      Either is defensible; the current code is neither.
- [ ] `P6-03` — resolve Phase 7 in both `.qmd` wrappers.
- [ ] Fix the stale `Data/Python` capital-P comment at `Phase_6.jl:7`.
- [ ] Fix the stale `Phase_6.jl:4` header ("scripts 5, 6, and 10" — actually 5, 6, 10–14).
- [ ] Confirm Script 9's `get_acreage()` / `pick(everything())` patch actually produces
      tri-language output. The bug it fixed — silently degrading the Grand Mean to R-only with no
      error — is the exact failure mode that would recur silently.
- [ ] **Convert the 300 imputed CSVs to Parquet.** See §6.1 — this is the single biggest speed win
      available and it belongs to Phase 6's read path more than anywhere else.

---

## Part 3 — Stage 3: The cascade re-run

Only after **every** box in Part 2 is closed.

- [ ] **3.1** Pin environments first (§5.3). A re-run against unpinned packages is not
      reproducible and will have to be done again.
- [ ] **3.2** Re-run Phase 1 → 2 → 3 → 4 → 5 → 6, in order, all three languages.
- [ ] **3.3** Record for each phase: date, M, seed, package versions, wall time, host. This
      becomes the provenance block in the rewritten docs (§4.1) and is what makes `P3-01`
      impossible to repeat.
- [ ] **3.4** **Freeze.** Tag/commit the outputs. Every number in the thesis now traces to this
      run and nothing else.
- [ ] **3.5** Re-derive every published table from the frozen outputs — no transcription. The
      LaTeX `\input{}` fragments already do this correctly for three tables; extend the principle.
      `P3-03` and `P3-05` are both transcription failures; the fix is to stop transcribing.

**Expected:** the headline moves. Point estimates should be stable, but CIs will **narrow ~9%**
(the `√(1.2/1.01)` correction), and the 1.6% cross-language spread may change in either direction
once `V_B` is estimated from 100 draws instead of 5.

---

## Part 4 — Stage 4: Documentation rewrite

### 4.0 Why this is after the re-run

The stated goal is documentation that is *standalone and comprehensible without the thesis PDF*.
That means the docs must quote final numbers. Writing them before Part 3 guarantees rewriting them
after it.

### 4.1 Target structure

The current files mix three genres: **what the phase does**, **what was broken and fixed**, and
**audit logs against CLAUDE.md**. The stated goal only wants the first. Proposal — split:

| File | Audience | Content |
|---|---|---|
| `00_-_PhaseN_Summary.md` | Reader with no context | Purpose, method, results, handoff. Prose. Standalone. |
| `01_-_PhaseN_Reference.md` | Someone running the code | Inputs, outputs, file inventory, params, deps, runtime |
| `ARCHIVE/PhaseN_Devlog.md` | You, or a future auditor | Fix history, audit logs, standardisation passes |

The devlog content is genuinely valuable — the FIPS zero-padding story and the Julia world-age fix
are good engineering history — but it's *provenance*, not documentation, and it's what's currently
making the phase docs unreadable as documentation.

### 4.2 Per-phase Summary template

```markdown
# Phase N — <Title>

## Purpose
What this phase produces, and why the project cannot proceed without it. One paragraph.

## The problem it solves
The gap in the data or the analysis that motivates this phase existing at all.

## Inputs
| Source | Origin | Vintage | Format |

## Method
What it does, step by step. Every non-obvious choice carries its reasoning inline.

## Results
The numbers. Each with provenance: M, seed, run date.

## Design decisions
Choices made, alternatives rejected, and why. (The dual-proxy rationale and the M=100
argument are already model examples of this — keep that voice.)

## Known limitations
What this phase does not establish. Honest scope.

## How Phase N+1 builds on this
The handoff: which outputs, consumed how, to what end.
```

### 4.3 Checklist

- [ ] Agree the split and the template on Phase 1, then apply mechanically to 2–6
- [ ] Rewrite each `00_-_PhaseN_Summary.md` to the template
- [ ] Extract audit/fix history to `ARCHIVE/`
- [ ] Add the §4.1 provenance block to every phase
- [ ] Add a top-level `README.md` — the project has no entry point; a reader currently has to
      guess that `00_-_Phase1_Summary.md` is the front door
- [ ] Reconcile every cross-phase number one final time (`P5-01`, `P6-01`, `P3-05`)
- [ ] Resolve Phase 7 in both `.qmd` wrappers

---

## Part 5 — Stage 5: Refinement

- [ ] **5.1 Standardise comment structure.** The four-section layout
      (`LIBRARIES` / `GLOBALS & PATHS` / `FUNCTIONS` / `EXECUTION`) and the `[METHODOLOGY]` tag
      convention are already good and mostly enforced. Formalise into a linter-checkable rule
      rather than a review-by-hand pass — the audit logs show these drift back.
- [ ] **5.2 Minimise structure.** Candidates: `Phase_6.R` (3,084 lines) and `Phase_6.jl` (2,126)
      are monoliths that were *deliberately* consolidated from modules. Note the tension — the
      audit log shows the consolidation itself introduced bugs (top-level execution blocks, stale
      loop variables). Worth asking whether the monolith earned its keep.
- [ ] **5.3 Pin environments.** `renv` (R), `Project.toml`/`Manifest.toml` (Julia),
      `requirements.txt` or `uv.lock` (Python). Remove `Pkg.add()` from Julia scripts. **This is a
      prerequisite for Part 3, not a cleanup task** — an unpinned re-run isn't reproducible, and
      reproducibility is the thesis's core methodological claim.
- [ ] **5.4** Standardise the `python`/`Python` path casing. Currently works only because Windows
      is case-insensitive; it will break the first time this runs on Linux or in CI.

---

## Part 6 — Stage 6: Performance

### 6.0 Read this before doing any of it

Three facts that change the plan:

**1. The dataset is small.** 16,297 rows. This is not a big-data problem, and most GPU/RAPIDS
reasoning doesn't apply. The cost is `100 datasets × 10 iterations × RF fits`, which is an
*algorithm × M* problem. The right lever is **parallelism across M**, not hardware.

**2. The imputations are embarrassingly parallel.** Each of the 100 is independent by
construction. On an N-core machine this is near-linear speedup with no algorithmic risk —
by far the best effort:reward ratio available. `miceforest` (`n_jobs`) and R's `futuremice()`
already expose this; the first question is whether it's actually engaged, not what to add.

**3. The hot loops are already compiled.** LightGBM is C++. R's `ranger`/`rf` backend is C++.
GDAL/GEOS is C++. NumPy is C/Fortran BLAS. Python isn't slow here because Python is
interpreted — it's slow because it's asking a compiled library to fit 1,000 random forests.
**Cython and a C rewrite cannot fix that**, because the time isn't being spent in the code they'd
replace. Profile before writing a single line of either.

### 6.1 Do these first (cheap, large, low-risk)

- [ ] **Profile.** Establish where the wall time actually goes, per phase, per language. Everything
      below is speculation until this exists.
- [ ] **CSV → Parquet for the 300 imputed datasets.** They're written once and read repeatedly by
      Phases 4, 5, and 6 — Phase 6 alone reads all 300 several times over. Parquet is typically
      5–20× faster to read, with type fidelity as a bonus (it also kills `P3-06`). Likely the
      single biggest win in the project, and it's an afternoon.
- [ ] **Verify parallelism is actually on.** `Phase_1.R` loads `future`/`furrr` and calls
      `plan(multisession)` but never calls `future_map` — parallel setup that does nothing. Check
      the same pattern everywhere before adding more.
- [ ] **Fix the Julia thread bug.** `ENV["JULIA_NUM_THREADS"] = "24"` inside the script is a
      no-op. Julia has likely been running single-threaded this whole time. Set at launch.
- [ ] **Auto-size the worker pool** to the host: `parallelly::availableCores()`,
      `Threads.nthreads()`, `multiprocessing.cpu_count()`, with a memory-aware cap — 100
      concurrent workers each holding a 16k-row frame is fine; 100 each holding an 11 GB PBF is not.

### 6.2 Answers to the questions in the notes

**Are R and Julia compiled? Closer to Python or to C/Fortran?**

| Language | Execution | Tier |
|---|---|---|
| **Julia** | JIT → native machine code via LLVM | **C/Fortran tier**, *when type-stable* |
| **R** | Interpreted; bytecode compiler on by default since 3.4 | **Python tier**. Bytecode ≠ native |
| **Python** | Interpreted; CPython bytecode | Python tier |

So: Julia is the odd one out and genuinely can hit C speed. R and Python are peers. **But per
§6.0 fact 3, this matters far less than it appears** — all three spend their time inside compiled
C/C++/Fortran libraries. The language is glue.

**Can RAPIDS accelerate the Python MICE?**

- RAPIDS is **CUDA-only → Nvidia only.** No AMD, no APU. ROCm exists but has no comparable stack.
- **The P2000 won't run it.** Pascal, compute capability 6.1. Modern RAPIDS requires 7.0+
  (Volta/Turing/Ampere). Support for Pascal was dropped. 5 GB VRAM is also thin.
- **cuML has no MICE.** It has RandomForest, so chained equations could be hand-rolled on top —
  that's a project, not a config change, and it would introduce a *fourth* imputation backend
  whose agreement with the others you'd then have to establish.
- **At 16,297 rows, GPU transfer overhead may exceed the compute saved.** GPUs win on large
  matrices; this dataset is small.

**Verdict: RAPIDS is the wrong lever here.** The slowness is `M × iterations × RF`, and the
answer is cores, not CUDA. Revisit only if profiling contradicts this.

**GPU for graphing and mapping?**

No. `ggplot2`/`sf` and `CairoMakie` are CPU rasterisers and don't use the GPU. (`GLMakie` does,
but it targets interactive display, not 300 DPI publication PNGs.) The Phase 6 bottleneck is
almost certainly reading 300 CSVs — see §6.1.

**Cython?**

Only after profiling, and only if profiling shows time in Python-level numeric loops. If the time
is in `miceforest`/`geopandas`/`shapely`, Cython buys nothing. **"Check NumPy is being used
properly" is the right instinct and strictly precedes this** — vectorising a Python loop is
usually the whole win, and Cython is what you reach for when it isn't.

### 6.3 Then, if profiling justifies it

- [ ] Vectorise any hot Python loops NumPy should own
- [ ] Type-stability audit of the Julia hot paths (`@code_warntype`) — this is where Julia's
      C-tier ceiling is actually claimed or lost
- [ ] Cython only where profiling points
- [ ] Re-profile and confirm against the frozen numbers (§0.2)

---

## Part 7 — Stage 7: Additional language arms (C / Fortran / Rust)

### 7.0 Reframe

The notes file these under performance. **They're worth more as validation than as speed** — per
§6.0 fact 3, a C rewrite of the glue won't beat a C++ RF library that's already doing the work.

But as a *fourth and fifth independent implementation*, they're a genuine methodological
contribution. The whole thesis argument is "three independent stacks converge, therefore the
result isn't an artefact of one toolchain." Extending that to a compiled systems language —
especially one with no MICE ecosystem, forcing an implementation from the algorithm rather than
from a package — is a much stronger claim than "it also runs fast in C."

That reframe also makes the scope tractable: you don't need to port Phases 1–6. You need the
**imputation + pooling core** (Phase 3), because that's where the backend-dependence risk lives
and where the current three arms disagree most.

- [ ] Decide the language. **Rust** is the strongest candidate: memory safety on a from-scratch
      numerical implementation, `polars` for dataframes, `linfa` for ML, real ecosystem, and it's
      the more interesting claim in 2026. Fortran is the more *traditional* econometric gesture;
      C is the least additive.
- [ ] Scope to **Phase 3 core only** (MICE + Rubin's Rules) unless there's a reason to go wider
- [ ] Acceptance test: reproduces the frozen Phase 3 aggregate within the cross-language spread
- [ ] If it doesn't — that's a **result**, not a failure, and a considerably more interesting one
      than if it does

---

## Part 8 — Stage 8: Distribution

Ordered by effort:reward. Each is independently shippable; none blocks the thesis.

### 8.1 R data package — **do this one**

- [ ] Package the golf course dataset (real + imputed) as an R data package
- [ ] Well-trodden pattern (`nycflights13`), low risk, genuinely useful, and it's the artefact
      most likely to actually get used by anyone else
- [ ] Decide what ships: raw, Phase 1 baseline, Phase 2 matched, a sample of imputed sets. 300
      full imputed datasets is a lot of package; consider shipping the seed + a regeneration
      function instead
- [ ] This is also the cleanest path to "summon all the golf course data I have found"

### 8.2 Interactive parameter tool

Goal from the notes: a live user adjusts M, iterations, and RF seed, per language or across all.

- [ ] **Reality check:** one binary bundling three language runtimes is a hard problem, and the
      hard part is packaging, not the tool. Options:
      - **Shiny app (R)** — easiest, deployable to shinyapps.io, no install for the user.
        **Recommended first cut.**
      - **Pluto.jl** — reactive by default, nice fit, Julia-only.
      - **Docker image** — the only clean way to get all three runtimes reproducibly. Best fit for
        the *actual* stated goal, and it doubles as the reproducibility artefact for §5.3.
      - **PyInstaller/Nuitka → exe → MSIX** — Python-only, and MSIX signing/Store submission is
        real overhead for a demo tool.
- [ ] **Scope warning:** letting a user set M=100 live means a multi-minute wait per click. Either
      pre-compute a grid, cap M, or make it explicitly a background job.

### 8.3 Unified interactive program

- [ ] Deferred. This is §8.2 plus §8.1 plus orchestration. Revisit once both exist and there's
      evidence anyone wants it.

---

## Sequencing summary

```
Part 1  Diagnose ────────────────────► no code changes
Part 2  Fix code, Phase 1 → 6 ───────► no re-runs, no doc rewrites
Part 5.3 Pin environments ───────────► prerequisite for Part 3
Part 3  ONE cascade re-run ──────────► FREEZE NUMBERS
Part 4  Rewrite documentation ───────► quotes frozen numbers
Part 6  Profile → parallelise ───────► acceptance test = reproduces frozen numbers
Part 7  Rust/C/Fortran arm ──────────► validation, not speed
Part 8  Package & distribute ────────► independent, non-blocking
```

Everything before the freeze is correctness. Everything after is performance, presentation, and
reach. The line between them is the most important structural decision in the plan.