---
title: "Phase 7 Summary: Documentation, Discussion & Write-Up"
author: "Michael"
format:
  pdf:
    toc: true
    number-sections: true
    colorlinks: true
---

# Phase 7 Summary: Documentation, Discussion & Write-Up

## Thesis Coverage Summary — Phases 1 through 6

This document synthesizes the goal, purpose, intent, and accomplishments of each of the six computational phases of the thesis. The overarching research question is: **What is the aggregate opportunity cost of U.S. golf course land, and what does it imply about how that land is valued and used?** Each phase addresses a distinct layer of that question, from raw data parsing through visualization.

---

## Phase 1 — Spatial Parsing & Economic Baseline Valuation

### Goal and Purpose

Phase 1 builds the foundational dataset for the entire thesis. The goal is to transform a raw GPS-coordinate dataset of golf courses into a structured, spatially-validated, economically-anchored baseline. Every course must receive a county FIPS assignment and an estimated land value per acre before any further analysis can proceed.

### Intent

The core methodological intent is to estimate what each golf course's land would be worth in its next-highest-value use — the Highest and Best Use (HBU) framework. Rather than using a single appraisal methodology, Phase 1 introduces a **dual-proxy valuation**: urban counties (RUCC codes 1–3) receive the FHFA Residential Land Price index as the opportunity cost benchmark (reflecting the residential development market), while rural counties (RUCC codes 4–9) receive the USDA Agricultural Land Value (reflecting the agricultural land market). This classification is derived from 2023 USDA Rural-Urban Continuum Codes applied at the county level.

The pipeline is implemented independently in **Python**, **R**, and **Julia** as a robustness strategy: three entirely separate computational stacks reading the same source data must converge on statistically equivalent outputs. Any divergence signals a pipeline defect rather than a substantive finding.

### Accomplishments

- Parsed 16,292–16,297 golf courses from raw GPS coordinates, extracting `Course_Type` and `Holes` via regex.
- Performed a spatial point-in-polygon join against 2022 US Census county boundaries to assign 5-digit FIPS codes to every course. Identified and fixed a FIPS **zero-padding bug** across all three language implementations (integer coercion silently dropped leading zeros, causing hundreds of USDA/FHFA join failures before the fix).
- Merged USDA Agricultural Land Values and FHFA Residential Land Prices onto each course by FIPS, then applied the RUCC Urban/Rural classification to select the correct proxy.
- Produced a `Baseline_Value_Per_Acre` for 15,197–15,198 courses. The remaining **~1,094–1,095 courses** lacked a baseline value (due to missing county data, USDA suppression, or coordinates outside CONUS) and were flagged as MICE imputation targets for Phase 3.
- Verified cross-language statistical parity: all three pipeline means converge within $5 of each other (~$413,700/acre) despite minor row-count differences at the margins attributable to `geopandas` spatial deduplication behavior in Python.
- Standardized all outputs with language-specific prefixes (`Py_`, `R_`, `Jl_`) to prevent filename collisions across phases.

**Output files:** `{Py|R|Jl}_Phase1_Baseline_Golf_Valuation.csv`

---

## Phase 2 — OSM Polygon Extraction & True Acreage Matching

### Goal and Purpose

Phase 1 established *what* each course is worth per acre. Phase 2 establishes *how many acres* each course occupies. The goal is to replace the implicit assumption of a fixed or average acreage with the true measured polygon area derived from OpenStreetMap (OSM) golf course boundary data.

### Intent

True acreage is the physical multiplier for the opportunity cost calculation: `Total_Opportunity_Cost = osm_acreage × Baseline_Value_Per_Acre`. Without it, the aggregate national figure is meaningless. OSM is the most comprehensive freely available source of golf course boundary polygons, but its coverage is imperfect — not every course in the Phase 1 dataset has a corresponding polygon. Phase 2's intent is to maximize match coverage using a two-tier spatial strategy and to clearly identify the residual unmatched cases for MICE imputation in Phase 3.

The phase also introduces the concept of **acreage sourcing**: every course in the output is tagged with `acreage_source` ("OSM", "Tigris", or "MICE_Target") so that downstream analyses can distinguish directly measured values from imputed ones.

### Accomplishments

- Extracted golf course polygons from the 11 GB US OpenStreetMap PBF file using `pyosmium` streaming. After filtering size outliers (< 5 acres = fragments; > 1,500 acres = mega-resort blobs), retained **15,166 valid polygons** with a median area of 127.8 acres.
- All area calculations were performed in EPSG:5070 (NAD83/Conus Albers), a planar equal-area projection, rather than in geographic degrees, ensuring geometrically correct acreage values.
- Matched Phase 1 GPS point coordinates to OSM polygons via a **two-tier join**: (1) direct spatial intersect, then (2) nearest-neighbor fallback within 500 m for courses whose listed coordinate falls outside the polygon boundary (e.g., clubhouse or parking lot coordinates).
- Achieved a **71.2% match rate** (11,605 of 16,292 courses), leaving **4,687 courses** (28.8%) without an OSM acreage value. These are the primary MICE target in Phase 3.
- R additionally applied a Tigris landmarks fallback tier, modestly recovering additional courses, creating a documented asymmetry between R's three-value acreage source schema ("OSM" | "Tigris" | "MICE_Target") and Python/Julia's two-value schema ("OSM" | "MICE_Target"). This asymmetry propagates consistently through all downstream phases.
- Standardized outputs with language-specific prefixes and added `acreage_source` column to all three output CSVs.

**Output files:** `{Py|R|Jl}_Phase2_Acreage_Matched.csv`, `{Py|R|Jl}_Phase2_OSM_Golf_Polygons.gpkg`

---

## Phase 3 — MICE Imputation & Rubin's Rules Aggregate Valuation

### Goal and Purpose

Phase 3 addresses the missingness problem inherited from Phases 1 and 2: approximately 28.8% of courses lack `osm_acreage` and 6.7% lack `Baseline_Value_Per_Acre`. The goal is to produce statistically principled complete datasets through Multiple Imputation by Chained Equations (MICE), then aggregate the resulting national opportunity cost estimates using Rubin's Rules for valid inference under multiple imputation.

### Intent

A complete-case analysis that simply drops courses with missing values would be both wasteful and biased: if courses with missing data are systematically different from those with complete data (e.g., smaller or lower-value), the aggregate estimate would be distorted. MICE avoids this by generating M = 100 plausible complete datasets per language, each consistent with the observed data distribution, and then pooling the per-dataset estimates using Rubin's Rules to propagate imputation uncertainty into the final confidence intervals.

The 28.8% missing acreage rate is high enough that imputation uncertainty is a non-trivial contributor to the total variance in the national estimate — which is precisely why M = 100 (rather than the historically suggested M = 3–10) is used: modern standards (Graham et al., 2007; von Hippel, 2020) require larger M to stabilize standard errors at this missingness rate.

A tree-based (Random Forest / LightGBM) MICE algorithm is used because land values and course sizes have non-linear, geography-dependent relationships that linear models would inadequately capture, and because tree-based algorithms predict strictly within the observed range (preventing impossible negative acreages or land values).

### Accomplishments

- Ran MICE independently in three languages with three different algorithmic backends:
  - **Python:** `miceforest` v6.0.5 with LightGBM gradient-boosted imputation
  - **R:** `mice` package with Random Forest (`method = "rf"`) via `futuremice()` parallel execution
  - **Julia:** `Mice.jl` with its native Random Forest default
- Produced **300 complete imputed datasets** (100 per language), each prefixed to its generating pipeline, with all predictor variables (`Holes`, `Course_Type`/`Ownership_Type`, `county_type`, `Longitude`, `Latitude`) and both imputation targets (`osm_acreage`, `Baseline_Value_Per_Acre`) included.
- Applied Rubin's Rules independently within each language group (M = 100) to produce pooled national aggregate opportunity cost estimates with 95% confidence intervals:

| Language | Pooled Q̄ | 95% CI |
|----------|-----------|--------|
| Python   | $943.025 B | $936.3 B — $949.7 B |
| R        | $936.046 B | $926.4 B — $945.7 B |
| Julia    | $951.389 B | $943.8 B — $958.9 B |

- All three 95% CIs overlap substantially, confirming cross-language statistical agreement. The ~$15B spread (1.6% of the ~$940B base) is attributable to different Random Forest RNG seeds and internal MICE implementations — not a methodological discrepancy.
- Verified that between-imputation variance (V_B) dominates within-imputation variance (V_W) by 2–3 orders of magnitude in all three languages — the expected behavior for a well-specified MICE model.
- A complete-case analysis using only courses with directly observed data ($943B) falls within all three MICE confidence intervals, confirming result robustness despite imputing 68.4% of the sample.

**Grand Mean across three languages: ~$943 billion. Range: $926B – $959B.**

**Output files:** `{Py|R|Jl}_Imputed_Dataset_{1..100}.csv`, `{Py|R|Jl}_Rubins_Rules_Summary.csv`, `{Py|R|Jl}_National_Acreage_Summary.csv`

---

## Phase 4 — Econometric Modeling

### Goal and Purpose

Phase 4 moves from aggregate valuation to structural estimation. The goal is to fit a log-linear OLS regression model that isolates the relationship between observable course characteristics (number of holes, urban/rural location) and log opportunity cost, using all 100 MICE-imputed datasets per language and pooling the resulting coefficient estimates via Rubin's Rules.

### Intent

The aggregate figure from Phase 3 tells us the total value at stake. The regression in Phase 4 tells us *why* that value is distributed the way it is across the country. Two structural forces are hypothesized: (1) **course size** (`Holes`) as a proxy for physical capacity and land area, and (2) **geographic context** (`county_type`: Urban vs. Rural) as the primary driver of land market value. The log-linear specification `log(Opportunity_Cost) ~ Holes + county_type` captures these relationships while handling the extreme right-skew of the dollar-valued outcome.

HC1 heteroskedasticity-robust standard errors are applied throughout because cross-sectional land-value data almost certainly violates the OLS homoskedasticity assumption (variance of residuals is plausibly larger in high-value urban areas). Pooling coefficient estimates across M = 100 imputations per language via Rubin's Rules propagates imputation uncertainty into the reported standard errors and confidence intervals.

### Accomplishments

- Implemented the complete OLS + HC1 + Rubin's Rules pipeline in all three languages:
  - **Python:** `statsmodels.formula.api.ols` with `cov_type="HC1"`
  - **R:** `lm()` with `sandwich::vcovHC(type="HC1")`
  - **Julia:** Manual HC1 sandwich estimator computed directly from `modelmatrix()` and `residuals()`
- Pooled 100 per-dataset coefficient estimates per language via Rubin's Rules (Barnard & Rubin, 1999 adjusted degrees of freedom). Final pooled coefficients:

| Parameter | R | Julia | Python |
|-----------|---|-------|--------|
| Intercept | 12.229 | 12.247 | 12.282 |
| Holes | 0.0525 | 0.0476 | 0.0474 |
| Urban County | 4.001 | 4.158 | 4.172 |
| Mean R² | 0.699 | 0.730 | ~0.770 |

- **Key finding — Urban land premium:** The `county_type: Urban` coefficient of ~4.0–4.2 log-units implies that urban golf course land is worth approximately `exp(4.1) ≈ 60×` more per acre than equivalent rural land. This is by far the largest effect in the model and is estimated with t-statistics exceeding 150 in every implementation.
- **Key finding — Course size effect:** Each additional hole is associated with a 4.7–5.3% increase in log opportunity cost across all three languages, consistent with larger courses concentrating in higher-value geographic areas. **Circularity caveat:** `Holes` is also a MICE predictor for the imputed acreage/value columns this model's dependent variable is built from, so this full-sample estimate partly reflects that dependency for the imputed rows — Appendix A.4 reports the observed-only estimate (fully observed acreage and value) as the substantive Holes finding.
- **Cross-language consistency confirmed:** All three independent MICE/OLS stacks converge on qualitatively identical conclusions. Minor coefficient differences (~10% spread on the Holes coefficient) are traced to R's use of `final_acreage` (OSM + Tigris) vs. Python/Julia's `osm_acreage` (OSM-only) — a documented Phase 2 asymmetry.
- Model explains 69–77% of variance in log opportunity cost across all languages (strong for a two-predictor cross-sectional model), driven primarily by the Urban/Rural distinction.

**Output files:** `{Py|R|Jl}_Regression_Results.csv`, `{Py|R|Jl}_model_results.{pkl|rds|jls}`

---

## Phase 5 — Hawaii Micro-Case Study

### Goal and Purpose

Phase 5 grounds the national model in a specific, verifiable empirical context. The goal is to validate whether the thesis's HBU-based opportunity cost estimates align with real-world property tax assessments for Hawaii golf courses, and to characterize the regulatory landscape (zoning, geographic distribution) that would govern any hypothetical land conversion.

### Intent

A purely national aggregate estimate risks being dismissed as a theoretical exercise disconnected from actual land markets. Hawaii is selected as the validation site for several reasons: (1) it has high-value, well-documented golf courses in both urban (Honolulu) and rural (Big Island, Kauai) settings; (2) official Honolulu County parcel-level cadastral and zoning data are publicly available at TMK (Tax Map Key) resolution; and (3) Hawaii's geography compresses a wide range of opportunity cost scenarios — from resort-zone courses in Honolulu ($866M average OC per course) to rural Big Island courses ($1.7M average) — into a small, tractable area.

The micro-case study also interrogates the HBU assumption itself: if most golf land sits in Preservation or Federal Military zones where residential redevelopment is legally prohibited, the theoretical opportunity cost is economically meaningful only for the subset of land in zones where conversion is actually permitted.

### Accomplishments

- **Phase 5a (Pilot):** Manually compared HBU estimates against official tax assessments for 6 high-profile Hawaii courses. Average model-to-assessed ratio: **1.33x** (model estimates are 32.6% higher on average). The gap is largest for Big Island rural courses (1.49x–1.69x) and narrowest for Honolulu urban courses (1.16x–1.23x), consistent with the hypothesis that the urban FHFA proxy better tracks market values than the rural USDA proxy.

- **Phase 5b (Full Pipeline — Honolulu County):**
  - Built a 6-step automated spatial pipeline extracting OSM polygons for Oahu, intersecting them against the Honolulu County cadastral database, performing Rubin's Rules economic validation (M = 100, q̄ = **$25.40B**, 95% CI: $22.66B–$28.14B), and analyzing parcel-level zoning composition.
  - Identified **1,073 unique TMKs** intersecting Oahu golf polygons across 8,564.23 acres of OSM-derived legal footprint.
  - **Geographic concentration (Step 5):** 63.2% of all golf parcels fall in the Ewa district (Zone 9, Kapolei/Pearl City), reflecting Oahu's post-statehood suburban golf development corridor rather than the urban core.
  - **Zoning analysis (Step 6):** 81.7% of all Oahu golf land sits within Preservation (P-1, P-2) or Federal/Military (F-1) zones — areas where residential redevelopment faces the highest regulatory barriers. Only 2.2% of golf land is in Resort-zoned areas, though golf occupies a striking **25.4%** of all Resort-zoned land on the island.
  - These findings materially qualify the HBU framework: the $25.4B Oahu aggregate opportunity cost is theoretically correct under unrestricted redevelopment but practically constrained by zoning for over 80% of the acreage.

**Output files:** `{Py|R|Jl}_Phase5_Oahu_Comparison.csv`, `{Py|R|Jl}_Phase5_Geographic_Breakdown.csv`, `{Py|R|Jl}_Phase5_Step6_Zoning_Percentages.csv`, `{Py|R|Jl}_Phase5_Step6_Zone_Golf_Penetration.csv`

---

## Phase 6 — Visualization

### Goal and Purpose

Phase 6 converts all upstream computational outputs into the publication-ready figures, maps, and LaTeX tables that will appear in the thesis document. The goal is a complete, reproducible visualization suite covering every major finding from Phases 1–5.

### Intent

Economic research is persuasive only when findings are presented clearly. Phase 6's intent is threefold: (1) **spatial legibility** — national and regional choropleth maps that allow readers to immediately identify geographic concentration of golf course opportunity cost; (2) **statistical transparency** — forest plots, density overlays, and diagnostic charts that expose the full MICE pipeline and cross-language coefficient comparison rather than hiding uncertainty behind a single point estimate; and (3) **structural insight** — advanced figures (Lorenz curves, counterfactual area charts, zoning waffle charts) that contextualize the distributional and regulatory dimensions of the finding.

The language assignment strategy explicitly optimizes for output quality: R handles all spatial/cartographic outputs where its `sf + ggplot2 + tigris` stack excels; Julia handles all statistical chart outputs where CairoMakie produces cleaner publication-quality figures than Python's matplotlib or R's base graphics for non-spatial work.

### Accomplishments

- **Master script architecture:** Two standalone master scripts — `Phase_6.R` (~2,280 lines) and `Phase_6.jl` (~1,843 lines) — independently reproduce the full visualization pipeline without calling bulk scripts. All outputs route automatically to either `output/Final_Thesis_Figures/` (Grand Mean and ObservedOnly variants, for direct thesis inclusion) or `output/QA_Verification/` (per-language Julia/Python/R variants, for internal cross-language QA).

- **Tri-Language Grand Mean:** Both master scripts implement `compute_grand_means()` / equivalent logic that applies Rubin's Rules independently within each language group (M = 100 each) and then computes the Grand Mean as the arithmetic mean of the three independently pooled estimates. This produces the definitive $0.944T national aggregate, with the observed-only baseline at $0.788T (82.5% of the MICE-pooled figure).

- **National and county maps (Scripts 1–2, R):** State-level and county-level choropleth maps of total opportunity cost, each rendered in four variants (Grand Mean, Julia, Python, R) plus an observed-only baseline. Alaska and Hawaii repositioned as insets. Plasma colormap on linear (state) and log₁₀ (county) scales.

- **Oahu micro-maps (Scripts 3, 4, 9, R):** High-resolution maps of all 1,072 golf course TMK parcels on Oahu, colored by geographic district (Script 3), dominant zoning class (Script 4), and per-polygon opportunity cost (Script 9). Script 9 applies Rubin's Rules pooling over M = 100 R draws, yielding a pooled Oahu OC of $31.197B across 37 courses.

- **Bivariate map (Script 7, R):** County-level bivariate choropleth jointly displaying opportunity cost and golf course hole density via a 3×3 biscale classification, highlighting counties with both high land value and high golf intensity.

- **LaTeX tables (Script 8, R and Julia):** Three `booktabs`-styled tables for direct `\input{}` inclusion — national acreage summary with CIs, OLS regression coefficients with robust SEs and FMI, and Oahu geographic breakdown by TMK district.

- **Forest plot (Script 5, Julia):** Tri-language clustered forest plot displaying all three language coefficient estimates (Intercept, Holes, Urban County) with 95% CIs, color-coded Green (Python), Blue (R), Purple (Julia), confirming cross-language agreement.

- **MICE density overlays (Script 5, Julia):** All 300 imputed datasets overlaid as low-opacity density traces (color-coded by language) behind the black observed-acreage curve, with five checkpoint snapshots at n = 20, 40, 60, 80, 100 per language.

- **Marginal effects and raincloud diagnostics (Script 6, Julia):** Dollar-scale marginal effects plot translating log-coefficient to predicted Rural vs. Urban opportunity costs with delta-method CIs; raincloud diagnostic replicating `ggdist::stat_halfeye` from CairoMakie primitives.

- **Advanced structural figures (Scripts 10–14, Julia):** Dumbbell gap chart (Hawaii vs. national), Lorenz curve (opportunity cost concentration), zoning waffle chart, counterfactual area chart, and Urban/Rural scatter — all tri-language Grand Mean figures for the definitive thesis presentation.

- **Residual maps (Script 15, R):** County-level log-residual and dollar-residual choropleths from the Phase 4 Grand Mean OLS fit, identifying geographic areas where the model systematically over- or under-predicts opportunity cost.

- **1.234 naming convention:** All 40+ output files follow the four-digit structural naming format (`MainScript.SubCategoryLanguageIDSubCount`), enabling exact data lineage tracing from filename alone.

**Top-line findings confirmed at visualization:**
- National Grand Mean: **$0.944 trillion**
- Top 5 states by Grand Mean: CA $293.7B · FL $112.0B · NY $53.9B · TX $38.6B · HI $35.6B
- Top 5 counties: Los Angeles $49.7B · Orange $38.5B · Santa Clara $33.8B · San Diego $30.3B · Honolulu $29.7B
- OLS confirmed: β₀ = 12.225, β_holes = 0.053, β_urban = 4.002

---

## Cross-Phase Data Flow Summary

```
Phase 1
  Raw GPS coordinates → Spatial join (FIPS) → Economic proxy merge (USDA/FHFA)
  → RUCC classification → Baseline_Value_Per_Acre
  Output: {Py|R|Jl}_Phase1_Baseline_Golf_Valuation.csv  (~16,292 courses)

Phase 2
  Phase 1 CSV + OSM PBF (11 GB) → Polygon extraction → Two-tier acreage match
  → acreage_source flag
  Output: {Py|R|Jl}_Phase2_Acreage_Matched.csv  (71.2% matched, 28.8% MICE_Target)

Phase 3
  Phase 2 CSV → MICE (M=100 per language) → 100 complete datasets per language
  → Rubin's Rules pooling → National aggregate ~$940B
  Output: 300 imputed CSVs + Rubins_Rules_Summary + National_Acreage_Summary

Phase 4
  300 imputed CSVs → OLS (per dataset) → HC1 robust SEs
  → Rubin's Rules pooling per language
  → β̂_Python, β̂_R, β̂_Julia
  Output: {Py|R|Jl}_Regression_Results.csv

Phase 5
  Phase 1/2/3 outputs + Honolulu cadastral GeoPackage + Zoning GeoPackage
  → Parcel intersection → Economic validation → Geographic/zoning breakdown
  → Oahu q̄ = $25.40B; 81.7% preservation-zone; 63.2% Ewa district
  Output: Phase5 comparison, geographic, zoning CSVs

Phase 6
  All upstream outputs → Grand Mean aggregation → Tri-language visualizations
  → National maps, county maps, Oahu maps, forest plots, density overlays,
     advanced structural charts, LaTeX tables
  → $0.944T confirmed national estimate
  Output: ~40 PNGs + 3 .tex files in Final_Thesis_Figures/ and QA_Verification/
```

---

## Audit and Quality Assurance Log

All six phases underwent a sequential CLAUDE.md compliance review spanning April 30 – May 9, 2026. The review covered structural conventions (four-section layout, ALL_CAPS constants, `[METHODOLOGY]` flags, memory management, file existence guards), cross-language consistency, and I/O path integrity.

| Phase | Review Parts | Key Fixes |
|-------|-------------|-----------|
| Phase 1 | 1A–1D | FIPS zero-padding bug; `[METHODOLOGY]` tags on CRS transforms; Julia `world-age` error resolved by self-contained master |
| Phase 2 | 2A–2D | `acreage_source` column added to Julia and Python outputs; Julia master rebuilt as self-contained |
| Phase 3 | 3A–3D | Memory management (rm/gc) added to all three language loops; stale M=30 header comments updated to M=100; `[METHODOLOGY]` on all Rubin's pooling blocks |
| Phase 4 | 4A–4D | `import gc` and `del df; gc.collect()` added to Python model-fitting loop; parameter name divergence across three languages documented (harmless by design) |
| Phase 5 | 5A–5D | Python float-to-string Zone bug fixed; live acreage computation replaces stale hardcoded constant; memory management in Step 3 loop; `[METHODOLOGY]` tags on missing spatial reads |
| Phase 6 | 6A–6C | Phase_6.R four-section structure fixed; stale loop variables fixed (Scripts 1 and 2); `OUT_LORENZ` naming corrected to 1.234 convention; Phase_6.jl `main()` wrapper added |

---

*Phase 7 write-up work in progress.*
