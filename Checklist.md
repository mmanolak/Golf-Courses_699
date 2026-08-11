# Checklist — Script 16 Extended LaTeX Tables

This checklist tracks the build of `16_LaTeX_Tables_Extended.R` in
`Phase 6 Visualization/Bulk/R/`, which generates five new tri-language
LaTeX tables for thesis and defense use.

---

## Script Build

- [ ] Survey existing `8_LaTeX_Tables.R` structure and output conventions
- [ ] Identify all available data sources (Phases 3–5 CSVs, all three languages)
- [ ] Confirm output naming convention (`16.XYZ_Description.tex`)
- [ ] Write `16_LaTeX_Tables_Extended.R` — full script
- [ ] Run script; confirm 5 `.tex` files appear in `Bulk/R/output/`

---

## Table 16.1 — National Opportunity Cost (Tri-Language)

**Source:** `R/Py/Jl_Rubins_Rules_Summary.csv`
**Output:** `16.141_Table_NationalOC_TriLanguage.tex`

- [ ] Extract pooled OC ($B), SE ($B), 95% CI Lower/Upper for each language
- [ ] Compute Grand Mean row (arithmetic mean of three q_bar values)
- [ ] Verify Grand Mean ≈ $942.7B (Phase 7 report baseline)
- [ ] Write `.tex` file

---

## Table 16.2 — Regression Results (Tri-Language, Wide Format)

**Source:** `R/Py/Jl_Regression_Results.csv`
**Output:** `16.241_Table_Regression_TriLanguage.tex`

- [ ] Normalize parameter names across languages (Intercept, Holes, Urban County)
- [ ] Pivot to wide format: one row per parameter, one column per language
- [ ] Include SE in parentheses and significance stars in each cell
- [ ] Compute Grand Mean β column
- [ ] Include FMI values in footnote
- [ ] Verify β_urban row present for all three languages
- [ ] Write `.tex` file

---

## Table 16.3 — National Acreage Summary (Tri-Language)

**Source:** `R/Py/Jl_National_Acreage_Summary.csv`
**Output:** `16.341_Table_Acreage_TriLanguage.tex`

- [ ] Merge all three language acreage summaries (National/Urban/Rural rows)
- [ ] Add Grand Mean column
- [ ] Format acreage with comma separators
- [ ] Write `.tex` file

---

## Table 16.4 — Oahu Opportunity Cost (Tri-Language)

**Source:** `Phase5_Oahu_Comparison.csv` (R pre-pooled); `Py/Jl_Phase5_Oahu_Comparison.csv` (raw draws)
**Output:** `16.441_Table_OahuOC_TriLanguage.tex`

- [ ] Read R pre-pooled OC directly (q_bar=26.684B, SE=0.962B)
- [ ] Extract Python imputation draws (rows 5–104); apply Rubin's approximation
- [ ] Extract Julia imputation draws (rows 5–104); apply Rubin's approximation
- [ ] Compute Grand Mean row
- [ ] Add "Oahu as % of National OC" column (Oahu / National × 100)
- [ ] Verify Oahu Grand Mean ≈ $26.67B (Phase 7 baseline)
- [ ] Write `.tex` file

---

## Table 16.5 — Oahu Zoning Breakdown (Combined)

**Source:** `Phase5_Step6_Zoning_Percentages.csv` + `Phase5_Step6_Zone_Golf_Penetration.csv`
**Output:** `16.541_Table_OahuZoning_Combined.tex`

- [ ] Left-join zoning CSVs on `zone_class`
- [ ] Columns: Zone | Description | Golf Acres | % of Golf Total | Golf as % of Zone
- [ ] Sort by Golf Acres descending
- [ ] Format acres with 2 decimal places, percentages with 2 decimal places
- [ ] Write `.tex` file

---

## Verification

- [ ] All 5 `.tex` files present in `Bulk/R/output/`
- [ ] Grand Mean National OC ≈ $942.7B (Table 16.1)
- [ ] β_urban in all three language columns (Table 16.2)
- [ ] Oahu Grand Mean ≈ $26.67B (Table 16.4)
- [ ] Zoning table has 19 rows (Table 16.5)

---

## Summary Tracker

| Table | Description                             | Status     |
| ----- | --------------------------------------- | ---------- |
| 16.1  | National OC — Tri-Language             | ☐ PENDING |
| 16.2  | Regression Results — Tri-Language Wide | ☐ PENDING |
| 16.3  | National Acreage — Tri-Language        | ☐ PENDING |
| 16.4  | Oahu OC — Tri-Language                 | ☐ PENDING |
| 16.5  | Oahu Zoning — Combined                 | ☐ PENDING |
