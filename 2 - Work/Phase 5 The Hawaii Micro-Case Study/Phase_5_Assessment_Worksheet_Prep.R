# Purpose: Prepare the two manual-retrieval worksheets for Issue_Register.md
#          P5-22's assessment-gap reproduction -- one full-census worksheet
#          for the load-bearing Preservation figures, one documented-subset
#          worksheet for the context-only Residential/Hotel-Resort figures.
#
# Reads:  Data/R/Assessment_Retrieval_Targets.csv (1,072 rows, all classes,
#         from Phase_5_Assessment_Target_List.R)
# Writes: Data/R/Assessment_Retrieval_Worksheet.csv   (164 rows: 142 P-1/P-2
#           + 22 F-1, full census -- these are load-bearing, not sampled)
#         Data/R/Assessment_Retrieval_Comparison.csv  (41 rows: all 11 Resort
#           + a 30-parcel stratified sample of R-3.5/R-5/R-7.5/R-10/R-20 --
#           context only, sampling method below and in the file's own header)
#
# Sampling method (Comparison worksheet only; the Worksheet above is a full
# census, not sampled): stratified by dominant_zone_class among the
# Residential-District codes R-3.5/R-5/R-7.5/R-10/R-20 (NOT the Apartment
# codes A-1/A-2 -- this comparison worksheet is scoped narrower than
# Phase_5_Assessment_By_Class.R's "Residential" manuscript-category, which
# does include A-1/A-2; noted so the two aren't conflated). Allocation:
# proportional to each stratum's population among these five codes, with a
# floor of 1 parcel per non-empty stratum so R-20 (population 1) is still
# represented, remainder distributed by largest population share, total
# capped at 30. Within each stratum, parcels are drawn uniformly at random,
# seed = 5122 (fixed for reproducibility; re-running this script draws the
# identical sample). All 11 Resort parcels are included -- population small
# enough that a full census costs nothing extra.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(this.path)
})

SCRIPT_DIR <- this.path::this.dir()
TARGETS_CSV <- file.path(SCRIPT_DIR, "Data", "R", "Assessment_Retrieval_Targets.csv")
OUT_DIR <- file.path(SCRIPT_DIR, "Data", "R")
OUT_WORKSHEET  <- file.path(OUT_DIR, "Assessment_Retrieval_Worksheet.csv")
OUT_COMPARISON <- file.path(OUT_DIR, "Assessment_Retrieval_Comparison.csv")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(TARGETS_CSV)) {
  stop(sprintf(
    "[FATAL] %s not found.\nRun Phase_5_Assessment_Target_List.R first.", TARGETS_CSV
  ))
}

targets <- read_csv(TARGETS_CSV, show_col_types = FALSE)
cat(sprintf("[1] Loaded %s: %d parcels.\n", basename(TARGETS_CSV), nrow(targets)))

n_dup <- sum(duplicated(targets$TMK))
cat(sprintf("    Duplicate-TMK check: %d duplicates (should be 0).\n", n_dup))
if (n_dup > 0) stop("[FATAL] Duplicate TMKs found in the target list -- fix before building worksheets.")

blank_cols <- c(
  "assessed_land_value", "assessed_building_value", "tax_class",
  "exempt_value", "retrieval_date", "notes"
)

# === Worksheet: full census, 142 P-1/P-2 + 22 F-1, no sampling ===
cat("\n[2] Building the Preservation retrieval worksheet (full census, not sampled)...\n")
worksheet <- targets |>
  filter(dominant_zone_class %in% c("P-1", "P-2", "F-1")) |>
  arrange(course_name, TMK) |>
  select(TMK, dominant_zone_class, course_name, golf_clipped_acres, expected_taxable) |>
  mutate(
    assessed_land_value     = NA_real_,
    assessed_building_value = NA_real_,
    tax_class                = NA_character_,
    exempt_value             = NA_real_,
    retrieval_date            = NA_character_,
    notes                     = NA_character_
  )
write_csv(worksheet, OUT_WORKSHEET)
cat(sprintf("    Saved: %s (%d rows: %d P-1/P-2 + %d F-1)\n",
            basename(OUT_WORKSHEET),
            nrow(worksheet),
            sum(worksheet$dominant_zone_class %in% c("P-1", "P-2")),
            sum(worksheet$dominant_zone_class == "F-1")))

cat("\n[3] Distribution across courses (142 P-1/P-2 parcels, before F-1):\n")
print(
  worksheet |>
    filter(dominant_zone_class %in% c("P-1", "P-2")) |>
    count(course_name, sort = TRUE) |>
    as.data.frame()
)
cat("\n    Distribution across courses (22 F-1 parcels):\n")
print(
  worksheet |>
    filter(dominant_zone_class == "F-1") |>
    count(course_name, sort = TRUE) |>
    as.data.frame()
)

# === Comparison: documented subset, Resort full census + stratified Residential sample ===
cat("\n[4] Building the Residential/Hotel-Resort comparison worksheet (documented subset)...\n")

resort_rows <- targets |> filter(dominant_zone_class == "Resort")
cat(sprintf("    Resort: %d parcels, full census (no sampling).\n", nrow(resort_rows)))

resid_codes <- c("R-3.5", "R-5", "R-7.5", "R-10", "R-20")
resid_pool  <- targets |> filter(dominant_zone_class %in% resid_codes)
strata_pop  <- resid_pool |> count(dominant_zone_class, name = "population")
cat("    Residential strata population (R-* only, A-1/A-2 excluded from this comparison):\n")
print(as.data.frame(strata_pop))

CAP <- 30
# Floor of 1 per non-empty stratum, remainder allocated proportionally to
# population, largest-remainder rounding so the total lands exactly on CAP.
strata_pop <- strata_pop |>
  mutate(
    floor_alloc = 1L,
    remaining_pop = population - floor_alloc
  )
remaining_budget <- CAP - sum(strata_pop$floor_alloc)
strata_pop <- strata_pop |>
  mutate(
    raw_share = remaining_pop / sum(remaining_pop) * remaining_budget,
    add_alloc = floor(raw_share),
    remainder = raw_share - add_alloc
  )
short_by <- remaining_budget - sum(strata_pop$add_alloc)
if (short_by > 0) {
  bump_idx <- order(-strata_pop$remainder)[seq_len(short_by)]
  strata_pop$add_alloc[bump_idx] <- strata_pop$add_alloc[bump_idx] + 1L
}
strata_pop <- strata_pop |>
  mutate(sample_n = pmin(floor_alloc + add_alloc, population))

cat("    Allocation (proportional, floor 1, largest-remainder to reach cap):\n")
print(as.data.frame(strata_pop |> select(dominant_zone_class, population, sample_n)))
cat(sprintf("    Total sampled: %d (target cap: %d)\n", sum(strata_pop$sample_n), CAP))

set.seed(5122)
resid_sample <- bind_rows(lapply(seq_len(nrow(strata_pop)), function(i) {
  zc <- strata_pop$dominant_zone_class[i]
  n  <- strata_pop$sample_n[i]
  pool_z <- resid_pool |> filter(dominant_zone_class == zc)
  pool_z[sample(nrow(pool_z), n), ]
}))

comparison <- bind_rows(resort_rows, resid_sample) |>
  arrange(dominant_zone_class, course_name, TMK) |>
  select(TMK, dominant_zone_class, course_name, golf_clipped_acres, expected_taxable) |>
  mutate(
    assessed_land_value     = NA_real_,
    assessed_building_value = NA_real_,
    tax_class                = NA_character_,
    exempt_value             = NA_real_,
    retrieval_date            = NA_character_,
    notes                     = NA_character_,
    sample_type               = if_else(
      dominant_zone_class == "Resort", "full_census", "stratified_sample_seed5122"
    )
  )

write_csv(comparison, OUT_COMPARISON)
cat(sprintf("\n    Saved: %s (%d rows: %d Resort + %d Residential sample)\n",
            basename(OUT_COMPARISON), nrow(comparison), nrow(resort_rows), nrow(resid_sample)))

cat("\n=== Sampling method (also recorded in this script's own header, and in the\n")
cat("    'sample_type' column of Assessment_Retrieval_Comparison.csv itself) ===\n")
cat("  Resort: full census, all 11 parcels, no sampling.\n")
cat("  Residential (R-3.5/R-5/R-7.5/R-10/R-20 only, A-1/A-2 excluded): stratified random\n")
cat("  sample, n=30, allocated proportionally to stratum population with a floor of 1 per\n")
cat("  stratum and largest-remainder rounding to hit the cap exactly; seed=5122.\n")
