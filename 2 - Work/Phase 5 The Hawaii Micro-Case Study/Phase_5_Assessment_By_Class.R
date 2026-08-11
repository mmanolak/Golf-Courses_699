# Purpose: Compute Section 5.1's per-class assessed-value figures for parcels
#          intersecting Oahu's golf footprint (Issue_Register.md P5-22), from
#          a manually-completed retrieval sheet, not from anything scripted
#          end-to-end -- no assessed-value/exemption field exists anywhere in
#          this repository's committed data (verified, P5-22).
#
# Workflow:
#   1. Phase_5_Assessment_Target_List.R emits Data/R/Assessment_Retrieval_Targets.csv
#      (1,072 rows: TMK, dominant_zone_class, golf_clipped_acres,
#      recorded_area_acres, course_name, expected_taxable, ...).
#   2. The author retrieves each parcel's assessed LAND value from qPublic
#      (see the bookmark under '00 - Data Sources/Data Sources - Via HTML/')
#      and adds it to that same file as a new column named exactly
#      'assessed_land_value' (numeric, USD, blank/NA if the parcel is absent
#      from qPublic or shows no value -- do not write 0 for "not found",
#      only for a genuine $0 assessment; the two are counted separately below).
#   3. This script reads that completed file and computes the figures.
#
# Zoning-class -> manuscript-category mapping (a methodological choice made
# here, not given by the manuscript -- stated explicitly so it can be
# checked or overridden):
#   Preservation = P-1, P-2, F-1 (broad, matches how the manuscript's
#     original "Preservation" figure appears to have been scoped -- see
#     the with/without-federal split below, which exists precisely because
#     lumping F-1 in here is contested).
#   Residential  = R-3.5, R-5, R-7.5, R-10, R-20 (Residential Districts) and
#     A-1, A-2 (Apartment Districts) -- grouped together as "Residential"
#     for this 3-category exercise; Apartment districts are a distinct
#     zoning family from Residential Districts in Honolulu's own code and
#     this grouping is a simplification, flagged as such.
#   Hotel/Resort = Resort (the only zoning code of that kind that appears in
#     the golf footprint; there is no separate "Hotel" zoning code here).
#   Other        = everything else present in the footprint (AG-1, AG-2,
#     C, B-1, B-2, IMX-1, I-2 -- Agriculture/Country/Business/Industrial).
#     Not one of the manuscript's three cited categories; reported for
#     completeness, not compared against a manuscript figure.
#
# Acreage basis: BOTH golf_clipped_acres and recorded_area_acres are used,
# every figure computed against each, because they differ by roughly 12x
# across this footprint (Issue_Register.md P5-22) and the manuscript's
# original figures never stated which basis they used. Neither is called
# "the" answer here.
#
# Reads:  Data/R/Assessment_Retrieval_Targets.csv (must have assessed_land_value added)
# Writes: Data/R/R_Assessment_By_Class.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(this.path)
})

SCRIPT_DIR <- this.path::this.dir()
TARGETS_CSV <- file.path(SCRIPT_DIR, "Data", "R", "Assessment_Retrieval_Targets.csv")
OUT_DIR <- file.path(SCRIPT_DIR, "Data", "R")
OUT_CSV <- file.path(OUT_DIR, "R_Assessment_By_Class.csv")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(TARGETS_CSV)) {
  stop(sprintf(
    "[FATAL] %s not found.\nRun Phase_5_Assessment_Target_List.R first.",
    TARGETS_CSV
  ))
}

targets <- read_csv(TARGETS_CSV, show_col_types = FALSE)
cat(sprintf("[1] Loaded %s: %d parcels.\n", basename(TARGETS_CSV), nrow(targets)))

if (!"assessed_land_value" %in% names(targets)) {
  cat("\n[STOP] No 'assessed_land_value' column found in the retrieval sheet.\n")
  cat("This file is still the un-retrieved target list, not a completed retrieval.\n")
  cat("Add a numeric 'assessed_land_value' column (USD, blank/NA where a parcel has no\n")
  cat("qPublic record) before running this script. See this script's header for the\n")
  cat("expected retrieval workflow.\n\n")
  result_df <- data.frame(
    Class = "ALL", Parcel_Count = nrow(targets), Data_Available = FALSE,
    Reason = "assessed_land_value column not present -- retrieval not yet completed."
  )
  write_csv(result_df, OUT_CSV)
  cat(sprintf("Saved (stub): %s\n", OUT_CSV))
  quit(save = "no", status = 0)
}

n_have_value <- sum(!is.na(targets$assessed_land_value))
cat(sprintf(
  "    assessed_land_value populated for %d of %d parcels (%.1f%%).\n",
  n_have_value, nrow(targets), n_have_value / nrow(targets) * 100
))

# === Zoning-class -> manuscript-category mapping (see header) ===
preservation_codes <- c("P-1", "P-2", "F-1")
residential_codes  <- c("R-3.5", "R-5", "R-7.5", "R-10", "R-20", "A-1", "A-2")
resort_codes       <- c("Resort")

targets <- targets |>
  mutate(
    manuscript_category = case_when(
      dominant_zone_class %in% preservation_codes ~ "Preservation",
      dominant_zone_class %in% residential_codes  ~ "Residential",
      dominant_zone_class %in% resort_codes       ~ "Hotel/Resort",
      TRUE                                        ~ "Other"
    ),
    value_status = case_when(
      is.na(assessed_land_value)        ~ "missing",
      assessed_land_value == 0          ~ "zero",
      TRUE                              ~ "valued"
    ),
    per_acre_golf_clipped = if_else(
      value_status == "valued" & !is.na(golf_clipped_acres) & golf_clipped_acres > 0,
      assessed_land_value / golf_clipped_acres, NA_real_
    ),
    per_acre_recorded = if_else(
      value_status == "valued" & !is.na(recorded_area_acres) & recorded_area_acres > 0,
      assessed_land_value / recorded_area_acres, NA_real_
    )
  )

summarise_class <- function(df, label) {
  n <- nrow(df)
  n_missing <- sum(df$value_status == "missing")
  n_zero    <- sum(df$value_status == "zero")
  n_valued  <- sum(df$value_status == "valued")
  tot_value <- sum(df$assessed_land_value, na.rm = TRUE)
  tot_golf_ac  <- sum(df$golf_clipped_acres, na.rm = TRUE)
  tot_rec_ac   <- sum(df$recorded_area_acres, na.rm = TRUE)
  n_na_golf_ac <- sum(is.na(df$golf_clipped_acres))
  n_na_rec_ac  <- sum(is.na(df$recorded_area_acres))
  data.frame(
    Class                              = label,
    Parcel_Count                       = n,
    N_Missing_Value                    = n_missing,
    N_Zero_Value                       = n_zero,
    N_Zero_Or_Missing                  = n_missing + n_zero,
    Share_Zero_Or_Missing              = (n_missing + n_zero) / n,
    N_Valued                           = n_valued,
    Total_Golf_Clipped_Acres           = tot_golf_ac,
    N_Missing_Golf_Clipped_Acres       = n_na_golf_ac,
    Total_Recorded_Acres               = tot_rec_ac,
    N_Missing_Recorded_Acres           = n_na_rec_ac,
    Total_Assessed_Land_Value          = tot_value,
    Mean_Value_Per_Acre_GolfClipped_Weighted   = if (tot_golf_ac > 0) tot_value / tot_golf_ac else NA_real_,
    Mean_Value_Per_Acre_Recorded_Weighted      = if (tot_rec_ac > 0) tot_value / tot_rec_ac else NA_real_,
    Mean_Value_Per_Acre_GolfClipped_Unweighted = mean(df$per_acre_golf_clipped, na.rm = TRUE),
    Mean_Value_Per_Acre_Recorded_Unweighted    = mean(df$per_acre_recorded, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

cat("\n[2] Computing per-category figures...\n")
categories <- c("Preservation", "Residential", "Hotel/Resort", "Other")
rows <- lapply(categories, function(cat_name) {
  summarise_class(targets |> filter(manuscript_category == cat_name), cat_name)
})

# Preservation, with vs. without federal parcels (the requested split)
pres_all <- targets |> filter(manuscript_category == "Preservation")
pres_incl_fed <- summarise_class(pres_all, "Preservation (incl. F-1 federal/military)")
pres_excl_fed <- summarise_class(pres_all |> filter(expected_taxable), "Preservation (excl. F-1 federal/military)")
n_f1_in_pres <- sum(pres_all$dominant_zone_class == "F-1")

result_df <- bind_rows(rows, list(pres_incl_fed, pres_excl_fed))
write_csv(result_df, OUT_CSV)

cat("\n=== RESULT (per manuscript category) ===\n")
print(result_df |> select(Class, Parcel_Count, Total_Assessed_Land_Value,
                           Mean_Value_Per_Acre_GolfClipped_Weighted,
                           Mean_Value_Per_Acre_Recorded_Weighted,
                           Share_Zero_Or_Missing))

cat(sprintf(
  "\n%d of the %d Preservation-class (P-1+P-2+F-1) parcels are F-1 (federal/military, expected untaxed).\n",
  n_f1_in_pres, nrow(pres_all)
))
cat("Preservation figures reported both including and excluding F-1 above -- the manuscript\n")
cat("should cite the excl.-F-1 (taxable-only) row, per the concern that averaging in\n")
cat("constitutionally tax-exempt federal parcels as $0/low-value would manufacture part of\n")
cat("the reported assessment gap rather than reflect a genuine one.\n")

cat(sprintf("\nSaved: %s\n", OUT_CSV))
