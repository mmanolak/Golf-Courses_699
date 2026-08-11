# Purpose: Compute Section 5.1's per-class assessed-value figures for parcels
#          intersecting Oahu's golf footprint (Issue_Register.md P5-22), from
#          the author's manually-completed qPublic retrieval worksheets --
#          no assessed-value/exemption field exists anywhere in this
#          repository's committed data (verified, P5-22).
#
# Workflow:
#   1. Phase_5_Assessment_Target_List.R      -> Assessment_Retrieval_Targets.csv
#      (1,072 rows, all classes, no sampling)
#   2. Phase_5_Assessment_Worksheet_Prep.R   -> Assessment_Retrieval_Worksheet.csv
#      (164 rows: 142 P-1/P-2 + 22 F-1, full census -- load-bearing)
#                                              Assessment_Retrieval_Comparison.csv
#      (41 rows: 11 Resort full census + 30-parcel stratified Residential
#      sample -- context only, see that script's header for the sampling method)
#   3. The author retrieves each parcel's assessed LAND value from qPublic and
#      fills in 'assessed_land_value' (numeric, USD) in both files. Leave
#      blank/NA for a parcel with no value shown; write 0 only for a
#      genuine $0 assessment. Use 'notes' to record when a parcel could not
#      be found on qPublic at all (any text containing "not on roll", "not
#      found", "no record", or "absent" is parsed as that case below --
#      exact wording doesn't matter, one of those phrases does).
#   4. This script reads both completed worksheets and computes the figures.
#
# Acreage basis: golf_clipped_acres ONLY. recorded_area_acres (the tax
# roll's cadastral-area field) is blank for 77.8% of this footprint's 1,072
# parcels (Issue_Register.md P5-22 addendum) and cannot serve as a
# denominator for a figure meant to represent the golf footprint -- stated
# here, not silently substituted.
#
# Zoning-class -> manuscript-category mapping (a methodological choice made
# here, not given by the manuscript -- stated explicitly so it can be
# checked or overridden):
#   Preservation = P-1, P-2, F-1 (broad; reported with vs. without F-1 below,
#     since lumping federally tax-exempt parcels in is exactly the concern
#     that split exists to address). Full census, from the Worksheet file.
#   Hotel/Resort = Resort. Full census, from the Comparison file.
#   Residential  = R-3.5, R-5, R-7.5, R-10, R-20, from the Comparison file --
#     a 30-parcel STRATIFIED SAMPLE of a 712-parcel population, not a
#     census; every Residential figure below is a sample estimate, reported
#     as such, not with the same standing as the Preservation/Resort
#     figures. A-1/A-2 (Apartment Districts) are part of this script's
#     broader "Residential" category definition but were not included in
#     the Comparison worksheet's sample -- if present in a future retrieval
#     they will be picked up automatically; absent here, they contribute
#     nothing to this run's Residential figures.
#   Other        = everything else in the footprint (AG-1, AG-2, C, B-1,
#     B-2, IMX-1, I-2). Not sampled at all here (out of scope for this
#     retrieval); reported as a stub with 0 valued parcels if it appears.
#
# Three-way value-status breakdown ($0 vs. blank-unspecified vs.
# not-on-roll), since only $0 (a genuine assessment) belongs in a mean,
# and blank/not-on-roll cannot be distinguished from assessed_land_value
# alone -- not-on-roll is inferred from a keyword scan of the 'notes'
# column (see workflow step 3), a heuristic on free text, not a guaranteed
# clean signal, stated as such in the output.
#
# Reads:  Data/R/Assessment_Retrieval_Worksheet.csv (completed)
#         Data/R/Assessment_Retrieval_Comparison.csv (completed)
# Writes: Data/R/R_Assessment_By_Class.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(this.path)
})

SCRIPT_DIR <- this.path::this.dir()
WORKSHEET_CSV  <- file.path(SCRIPT_DIR, "Data", "R", "Assessment_Retrieval_Worksheet.csv")
COMPARISON_CSV <- file.path(SCRIPT_DIR, "Data", "R", "Assessment_Retrieval_Comparison.csv")
OUT_DIR <- file.path(SCRIPT_DIR, "Data", "R")
OUT_CSV <- file.path(OUT_DIR, "R_Assessment_By_Class.csv")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

for (f in c(WORKSHEET_CSV, COMPARISON_CSV)) {
  if (!file.exists(f)) {
    stop(sprintf("[FATAL] %s not found.\nRun Phase_5_Assessment_Worksheet_Prep.R first.", f))
  }
}

worksheet  <- read_csv(WORKSHEET_CSV,  show_col_types = FALSE)
comparison <- read_csv(COMPARISON_CSV, show_col_types = FALSE) |>
  select(-any_of("sample_type"))
cat(sprintf("[1] Loaded %s: %d parcels (Preservation, full census).\n",
            basename(WORKSHEET_CSV), nrow(worksheet)))
cat(sprintf("    Loaded %s: %d parcels (Resort full census + Residential sample).\n",
            basename(COMPARISON_CSV), nrow(comparison)))

if (!"assessed_land_value" %in% names(worksheet) ||
    all(is.na(worksheet$assessed_land_value))) {
  cat("\n[STOP] 'assessed_land_value' is missing or entirely blank in the Worksheet file.\n")
  cat("Retrieval has not started (or not been saved back to this file). Fill in at least\n")
  cat("one parcel's assessed_land_value before running this script.\n\n")
  result_df <- data.frame(
    Class = "ALL", Parcel_Count = nrow(worksheet) + nrow(comparison),
    Data_Available = FALSE,
    Reason = "assessed_land_value not present/entirely blank -- retrieval not started."
  )
  write_csv(result_df, OUT_CSV)
  cat(sprintf("Saved (stub): %s\n", OUT_CSV))
  quit(save = "no", status = 0)
}

all_parcels <- bind_rows(worksheet, comparison)
n_have_value <- sum(!is.na(all_parcels$assessed_land_value))
cat(sprintf(
  "    assessed_land_value populated for %d of %d retrieved parcels (%.1f%%).\n",
  n_have_value, nrow(all_parcels), n_have_value / nrow(all_parcels) * 100
))

# === Zoning-class -> manuscript-category mapping (see header) ===
preservation_codes <- c("P-1", "P-2", "F-1")
residential_codes  <- c("R-3.5", "R-5", "R-7.5", "R-10", "R-20", "A-1", "A-2")
resort_codes       <- c("Resort")

not_on_roll_pattern <- regex("not on roll|not found|no record|absent", ignore_case = TRUE)

all_parcels <- all_parcels |>
  mutate(
    manuscript_category = case_when(
      dominant_zone_class %in% preservation_codes ~ "Preservation",
      dominant_zone_class %in% residential_codes  ~ "Residential",
      dominant_zone_class %in% resort_codes       ~ "Hotel/Resort",
      TRUE                                        ~ "Other"
    ),
    is_not_on_roll = is.na(assessed_land_value) &
      !is.na(notes) & str_detect(notes, not_on_roll_pattern),
    value_status = case_when(
      is_not_on_roll                       ~ "not_on_roll",
      is.na(assessed_land_value)           ~ "blank_unspecified",
      assessed_land_value == 0             ~ "zero",
      TRUE                                 ~ "valued"
    ),
    per_acre_golf_clipped = if_else(
      value_status == "valued" & !is.na(golf_clipped_acres) & golf_clipped_acres > 0,
      assessed_land_value / golf_clipped_acres, NA_real_
    )
  )

summarise_class <- function(df, label, is_sample = FALSE, population_n = NA_integer_) {
  n <- nrow(df)
  n_zero        <- sum(df$value_status == "zero")
  n_blank       <- sum(df$value_status == "blank_unspecified")
  n_not_on_roll <- sum(df$value_status == "not_on_roll")
  n_valued      <- sum(df$value_status == "valued")
  tot_value    <- sum(df$assessed_land_value[df$value_status %in% c("valued", "zero")], na.rm = TRUE)
  tot_golf_ac  <- sum(df$golf_clipped_acres, na.rm = TRUE)
  n_na_golf_ac <- sum(is.na(df$golf_clipped_acres))
  data.frame(
    Class                                    = label,
    Is_Sample                                = is_sample,
    Population_N                             = population_n,
    Parcel_Count_Retrieved                   = n,
    N_Valued                                 = n_valued,
    N_Zero_Value                             = n_zero,
    N_Blank_Unspecified                      = n_blank,
    N_Not_On_Roll                            = n_not_on_roll,
    Share_Zero_Blank_Or_NotOnRoll            = (n_zero + n_blank + n_not_on_roll) / n,
    Total_Golf_Clipped_Acres                 = tot_golf_ac,
    N_Missing_Golf_Clipped_Acres             = n_na_golf_ac,
    Total_Assessed_Land_Value                = tot_value,
    Mean_Value_Per_Acre_GolfClipped_Weighted   = if (tot_golf_ac > 0) tot_value / tot_golf_ac else NA_real_,
    Mean_Value_Per_Acre_GolfClipped_Unweighted = mean(df$per_acre_golf_clipped, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
}

cat("\n[2] Computing per-category figures (golf-clipped acreage only; see header for why)...\n")

pres_all <- all_parcels |> filter(manuscript_category == "Preservation")
pres_incl_fed <- summarise_class(pres_all, "Preservation (incl. F-1)", FALSE, 164)
pres_excl_fed <- summarise_class(pres_all |> filter(expected_taxable), "Preservation (excl. F-1, taxable-only)", FALSE, 142)
n_f1_in_pres  <- sum(pres_all$dominant_zone_class == "F-1")

resort_rows <- all_parcels |> filter(manuscript_category == "Hotel/Resort")
resort_summary <- summarise_class(resort_rows, "Hotel/Resort", FALSE, 11)

resid_rows <- all_parcels |> filter(manuscript_category == "Residential")
resid_population <- 536 + 82 + 55 + 38 + 1  # R-5+R-3.5+R-7.5+R-10+R-20, from Phase_5_Assessment_Target_List.R's own count
resid_summary <- summarise_class(resid_rows, "Residential (R-3.5/R-5/R-7.5/R-10/R-20 sample)", TRUE, resid_population)

other_rows <- all_parcels |> filter(manuscript_category == "Other")
other_summary <- if (nrow(other_rows) > 0) {
  summarise_class(other_rows, "Other (not retrieved by design)", TRUE, NA_integer_)
} else {
  data.frame(Class = "Other (not retrieved by design)", Is_Sample = TRUE, Population_N = NA_integer_,
             Parcel_Count_Retrieved = 0, N_Valued = 0, N_Zero_Value = 0, N_Blank_Unspecified = 0,
             N_Not_On_Roll = 0, Share_Zero_Blank_Or_NotOnRoll = NA_real_,
             Total_Golf_Clipped_Acres = 0, N_Missing_Golf_Clipped_Acres = 0,
             Total_Assessed_Land_Value = 0, Mean_Value_Per_Acre_GolfClipped_Weighted = NA_real_,
             Mean_Value_Per_Acre_GolfClipped_Unweighted = NA_real_, stringsAsFactors = FALSE)
}

result_df <- bind_rows(pres_incl_fed, pres_excl_fed, resort_summary, resid_summary, other_summary)
write_csv(result_df, OUT_CSV)

cat("\n=== RESULT ===\n")
print(result_df |> select(Class, Is_Sample, Population_N, Parcel_Count_Retrieved,
                           N_Valued, Total_Assessed_Land_Value,
                           Mean_Value_Per_Acre_GolfClipped_Weighted,
                           Mean_Value_Per_Acre_GolfClipped_Unweighted,
                           Share_Zero_Blank_Or_NotOnRoll))

cat(sprintf(
  "\n%d of the 164 Preservation-class (P-1+P-2+F-1) parcels are F-1 (federal/military).\n",
  n_f1_in_pres
))
cat("Preservation reported both including and excluding F-1 -- the manuscript should cite\n")
cat("the excl.-F-1 (taxable-only) row; averaging F-1 in at $0/low-value would manufacture\n")
cat("part of the reported assessment gap rather than reflect a genuine one.\n")
cat("\nResidential is a 30-of-712 stratified sample (Is_Sample = TRUE, Population_N = 712);\n")
cat("Preservation and Hotel/Resort are full censuses (Is_Sample = FALSE) -- do not present\n")
cat("them with the same standing in the manuscript.\n")

cat(sprintf("\nSaved: %s\n", OUT_CSV))
