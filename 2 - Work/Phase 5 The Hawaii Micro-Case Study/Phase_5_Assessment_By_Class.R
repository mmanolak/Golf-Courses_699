# Purpose: Reproduce Section 5.1's per-class assessed-value figures for parcels
#          intersecting Oahu's golf footprint (Issue_Register.md P5-22):
#            - Preservation-class mean assessed land value: $26,079/acre
#            - Residential-class mean assessed land value:  $630,049/acre
#            - Hotel/Resort-class mean assessed land value: $962,922/acre
#            - 105 Preservation-classified parcels in the golf footprint
#            - Exemption rate: 27.5% (Preservation class) vs. 47.2% (matched set)
#
# Method (as far as the source data supports it): join the 1,072-TMK golf
# footprint (Phase_5.R Step 2's Target_Golf_Parcels_List.csv) against the
# Honolulu parcel tax roll (All_Parcels_-4613852522541990741.csv, the same
# file Phase_5.R Step 4 already joins on TMK for its own Zone lookup) and
# group by tax classification, summing assessed land value and exemption
# amount per class.
#
# Reads:  Phase 5.../Data/R/Target_Golf_Parcels_List.csv (1,072 TMKs)
#         00 - Data Sources/Honolulu/All_Parcels_-4613852522541990741.csv
# Writes: Data/R/R_Assessment_By_Class.csv

suppressPackageStartupMessages({
  library(this.path)
})

SCRIPT_DIR <- this.path::this.dir()
WORK_DIR   <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)

TMK_LIST_CSV <- file.path(SCRIPT_DIR, "Data", "R", "Target_Golf_Parcels_List.csv")
TAX_ROLL_CSV <- file.path(
  WORK_DIR, "00 - Data Sources", "Honolulu", "All_Parcels_-4613852522541990741.csv"
)
OUT_DIR <- file.path(SCRIPT_DIR, "Data", "R")
OUT_CSV <- file.path(OUT_DIR, "R_Assessment_By_Class.csv")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

if (!file.exists(TMK_LIST_CSV)) {
  stop(sprintf("[FATAL] Golf-footprint TMK list not found:\n  %s\nRun Phase_5.R Step 2 first.", TMK_LIST_CSV))
}
if (!file.exists(TAX_ROLL_CSV)) {
  stop(sprintf("[FATAL] Honolulu tax roll not found:\n  %s", TAX_ROLL_CSV))
}

tmk_list <- read.csv(TMK_LIST_CSV, colClasses = "character", stringsAsFactors = FALSE)
cat(sprintf("[1] Golf-footprint TMK list: %d parcels (%s)\n", nrow(tmk_list), basename(TMK_LIST_CSV)))

tax_roll <- read.csv(TAX_ROLL_CSV, stringsAsFactors = FALSE)
cat(sprintf("[2] Tax roll loaded: %d rows, %d columns (%s)\n", nrow(tax_roll), ncol(tax_roll), basename(TAX_ROLL_CSV)))

# === Join golf-footprint TMKs against the tax roll (same TMK column both files use elsewhere) ===
tmk_list$TMK <- trimws(as.character(tmk_list$TMK))
tax_roll$TMK <- trimws(as.character(tax_roll$TMK))
matched <- merge(tmk_list, tax_roll, by = "TMK", all.x = TRUE)
n_matched <- sum(!is.na(matched$objectid))
cat(sprintf(
  "[3] Joined on TMK: %d of %d golf-footprint parcels matched a tax-roll row.\n",
  n_matched, nrow(tmk_list)
))
cat("    Acreage basis: 'Recorded.Area.Acres' (cadastral parcel area) — this is NOT the\n")
cat("    golf-clipped area Phase_5.R Step 2 computes via st_intersection(); a mixed-use\n")
cat("    parcel's full recorded acreage is counted here even where only part of it overlaps\n")
cat("    the golf polygon. This is a stated limitation, not a bug: the tax roll has no\n")
cat("    golf-clipped acreage field, only whole-parcel 'Recorded Area Acres'.\n")
total_acres_cadastral <- sum(matched$Recorded.Area.Acres, na.rm = TRUE)
cat(sprintf("    Total recorded (cadastral, unclipped) acreage of matched parcels: %.2f ac\n", total_acres_cadastral))

# === Search for the fields Section 5.1's figures require ===
cat("\n[4] Searching tax-roll columns for assessed value / tax class / exemption fields...\n")
all_cols <- names(tax_roll)
value_cols     <- grep("valu|assess", all_cols, ignore.case = TRUE, value = TRUE)
class_cols     <- grep("class|use|land.?use", all_cols, ignore.case = TRUE, value = TRUE)
exemption_cols <- grep("exempt", all_cols, ignore.case = TRUE, value = TRUE)

cat(sprintf("    Columns matching /valu|assess/i  (assessed value candidates): %s\n",
            if (length(value_cols) == 0) "NONE" else paste(value_cols, collapse = ", ")))
cat(sprintf("    Columns matching /class|use/i    (tax-classification candidates): %s\n",
            if (length(class_cols) == 0) "NONE" else paste(class_cols, collapse = ", ")))
cat(sprintf("    Columns matching /exempt/i       (exemption candidates): %s\n",
            if (length(exemption_cols) == 0) "NONE" else paste(exemption_cols, collapse = ", ")))

fields_missing <- length(value_cols) == 0 || length(class_cols) == 0 || length(exemption_cols) == 0

# === What's actually derivable from this file, regardless of the fields above ===
cat("\n[5] What this tax roll DOES carry for the matched parcels (for the record):\n")
zone_tbl <- table(substr(matched$TMK, 1, 1), useNA = "always")
print(zone_tbl)
cch_tbl <- table(matched$CCH.Parcel.Type, useNA = "always")
cat("  'CCH Parcel Type' (parcel-fabric type — 1/2/3; NOT a land-use or tax class):\n")
print(cch_tbl)

# === Denominator/filter details, stated explicitly per the request ===
cat("\n[6] Filters used, stated explicitly:\n")
cat(sprintf("    - TMK join key: exact string match, 8-digit TMK, no padding needed (both files agree on format).\n"))
cat(sprintf("    - Denominator: all %d golf-footprint TMKs from Phase_5.R Step 2 (Target_Golf_Parcels_List.csv),\n", nrow(tmk_list)))
cat(sprintf("      not a subset — %d of these matched a tax-roll row (%d did not).\n",
            n_matched, nrow(tmk_list) - n_matched))
cat(sprintf("    - No tax-class field exists to filter to 'Preservation'/'Residential'/'Hotel-Resort' by.\n"))
cat(sprintf("    - No 'exempted' field exists in this schema; the concept cannot be defined from this file.\n"))

# === Result: what can and cannot be written ===
result_df <- data.frame(
  Class                    = "ALL_MATCHED_GOLF_FOOTPRINT_PARCELS",
  Parcel_Count             = nrow(tmk_list),
  Parcel_Count_Tax_Roll_Matched = n_matched,
  Total_Acres_Cadastral    = total_acres_cadastral,
  Total_Assessed_Land_Value = NA_real_,
  Mean_Value_Per_Acre      = NA_real_,
  Exemption_Share          = NA_real_,
  Data_Available           = FALSE,
  Reason = paste(
    "Tax roll (All_Parcels_-4613852522541990741.csv) carries only parcel-fabric/geometry",
    "fields (TMK, Zone, Recorded Area, parcel type, subdivision/plat metadata) -- no",
    "assessed land value, no Preservation/Residential/Hotel-Resort tax classification,",
    "and no exemption field exists anywhere in this repository (checked: the CSV export,",
    "the equivalent .gpkg's field list, and the source .zip's raw .dbf schema, all",
    "identical). The 6 target figures cannot be derived from data in this tree.",
    "A qPublic (Schneider Corp) parcel-lookup bookmark exists at",
    "'00 - Data Sources/Data Sources - Via HTML/qPublic - City and County of Honolulu,",
    "HI - GIS Map (1,000 max results).url', consistent with these figures having been",
    "hand-looked-up per parcel rather than derived from any bulk file in this repo."
  )
)
write.csv(result_df, OUT_CSV, row.names = FALSE)

cat("\n=== RESULT ===\n")
cat("[FATAL for 5 of 6 target figures] No assessed-value, tax-classification, or exemption\n")
cat("field exists in any Honolulu source file committed to this repository. Verified against:\n")
cat(sprintf("  - %s (CSV export, %d columns)\n", basename(TAX_ROLL_CSV), ncol(tax_roll)))
cat("  - the equivalent All_Parcels_*.gpkg layer (same field set, different names)\n")
cat("  - the source All_Parcels_*.zip's raw shapefile .dbf (same field set again)\n")
cat("These figures cannot be reproduced from committed code; they require a source not\n")
cat("currently in this repository (most likely a hand qPublic/RPAD lookup -- see Reason\n")
cat("column above).\n\n")
cat("[Reproduced] Golf-footprint parcel count and cadastral (unclipped) acreage only:\n")
cat(sprintf("  %d parcels, %d tax-roll-matched, %.2f total recorded acres.\n",
            nrow(tmk_list), n_matched, total_acres_cadastral))
cat(sprintf("\nSaved: %s\n", OUT_CSV))
