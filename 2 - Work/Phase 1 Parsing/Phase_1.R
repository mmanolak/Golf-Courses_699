# Purpose: Master pipeline - parse, spatial-join, proxy-merge, and classify
#          baseline land values for all US golf courses (Phase 1).
# Inputs:  00 - Data Sources/Original Data/Golf Courses-USA.csv
#          00 - Data Sources/Original Data/2022 - USDA County Data - Ag Use.csv
#          00 - Data Sources/Original Data/2024 - FHFA June 20 Land Prices.xlsx
#          00 - Data Sources/Original Data/tl_2022_us_county.shp (vendored 2026-07-27,
#            source https://www2.census.gov/geo/tiger/TIGER2022/COUNTY/tl_2022_us_county.zip)
#          00 - Data Sources/Secondary/2023-rural-urban-continuum-codes.csv (vendored
#            2026-07-27, source https://www.ers.usda.gov/media/5768/2023-rural-urban-continuum-codes.csv)
# Outputs: Phase 1 Parsing/Data/R/R_Phase1_Parsed_Golf_Courses.csv
#          Phase 1 Parsing/Data/R/R_Phase1_Spatial_Joined_Golf_Courses.csv
#          Phase 1 Parsing/Data/R/R_Phase1_Valuation_Joined_Golf_Courses.csv
#          Phase 1 Parsing/Data/R/R_Phase1_Baseline_Golf_Valuation.csv


# === 1. LIBRARIES ===

# X-08/Decision 1 (2026-07-27): activate the pinned renv project library before
# loading any packages, so this script runs against the versions in renv.lock,
# not whatever happens to be in this machine's personal R library.
local({
  cmd_args <- commandArgs(trailingOnly = FALSE)
  m <- grep("^--file=", cmd_args)
  if (length(m) == 0) return(invisible(NULL))
  script_path <- normalizePath(sub("^--file=", "", cmd_args[m]))
  proj_dir <- dirname(dirname(script_path))
  activate_r <- file.path(proj_dir, "renv", "activate.R")
  if (file.exists(activate_r)) {
    Sys.setenv(RENV_PROJECT = proj_dir)
    source(activate_r)
  }
})

suppressPackageStartupMessages({
  library(wooldridge)
  library(tidyverse)
  library(readxl)
  library(sf)
  library(tigris)
  library(future)
  library(furrr)
  library(parallelly)
  library(this.path)
})


# === 2. GLOBALS & PATHS ===

options(tigris_use_cache = TRUE)

SCRIPT_DIR   <- this.path::this.dir()
ROOT_DIR     <- file.path(SCRIPT_DIR, "..")
DATA_DIR     <- file.path(ROOT_DIR, "00 - Data Sources", "Original Data")
OUTPUT_DIR   <- file.path(SCRIPT_DIR, "Data", "R")

SAFE_WORKERS <- min(availableCores() - 6, 20)
plan(multisession, workers = SAFE_WORKERS)

if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

RAW_CSV      <- file.path(DATA_DIR, "Golf Courses-USA.csv")
USDA_IN      <- file.path(DATA_DIR, "2022 - USDA County Data - Ag Use.csv")
FHFA_IN      <- file.path(DATA_DIR, "2024 - FHFA June 20 Land Prices.xlsx")
# Vendored 2026-07-27 (X-08/Gate-3 policy: no master script performs a network fetch at
# run time). Source: https://www.ers.usda.gov/media/5768/2023-rural-urban-continuum-codes.csv
RUCC_CSV     <- file.path(ROOT_DIR, "00 - Data Sources", "Secondary", "2023-rural-urban-continuum-codes.csv")
# Vendored 2026-07-27. Source: https://www2.census.gov/geo/tiger/TIGER2022/COUNTY/tl_2022_us_county.zip
COUNTY_SHP   <- file.path(DATA_DIR, "tl_2022_us_county.shp")

OUT_PARSED   <- file.path(OUTPUT_DIR, "R_Phase1_Parsed_Golf_Courses.csv")
OUT_SPATIAL  <- file.path(OUTPUT_DIR, "R_Phase1_Spatial_Joined_Golf_Courses.csv")
OUT_VAL_JOIN <- file.path(OUTPUT_DIR, "R_Phase1_Valuation_Joined_Golf_Courses.csv")
OUT_BASELINE <- file.path(OUTPUT_DIR, "R_Phase1_Baseline_Golf_Valuation.csv")


# === 3. FUNCTIONS ===

extract_ownership <- function(details) {
  # [METHODOLOGY] first-parenthetical extraction, searching anywhere in the string (not
  # anchored to position 0) so a "CLOSED|" prefix doesn't defeat the match (P1-05)
  m <- str_extract(details, "\\([^)]+\\)")
  if (is.na(m)) return(NA_character_)
  str_remove_all(m, "[()]")
}

extract_holes <- function(details) {
  # [METHODOLOGY] strict "(N Holes)" pattern first
  strict <- str_extract(details, "(?<=\\()\\d+(?= Holes\\))")
  if (!is.na(strict)) return(as.numeric(strict))
  # [METHODOLOGY] combo courses, e.g. "(18 Holes & 9 Holes)" -> sum the two nines
  combo <- str_match(details, "\\((\\d+) Holes & (\\d+) Holes\\)")
  if (!is.na(combo[1, 1])) return(as.numeric(combo[1, 2]) + as.numeric(combo[1, 3]))
  # [METHODOLOGY] bare "(N)" with no "Holes" text, restricted to the prefix before the
  # first comma (the Ownership/Holes segment) so a phone area code or zip can't match
  prefix <- str_split_fixed(details, ",", 2)[, 1]
  bare <- str_extract(prefix, "(?<=\\()\\d+(?=\\))")
  if (!is.na(bare)) return(as.numeric(bare))
  return(NA_real_)
}


# === 4. EXECUTION ===

for (path in c(RAW_CSV, USDA_IN, FHFA_IN)) {
  if (!file.exists(path)) stop(paste("Input file not found:", path))
}

# PART A: Parse & Deduplicate
cat(" 1  Loading and Parsing raw Golf Courses CSV\n")
courses_df <- read_csv(
  RAW_CSV,
  col_names = c("Longitude", "Latitude", "Name_State", "Details"),
  show_col_types = FALSE
)
original_n <- nrow(courses_df)

courses_df <- courses_df |>
  mutate(
    Course_Name    = str_remove(Name_State, "-.*$"),
    State_Abbr     = str_extract(Name_State, "[A-Z]{2}$"),
    Ownership_Type = map_chr(Details, extract_ownership),
    Holes          = map_dbl(Details, extract_holes),
    Zip_Code       = str_extract(Details, paste0("(?<=", State_Abbr, " )\\d{5}")),
    City           = str_remove(str_remove(str_extract(Details, paste0(",([^,]+),", State_Abbr)), "^,\\s*"), paste0(",", State_Abbr)),
    Address        = str_remove(str_remove(str_extract(Details, "\\), (.*?),(?=\\s*[^,]+,[A-Z]{2})"), "^\\), "), ",$")
  ) |>
  select(Longitude, Latitude, Course_Name, State_Abbr, Ownership_Type, Holes, Address, City, Zip_Code)

cat(" 2  Deduplicating and Generating IDs\n")
courses_df <- courses_df |>
  mutate(Lat_Round = round(Latitude, 4), Lon_Round = round(Longitude, 4)) |>
  group_by(Lat_Round, Lon_Round, Course_Name) |>
  arrange(desc(Holes)) |>
  slice(1) |>
  ungroup() |>
  mutate(course_id = row_number()) |>
  select(course_id, Course_Name, Ownership_Type, Holes, Address, City, State_Abbr, Zip_Code, Longitude, Latitude)

write_csv(courses_df, OUT_PARSED)
cat(sprintf("    [OK] Parsed data saved -> %s\n", OUT_PARSED))


# PART B: Spatial Join
cat(" 3  Converting to sf object (EPSG:4326)\n")
courses_sf <- courses_df |>
  filter(!is.na(Longitude), !is.na(Latitude)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE)

cat(" 4  Reading 2022 US County boundaries (vendored TIGER/Line)\n")
# [METHODOLOGY] CRS: EPSG 4326 (WGS 84) - projects county boundaries to match golf course point CRS for spatial join
county_sf <- st_read(COUNTY_SHP, quiet = TRUE) |>
  st_transform(4326)

cat(" 5  Spatial point-in-polygon join\n")
courses_sf <- st_join(courses_sf, county_sf, join = st_intersects)  # [METHODOLOGY]

data("fips_codes", package = "tigris")
state_map_df <- unique(fips_codes[, c("state_code", "state")])
courses_sf   <- left_join(courses_sf, state_map_df, by = c("STATEFP" = "state_code"))

courses_df <- st_drop_geometry(courses_sf) |>
  mutate(
    FIPS              = str_pad(as.character(GEOID), width = 5, pad = "0"),
    County_Name       = NAME,
    Tigris_State_Abbr = state
  ) |>
  select(course_id, Course_Name, Ownership_Type, Holes, Address, City, State_Abbr,
          Zip_Code, Longitude, Latitude, FIPS, County_Name, Tigris_State_Abbr)

write_csv(courses_df, OUT_SPATIAL)
cat(sprintf("    [OK] Spatial data saved -> %s\n", OUT_SPATIAL))


# PART C: Economic Proxy Merge
cat(" 6  Processing USDA County Ag-Use data\n")
usda_df <- read_csv(USDA_IN, show_col_types = FALSE) |>
  filter(`Data Item` == "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / ACRE") |>
  mutate(
    FIPS = paste0(
      str_pad(as.integer(`State ANSI`), 2, pad = "0"),
      str_pad(as.integer(`County ANSI`), 3, pad = "0")
    ),
    USDA_Ag_Value_Per_Acre = as.numeric(gsub(",", "", Value))
  ) |>
  filter(!is.na(USDA_Ag_Value_Per_Acre)) |>
  distinct(FIPS, .keep_all = TRUE) |>
  select(FIPS, USDA_Ag_Value_Per_Acre)

cat(" 7  Processing FHFA Panel Counties data\n")
fhfa_df <- read_excel(FHFA_IN, sheet = "Panel Counties", skip = 1) |>
  filter(Year == 2022) |>
  mutate(FIPS = str_pad(as.character(FIPS), width = 5, pad = "0"))

as_is_col <- grep("Per Acre, As-Is", names(fhfa_df), value = TRUE)
fhfa_df <- fhfa_df |>
  mutate(FHFA_Res_Value_Per_Acre = as.numeric(.data[[as_is_col]])) |>
  distinct(FIPS, .keep_all = TRUE) |>
  select(FIPS, FHFA_Res_Value_Per_Acre)

cat(" 8  Left-joining proxies onto golf courses\n")
courses_df <- courses_df |>
  left_join(usda_df, by = "FIPS") |>
  left_join(fhfa_df, by = "FIPS")

write_csv(courses_df, OUT_VAL_JOIN)
cat(sprintf("    [OK] Valuation data saved -> %s\n", OUT_VAL_JOIN))


# PART D: RUCC Classification & Baseline Valuation
cat(" 9  Reading 2023 RUCC data (vendored)\n")
rucc_df <- read_csv(
  RUCC_CSV,
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
) |>
  filter(Attribute == "RUCC_2023") |>
  mutate(
    FIPS      = str_pad(as.character(FIPS), width = 5, pad = "0"),
    RUCC_2023 = as.integer(Value)
  ) |>
  distinct(FIPS, .keep_all = TRUE) |>
  select(FIPS, RUCC_2023)

cat(" 10 Merging RUCC and Classifying Urban/Rural\n")
courses_df <- courses_df |>
  left_join(rucc_df, by = "FIPS") |>
  mutate(
    county_type = case_when(
      RUCC_2023 %in% 1:3 ~ "Urban",
      RUCC_2023 %in% 4:9 ~ "Rural",
      TRUE ~ NA_character_
    ),
    Baseline_Value_Per_Acre = case_when(
      county_type == "Urban" ~ FHFA_Res_Value_Per_Acre,
      county_type == "Rural" ~ USDA_Ag_Value_Per_Acre,
      TRUE ~ NA_real_
    )
  )


n            <- nrow(courses_df)
missing_fips <- sum(is.na(courses_df$FIPS))
usda_hit     <- sum(!is.na(courses_df$USDA_Ag_Value_Per_Acre))
fhfa_hit     <- sum(!is.na(courses_df$FHFA_Res_Value_Per_Acre))
urban        <- sum(courses_df$county_type == "Urban", na.rm = TRUE)
rural        <- sum(courses_df$county_type == "Rural", na.rm = TRUE)
unclassified <- sum(is.na(courses_df$county_type))
missing_base <- sum(is.na(courses_df$Baseline_Value_Per_Acre))
bv           <- courses_df$Baseline_Value_Per_Acre[!is.na(courses_df$Baseline_Value_Per_Acre)]

cat("\n=== OUTPUT STATISTICS ===\n")
cat(sprintf("  Original raw rows:        %s\n",   formatC(original_n, big.mark = ",")))
cat(sprintf("  Total golf courses:       %s  (after deduplication)\n", formatC(n, big.mark = ",")))
cat(sprintf("  Missing FIPS (no county): %s\n",   formatC(missing_fips, big.mark = ",")))
cat(sprintf("  USDA match rate:          %s / %s  (%.2f%%)\n",
            formatC(usda_hit, big.mark = ","), formatC(n, big.mark = ","), 100 * usda_hit / n))
cat(sprintf("  FHFA match rate:          %s / %s  (%.2f%%)\n",
            formatC(fhfa_hit, big.mark = ","), formatC(n, big.mark = ","), 100 * fhfa_hit / n))
cat(sprintf("  Urban courses:            %s\n",   formatC(urban, big.mark = ",")))
cat(sprintf("  Rural courses:            %s\n",   formatC(rural, big.mark = ",")))
cat(sprintf("  Unclassified (no RUCC):   %d\n",   unclassified))
cat(sprintf("  Missing Baseline value:   %s  (MICE imputation target)\n", formatC(missing_base, big.mark = ",")))
cat("\n  Baseline_Value_Per_Acre summary:\n")
cat(sprintf("    Min:    $%14s\n", formatC(min(bv),    format = "f", digits = 2, big.mark = ",")))
cat(sprintf("    Median: $%14s\n", formatC(median(bv), format = "f", digits = 2, big.mark = ",")))
cat(sprintf("    Mean:   $%14s\n", formatC(mean(bv),   format = "f", digits = 2, big.mark = ",")))
cat(sprintf("    Max:    $%14s\n", formatC(max(bv),    format = "f", digits = 2, big.mark = ",")))

write_csv(courses_df, OUT_BASELINE)
cat(sprintf("\n  [OK] Final Baseline saved -> %s\n", OUT_BASELINE))
