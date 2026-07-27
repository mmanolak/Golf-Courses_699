# Phase 2 - Two-Tier Acreage Pipeline
# Master Script: Phase_2.R
#
# Fully self-contained - no bulk scripts required.
#
# Pipeline stages:
#   Step 0  (OSM Parse):  Read golf-course polygons from the pre-built Python
#                         GPKG (pyosmium output). Reproject to EPSG:5070 and
#                         compute true acreage in parallel. Save canonical GPKG
#                         to Data/R/.
#   Tier 1  (OSM Match):  Point-in-polygon + 500 m nearest-neighbour fallback.
#   Tier 2  (MICE):       Label remaining unmatched rows "MICE_Target".
#   Finalize:             final_acreage = osm_acres (acres).
#
# acreage_source values: "OSM" | "MICE_Target"
#
# X-02/P2-03/P2-01 resolved (author decision, 2026-07-27): the Tigris Area
# Landmarks fallback tier (Census FULLNAME: Golf/Country Club) has been
# removed. It recovered 0 courses in the 2026-06-12 run that fed every
# published number to date, and its live network fetch is non-deterministic
# by construction -- a one-time offline diagnostic (2026-07-27, run against
# 7 states, not committed as pipeline code) confirmed this was a network/
# environment miss, not a structural non-match: tigris::landmarks() does
# return real Golf/Country Club area features today (109 in FL, 504 in CA,
# 67 in AZ, 26 in HI, etc.), so a future run on a different day could have
# silently repopulated this tier and changed final_acreage for a subset of
# MICE_Target rows -- shifting R's Phase 3 input between runs for reasons
# unrelated to any code change. Removing it makes R's final_acreage
# identical in construction to Python/Julia's osm_acreage (no coalesce, no
# live fetch), closing X-02/P2-01's three-tier-vs-two-tier structural
# question and P2-03's reproducibility risk in one move. See Issue_Register.md.
#
# Reads:
#   00 - Data Sources/Original Data/us-260413.osm.pbf      (Step 0, primary)
#   Phase 2 .../Data/python/Py_Phase2_OSM_Golf_Polygons.gpkg (Step 0, fallback)
#   Phase 1 Parsing/Data/R/R_Phase1_Baseline_Golf_Valuation.csv
#
# Writes:
#   Phase 2 .../Data/R/R_Phase2_OSM_Golf_Polygons.gpkg
#   Phase 2 .../Data/R/R_Phase2_Acreage_Matched_v2.csv
#
# Run from anywhere - all paths resolve relative to this script's location.


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
  library(sf)
  library(future)
  library(furrr)
  library(parallelly)
  library(this.path)
})


# === 2. GLOBALS & PATHS ===

SAFE_WORKERS <- min(availableCores() - 2, 22)
SAFE_WORKERS <- max(SAFE_WORKERS, 1L)

options(future.globals.maxSize = 48 * 1024^3)  # 48gb of usuable Memory
plan(multisession, workers = SAFE_WORKERS)

sf_use_s2(FALSE)

TARGET_CRS    <- 5070
MAX_NEAREST_M <- 500
MIN_ACRES     <- 5
MAX_ACRES     <- 1500
SQ_M_PER_ACRE <- 4046.8564224
SQ_FT_PER_ACRE <- 43560

SCRIPT_DIR <- this.path::this.dir()
ROOT_DIR   <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)

PBF_FILE     <- file.path(ROOT_DIR, "00 - Data Sources", "Original Data", "us-260413.osm.pbf")
PY_GPKG      <- file.path(SCRIPT_DIR, "Data", "python", "Py_Phase2_OSM_Golf_Polygons.gpkg")
OSM_GPKG_OUT <- file.path(SCRIPT_DIR, "Data", "R", "R_Phase2_OSM_Golf_Polygons.gpkg")
PHASE1_CSV   <- file.path(ROOT_DIR, "Phase 1 Parsing", "Data", "R",
                           "R_Phase1_Baseline_Golf_Valuation.csv")
OUT_CSV      <- file.path(SCRIPT_DIR, "Data", "R", "R_Phase2_Acreage_Matched_v2.csv")


# === 3. FUNCTIONS ===

print_separator <- function(char = "=") {
  cat(paste(rep(char, 80), collapse = ""), "\n")
}


# === 4. EXECUTION ===

cat("\n")
print_separator()
cat("Phase 2 - Three-Tier Acreage Pipeline\n")
cat("Script: Phase_2.R (master - Parse OSM + OSM Match + Finalize)\n")
print_separator()
cat(sprintf("  Script dir      : %s\n", SCRIPT_DIR))
cat(sprintf("  Work dir        : %s\n", ROOT_DIR))
cat(sprintf("  PBF source      : %s\n", PBF_FILE))
cat(sprintf("  Python GPKG     : %s\n", PY_GPKG))
cat(sprintf("  OSM GPKG out    : %s\n", OSM_GPKG_OUT))
cat(sprintf("  Phase 1 CSV     : %s\n", PHASE1_CSV))
cat(sprintf("  Output CSV      : %s\n", OUT_CSV))
cat(sprintf("  Parallel workers: %d\n", SAFE_WORKERS))
cat("\n")

if (!file.exists(PHASE1_CSV)) {
  stop("Required input not found:\n  ", PHASE1_CSV)
}


# STEP 0 - Parse OSM Golf Polygons

print_separator()
cat("STEP 0: Parse OSM Golf Polygons\n")
print_separator()

cat("\n[Step 0] Loading OSM golf course polygons...\n")

# NOTE: GDAL's OGR driver cannot reliably parse this particular 11 GB PBF file
# (crashes at ~byte 3,049,247,581 due to data corruption). The Python pipeline
# used pyosmium (C++ streaming handler) which tolerates the corruption.
# Priority: use the Python GPKG if it exists (fast, clean); only attempt the
# PBF as a last resort when the GPKG is absent.

osm_golf_sf <- NULL

if (file.exists(PY_GPKG)) {
  cat(sprintf("  Reading Python GPKG: %s\n", PY_GPKG))
  osm_golf_sf <- st_read(PY_GPKG, quiet = TRUE)  # [METHODOLOGY] st_read from pyosmium GPKG
  cat(sprintf(
    "  Loaded from GPKG: %s polygons\n",
    formatC(nrow(osm_golf_sf), big.mark = ",")
  ))
} else if (file.exists(PBF_FILE)) {
  cat(sprintf("  Python GPKG not found -- attempting PBF read: %s\n", PBF_FILE))
  cat("  (Warning: GDAL may crash on this PBF; run Phase_2.py first if this hangs.)\n")
  tryCatch(
    {
      osm_golf_sf <- st_read(  # [METHODOLOGY] st_read from OSM PBF
        PBF_FILE,
        layer = "multipolygons",
        query = "SELECT * FROM multipolygons WHERE leisure = 'golf_course'",
        quiet = TRUE
      )
      cat(sprintf(
        "  PBF read succeeded: %s raw polygons\n",
        formatC(nrow(osm_golf_sf), big.mark = ",")
      ))
    },
    error = function(e) {
      cat(sprintf("  [ERROR] PBF read failed: %s\n", conditionMessage(e)))
    }
  )
}

if (is.null(osm_golf_sf)) {
  stop(
    "OSM source not available.\n",
    "  Python GPKG not found : ", PY_GPKG,  "\n",
    "  PBF not found or failed: ", PBF_FILE, "\n",
    "  Run Phase_2.py first to produce the Python GPKG."
  )
}

osm_golf_sf <- st_make_valid(osm_golf_sf)

cat(sprintf(
  "\n  Reprojecting to EPSG:%d and computing acreage (%d workers)...\n",
  TARGET_CRS, SAFE_WORKERS
))

osm_chunks <- osm_golf_sf |>
  mutate(chunk_id = row_number() %% SAFE_WORKERS) |>
  group_split(chunk_id)

osm_processed <- future_map(osm_chunks, function(chunk) {
  chunk_proj             <- st_transform(chunk, TARGET_CRS)  # [METHODOLOGY] EPSG:5070 - equal-area CRS
  chunk_proj$area_m2     <- as.numeric(st_area(chunk_proj))  # [METHODOLOGY]
  chunk_proj$osm_acreage <- chunk_proj$area_m2 / SQ_M_PER_ACRE
  chunk_proj
}, .progress = TRUE, .options = furrr_options(seed = TRUE))

osm_golf_sf <- bind_rows(osm_processed) |> select(-chunk_id)

raw_count   <- nrow(osm_golf_sf)
osm_golf_sf <- osm_golf_sf |> filter(osm_acreage >= MIN_ACRES, osm_acreage <= MAX_ACRES)
kept_count  <- nrow(osm_golf_sf)

cat(sprintf("  Raw polygons: %s\n", formatC(raw_count, big.mark = ",")))
cat(sprintf(
  "  Dropped (< %d or > %d acres): %s\n",
  MIN_ACRES, MAX_ACRES, formatC(raw_count - kept_count, big.mark = ",")
))
cat(sprintf("  Final polygon count: %s\n", formatC(kept_count, big.mark = ",")))

ac <- osm_golf_sf$osm_acreage
cat("\n  osm_acreage summary:\n")
cat(sprintf("    Min:    %10.1f acres\n", min(ac)))
cat(sprintf("    Median: %10.1f acres\n", median(ac)))
cat(sprintf("    Mean:   %10.1f acres\n", mean(ac)))
cat(sprintf("    Max:    %10.1f acres\n", max(ac)))

cat("\n  First 5 rows:\n")
print(head(osm_golf_sf |>
  st_drop_geometry() |>
  select(any_of(c("osm_id", "name", "id")), osm_acreage), 5))

gpkg_dir <- dirname(OSM_GPKG_OUT)
if (!dir.exists(gpkg_dir)) dir.create(gpkg_dir, recursive = TRUE)
st_write(osm_golf_sf, OSM_GPKG_OUT, delete_dsn = TRUE, quiet = TRUE)  # [METHODOLOGY]
cat(sprintf("\n  [OK] Saved OSM GPKG -> %s\n", OSM_GPKG_OUT))

rm(osm_chunks, osm_processed)
gc(full = TRUE)


# TIER 1 - OSM Matching

print_separator()
cat("TIER 1: OSM Polygon Matching\n")
print_separator()

cat("\n[Step 1] Loading Phase 1 baseline dataset...\n")

baseline_df <- read_csv(PHASE1_CSV, show_col_types = FALSE)
cat(sprintf("  Loaded %s courses\n", formatC(nrow(baseline_df), big.mark = ",")))

required_cols <- c("Course_Name", "County_Name", "State_Abbr", "Latitude", "Longitude")
missing_cols  <- setdiff(required_cols, names(baseline_df))
if (length(missing_cols) > 0) {
  stop(
    "Missing required columns: ", paste(missing_cols, collapse = ", "),
    "\n  Available: ", paste(names(baseline_df), collapse = ", ")
  )
}

cat(sprintf("  Columns: %s\n", paste(names(baseline_df), collapse = ", ")))

baseline_df <- baseline_df |> mutate(row_idx = row_number())

cat("\n[Step 2] Preparing OSM polygons...\n")

osm_golf_sf$area_sqft <- as.numeric(st_area(osm_golf_sf)) * (SQ_FT_PER_ACRE / SQ_M_PER_ACRE)  # [METHODOLOGY]

cat(sprintf(
  "  %s OSM features ready  |  CRS: %s\n",
  formatC(nrow(osm_golf_sf), big.mark = ","),
  st_crs(osm_golf_sf)$input
))

cat(sprintf("\n[Step 3] Converting baseline to sf (EPSG:%d)...\n", TARGET_CRS))

courses_sf <- st_as_sf(  # [METHODOLOGY]
  baseline_df,
  coords = c("Longitude", "Latitude"),
  crs    = 4326,
  remove = FALSE
) |> st_transform(TARGET_CRS)  # [METHODOLOGY] EPSG:5070

cat(sprintf(
  "  %s course points in EPSG:%d\n",
  formatC(nrow(courses_sf), big.mark = ","), TARGET_CRS
))

cat("\n[Step 4] Pass 1 -- st_intersects (point-in-polygon)...\n")

intersects_result <- st_join(  # [METHODOLOGY] point-in-polygon primary match
  courses_sf,
  osm_golf_sf |> select(area_sqft),
  join = st_intersects,
  left = TRUE
)

intersects_df <- as.data.frame(intersects_result) |>
  arrange(row_idx, desc(area_sqft)) |>
  filter(!duplicated(row_idx))

baseline_df <- baseline_df |>
  left_join(
    intersects_df |> select(row_idx, OSM_Area_SqFt = area_sqft),
    by = "row_idx"
  )

pass1_hits <- sum(!is.na(baseline_df$OSM_Area_SqFt))
cat(sprintf(
  "  Pass 1 matches (exact intersection): %s\n",
  formatC(pass1_hits, big.mark = ",")
))

cat(sprintf(
  "\n[Step 5] Pass 2 -- nearest-neighbour fallback (<= %d m)...\n",
  MAX_NEAREST_M
))

miss_mask <- is.na(baseline_df$OSM_Area_SqFt)
cat(sprintf("  Courses still missing: %s\n", formatC(sum(miss_mask), big.mark = ",")))

if (any(miss_mask)) {
  miss_sf      <- courses_sf[miss_mask, ]
  nearest_idx  <- st_nearest_feature(miss_sf, osm_golf_sf)
  nearest_dist <- as.numeric(
    st_distance(miss_sf, osm_golf_sf[nearest_idx, ], by_element = TRUE)
  )
  within_range <- nearest_dist <= MAX_NEAREST_M

  if (any(within_range)) {
    baseline_df$OSM_Area_SqFt[miss_mask][within_range] <-
      osm_golf_sf$area_sqft[nearest_idx[within_range]]
  }

  cat(sprintf(
    "  Pass 2 recoveries (NN <= %d m): %s\n",
    MAX_NEAREST_M, formatC(sum(within_range), big.mark = ",")
  ))
}

baseline_df$acreage_source <- ifelse(
  !is.na(baseline_df$OSM_Area_SqFt), "OSM", NA_character_
)

osm_total <- sum(!is.na(baseline_df$OSM_Area_SqFt))
cat(sprintf(
  "  Total OSM-sourced: %s  |  Still missing: %s\n",
  formatC(osm_total, big.mark = ","),
  formatC(sum(is.na(baseline_df$OSM_Area_SqFt)), big.mark = ",")
))

acreage_df <- st_drop_geometry(courses_sf) |>
  select(-any_of("OSM_Area_SqFt")) |>
  left_join(
    baseline_df |> select(row_idx, OSM_Area_SqFt, acreage_source),
    by = "row_idx"
  )

rm(courses_sf, intersects_result, intersects_df, osm_golf_sf)


# TIER 2 - Label remaining as MICE_Target
# (Formerly Tier 2 was a Tigris Area Landmarks fallback here; removed
# 2026-07-27, see the header note and Issue_Register.md X-02/P2-01/P2-03.)

print_separator()
cat("TIER 2: Label remaining missing as MICE_Target\n")
print_separator()

mice_mask <- is.na(acreage_df$acreage_source)
acreage_df$acreage_source[mice_mask] <- "MICE_Target"

cat(sprintf("\n  MICE_Target rows: %s\n", formatC(sum(mice_mask), big.mark = ",")))


# Finalize: Build final_acreage column

cat("\n[Step 6] Building final_acreage column (acres)...\n")

acreage_df <- acreage_df |>
  mutate(
    osm_acres = if ("OSM_Area_SqFt" %in% names(acreage_df)) {
      OSM_Area_SqFt / SQ_FT_PER_ACRE
    } else {
      NA_real_
    },
    final_acreage = osm_acres
  ) |>
  select(-any_of(c("osm_acres", "OSM_Area_SqFt", "row_idx")))

cat(sprintf(
  "  final_acreage non-NA: %s (%.1f%%)\n",
  formatC(sum(!is.na(acreage_df$final_acreage)), big.mark = ","),
  100 * mean(!is.na(acreage_df$final_acreage))
))


# Summary report

print_separator()
cat("FINAL SUMMARY - Acreage Source Distribution\n")
print_separator()

source_counts <- acreage_df |>
  count(acreage_source) |>
  mutate(
    Percentage            = round(n / sum(n) * 100, 2),
    Cumulative_Percentage = round(cumsum(n) / sum(n) * 100, 2)
  )

cat("\nAcreage Source Counts:\n")
print(source_counts)

cat("\nAdditional Statistics by Acreage Source:\n")
stats_by_source <- acreage_df |>
  group_by(acreage_source) |>
  summarise(
    Count = n(),
    Mean_Acreage = if (all(is.na(final_acreage))) {
      NA_real_
    } else {
      round(mean(final_acreage, na.rm = TRUE), 2)
    },
    Median_Acreage = if (all(is.na(final_acreage))) {
      NA_real_
    } else {
      round(median(final_acreage, na.rm = TRUE), 2)
    },
    Min_Acreage = if (all(is.na(final_acreage))) {
      NA_real_
    } else {
      round(min(final_acreage, na.rm = TRUE), 2)
    },
    Max_Acreage = if (all(is.na(final_acreage))) {
      NA_real_
    } else {
      round(max(final_acreage, na.rm = TRUE), 2)
    },
    .groups = "drop"
  ) |>
  arrange(acreage_source)
print(stats_by_source)

obs <- acreage_df$final_acreage[!is.na(acreage_df$final_acreage)]
if (length(obs) > 0) {
  cat(sprintf(
    "\n  final_acreage (observed only, n = %s):\n",
    formatC(length(obs), big.mark = ",")
  ))
  cat(sprintf("    Min:    %10.1f acres\n", min(obs)))
  cat(sprintf("    Median: %10.1f acres\n", median(obs)))
  cat(sprintf("    Mean:   %10.1f acres\n", mean(obs)))
  cat(sprintf("    Max:    %10.1f acres\n", max(obs)))
}

cat("\n[Step 7] Saving final output...\n")

out_dir <- dirname(OUT_CSV)
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

write_csv(acreage_df, OUT_CSV)

cat(sprintf("  [OK] Saved -> %s\n", OUT_CSV))
cat(sprintf(
  "       %s rows  |  %d columns\n",
  formatC(nrow(acreage_df), big.mark = ","),
  ncol(acreage_df)
))

print_separator()
cat("PHASE 2 COMPLETE\n")
print_separator()
