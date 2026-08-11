# Purpose: Build the manual-retrieval target list for Issue_Register.md P5-22
#          (Section 5.1's per-class assessed-value figures, unreproducible from
#          any data already in the tree). Emits one row per golf-footprint
#          parcel (TMK) with its dominant Development Plan zoning class,
#          golf-clipped acreage, recorded (cadastral) acreage, dominant
#          overlapping course, and whether the parcel is expected to be
#          taxable at all -- so the author can retrieve assessed land value
#          per parcel from qPublic and Phase_5_Assessment_By_Class.R can
#          compute Section 5.1's figures from that completed retrieval.
#
# This intentionally re-derives geometry Phase_5.R's Step 2/Step 6 already
# touch, because neither step keeps per-parcel identity through to its own
# output: Step 2 collapses to a bare TMK list (Target_Golf_Parcels_List.csv,
# no zone, no acreage retained past the aggregate sum); Step 6 intersects
# golf polygons against zoning directly, never against parcels, so it never
# produces a TMK-to-zone mapping at all. Both are needed together here.
#
# [METHODOLOGY, following Phase_5.R exactly, same fixes applied identically]
#   - P5-13: drop cadastre's duplicate tax-boundary parcels (tmk NA) before
#     any intersection.
#   - P5-15: drop OSM's duplicate Ko'olau Golf Club polygon (osm_id 22249545).
#   - Golf-clipped acreage: st_intersection(golf polygons, parcels), summed
#     per TMK across however many golf-polygon fragments touch it.
#   - Dominant zone: st_intersection(golf-clipped parcel fragments, zoning),
#     summed per (TMK, zone_class); the class with the largest intersected
#     area for that TMK is its dominant_zone_class.
#   - Dominant course: the golf polygon contributing the largest golf-clipped
#     area to that TMK (almost always unambiguous -- one course per parcel --
#     but a handful of parcels may straddle two courses' polygons).
#   - Recorded (cadastral) acreage: 'Recorded Area Acres' from the tax roll
#     CSV, joined by TMK -- same file and join Phase_5.R Step 4 already uses.
#
# Row universe: filtered to exactly the 1,072 TMKs already committed in
# Target_Golf_Parcels_List.csv (Phase_5.R Step 2's own output), NOT whatever
# TMK set this script's own fresh st_intersection() produces. Per
# Issue_Register.md P5-21, st_intersection() has already been shown to be
# sensitive to the GEOS/GDAL/PROJ stack it runs under -- re-deriving the row
# universe here risked silently producing a *third* different parcel count
# next to the manuscript's original and Phase_5.R's committed 1,072. Any
# mismatch between this script's fresh geometry and the committed list is
# reported explicitly, not silently reconciled.
#
# Reads:  Phase 2.../Data/R/R_Phase2_OSM_Golf_Polygons.gpkg
#         00 - Data Sources/Honolulu/All_Parcels_6378200148342636690.gpkg
#         00 - Data Sources/Honolulu/All_Parcels_-4613852522541990741.csv
#         00 - Data Sources/Honolulu/Zoning_-2205419429161838665.gpkg
#         00 - Data Sources/Original Data/tl_2022_us_county.shp
#         Phase 5.../Data/R/Target_Golf_Parcels_List.csv (canonical TMK universe)
# Writes: Data/R/Assessment_Retrieval_Targets.csv

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(readr)
  library(this.path)
})

SCRIPT_DIR <- this.path::this.dir()
WORK_DIR   <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)

OSM_IN <- file.path(
  WORK_DIR, "Phase 2 Spatial Polygons and True Acreage",
  "Data", "R", "R_Phase2_OSM_Golf_Polygons.gpkg"
)
HONOLULU_DIR  <- file.path(WORK_DIR, "00 - Data Sources", "Honolulu")
PARCELS_GPKG  <- file.path(HONOLULU_DIR, "All_Parcels_6378200148342636690.gpkg")
PARCELS_CSV   <- file.path(HONOLULU_DIR, "All_Parcels_-4613852522541990741.csv")
ZONING_GPKG   <- file.path(HONOLULU_DIR, "Zoning_-2205419429161838665.gpkg")
COUNTY_SHP    <- file.path(WORK_DIR, "00 - Data Sources", "Original Data", "tl_2022_us_county.shp")
CANONICAL_TMK_LIST <- file.path(SCRIPT_DIR, "Data", "R", "Target_Golf_Parcels_List.csv")

OUT_DIR <- file.path(SCRIPT_DIR, "Data", "R")
OUT_CSV <- file.path(OUT_DIR, "Assessment_Retrieval_Targets.csv")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

M2_PER_ACRE <- 4046.856422

for (f in c(OSM_IN, PARCELS_GPKG, PARCELS_CSV, ZONING_GPKG, COUNTY_SHP, CANONICAL_TMK_LIST)) {
  if (!file.exists(f)) stop(sprintf("[FATAL] Input not found:\n  %s", f))
}

# === Step 1: golf polygons, filtered to Oahu, P5-15 Ko'olau dedup ===
cat("[1] Loading OSM golf polygons...\n")
osm_golf_sf <- st_read(OSM_IN, quiet = TRUE)

oahu_boundary_sf <- st_read(COUNTY_SHP, quiet = TRUE) |>
  filter(STATEFP == "15", NAME == "Honolulu") |>
  st_transform(st_crs(osm_golf_sf))

oahu_golf_sf <- st_filter(osm_golf_sf, oahu_boundary_sf, .predicate = st_intersects)
n_before_koolau <- nrow(oahu_golf_sf)
oahu_golf_sf <- oahu_golf_sf |> filter(osm_id != 22249545)  # P5-15
cat(sprintf(
  "    %d Oahu golf polygons (dropped %d P5-15 Ko'olau duplicate).\n",
  nrow(oahu_golf_sf), n_before_koolau - nrow(oahu_golf_sf)
))

# === Step 2: parcels, P5-13 dedup, reproject to match golf CRS ===
cat("[2] Loading Honolulu cadastral parcels...\n")
parcels_sf <- st_read(PARCELS_GPKG, quiet = TRUE)
n_before_dedup <- nrow(parcels_sf)
parcels_sf <- parcels_sf |> filter(!is.na(tmk))  # P5-13
cat(sprintf(
  "    Dropped %s duplicate tax-boundary parcels (tmk NA) of %s total.\n",
  formatC(n_before_dedup - nrow(parcels_sf), big.mark = ","),
  formatC(n_before_dedup, big.mark = ",")
))
if (!isTRUE(st_crs(parcels_sf) == st_crs(oahu_golf_sf))) {
  parcels_sf <- st_transform(parcels_sf, st_crs(oahu_golf_sf))
}

# === Step 3: golf-clipped parcel fragments (golf ∩ parcels), per-TMK, per-course ===
cat("[3] Intersecting golf polygons against parcels (golf-clipped fragments)...\n")
golf_geom_col <- attr(oahu_golf_sf, "sf_column")
frag_sf <- st_intersection(
  oahu_golf_sf[c("osm_id", "name", golf_geom_col)],
  parcels_sf[c("tmk", attr(parcels_sf, "sf_column"))]
)
frag_sf$area_acres <- as.numeric(st_area(frag_sf)) / M2_PER_ACRE
cat(sprintf("    %d golf-parcel fragments.\n", nrow(frag_sf)))

# golf_clipped_acres per TMK (sum across every fragment/course touching it)
golf_clipped_by_tmk <- st_drop_geometry(frag_sf) |>
  group_by(tmk) |>
  summarise(golf_clipped_acres = sum(area_acres, na.rm = TRUE), .groups = "drop")

# dominant course per TMK: the osm_id contributing the most golf-clipped area
course_by_tmk <- st_drop_geometry(frag_sf) |>
  group_by(tmk, osm_id, name) |>
  summarise(course_area = sum(area_acres, na.rm = TRUE), .groups = "drop") |>
  arrange(tmk, desc(course_area)) |>
  filter(!duplicated(tmk)) |>
  select(tmk, course_name = name, dominant_course_area_acres = course_area)

n_multi_course_tmk <- st_drop_geometry(frag_sf) |>
  distinct(tmk, osm_id) |>
  count(tmk) |>
  filter(n > 1) |>
  nrow()
cat(sprintf(
  "    %d TMKs touch more than one golf polygon (course_name is the dominant one by area).\n",
  n_multi_course_tmk
))

# === Step 4: dominant zoning class per TMK (golf-clipped fragments ∩ zoning) ===
cat("[4] Intersecting golf-clipped fragments against Development Plan zoning...\n")
zoning_sf <- st_read(ZONING_GPKG, quiet = TRUE)
if (!isTRUE(st_crs(zoning_sf) == st_crs(frag_sf))) {
  zoning_sf <- st_transform(zoning_sf, st_crs(frag_sf))
}
zone_frag_sf <- st_intersection(
  frag_sf[c("tmk", attr(frag_sf, "sf_column"))],
  zoning_sf[c("zone_class", "zoning_description")]
)
zone_frag_sf$zone_area_acres <- as.numeric(st_area(zone_frag_sf)) / M2_PER_ACRE
cat(sprintf("    %d parcel-zone fragments.\n", nrow(zone_frag_sf)))

zone_by_tmk <- st_drop_geometry(zone_frag_sf) |>
  group_by(tmk, zone_class, zoning_description) |>
  summarise(zone_area_acres = sum(zone_area_acres, na.rm = TRUE), .groups = "drop") |>
  arrange(tmk, desc(zone_area_acres)) |>
  filter(!duplicated(tmk)) |>
  select(tmk, dominant_zone_class = zone_class, dominant_zone_description = zoning_description)

n_unzoned <- length(setdiff(unique(frag_sf$tmk), unique(zone_by_tmk$tmk)))
if (n_unzoned > 0) {
  cat(sprintf(
    "    [Note] %d TMKs with a golf-clipped fragment had no zoning-layer overlap (left NA).\n",
    n_unzoned
  ))
}

# === Step 5: recorded (cadastral) acreage per TMK, from the tax roll ===
cat("[5] Reading tax roll for recorded (cadastral) acreage...\n")
tax_roll <- read_csv(PARCELS_CSV, show_col_types = FALSE) |>
  mutate(TMK = trimws(as.character(TMK))) |>
  select(tmk = TMK, recorded_area_acres = `Recorded Area Acres`)

n_blank_roll <- sum(is.na(tax_roll$recorded_area_acres))
cat(sprintf(
  "    [Data-quality flag] 'Recorded Area Acres' is blank for %s of %s tax-roll rows (%.1f%%)\n",
  formatC(n_blank_roll, big.mark = ","), formatC(nrow(tax_roll), big.mark = ","),
  n_blank_roll / nrow(tax_roll) * 100
))
cat("    -- a genuine source-data gap (confirmed against the raw CSV directly, not a parse\n")
cat("    error), not specific to the golf footprint. Any total built by summing this column\n")
cat("    with NAs removed will undercount true cadastral acreage; reported below explicitly\n")
cat("    for the golf-footprint subset rather than silently netted out.\n")

# === Step 6: assemble, restricted to the canonical 1,072-TMK universe ===
cat("[6] Assembling target list against the canonical committed TMK universe...\n")
canonical <- read_csv(CANONICAL_TMK_LIST, col_types = cols(TMK = col_character())) |>
  mutate(tmk = trimws(TMK)) |>
  select(tmk)
cat(sprintf("    Canonical (Target_Golf_Parcels_List.csv): %d TMKs.\n", nrow(canonical)))

fresh_tmks <- unique(golf_clipped_by_tmk$tmk)
extra_in_fresh    <- setdiff(fresh_tmks, canonical$tmk)
missing_from_fresh <- setdiff(canonical$tmk, fresh_tmks)
cat(sprintf(
  "    This run's fresh st_intersection(): %d TMKs (%d not in the canonical list, %d canonical TMKs not reproduced here).\n",
  length(fresh_tmks), length(extra_in_fresh), length(missing_from_fresh)
))
if (length(extra_in_fresh) > 0 || length(missing_from_fresh) > 0) {
  cat("    [P5-21] Non-empty set difference, consistent with st_intersection()'s already-\n")
  cat("    documented cross-run sensitivity. Row universe below is the canonical 1,072,\n")
  cat("    not this run's own fresh set -- any TMK missing from the fresh geometry gets\n")
  cat("    NA acreage/zone/course and is flagged in a dedicated column.\n")
}

targets <- canonical |>
  left_join(golf_clipped_by_tmk, by = "tmk") |>
  left_join(zone_by_tmk, by = "tmk") |>
  left_join(course_by_tmk |> select(tmk, course_name), by = "tmk") |>
  left_join(tax_roll, by = "tmk") |>
  mutate(
    geometry_reproduced_this_run = tmk %in% fresh_tmks,
    is_federal_or_military = !is.na(dominant_zone_description) &
      grepl("federal|military", dominant_zone_description, ignore.case = TRUE),
    expected_taxable = !is_federal_or_military
  ) |>
  select(
    TMK = tmk, dominant_zone_class, golf_clipped_acres, recorded_area_acres,
    course_name, expected_taxable, dominant_zone_description,
    geometry_reproduced_this_run
  ) |>
  arrange(dominant_zone_class, desc(golf_clipped_acres))

write_csv(targets, OUT_CSV)
cat(sprintf("\nSaved: %s (%d rows)\n", OUT_CSV, nrow(targets)))

# === Report: parcel count per zoning class ===
cat("\n=== Parcel count per dominant_zone_class (n = 1,072 canonical TMKs) ===\n")
print(targets |> count(dominant_zone_class, sort = TRUE))

n_na_zone <- sum(is.na(targets$dominant_zone_class))
cat(sprintf("\n%d of %d TMKs have no dominant_zone_class (no reproduced fragment/no zoning overlap).\n",
            n_na_zone, nrow(targets)))

n_p1p2       <- sum(targets$dominant_zone_class %in% c("P-1", "P-2"), na.rm = TRUE)
n_p1p2f1     <- sum(targets$dominant_zone_class %in% c("P-1", "P-2", "F-1"), na.rm = TRUE)
n_f1_only    <- sum(targets$dominant_zone_class == "F-1", na.rm = TRUE)

cat("\n=== 105-parcel Preservation-count check ===\n")
cat(sprintf("  P-1 + P-2 only (excludes F-1):        %d parcels\n", n_p1p2))
cat(sprintf("  P-1 + P-2 + F-1 (broad Preservation):  %d parcels\n", n_p1p2f1))
cat(sprintf("  Of which F-1 (federal/military):       %d parcels\n", n_f1_only))
cat(sprintf(
  "  Manuscript's cited figure: 105. %s\n",
  if (n_p1p2 == 105) "MATCHES the narrow (P-1+P-2) definition exactly."
  else if (n_p1p2f1 == 105) "MATCHES the broad (P-1+P-2+F-1) definition exactly."
  else sprintf(
    "MATCHES NEITHER definition (narrow=%d, broad=%d) -- 105 does not reproduce under either grouping.",
    n_p1p2, n_p1p2f1
  )
))

cat("\n=== expected_taxable summary ===\n")
print(table(targets$expected_taxable, useNA = "always"))

n_na_recorded <- sum(is.na(targets$recorded_area_acres))
cat(sprintf(
  "\n=== recorded_area_acres coverage (golf-footprint subset) ===\n  %d of %d parcels (%.1f%%) have NO recorded cadastral acreage in the tax roll.\n",
  n_na_recorded, nrow(targets), n_na_recorded / nrow(targets) * 100
))
cat(sprintf(
  "  Sum of what IS populated: %.2f ac (na.rm=TRUE) -- an undercount of true cadastral\n",
  sum(targets$recorded_area_acres, na.rm = TRUE)
))
cat("  area, not a total; do not cite this as 'the' recorded acreage of the footprint.\n")
