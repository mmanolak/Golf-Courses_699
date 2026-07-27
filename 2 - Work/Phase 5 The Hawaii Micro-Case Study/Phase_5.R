# Purpose: Validate the national econometric model against micro-level cadastral
#          reality using Oahu, Hawaii (Honolulu County) as a micro-case study.
#          Runs the complete four-step pipeline end-to-end.
# Inputs:  Phase 1 Parsing/Data/R/R_Phase1_Baseline_Golf_Valuation.csv
#          Phase 2 Spatial Polygons and True Acreage/Data/R/R_Phase2_OSM_Golf_Polygons.gpkg
#          Phase 3 Economic Merge and MICE Imputation/Data/R/R_Imputed_Dataset_[1-100].csv
#          00 - Data Sources/Honolulu/All_Parcels_6378200148342636690.gpkg
#          00 - Data Sources/Honolulu/All_Parcels_-4613852522541990741.csv
#          00 - Data Sources/Honolulu/Zoning_-2205419429161838665.gpkg
# Outputs: Data/R/Target_Golf_Polygons.gpkg
#          Data/R/Honolulu_Parcels_Reprojected.gpkg
#          Data/R/Target_Golf_Parcels_List.csv
#          Data/R/Phase5_Oahu_Comparison.csv
#          Data/R/Phase5_Geographic_Breakdown.csv
#          Data/R/Phase5_Step6_Zoning_Percentages.csv
#          Data/R/Phase5_Step6_Zone_Golf_Penetration.csv


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
    library(sf)
    library(tidyverse)
    library(future)
    library(furrr)
    library(parallelly)
    library(this.path)
})


# === 2. GLOBALS & PATHS ===

SCRIPT_DIR    <- this.path::this.dir()
WORK_DIR      <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
OUTPUT_DIR    <- file.path(SCRIPT_DIR, "Data", "R")

PROV_START <- Sys.time()
source(file.path(SCRIPT_DIR, "..", "provenance.R"))
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR, recursive = TRUE)

PHASE1_IN     <- file.path(
    WORK_DIR, "Phase 1 Parsing", "Data", "R",
    "R_Phase1_Baseline_Golf_Valuation.csv"
)
OSM_IN        <- file.path(
    WORK_DIR, "Phase 2 Spatial Polygons and True Acreage",
    "Data", "R", "R_Phase2_OSM_Golf_Polygons.gpkg"
)
PHASE3_DIR    <- file.path(
    WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "R"
)
IMPUTED_PATHS <- file.path(PHASE3_DIR, paste0("R_Imputed_Dataset_", 1:100, ".csv"))

HONOLULU_DIR      <- file.path(WORK_DIR, "00 - Data Sources", "Honolulu")
PARCELS_GPKG      <- file.path(HONOLULU_DIR, "All_Parcels_6378200148342636690.gpkg")
PARCELS_CSV          <- file.path(HONOLULU_DIR, "All_Parcels_-4613852522541990741.csv")
ZONING_GPKG          <- file.path(HONOLULU_DIR, "Zoning_-2205419429161838665.gpkg")
# Vendored 2026-07-27 (X-08/Gate-3 policy: no master script performs a network fetch at
# run time). Same TIGER/Line file Phase 1 uses -- source
# https://www2.census.gov/geo/tiger/TIGER2022/COUNTY/tl_2022_us_county.zip
COUNTY_SHP        <- file.path(WORK_DIR, "00 - Data Sources", "Original Data", "tl_2022_us_county.shp")

TARGET_GOLF_OUT      <- file.path(OUTPUT_DIR, "Target_Golf_Polygons.gpkg")
PARCELS_OUT          <- file.path(OUTPUT_DIR, "Honolulu_Parcels_Reprojected.gpkg")
TMK_LIST_OUT         <- file.path(OUTPUT_DIR, "Target_Golf_Parcels_List.csv")
COMPARISON_OUT       <- file.path(OUTPUT_DIR, "Phase5_Oahu_Comparison.csv")
GEO_BREAKDOWN_OUT    <- file.path(OUTPUT_DIR, "Phase5_Geographic_Breakdown.csv")
ZONING_PCT_OUT       <- file.path(OUTPUT_DIR, "Phase5_Step6_Zoning_Percentages.csv")
ZONE_PENETRATION_OUT <- file.path(OUTPUT_DIR, "Phase5_Step6_Zone_Golf_Penetration.csv")

M            <- 100L
M2_PER_ACRE  <- 4046.856422
SAFE_WORKERS <- max(min(availableCores() - 8, 20), 1L)
options(future.globals.maxSize = 20 * 1024^3)
plan(multisession, workers = SAFE_WORKERS)
sf_use_s2(FALSE)


# === 3. FUNCTIONS ===

# (none)


# === 4. EXECUTION ===

cat("\n======================================================================\n")
cat("PHASE 5: THE HAWAII MICRO-CASE STUDY\n")
cat("======================================================================\n")
cat(sprintf("  Work Dir   : %s\n", WORK_DIR))
cat(sprintf("  Output Dir : %s\n", OUTPUT_DIR))

# ---------- Step 1: Geographic Boundary Extraction & Error Analysis ----------

cat("\n--- Step 1: Geographic Boundary Extraction & Error Analysis ---\n")
cat("  Loading datasets...\n")
if (!file.exists(PHASE1_IN)) stop(paste("Input file not found:", PHASE1_IN))
baseline_df <- read_csv(PHASE1_IN, show_col_types = FALSE)
# [METHODOLOGY] st_read - spatial read of Phase 2 OSM golf polygons
if (!file.exists(OSM_IN)) stop(paste("Input file not found:", OSM_IN))
osm_golf_sf <- st_read(OSM_IN, quiet = TRUE)
# [METHODOLOGY] st_read - spatial read of Honolulu cadastral parcel layer
if (!file.exists(PARCELS_GPKG)) stop(paste("Input file not found:", PARCELS_GPKG))
parcels_sf  <- st_read(PARCELS_GPKG, quiet = TRUE)

cat("  Reading Oahu boundary (vendored TIGER/Line)...\n")
if (!file.exists(COUNTY_SHP)) stop(paste("Input file not found:", COUNTY_SHP))
oahu_boundary_sf <- st_read(COUNTY_SHP, quiet = TRUE) |>
    filter(STATEFP == "15", NAME == "Honolulu") |>
    # [METHODOLOGY] st_transform - reproject county boundary to match OSM CRS
    st_transform(st_crs(osm_golf_sf))

cat("  Extracting OSM polygons within Oahu...\n")
# [METHODOLOGY] st_filter - spatial subset of all OSM golf polygons to Honolulu county
oahu_golf_sf <- st_filter(osm_golf_sf, oahu_boundary_sf, .predicate = st_intersects)
if (nrow(oahu_golf_sf) == 0) stop("[FATAL] No OSM polygons found on Oahu.")

oahu_baseline_sf <- baseline_df |>
    filter(County_Name == "Honolulu" | FIPS == 15003) |>
    # [METHODOLOGY] st_as_sf - convert Phase 1 tabular baseline to spatial points
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) |>
    # [METHODOLOGY] st_transform - reproject Phase 1 points to match OSM CRS
    st_transform(st_crs(oahu_golf_sf))

# [METHODOLOGY] st_intersects - check which Phase 1 points fall within an OSM polygon;
#               mismatch rate quantifies Phase 1-to-Phase 2 representational error
intersections <- st_intersects(oahu_baseline_sf, oahu_golf_sf)
hits   <- sum(lengths(intersections) > 0)
misses <- sum(lengths(intersections) == 0)

cat(sprintf("    Phase 1 Baseline Points : %d courses\n", nrow(oahu_baseline_sf)))
cat(sprintf("    Phase 2 OSM Polygons    : %d courses\n", nrow(oahu_golf_sf)))
cat(sprintf(
    "    Direct Point Match Rate : %.1f%%\n",
    (hits / nrow(oahu_baseline_sf)) * 100
))

if (st_crs(oahu_golf_sf) != st_crs(parcels_sf)) {
    cat("  Reprojecting parcels to match OSM CRS...\n")
    # [METHODOLOGY] st_transform - align parcel CRS to OSM CRS for Step 2 overlay
    parcels_sf <- st_transform(parcels_sf, st_crs(oahu_golf_sf))
}

# [METHODOLOGY] st_write - persist Oahu OSM golf polygons for Step 2 parcel intersection
st_write(oahu_golf_sf, TARGET_GOLF_OUT, append = FALSE, quiet = TRUE)
# [METHODOLOGY] st_write - persist reprojected parcel cadastre for Step 2
st_write(parcels_sf, PARCELS_OUT, append = FALSE, quiet = TRUE)

# ---------- Step 2: Island-Wide Parcel Intersection ----------

cat("\n--- Step 2: Island-Wide Parcel Intersection ---\n")
cat("  Performing spatial intersection (cookie-cutter)...\n")
# [METHODOLOGY] st_intersection - cookie-cutter of Phase 2 OSM polygons over the
#               Phase 5 legal cadastre to isolate golf-course parcel fragments
parcel_intersection_sf <- st_intersection(oahu_golf_sf, parcels_sf)
cat(sprintf(
    "  Intersection complete: %d parcel fragments found.\n",
    nrow(parcel_intersection_sf)
))

# P5-07/Decision 6 (2026-07-27): extended to match Python/Julia's 10-candidate list --
# R was previously missing Tax_Map_Key, tax_map_key, MAPKEY, mapkey (dormant on the
# current cadastre schema, but a latent hard-crash risk on a future schema change).
tmk_columns  <- c("TMK", "PARCEL_ID", "Parcel_ID", "parcel_id", "TAX_MAP_KEY",
                   "Tax_Map_Key", "tax_map_key", "MAPKEY", "mapkey", "tmk")
found_column <- intersect(tmk_columns, names(parcel_intersection_sf))[1]
if (is.na(found_column)) stop("[FATAL] No TMK column identified in intersection.")

unique_tmk        <- unique(as.character(parcel_intersection_sf[[found_column]]))
unique_tmk_sorted <- sort(unique_tmk[!is.na(unique_tmk)])
cat(sprintf(
    "  Found %d unique TMKs across the %d golf courses.\n",
    length(unique_tmk_sorted),
    nrow(oahu_golf_sf)
))

tmk_df <- data.frame(TMK = unique_tmk_sorted)
write_csv(tmk_df, TMK_LIST_OUT)

# [METHODOLOGY] st_area - compute legal footprint area from intersection geometry
osm_derived_acres <- as.numeric(sum(st_area(parcel_intersection_sf))) / 4046.86
cat(sprintf(
    "  Total Legal Footprint: %s Acres\n",
    formatC(osm_derived_acres, format = "f", big.mark = ",", digits = 2)
))

# ---------- Step 3: Economic Validation & Spatial Deduplication ----------

cat("\n--- Step 3: Economic Validation & Spatial Deduplication ---\n")
cat("  Loading Phase 3 imputed datasets...\n")
missing_imputed <- IMPUTED_PATHS[!file.exists(IMPUTED_PATHS)]
if (length(missing_imputed) > 0) {
    stop(sprintf(
        "[FATAL] %d imputed dataset(s) not found. Run Phase_3.R first.\n  First missing: %s",
        length(missing_imputed), missing_imputed[1]
    ))
}
oahu_estimates <- vector("list", M)

for (i in seq_len(M)) {
    df_i      <- read_csv(IMPUTED_PATHS[i], show_col_types = FALSE)
    oahu_mask <- !is.na(df_i$Longitude) & !is.na(df_i$Latitude) &
        # [METHODOLOGY] bounding box 21.2–21.9°N, -158.5 to -157.6°W - Oahu geographic filter
        df_i$Latitude  >= 21.2 & df_i$Latitude  <= 21.9 &
        df_i$Longitude >= -158.5 & df_i$Longitude <= -157.6
    oahu_estimates[[i]] <- df_i[oahu_mask, ] |>
        mutate(
            Total_Opportunity_Cost = final_acreage * Baseline_Value_Per_Acre,
            imputation = i
        )
    rm(df_i); gc()
}
oahu_all <- bind_rows(oahu_estimates)

cat("  Applying Spatial Deduplication using true OSM Polygons...\n")
osm_polys_sf <- oahu_golf_sf |> mutate(poly_id = row_number())

unique_courses <- oahu_all |>
    select(Longitude, Latitude, Holes) |>
    group_by(Longitude, Latitude) |>
    summarise(Holes = max(Holes, na.rm = TRUE), .groups = "drop")

courses_sf <- unique_courses |>
    # [METHODOLOGY] st_as_sf - convert deduplicated baseline points to spatial
    st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) |>
    # [METHODOLOGY] st_transform - reproject baseline points to match OSM CRS
    st_transform(st_crs(osm_polys_sf))

# [METHODOLOGY] st_nearest_feature - assign each Phase 1 point to nearest OSM polygon
nearest_idx  <- st_nearest_feature(courses_sf, osm_polys_sf)
# [METHODOLOGY] st_distance - 500m nearest-neighbor cap: points beyond threshold become orphans
nearest_dist <- as.numeric(st_distance(
    courses_sf, osm_polys_sf[nearest_idx, ], by_element = TRUE
))
courses_sf$poly_id <- ifelse(nearest_dist <= 500, osm_polys_sf$poly_id[nearest_idx], NA)

master_keep_list <- courses_sf |>
    mutate(group_id = ifelse(
        is.na(poly_id),
        paste0("orphan_", row_number()),
        as.character(poly_id)
    )) |>
    arrange(group_id, desc(Holes)) |>
    filter(!duplicated(group_id)) |>
    st_drop_geometry() |>
    select(Longitude, Latitude, Holes)

cat(sprintf(
    "  Unique Oahu courses after spatial deduplication: %d\n",
    nrow(master_keep_list)
))

oahu_deduped_list <- lapply(seq_len(M), function(i) {
    oahu_all |>
        filter(imputation == i) |>
        inner_join(master_keep_list, by = c("Longitude", "Latitude", "Holes"))
})

oahu_agg_dedup <- sapply(
    oahu_deduped_list,
    function(d) sum(d$Total_Opportunity_Cost, na.rm = TRUE)
)

q_bar <- mean(oahu_agg_dedup)
v_w   <- mean(sapply(
    oahu_deduped_list,
    function(d) var(d$Total_Opportunity_Cost, na.rm = TRUE)
))
v_b   <- var(oahu_agg_dedup)
v_t   <- v_w + v_b + v_b / M
se    <- sqrt(v_t)
ci_lo <- q_bar - 2.576 * se
ci_hi <- q_bar + 2.576 * se

cat(sprintf(
    "  Pooled Oahu Opportunity Cost: $%.3fB (99%% CI: $%.3fB - $%.3fB)\n",
    q_bar / 1e9, ci_lo / 1e9, ci_hi / 1e9
))

comparison_df <- data.frame(
    Metric = c(
        "Total Golf Courses (Oahu, OSM polygons)",
        "Total Unique TMKs (Step 2)",
        "OSM-Derived Legal Footprint (acres)",
        "Pooled Oahu Opportunity Cost - q_bar ($B)",
        "Standard Error ($B)",
        "95% CI Lower ($B)",
        "95% CI Upper ($B)"
    ),
    Value = c(
        nrow(osm_polys_sf),
        nrow(tmk_df),
        formatC(osm_derived_acres, format = "f", digits = 2, big.mark = ","),
        sprintf("%.3f", q_bar / 1e9),
        sprintf("%.3f", se / 1e9),
        sprintf("%.3f", ci_lo / 1e9),
        sprintf("%.3f", ci_hi / 1e9)
    )
)
write_csv(comparison_df, COMPARISON_OUT)

# ---------- Step 4: Geographic Concentration & Fragmentation Analysis ----------

cat("\n--- Step 4: Geographic Concentration & Fragmentation Analysis ---\n")
cat("  Loading Honolulu Cadastral CSV...\n")
if (!file.exists(PARCELS_CSV)) stop(paste("Input file not found:", PARCELS_CSV))
tax_data <- read_csv(PARCELS_CSV, show_col_types = FALSE)

tmk_df$TMK_clean   <- str_remove_all(as.character(tmk_df$TMK), "[^0-9]")
tmk_col            <- grep("(?i)^tmk$", names(tax_data), value = TRUE)[1]
tax_data$TMK_clean <- str_remove_all(
    as.character(tax_data[[tmk_col]]), "[^0-9]"
)

if (all(nchar(tmk_df$TMK_clean) == 8) &&
    all(nchar(na.omit(tax_data$TMK_clean)) == 9)) {
    tmk_df$TMK_clean <- paste0("1", tmk_df$TMK_clean)
} else if (all(nchar(tmk_df$TMK_clean) == 9) &&
    all(nchar(na.omit(tax_data$TMK_clean)) == 8)) {
    tax_data$TMK_clean <- paste0("1", tax_data$TMK_clean)
}

merged_data <- tmk_df |> inner_join(tax_data, by = "TMK_clean")
cat(sprintf(
    "  Successfully matched %d out of %d TMKs.\n",
    nrow(merged_data), nrow(tmk_df)
))

district_map <- c(
    "1" = "Honolulu (Urban Core)",
    "2" = "Honolulu (East/Anomalies)",
    "3" = "Honolulu (Anomalies)",
    "4" = "Koolaupoko (Kailua/Kaneohe)",
    "5" = "Koolauloa (North/East)",
    "6" = "Waialua (North Shore)",
    "7" = "Wahiawa (Central)",
    "8" = "Waianae (West)",
    "9" = "Ewa (Kapolei/Pearl City)"
)

merged_data <- merged_data |>
    mutate(
        Zone_Code     = as.character(Zone),
        District_Name = ifelse(
            Zone_Code %in% names(district_map),
            district_map[Zone_Code],
            paste("Zone", Zone_Code)
        )
    )

geo_summary <- merged_data |>
    group_by(Zone_Code, District_Name) |>
    summarise(Parcel_Count = n(), .groups = "drop") |>
    mutate(Pct_of_Total_Parcels = (Parcel_Count / sum(Parcel_Count)) * 100) |>
    arrange(desc(Parcel_Count))

cat("\n  Geographic Breakdown:\n")
cat(sprintf(
    "  %-5s %-35s %-15s %-15s\n",
    "Zone", "Geographic District", "Parcel Count", "% of Parcels"
))
for (i in seq_len(nrow(geo_summary))) {
    cat(sprintf(
        "  %-5s %-35s %-15d %-15.1f%%\n",
        geo_summary$Zone_Code[i],
        geo_summary$District_Name[i],
        geo_summary$Parcel_Count[i],
        geo_summary$Pct_of_Total_Parcels[i]
    ))
}

write_csv(geo_summary, GEO_BREAKDOWN_OUT)

# ---------- Step 6: Zoning Intersection Analysis ----------

cat("\n--- Step 6: Zoning Intersection Analysis ---\n")

if (!file.exists(ZONING_GPKG)) {
    stop(sprintf("[FATAL] Zoning layer not found:\n  %s", ZONING_GPKG))
}

# [METHODOLOGY] st_read - spatial read of Honolulu zoning layer
zoning_sf <- st_read(ZONING_GPKG, quiet = TRUE)
cat(sprintf("  Loaded zoning layer: %d features\n", nrow(zoning_sf)))

# [METHODOLOGY] Zoning is in EPSG 3760 (ftUS); reprojected to match golf CRS (EPSG 5070,
#               metres) so st_area() returns m², convertible to acres via 4,046.856422 m²/ac.
if (!isTRUE(st_crs(zoning_sf) == st_crs(oahu_golf_sf))) {
    cat(sprintf("  Reprojecting zoning to EPSG %d...\n", st_crs(oahu_golf_sf)$epsg))
    zoning_sf <- st_transform(zoning_sf, st_crs(oahu_golf_sf))
}

zone_areas_m2     <- as.numeric(st_area(zoning_sf))
county_zone_acres <- st_drop_geometry(zoning_sf) |>
    mutate(zone_total_acres = zone_areas_m2 / M2_PER_ACRE) |>
    group_by(zone_class) |>
    summarise(county_total_acres = sum(zone_total_acres, na.rm = TRUE), .groups = "drop")

# [METHODOLOGY] st_intersection - clips zoning polygons to golf course boundaries,
#               producing fragment geometries whose area quantifies which zoning classes
#               overlap the golf course footprint (Pebesma 2018).
cat("  Performing spatial intersection (golf courses ∩ zoning)...\n")
golf_geom_col   <- attr(oahu_golf_sf, "sf_column")
intersection_sf <- st_intersection(
    oahu_golf_sf[golf_geom_col],
    zoning_sf[c("zone_class", "zoning_description")]
)
cat(sprintf("  Intersection produced %d fragments.\n", nrow(intersection_sf)))

intersection_sf$area_acres <- as.numeric(st_area(intersection_sf)) / M2_PER_ACRE
total_golf_acres            <- sum(intersection_sf$area_acres, na.rm = TRUE)
cat(sprintf("  Total intersected golf footprint: %.1f acres\n", total_golf_acres))

zone_summary_z6 <- intersection_sf |>
    st_drop_geometry() |>
    group_by(zone_class, zoning_description) |>
    summarise(
        acres     = sum(area_acres, na.rm = TRUE),
        fragments = n(),
        .groups   = "drop"
    ) |>
    mutate(pct_of_total = acres / total_golf_acres * 100) |>
    arrange(desc(acres))

zone_penetration_z6 <- zone_summary_z6 |>
    select(zone_class, zoning_description, golf_acres = acres) |>
    left_join(county_zone_acres, by = "zone_class") |>
    mutate(pct_zone_as_golf = golf_acres / county_total_acres * 100) |>
    arrange(desc(pct_zone_as_golf))

write_csv(zone_summary_z6,    ZONING_PCT_OUT)
write_csv(zone_penetration_z6, ZONE_PENETRATION_OUT)
cat(sprintf("[+] Zoning percentages saved  -> %s\n", basename(ZONING_PCT_OUT)))
cat(sprintf("[+] Zone penetration saved    -> %s\n", basename(ZONE_PENETRATION_OUT)))

cat("\n======================================================================\n")
cat("PHASE 5 COMPLETE\n")
cat("All outputs successfully saved to:\n")
cat(sprintf("  %s\n", OUTPUT_DIR))
cat("======================================================================\n")

record_provenance("Phase 5", "Phase_5.R", SCRIPT_DIR, PROV_START)
