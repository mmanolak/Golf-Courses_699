# CONTROL PANEL
# Adjust the parameters in this section to control the script's behavior.

# 1. File Paths
INPUT_CSV_PATH  <- "Golf_Courses_Final_Master.csv"
# I've updated the output name to prevent overwriting the old, incomplete file.
OUTPUT_CSV_PATH <- "Golf_Courses_With_Acreage_OSM.csv"

# 2. Matching Logic
# The maximum distance (in meters) to accept a match.
DISTANCE_THRESHOLD_METERS <- 500

# SCRIPT EXECUTION
# You should not need to edit below this line.

# STEP 0: LOAD LIBRARIES
library(tidyverse)
library(sf)
library(osmdata) # The key package for querying OpenStreetMap

# STEP 1: LOAD YOUR MASTER DATA
if (!file.exists(INPUT_CSV_PATH)) {
  stop("Error: Input file not found at '", INPUT_CSV_PATH, "'. Please check the path.")
}
df_master <- read_csv(INPUT_CSV_PATH)

results_list <- list()
states_to_process <- unique(na.omit(df_master$state))

print(paste("Found", length(states_to_process), "states to process using OpenStreetMap data."))

# STEP 2: THE STATE-BY-STATE PROCESSING LOOP

for (st in states_to_process) {
  
  print(paste("Processing state:", st, "..."))
  
  # A. Filter points for the current state
  state_points <- df_master %>%
    filter(state == st) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # B. Get the bounding box for the entire state's points.
  state_bbox <- st_bbox(state_points)
  
  # C. Query OpenStreetMap for golf course polygons within the bounding box.
  osm_query_result <- tryCatch({
    opq(bbox = state_bbox) %>%
      add_osm_feature(key = "leisure", value = "golf_course") %>%
      osmdata_sf()
  }, error = function(e) {
    print(paste("  -> OSM query failed for", st, ". Skipping. Error:", e$message))
    return(NULL)
  })
  
  # If the query failed or returned nothing, skip to the next state.
  if (is.null(osm_query_result)) {
    next
  }
  
  # Extract just the polygons from the result.
  osm_polygons <- osm_query_result$osm_polygons
  
  # If no polygons were found in this state, add NAs and skip.
  if (is.null(osm_polygons) || nrow(osm_polygons) == 0) {
    print(paste("  -> No OSM golf course polygons found for", st))
    state_points$osm_name <- NA
    state_points$raw_acres <- NA
    state_points$dist_meters <- NA
    state_points$final_acres <- NA
    results_list[[st]] <- state_points %>% st_drop_geometry()
    next
  }
  
  # D. Prepare OSM Polygons for Matching
  # We add st_make_valid() to automatically repair geometric errors
  # like the "duplicate vertex" issue that caused the crash in Colorado.
  osm_polygons <- osm_polygons %>%
    st_make_valid() %>% 
    st_transform(crs = 4326) %>%
    mutate(
      calc_acres = as.numeric(st_area(geometry)) * 0.000247105
    )
  
  # E. Nearest Neighbor Matching
  nearest_idx <- st_nearest_feature(state_points, osm_polygons)
  dists <- st_distance(state_points, osm_polygons[nearest_idx,], by_element = TRUE)
  
  # F. Attach Data
  state_points <- state_points %>%
    mutate(
      osm_name = osm_polygons$name[nearest_idx], # OSM uses a 'name' column
      raw_acres = osm_polygons$calc_acres[nearest_idx],
      dist_meters = as.numeric(dists),
      final_acres = ifelse(dist_meters < DISTANCE_THRESHOLD_METERS, raw_acres, NA)
    )
  
  # G. Save to list
  results_list[[st]] <- state_points %>% st_drop_geometry()
  
  print(paste("  -> Successfully processed and matched", nrow(state_points), "points."))
}

# STEP 3: COMBINE AND SAVE
final_acreage_df <- bind_rows(results_list)
write_csv(final_acreage_df, OUTPUT_CSV_PATH)

print(paste("Done! File saved as '", OUTPUT_CSV_PATH, "'"))