library(tidyverse)
library(tigris)
library(sf)
setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/2 - Work")

# CONTROL PANEL
# Adjust the parameters in this section to control the script's behavior.

# 1. File Paths
INPUT_CSV_PATH  <- "Golf_Courses_Final_Master.csv"
OUTPUT_CSV_PATH <- "Golf_Courses_With_Acreage_tigris.csv"

# 2. Matching Logic
# The maximum distance (in meters) to accept a match. If the nearest polygon
# is further away than this, the acreage will be NA.
DISTANCE_THRESHOLD_METERS <- 500

# The search pattern used to find golf courses in the Census data.
# The `|` symbol means OR. `(?i)` makes it case-insensitive.
CENSUS_SEARCH_PATTERN <- "(?i)Golf Course|Golf Club|Country Club"

# SCRIPT EXECUTION
# You should not need to edit below this line.

# Configuration
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# STEP 1: LOAD YOUR MASTER DATA
if (!file.exists(INPUT_CSV_PATH)) {
  stop("Error: Input file not found at '", INPUT_CSV_PATH, "'. Please check the path in the Control Panel.")
}
df_master <- read_csv(INPUT_CSV_PATH)

results_list <- list()
states_to_process <- unique(na.omit(df_master$state))

print(paste("Found", length(states_to_process), "states to process."))
print(paste("Distance Threshold set to:", DISTANCE_THRESHOLD_METERS, "meters."))

# STEP 2: THE STATE-BY-STATE PROCESSING LOOP

for (st in states_to_process) {
  
  print(paste("Processing state:", st, "..."))
  
  # A. Filter points and convert to spatial object
  state_points <- df_master %>%
    filter(state == st) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # B. Download Census Landmarks for this state
  census_shapes <- tryCatch({
    landmarks(st, type = "area", progress_bar = FALSE)
  }, error = function(e) return(NULL))
  
  if (is.null(census_shapes)) {
    print(paste("  -> No census data found for", st, "- Skipping."))
    next
  }
  
  # C. Filter Census shapes for Golf Courses using the pattern from the Control Panel
  golf_shapes <- census_shapes %>%
    filter(str_detect(FULLNAME, CENSUS_SEARCH_PATTERN)) %>%
    st_transform(crs = 4326)
  
  # If the state has 0 valid golf course polygons, add NAs and skip
  if (nrow(golf_shapes) == 0) {
    print(paste("  -> No matching polygons found in Census data for", st))
    state_points$census_name <- NA
    state_points$raw_acres <- NA
    state_points$dist_meters <- NA
    state_points$final_acres <- NA
    results_list[[st]] <- state_points %>% st_drop_geometry()
    next
  }
  
  # D. Calculate Acreage for the shapes
  golf_shapes <- golf_shapes %>%
    mutate(
      calc_acres = as.numeric(st_area(geometry)) * 0.000247105
    )
  
  # E. Nearest Neighbor Matching
  nearest_idx <- st_nearest_feature(state_points, golf_shapes)
  dists <- st_distance(state_points, golf_shapes[nearest_idx,], by_element = TRUE)
  
  # F. Attach Data
  state_points <- state_points %>%
    mutate(
      census_name = golf_shapes$FULLNAME[nearest_idx],
      raw_acres = golf_shapes$calc_acres[nearest_idx],
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