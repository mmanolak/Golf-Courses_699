library(tidyverse)
library(tigris)
library(sf)

# Enable caching so if you re-run, it doesn't re-download files
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

# -------------------------------------------------------------------------
# STEP 1: LOAD YOUR MASTER DATA
# -------------------------------------------------------------------------
# Replace with your actual master file
df_master <- read_csv("Golf_Courses_Final_Master.csv")

# Create an empty list to store results
results_list <- list()

# Get list of unique states in your data (e.g., "AL", "AK", "TX")
# We filter out NAs just in case
states_to_process <- unique(na.omit(df_master$state))

print(paste("Found", length(states_to_process), "states to process."))

# -------------------------------------------------------------------------
# STEP 2: THE STATE LOOP
# -------------------------------------------------------------------------

for (st in states_to_process) {
  
  print(paste("Processing state:", st, "..."))
  
  # A. Filter your points for just this state
  state_points <- df_master %>%
    filter(state == st) %>%
    # Convert to spatial object (using Long/Lat)
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE)
  
  # B. Download Census Landmarks for this state
  # We use tryCatch to skip the state if the download fails or state code is invalid
  census_shapes <- tryCatch({
    landmarks(st, type = "area", progress_bar = FALSE)
  }, error = function(e) return(NULL))
  
  # If no shapes found (or download failed), skip to next state
  if (is.null(census_shapes)) {
    print(paste("  -> No census data found for", st, "- Skipping."))
    next
  }
  
  # C. Filter Census shapes for Golf Courses
  golf_shapes <- census_shapes %>%
    filter(str_detect(FULLNAME, "(?i)Golf|Country Club")) %>%
    st_transform(crs = 4326)
  
  # If the state has 0 golf courses listed in Census, skip
  if (nrow(golf_shapes) == 0) {
    print(paste("  -> No golf courses found in Census data for", st))
    # We still want to keep the original points, just with NA acres
    state_points$census_name <- NA
    state_points$census_acres <- NA
    state_points$dist_meters <- NA
    results_list[[st]] <- state_points %>% st_drop_geometry()
    next
  }
  
  # D. Calculate Acreage for the shapes
  golf_shapes <- golf_shapes %>%
    mutate(
      calc_acres = as.numeric(st_area(geometry)) * 0.000247105
    )
  
  # E. Nearest Neighbor Matching
  # Find nearest shape for every point
  nearest_idx <- st_nearest_feature(state_points, golf_shapes)
  
  # Calculate distance to that nearest shape
  dists <- st_distance(state_points, golf_shapes[nearest_idx,], by_element = TRUE)
  
  # F. Attach Data
  state_points <- state_points %>%
    mutate(
      census_name = golf_shapes$FULLNAME[nearest_idx],
      raw_acres = golf_shapes$calc_acres[nearest_idx],
      dist_meters = as.numeric(dists),
      
      # THE LOGIC: Only accept if within 500 meters (approx 0.3 miles)
      final_acres = ifelse(dist_meters < 500, raw_acres, NA)
    )
  
  # G. Save to list (dropping the geometry column to keep it a clean CSV)
  results_list[[st]] <- state_points %>% st_drop_geometry()
}

# -------------------------------------------------------------------------
# STEP 3: COMBINE AND SAVE
# -------------------------------------------------------------------------

# Combine all state results into one big table
final_acreage_df <- bind_rows(results_list)

# Save
write_csv(final_acreage_df, "Golf_Courses_With_Acreage.csv")

print("Done! File saved as 'Golf_Courses_With_Acreage.csv'")
