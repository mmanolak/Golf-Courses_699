library(tidyverse)
setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/2 - Work")


# STEP 1: LOAD YOUR TWO SEPARATE RESULTS

df_tigris <- read_csv("Golf_Courses_With_Acreage_tigris.csv")
df_osm <- read_csv("Golf_Courses_With_Acreage_OSM.csv")


# STEP 2: PREPARE AND DE-DUPLICATE THE OSM DATA

osm_acres_only <- df_osm %>%
  select(course_name, latitude, longitude, final_acres) %>%
  rename(acres_osm = final_acres) %>%
  
  group_by(course_name, latitude, longitude) %>%
  summarise(acres_osm = sum(acres_osm, na.rm = TRUE), .groups = 'drop') %>%
  mutate(acres_osm = if_else(acres_osm == 0, NA_real_, acres_osm))


# STEP 3: COMBINE USING THE "COALESCE" STRATEGY

combined_data <- df_tigris %>%
  left_join(osm_acres_only, by = c("course_name", "latitude", "longitude")) %>%
  
  mutate(
    final_acreage = coalesce(final_acres, acres_osm),
    acreage_source = case_when(
      !is.na(final_acres) ~ "Tigris (High Reliability)",
      !is.na(acres_osm)   ~ "OSM (Medium Reliability)",
      TRUE                ~ "No Match Found"
    )
  )


# STEP 4: FINALIZE AND SAVE

final_master <- combined_data %>%
  select(
    course_name, 
    city, 
    state, 
    street_address, 
    zip_code, 
    holes, 
    course_type,
    final_acreage, 
    acreage_source, 
    longitude, 
    latitude
  )

# Save the final, clean master file
write_csv(final_master, "Golf_Courses_Acreage_Combo.csv", na = "")

print("Master file with corrected join saved.")
print(paste("Final row count:", nrow(final_master)))