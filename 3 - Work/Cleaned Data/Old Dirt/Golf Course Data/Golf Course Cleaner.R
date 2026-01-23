library(tidyverse)


# Stage 1: Staging and Importing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Set the working directory
setwd('G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/3 - Work/Cleaned Data')

# CSV Summoning
raw_data <- read_csv("Golf Courses-USA.csv", col_names = FALSE, show_col_types = FALSE)

# Stage 2: The Golf Pipeline ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clean_data <- raw_data %>%
  # 1. Rename coordinates (Assumes columns are in the order: Long, Lat, Name-Loc, Details)
  rename(
    longitude = X1,
    latitude = X2,
    raw_name_loc = X3,
    raw_details = X4
  ) %>%
  
  # 2. Split Name, City, State from Column C
  separate(raw_name_loc, into = c("course_name", "location_temp"), sep = "-(?=[^-]+$)", extra = "merge") %>%
  separate(location_temp, into = c("city", "state"), sep = ",") %>%
  
  # 3. Extract Details from Column D
  mutate(
    # Type: Text inside first parens
    course_type = str_extract(raw_details, "^\\(.*?\\)"),
    course_type = str_remove_all(course_type, "[()]"),
    
    # Holes: Text inside second parens
    holes = str_extract(raw_details, "(?<=\\)\\s)\\((.*?)\\)"),
    holes = str_remove_all(holes, "[()]"),
    
    # Phone: Pattern at end of string
    phone = str_extract(raw_details, "\\(?\\d{3}\\)?[-. ]?\\d{3}[-. ]?\\d{4}$"),
    
    # Zip Code: Look for 5 digits, optional dash, 4 digits
    zip_code = str_extract(raw_details, "\\b\\d{5}(?:-\\d{4})?\\b"),
    
    # Street Address: 
    # Logic: Look for text AFTER the "), " and BEFORE the next comma ","
    street_address = str_extract(raw_details, "(?<=\\), ).+?(?=,)")
  ) %>%
  
  # 4. Final Selection & Ordering
  select(course_name, street_address, city, state, zip_code, course_type, holes, phone, longitude, latitude)


# Stage 3: Results ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check the first few rows to ensure it worked
print(head(clean_data))

# Save the cleaned data to a new file in the same folder
write_csv(clean_data, "Golf_Courses_Cleaned.csv")

print("Cleaning complete. File saved as 'Golf_Courses_Cleaned.csv'")


# Bottom Text Information // Extra Garbage

# For editing the Missing Data
#na_rows <- clean_data %>% 
#  filter(if_any(everything(), is.na))
#write_csv(na_rows, "Golf_Courses_For_Editing.csv", na = "")

# We now Merger it back in
original_full <- read_csv("Golf_Courses_Cleaned.csv")
fixed_subset <- read_csv("Golf_Courses_For_Editing.csv")

# 1. Create a "Good Only" set from the original
# We remove ANY row that has an NA. These are the rows you didn't need to touch.
original_good_only <- original_full %>%
  drop_na()

final_dataset <- bind_rows(original_good_only, fixed_subset)

print(paste("Remaining NAs:", sum(is.na(final_dataset))))

print(paste("Original Row Count:", nrow(original_full)))
print(paste("Final Row Count:   ", nrow(final_dataset)))

write_csv(final_dataset, "Golf_Courses_Final_Master.csv")
