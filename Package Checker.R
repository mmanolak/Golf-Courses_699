library(tidyverse)

setwd('G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/3 - Work/Cleaned Data')

# STEP 1: sample data
# Replace with: raw_data <- read_csv("your_file.csv", col_names = FALSE)
raw_data <- tribble(
  ~X1, ~X2, ~X3, ~X4,
  -151.820102, 59.773222, "Fireweed Meadows Golf Course-Anchor Point,AK", "(Public) (9 Holes), Mylofritz Ave, Anchor Point,AK 99556, (907) 226-2582",
  -149.814495, 61.127089, "Anchorage Golf Course-Anchorage,AK", "(Public) (18 Holes), 3651 Omalley Rd, Anchorage,AK 99507-4266, (907) 522-3363",
  -149.788303, 61.206435, "Russian Jack Springs Golf Course-Anchorage,AK", "(Municipal) (9 Holes), 1600 Lidia Selkregg Ln, Anchorage,AK 99508-3131, (907) 343-6992"
)

# STEP 2: CLEANING PIPELINE

clean_data <- raw_data %>%
  # 1. Rename coordinates
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
    street_address = str_extract(raw_details, "(?<=\\), ).+?(?=,)")
  ) %>%
  
  # 4. Final Selection & Ordering
  select(course_name, street_address, city, state, zip_code, course_type, holes, phone, longitude, latitude)


print(clean_data)
