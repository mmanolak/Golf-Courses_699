library(tidyverse)
library(readxl)
library(lubridate) 

setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/3 - Work/Cleaned Data")

# STEP 1: Read and Review Raw XLSX
# Replace with your actual file name
raw_hpi <- read_excel("1991 Seasonally Adjusted Pricing.xlsx")

# STEP 2: Clean and Name
hpi_clean <- raw_hpi %>%
  # 1. Rename the long columns to short, easy codes
  # Structure: new_name = old_name

  rename(
    region = 1,
    year = 2,
    quarter = 3,
    index_nsa = 4,
    index_sa = 5,
    pct_change_prev_q_nsa = 6,
    pct_change_prev_q_sa = 7,
    pct_change_prev_4q_nsa = 8,
    pct_change_prev_4q_sa = 9
  ) %>%
  
  # 2. Create a proper Date column
  # yq() takes a string like "1991.1" and turns it into a date
  mutate(
    date = yq(paste(year, quarter))
  ) %>%
  
  # 3. Reorder columns to put Date first (personal preference)
  select(date, region, year, quarter, everything())


# STEP 3: Save CSV
write_csv(hpi_clean, "HPI_Cleaned.csv")

# View the result
print(head(hpi_clean))

# STEP 4: Rinse Repeat above

target_file <- "Land Prices - June 20 2024.xlsx" 

df_counties <- read_excel(target_file, sheet = "Panel Counties", skip = 1) %>%
  rename(
    state_name = 1,
    county_name = 2,
    fips = 3,
    year = 4,
    land_val_std = 5,      # Standardized 1/4 Acre
    land_val_asis = 6,     # Per Acre As-Is
    land_share_prop = 7,   # Land Share of Property Value
    prop_val_std = 8,      # Property Value Standardized
    prop_val_asis = 9      # Property Value As-Is
  ) %>%
  mutate(fips = as.character(fips)) # Ensure FIPS is text, not math

df_zips <- read_excel(target_file, sheet = "Panel ZIP Codes", skip = 1) %>%
  rename(
    zip_code = 1,
    year = 2,
    land_val_std = 3,
    land_val_asis = 4,
    land_share_prop = 5,
    prop_val_std = 6,
    prop_val_asis = 7
  ) %>%
  mutate(zip_code = as.character(zip_code))

df_states <- read_excel(target_file, sheet = "Panel States", skip = 1) %>%
  rename(
    state_name = 1,
    state_abbr = 2,
    state_fips = 3,
    year = 4,
    land_val_std = 5,
    land_val_asis = 6,
    land_share_prop = 7,
    prop_val_std = 8,
    prop_val_asis = 9
  )

write_csv(df_counties, "Land_Prices_Counties.csv")
write_csv(df_zips, "Land_Prices_Zips.csv")
write_csv(df_states, "Land_Prices_States.csv")

print("Processing Complete. Files saved.")
