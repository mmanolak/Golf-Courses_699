library(tidyverse)
setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/3 - Work/Cleaned Data")

raw_county <- read_csv("2022 USDA County Data - Ag Use.csv")
raw_state  <- read_csv("2025 USDA State Data - Ag Use.csv")


clean_usda_county <- raw_county %>%
  # 1. Standardize Column Names to snake_case
  rename_with(~ tolower(gsub(" ", "_", .x))) %>%
  
  mutate(
    # 2. Fix Geography
    # Convert State Name (ALABAMA) to Abbreviation (AL)
    state_abbr = state.abb[match(str_to_title(state), state.name)],
    
    # Fix County Name: "AUTAUGA" -> "Autauga County"
    # add " County" to the end to match your other datasets if needed
    county_clean = paste0(str_to_title(county), " County"),
    
    # 3. Fix Numbers
    value_numeric = as.numeric(gsub(",", "", value)),
    
    # 4. Simplify the "Data Item" Description
    # extract the key part: "INCL BUILDINGS", "CROPLAND", or "PASTURELAND"
    measure_type = case_when(
      str_detect(data_item, "INCL BUILDINGS") ~ "Total Ag Land (Incl Buildings)",
      str_detect(data_item, "CROPLAND") ~ "Cropland",
      str_detect(data_item, "PASTURELAND") ~ "Pastureland",
      TRUE ~ "Other"
    )
  ) %>%
  
  # 5. Select only what we need
  select(year, state_abbr, county_clean, measure_type, value_numeric)


clean_usda_state <- raw_state %>%
  rename_with(~ tolower(gsub(" ", "_", .x))) %>%
  mutate(
    state_abbr = state.abb[match(str_to_title(state), state.name)],
    value_numeric = as.numeric(gsub(",", "", value)),
    measure_type = case_when(
      str_detect(data_item, "INCL BUILDINGS") ~ "Total Ag Land (Incl Buildings)",
      str_detect(data_item, "CROPLAND") ~ "Cropland",
      str_detect(data_item, "PASTURELAND") ~ "Pastureland",
      TRUE ~ "Other"
    )
  ) %>%
  select(year, state_abbr, measure_type, value_numeric)

write_csv(clean_usda_county, "USDA_Ag_County_2022.csv")
write_csv(clean_usda_state, "USDA_Ag_State_2025.csv")

# View results
print(head(clean_usda_county))
print(head(clean_usda_state))