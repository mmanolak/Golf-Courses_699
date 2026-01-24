library(tidyverse)
setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/3 - Work/Cleaned Data")


raw_loans <- read_csv("FHFA Loan Limits.csv")

clean_loans <- raw_loans %>%
  rename(
    cbsa_code = 1,          # CBSA Code
    cbsa_name = 2,          # CBSA Name
    county_fips = 3,        # County FIPS
    county = 4,             # County Name
    loan_limit_raw = 5,     # LL Amount
    state_full = 8,         # State Name
    latitude = 12,          # Latitude (generated)
    longitude = 13          # Longitude (generated)
  ) %>%
mutate(
    # Create State Abbreviation (to match Golf Data's "AK", "AL", etc.)
    state = state.abb[match(state_full, state.name)],
    
    # Manual fix for DC (since standard R lists often exclude it)
    state = ifelse(state_full == "District of Columbia", "DC", state),
    
    # Clean the Money Column
    loan_limit = as.numeric(gsub("[$,]", "", loan_limit_raw))
  ) %>%
  
  # Final Selection & Ordering
  select(cbsa_code, cbsa_name, county, state, loan_limit, longitude, latitude)


write_csv(clean_loans, "Loan_Limits_Cleaned.csv")

# View to confirm it looks like the Golf Data structure
print(head(clean_loans))