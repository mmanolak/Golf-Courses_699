library(tidyverse)
setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/2 - Work")


# Step 1: Load Required Libraries and Data


# Load the master golf course file from our last step
# NOTE: The user prompt mentioned "Combo", but our final file was "MASTER".
# I am using the final, de-duplicated master file.
golf_master <- read_csv("Golf_Courses_Acreage_Combo.csv")

# Load the cleaned, state-level land price data
land_prices_state <- read_csv("Land_Prices_States.csv")


# Step 2: Select Core Variables for Imputation

# We create a unique ID, rename acreage for simplicity, and select predictors.
imputation_data <- golf_master %>%
  mutate(course_id = row_number()) %>%
  rename(acreage = final_acreage) %>%
  select(
    course_id,
    acreage,
    holes,
    course_type,
    longitude,
    latitude,
    state
  )


# Step 3: Prepare and Enrich with Economic Data


# The land price data is a panel (multiple years per state). We need a cross-section.
# We will calculate the average standardized land value for each state across all years.
state_avg_land_value <- land_prices_state %>%
  group_by(state_abbr) %>%
  summarise(state_land_value = mean(land_val_std, na.rm = TRUE)) %>%
  ungroup()

# Now, perform the left join to add the economic predictor
imputation_data <- imputation_data %>%
  left_join(state_avg_land_value, by = c("state" = "state_abbr"))


# Step 4: Final Data Cleaning and Type Conversion


imputation_data <- imputation_data %>%
  # Convert character columns for modeling into factors
  mutate(
    course_type = as.factor(course_type),
    state = as.factor(state),
    holes = as.numeric(str_remove(holes, " Holes"))
  )

#  Verification Checks 

# A) Inspect the data frame structure to confirm types
print("--- Data Structure Verification ---")
str(imputation_data)

# B) Check for missing values in PREDICTOR variables
# We expect NAs in 'acreage', but not anywhere else.
print("--- Missing Predictor Value Check ---")
predictor_na_counts <- colSums(is.na(select(imputation_data, -acreage)))
print(predictor_na_counts)

# C) If any predictors have NAs, remove those rows.
# This is crucial for the imputation model to run.
# The `-acreage` ensures we don't drop the very rows we want to impute.
imputation_data <- imputation_data %>%
  drop_na(-acreage)


# Step 5: Save the Prepared Data

write_csv(imputation_data, "Imputation_Preperation.csv")

print(paste0(
  "Process complete. Final dataset has ", nrow(imputation_data), 
  " rows and is saved as 'Imputation_Preperation.csv'"
))