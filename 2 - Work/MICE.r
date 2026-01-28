library(tidyverse)
library(mice)
library(VIM) # Used for advanced missing data visualization, good practice to load
setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/2 - Work")

# Load the prepared data from our previous step
imputation_data <- read_csv("Imputation_Preperation.csv")

# Ensure categorical variables are treated as factors (critical for MICE)
# This tells MICE to use appropriate methods for categorical predictors.
imputation_data <- imputation_data %>%
  mutate(
    course_type = as.factor(course_type),
    state = as.factor(state)
  )

# Step 1: Perform a "Dry Run" to Inspect the Model Setup
print("--- Performing MICE Dry Run ---")
# Perform a "dry run" with zero iterations to see the setup
init <- mice(imputation_data, maxit = 0)

# View the predictor matrix
# This shows which variables (columns) are used to predict which other variables (rows).
pred_matrix <- init$predictorMatrix
print("Predictor Matrix:")
print(pred_matrix)
# INTERPRETATION: The row for 'acreage' should have a '1' for every other
# variable, confirming they will all be used as predictors. The diagonal is 0
# because a variable cannot predict itself.

# Step 2: Run the Full MICE Algorithm
print("--- Running Full MICE Imputation (this may take a moment) ---")
# Set a random seed for reproducibility
set.seed(2026)

# Run the main MICE algorithm
# m = 5 creates 5 complete datasets
# method = 'pmm' (Predictive Mean Matching) is a robust default that ensures
# imputed values are plausible (i.e., they are real, observed values).
# maxit = 5 is the number of iterations for the algorithm to converge.
mice_output <- mice(imputation_data, m = 5, maxit = 5, method = 'pmm', seed = 2026)

print("--- MICE algorithm complete. ---")

# Step 3: Perform Critical Diagnostic Checks
print("--- Generating Diagnostic Plots ---")

# 3.1. Check for Convergence
png("convergence_plot.png", width = 800, height = 600)
plot(mice_output)
dev.off()
# INTERPRETATION: The plot shows the mean and variance of the imputed values
# at each iteration for each of the 5 imputed datasets. The lines should appear
# random and intermingled, without any strong upward or downward trend. This
# indicates the algorithm has stabilized (converged).

# 3.2. Compare Distributions of Observed vs. Imputed Data
png("density_plot.png", width = 800, height = 600)
densityplot(mice_output)
dev.off()
# INTERPRETATION: This plot is crucial. The blue line is the distribution of
# the original, observed acreage. The red lines are the distributions of the
# imputed values in each of the 5 datasets. For a good imputation, the red
# lines should have a similar shape and spread to the blue line.

print("Diagnostic plots 'convergence_plot.png' and 'density_plot.png' have been saved.")

# Step 4: Save the Imputation Output
# Save the entire MICE output object to a file. This allows us to load it
# later for analysis without re-running the time-consuming imputation.
saveRDS(mice_output, file = "MICE_Output_Object.rds")

print("MICE output object saved as 'MICE_Output_Object.rds'.")
print("--- Process Complete ---")
