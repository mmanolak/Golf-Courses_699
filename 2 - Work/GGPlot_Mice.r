library(tidyverse)
library(mice)
library(ggmice)    # Provides plot_trace() and enhances ggplot
library(patchwork) # For combining plots

setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/2 - Work")

# Step 1: Load the MICE Object
mice_output <- readRDS("MICE_Output_Object.rds")

# Step 2: Create the Convergence Plot (This function is correct and works directly on the mice object)
plot_convergence <- plot_trace(mice_output, "acreage") +
  labs(
    title = "MICE Convergence Diagnostics",
    subtitle = "Mean and Standard Deviation of imputed values across iterations",
    x = "Iteration Number",
    y = "Value"
  ) +
  theme_bw(base_size = 14)

# Step 3: Prepare Data for the Density Plot (THE CRITICAL FIX)
plot_data <- mice::complete(mice_output, action = "long", include = TRUE)

# Step 4: Create the Density Plot using the Prepared Data
# Now we pass the tidy 'plot_data' data frame to ggplot, not the 'mice_output' object.
plot_density <- ggplot(plot_data, aes(x = acreage, group = .imp, color = (.imp > 0))) +
  geom_density(linewidth = 1.1) +
  scale_color_manual(
    name = "Data Type",
    values = c("FALSE" = "#0205c7ff", "TRUE" = "#ff0000ff"), # Blue for Observed, Orange/Red for Imputed
    labels = c("Observed", "Imputed")
  ) +
  labs(
    title = "Distribution of Observed vs. Imputed Acreage",
    subtitle = "Imputed distributions should match the observed distribution",
    x = "Acreage",
    y = "Density"
  ) +
  theme_bw(base_size = 14) +
  theme(legend.position = "bottom")

# Step 5: Combine and Save the Plots

print(plot_convergence)
print(plot_density)

ggsave("plot_convergence.png", plot_convergence, width = 16, height = 8, dpi = 300)

# Save the combined plot
ggsave("plot_density.png", plot_density, width = 16, height = 8, dpi = 300)

print("Combined diagnostic plot saved as 'MICE_diagnostics_ggmice.png'")