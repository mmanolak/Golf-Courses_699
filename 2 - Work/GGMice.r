library(tidyverse)
library(mice)
library(ggmice)
library(patchwork)

setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)/2 - Work")

convergence_plot_colors <- c(
  "1" = "#ff0004ff", # Red
  "2" = "#007de4ff", # Blue
  "3" = "#138a0fff", # Green
  "4" = "#730085ff", # Purple
  "5" = "#eb7500ff"  # Orange
)

# Step 1: Load the MICE Object
mice_output <- readRDS("MICE_Output_Object.rds")

# Step 2a: Extract and Tidy the Mean Data
mean_data_long <- mice_output$chainMean["acreage", , ] %>%
  t() %>%
  as.data.frame() %>%
  mutate(Iteration = row_number()) %>%
  pivot_longer(
    cols = -Iteration,
    names_to = "Imputation",
    values_to = "mean",
    names_prefix = "V"
  )

# Step 2b: Extract and Tidy the Standard Deviation Data
sd_data_long <- mice_output$chainVar["acreage", , ] %>%
  sqrt() %>%
  t() %>%
  as.data.frame() %>%
  mutate(Iteration = row_number()) %>%
  pivot_longer(
    cols = -Iteration,
    names_to = "Imputation",
    values_to = "sd",
    names_prefix = "V"
  )

# Step 2c: Join the Tidy Datasets
convergence_data <- left_join(mean_data_long, sd_data_long, by = c("Iteration", "Imputation"))

# Step 2d: Create the Mean Convergence Plot
plot_mean_convergence <- ggplot(convergence_data, aes(x = Iteration, y = mean, color = Imputation, group = Imputation)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = convergence_plot_colors) + # This line sets the colors
  labs(
    title = "MICE Convergence: Mean of Imputed Acreage",
    subtitle = "The mean of imputed values for each chain across iterations",
    x = "Iteration Number",
    y = "Mean of Acreage"
  ) +
  theme_bw(base_size = 14)

# Step 2e: Create the Standard Deviation Convergence Plot
plot_sd_convergence <- ggplot(convergence_data, aes(x = Iteration, y = sd, color = Imputation, group = Imputation)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = convergence_plot_colors) + # This line sets the colors
  labs(
    title = "MICE Convergence: Standard Deviation of Imputed Acreage",
    subtitle = "The SD of imputed values for each chain across iterations",
    x = "Iteration Number",
    y = "Standard Deviation of Acreage"
  ) +
  theme_bw(base_size = 14)

# Step 3: Prepare Data for the Density Plot
plot_data <- mice::complete(mice_output, action = "long", include = TRUE)

# Step 4: Create the Density Plot
plot_density <- ggplot(plot_data, aes(x = acreage, group = .imp, color = (.imp > 0))) +
  geom_density(linewidth = 1.1) +
  scale_color_manual(
    name = "Data Type",
    values = c("FALSE" = "#0205c7ff", "TRUE" = "#ff0000ff"),
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

# Step 5: Print and Save the Three Separate Plots
print(plot_mean_convergence)
print(plot_sd_convergence)
print(plot_density)

ggsave("plot_mean_convergence.png", plot_mean_convergence, width = 12, height = 7, dpi = 300)
ggsave("plot_sd_convergence.png", plot_sd_convergence, width = 12, height = 7, dpi = 300)
ggsave("plot_density.png", plot_density, width = 16, height = 8, dpi = 300)

print("Diagnostic plots saved as 'plot_mean_convergence.png', 'plot_sd_convergence.png', and 'plot_density.png'")