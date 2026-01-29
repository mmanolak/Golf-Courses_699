# List of packages to install and verify
packages <- c("tidyverse", "wooldridge", "plotly", "sf", "leaflet", "tidycensus", "readxl", "osmdata", "tigris", "mice", "VIM", "patchwork", "ggmice")

setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)")
missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]

#Output the result for packages
if (length(missing_packages) > 0) {
  print("The following packages are missing:")
  print(missing_packages)
  stop("Required packages are missing. Please install them and try again.")
} else {
  print("There are no missing packages.")
}

