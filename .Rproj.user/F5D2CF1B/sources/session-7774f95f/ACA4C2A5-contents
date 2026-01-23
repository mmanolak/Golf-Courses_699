# List of packages to install and verify
packages <- c("tidyverse", "wooldridge", "plotly", "sf", "leaflet", "tidycensus")


missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]


#Output the result for packages
if (length(missing_packages) > 0) {
  print("The following packages are missing:")
  print(missing_packages)
  stop("Required packages are missing. Please install them and try again.")
} else {
  print("There are no missing packages.")
}