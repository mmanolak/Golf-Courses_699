# List of packages to install and verify
packages <- c("tidyverse", "wooldridge", "plotly", "sf", "leaflet", "tidycensus", "readxl", "osmdata", "tigris", "mice", "VIM")

setwd("G:/Shared drives/School Stuff/Old Sessions/9 - Spring 2026/02 - Econ 699 (Golf Course)")
# Function to check and install missing packages
install_and_verify <- function(pkg_list) {
  # Install missing packages
  for (pkg in pkg_list) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
  }
  
  # Verify installation
  installed_status <- sapply(pkg_list, requireNamespace, quietly = TRUE)
  
  # Report results
  if (all(installed_status)) {
    message("All packages are properly installed and ready to use!")
  } else {
    message("The following packages failed to install or load properly:")
    print(names(installed_status[!installed_status]))
  }
}

# Run the installation and verification process
install_and_verify(packages)

# Optionally, load all installed packages (uncomment if needed)
# lapply(packages, library, character.only = TRUE)