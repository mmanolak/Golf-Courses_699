# List of packages to install and verify
packages <- c("tidyverse", "wooldridge", "plotly", "sf", "leaflet", "tidycensus")


missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
missing_packages

#windowsOS checker for dependent directory
if (Sys.info()['sysname'] == "Windows") {
  if (Sys.info()['nodename'] == "DegreeLaptop" || Sys.getenv('USERNAME') == "Degree Laptop") { 
    setwd("C:/Users/Degree Laptop/Desktop/Spring 2025/3 - Econ 256 (Data Vis)/exercise1")} 
  else if (Sys.info()['nodename'] == "Michael" || Sys.getenv('USERNAME') == "Michael") { 
    setwd("C:/Users/Michael/Desktop/Spring 2025/3 - Econ 256/R Studio Files/Econ256/exercise1")}
  #LinuxOS Checker
} else if (Sys.info()['sysname'] == "Linux") {
  setwd("~/Desktop/R Files/Econ256")
  #macOS check
} else if (Sys.info()['sysname'] == "Darwin") {
  setwd("~/Desktop/Spring 2025/3 - Econ 256")}

#Output the result for packages
if (length(missing_packages) > 0) {
  print("The following packages are missing:")
  print(missing_packages)
  stop("Required packages are missing. Please install them and try again.")
} else {
  print("There are no missing packages.")
}

osdisplay<-switch(Sys.info()[['sysname']],
                  Windows= {print("I'm a PC.")},
                  Linux  = {print("I'm a penguin.")},
                  Darwin = {print("I'm a Mac.")})
