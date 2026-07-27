# === 1. GLOBALS & PATHS ===

# All packages required across Phases 1-5
PACKAGES <- c(
    # Phase 1 Parsing, Phase 2 Spatial Polygons, Phase 5 Hawaii Micro-Case Study
    "tidyverse", "wooldridge", "sf", "tigris",
    "future", "furrr", "parallelly", "this.path",
    # Phase 3 Economic Merge and MICE Imputation
    "mice", "VIM", "patchwork", "ggmice",
    # Phase 4 Econometric Modeling
    "lmtest", "sandwich", "broom","fixest", "estimatr", "plm", "marginaleffects", "modelsummary",
    # Phase 6 Images and Graphs
    "ggspatial", "kableExtra", "xtable", "ggdist", "biscale", "scales", "cowplot", "knitr"
)


# === 2. FUNCTIONS ===

# Check which packages from a list are not yet installed.
#
# @param pkg_list Character vector of package names.
# @return Character vector of package names that are missing.
find_missing <- function(pkg_list) {
    pkg_list[!sapply(pkg_list, requireNamespace, quietly = TRUE)]
}

# Report installation status for every package in a list.
#
# @param pkg_list Character vector of package names to check.
# @return Invisibly returns a named logical vector (TRUE = installed).
report_status <- function(pkg_list) {
    cat("Checking installed R packages...\n")
    status <- sapply(pkg_list, requireNamespace, quietly = TRUE)
    for (pkg in pkg_list) {
        if (status[[pkg]]) {
            cat(sprintf("  %s is already installed\n", pkg))
        } else {
            cat(sprintf("  %s - MISSING\n", pkg))
        }
    }
    invisible(status)
}

# Install missing packages, then verify the full list loaded correctly.
#
# @param pkg_list Character vector of all required package names.
# @return Invisibly returns NULL. Prints a final pass/fail summary.
install_and_verify <- function(pkg_list) {
    report_status(pkg_list)

    missing_pkgs <- find_missing(pkg_list)

    if (length(missing_pkgs) == 0) {
        cat("\nNo missing packages xD\n")
        return(invisible(NULL))
    }

    cat(sprintf("\nFound %d missing package(s). Installing...\n", length(missing_pkgs)))

    for (pkg in missing_pkgs) {
        tryCatch(
            {
                install.packages(pkg, quiet = TRUE)
                cat(sprintf("  Successfully installed: %s\n", pkg))
            },
            error = function(e) {
                cat(sprintf("  Failed to install: %s (%s)\n", pkg, conditionMessage(e)))
            }
        )
    }

    # Final verification pass after installation attempts
    cat("\nVerifying installation...\n")
    still_missing <- find_missing(pkg_list)

    if (length(still_missing) > 0) {
        cat("\nThe following packages failed to install or load:\n")
        cat(paste0("  - ", still_missing, collapse = "\n"), "\n")
    } else {
        cat("\nAll packages are now properly installed and ready to use!\n")
    }

    invisible(NULL)
}


# === 3. EXECUTION ===

install_and_verify(PACKAGES)