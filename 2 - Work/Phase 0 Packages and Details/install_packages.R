# === 1. GLOBALS & PATHS ===

# P0-01 (2026-07-28): locate this script's own directory without depending on
# any package (mirrors the bootstrap every Phase_1..6.R master script runs in
# its own LIBRARIES section) so the project root and renv.lock can be found
# even on a machine with nothing installed yet, including renv itself.
SCRIPT_DIR <- local({
    cmd_args <- commandArgs(trailingOnly = FALSE)
    m <- grep("^--file=", cmd_args)
    if (length(m) == 0) return(getwd())
    dirname(normalizePath(sub("^--file=", "", cmd_args[m])))
})
PROJ_DIR   <- dirname(SCRIPT_DIR)
RENV_LOCK  <- file.path(PROJ_DIR, "renv.lock")
ACTIVATE_R <- file.path(PROJ_DIR, "renv", "activate.R")

# Fallback package list -- used only when renv.lock is absent (see EXECUTION).
# When renv.lock is present, renv::restore() installs exactly what it pins,
# including packages invoked only via another package's internal dispatch
# (e.g. `ranger`, mice's real random-forest backend -- see P0-01) that no
# hand-maintained list like this one can ever fully capture.
PACKAGES <- c(
    # Phase 1 Parsing, Phase 2 Spatial Polygons, Phase 5 Hawaii Micro-Case Study
    "tidyverse", "wooldridge", "sf", "tigris", "readxl",
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

# P0-01 (2026-07-28): restore the pinned environment from renv.lock when one
# exists, rather than resolving whatever's currently newest on CRAN for any
# package this script decides is missing -- the old behaviour silently
# defeated X-08's fix on a fresh machine. Falls back to the pre-existing
# ad hoc install-what's-missing behaviour when no renv.lock is present yet.
if (file.exists(RENV_LOCK) && file.exists(ACTIVATE_R)) {
    cat(sprintf("Found renv.lock at %s -- restoring pinned environment...\n", RENV_LOCK))
    Sys.setenv(RENV_PROJECT = PROJ_DIR)
    source(ACTIVATE_R)
    renv::restore(project = PROJ_DIR, prompt = FALSE)
    cat("\nrenv::restore() complete -- environment matches renv.lock.\n")
} else {
    cat("No renv.lock found -- falling back to ad hoc installation of the package list below.\n")
    cat("(This does not pin versions -- run renv::init() + renv::snapshot() once installed to fix that.)\n")
    install_and_verify(PACKAGES)
}