# Purpose: Shared run-provenance capture for the R master scripts (Roadmap Sec3.3).
#          Each phase script sources this file and calls record_provenance() once,
#          at the end of its run, to append one row to Run_Provenance_R.csv.
# Not part of the analysis pipeline itself - instrumentation only.

# P0-01 (2026-07-28): packages that matter for reproducibility but are loaded
# via requireNamespace() internally by another package's own dispatch (never
# attached, so they never appear in sessionInfo()$otherPkgs) -- e.g. `ranger`,
# Phase_3.R's actual random-forest imputation backend via mice::mice.impute.rf,
# invisible to key_packages until now. R has no whitelist mechanism analogous
# to provenance.py/provenance.jl's _KEY_PACKAGE_WHITELIST (key_packages here is
# a dynamic reflection, not a fixed list), so this plays the same role: a small,
# explicit, extensible list of namespace-only packages worth recording anyway.
.EXTRA_KEY_PACKAGES <- c("ranger")

record_provenance <- function(phase, script, script_dir, start_time,
                               M = NA, maxit = NA, n_workers = NA, seed = NA,
                               key_packages = NULL) {
  tryCatch({
    repo_dir <- normalizePath(script_dir, mustWork = FALSE)
    csv_path <- normalizePath(file.path(script_dir, "..", "Run_Provenance_R.csv"), mustWork = FALSE)

    git_sha <- tryCatch({
      out <- suppressWarnings(system2("git", c("-C", shQuote(repo_dir), "rev-parse", "HEAD"),
                                       stdout = TRUE, stderr = FALSE))
      if (length(out) == 1 && !grepl("fatal", out)) out else NA_character_
    }, error = function(e) NA_character_)

    git_dirty <- tryCatch({
      out <- suppressWarnings(system2("git", c("-C", shQuote(repo_dir), "status", "--porcelain"),
                                       stdout = TRUE, stderr = FALSE))
      length(out) > 0
    }, error = function(e) NA)

    if (is.null(key_packages)) {
      pkgs <- tryCatch(utils::sessionInfo()$otherPkgs, error = function(e) NULL)
      attached_parts <- if (length(pkgs) > 0) {
        sprintf("%s=%s", names(pkgs), vapply(pkgs, function(p) p$Version, character(1)))
      } else {
        character(0)
      }
      extra_parts <- vapply(.EXTRA_KEY_PACKAGES, function(p) {
        if (requireNamespace(p, quietly = TRUE)) {
          sprintf("%s=%s", p, as.character(utils::packageVersion(p)))
        } else {
          NA_character_
        }
      }, character(1))
      extra_parts <- extra_parts[!is.na(extra_parts)]
      all_parts <- c(attached_parts, extra_parts)
      key_packages <- if (length(all_parts) > 0) paste(all_parts, collapse = ";") else NA_character_
    }

    row <- data.frame(
      timestamp_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
      phase         = phase,
      language      = "R",
      script        = script,
      git_sha       = git_sha,
      git_dirty     = git_dirty,
      M             = M,
      maxit         = maxit,
      n_workers     = n_workers,
      seed          = seed,
      wall_time_sec = as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      host          = Sys.info()[["nodename"]],
      os            = paste(Sys.info()[["sysname"]], Sys.info()[["release"]]),
      lang_version  = R.version.string,
      key_packages  = key_packages,
      stringsAsFactors = FALSE
    )

    write.table(row, csv_path, sep = ",", row.names = FALSE,
                col.names = !file.exists(csv_path), append = file.exists(csv_path))
  }, error = function(e) {
    warning(sprintf("[provenance] recording failed, continuing run: %s", conditionMessage(e)))
  })
  invisible(NULL)
}
