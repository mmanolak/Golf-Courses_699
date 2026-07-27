# Purpose: Shared run-provenance capture for the R master scripts (Roadmap Sec3.3).
#          Each phase script sources this file and calls record_provenance() once,
#          at the end of its run, to append one row to Run_Provenance_R.csv.
# Not part of the analysis pipeline itself - instrumentation only.

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
      key_packages <- if (length(pkgs) > 0) {
        paste(sprintf("%s=%s", names(pkgs), vapply(pkgs, function(p) p$Version, character(1))),
              collapse = ";")
      } else {
        NA_character_
      }
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
