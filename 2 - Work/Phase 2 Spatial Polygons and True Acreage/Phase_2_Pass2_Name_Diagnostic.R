# Purpose: Read-only re-verification of Issue_Register.md P2-05 (Pass-2 nearest-
#          feature fallback silently mis-assigning polygons) against the fresh
#          strix Phase 2 output, prompted by the Charlotte Golf Links / Providence
#          Country Club case found while diagnosing P2-06. P2-05's original numbers
#          were computed on a pre-migration run whose Pass-1 count (5,458) matches
#          Julia's fresh count, not R's (5,463) -- this checks whether the closed
#          conclusion still holds on the data actually feeding this run's cascade.
#
# Does NOT touch or re-run Phase_2.R, does NOT change any matching logic, does NOT
# write into Phase 2's own Data/ output. Reproduces Pass 1 (st_intersects) + Pass 2
# (nearest, <= 500 m, largest-area tie-break) read-only, exactly as Phase_2.R
# implements it, but additionally retains which polygon and which pass matched
# each course -- information the production pipeline computes internally but does
# not persist to R_Phase2_Acreage_Matched_v2.csv.
#
# Name-match method: lowercase both Course_Name and the matched polygon's OSM
# `name`, strip punctuation (apostrophes, periods, commas, &->and, hyphens->space),
# split on whitespace into tokens, Jaccard = |intersection| / |union|. Generic
# words ("golf", "club", "country", "course") are NOT stripped -- this is a
# straightforward reimplementation matching P5-14/P2-05's described method, not a
# byte-identical rerun of the original one-off diagnostic script, which was never
# committed to the repo.
#
# Reads:  Phase 2 Spatial Polygons and True Acreage/Data/R/R_Phase2_OSM_Golf_Polygons.gpkg
#         Phase 1 Parsing/Data/R/R_Phase1_Baseline_Golf_Valuation.csv
#         Phase 2 Spatial Polygons and True Acreage/Data/R/R_Phase2_Acreage_Matched_v2.csv
# Writes: Data/R/R_Pass2_Name_Diagnostic.csv (this script's own output dir; not
#         read by any master script)

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(this.path)
})
sf_use_s2(FALSE)

SCRIPT_DIR <- this.path::this.dir()
MAX_NEAREST_M <- 500

POLY_GPKG <- file.path(SCRIPT_DIR, "Data", "R", "R_Phase2_OSM_Golf_Polygons.gpkg")
PHASE1_CSV <- file.path(SCRIPT_DIR, "..", "Phase 1 Parsing", "Data", "R",
                         "R_Phase1_Baseline_Golf_Valuation.csv")
PRODUCTION_CSV <- file.path(SCRIPT_DIR, "Data", "R", "R_Phase2_Acreage_Matched_v2.csv")
OUT_DIR <- file.path(SCRIPT_DIR, "Data", "R")
OUT_CSV <- file.path(OUT_DIR, "R_Pass2_Name_Diagnostic.csv")

# === Load ===
poly <- st_read(POLY_GPKG, quiet = TRUE)
cat(sprintf("Loaded %d OSM polygons.\n", nrow(poly)))

courses <- read.csv(PHASE1_CSV, stringsAsFactors = FALSE) |>
  filter(!is.na(Longitude), !is.na(Latitude))
cat(sprintf("Loaded %d Phase 1 courses.\n", nrow(courses)))

pts <- st_as_sf(courses, coords = c("Longitude", "Latitude"), crs = 4326) |>
  st_transform(st_crs(poly))

# === Pass 1: st_intersects, largest-area tie-break ===
inter <- st_intersects(pts, poly)
pass1_idx <- vapply(inter, function(ix) {
  if (length(ix) == 0) return(NA_integer_)
  ix[which.max(poly$osm_acreage[ix])]
}, integer(1))

# === Pass 2: nearest within MAX_NEAREST_M for unmatched, largest-area tie-break ===
need_pass2 <- which(is.na(pass1_idx))
cat(sprintf("Pass 1 matches: %s\n", format(sum(!is.na(pass1_idx)), big.mark = ",")))
cat(sprintf("Need Pass 2: %s\n", format(length(need_pass2), big.mark = ",")))

pass2_idx  <- rep(NA_integer_, nrow(pts))
pass2_dist <- rep(NA_real_, nrow(pts))

nn <- st_nearest_feature(pts[need_pass2, ], poly)
d  <- st_distance(pts[need_pass2, ], poly[nn, ], by_element = TRUE)
d  <- as.numeric(d)

within_cap <- d <= MAX_NEAREST_M
pass2_idx[need_pass2[within_cap]]  <- nn[within_cap]
pass2_dist[need_pass2[within_cap]] <- d[within_cap]

cat(sprintf("Pass 2 recoveries (<= %d m): %s\n", MAX_NEAREST_M,
            format(sum(within_cap), big.mark = ",")))

matched_idx  <- ifelse(!is.na(pass1_idx), pass1_idx, pass2_idx)
matched_pass <- ifelse(!is.na(pass1_idx), "Pass1", ifelse(!is.na(pass2_idx), "Pass2", NA))
cat(sprintf("Total OSM-sourced: %s\n", format(sum(!is.na(matched_idx)), big.mark = ",")))

courses$matched_pass  <- matched_pass
courses$matched_name  <- ifelse(!is.na(matched_idx), poly$name[matched_idx], NA)
courses$matched_acres <- ifelse(!is.na(matched_idx), poly$osm_acreage[matched_idx], NA)
courses$matched_dist_m <- pass2_dist

# === Sanity check against production output ===
prod <- read.csv(PRODUCTION_CSV, stringsAsFactors = FALSE)
cat(sprintf("\nProduction acreage_source==OSM count: %s\n",
            format(sum(prod$acreage_source == "OSM"), big.mark = ",")))
cat(sprintf("This diagnostic's matched count:        %s\n",
            format(sum(!is.na(matched_idx)), big.mark = ",")))

# === Name normalization + Jaccard ===
normalize_tokens <- function(x) {
  x <- tolower(x)
  x <- gsub("&", " and ", x, fixed = TRUE)
  x <- gsub("[-/]", " ", x)
  x <- gsub("['.,]", "", x)
  x <- gsub("[^a-z0-9 ]", " ", x)
  x <- trimws(gsub("\\s+", " ", x))
  strsplit(x, " ")
}

jaccard <- function(a, b) {
  ta <- unique(a[[1]]); tb <- unique(b[[1]])
  if (length(ta) == 0 || length(tb) == 0) return(NA_real_)
  length(intersect(ta, tb)) / length(union(ta, tb))
}

p2 <- courses |> filter(matched_pass == "Pass2")
cat(sprintf("\nPass 2 matched rows for name analysis: %d\n", nrow(p2)))

course_tok <- normalize_tokens(p2$Course_Name)
poly_tok   <- normalize_tokens(p2$matched_name)
p2$jaccard <- mapply(function(i) jaccard(course_tok[i], poly_tok[i]), seq_len(nrow(p2)))
p2$poly_unnamed <- is.na(p2$matched_name) | p2$matched_name %in% c("Unknown", "", NA)

p2 <- p2 |>
  mutate(category = case_when(
    poly_unnamed              ~ "Unnamed polygon (unverifiable)",
    is.na(jaccard)             ~ "Unnamed polygon (unverifiable)",
    jaccard >= 0.9             ~ "Exact/near-exact (Jaccard >= 0.9)",
    jaccard >= 0.5             ~ "Strong (Jaccard >= 0.5)",
    jaccard > 0                ~ "Weak (0 < Jaccard < 0.5)",
    TRUE                       ~ "Zero overlap (confirmed-wrong candidate)"
  ))

cat("\n=== Pass-2 name-match category breakdown ===\n")
tab <- p2 |> count(category) |> mutate(pct_of_pass2 = round(100 * n / nrow(p2), 1))
print(tab)

confirmed_wrong <- p2 |> filter(category == "Zero overlap (confirmed-wrong candidate)")
n_confirmed <- nrow(confirmed_wrong)
cat(sprintf(
  "\nConfirmed-wrong (zero name overlap, real-named polygon): %d (%.1f%% of Pass 2, %.2f%% of national %d-course baseline)\n",
  n_confirmed, 100 * n_confirmed / nrow(p2), 100 * n_confirmed / nrow(courses), nrow(courses)
))

# Charlotte Golf Links sanity check
cgl <- courses |> filter(grepl("Charlotte Golf Links", Course_Name, ignore.case = TRUE))
cat("\n=== Charlotte Golf Links sanity check ===\n")
print(cgl[, c("Course_Name", "matched_pass", "matched_name", "matched_dist_m")])

# === Acreage distribution: confirmed-wrong vs Pass-1 ===
pass1_acres <- courses |> filter(matched_pass == "Pass1") |> pull(matched_acres)
cat("\n=== Acreage: Pass-1 matches ===\n"); print(summary(pass1_acres))
cat("\n=== Acreage: Pass-2 confirmed-wrong ===\n"); print(summary(confirmed_wrong$matched_acres))

# === Aggregate $ contribution ===
# Baseline_Value_Per_Acre comes from Phase 1 too
bvpa <- courses$Baseline_Value_Per_Acre
courses$est_value <- courses$matched_acres * bvpa
observed_total <- sum(courses$est_value[!is.na(matched_idx)], na.rm = TRUE)
confirmed_wrong_value <- sum(courses$est_value[courses$Course_Name %in% confirmed_wrong$Course_Name &
                                                  !is.na(courses$est_value)], na.rm = TRUE)
cat(sprintf(
  "\nObserved-only total value (all OSM-matched courses): $%.3fB\n", observed_total / 1e9
))
cat(sprintf(
  "Confirmed-wrong subset's contribution to that total:  $%.3fB (%.3f%%)\n",
  confirmed_wrong_value / 1e9, 100 * confirmed_wrong_value / observed_total
))

dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
write.csv(courses |> select(Course_Name, Longitude, Latitude, matched_pass, matched_name,
                              matched_acres, matched_dist_m),
          OUT_CSV, row.names = FALSE)
cat(sprintf("\nSaved: %s\n", OUT_CSV))
