# Purpose: Phase 6 master visualization script - spatial maps, LaTeX tables,
#          and econometric charts for the golf course opportunity cost thesis.
# Inputs:  R_Phase1_Baseline_Golf_Valuation.csv
#          R_Phase2_Acreage_Matched_v2.csv
#          R_Imputed_Dataset_1..100.csv, Py_Imputed_Dataset_1..100.csv,
#          Jl_Imputed_Dataset_1..100.csv
#          R_Regression_Results.csv, National_Acreage_Summary.csv
#          Phase5_Geographic_Breakdown.csv, Target_Golf_Parcels_List.csv
#          Honolulu_Parcels_Reprojected.gpkg, Target_Golf_Polygons.gpkg
#          Zoning_-2205419429161838665.gpkg
# Outputs: output/Final_Thesis_Figures/*.png, output/Final_Thesis_Figures/*.tex
#          output/QA_Verification/*.png


# === 1. LIBRARIES ===

# X-08/Decision 1 (2026-07-27): activate the pinned renv project library before
# loading any packages, so this script runs against the versions in renv.lock,
# not whatever happens to be in this machine's personal R library.
local({
    cmd_args <- commandArgs(trailingOnly = FALSE)
    m <- grep("^--file=", cmd_args)
    if (length(m) == 0) return(invisible(NULL))
    script_path <- normalizePath(sub("^--file=", "", cmd_args[m]))
    proj_dir <- dirname(dirname(script_path))
    activate_r <- file.path(proj_dir, "renv", "activate.R")
    if (file.exists(activate_r)) {
        Sys.setenv(RENV_PROJECT = proj_dir)
        source(activate_r)
    }
})

suppressPackageStartupMessages({
    library(biscale)
    library(cowplot)
    library(furrr)
    library(future)
    library(ggspatial)
    library(kableExtra)
    library(knitr)
    library(parallelly)
    library(scales)
    library(sf)
    library(this.path)
    library(tidyverse)
    library(tigris)
})


# === 2. GLOBALS & PATHS ===
# (Paths and constants are defined per-function inside each run_X_() Section 2
#  block, using this.path::this.dir() for script-relative root resolution.)

SCRIPT_DIR <- this.path::this.dir()
PROV_START <- Sys.time()
source(file.path(SCRIPT_DIR, "..", "provenance.R"))

# Colors for UH Manoa
UHM_GREEN <- "#024731"     #- [Green]
UHM_GOLD <- "#B3995D"      #- [Gold]
UHM_SILVER <- "#B2B2B2"    #- [Silver/Grey]
UHM_BLACK <- "#000000"     #- [Black]
UHM_WHITE <- "#FFFFFF"     #- [White]
OCEAN <- "#00758D"         #- [Darker Cyan]
SKY <- "#00A4E2"           #- [Lighter Blue]
LEHUA <- "#E3002C"         #- [Red]
ILIMA <- "#F2A900"         #- [Dark Yellow]
PUA_KENIKENI <- "#FAD561"  #- [Dark Gold]
KUKUI <- "#D6CBAE"         #- [Beige]
AKALA <- "#E06E8C"         #- [Dark Pink]
MAO <- "#82B53F"           #- [Dark Lime Green]
LAI <- "#00846B"           #- [Royal Green]
JColor <- "#800080"        #- [Julia Base Color]
RScColor <- "#008000"      #- [R Base Color]
PColor <- "#0000FF"        #- [Python Base Color]

# === 3. FUNCTIONS ===

# ---------- Grand Mean Aggregator ----------
compute_grand_means <- function() {
    cat("\n--- Grand Mean: Computing Tri-Language Grand Means ---\n")
    SCRIPT_DIR <- this.path::this.dir()
    WORK_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
    PHASE1_CSV <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "R",
        "R_Phase1_Baseline_Golf_Valuation.csv"
    )
    phase1_df <- read_csv(PHASE1_CSV, show_col_types = FALSE) |>
        mutate(FIPS = sprintf("%05d", as.integer(FIPS)))

    county_lookup <- phase1_df |>
        select(Longitude, Latitude, FIPS, County_Name, State_Abbr) |>
        distinct()

    PHASE3_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "R")
    IMPUTED_PATHS <- file.path(PHASE3_DIR, paste0("R_Imputed_Dataset_", 1:100, ".csv"))

    plan(multisession, workers = parallelly::availableCores() - 1)

    cat("  Parallelizing M=100 R MICE loads with furrr...\n")
    # [METHODOLOGY] Rubin's Rules pooling - R language group, M=100; applied independently
    county_total_list <- future_map(seq_len(100), function(i) {
        if (!file.exists(IMPUTED_PATHS[i])) {
            return(NULL)
        }
        read_csv(IMPUTED_PATHS[i], show_col_types = FALSE) |>
            mutate(total_opp_cost = final_acreage * Baseline_Value_Per_Acre) |>
            left_join(county_lookup, by = c("Longitude", "Latitude")) |>
            filter(!is.na(FIPS)) |>
            group_by(FIPS, County_Name, State_Abbr) |>
            summarise(total_opp_cost = sum(total_opp_cost, na.rm = TRUE), .groups = "drop")
    }, .progress = TRUE)

    county_total_list <- compact(county_total_list)

    if (length(county_total_list) == 0) {
        cat("  [Warning] Imputed datasets not found. Grand Mean will be empty.\n")
        return(list(county = list(), state = list()))
    }

    r_pooled <- bind_rows(county_total_list) |>
        group_by(FIPS, County_Name, State_Abbr) |>
        summarise(pooled_opp_cost = mean(total_opp_cost, na.rm = TRUE), .groups = "drop")

    rm(county_total_list)
    gc()

    PHASE3_PY_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "python")
    IMPUTED_PY_PATHS <- file.path(PHASE3_PY_DIR, paste0("Py_Imputed_Dataset_", 1:100, ".csv"))

    cat("  Parallelizing M=100 Python MICE loads with furrr...\n")
    # [METHODOLOGY] Rubin's Rules pooling - Python language group, M=100; applied independently
    py_county_list <- future_map(seq_len(100), function(i) {
        if (!file.exists(IMPUTED_PY_PATHS[i])) {
            return(NULL)
        }
        read_csv(IMPUTED_PY_PATHS[i], show_col_types = FALSE) |>
            mutate(total_opp_cost = osm_acreage * Baseline_Value_Per_Acre) |>
            select(-any_of(c("FIPS", "County_Name", "State_Abbr"))) |>
            left_join(county_lookup, by = c("Longitude", "Latitude")) |>
            filter(!is.na(FIPS)) |>
            group_by(FIPS, County_Name, State_Abbr) |>
            summarise(total_opp_cost = sum(total_opp_cost, na.rm = TRUE), .groups = "drop")
    }, .progress = TRUE)
    py_county_list <- compact(py_county_list)

    if (length(py_county_list) == 0) {
        cat("  [Warning] Python imputed datasets not found. Skipping Python pooling.\n")
        py_pooled <- r_pooled
    } else {
        py_pooled <- bind_rows(py_county_list) |>
            group_by(FIPS, County_Name, State_Abbr) |>
            summarise(pooled_opp_cost = mean(total_opp_cost, na.rm = TRUE), .groups = "drop")
    }
    rm(py_county_list)
    gc()

    PHASE3_JL_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "Julia")
    IMPUTED_JL_PATHS <- file.path(PHASE3_JL_DIR, paste0("Jl_Imputed_Dataset_", 1:100, ".csv"))

    cat("  Parallelizing M=100 Julia MICE loads with furrr...\n")
    # [METHODOLOGY] Rubin's Rules pooling - Julia language group, M=100; applied independently
    jl_county_list <- future_map(seq_len(100), function(i) {
        if (!file.exists(IMPUTED_JL_PATHS[i])) {
            return(NULL)
        }
        read_csv(IMPUTED_JL_PATHS[i], show_col_types = FALSE) |>
            mutate(total_opp_cost = osm_acreage * Baseline_Value_Per_Acre) |>
            select(-any_of(c("FIPS", "County_Name", "State_Abbr"))) |>
            left_join(county_lookup, by = c("Longitude", "Latitude")) |>
            filter(!is.na(FIPS)) |>
            group_by(FIPS, County_Name, State_Abbr) |>
            summarise(total_opp_cost = sum(total_opp_cost, na.rm = TRUE), .groups = "drop")
    }, .progress = TRUE)
    jl_county_list <- compact(jl_county_list)

    if (length(jl_county_list) == 0) {
        cat("  [Warning] Julia imputed datasets not found. Skipping Julia pooling.\n")
        jl_pooled <- r_pooled
    } else {
        jl_pooled <- bind_rows(jl_county_list) |>
            group_by(FIPS, County_Name, State_Abbr) |>
            summarise(pooled_opp_cost = mean(total_opp_cost, na.rm = TRUE), .groups = "drop")
    }
    rm(jl_county_list)
    gc()

    # [METHODOLOGY] Grand Mean = arithmetic mean of three independently Rubin-pooled
    #               estimates. Joined on FIPS to prevent positional mismatch when
    #               county coverage differs across language pools.
    grand_mean_county <- r_pooled |>
        rename(opp_r = pooled_opp_cost) |>
        full_join(
            py_pooled |> rename(opp_py = pooled_opp_cost),
            by = c("FIPS", "County_Name", "State_Abbr")
        ) |>
        full_join(
            jl_pooled |> rename(opp_jl = pooled_opp_cost),
            by = c("FIPS", "County_Name", "State_Abbr")
        ) |>
        mutate(
            pooled_opp_cost = rowMeans(cbind(opp_r, opp_py, opp_jl), na.rm = TRUE)
        ) |>
        select(FIPS, County_Name, State_Abbr, pooled_opp_cost) |>
        arrange(desc(pooled_opp_cost))

    make_state <- function(df) {
        df |>
            group_by(State_Abbr) |>
            summarise(pooled_opp_cost = sum(pooled_opp_cost, na.rm = TRUE), .groups = "drop") |>
            arrange(desc(pooled_opp_cost))
    }

    return(list(
        county = list(
            GrandMean = grand_mean_county, Python = py_pooled,
            R = r_pooled, Julia = jl_pooled
        ),
        state = list(
            GrandMean = make_state(grand_mean_county), Python = make_state(py_pooled),
            R = make_state(r_pooled), Julia = make_state(jl_pooled)
        )
    ))
}


# ---------- Script 1: Macro Maps ----------
run_1_Macro_Maps <- function() {
    # === 2. GLOBALS & PATHS ===

    SCRIPT_DIR <- this.path::this.dir()
    WORK_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
    PHASE1_CSV <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "R",
        "R_Phase1_Baseline_Golf_Valuation.csv"
    )
    PHASE2_CSV <- file.path(
        WORK_DIR, "Phase 2 Spatial Polygons and True Acreage", "Data", "R",
        "R_Phase2_Acreage_Matched_v2.csv"
    )
    PHASE3_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "R")
    IMPUTED_PATHS <- file.path(PHASE3_DIR, paste0("R_Imputed_Dataset_", 1:100, ".csv"))
    OUTPUT_DIR <- file.path(SCRIPT_DIR, "output")
    THESIS_DIR <- file.path(OUTPUT_DIR, "Final_Thesis_Figures")
    QA_DIR <- file.path(OUTPUT_DIR, "QA_Verification")
    dir.create(THESIS_DIR, showWarnings = FALSE, recursive = TRUE)
    dir.create(QA_DIR, showWarnings = FALSE, recursive = TRUE)
    OUT_PNG1 <- file.path(OUTPUT_DIR, "1.1X1_National_Opportunity_Cost_Map.png")
    OUT_PNG2 <- file.path(THESIS_DIR, "1.101_National_Opportunity_Cost_Map_ObservedOnly.png")

    M <- 100L
    TERRITORY_EXCLUDE <- c("AS", "GU", "MP", "PR", "VI", "UM")


    # === 3. FUNCTIONS ===

    # Render national state choropleth and return the ggplot object.
    # Expects a column named `pooled_opp_cost` in states_joined (billions if /1e9).
    build_state_map <- function(states_joined, subtitle, caption_text) {
        ggplot(states_joined) +
            geom_sf(
                aes(fill = pooled_opp_cost / 1e9),
                colour = "white",
                linewidth = 0.25
            ) +
            scale_fill_viridis_c(
                option = "magma",
                na.value = "#8f8f8f",
                name = "Opportunity Cost",
                labels = label_dollar(suffix = "B", accuracy = 1),
                guide = guide_colorbar(
                    barwidth       = unit(21, "cm"),
                    barheight      = unit(0.45, "cm"),
                    title.position = "top",
                    title.hjust    = 0.5,
                    ticks.colour   = "white"
                )
            ) +
            labs(
                title    = "Golf Course Opportunity Cost by State",
                subtitle = subtitle,
                caption  = stringr::str_wrap(caption_text, width = 192)
            ) +
            theme_void(base_size = 12) +
            theme(
                plot.title = element_text(
                    face = "bold", size = 18, hjust = 0.5, margin = margin(b = 5)
                ),
                plot.subtitle = element_text(
                    size = 10, hjust = 0.5, colour = UHM_GREEN, margin = margin(b = 0)
                ),
                plot.caption = element_text(
                    size = 10, colour = UHM_GREEN, hjust = 0, margin = margin(t = 6), lineheight = 0.9
                ),
                plot.caption.position = "plot",
                legend.position = "bottom",
                legend.title = element_text(size = 14, face = "bold"),
                legend.text = element_text(size = 12),
                plot.margin = margin(12, 24, 8, 24)
            )
    }


    # === 4. EXECUTION ===

    cat("\n--- Script 1: National Opportunity Cost by State ---\n\n")
    dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

    for (f in c(PHASE1_CSV, PHASE2_CSV, IMPUTED_PATHS)) {
        if (!file.exists(f)) stop(sprintf("[FATAL] Input not found:\n  %s", f))
    }


    #  Step 1: Build state lookup

    cat("[Step 1] Building state lookup from Phase 1 baseline...\n")
    phase1_df <- read_csv(PHASE1_CSV, show_col_types = FALSE)
    state_lookup <- phase1_df |>
        select(Longitude, Latitude, State_Abbr) |>
        distinct()
    cat(sprintf("  %d unique course coordinates loaded.\n", nrow(state_lookup)))


    #  Step 2: Pool opportunity costs across M imputations
    # [METHODOLOGY] Total_Opportunity_Cost = final_acreage * Baseline_Value_Per_Acre.
    #               State totals summed within each imputed dataset, then averaged
    #               across M = 100 datasets (Rubin's Rules q_bar at the state level).

    cat("[Step 2] Using global Tri-Language Grand Means for State.\n")
    pooled_state <- grand_means$state$GrandMean

    cat(sprintf(
        "\n  MICE pooled national total: $%.3fT across %d states\n",
        sum(pooled_state$pooled_opp_cost) / 1e12,
        nrow(pooled_state)
    ))

    cat("\n  Top 10 states by opportunity cost:\n")
    cat(sprintf("  %-6s  %s\n", "State", "Pooled Cost ($B)"))
    cat(sprintf("  %s\n", strrep("-", 30)))
    for (i in seq_len(min(10L, nrow(pooled_state)))) {
        cat(sprintf(
            "  %-6s  $%.2fB\n",
            pooled_state$State_Abbr[i],
            pooled_state$pooled_opp_cost[i] / 1e9
        ))
    }


    #  Step 3: Read state boundaries (shared between both maps)
    # [METHODOLOGY] vendored Census cartographic boundary file (1:500k, GENZ2022).
    #               Territories are excluded. shift_geometry() repositions Alaska and
    #               Hawaii as insets below the lower 48.

    cat("\n[Step 3] Reading state boundaries (vendored, cb_2022_us_state_500k)...\n")
    STATE_500K_SHP <- file.path(WORK_DIR, "00 - Data Sources", "Secondary",
                                 "cb_2022_us_state_500k", "cb_2022_us_state_500k.shp")
    states_sf <- st_read(STATE_500K_SHP, quiet = TRUE) |>
        filter(!STUSPS %in% TERRITORY_EXCLUDE) |>
        shift_geometry() |>
        st_transform(5070)
    cat(sprintf("  %d states loaded with AK/HI insets (EPSG 5070).\n", nrow(states_sf)))


    #  Step 4: Render + save Map 1.1 (MICE-pooled)

    cat("\n[Step 4] Rendering Map 1.1: MICE-pooled state maps (4 variations)...\n")

    for (model_name in names(grand_means$state)) {
        cat(sprintf("  -> Rendering %s map...\n", model_name))
        pooled_state <- grand_means$state[[model_name]]

        states_mice <- states_sf |>
            left_join(pooled_state, by = c("STUSPS" = "State_Abbr"))

        map1 <- build_state_map(
            states_mice,
            subtitle = paste0(
                model_name, " Pooled Estimate  -  ",
                "Opportunity Cost = OSM Acreage × Baseline Land Value per Acre"
            ),
            caption_text = if (model_name == "GrandMean") {
                paste0(
                    "Pooled across 300 MICE imputations (100 Python, 100 R, 100 Julia). ",
                    "Grand Mean = arithmetic mean of three independently Rubin-pooled estimates.\n",
                    "Sources: OpenStreetMap golf course polygons; FHFA residential land price index ",
                    "(urban counties, RUCC 1-3); USDA agricultural land values (rural counties, ",
                    "RUCC 4-9). CRS: NAD83 / Conus Albers (EPSG 5070). Alaska and Hawaii shown as insets."
                )
            } else {
                paste0(
                    "Pooled across 100 ", model_name, " MICE imputations (Rubin's Rules q_bar). ",
                    "QA verification map.\n",
                    "Sources: OpenStreetMap golf course polygons; FHFA residential land price index ",
                    "(urban counties, RUCC 1-3); USDA agricultural land values (rural counties, ",
                    "RUCC 4-9). CRS: NAD83 / Conus Albers (EPSG 5070). Alaska and Hawaii shown as insets."
                )
            }
        )

        lang_code <- c("GrandMean" = "4", "Python" = "2", "R" = "3", "Julia" = "1")[[model_name]]
        sub_dir <- if (lang_code %in% c("4", "0")) "Final_Thesis_Figures" else "QA_Verification"
        out_file <- sub("X", lang_code, OUT_PNG1)
        out_file <- sub("\\.png$", paste0("_", model_name, ".png"), out_file)
        out_file <- sub("output", paste0("output/", sub_dir), out_file)

        ggsave(out_file, map1, width = 14, height = 9, dpi = 300, units = "in")
        cat(sprintf("  Saved: %s\n", out_file))
    }


    #  Step 5: Compute observed-only state totals
    # [METHODOLOGY] Phase 2 records acreage_source for every course. Filtering to
    #               acreage_source != "MICE_Target" retains only courses with a directly
    #               measured OSM polygon area, eliminating all imputed contributions.
    #               No pooling is required since there is only one observed value per course.

    cat("\n[Step 5] Computing observed-only state totals from Phase 2...\n")

    obs_state <- read_csv(PHASE2_CSV, show_col_types = FALSE) |>
        filter(acreage_source != "MICE_Target") |>
        mutate(opp_cost = final_acreage * Baseline_Value_Per_Acre) |>
        filter(!is.na(State_Abbr)) |>
        group_by(State_Abbr) |>
        summarise(
            pooled_opp_cost = sum(opp_cost, na.rm = TRUE),
            .groups = "drop"
        ) |>
        arrange(desc(pooled_opp_cost))

    cat(sprintf(
        "  Observed-only national total: $%.3fT across %d states\n",
        sum(obs_state$pooled_opp_cost) / 1e12,
        nrow(obs_state)
    ))
    cat(sprintf(
        "  Coverage vs. MICE-pooled: %.1f%%\n",
        sum(obs_state$pooled_opp_cost) / sum(grand_means$state$GrandMean$pooled_opp_cost) * 100
    ))


    #  Step 6: Render + save Map 1.2 (Observed-only)

    cat("\n[Step 6] Rendering Map 1.2: Observed-only state map...\n")

    states_obs <- states_sf |>
        left_join(obs_state, by = c("STUSPS" = "State_Abbr"))

    map2 <- build_state_map(
        states_obs,
        subtitle = paste0(
            "Observed acreage only - no imputation  -  ",
            "Opportunity Cost = OSM Acreage × Baseline Land Value per Acre"
        ),
        caption_text = paste0(
            "Restricted to courses with directly measured OSM polygon acreage ",
            "(acreage_source ≠ MICE_Target). Courses lacking polygon coverage excluded.\n",
            "Sources: OpenStreetMap; FHFA residential land price index (urban); ",
            "USDA agricultural land values (rural). CRS: EPSG 5070. Alaska and Hawaii shown as insets."
        )
    )
    ggsave(OUT_PNG2, map2, width = 14, height = 9, dpi = 300, units = "in")
    cat(sprintf("  Saved: output/%s\n", basename(OUT_PNG2)))

    cat("--- Done ---\n")
    gc()
}


# ---------- Script 2: County Map ----------
run_2_County_Map <- function() {
    # === 2. GLOBALS & PATHS ===

    SCRIPT_DIR <- this.path::this.dir()
    WORK_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
    PHASE1_CSV <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "R",
        "R_Phase1_Baseline_Golf_Valuation.csv"
    )
    PHASE2_CSV <- file.path(
        WORK_DIR, "Phase 2 Spatial Polygons and True Acreage", "Data", "R",
        "R_Phase2_Acreage_Matched_v2.csv"
    )
    PHASE3_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "R")
    IMPUTED_PATHS <- file.path(PHASE3_DIR, paste0("R_Imputed_Dataset_", 1:100, ".csv"))
    OUTPUT_DIR <- file.path(SCRIPT_DIR, "output")
    THESIS_DIR <- file.path(OUTPUT_DIR, "Final_Thesis_Figures")
    QA_DIR <- file.path(OUTPUT_DIR, "QA_Verification")
    dir.create(THESIS_DIR, showWarnings = FALSE, recursive = TRUE)
    dir.create(QA_DIR, showWarnings = FALSE, recursive = TRUE)
    OUT_PNG1 <- file.path(OUTPUT_DIR, "2.1X1_County_Opportunity_Cost_Map.png")
    OUT_PNG2 <- file.path(THESIS_DIR, "2.101_County_Opportunity_Cost_Map_ObservedOnly.png")

    M <- 100L
    TERRITORY_STATEFP <- c("60", "66", "69", "72", "74", "78")


    # === 3. FUNCTIONS ===

    # Render national county choropleth (log10 fill) and return the ggplot object.
    # Expects a column named `pooled_opp_cost` in counties_joined (millions if /1e6).
    build_county_map <- function(counties_joined, subtitle, caption_text) {
    ggplot(counties_joined) +
            geom_sf(
                aes(fill = pooled_opp_cost / 1e6),
                colour    = "white",
                linewidth = 0.08
            ) +
            scale_fill_viridis_c(
                option   = "magma",
                trans    = "log10",
                na.value = "#8f8f8f",
                name     = "Opportunity Cost",
                breaks   = c(1, 10, 100, 1000, 10000),
                labels   = c("$1M", "$10M", "$100M", "$1B", "$10B"),
                guide    = guide_colorbar(
                    barwidth       = unit(21, "cm"),
                    barheight      = unit(0.45, "cm"),
                    title.position = "top",
                    title.hjust    = 0.5,
                    ticks.colour   = "white"
                )
            ) +
            labs(
                title    = "Golf Course Opportunity Cost by County",
                subtitle = subtitle,
                caption  = stringr::str_wrap(caption_text, width = 192)
            ) +
            theme_void(base_size = 12) +
            theme(
                plot.title      = element_text(
                    face = "bold", size = 18, hjust = 0.5, margin = margin(b = 5)
                ),
                plot.subtitle   = element_text(
                    size = 10, hjust = 0.5, colour = UHM_GREEN, margin = margin(b = 0)
                ),
                plot.caption    = element_text(
                    size = 10, colour = UHM_GREEN, hjust = 0, margin = margin(t = 6), lineheight = 0.9
                ),
                plot.caption.position = "plot",
                legend.position = "bottom",
                legend.title    = element_text(size = 14, face = "bold"),
                legend.text     = element_text(size = 12),
                plot.margin     = margin(12, 24, 8, 24)
            )
    }


    # === 4. EXECUTION ===

    cat("\n--- Script 2: County-Level Opportunity Cost ---\n\n")
    dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

    for (f in c(PHASE1_CSV, PHASE2_CSV, IMPUTED_PATHS)) {
        if (!file.exists(f)) stop(sprintf("[FATAL] Input not found:\n  %s", f))
    }


    #  Step 1: Build county lookup
    # FIPS is zero-padded to 5 characters to match tigris GEOID format.

    cat("[Step 1] Building county lookup from Phase 1 baseline...\n")
    phase1_df <- read_csv(PHASE1_CSV, show_col_types = FALSE)
    county_lookup <- phase1_df |>
        select(Longitude, Latitude, FIPS, County_Name, State_Abbr) |>
        distinct() |>
        mutate(FIPS = sprintf("%05d", as.integer(FIPS)))
    cat(sprintf("  %d unique course coordinates loaded.\n", nrow(county_lookup)))


    #  Step 2: Pool opportunity costs across M imputations
    # [METHODOLOGY] County totals summed within each imputed dataset, then averaged
    #               across M = 100 datasets (Rubin's Rules q_bar at the county level).

    cat("[Step 2] Using global Tri-Language Grand Means for County.\n")
    pooled_county <- grand_means$county$GrandMean

    cat(sprintf(
        "\n  MICE pooled national total: $%.3fT across %d counties\n",
        sum(pooled_county$pooled_opp_cost) / 1e12,
        nrow(pooled_county)
    ))

    cat("\n  Top 10 counties by opportunity cost:\n")
    cat(sprintf("  %-30s  %-6s  %s\n", "County", "State", "Pooled Cost ($B)"))
    cat(sprintf("  %s\n", strrep("-", 52)))
    for (i in seq_len(min(10L, nrow(pooled_county)))) {
        cat(sprintf(
            "  %-30s  %-6s  $%.2fB\n",
            pooled_county$County_Name[i],
            pooled_county$State_Abbr[i],
            pooled_county$pooled_opp_cost[i] / 1e9
        ))
    }


    #  Step 3: Read county and state boundaries (shared between both maps)
    # [METHODOLOGY] vendored Census cartographic boundary file (1:500k, GENZ2022).
    #               Territories are excluded by STATEFP code. shift_geometry()
    #               repositions Alaska and Hawaii as insets.

    cat("\n[Step 3] Reading county boundaries (vendored, cb_2022_us_county_500k)...\n")
    COUNTY_500K_SHP <- file.path(WORK_DIR, "00 - Data Sources", "Secondary",
                                  "cb_2022_us_county_500k", "cb_2022_us_county_500k.shp")
    counties_sf <- st_read(COUNTY_500K_SHP, quiet = TRUE) |>
        filter(!STATEFP %in% TERRITORY_STATEFP) |>
        shift_geometry() |>
        st_transform(5070)
    cat(sprintf("  %d counties loaded with AK/HI insets (EPSG 5070).\n", nrow(counties_sf)))


    #  Step 4: Render + save Map 2.1 (MICE-pooled)

    cat("\n[Step 4] Rendering Map 2.1: MICE-pooled county maps (4 variations)...\n")

    for (model_name in names(grand_means$county)) {
        cat(sprintf("  -> Rendering %s map...\n", model_name))
        pooled_county <- grand_means$county[[model_name]]

        counties_mice <- counties_sf |>
            left_join(pooled_county, by = c("GEOID" = "FIPS"))

        map1 <- build_county_map(
            counties_mice,
            subtitle = paste0(
                model_name, " Pooled Estimate  -  ",
                "Opportunity Cost = OSM Acreage × Baseline Land Value per Acre  -  ",
                "Log₁₀ scale"
            ),
            caption_text = if (model_name == "GrandMean") {
                paste0(
                    "Pooled across 300 MICE imputations (100 Python, 100 R, 100 Julia). ",
                    "Grand Mean = arithmetic mean of three independently Rubin-pooled estimates.\n",
                    "Sources: OpenStreetMap golf course polygons; FHFA residential land price index ",
                    "(urban counties, RUCC 1-3); USDA agricultural land values (rural counties, ",
                    "RUCC 4-9). CRS: NAD83 / Conus Albers (EPSG 5070). Alaska and Hawaii shown as insets."
                )
            } else {
                paste0(
                    "Pooled across 100 ", model_name, " MICE imputations (Rubin's Rules q_bar). ",
                    "QA verification map.\n",
                    "Sources: OpenStreetMap golf course polygons; FHFA residential land price index ",
                    "(urban counties, RUCC 1-3); USDA agricultural land values (rural counties, ",
                    "RUCC 4-9). CRS: NAD83 / Conus Albers (EPSG 5070). Alaska and Hawaii shown as insets."
                )
            }
        )

        lang_code <- c("GrandMean" = "4", "Python" = "2", "R" = "3", "Julia" = "1")[[model_name]]
        sub_dir <- if (lang_code %in% c("4", "0")) "Final_Thesis_Figures" else "QA_Verification"
        out_file <- sub("X", lang_code, OUT_PNG1)
        out_file <- sub("\\.png$", paste0("_", model_name, ".png"), out_file)
        out_file <- sub("output", paste0("output/", sub_dir), out_file)

        ggsave(out_file, map1, width = 14, height = 9, dpi = 300, units = "in")
        cat(sprintf("  Saved: %s\n", out_file))
    }


    #  Step 5: Compute observed-only county totals
    # [METHODOLOGY] Filtering Phase 2 to acreage_source != "MICE_Target" retains
    #               only courses with directly measured OSM polygon acreage. County
    #               totals are a simple sum - no pooling required.

    cat("\n[Step 5] Computing observed-only county totals from Phase 2...\n")

    obs_county <- read_csv(PHASE2_CSV, show_col_types = FALSE) |>
        filter(acreage_source != "MICE_Target") |>
        mutate(
            opp_cost = final_acreage * Baseline_Value_Per_Acre,
            FIPS     = sprintf("%05d", as.integer(FIPS))
        ) |>
        filter(!is.na(FIPS)) |>
        group_by(FIPS, County_Name, State_Abbr) |>
        summarise(
            pooled_opp_cost = sum(opp_cost, na.rm = TRUE),
            .groups = "drop"
        ) |>
        arrange(desc(pooled_opp_cost))

    cat(sprintf(
        "  Observed-only national total: $%.3fT across %d counties\n",
        sum(obs_county$pooled_opp_cost) / 1e12,
        nrow(obs_county)
    ))
    cat(sprintf(
        "  Coverage vs. MICE-pooled: %.1f%%\n",
        sum(obs_county$pooled_opp_cost) / sum(grand_means$county$GrandMean$pooled_opp_cost) * 100
    ))


    #  Step 6: Render + save Map 2.2 (Observed-only)

    cat("\n[Step 6] Rendering Map 2.2: Observed-only county map...\n")

    counties_obs <- counties_sf |>
        left_join(obs_county, by = c("GEOID" = "FIPS"))

    map2 <- build_county_map(
        counties_obs,
        subtitle = paste0(
            "Observed acreage only - no imputation  -  ",
            "Opportunity Cost = OSM Acreage × Baseline Land Value per Acre  -  ",
            "Log₁₀ scale"
        ),
        caption_text = paste0(
            "Restricted to courses with directly measured OSM polygon acreage ",
            "(acreage_source ≠ MICE_Target). Courses lacking polygon coverage excluded.\n",
            "Sources: OpenStreetMap; FHFA residential land price index (urban); ",
            "USDA agricultural land values (rural). CRS: EPSG 5070. ",
            "Alaska and Hawaii shown as insets."
        )
    )
    ggsave(OUT_PNG2, map2, width = 14, height = 9, dpi = 300, units = "in")
    cat(sprintf("  Saved: output/%s\n", basename(OUT_PNG2)))

    cat("--- Done ---\n")
    gc()
}


# ---------- Script 3: Oahu TMK Map ----------
run_3_Oahu_TMK_Map <- function() {
    # === 2. GLOBALS & PATHS ===

    SCRIPT_DIR <- this.path::this.dir()
    WORK_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
    PHASE5_DATA <- file.path(WORK_DIR, "Phase 5 The Hawaii Micro-Case Study", "Data", "R")
    PARCELS_GPKG <- file.path(PHASE5_DATA, "Honolulu_Parcels_Reprojected.gpkg")
    TMK_CSV <- file.path(PHASE5_DATA, "Target_Golf_Parcels_List.csv")
    GEO_CSV <- file.path(PHASE5_DATA, "Phase5_Geographic_Breakdown.csv")
    OUTPUT_DIR <- file.path(SCRIPT_DIR, "output")
    THESIS_DIR <- file.path(OUTPUT_DIR, "Final_Thesis_Figures")
    QA_DIR <- file.path(OUTPUT_DIR, "QA_Verification")
    dir.create(THESIS_DIR, showWarnings = FALSE, recursive = TRUE)
    dir.create(QA_DIR, showWarnings = FALSE, recursive = TRUE)
    OUT_PNG <- file.path(THESIS_DIR, "3.101_Oahu_TMK_Concentration_Map.png")

    OAHU_CRS <- 32604L # WGS 84 / UTM Zone 4N - correct local projection for Oahu
    ZONE_EWA <- "9"
    COL_EWA <- LEHUA # bright orange-red - Ewa District (Zone 9)
    COL_OTHER <- "#3a3a3a" # dark gray - all other districts
    COL_ISLAND <- "#e8e8e8" # light gray - island base fill
    COL_COAST <- "#aaaaaa" # medium gray - coastline border

    # === 3. EXECUTION ===

    cat("\n--- Script 3: Oahu Golf Course TMK Concentration ---\n\n")

    for (f in c(PARCELS_GPKG, TMK_CSV, GEO_CSV)) {
        if (!file.exists(f)) stop(sprintf("[FATAL] Input not found:\n  %s", f))
    }

    cat("[Step 1] Loading TMK list and geographic breakdown...\n")
    tmk_list <- read_csv(TMK_CSV, show_col_types = FALSE)
    geo_df <- read_csv(GEO_CSV, show_col_types = FALSE)

    ewa_row <- geo_df |> filter(Zone_Code == as.integer(ZONE_EWA))
    cat(sprintf("  %d target TMKs loaded.\n", nrow(tmk_list)))
    cat(sprintf(
        "  Zone 9 (Ewa): %d parcels (%.1f%% of total per breakdown CSV).\n",
        ewa_row$Parcel_Count,
        ewa_row$Pct_of_Total_Parcels
    ))

    cat("\n[Step 2] Loading Honolulu parcels GeoPackage and reprojecting...\n")
    # [METHODOLOGY] GeoPackage is stored in EPSG 5070 (NAD83/Conus Albers). Reprojecting
    #               to EPSG 32604 (WGS 84/UTM Zone 4N) gives a north-up equal-distance
    #               view centred on Oahu and produces correct metric scale bar units.
    all_parcels_sf <- st_read(PARCELS_GPKG, quiet = TRUE) |>
        st_transform(OAHU_CRS)
    cat(sprintf(
        "  %d total parcels loaded (EPSG %d).\n",
        nrow(all_parcels_sf), OAHU_CRS
    ))

    cat("\n[Step 3] Filtering to golf TMKs and flagging Ewa District (Zone 9)...\n")
    golf_sf <- all_parcels_sf |>
        filter(tmk8num %in% tmk_list$TMK) |>
        mutate(district = if_else(
            zone == ZONE_EWA,
            "Ewa District (Zone 9)",
            "Other Districts"
        )) |>
        mutate(district = factor(
            district,
            levels = c("Ewa District (Zone 9)", "Other Districts")
        ))

    n_ewa <- sum(golf_sf$zone == ZONE_EWA, na.rm = TRUE)
    n_other <- nrow(golf_sf) - n_ewa
    cat(sprintf(
        "  %d golf TMKs: %d Ewa (Zone 9), %d across other districts.\n",
        nrow(golf_sf), n_ewa, n_other
    ))

    if (nrow(golf_sf) != nrow(tmk_list)) {
        cat(sprintf(
            "  [WARNING] TMK match: %d matched out of %d in list.\n",
            nrow(golf_sf), nrow(tmk_list)
        ))
    }

    cat("\n[Step 4] Building Oahu island outline via st_union (may take ~20 sec)...\n")
    # [METHODOLOGY] st_union() merges all 177k parcel polygons into a single outline,
    #               eliminating internal cadastral boundaries. Used in place of a
    #               separate boundary shapefile to keep inputs self-contained.
    oahu_outline_sf <- st_sf(geometry = st_union(all_parcels_sf))
    cat("  Island outline complete.\n")

    cat("\n  Golf parcel breakdown by zone:\n")
    cat(sprintf("  %-6s  %-35s  %s\n", "Zone", "District", "Parcels"))
    cat(sprintf("  %s\n", strrep("-", 55)))
    zone_summary <- golf_sf |>
        st_drop_geometry() |>
        count(zone, district, name = "n") |>
        arrange(desc(n))
    for (i in seq_len(nrow(zone_summary))) {
        cat(sprintf(
            "  %-6s  %-35s  %d\n",
            zone_summary$zone[i],
            as.character(zone_summary$district[i]),
            zone_summary$n[i]
        ))
    }

    # [METHODOLOGY] Parcels are sorted so Other Districts (dark gray) render first
    #               and Ewa (orange-red) renders on top, ensuring maximum visibility
    #               of the highlighted zone. A single geom_sf call preserves a unified
    #               legend. ggspatial::annotation_scale() computes the bar from UTM
    #               metres. Legend is floated over ocean in the lower-right corner.
    cat("\n[Step 5] Rendering Oahu micro-map...\n")

    oahu_map <- ggplot() +
        geom_sf(
            data      = oahu_outline_sf,
            fill      = COL_ISLAND,
            colour    = COL_COAST,
            linewidth = 0.35
        ) +
        geom_sf(
            data   = arrange(golf_sf, district == "Ewa District (Zone 9)"),
            aes(fill = district),
            colour = NA
        ) +
        scale_fill_manual(
            name   = "Golf Course Parcels",
            values = c(
                "Ewa District (Zone 9)" = COL_EWA,
                "Other Districts"       = COL_OTHER
            ),
            guide = guide_legend(
                override.aes = list(colour = NA),
                keywidth     = unit(0.65, "cm"),
                keyheight    = unit(0.65, "cm")
            )
        ) +
        annotation_scale(
            location   = "br",
            width_hint = 0.22,
            style      = "ticks",
            text_cex   = 0.75,
            pad_x      = unit(0.5, "cm"),
            pad_y      = unit(0.5, "cm")
        ) +
        labs(
            title    = "Golf Course Parcel Concentration - Oahu, Hawaiʻi",
            subtitle = sprintf(
                "%d golf TMKs  |  Ewa District (Zone 9): %d parcels (%.1f%%)  |  %d zones represented",
                nrow(golf_sf),
                n_ewa,
                ewa_row$Pct_of_Total_Parcels,
                n_distinct(golf_sf$zone)
            ),
            caption  = paste0(
                "Source: City & County of Honolulu parcel data; OSM golf course polygons. ",
                "CRS: WGS 84 / UTM Zone 4N (EPSG 32604).\n",
                "TMK = Tax Map Key. Zone 9 corresponds to the Ewa District ",
                "(Kapolei / Pearl City / Ewa Beach)."
            )
        ) +
        theme_void(base_size = 12) +
        theme(
            plot.title       = element_text(
                face = "bold", size = 16, hjust = 0.5, margin = margin(b = 4)
            ),
            plot.subtitle    = element_text(
                size = 10, hjust = 0.5, colour = UHM_GREEN, margin = margin(b = 8)
            ),
            plot.caption     = element_text(
                size = 10, colour = UHM_GREEN, hjust = 0, margin = margin(t = 10)
            ),
            legend.position  = c(0.87, 0.87),
            legend.background = element_rect(
                fill = alpha("white", 0.88), colour = "grey75", linewidth = 0.3
            ),
            legend.margin    = margin(5, 9, 5, 9),
            legend.title     = element_text(size = 11, face = "bold"),
            legend.text      = element_text(size = 9),
            plot.background  = element_rect(fill = "white", colour = NA),
            plot.margin      = margin(12, 16, 8, 16)
        )

    dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    ggsave(
        filename = OUT_PNG,
        plot     = oahu_map,
        width    = 12,
        height   = 10,
        dpi      = 300,
        units    = "in"
    )
    cat(sprintf("\n  Saved: output/%s\n", basename(OUT_PNG)))

    cat("--- Done ---\n")
    gc()
}


# ---------- Script 4: Oahu Zoning Map ----------
run_4_Oahu_Zoning_Map <- function() {
    # === 2. GLOBALS & PATHS ===

    SCRIPT_DIR <- this.path::this.dir()
    WORK_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
    PHASE5_DATA <- file.path(WORK_DIR, "Phase 5 The Hawaii Micro-Case Study", "Data", "R")
    PARCELS_GPKG <- file.path(PHASE5_DATA, "Honolulu_Parcels_Reprojected.gpkg")
    TMK_CSV <- file.path(PHASE5_DATA, "Target_Golf_Parcels_List.csv")
    ZONING_GPKG <- file.path(
        WORK_DIR, "00 - Data Sources", "Honolulu",
        "Zoning_-2205419429161838665.gpkg"
    )
    OUTPUT_DIR <- file.path(SCRIPT_DIR, "output")
    THESIS_DIR <- file.path(OUTPUT_DIR, "Final_Thesis_Figures")
    QA_DIR <- file.path(OUTPUT_DIR, "QA_Verification")
    dir.create(THESIS_DIR, showWarnings = FALSE, recursive = TRUE)
    dir.create(QA_DIR, showWarnings = FALSE, recursive = TRUE)
    OUT_PNG <- file.path(THESIS_DIR, "4.101_Oahu_Golf_Zoning_Map.png")

    OAHU_CRS <- 32604L # WGS 84 / UTM Zone 4N
    COL_ISLAND <- "#e8e8e8"
    COL_COAST <- "#aaaaaa"

    # [METHODOLOGY] Colors are assigned semantically by zone type so that related
    #               land-use categories share a hue family, aiding interpretation.
    #               Preservation = greens, Federal = navy, Agriculture = browns,
    #               Resort = amber, Country = teal, Residential = purples
    #               (darkest = most dense), Apartments = oranges, Commercial = reds,
    #               Industrial = blue-grays.
    ZONE_COLORS <- c(
        "P-2"    = "#2e7d32",
        "P-1"    = "#81c784",
        "F-1"    = "#1565c0",
        "AG-2"   = "#6d4c41",
        "AG-1"   = "#a1887f",
        "Resort" = "#f9a825",
        "C"      = "#00897b",
        "R-3.5"  = "#4a148c",
        "R-5"    = "#7b1fa2",
        "R-7.5"  = "#ab47bc",
        "R-10"   = "#ce93d8",
        "R-20"   = "#e1bee7",
        "A-1"    = "#e65100",
        "A-2"    = "#ff8f00",
        "B-1"    = "#b71c1c",
        "B-2"    = "#e53935",
        "BMX-3"  = "#ff7043",
        "I-2"    = "#37474f",
        "IMX-1"  = "#90a4ae"
    )

    # === 3. EXECUTION ===

    cat("\n--- Script 4: Oahu Golf Parcels by Zoning Classification ---\n\n")

    for (f in c(PARCELS_GPKG, TMK_CSV, ZONING_GPKG)) {
        if (!file.exists(f)) stop(sprintf("[FATAL] Input not found:\n  %s", f))
    }

    cat("[Step 1] Loading TMK list...\n")
    tmk_list <- read_csv(TMK_CSV, show_col_types = FALSE)
    cat(sprintf("  %d target TMKs loaded.\n", nrow(tmk_list)))

    cat("\n[Step 2] Loading Honolulu parcels and filtering to golf TMKs...\n")
    # [METHODOLOGY] Spatial read of Honolulu parcels; st_transform to EPSG 32604
    #               (WGS 84 / UTM Zone 4N) centres the projection on Oahu.
    golf_sf <- st_read(PARCELS_GPKG, quiet = TRUE) |>
        filter(tmk8num %in% tmk_list$TMK) |>
        st_transform(OAHU_CRS)
    cat(sprintf("  %d golf TMK parcels loaded (EPSG %d).\n", nrow(golf_sf), OAHU_CRS))

    cat("\n[Step 3] Loading Honolulu zoning layer...\n")
    # [METHODOLOGY] The Honolulu zoning GeoPackage uses ArcGIS convention: geometry
    #               column is named SHAPE and CRS is EPSG 3760 (NAD83 HARN / Hawaii).
    #               Only zone_class and zoning_description are retained for the join.
    zoning_sf <- st_read(ZONING_GPKG, quiet = TRUE) |>
        select(zone_class, zoning_description) |>
        st_transform(OAHU_CRS)
    cat(sprintf(
        "  %d zoning polygons loaded (%d unique classes).\n",
        nrow(zoning_sf),
        n_distinct(zoning_sf$zone_class)
    ))

    cat("\n[Step 4] Assigning dominant zoning to each golf parcel (st_join, largest = TRUE)...\n")
    # [METHODOLOGY] st_join(largest = TRUE) selects the zoning polygon whose intersection
    #               area with each golf parcel is greatest. This avoids assigning edge-
    #               clipping zones to parcels that are overwhelmingly within one class.
    golf_zoned_sf <- st_join(golf_sf, zoning_sf, largest = TRUE)

    n_unzoned <- sum(is.na(golf_zoned_sf$zone_class))
    if (n_unzoned > 0) {
        cat(sprintf(
            "  [WARNING] %d parcels with no zoning match (rendered gray).\n",
            n_unzoned
        ))
    }
    golf_zoned_sf <- golf_zoned_sf |>
        mutate(zone_class = replace_na(zone_class, "Unzoned"))

    cat(sprintf(
        "  %d parcels zoned; %d unique zone classes present.\n",
        nrow(golf_zoned_sf) - n_unzoned,
        n_distinct(golf_zoned_sf$zone_class)
    ))

    cat("\n  Parcel breakdown by dominant zone class:\n")
    cat(sprintf("  %-10s  %-40s  %s\n", "Zone", "Description", "Parcels"))
    cat(sprintf("  %s\n", strrep("-", 60)))
    zone_counts <- golf_zoned_sf |>
        st_drop_geometry() |>
        count(zone_class, zoning_description, name = "n") |>
        arrange(desc(n))
    for (i in seq_len(nrow(zone_counts))) {
        cat(sprintf(
            "  %-10s  %-40s  %d\n",
            zone_counts$zone_class[i],
            coalesce(zone_counts$zoning_description[i], "-"),
            zone_counts$n[i]
        ))
    }

    cat("\n[Step 5] Building Oahu island outline (dissolving all parcels)...\n")
    # [METHODOLOGY] Spatial read of full parcel layer for st_union outline construction;
    #               reprojected to EPSG 32604 to match golf_sf CRS.
    all_parcels_sf <- st_read(PARCELS_GPKG, quiet = TRUE) |> st_transform(OAHU_CRS)
    oahu_outline_sf <- st_sf(geometry = st_union(all_parcels_sf))
    cat("  Island outline complete.\n")

    observed_zones <- sort(unique(golf_zoned_sf$zone_class))
    zone_colors_used <- ZONE_COLORS[names(ZONE_COLORS) %in% observed_zones]
    if ("Unzoned" %in% observed_zones) {
        zone_colors_used <- c(zone_colors_used, Unzoned = "#d4d4d4")
    }

    zone_label_df <- golf_zoned_sf |>
        st_drop_geometry() |>
        count(zone_class, zoning_description, name = "n") |>
        mutate(label = sprintf(
            "%s - %s (%d parcels)",
            zone_class,
            coalesce(str_remove(zoning_description, "^[A-Z0-9-]+ "), zone_class),
            n
        ))
    zone_labels <- setNames(zone_label_df$label, zone_label_df$zone_class)

    cat("\n[Step 6] Rendering Oahu zoning map...\n")

    oahu_zoning_map <- ggplot() +
        geom_sf(
            data      = oahu_outline_sf,
            fill      = COL_ISLAND,
            colour    = COL_COAST,
            linewidth = 0.3
        ) +
        geom_sf(
            data   = golf_zoned_sf,
            aes(fill = zone_class),
            colour = UHM_SILVER
        ) +
        scale_fill_manual(
            name   = "Dominant Zoning Classification",
            values = zone_colors_used,
            labels = zone_labels,
            guide  = guide_legend(
                ncol         = 1,
                override.aes = list(colour = NA),
                keywidth     = unit(0.55, "cm"),
                keyheight    = unit(0.55, "cm")
            )
        ) +
        annotation_scale(
            location   = "br",
            width_hint = 0.22,
            style      = "ticks",
            text_cex   = 0.75,
            pad_x      = unit(0.5, "cm"),
            pad_y      = unit(0.5, "cm")
        ) +
        labs(
            title    = "Golf Course Parcels by Dominant Zoning Class - Oahu, Hawaiʻi",
            subtitle = sprintf(
                "%d golf TMKs  |  %d unique zone classes  |  Each parcel colored by largest-overlap zoning assignment",
                nrow(golf_zoned_sf),
                n_distinct(golf_zoned_sf$zone_class)
            ),
            caption  = paste0(
                "Source: City & County of Honolulu parcel & zoning data; OSM golf course polygons. ",
                "CRS: WGS 84 / UTM Zone 4N (EPSG 32604).\n",
                "Dominant zone assigned via largest intersection area (st_join, largest = TRUE). ",
                "Color families: green = Preservation, navy = Federal,\n",
                "brown = Agriculture, amber = Resort, teal = Country, ",
                "purple = Residential, orange = Apartment, red = Commercial, gray = Industrial."
            )
        ) +
        theme_void(base_size = 11) +
        theme(
            plot.title      = element_text(
                face = "bold", size = 15, hjust = 0.5, margin = margin(b = 4)
            ),
            plot.subtitle   = element_text(
                size = 11, hjust = 0.5, colour = UHM_GREEN, margin = margin(b = 6)
            ),
            plot.caption    = element_text(
                size = 9, colour = UHM_GREEN, hjust = 0, margin = margin(t = 8)
            ),
            legend.position  = "right",
            legend.title     = element_text(size = 8, face = "bold", margin = margin(b = 4)),
            legend.text      = element_text(size = 7.5),
            legend.margin    = margin(0, 6, 0, 6),
            legend.key.spacing.y = unit(1, "pt"),
            plot.background  = element_rect(fill = "white", colour = NA),
            plot.margin      = margin(12, 4, 8, 12)
        )

    dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
    ggsave(
        filename = OUT_PNG,
        plot     = oahu_zoning_map,
        width    = 14,
        height   = 10,
        dpi      = 300,
        units    = "in"
    )
    cat(sprintf("\n  Saved: output/%s\n", basename(OUT_PNG)))

    cat("--- Done ---\n")
    gc()
}


# ---------- Script 7: Bivariate Econometric Map ----------
run_7_Bivariate_Econometric_Map <- function() {
    # === 2. GLOBALS & PATHS ===

    SCRIPT_DIR <- this.path::this.dir()
    WORK_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
    PHASE1_CSV <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "R",
        "R_Phase1_Baseline_Golf_Valuation.csv"
    )
    PHASE2_CSV <- file.path(
        WORK_DIR, "Phase 2 Spatial Polygons and True Acreage", "Data", "R",
        "R_Phase2_Acreage_Matched_v2.csv"
    )
    PHASE3_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "R")
    IMPUTED_PATHS <- file.path(PHASE3_DIR, paste0("R_Imputed_Dataset_", 1:100, ".csv"))
    OUTPUT_DIR <- file.path(SCRIPT_DIR, "output")
    THESIS_DIR <- file.path(OUTPUT_DIR, "Final_Thesis_Figures")
    QA_DIR <- file.path(OUTPUT_DIR, "QA_Verification")
    dir.create(THESIS_DIR, showWarnings = FALSE, recursive = TRUE)
    dir.create(QA_DIR, showWarnings = FALSE, recursive = TRUE)
    OUT_PNG1 <- file.path(OUTPUT_DIR, "7.1X1_Bivariate_Cost_vs_Density_Map.png")
    OUT_PNG2 <- file.path(THESIS_DIR, "7.101_Bivariate_Cost_vs_Density_Map_ObservedOnly.png")

    M <- 100L
    TERRITORY_EXCLUDE_FP <- c("60", "66", "69", "72", "74", "78")


    # === 3. FUNCTIONS ===

    # Classify county_data into a 3x3 bivariate grid, render the choropleth with
    # an inset legend, and return the composed cowplot figure.
    # county_data must have columns: FIPS, pooled_opp_cost, total_holes.
    build_bivariate_map <- function(counties_sf, states_sf, county_data,
                                    subtitle, caption_text) {
        counties_joined <- counties_sf |>
            left_join(county_data, by = c("GEOID" = "FIPS"))

        no_data_n <- sum(is.na(counties_joined$pooled_opp_cost))
        cat(sprintf(
            "    %d counties with no data (gray)  |  %d with data.\n",
            no_data_n, nrow(counties_joined) - no_data_n
        ))

        counties_golf <- counties_joined |>
            filter(!is.na(pooled_opp_cost), !is.na(total_holes))

        counties_bi <- bi_class(
            counties_golf,
            x     = pooled_opp_cost,
            y     = total_holes,
            style = "quantile",
            dim   = 3
        )

        tbl <- table(
            OC_tertile    = sub("-.*", "", counties_bi$bi_class),
            Holes_tertile = sub(".*-", "", counties_bi$bi_class)
        )
        cat("    Bivariate class distribution (col = OC tertile, row = Holes tertile):\n")
        capture.output(print(tbl)) |> (\(x) cat(paste0("    ", x, "\n")))()

        map_plot <- ggplot() +
            geom_sf(
                data   = counties_sf,
                fill   = "#d9d9d9",
                colour = NA
            ) +
            geom_sf(
                data        = counties_bi,
                aes(fill = bi_class),
                colour      = NA,
                show.legend = FALSE
            ) +
            bi_scale_fill(pal = "DkViolet", dim = 3) +
            geom_sf(
                data      = states_sf,
                fill      = NA,
                colour    = UHM_WHITE,
                linewidth = 0.30
            ) +
            labs(
                title = paste0(
                    "Golf Course Opportunity Cost vs. Course Density",
                    " - U.S. Counties"
                ),
                subtitle = subtitle,
                caption  = stringr::str_wrap(caption_text, width = 192)
            ) +
            theme_void(base_size = 12) +
            theme(
                plot.title = element_text(
                    face = "bold", size = 18, hjust = 0.5,
                    margin = margin(b = 5)
                ),
                plot.subtitle = element_text(
                    size = 11, hjust = 0.5, colour = UHM_GREEN,
                    margin = margin(b = 10)
                ),
                plot.caption = element_text(
                    size = 10, colour = UHM_GREEN, hjust = 0,
                    margin = margin(t = 10)
                ),
                plot.margin = margin(12, 24, 8, 24)
            )

        legend_plot <- bi_legend(
            pal  = "DkViolet",
            dim  = 3,
            xlab = "Higher OC →",
            ylab = "More Holes →",
            size = 9
        )

        ggdraw() +
            draw_plot(map_plot, x = 0, y = 0, width = 1, height = 1) +
            draw_plot(legend_plot, x = 0.58, y = 0.08, width = 0.18, height = 0.18)
    }


    # === 4. EXECUTION ===

    cat("\n--- Script 7: Bivariate Cost vs. Golf Density (National) ---\n\n")
    dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

    for (f in c(PHASE1_CSV, PHASE2_CSV, IMPUTED_PATHS)) {
        if (!file.exists(f)) stop(sprintf("[FATAL] Input not found:\n  %s", f))
    }


    #  Step 1: Build county lookup

    cat("[Step 1] Building county lookup from Phase 1 baseline...\n")
    phase1_df <- read_csv(PHASE1_CSV, show_col_types = FALSE)
    county_lookup <- phase1_df |>
        select(Longitude, Latitude, FIPS, County_Name, State_Abbr) |>
        distinct() |>
        mutate(FIPS = sprintf("%05d", as.integer(FIPS)))
    cat(sprintf("  %d unique course coordinates loaded.\n", nrow(county_lookup)))


    #  Step 2: Pool opportunity costs across M imputations

    cat("[Step 2] Using global Tri-Language Grand Means for County.\n")
    pooled_county <- grand_means$county$GrandMean

    cat(sprintf(
        "\n  Pooled national total: $%.3fT across %d counties\n",
        sum(pooled_county$pooled_opp_cost) / 1e12,
        nrow(pooled_county)
    ))


    #  Step 3: Total holes per county from imputation 1
    # Holes is not a MICE-imputed variable; all 100 draws are identical.

    cat("\n[Step 3] Computing total holes per county (imputation 1)...\n")
    county_holes_mice <- read_csv(IMPUTED_PATHS[1], show_col_types = FALSE) |>
        left_join(county_lookup, by = c("Longitude", "Latitude")) |>
        filter(!is.na(FIPS)) |>
        group_by(FIPS) |>
        summarise(total_holes = sum(Holes, na.rm = TRUE), .groups = "drop")

    county_data_list <- list()
    for (model_name in names(grand_means$county)) {
        county_data_list[[model_name]] <- inner_join(
            grand_means$county[[model_name]], county_holes_mice,
            by = "FIPS"
        )
    }


    #  Step 4: Read county and state boundaries (shared between both maps)

    cat("\n[Step 4] Reading county and state boundaries (vendored, GENZ2022 500k)...\n")
    # [METHODOLOGY] vendored Census cartographic boundary files (1:500k, GENZ2022);
    #               shift_geometry() repositions AK and HI as insets; st_transform
    #               to EPSG 5070 (NAD83 / Conus Albers) for equal-area national display.
    COUNTY_500K_SHP <- file.path(WORK_DIR, "00 - Data Sources", "Secondary",
                                  "cb_2022_us_county_500k", "cb_2022_us_county_500k.shp")
    STATE_500K_SHP  <- file.path(WORK_DIR, "00 - Data Sources", "Secondary",
                                  "cb_2022_us_state_500k", "cb_2022_us_state_500k.shp")
    counties_sf <- st_read(COUNTY_500K_SHP, quiet = TRUE) |>
        filter(!STATEFP %in% TERRITORY_EXCLUDE_FP) |>
        shift_geometry() |>
        st_transform(5070)
    cat(sprintf("  %d counties loaded with AK/HI insets (EPSG 5070).\n", nrow(counties_sf)))

    # [METHODOLOGY] Same CRS pipeline applied to state boundaries for overlay alignment.
    states_sf <- st_read(STATE_500K_SHP, quiet = TRUE) |>
        filter(!STATEFP %in% TERRITORY_EXCLUDE_FP) |>
        shift_geometry() |>
        st_transform(5070)
    cat(sprintf("  %d states loaded with AK/HI insets.\n", nrow(states_sf)))


    #  Step 5: Render + save Map 7.1 (MICE-pooled)

    cat("\n[Step 5] Rendering Map 7.1: MICE-pooled bivariate maps (4 variations)...\n")

    for (model_name in names(county_data_list)) {
        cat(sprintf("  -> Rendering %s bivariate map...\n", model_name))
        final_plot1 <- build_bivariate_map(
            counties_sf = counties_sf,
            states_sf = states_sf,
            county_data = county_data_list[[model_name]],
            subtitle = paste0(
                "Bivariate map (", model_name, ")  │  X: total OC  ",
                "│  Y: total holes  │  AK and HI shown as insets"
            ),
            caption_text = paste0(
                "OC = OSM acreage × baseline land value per acre. ",
                "Tertile quantile breaks applied independently to each dimension. ",
                "Counties without golf courses shown in gray. CRS: NAD83/Conus Albers (EPSG 5070). ",
                "Alaska and Hawaii repositioned as insets."
            )
        )
        lang_code <- c("GrandMean" = "4", "Python" = "2", "R" = "3", "Julia" = "1")[[model_name]]
        sub_dir <- if (lang_code %in% c("4", "0")) "Final_Thesis_Figures" else "QA_Verification"
        out_file <- sub("X", lang_code, OUT_PNG1)
        out_file <- sub("\\.png$", paste0("_", model_name, ".png"), out_file)
        out_file <- sub("output", paste0("output/", sub_dir), out_file)

        ggsave(out_file, final_plot1, width = 14, height = 9, dpi = 300, units = "in")
        cat(sprintf("  Saved: %s\n", out_file))
    }


    #  Step 6: Observed-only county data (OC + Holes restricted to measured courses)
    # [METHODOLOGY] Phase 2 acreage_source identifies directly measured courses
    #               (acreage_source != "MICE_Target"). Both OC and Holes are aggregated
    #               from the same observed-course subset so the two bivariate dimensions
    #               remain consistent - only counties with known acreage data contribute
    #               to either axis. Since Holes is not a MICE-imputed variable, filtering
    #               here has the effect of restricting to counties where we also have
    #               observed acreage, enabling a clean like-for-like comparison.

    cat("\n[Step 6] Computing observed-only county OC + Holes from Phase 2...\n")

    obs_county_data <- read_csv(PHASE2_CSV, show_col_types = FALSE) |>
        filter(acreage_source != "MICE_Target") |>
        mutate(
            opp_cost = final_acreage * Baseline_Value_Per_Acre,
            FIPS     = sprintf("%05d", as.integer(FIPS))
        ) |>
        filter(!is.na(FIPS)) |>
        group_by(FIPS, County_Name, State_Abbr) |>
        summarise(
            pooled_opp_cost = sum(opp_cost, na.rm = TRUE),
            total_holes = sum(Holes, na.rm = TRUE),
            .groups = "drop"
        )

    cat(sprintf(
        "  Observed-only: %d counties  |  national OC $%.3fT\n",
        nrow(obs_county_data),
        sum(obs_county_data$pooled_opp_cost) / 1e12
    ))
    cat(sprintf(
        "  Coverage vs. MICE-pooled: %.1f%%\n",
        sum(obs_county_data$pooled_opp_cost) /
            sum(county_data_list$GrandMean$pooled_opp_cost) * 100
    ))


    #  Step 7: Render + save Map 7.2 (Observed-only)

    cat("\n[Step 7] Rendering Map 7.2: Observed-only bivariate map...\n")

    final_plot2 <- build_bivariate_map(
        counties_sf = counties_sf,
        states_sf = states_sf,
        county_data = obs_county_data,
        subtitle = paste0(
            "Bivariate map  │  X: total OC (observed acreage only)  ",
            "│  Y: total holes  │  AK and HI shown as insets"
        ),
        caption_text = paste0(
            "OC restricted to courses with directly measured OSM polygon acreage ",
            "(acreage_source ≠ MICE_Target). Holes aggregated from the same observed ",
            "subset. Tertile breaks applied independently per dimension. ",
            "CRS: NAD83/Conus Albers (EPSG 5070). Alaska and Hawaii repositioned as insets."
        )
    )
    ggsave(OUT_PNG2, final_plot2, width = 14, height = 9, dpi = 300, units = "in")
    cat(sprintf("\n  Saved: output/%s\n", basename(OUT_PNG2)))

    cat("--- Done ---\n")
    gc()
}


# ---------- Script 8: LaTeX Tables ----------
run_8_LaTeX_Tables <- function() {
    # === 2. GLOBALS & PATHS ===

    SCRIPT_DIR <- this.path::this.dir()
    WORK_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
    ACREAGE_CSV <- file.path(
        WORK_DIR, "Phase 3 Economic Merge and MICE Imputation",
        "Data", "R", "R_National_Acreage_Summary.csv"
    )
    REGRESSION_R_CSV <- file.path(
        WORK_DIR, "Phase 4 Econometric Modeling", "Data", "R",
        "R_Regression_Results.csv"
    )
    REGRESSION_PY_CSV <- file.path(
        WORK_DIR, "Phase 4 Econometric Modeling", "Data", "python",
        "Py_Regression_Results.csv"
    )
    REGRESSION_JL_CSV <- file.path(
        WORK_DIR, "Phase 4 Econometric Modeling", "Data", "Julia",
        "Jl_Regression_Results.csv"
    )
    HAWAII_CSV <- file.path(
        WORK_DIR, "Phase 5 The Hawaii Micro-Case Study", "Data", "R",
        "Phase5_Geographic_Breakdown.csv"
    )
    OUTPUT_DIR <- file.path(SCRIPT_DIR, "output")
    THESIS_DIR <- file.path(OUTPUT_DIR, "Final_Thesis_Figures")
    QA_DIR <- file.path(OUTPUT_DIR, "QA_Verification")
    dir.create(THESIS_DIR, showWarnings = FALSE, recursive = TRUE)
    dir.create(QA_DIR, showWarnings = FALSE, recursive = TRUE)
    OUT_TEX1 <- file.path(THESIS_DIR, "8.141_Table1_Acreage.tex")
    OUT_TEX2 <- file.path(THESIS_DIR, "8.241_Table2_Regression.tex")
    OUT_TEX3 <- file.path(THESIS_DIR, "8.301_Table3_Hawaii_Geo.tex")


    # === 3. FUNCTIONS ===

    # Escape LaTeX special characters in a character vector.
    # Backslash is processed first to avoid double-escaping characters introduced
    # by the subsequent substitutions.
    latex_escape <- function(x) {
        x <- gsub("\\\\", "\\\\textbackslash{}", x)
        x <- gsub("%", "\\\\%", x, fixed = TRUE)
        x <- gsub("\\$", "\\\\$", x)
        x <- gsub("_", "\\\\_", x, fixed = TRUE)
        x <- gsub("&", "\\\\&", x, fixed = TRUE)
        x <- gsub("#", "\\\\#", x, fixed = TRUE)
        x <- gsub("\\^", "\\\\^{}", x)
        x <- gsub("~", "\\\\~{}", x, fixed = TRUE)
        x
    }

    # Format p-values: values below 0.001 rendered as "$<$ 0.001" (math-mode <),
    # all others rounded to 3 decimal places.
    fmt_pval <- function(p) {
        ifelse(p < 0.001, "$<$ 0.001", sprintf("%.3f", p))
    }


    # === 4. EXECUTION ===

    cat("\n--- Script 8: LaTeX Table Generation ---\n\n")
    dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

    for (f in c(
        ACREAGE_CSV, REGRESSION_R_CSV, REGRESSION_PY_CSV,
        REGRESSION_JL_CSV, HAWAII_CSV
    )) {
        if (!file.exists(f)) stop(sprintf("[FATAL] Input not found:\n  %s", f))
    }


    #  4.1  Table 1: National Acreage Summary

    cat("[Table 1] Formatting national acreage summary...\n")

    acreage_raw <- read_csv(ACREAGE_CSV, show_col_types = FALSE)
    cat(sprintf("  %d rows loaded.\n", nrow(acreage_raw)))

    acreage_tbl <- acreage_raw |>
        select(Category, County_Type, Pooled_Acres, SD_Between,
               CI_99_Lower_Acres, CI_99_Upper_Acres) |>
        mutate(
            Category = latex_escape(Category),
            County_Type = latex_escape(County_Type),
            across(where(is.numeric), ~ format(
                round(.x, 1),
                big.mark = ",", nsmall = 1, trim = TRUE
            ))
        )

    tbl1 <- kable(
        acreage_tbl,
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        caption = "National Golf Course Acreage Summary (MICE-Pooled, $M = 100$)",
        label = "acreage_summary",
        col.names = c(
            "Category", "County Type",
            "Pooled Acres", "SD (Between)",
            "99\\% CI Lower", "99\\% CI Upper"
        )
    ) |>
        kable_styling(latex_options = c("hold_position")) |>
        footnote(
            general = paste0(
                "99\\% confidence intervals reported; ",
                "95\\% intervals available in the replication package."
            ),
            general_title     = "",
            footnote_as_chunk = TRUE,
            escape            = FALSE
        )

    writeLines(as.character(tbl1), OUT_TEX1)
    cat(sprintf("  Saved: %s\n\n", basename(OUT_TEX1)))


    #  4.2  Table 2: Rubin's Rules Regression Results (Tri-Language)

    cat("[Table 2] Formatting tri-language regression results (Py / R / Jl)...\n")

    prep_reg <- function(path) {
        read_csv(path, show_col_types = FALSE) |>
            mutate(
                Parameter = case_when(
                    Parameter == "(Intercept)" ~ "Intercept",
                    Parameter == "Holes" ~ "Holes",
                    Parameter == "factor(county_type)Urban"  ~ "Urban County",
                    Parameter == "C(county_type)[T.Urban]"  ~ "Urban County",
                    Parameter == "county_type: Urban"       ~ "Urban County",
                    TRUE ~ latex_escape(Parameter)
                ),
                Coef = sprintf("%.3f", Coef),
                SE = sprintf("%.3f", Std_Error),
                p = fmt_pval(p_value)
            ) |>
            select(Parameter, Coef, SE, p, Sig)
    }

    reg_py <- prep_reg(REGRESSION_PY_CSV)
    reg_r <- prep_reg(REGRESSION_R_CSV)
    reg_jl <- prep_reg(REGRESSION_JL_CSV)
    cat(sprintf(
        "  Parameters loaded - Py: %d  R: %d  Jl: %d\n",
        nrow(reg_py), nrow(reg_r), nrow(reg_jl)
    ))

    reg_tri <- reg_py |>
        rename(Coef_Py = Coef, SE_Py = SE, p_Py = p, Sig_Py = Sig) |>
        inner_join(
            reg_r |> rename(Coef_R = Coef, SE_R = SE, p_R = p, Sig_R = Sig),
            by = "Parameter"
        ) |>
        inner_join(
            reg_jl |> rename(Coef_Jl = Coef, SE_Jl = SE, p_Jl = p, Sig_Jl = Sig),
            by = "Parameter"
        )

    cat("  Parameter labels:\n")
    for (p in reg_tri$Parameter) cat(sprintf("    %s\n", p))

    tbl2 <- kable(
        reg_tri,
        format = "latex",
        booktabs = TRUE,
        escape = FALSE,
        caption = paste0(
            "MICE-Pooled OLS Regression Results (Rubin's Rules, $M = 300$: ",
            "100 Python, 100 R, 100 Julia). ",
            "Dep.\\ var.: $\\log(\\text{Opportunity\\_Cost})$."
        ),
        label = "regression_results",
        col.names = c(
            "Parameter",
            "Coef.", "SE", "$p$", "Sig.",
            "Coef.", "SE", "$p$", "Sig.",
            "Coef.", "SE", "$p$", "Sig."
        )
    ) |>
        kable_styling(
            latex_options = c("hold_position", "scale_down"),
            font_size     = 9
        ) |>
        add_header_above(c(" " = 1, "Python" = 4, "R" = 4, "Julia" = 4)) |>
        footnote(
            general = paste0(
                "Sig.\\ codes: *** $p < 0.001$. ",
                "Rubin's Rules applied independently per language group ($M = 100$ each). ",
                "Grand Mean = arithmetic mean of three independently pooled estimates."
            ),
            general_title = "Note: ",
            escape = FALSE,
            threeparttable = TRUE
        )

    writeLines(as.character(tbl2), OUT_TEX2)
    cat(sprintf("  Saved: %s\n\n", basename(OUT_TEX2)))


    #  4.3  Table 3: Hawaii Geographic Breakdown

    cat("[Table 3] Formatting Hawaii geographic breakdown...\n")

    hawaii_raw <- read_csv(HAWAII_CSV, show_col_types = FALSE)
    cat(sprintf("  %d districts loaded.\n", nrow(hawaii_raw)))

    hawaii_tbl <- hawaii_raw |>
        mutate(
            District_Name        = latex_escape(District_Name),
            Pct_of_Total_Parcels = sprintf("%.1f\\%%", Pct_of_Total_Parcels)
        ) |>
        select(Zone_Code, District_Name, Parcel_Count, Pct_of_Total_Parcels)

    tbl3 <- kable(
        hawaii_tbl,
        format    = "latex",
        booktabs  = TRUE,
        escape    = FALSE,
        caption   = "Hawaii Golf Course Parcel Distribution by Geographic Zone",
        label     = "hawaii_geo",
        col.names = c("Zone", "District", "Parcels", "Share (\\%)")
    ) |>
        kable_styling(latex_options = c("hold_position"))

    writeLines(as.character(tbl3), OUT_TEX3)
    cat(sprintf("  Saved: %s\n\n", basename(OUT_TEX3)))

    cat("--- Done ---\n")
    gc()
}


# ---------- Script 9: Oahu Opportunity Cost Map ----------
run_9_Oahu_Opportunity_Cost_Map <- function() {
    # === 2. GLOBALS & PATHS ===

    SCRIPT_DIR <- this.path::this.dir()
    WORK_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
    PHASE5_DATA <- file.path(WORK_DIR, "Phase 5 The Hawaii Micro-Case Study", "Data", "R")
    POLYGONS_GPKG <- file.path(PHASE5_DATA, "Target_Golf_Polygons.gpkg")
    PARCELS_GPKG <- file.path(PHASE5_DATA, "Honolulu_Parcels_Reprojected.gpkg")
    PHASE2_CSV <- file.path(
        WORK_DIR, "Phase 2 Spatial Polygons and True Acreage", "Data", "R",
        "R_Phase2_Acreage_Matched_v2.csv"
    )
    PHASE3_R_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "R")
    PHASE3_PY_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "python")
    PHASE3_JL_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "Julia")
    R_IMPUTED_PATHS <- file.path(PHASE3_R_DIR, paste0("R_Imputed_Dataset_", 1:100, ".csv"))
    PY_IMPUTED_PATHS <- file.path(PHASE3_PY_DIR, paste0("Py_Imputed_Dataset_", 1:100, ".csv"))
    JL_IMPUTED_PATHS <- file.path(PHASE3_JL_DIR, paste0("Jl_Imputed_Dataset_", 1:100, ".csv"))
    # P5-12 (2026-07-28): hand-verified crosswalk, replaces this script's own nearest-feature
    # polygon join -- see join_oc_to_polygons below.
    CROSSWALK_CSV <- file.path(
        WORK_DIR, "Phase 5 The Hawaii Micro-Case Study", "Data", "Oahu_Course_Polygon_Crosswalk.csv"
    )
    PHASE1_R_PATH <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "R", "R_Phase1_Baseline_Golf_Valuation.csv"
    )
    PHASE1_PY_PATH <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "python", "Py_Phase1_Baseline_Golf_Valuation.csv"
    )
    PHASE1_JL_PATH <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "Julia", "Jl_Phase1_Baseline_Golf_Valuation.csv"
    )
    OUTPUT_DIR <- file.path(SCRIPT_DIR, "output")
    THESIS_DIR <- file.path(OUTPUT_DIR, "Final_Thesis_Figures")
    QA_DIR <- file.path(OUTPUT_DIR, "QA_Verification")
    dir.create(THESIS_DIR, showWarnings = FALSE, recursive = TRUE)
    dir.create(QA_DIR, showWarnings = FALSE, recursive = TRUE)
    OUT_PNG1 <- file.path(THESIS_DIR, "9.141_Oahu_Opportunity_Cost_Map_GrandMean.png")
    OUT_PNG2 <- file.path(THESIS_DIR, "9.101_Oahu_Opportunity_Cost_Map_ObservedOnly.png")

    M <- 100L
    OAHU_CRS <- 32604L # WGS 84 / UTM Zone 4N

    OAHU_LAT_MIN <- 21.2
    OAHU_LAT_MAX <- 21.9
    OAHU_LON_MIN <- -158.5
    OAHU_LON_MAX <- -157.6

    JOIN_DIST_CAP <- 500L


    # === 3. FUNCTIONS ===

    # Auto-scale dollar labels: $B if >= 1B, else $M.
    label_oc <- function(x) {
        if_else(x >= 1e9,
            sprintf("$%.1fB", x / 1e9),
            sprintf("$%.0fM", x / 1e6)
        )
    }

    # Column-agnostic acreage extractor: returns the correct acreage vector from a
    # data frame regardless of which column name the language group uses.
    # R datasets use final_acreage; Python/Julia datasets use osm_acreage.
    # Called via pick(everything()) inside mutate() so the full data frame is available.
    get_acreage <- function(df) {
        if ("osm_acreage" %in% names(df)) df[["osm_acreage"]] else df[["final_acreage"]]
    }

    # P5-12 (2026-07-28): st_nearest_feature()'s unverified many-to-one geometric snap
    # silently merged distinct, adjacent courses (Kahuku, Hoakalei, Ted Makalena -> the
    # wrong polygon) both in Phase 5's own Step 3 and in this script's polygon join.
    # build_crosswalk_keeplist() replicates Phase 5 Step 3's crosswalk-based course
    # identification for one language's own Phase 1 baseline (Course_Name, coordinates
    # rounded to 6dp -- P5-01 already found exact-float coordinate joins silently drop
    # rows across a CSV round-trip; the crosswalk itself is language-agnostic (one file,
    # shared by all three), only the Phase 1 Course_Name/coordinate join differs per language.
    # P5-17 (2026-07-28): Python/Julia's Phase 1 Course_Name carries a "-City,State" suffix
    # R's does not ("Pearl Country Club-Aiea,HI" vs "Pearl Country Club") -- joining the
    # crosswalk (R-sourced Course_Name) against Python/Julia's own baseline by Course_Name
    # matches zero rows. Resolve the crosswalk against R's baseline for canonical
    # coordinates (VERIFIED: rounded coordinates match 37-of-37 across all three languages'
    # independent Phase 1 outputs), then match those coordinates into phase1_path's own
    # Holes/Baseline_Value_Per_Acre -- works for R's own call too (coordinates match
    # themselves trivially), so one code path serves all three languages.
    build_crosswalk_keeplist <- function(phase1_path, crosswalk, phase1_r_path) {
        baseline_r <- read_csv(phase1_r_path, show_col_types = FALSE) |>
            filter(County_Name == "Honolulu" | FIPS == 15003) |>
            mutate(lon6 = round(Longitude, 6), lat6 = round(Latitude, 6)) |>
            select(Course_Name, lon6, lat6)
        baseline_own <- read_csv(phase1_path, show_col_types = FALSE) |>
            filter(County_Name == "Honolulu" | FIPS == 15003) |>
            mutate(lon6 = round(Longitude, 6), lat6 = round(Latitude, 6)) |>
            select(lon6, lat6, Holes)
        baseline <- baseline_r |> left_join(baseline_own, by = c("lon6", "lat6"))
        crosswalk |>
            left_join(baseline, by = "Course_Name") |>
            mutate(group_id = ifelse(
                is.na(Poly_OSM_ID),
                paste0("solo_", Course_Name),
                paste0("osmid_", Poly_OSM_ID)
            )) |>
            # Makaha Valley/Makaha Resort share a polygon and are genuinely ambiguous
            # (crosswalk Notes); keep the higher-Holes record.
            arrange(group_id, desc(Holes)) |>
            filter(!duplicated(group_id)) |>
            select(group_id, lon6, lat6, Holes, Poly_OSM_ID)
    }

    # Direct crosswalk join: attach pooled_opp_cost to each polygon by matching its own
    # osm_id against the pooled group's Poly_OSM_ID -- no distance, no snapping, no cap.
    # Solo/orphan courses (no Poly_OSM_ID, e.g. Barbers Point, Luana Hills) have no
    # polygon to render onto and are correctly left unmatched (gray on the map), same as
    # a course that exceeded the old distance cap -- but for a documented reason instead
    # of a silent one.
    join_oc_to_polygons <- function(polygons_sf, pooled_df) {
        polygons_sf |>
            mutate(osm_id_num = as.numeric(osm_id)) |>
            left_join(
                pooled_df |> filter(!is.na(Poly_OSM_ID)) |> select(Poly_OSM_ID, pooled_opp_cost),
                by = c("osm_id_num" = "Poly_OSM_ID")
            ) |>
            select(-osm_id_num)
    }

    # Filter one language's imputed datasets to the Oahu bounding box, identify courses
    # via the crosswalk-based keep_list (not raw bbox + no dedup), compute OC per course
    # for each imputation, then return Rubin's q_bar (mean), keyed by the crosswalk's
    # stable group_id (not exact-float Longitude/Latitude -- see P5-01).
    pool_oahu_oc <- function(paths, lang_label, keep_list) {
        total_list <- vector("list", M)
        cat(sprintf("  [%s] Pooling %d imputations (crosswalk-identified)...\n", lang_label, M))
        for (i in seq_len(M)) {
            df <- read_csv(paths[i], show_col_types = FALSE) |>
                filter(
                    Latitude >= OAHU_LAT_MIN, Latitude <= OAHU_LAT_MAX,
                    Longitude >= OAHU_LON_MIN, Longitude <= OAHU_LON_MAX
                ) |>
                mutate(
                    acreage  = get_acreage(pick(everything())),
                    opp_cost = acreage * Baseline_Value_Per_Acre,
                    lon6 = round(Longitude, 6),
                    lat6 = round(Latitude, 6)
                ) |>
                inner_join(keep_list, by = c("lon6", "lat6", "Holes"))
            total_list[[i]] <- df |>
                group_by(group_id) |>
                summarise(
                    total_opp_cost = sum(opp_cost, na.rm = TRUE),
                    Poly_OSM_ID     = first(Poly_OSM_ID),
                    .groups        = "drop"
                ) |>
                mutate(imputation = i)
            rm(df)
            gc()
        }
        # [METHODOLOGY] Rubin's Rules q_bar: mean of per-imputation course-level OC
        #               values across M = 100 imputations for this language group.
        bind_rows(total_list) |>
            group_by(group_id) |>
            summarise(
                pooled_opp_cost = mean(total_opp_cost, na.rm = TRUE),
                Poly_OSM_ID     = first(Poly_OSM_ID),
                .groups         = "drop"
            )
    }

    # Render Oahu OC micro-map and return the ggplot object.
    build_oahu_oc_map <- function(golf_oc_sf, oahu_outline_sf,
                                    n_matched, subtitle, caption_text) {
        ggplot() +
            geom_sf(
                data      = oahu_outline_sf,
                fill      = "#e8e8e8",
                colour    = "#aaaaaa",
                linewidth = 0.35
            ) +
            geom_sf(
                data = golf_oc_sf,
                aes(fill = pooled_opp_cost),
                colour = NA
            ) +
            scale_fill_viridis_c(
                option = "magma",
                na.value = "#cccccc",
                name = "Opportunity Cost",
                labels = label_oc,
                guide = guide_colorbar(
                    barwidth       = unit(0.5, "cm"),
                    barheight      = unit(5, "cm"),
                    title.position = "top",
                    title.hjust    = 0.5,
                    ticks.colour   = "white"
                )
            ) +
            annotation_scale(
                location   = "tl",
                width_hint = 0.22,
                style      = "ticks",
                text_cex   = 0.75,
                pad_x      = unit(0.5, "cm"),
                pad_y      = unit(0.5, "cm")
            ) +
            annotation_north_arrow(
                location    = "tl",
                which_north = "true",
                pad_x       = unit(0.5, "cm"),
                pad_y       = unit(1.5, "cm"),
                style       = north_arrow_fancy_orienteering(
                    fill      = c("white", "#444444"),
                    line_col  = "#444444",
                    text_col  = "#444444",
                    text_size = 8
                )
            ) +
            labs(
                title    = "Golf Course Opportunity Cost - Oahu, Hawaiʻi",
                subtitle = subtitle,
                caption  = stringr::str_wrap(caption_text, width = 185)
            ) +
            theme_void(base_size = 12) +
            theme(
                plot.title = element_text(
                    face = "bold", size = 16, hjust = 0.5, margin = margin(b = 4)
                ),
                plot.subtitle = element_text(
                    size = 11, hjust = 0.5, colour = UHM_GREEN, margin = margin(b = 8)
                ),
                plot.caption = element_text(
                    size = 9, colour = UHM_GREEN, hjust = 0, margin = margin(t = 10)
                ),
                plot.caption.position = "plot",
                legend.position  = "right",
                legend.direction = "vertical",
                legend.title     = element_text(size = 9, face = "bold"),
                legend.text      = element_text(size = 8),
                plot.background = element_rect(fill = "white", colour = NA),
                plot.margin = margin(12, 16, 8, 16)
            )
    }


    # === 4. EXECUTION ===

    cat("\n--- Script 9: Oahu Golf Course Opportunity Cost ---\n\n")
    dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

    for (f in c(
        POLYGONS_GPKG, PARCELS_GPKG, PHASE2_CSV, CROSSWALK_CSV,
        PHASE1_R_PATH, PHASE1_PY_PATH, PHASE1_JL_PATH,
        R_IMPUTED_PATHS, PY_IMPUTED_PATHS, JL_IMPUTED_PATHS
    )) {
        if (!file.exists(f)) stop(sprintf("[FATAL] Input not found:\n  %s", f))
    }

    crosswalk_raw <- read_csv(CROSSWALK_CSV, show_col_types = FALSE)
    keep_list_r  <- build_crosswalk_keeplist(PHASE1_R_PATH,  crosswalk_raw, PHASE1_R_PATH)
    keep_list_py <- build_crosswalk_keeplist(PHASE1_PY_PATH, crosswalk_raw, PHASE1_R_PATH)
    keep_list_jl <- build_crosswalk_keeplist(PHASE1_JL_PATH, crosswalk_raw, PHASE1_R_PATH)


    #  Step 1: Filter imputed datasets to Oahu, identify courses via the crosswalk, and
    #          pool OC per language; compute Grand Mean

    cat("[Step 1] Filtering imputed datasets to Oahu and pooling OC (tri-language, crosswalk-identified)...\n")
    pooled_r <- pool_oahu_oc(R_IMPUTED_PATHS, "R", keep_list_r)
    pooled_py <- pool_oahu_oc(PY_IMPUTED_PATHS, "Py", keep_list_py)
    pooled_jl <- pool_oahu_oc(JL_IMPUTED_PATHS, "Jl", keep_list_jl)

    # [METHODOLOGY] Grand Mean = arithmetic mean of three independently Rubin-pooled
    #               OC vectors (M=100 each). full_join on the crosswalk's group_id --
    #               a stable string key shared identically across all three languages'
    #               copies of the crosswalk -- replaces the P5-01-identified exact-float
    #               (Longitude, Latitude) join, which silently dropped 2 of 39 courses to
    #               floating-point mismatch across the R/Python/Julia CSV round-trip.
    pooled_oahu <- pooled_r |>
        select(group_id, Poly_OSM_ID, oc_r = pooled_opp_cost) |>
        full_join(
            pooled_py |> select(group_id, oc_py = pooled_opp_cost),
            by = "group_id"
        ) |>
        full_join(
            pooled_jl |> select(group_id, oc_jl = pooled_opp_cost),
            by = "group_id"
        ) |>
        mutate(
            pooled_opp_cost = rowMeans(cbind(oc_r, oc_py, oc_jl), na.rm = TRUE)
        ) |>
        select(group_id, Poly_OSM_ID, pooled_opp_cost)

    cat(sprintf(
        "\n  Grand Mean Oahu total: $%.3fB across %d courses\n",
        sum(pooled_oahu$pooled_opp_cost, na.rm = TRUE) / 1e9,
        nrow(pooled_oahu)
    ))
    cat(sprintf(
        "  OC range: $%.1fM - $%.1fM\n",
        min(pooled_oahu$pooled_opp_cost, na.rm = TRUE) / 1e6,
        max(pooled_oahu$pooled_opp_cost, na.rm = TRUE) / 1e6
    ))


    #  Step 2: Load OSM golf course polygons

    cat("\n[Step 2] Loading OSM golf course polygons...\n")
    # [METHODOLOGY] Spatial read of Target_Golf_Polygons.gpkg (OSM-derived golf course
    #               footprints); st_transform to EPSG 32604 (WGS 84 / UTM Zone 4N)
    #               centres the projection on Oahu for accurate distance calculations.
    golf_polygons_sf <- st_read(POLYGONS_GPKG, quiet = TRUE) |>
        st_transform(OAHU_CRS)
    cat(sprintf(
        "  %d golf course polygons loaded (EPSG %d).\n",
        nrow(golf_polygons_sf), OAHU_CRS
    ))


    #  Step 3: Crosswalk join - Grand Mean group_ids to polygons (P5-12)

    cat("\n[Step 3] Crosswalk join for Map 9.1 (Grand Mean -> polygons)...\n")

    # [METHODOLOGY] join_oc_to_polygons now matches each polygon to its pooled OC by its
    # own osm_id (P5-12 crosswalk), replacing st_nearest_feature() -- no distance, no cap,
    # no risk of snapping to a geometrically-closer-but-wrong neighboring course.
    golf_oc_mice <- join_oc_to_polygons(golf_polygons_sf, pooled_oahu)

    n_matched_mice <- sum(!is.na(golf_oc_mice$pooled_opp_cost))
    n_unmatched_mice <- sum(is.na(golf_oc_mice$pooled_opp_cost))
    cat(sprintf(
        "  %d polygons matched via crosswalk  |  %d unmatched (gray -- no baseline course resolved to this polygon, e.g. P5-16).\n",
        n_matched_mice, n_unmatched_mice
    ))


    #  Step 4: Build Oahu island base (shared between both maps)

    cat("\n[Step 4] Dissolving Honolulu parcels to island outline (~20 sec)...\n")
    # [METHODOLOGY] Spatial read of Honolulu_Parcels_Reprojected.gpkg; st_transform to
    #               EPSG 32604; st_union() dissolves all cadastral boundaries into a
    #               single island outline. Consistent with Script 3.
    oahu_outline_sf <- st_read(PARCELS_GPKG, quiet = TRUE) |>
        st_transform(OAHU_CRS) |>
        st_geometry() |>
        st_union() |>
        st_sf()
    cat("  Island outline complete.\n")


    #  Step 5: Render + save Map 9.1 (MICE-pooled)

    cat("\n[Step 5] Rendering Map 9.1: MICE-pooled Oahu map...\n")

    map1 <- build_oahu_oc_map(
        golf_oc_sf = golf_oc_mice,
        oahu_outline_sf = oahu_outline_sf,
        n_matched = n_matched_mice,
        subtitle = sprintf(
            paste0(
                "%d courses  │  Grand Mean of Py/R/Jl Rubin-pooled estimates",
                " (M = 300: 100 each)  │  OSM polygon boundaries"
            ),
            n_matched_mice
        ),
        caption_text = paste0(
            "Opportunity Cost = Grand Mean of three independently Rubin-pooled OC estimates ",
            "(100 Python, 100 R, 100 Julia MICE imputations). ",
            "Polygon-to-course assignment via the hand-verified Oahu crosswalk (P5-12).\n",
            "Sources: OpenStreetMap; FHFA residential land price index (urban); ",
            "USDA agricultural land values (rural). CRS: WGS 84 / UTM Zone 4N (EPSG 32604)."
        )
    )
    ggsave(OUT_PNG1, map1, width = 12, height = 10, dpi = 300, units = "in")
    cat(sprintf("  Saved: output/%s\n", basename(OUT_PNG1)))


    #  Step 6: Observed-only Oahu OC from Phase 2

    cat("\n[Step 6] Computing observed-only Oahu OC from Phase 2 (crosswalk-identified)...\n")

    # [METHODOLOGY] Phase 2 acreage_source identifies directly measured courses.
    #               Filtering to acreage_source != "MICE_Target" and the Oahu
    #               bounding box yields observed-only OC values with no imputation.
    obs_oahu <- read_csv(PHASE2_CSV, show_col_types = FALSE) |>
        filter(
            acreage_source != "MICE_Target",
            Latitude >= OAHU_LAT_MIN, Latitude <= OAHU_LAT_MAX,
            Longitude >= OAHU_LON_MIN, Longitude <= OAHU_LON_MAX
        ) |>
        mutate(
            opp_cost = final_acreage * Baseline_Value_Per_Acre,
            lon6 = round(Longitude, 6),
            lat6 = round(Latitude, 6)
        ) |>
        # P5-12: identify by the R crosswalk (PHASE2_CSV is R-sourced), not raw coordinates,
        # so a course split across duplicate/adjacent records still resolves to one polygon.
        inner_join(keep_list_r |> select(group_id, lon6, lat6, Poly_OSM_ID), by = c("lon6", "lat6")) |>
        group_by(group_id) |>
        summarise(
            pooled_opp_cost = sum(opp_cost, na.rm = TRUE),
            Poly_OSM_ID     = first(Poly_OSM_ID),
            .groups         = "drop"
        )

    cat(sprintf(
        "  Observed-only: %d courses, Oahu total $%.3fB\n",
        nrow(obs_oahu),
        sum(obs_oahu$pooled_opp_cost) / 1e9
    ))
    cat(sprintf(
        "  Coverage vs. MICE-pooled: %.1f%%\n",
        sum(obs_oahu$pooled_opp_cost) / sum(pooled_oahu$pooled_opp_cost) * 100
    ))


    #  Step 7: Crosswalk join - observed-only group_ids to polygons

    cat("\n[Step 7] Crosswalk join for Map 9.2 (observed -> polygons)...\n")

    golf_oc_obs <- join_oc_to_polygons(golf_polygons_sf, obs_oahu)

    n_matched_obs <- sum(!is.na(golf_oc_obs$pooled_opp_cost))
    n_unmatched_obs <- sum(is.na(golf_oc_obs$pooled_opp_cost))
    cat(sprintf(
        "  %d polygons matched via crosswalk  |  %d unmatched (gray -- no observed, non-MICE_Target course resolved to this polygon).\n",
        n_matched_obs, n_unmatched_obs
    ))


    #  Step 8: Render + save Map 9.2 (Observed-only)

    cat("\n[Step 8] Rendering Map 9.2: Observed-only Oahu map...\n")

    map2 <- build_oahu_oc_map(
        golf_oc_sf = golf_oc_obs,
        oahu_outline_sf = oahu_outline_sf,
        n_matched = n_matched_obs,
        subtitle = sprintf(
            "%d courses  │  Observed acreage only - no imputation  │  OSM polygon boundaries",
            n_matched_obs
        ),
        caption_text = paste0(
            "Opportunity Cost = directly measured OSM acreage × baseline land value per acre. ",
            "Restricted to courses with acreage_source ≠ MICE_Target. ",
            "Polygon-to-course assignment via the hand-verified Oahu crosswalk (P5-12).\n",
            "Sources: OpenStreetMap; FHFA residential land price index (urban); ",
            "USDA agricultural land values (rural). CRS: WGS 84 / UTM Zone 4N (EPSG 32604)."
        )
    )
    ggsave(OUT_PNG2, map2, width = 12, height = 10, dpi = 300, units = "in")
    cat(sprintf("  Saved: output/%s\n", basename(OUT_PNG2)))

    cat("--- Done ---\n")
    gc()
}


# ---------- Script 9b: Oahu OC Map Rural-USDA Sensitivity ----------
run_9b_Oahu_OC_Rural_USDA_Sensitivity <- function() {
    # === 2. GLOBALS & PATHS ===

    SCRIPT_DIR <- this.path::this.dir()
    WORK_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)

    PHASE5_DATA   <- file.path(WORK_DIR, "Phase 5 The Hawaii Micro-Case Study", "Data", "R")
    POLYGONS_GPKG <- file.path(PHASE5_DATA, "Target_Golf_Polygons.gpkg")
    PARCELS_GPKG  <- file.path(PHASE5_DATA, "Honolulu_Parcels_Reprojected.gpkg")

    PHASE1_R_PATH <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "R",
        "R_Phase1_Baseline_Golf_Valuation.csv"
    )
    PHASE1_PY_PATH <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "python", "Py_Phase1_Baseline_Golf_Valuation.csv"
    )
    PHASE1_JL_PATH <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "Julia", "Jl_Phase1_Baseline_Golf_Valuation.csv"
    )
    CROSSWALK_CSV <- file.path(
        WORK_DIR, "Phase 5 The Hawaii Micro-Case Study", "Data", "Oahu_Course_Polygon_Crosswalk.csv"
    )
    DEV_PLAN_GEOJSON <- file.path(
        WORK_DIR, "00 - Data Sources", "Honolulu", "Zoning_Map_Boundary.geojson"
    )

    PHASE3_R_DIR  <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "R")
    PHASE3_PY_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "python")
    PHASE3_JL_DIR <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "Julia")

    R_IMPUTED_PATHS  <- file.path(PHASE3_R_DIR,  paste0("R_Imputed_Dataset_",  1:100, ".csv"))
    PY_IMPUTED_PATHS <- file.path(PHASE3_PY_DIR, paste0("Py_Imputed_Dataset_", 1:100, ".csv"))
    JL_IMPUTED_PATHS <- file.path(PHASE3_JL_DIR, paste0("Jl_Imputed_Dataset_", 1:100, ".csv"))

    USDA_PATH <- file.path(
        WORK_DIR, "00 - Data Sources", "Original Data",
        "2022 - USDA County Data - Ag Use.csv"
    )

    OUTPUT_DIR <- file.path(SCRIPT_DIR, "output")
    THESIS_DIR <- file.path(OUTPUT_DIR, "Final_Thesis_Figures")
    dir.create(THESIS_DIR, showWarnings = FALSE, recursive = TRUE)
    OUT_PNG <- file.path(THESIS_DIR, "9b.141_Oahu_OC_Map_Rural_USDA_Sensitivity_GrandMean.png")

    M             <- 100L
    OAHU_CRS      <- 32604L
    OAHU_LAT_MIN  <- 21.2
    OAHU_LAT_MAX  <- 21.9
    OAHU_LON_MIN  <- -158.5
    OAHU_LON_MAX  <- -157.6
    JOIN_DIST_CAP <- 500L
    RURAL_ZONES   <- c("15", "16", "17", "18", "19", "20")
    HONOLULU_FIPS <- "15003"

    # Development Plan zone classification (ZONMAP_NO from Zoning_Map_Boundary.geojson):
    # 0     : No Coverage                   - no courses present
    # 1-14  : Urban/suburban Oahu core      - FHFA
    # 15-20 : Rural (unambiguously rural)   - USDA override
    # 21-24 : Windward Oahu (suburban/res.) - FHFA


    # === 3. FUNCTIONS ===

    label_oc <- function(x) {
        if_else(x >= 1e9,
            sprintf("$%.1fB", x / 1e9),
            sprintf("$%.0fM", x / 1e6)
        )
    }

    get_acreage <- function(df) {
        if ("osm_acreage" %in% names(df)) df[["osm_acreage"]] else df[["final_acreage"]]
    }

    # Read 2022 USDA agricultural land value ($/acre) for the given FIPS code.
    load_usda_value <- function(usda_path, fips) {
        df <- read_csv(usda_path, show_col_types = FALSE) |>
            filter(
                `Data Item` == "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / ACRE"
            ) |>
            mutate(
                FIPS = paste0(
                    str_pad(as.integer(`State ANSI`), 2, pad = "0"),
                    str_pad(as.integer(`County ANSI`), 3, pad = "0")
                ),
                Value_Numeric = as.numeric(gsub(",", "", Value))
            ) |>
            filter(FIPS == fips)
        if (nrow(df) == 0) stop(sprintf("[FATAL] USDA value for FIPS %s not found.", fips))
        df$Value_Numeric[[1]]
    }

    # Read FHFA residential land value ($/acre) from Phase 1 R baseline output.
    # Normalizes ALL Oahu BVPA before the rural override, correcting FIPS-NA draws
    # for Hawaii Kai and Mid-Pacific (MICE-imputed BVPA in Py/Jl datasets).
    load_fhfa_oahu_value <- function(path, fips) {
        df <- read_csv(path, show_col_types = FALSE) |>
            filter(FIPS == fips, !is.na(Baseline_Value_Per_Acre))
        if (nrow(df) == 0) stop(sprintf(
            "[FATAL] FHFA value for FIPS %s not found in Phase 1 R output.", fips
        ))
        df$Baseline_Value_Per_Acre[[1]]
    }

    # P5-12/P5-17 (2026-07-28): same crosswalk-based course identification as run_9 --
    # resolve the R-sourced crosswalk against R's baseline for canonical coordinates
    # (P5-17: Python/Julia's own Course_Name field doesn't match the crosswalk's), then
    # match those coordinates into phase1_path's own Holes/Baseline_Value_Per_Acre.
    build_crosswalk_keeplist <- function(phase1_path, crosswalk, phase1_r_path) {
        baseline_r <- read_csv(phase1_r_path, show_col_types = FALSE) |>
            filter(County_Name == "Honolulu" | FIPS == 15003) |>
            mutate(lon6 = round(Longitude, 6), lat6 = round(Latitude, 6)) |>
            select(Course_Name, lon6, lat6)
        baseline_own <- read_csv(phase1_path, show_col_types = FALSE) |>
            filter(County_Name == "Honolulu" | FIPS == 15003) |>
            mutate(lon6 = round(Longitude, 6), lat6 = round(Latitude, 6)) |>
            select(lon6, lat6, Holes)
        baseline <- baseline_r |> left_join(baseline_own, by = c("lon6", "lat6"))
        crosswalk |>
            left_join(baseline, by = "Course_Name") |>
            mutate(group_id = ifelse(
                is.na(Poly_OSM_ID),
                paste0("solo_", Course_Name),
                paste0("osmid_", Poly_OSM_ID)
            )) |>
            arrange(group_id, desc(Holes)) |>
            filter(!duplicated(group_id)) |>
            select(group_id, lon6, lat6, Holes, Poly_OSM_ID)
    }

    # Build an osm_id -> Zone_Code lookup from golf course polygon centroids joined
    # against the Development Plan boundary layer (ZONMAP_NO 0-24). osm_id (not
    # positional poly_id) so this lookup composes safely with the P5-12 crosswalk below.
    build_devplan_lookup <- function(golf_polygons_sf, devplan_sf) {
        golf_centroids <- golf_polygons_sf |>
            st_centroid()  # [METHODOLOGY]

        zone_sf <- st_join(  # [METHODOLOGY]
            golf_centroids,
            devplan_sf |> select(ZONMAP_NO),
            join = st_within,
            left = TRUE
        )

        poly_zones <- zone_sf |>
            st_drop_geometry() |>
            group_by(osm_id) |>
            slice_head(n = 1) |>
            ungroup() |>
            mutate(Zone_Code = as.character(ZONMAP_NO)) |>
            select(osm_id, Zone_Code)

        na_ids <- poly_zones |> filter(is.na(Zone_Code)) |> pull(osm_id)
        if (length(na_ids) > 0) {
            na_centroids <- golf_centroids |> filter(osm_id %in% na_ids)
            nn_idx <- st_nearest_feature(na_centroids, devplan_sf)  # [METHODOLOGY]
            fallback <- tibble(
                osm_id    = na_centroids$osm_id,
                Zone_Code = as.character(devplan_sf$ZONMAP_NO[nn_idx])
            )
            poly_zones <- poly_zones |>
                filter(!osm_id %in% na_ids) |>
                bind_rows(fallback)
        }
        poly_zones
    }

    # P5-12 (2026-07-28): each course's Development Plan zone is now looked up via its
    # OWN crosswalk-resolved polygon (Poly_OSM_ID), replacing st_nearest_feature() --
    # which could assign a course the zone of a geometrically-closer but wrong neighbor
    # (the same defect class found in Phase 5's Step 3 and this script's own pooling join).
    assign_course_zones <- function(keep_list, poly_zones) {
        keep_list |> left_join(poly_zones, by = c("Poly_OSM_ID" = "osm_id"))
    }

    # Pool one language group's 100 imputed datasets with the Development Plan zone
    # BVPA override, identifying courses via the crosswalk-based keep_list (not raw
    # bbox + no dedup -- P5-12). All Oahu BVPA values are first normalized to
    # fhfa_value, then courses in rural_zones are overridden with usda_value.
    pool_oahu_oc_sensitivity <- function(paths, lang_label, keep_list_with_zone,
                                            fhfa_value, usda_value, rural_zones) {
        total_list <- vector("list", M)
        cat(sprintf(
            "  [%s] Pooling %d imputations (FHFA normalization + Dev Plan rural override, crosswalk-identified)...\n",
            lang_label, M
        ))
        for (i in seq_len(M)) {
            df <- read_csv(paths[i], show_col_types = FALSE) |>
                filter(
                    Latitude  >= OAHU_LAT_MIN, Latitude  <= OAHU_LAT_MAX,
                    Longitude >= OAHU_LON_MIN, Longitude <= OAHU_LON_MAX
                ) |>
                mutate(lon6 = round(Longitude, 6), lat6 = round(Latitude, 6)) |>
                inner_join(keep_list_with_zone, by = c("lon6", "lat6", "Holes")) |>
                mutate(
                    # Normalize ALL Oahu BVPA to verified FHFA value first.
                    # Corrects FIPS-NA courses (Hawaii Kai, Mid-Pacific) whose BVPA
                    # was MICE-imputed in Py/Jl datasets rather than assigned FHFA.
                    Baseline_Value_Per_Acre = fhfa_value,
                    # Override Development Plan rural zones (15-20) with USDA value.
                    Baseline_Value_Per_Acre = if_else(
                        !is.na(Zone_Code) & Zone_Code %in% rural_zones,
                        usda_value,
                        Baseline_Value_Per_Acre
                    ),
                    acreage  = get_acreage(pick(everything())),
                    opp_cost = acreage * Baseline_Value_Per_Acre
                )

            total_list[[i]] <- df |>
                group_by(group_id) |>
                summarise(
                    total_opp_cost = sum(opp_cost, na.rm = TRUE),
                    Poly_OSM_ID     = first(Poly_OSM_ID),
                    .groups        = "drop"
                ) |>
                mutate(imputation = i)
            rm(df)
            gc()
        }

        # [METHODOLOGY] Rubin's Rules q_bar: mean of per-imputation course-level OC
        #               values across M = 100 imputations for this language group.
        bind_rows(total_list) |>
            group_by(group_id) |>
            summarise(
                pooled_opp_cost = mean(total_opp_cost, na.rm = TRUE),
                Poly_OSM_ID     = first(Poly_OSM_ID),
                .groups         = "drop"
            )
    }

    # Direct crosswalk join (P5-12): attach pooled OC values to each polygon by its
    # own osm_id, replacing st_nearest_feature().
    join_oc_to_polygons <- function(polygons_sf, pooled_df) {
        polygons_sf |>
            mutate(osm_id_num = as.numeric(osm_id)) |>
            left_join(
                pooled_df |> filter(!is.na(Poly_OSM_ID)) |> select(Poly_OSM_ID, pooled_opp_cost),
                by = c("osm_id_num" = "Poly_OSM_ID")
            ) |>
            select(-osm_id_num)
    }

    build_oahu_oc_map <- function(golf_oc_sf, oahu_outline_sf,
                                    n_matched, subtitle, caption_text) {
        ggplot() +
            geom_sf(
                data      = oahu_outline_sf,
                fill      = "#e8e8e8",
                colour    = "#aaaaaa",
                linewidth = 0.35
            ) +
            geom_sf(
                data   = golf_oc_sf,
                aes(fill = pooled_opp_cost),
                colour = NA
            ) +
            scale_fill_viridis_c(
                option   = "magma",
                na.value = "#cccccc",
                name     = "Opportunity Cost",
                labels   = label_oc,
                guide    = guide_colorbar(
                    barwidth       = unit(0.5, "cm"),
                    barheight      = unit(5, "cm"),
                    title.position = "top",
                    title.hjust    = 0.5,
                    ticks.colour   = "white"
                )
            ) +
            annotation_scale(
                location   = "tl",
                width_hint = 0.22,
                style      = "ticks",
                text_cex   = 0.75,
                pad_x      = unit(0.5, "cm"),
                pad_y      = unit(0.5, "cm")
            ) +
            annotation_north_arrow(
                location    = "tl",
                which_north = "true",
                pad_x       = unit(0.5, "cm"),
                pad_y       = unit(1.5, "cm"),
                style       = north_arrow_fancy_orienteering(
                    fill      = c("white", "#444444"),
                    line_col  = "#444444",
                    text_col  = "#444444",
                    text_size = 8
                )
            ) +
            labs(
                title    = "Golf Course Opportunity Cost - Oahu, Hawaiʻi (Rural-USDA Sensitivity)",
                subtitle = subtitle,
                caption  = stringr::str_wrap(caption_text, width = 185)
            ) +
            theme_void(base_size = 12) +
            theme(
                plot.title = element_text(
                    face = "bold", size = 16, hjust = 0.5, margin = margin(b = 4)
                ),
                plot.subtitle = element_text(
                    size = 11, hjust = 0.5, colour = UHM_GREEN, margin = margin(b = 8)
                ),
                plot.caption = element_text(
                    size = 9, colour = UHM_GREEN, hjust = 0, margin = margin(t = 10)
                ),
                plot.caption.position = "plot",
                legend.position       = "right",
                legend.direction      = "vertical",
                legend.title          = element_text(size = 9, face = "bold"),
                legend.text           = element_text(size = 8),
                plot.background       = element_rect(fill = "white", colour = NA),
                plot.margin           = margin(12, 16, 8, 16)
            )
    }


    # === 4. EXECUTION ===

    cat("\n--- Script 9b: Oahu OC Map (Rural-USDA Sensitivity) ---\n\n")
    dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

    for (f in c(POLYGONS_GPKG, PARCELS_GPKG, DEV_PLAN_GEOJSON, PHASE1_R_PATH, USDA_PATH,
                R_IMPUTED_PATHS, PY_IMPUTED_PATHS, JL_IMPUTED_PATHS)) {
        if (!file.exists(f)) stop(sprintf("[FATAL] Input not found:\n  %s", f))
    }


    #  Step 1: Load economic baseline values

    cat("[Step 1] Loading USDA agricultural value for FIPS", HONOLULU_FIPS, "...\n")
    USDA_RURAL_VALUE <- load_usda_value(USDA_PATH, HONOLULU_FIPS)
    cat(sprintf(
        "  USDA agricultural value (FIPS %s, 2022): $%s/acre\n",
        HONOLULU_FIPS,
        format(as.integer(USDA_RURAL_VALUE), big.mark = ",")
    ))

    cat("[Step 1b] Loading FHFA residential value for FIPS", HONOLULU_FIPS, "...\n")
    FHFA_OAHU_VALUE <- load_fhfa_oahu_value(PHASE1_R_PATH, HONOLULU_FIPS)
    cat(sprintf(
        "  FHFA residential value (FIPS %s, 2022): $%s/acre\n",
        HONOLULU_FIPS,
        format(as.integer(FHFA_OAHU_VALUE), big.mark = ",")
    ))


    #  Step 2: Load spatial layers

    cat("\n[Step 2] Loading spatial layers...\n")

    # [METHODOLOGY] Spatial read of Target_Golf_Polygons.gpkg (OSM-derived golf course
    #               footprints); st_transform to EPSG 32604 (WGS 84 / UTM Zone 4N).
    golf_polygons_sf <- st_read(POLYGONS_GPKG, quiet = TRUE) |>
        st_transform(OAHU_CRS)
    cat(sprintf("  %d golf course polygons loaded (EPSG %d).\n",
                nrow(golf_polygons_sf), OAHU_CRS))

    # [METHODOLOGY] Spatial read of Zoning_Map_Boundary.geojson: City and County of
    #               Honolulu Development Plan boundary layer. ZONMAP_NO (integer 0-24)
    #               is the zone code used to classify courses as FHFA or USDA.
    devplan_sf <- st_read(DEV_PLAN_GEOJSON, quiet = TRUE) |>
        st_transform(OAHU_CRS)
    cat(sprintf("  %d Development Plan polygons loaded (ZONMAP_NO range: %d-%d).\n",
                nrow(devplan_sf),
                min(devplan_sf$ZONMAP_NO, na.rm = TRUE),
                max(devplan_sf$ZONMAP_NO, na.rm = TRUE)))

    # [METHODOLOGY] Spatial read of Honolulu_Parcels_Reprojected.gpkg; used only for
    #               island outline construction in Step 6 (st_union dissolve).
    cat("  Loading Honolulu parcels for island outline (may take ~20 sec)...\n")
    parcels_sf <- st_read(PARCELS_GPKG, quiet = TRUE) |>
        st_transform(OAHU_CRS)
    cat(sprintf("  %d parcel polygons loaded.\n", nrow(parcels_sf)))


    #  Step 3: Build Development Plan zone lookup + crosswalk-identified courses (P5-12)

    cat("\n[Step 3] Building Development Plan zone lookup and crosswalk keep-lists...\n")

    poly_zones <- build_devplan_lookup(golf_polygons_sf, devplan_sf)
    cat(sprintf(
        "  Zone assignment: %d/%d polygons assigned a zone.\n",
        sum(!is.na(poly_zones$Zone_Code)),
        nrow(golf_polygons_sf)
    ))

    crosswalk_raw <- read_csv(CROSSWALK_CSV, show_col_types = FALSE)
    keep_list_r  <- build_crosswalk_keeplist(PHASE1_R_PATH,  crosswalk_raw, PHASE1_R_PATH)
    keep_list_py <- build_crosswalk_keeplist(PHASE1_PY_PATH, crosswalk_raw, PHASE1_R_PATH)
    keep_list_jl <- build_crosswalk_keeplist(PHASE1_JL_PATH, crosswalk_raw, PHASE1_R_PATH)

    keep_list_r_zone  <- assign_course_zones(keep_list_r,  poly_zones)
    keep_list_py_zone <- assign_course_zones(keep_list_py, poly_zones)
    keep_list_jl_zone <- assign_course_zones(keep_list_jl, poly_zones)

    n_rural    <- sum(!is.na(keep_list_r_zone$Zone_Code) & keep_list_r_zone$Zone_Code %in% RURAL_ZONES)
    n_nonrural <- nrow(keep_list_r_zone) - n_rural
    cat(sprintf(
        "  Rural USDA override: %d courses (zones %s)  |  FHFA retained: %d courses\n",
        n_rural, paste(RURAL_ZONES, collapse = "/"), n_nonrural
    ))


    #  Step 4: Pool tri-language MICE with FHFA normalization + Dev Plan rural override; Grand Mean

    cat("\n[Step 4] Pooling tri-language MICE (FHFA normalization + Dev Plan rural override)...\n")

    pooled_r  <- pool_oahu_oc_sensitivity(
        R_IMPUTED_PATHS,  "R",  keep_list_r_zone, FHFA_OAHU_VALUE, USDA_RURAL_VALUE, RURAL_ZONES
    )
    pooled_py <- pool_oahu_oc_sensitivity(
        PY_IMPUTED_PATHS, "Py", keep_list_py_zone, FHFA_OAHU_VALUE, USDA_RURAL_VALUE, RURAL_ZONES
    )
    pooled_jl <- pool_oahu_oc_sensitivity(
        JL_IMPUTED_PATHS, "Jl", keep_list_jl_zone, FHFA_OAHU_VALUE, USDA_RURAL_VALUE, RURAL_ZONES
    )

    # [METHODOLOGY] Grand Mean = arithmetic mean of three independently Rubin-pooled
    #               OC vectors (M=100 each). full_join on the crosswalk's group_id --
    #               a stable string key shared identically across all three languages'
    #               copies of the crosswalk -- replaces the P5-01-identified exact-float
    #               (Longitude, Latitude) join.
    pooled_oahu <- pooled_r |>
        select(group_id, Poly_OSM_ID, oc_r = pooled_opp_cost) |>
        full_join(
            pooled_py |> select(group_id, oc_py = pooled_opp_cost),
            by = "group_id"
        ) |>
        full_join(
            pooled_jl |> select(group_id, oc_jl = pooled_opp_cost),
            by = "group_id"
        ) |>
        mutate(
            pooled_opp_cost = rowMeans(cbind(oc_r, oc_py, oc_jl), na.rm = TRUE)
        ) |>
        select(group_id, Poly_OSM_ID, pooled_opp_cost)

    cat(sprintf(
        "\n  Grand Mean Oahu total (Rural-USDA Sensitivity): $%.3fB across %d courses\n",
        sum(pooled_oahu$pooled_opp_cost, na.rm = TRUE) / 1e9,
        nrow(pooled_oahu)
    ))
    cat(sprintf(
        "  OC range: $%.1fM - $%.1fM\n",
        min(pooled_oahu$pooled_opp_cost, na.rm = TRUE) / 1e6,
        max(pooled_oahu$pooled_opp_cost, na.rm = TRUE) / 1e6
    ))


    #  Step 5: Crosswalk join Grand Mean sensitivity group_ids to golf polygons

    cat("\n[Step 5] Crosswalk join (Grand Mean sensitivity -> polygons)...\n")

    golf_oc_sens <- join_oc_to_polygons(golf_polygons_sf, pooled_oahu)

    n_matched   <- sum(!is.na(golf_oc_sens$pooled_opp_cost))
    n_unmatched <- sum( is.na(golf_oc_sens$pooled_opp_cost))
    cat(sprintf(
        "  %d polygons matched via crosswalk  |  %d unmatched (gray).\n",
        n_matched, n_unmatched
    ))
    #  Step 6: Build Oahu island outline from parcels

    cat("\n[Step 6] Dissolving parcels to island outline (~20 sec)...\n")
    # [METHODOLOGY] st_union() dissolves all Honolulu parcels into a single island
    #               outline, removing internal cadastral boundaries. Consistent with
    #               run_9_Oahu_Opportunity_Cost_Map.
    oahu_outline_sf <- parcels_sf |>
        st_geometry() |>
        st_union() |>
        st_sf()
    rm(parcels_sf)
    gc()
    cat("  Island outline complete.\n")


    #  Step 7: Render + save sensitivity map

    cat("\n[Step 7] Rendering Rural-USDA Sensitivity map...\n")

    caption_text <- paste0(
        "Sensitivity visualization: Honolulu County courses are reclassified by the City ",
        "and County of Honolulu's Development Plan boundary layer. Courses in unambiguously ",
        "rural Development Plan areas (codes 15-20) have been reclassified to use the 2022 ",
        "USDA agricultural per-acre value ($",
        format(as.integer(USDA_RURAL_VALUE), big.mark = ","),
        "/ac) rather than the FHFA residential value ($",
        format(as.integer(FHFA_OAHU_VALUE), big.mark = ","), "/ac)."
    )

    map_sens <- build_oahu_oc_map(
        golf_oc_sf      = golf_oc_sens,
        oahu_outline_sf = oahu_outline_sf,
        n_matched       = n_matched,
        subtitle        = sprintf(
            paste0(
                "%d courses  │  Grand Mean Py/R/Jl (M = 300: 100 each)  │  ",
                "Dev Plan Zones 15-20 → USDA $%s/ac  │  FHFA normalized"
            ),
            n_matched,
            format(as.integer(USDA_RURAL_VALUE), big.mark = ",")
        ),
        caption_text = caption_text
    )

    ggsave(OUT_PNG, map_sens, width = 12, height = 10, dpi = 300, units = "in")
    cat(sprintf("  Saved: output/%s\n", basename(OUT_PNG)))

    cat("--- Done ---\n")
    gc()
}


# ---------- Script 15: Residual Map ----------
run_15_Residual_Map <- function() {
    # === 2. GLOBALS & PATHS ===

    SCRIPT_DIR <- this.path::this.dir()
    WORK_DIR <- normalizePath(file.path(SCRIPT_DIR, ".."), mustWork = FALSE)
    PHASE1_CSV <- file.path(
        WORK_DIR, "Phase 1 Parsing", "Data", "R",
        "R_Phase1_Baseline_Golf_Valuation.csv"
    )
    PHASE3_DIR_R  <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "R")
    PHASE3_DIR_PY <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "python")
    PHASE3_DIR_JL <- file.path(WORK_DIR, "Phase 3 Economic Merge and MICE Imputation", "Data", "Julia")
    REG_R_CSV <- file.path(
        WORK_DIR, "Phase 4 Econometric Modeling", "Data", "R",
        "R_Regression_Results.csv"
    )
    REG_PY_CSV <- file.path(
        WORK_DIR, "Phase 4 Econometric Modeling", "Data", "python",
        "Py_Regression_Results.csv"
    )
    REG_JL_CSV <- file.path(
        WORK_DIR, "Phase 4 Econometric Modeling", "Data", "Julia",
        "Jl_Regression_Results.csv"
    )
    IMPUTED_PATHS_R  <- file.path(PHASE3_DIR_R,  paste0("R_Imputed_Dataset_",  1:100, ".csv"))
    IMPUTED_PATHS_PY <- file.path(PHASE3_DIR_PY, paste0("Py_Imputed_Dataset_", 1:100, ".csv"))
    IMPUTED_PATHS_JL <- file.path(PHASE3_DIR_JL, paste0("Jl_Imputed_Dataset_", 1:100, ".csv"))
    OUTPUT_DIR <- file.path(SCRIPT_DIR, "output")
    THESIS_DIR <- file.path(OUTPUT_DIR, "Final_Thesis_Figures")
    QA_DIR <- file.path(OUTPUT_DIR, "QA_Verification")
    dir.create(THESIS_DIR, showWarnings = FALSE, recursive = TRUE)
    dir.create(QA_DIR, showWarnings = FALSE, recursive = TRUE)
    OUT_PNG1 <- file.path(THESIS_DIR, "15.141_Log_Residual_Map_GrandMean.png")
    OUT_PNG2 <- file.path(THESIS_DIR, "15.241_Dollar_Residual_Map_GrandMean.png")

    M <- 100L
    TERRITORY_STATEFP <- c("60", "66", "69", "72", "74", "78")


    # === 3. FUNCTIONS ===

    # Render a diverging county choropleth. map_type = "log" uses the raw
    # Mean_Log_Residual; map_type = "dollar" applies a signed square-root
    # compression to handle the fat-tailed dollar distribution (LA, NYC outliers).
    build_residual_map <- function(counties_joined, fill_col, title, subtitle,
                                   caption_text, map_type = c("log", "dollar")) {
        map_type <- match.arg(map_type)

        if (map_type == "log") {
            lim <- max(abs(counties_joined[[fill_col]]), na.rm = TRUE)

            fill_scale <- scale_fill_gradient2(
                low = "#4575b4", # blue  = over-predict  (negative residual)
                mid = "white",
                high = "#d73027", # red   = under-predict (positive residual)
                midpoint = 0,
                limits = c(-lim, lim),
                na.value = "#d4d4d4",
                name = "Mean Log-Residual",
                guide = guide_colorbar(
                    barwidth       = unit(14, "cm"),
                    barheight      = unit(0.45, "cm"),
                    title.position = "top",
                    title.hjust    = 0.5,
                    ticks.colour   = "grey40"
                )
            )
        } else {
            # [METHODOLOGY] Signed square-root: plot_fill = sign(x) * sqrt(|x| / 1e6).
            # Preserves sign (direction) and compresses outlier magnitude symmetrically
            # on both sides of zero - unlike log, it handles negative residuals cleanly.
            counties_joined <- counties_joined |>
                mutate(
                    plot_fill = sign(.data[[fill_col]]) *
                        sqrt(abs(.data[[fill_col]]) / 1e6)
                )
            fill_col <- "plot_fill"
            lim <- max(abs(counties_joined$plot_fill), na.rm = TRUE)

            sqrt_breaks <- pretty(c(-lim, lim), n = 6)
            dollar_labels <- sapply(sqrt_breaks, function(s) {
                d <- sign(s) * s^2 * 1e6 # inverse transform
                if (abs(d) >= 1e9) {
                    sprintf("$%.1fB", d / 1e9)
                } else if (abs(d) >= 1e6) {
                    sprintf("$%.0fM", d / 1e6)
                } else if (abs(d) >= 1e3) {
                    sprintf("$%.0fK", d / 1e3)
                } else {
                    sprintf("$%.0f", d)
                }
            })

            fill_scale <- scale_fill_gradient2(
                low = "#4575b4",
                mid = "white",
                high = "#d73027",
                midpoint = 0,
                limits = c(-lim, lim),
                breaks = sqrt_breaks,
                labels = dollar_labels,
                na.value = "#d4d4d4",
                name = "Dollar Residual",
                guide = guide_colorbar(
                    barwidth       = unit(14, "cm"),
                    barheight      = unit(0.45, "cm"),
                    title.position = "top",
                    title.hjust    = 0.5,
                    ticks.colour   = "grey40"
                )
            )
        }

        ggplot(counties_joined) +
            geom_sf(
                aes(fill = .data[[fill_col]]),
                colour = "white",
                linewidth = 0.08
            ) +
            fill_scale +
            labs(title = title, subtitle = subtitle, caption = caption_text) +
            theme_void(base_size = 12) +
            theme(
                plot.title = element_text(
                    face = "bold", size = 18, hjust = 0.5, margin = margin(b = 5)
                ),
                plot.subtitle = element_text(
                    size = 10, hjust = 0.5, colour = UHM_GREEN, margin = margin(b = 12)
                ),
                plot.caption = element_text(
                    size = 7, colour = UHM_GREEN, hjust = 0, margin = margin(t = 12)
                ),
                legend.position = "bottom",
                legend.title = element_text(size = 9, face = "bold"),
                legend.text = element_text(size = 8),
                plot.margin = margin(12, 24, 8, 24)
            )
    }


    # Column-agnostic acreage extractor: R datasets use final_acreage, Py/Jl use osm_acreage.
    get_acreage <- function(df) {
        if ("osm_acreage" %in% names(df)) df[["osm_acreage"]] else df[["final_acreage"]]
    }

    # Per-language residual pooling: load M imputed datasets, compute log and dollar
    # residuals per course, summarise by county, return Rubin's q_bar (mean across M).
    # Requires county_lookup and b0/b_holes/b_urban from the enclosing scope.
    pool_lang_residuals <- function(paths, lang_label) {
        resid_list <- vector("list", M)
        cat(sprintf("  [%s] Pooling %d imputations...\n", lang_label, M))
        for (i in seq_len(M)) {
            imp_df <- read_csv(paths[i], show_col_types = FALSE) |>
                select(-any_of(c("FIPS", "County_Name", "State_Abbr", "Tigris_State_Abbr"))) |>
                # [METHODOLOGY] Re-resolve FIPS for all language groups via Phase 1 lookup
                #               keyed on Longitude/Latitude. R imputed files carry no FIPS;
                #               Py/Jl native FIPS/county columns are dropped above to guarantee
                #               consistent 5-digit formatting from the lookup.
                left_join(county_lookup, by = c("Longitude", "Latitude")) |>
                filter(
                    !is.na(FIPS),
                    !is.na(Holes),
                    !is.na(Baseline_Value_Per_Acre)
                ) |>
                mutate(acreage = get_acreage(pick(everything()))) |>
                filter(!is.na(acreage), acreage > 1) |>
                mutate(
                    Holes         = as.numeric(Holes),
                    is_urban      = as.integer(!is.na(county_type) & county_type == "Urban"),
                    predicted_log = b0 + b_holes * Holes + b_urban * is_urban,
                    # [METHODOLOGY] log-residual: log(actual OC) - predicted log(OC).
                    #   acreage > 1 guard ensures log() receives a value >= 0 on the log scale.
                    #   NOTE: predicted_log's coefficients (b0/b_holes/b_urban) were fit on
                    #   log1p(OC) in Phase 4; this residual compares against plain log(actual OC),
                    #   not log1p(actual OC) - a "+1" mismatch, negligible at real OC magnitudes
                    #   (P6-06).
                    log_residual  = log(acreage * Baseline_Value_Per_Acre) - predicted_log,
                    dollar_resid  = (acreage * Baseline_Value_Per_Acre) - exp(predicted_log)
                ) |>
                # Guard against log-linear model explosion for extreme Holes outliers.
                # Phase 1 contains a 252-hole aggregate record; exp(b_holes * 252) ≈ $3.7T.
                # 72 holes = 4 × 18H, the largest legitimate multi-course complex (24 records).
                filter(between(Holes, 9, 72))
            resid_list[[i]] <- imp_df |>
                group_by(FIPS, County_Name, State_Abbr) |>
                summarise(
                    mean_log_residual   = mean(log_residual, na.rm = TRUE),
                    sum_dollar_residual = sum(dollar_resid, na.rm = TRUE),
                    .groups = "drop"
                ) |>
                mutate(imputation = i)
            rm(imp_df)
            gc()
        }
        # [METHODOLOGY] Rubin's Rules q_bar: mean of per-imputation county-level
        #               residual vectors across M = 100 imputations for this language group.
        bind_rows(resid_list) |>
            group_by(FIPS, County_Name, State_Abbr) |>
            summarise(
                Mean_Log_Residual   = mean(mean_log_residual, na.rm = TRUE),
                Sum_Dollar_Residual = mean(sum_dollar_residual, na.rm = TRUE),
                .groups = "drop"
            )
    }


    # === 4. EXECUTION ===

    cat("\n--- Script 15: OLS Residual Maps (Log + Dollar) ---\n\n")
    dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

    for (f in c(PHASE1_CSV, REG_R_CSV, REG_PY_CSV, REG_JL_CSV,
                IMPUTED_PATHS_R, IMPUTED_PATHS_PY, IMPUTED_PATHS_JL)) {
        if (!file.exists(f)) stop(sprintf("[FATAL] Input not found:\n  %s", f))
    }


    #  Step 1: Load Phase 4 regression coefficients

    cat("[Step 1] Loading Phase 4 regression coefficients (tri-language Grand Mean)...\n")
    get_coef <- function(df, param) df$Coef[df$Parameter == param]

    # [METHODOLOGY] Grand Mean β̂ = arithmetic mean of three independently Rubin-pooled
    #               coefficient vectors (M = 100 each: Python, R, Julia). Applied uniformly
    #               to all imputed datasets for ŷ prediction in Step 3.
    reg_r <- read_csv(REG_R_CSV, show_col_types = FALSE)
    reg_py <- read_csv(REG_PY_CSV, show_col_types = FALSE)
    reg_jl <- read_csv(REG_JL_CSV, show_col_types = FALSE)

    b0 <- mean(c(
        get_coef(reg_r, "(Intercept)"),
        get_coef(reg_py, "(Intercept)"),
        get_coef(reg_jl, "(Intercept)")
    ))
    b_holes <- mean(c(
        get_coef(reg_r, "Holes"),
        get_coef(reg_py, "Holes"),
        get_coef(reg_jl, "Holes")
    ))
    b_urban <- mean(c(
        get_coef(reg_r, "factor(county_type)Urban"),
        get_coef(reg_py, "factor(county_type)Urban"),
        get_coef(reg_jl, "factor(county_type)Urban")
    ))

    cat(sprintf(
        "  Grand Mean: b0 = %.5f  |  b_holes = %.5f  |  b_urban = %.5f\n",
        b0, b_holes, b_urban
    ))


    #  Step 2: Build county FIPS lookup from Phase 1

    cat("\n[Step 2] Building county FIPS lookup from Phase 1...\n")
    phase1_df <- read_csv(PHASE1_CSV, show_col_types = FALSE)
    county_lookup <- phase1_df |>
        select(Longitude, Latitude, FIPS, County_Name, State_Abbr) |>
        distinct(Longitude, Latitude, .keep_all = TRUE) |>
        mutate(FIPS = if_else(!is.na(FIPS), sprintf("%05d", as.integer(FIPS)), NA_character_))
    cat(sprintf("  %d unique course coordinates loaded.\n", nrow(county_lookup)))


    #  Step 3: Compute residuals per imputation and pool (tri-language Grand Mean)

    cat("\n[Step 3] Computing residuals across tri-language imputed datasets...\n")
    pool_r  <- pool_lang_residuals(IMPUTED_PATHS_R,  "R")
    pool_py <- pool_lang_residuals(IMPUTED_PATHS_PY, "Py")
    pool_jl <- pool_lang_residuals(IMPUTED_PATHS_JL, "Jl")

    # [METHODOLOGY] Grand Mean: arithmetic mean of three independently Rubin-pooled
    #               county-level residual vectors (M = 100 each: R, Python, Julia).
    pooled_resid <- bind_rows(
        mutate(pool_r,  lang = "R"),
        mutate(pool_py, lang = "Py"),
        mutate(pool_jl, lang = "Jl")
    ) |>
        group_by(FIPS, County_Name, State_Abbr) |>
        summarise(
            Mean_Log_Residual   = mean(Mean_Log_Residual, na.rm = TRUE),
            Sum_Dollar_Residual = mean(Sum_Dollar_Residual, na.rm = TRUE),
            .groups = "drop"
        )

    cat(sprintf("\n  Residuals pooled: %d counties\n", nrow(pooled_resid)))
    cat(sprintf(
        "  Log-residual range:    [%.3f, %.3f]  (0 = perfect fit)\n",
        min(pooled_resid$Mean_Log_Residual, na.rm = TRUE),
        max(pooled_resid$Mean_Log_Residual, na.rm = TRUE)
    ))
    cat(sprintf(
        "  Dollar-residual range: [$%.2fB, $%.2fB]\n",
        min(pooled_resid$Sum_Dollar_Residual, na.rm = TRUE) / 1e9,
        max(pooled_resid$Sum_Dollar_Residual, na.rm = TRUE) / 1e9
    ))


    #  Step 4: Read county boundaries

    cat("\n[Step 4] Reading county boundaries (vendored, cb_2022_us_county_500k)...\n")
    # [METHODOLOGY] vendored Census cartographic boundary file (1:500k, GENZ2022);
    #               shift_geometry() repositions AK and HI as insets; st_transform
    #               to EPSG 5070 (NAD83 / Conus Albers) for equal-area national display.
    COUNTY_500K_SHP <- file.path(WORK_DIR, "00 - Data Sources", "Secondary",
                                  "cb_2022_us_county_500k", "cb_2022_us_county_500k.shp")
    counties_sf <- st_read(COUNTY_500K_SHP, quiet = TRUE) |>
        filter(!STATEFP %in% TERRITORY_STATEFP) |>
        shift_geometry() |>
        st_transform(5070)
    cat(sprintf("  %d counties loaded with AK/HI insets (EPSG 5070).\n", nrow(counties_sf)))

    counties_joined <- counties_sf |>
        left_join(pooled_resid, by = c("GEOID" = "FIPS"))

    no_data_n <- sum(is.na(counties_joined$Mean_Log_Residual))
    cat(sprintf(
        "  %d counties with no course data (gray)  |  %d with residuals.\n",
        no_data_n, nrow(counties_joined) - no_data_n
    ))


    #  Step 5: Render + save Map 15.1 (Log-Residual)

    cat("\n[Step 5] Rendering Map 15.1: Mean log-residual by county...\n")

    map1 <- build_residual_map(
        counties_joined,
        fill_col = "Mean_Log_Residual",
        title = "OLS Spatial Diagnostics: Mean Log-Residuals by County",
        subtitle = paste0(
            "log(1 + Actual OC) − log(1 + Predicted OC) per county  │  ",
            "Red = county OC exceeds prediction (model underpredicts)  │  ",
            "Blue = county OC falls short of prediction (model overpredicts) \n",
            "Grand Mean of three Rubin-pooled estimates (Python · R · Julia, M = 100 each)"
        ),
        caption_text = paste0(
            "OLS model: log(1 + Opportunity_Cost) = β₀ + β₁·Holes + β₂·I(Urban). ",
            "Each county's value = mean of course-level log-residuals for courses in that county. ",
            "No systematic geographic pattern in residuals indicates the model is spatially unbiased. ",
            "Gray counties contain no golf courses in the Phase 1 dataset.\n",
            "Sources: OpenStreetMap golf course polygons; FHFA residential land price index (urban); ",
            "USDA agricultural land values (rural). ",
            "CRS: NAD83 / Conus Albers (EPSG 5070). Alaska and Hawaii shown as insets."
        ),
        map_type = "log"
    )
    ggsave(OUT_PNG1, map1, width = 14, height = 9, dpi = 300, units = "in")
    cat(sprintf("  Saved: output/%s\n", basename(OUT_PNG1)))


    #  Step 6: Render + save Map 15.2 (Dollar-Residual)

    cat("\n[Step 6] Rendering Map 15.2: Sum dollar-residual by county...\n")

    map2 <- build_residual_map(
        counties_joined,
        fill_col = "Sum_Dollar_Residual",
        title = "Uncaptured Latent Value: Total Dollar Residuals by County",
        subtitle = paste0(
            "Σ (Actual OC − Predicted OC) per county  │  ",
            "Red = county value exceeds prediction (undercaptured latent value)  │  ",
            "Blue = county value falls short of prediction \n",
            "Grand Mean of three Rubin-pooled estimates (Python · R · Julia, M = 100 each)  │  Signed √ scale"
        ),
        caption_text = paste0(
            "Dollar residual = (acreage × Baseline_Value_Per_Acre) − exp(Predicted_log_OC); both terms in dollars. ",
            "Fill uses signed square-root compression to handle the fat-tailed distribution: ",
            "color encodes direction, intensity encodes magnitude; legend tick labels back-transform to dollar values.\n",
            "Gray counties contain no golf courses. ",
            "Sources: OpenStreetMap; FHFA/USDA land values. ",
            "CRS: NAD83 / Conus Albers (EPSG 5070). Alaska and Hawaii shown as insets."
        ),
        map_type = "dollar"
    )
    ggsave(OUT_PNG2, map2, width = 14, height = 9, dpi = 300, units = "in")
    cat(sprintf("  Saved: output/%s\n", basename(OUT_PNG2)))

    cat("--- Done ---\n")
    gc()
}


# ---------- Script 16: Magnitude Comparison Bar Chart ----------
run_16_Magnitude_Chart <- function() {
    # === 2. GLOBALS & PATHS ===

    SCRIPT_DIR <- this.path::this.dir()
    OUTPUT_DIR <- file.path(SCRIPT_DIR, "output")
    THESIS_DIR <- file.path(OUTPUT_DIR, "Final_Thesis_Figures")
    dir.create(THESIS_DIR, showWarnings = FALSE, recursive = TRUE)
    OUT_PNG    <- file.path(THESIS_DIR, "16.141_Magnitude_Comparison_TriLanguage.png")


    # === 3. FUNCTIONS ===

    build_magnitude_chart <- function(out_path) {

        UHM_GREEN       <- "#024731"
        UHM_SILVER      <- "#B2B2B2"
        PROJECTION_GRAY <- "#D8D8D8"

        # Values confirmed 2026-05-18:
        #   Golf  — National_Acreage_Summary.csv (MICE-pooled, M = 100)
        #   Solar — EIA Preliminary Monthly Generator Inventory, Aug 2024
        #            (107.4 GW × 6 ac/MW per SEIA central land-use intensity)
        #   DE+RI — U.S. Census Bureau, 2010 Census, land area only
        #            (Delaware 1,247,040 ac + Rhode Island 661,690 ac)
        #   NREL  — PV Magazine USA, June 2024 (forward-looking 100% solar scenario)
        bars <- tibble(
            label = c(
                "U.S. Utility-Scale Solar\n(EIA, Aug 2024)",
                "Delaware + Rhode Island\n(U.S. Census 2010, land area)",
                "U.S. Golf Courses\n(This thesis, Phase 2 OSM)",
                "NREL Full-Solar Scenario\n(Forward-looking projection)"
            ),
            acres = c(644000L, 1908730L, 2300521L, 10000000L),
            type  = c("comparison", "comparison", "golf", "projection")
        ) |>
            mutate(
                label = fct_reorder(label, acres),
                fill  = case_when(
                    type == "golf"       ~ UHM_GREEN,
                    type == "projection" ~ PROJECTION_GRAY,
                    TRUE                 ~ UHM_SILVER
                )
            )

        format_acres <- function(x) {
            case_when(
                x >= 1e6 ~ paste0(formatC(x / 1e6, digits = 1, format = "f"), "M ac"),
                TRUE     ~ paste0(round(x / 1e3), "K ac")
            )
        }

        golf_ac  <- 2300521L
        solar_ac <- 644000L
        deri_ac  <- 1908730L

        p <- ggplot(bars, aes(x = label, y = acres, fill = fill)) +
            geom_col(width = 0.65) +
            geom_text(
                aes(label = format_acres(acres)),
                hjust = -0.12,
                size  = 3.8,
                color = "gray25"
            ) +
            scale_fill_identity() +
            scale_y_continuous(
                breaks = c(0, 2e6, 4e6, 6e6, 8e6, 10e6),
                labels = c("0", "2", "4", "6", "8", "10"),
                expand = expansion(mult = c(0, 0.28))
            ) +
            coord_flip() +
            labs(
                title    = "U.S. Golf Course Land in Context",
                subtitle = paste0(
                    sprintf(
                        "Golf footprint (%.2fM ac) is %.1f× total U.S. utility-scale solar",
                        golf_ac / 1e6, golf_ac / solar_ac
                    ),
                    sprintf(
                        " — and %.0f%% larger than Delaware + Rhode Island combined",
                        (golf_ac / deri_ac - 1) * 100
                    )
                ),
                x       = NULL,
                y       = "Land Area (million acres)",
                caption = paste0(
                    "Sources — Golf: this thesis, Phase 2 OSM aggregate ",
                    "(MICE-pooled, M = 100; 2,300,521 ac). ",
                    "Solar: EIA Preliminary Monthly Electric Generator Inventory, Aug 2024 ",
                    "(107.4 GW × 6 ac/MW per SEIA central land-use intensity estimate). ",
                    "DE + RI: U.S. Census Bureau, 2010 Census, land area only ",
                    "(Delaware 1,247,040 ac + Rhode Island 661,690 ac). ",
                    "NREL: as reported in PV Magazine USA, June 2024 ",
                    "(forward-looking 100% solar electricity scenario; not a current deployment figure)."
                )
            ) +
            theme_minimal(base_size = 13) +
            theme(
                plot.title         = element_text(face = "bold", size = 16, color = UHM_GREEN),
                plot.subtitle      = element_text(
                    size = 11, color = "gray30", margin = margin(b = 14)
                ),
                plot.caption       = element_text(
                    size = 7.5, color = "gray50", hjust = 0, lineheight = 1.3,
                    margin = margin(t = 10)
                ),
                plot.margin        = margin(t = 14, r = 20, b = 14, l = 14),
                axis.text.y        = element_text(size = 10, lineheight = 1.25, color = "gray20"),
                axis.text.x        = element_text(size = 9),
                axis.title.x       = element_text(size = 10, color = "gray40"),
                panel.grid.major.y = element_blank(),
                panel.grid.minor   = element_blank(),
                panel.grid.major.x = element_line(color = "gray90", linewidth = 0.4)
            )

        ggsave(out_path, p, width = 12, height = 5.5, dpi = 300, units = "in")
        invisible(p)
    }


    # === 4. EXECUTION ===

    cat("\n--- Script 16: Magnitude Comparison Bar Chart ---\n")
    cat("[Chart] Building magnitude comparison chart...\n")
    build_magnitude_chart(OUT_PNG)
    cat(sprintf("  Saved: output/Final_Thesis_Figures/%s\n", basename(OUT_PNG)))
    cat("--- Done ---\n")
    gc()
}


# === 4. EXECUTION ===
grand_means <- compute_grand_means()
plan(sequential)

run_1_Macro_Maps()
run_2_County_Map()
run_3_Oahu_TMK_Map()
run_4_Oahu_Zoning_Map()
run_7_Bivariate_Econometric_Map()
run_8_LaTeX_Tables()
run_9_Oahu_Opportunity_Cost_Map()
run_9b_Oahu_OC_Rural_USDA_Sensitivity()
run_15_Residual_Map()
run_16_Magnitude_Chart()

record_provenance("Phase 6", "Phase_6.R", SCRIPT_DIR, PROV_START)
