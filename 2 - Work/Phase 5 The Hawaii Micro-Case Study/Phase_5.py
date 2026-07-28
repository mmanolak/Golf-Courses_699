# Purpose: Standalone master pipeline for the Phase 5 Hawaii Micro-Case Study.
#          Runs all six steps end-to-end without invoking the individual step scripts.
# Inputs:  Phase 1 Parsing/Data/Python/Py_Phase1_Baseline_Golf_Valuation.csv
#          Phase 2 Spatial Polygons and True Acreage/Data/Python/Py_Phase2_OSM_Golf_Polygons.gpkg
#          00 - Data Sources/Honolulu/All_Parcels_6378200148342636690.gpkg
#          00 - Data Sources/Honolulu/All_Parcels_-4613852522541990741.csv
#          00 - Data Sources/Honolulu/Zoning_-2205419429161838665.gpkg
#          Phase 3 Economic Merge and MICE Imputation/Data/Python/Py_Imputed_Dataset_{1..100}.csv
# Outputs: Bulk Tests/python/Target_Golf_Polygons.gpkg          (intermediate)
#          Bulk Tests/python/Honolulu_Parcels_Reprojected.gpkg  (intermediate)
#          Bulk Tests/python/Target_Golf_Parcels_List.csv       (intermediate)
#          Data/Python/Py_Phase5_Oahu_Comparison.csv
#          Data/Python/Py_Phase5_Step5_Geographic_Breakdown.csv
#          Data/Python/Py_Phase5_Step6_Zoning_Percentages.csv
#          Data/Python/Py_Phase5_Step6_Zone_Golf_Penetration.csv
# Note:    Run the R version first to generate the Geopackage File

# === 1. LIBRARIES ===

import gc
import pathlib
import re
import sys
import time
import numpy as np
import pandas as pd
import geopandas as gpd


# === 2. GLOBALS & PATHS ===

SCRIPT_DIR        = pathlib.Path(__file__).parent
WORK_DIR          = SCRIPT_DIR.parent

PROV_START = time.time()
sys.path.insert(0, str(WORK_DIR))
import provenance
HONOLULU_DATA_DIR = WORK_DIR / "00 - Data Sources" / "Honolulu"
BULK_PYTHON_DIR   = SCRIPT_DIR / "Bulk Tests" / "python"
DATA_PYTHON_DIR   = SCRIPT_DIR / "Data" / "python"

# --- Step 1 inputs ---
PHASE1_IN  = (
    WORK_DIR
    / "Phase 1 Parsing"
    / "Data"
    / "python"
    / "Py_Phase1_Baseline_Golf_Valuation.csv"
)
OSM_IN     = (
    WORK_DIR
    / "Phase 2 Spatial Polygons and True Acreage"
    / "Data"
    / "python"
    / "Py_Phase2_OSM_Golf_Polygons.gpkg"
)
PARCELS_IN = HONOLULU_DATA_DIR / "All_Parcels_6378200148342636690.gpkg"
# Vendored 2026-07-27 (X-08/Gate-3 policy: no master script performs a network fetch at
# run time). Same TIGER/Line file Phase 1 uses -- source
# https://www2.census.gov/geo/tiger/TIGER2022/COUNTY/tl_2022_us_county.zip
COUNTY_SHP = WORK_DIR / "00 - Data Sources" / "Original Data" / "tl_2022_us_county.shp"

# --- Intermediate files (shared by steps 1-5) ---
TARGET_GOLF_GPKG    = BULK_PYTHON_DIR / "Target_Golf_Polygons.gpkg"
PARCELS_REPROJECTED = BULK_PYTHON_DIR / "Honolulu_Parcels_Reprojected.gpkg"
TMK_LIST_CSV        = BULK_PYTHON_DIR / "Target_Golf_Parcels_List.csv"
COMPARISON_CSV      = DATA_PYTHON_DIR / "Py_Phase5_Oahu_Comparison.csv"
GEO_BREAKDOWN_CSV   = DATA_PYTHON_DIR / "Py_Phase5_Step5_Geographic_Breakdown.csv"

# --- Step 3 inputs ---
PHASE3_DATA_DIR = (
    WORK_DIR / "Phase 3 Economic Merge and MICE Imputation" / "Data" / "python"
)
IMPUTED_PATHS = [
    PHASE3_DATA_DIR / f"Py_Imputed_Dataset_{i}.csv" for i in range(1, 101)
]
# --- Steps 4 & 5 inputs ---
TAX_CSV_CANDIDATES = [
    HONOLULU_DATA_DIR / "All_Parcels_-4613852522541990741.csv",
    HONOLULU_DATA_DIR / "Cadastral_2020_8454252231025374231.csv",
]
ZONING_GPKG         = HONOLULU_DATA_DIR / "Zoning_-2205419429161838665.gpkg"
ZONING_PCT_CSV       = DATA_PYTHON_DIR / "Py_Phase5_Step6_Zoning_Percentages.csv"
ZONE_PENETRATION_CSV = DATA_PYTHON_DIR / "Py_Phase5_Step6_Zone_Golf_Penetration.csv"

# --- Constants ---
M           = 100
M2_PER_ACRE = 4046.856422
OAHU_LAT_MIN, OAHU_LAT_MAX =  21.2,   21.9
OAHU_LON_MIN, OAHU_LON_MAX = -158.5, -157.6

DISTRICT_MAP = {
    "1": "Honolulu (Urban Core)",
    "2": "Honolulu (East/Anomalies)",
    "3": "Honolulu (Anomalies)",
    "4": "Koolaupoko (Kailua/Kaneohe)",
    "5": "Koolauloa (North/East)",
    "6": "Waialua (North Shore)",
    "7": "Wahiawa (Central)",
    "8": "Waianae (West)",
    "9": "Ewa (Kapolei/Pearl City)",
}

TMK_CANDIDATES = [
    "TMK", "PARCEL_ID", "Parcel_ID", "parcel_id", "TAX_MAP_KEY",
    "Tax_Map_Key", "tax_map_key", "MAPKEY", "mapkey", "tmk",
]


# === 3. FUNCTIONS ===

def add_row(rows, metric, value):
    rows.append({"Metric": metric, "Value": str(value)})


# === 4. EXECUTION ===

def run_step1():
    print("\n" + "=" * 70)
    print("Phase 5 - Step 1: Data Acquisition")
    print("=" * 70 + "\n")

    for path in [PHASE1_IN, OSM_IN, PARCELS_IN]:
        if not path.exists():
            print(f"[FATAL] Input file not found:\n  {path}")
            raise SystemExit(1)

    print("Loading datasets...")
    baseline_df  = pd.read_csv(PHASE1_IN)
    # [METHODOLOGY] gpd.read_file - spatial read of Phase 2 OSM golf polygons
    osm_golf_geo = gpd.read_file(OSM_IN)
    # [METHODOLOGY] gpd.read_file - spatial read of Honolulu cadastral parcel layer
    parcels_geo  = gpd.read_file(PARCELS_IN)

    # P5-13 (2026-07-28): the raw cadastre carries an unreconciled duplicate tax-boundary
    # layer (type==3, tmk always NA) that geometrically overlaps already-TMK-assessed
    # parcels for large single-ownership tracts -- summing both roughly doubled Step 2's
    # acreage for ~half of Oahu's golf courses. Drop the untracked duplicate before any
    # intersection; tmk-NA is an exact proxy for type==3 (VERIFIED 1:1 correspondence).
    n_before_dedup = len(parcels_geo)
    parcels_geo = parcels_geo[parcels_geo["tmk"].notna()].copy()
    print(f"  [P5-13] Dropped {n_before_dedup - len(parcels_geo):,} duplicate tax-boundary "
          f"parcels (tmk NA) of {n_before_dedup:,} total.")

    print("Reading Oahu boundary (vendored TIGER/Line)...")
    oahu_boundary_geo = (
        gpd.read_file(COUNTY_SHP)
        .query("STATEFP == '15' and NAME == 'Honolulu'")
        # [METHODOLOGY] .to_crs - reproject county boundary to match OSM CRS
        .to_crs(osm_golf_geo.crs)
    )

    print("Extracting all OSM polygons within Oahu...")
    boundary_union = oahu_boundary_geo.geometry.union_all()
    # [METHODOLOGY] .intersects - spatial subset of all OSM golf polygons to Honolulu county
    oahu_golf_geo = osm_golf_geo[osm_golf_geo.geometry.intersects(boundary_union)].copy()
    if len(oahu_golf_geo) == 0:
        print("[FATAL] No OSM polygons found on Oahu.")
        raise SystemExit(1)

    # P5-15 (2026-07-28): osm_id 22249545 ("Ko'olau Golf Club") is a 100%-geometrically-
    # identical duplicate of osm_id 479916082 (same course, digitized twice in the source
    # OSM data) -- VERIFIED via direct geometry comparison (0m apart, full-area overlap).
    # Left in, this polygon double-counts ~221 ac in Step 2's intersection sum and
    # double-renders the course downstream. Canonical crosswalk keeps 479916082.
    n_before_koolau_dedup = len(oahu_golf_geo)
    oahu_golf_geo = oahu_golf_geo[oahu_golf_geo["osm_id"] != 22249545].copy()
    if len(oahu_golf_geo) < n_before_koolau_dedup:
        print(f"  [P5-15] Dropped {n_before_koolau_dedup - len(oahu_golf_geo)} duplicate "
              f"OSM polygon (osm_id 22249545, Ko'olau Golf Club).")

    oahu_mask = (
        (baseline_df["County_Name"] == "Honolulu") | (baseline_df["FIPS"] == 15003)
    )
    # [METHODOLOGY] gpd.GeoDataFrame - convert Phase 1 tabular baseline to spatial points
    oahu_baseline_geo = gpd.GeoDataFrame(
        baseline_df[oahu_mask].copy(),
        geometry=gpd.points_from_xy(
            baseline_df.loc[oahu_mask, "Longitude"],
            baseline_df.loc[oahu_mask, "Latitude"],
        ),
        crs=4326,
    )
    # [METHODOLOGY] .to_crs - reproject Phase 1 points to match OSM CRS
    oahu_baseline_geo = oahu_baseline_geo.to_crs(osm_golf_geo.crs)

    # [METHODOLOGY] gpd.sjoin - check which Phase 1 points fall within an OSM polygon;
    #               mismatch rate quantifies Phase 1-to-Phase 2 representational error
    joined    = gpd.sjoin(
        oahu_baseline_geo, oahu_golf_geo[["geometry"]], how="left", predicate="intersects"
    )
    hit_index = set(joined.loc[joined["index_right"].notna()].index)
    hits      = len(hit_index)
    misses    = len(oahu_baseline_geo) - hits

    print("\n" + "=" * 60)
    print("METHODOLOGICAL ERROR ANALYSIS (OAHU MICRO-CASE STUDY)")
    print("=" * 60)
    print(f"  Phase 1 Baseline Total (Points) : {len(oahu_baseline_geo):d} courses")
    print(f"  Phase 2 OSM Total (Polygons)    : {len(oahu_golf_geo):d} courses")
    print("  " + "-" * 50)
    print(f"  Points hitting a polygon        : {hits:d}")
    print(f"  Points missing a polygon        : {misses:d}")
    print(f"  Direct Point Match Rate         : {hits / len(oahu_baseline_geo) * 100:.1f}%")
    print("=" * 60 + "\n")

    if oahu_golf_geo.crs != parcels_geo.crs:
        print("Reprojecting parcels to match OSM CRS...")
        # [METHODOLOGY] .to_crs - align parcel CRS to OSM CRS for Step 2 overlay
        parcels_geo = parcels_geo.to_crs(oahu_golf_geo.crs)

    BULK_PYTHON_DIR.mkdir(parents=True, exist_ok=True)
    DATA_PYTHON_DIR.mkdir(parents=True, exist_ok=True)
    print(f"Exporting geometries to: {BULK_PYTHON_DIR}")
    # [METHODOLOGY] .to_file - persist Oahu OSM golf polygons for Step 2 parcel intersection
    oahu_golf_geo.to_file(TARGET_GOLF_GPKG, driver="GPKG")
    # [METHODOLOGY] .to_file - persist reprojected parcel cadastre for Step 2
    parcels_geo.to_file(PARCELS_REPROJECTED, driver="GPKG")

    print("\n[DONE] Step 1 Complete.")


def run_step2():
    print("\n" + "=" * 70)
    print("Phase 5 - Step 2: Parcel Intersection")
    print("=" * 70 + "\n")

    if not TARGET_GOLF_GPKG.exists():
        print("[FATAL] Target Golf Polygons not found. Run Step 1.")
        raise SystemExit(1)
    if not PARCELS_REPROJECTED.exists():
        print("[FATAL] Reprojected Parcels not found. Run Step 1.")
        raise SystemExit(1)

    # [METHODOLOGY] gpd.read_file - spatial read of Step 1 OSM golf polygons
    target_golf_geo = gpd.read_file(TARGET_GOLF_GPKG)
    # [METHODOLOGY] gpd.read_file - spatial read of Step 1 reprojected parcel cadastre
    parcels_geo     = gpd.read_file(PARCELS_REPROJECTED)

    print(f"  -> Loaded {len(target_golf_geo)} target golf polygons.")
    print(f"  -> Loaded {len(parcels_geo)} parcel features.")
    print("\nPerforming spatial intersection (this may take a moment)...")

    # [METHODOLOGY] gpd.overlay(how='intersection') - cookie-cutter of Phase 2 OSM polygons
    #               over the Phase 5 legal cadastre to isolate golf-course parcel fragments
    parcel_intersection_geo = gpd.overlay(target_golf_geo, parcels_geo, how="intersection")
    print(f"  -> Intersection complete: {len(parcel_intersection_geo)} parcel fragments found.")

    found_column = next(
        (col for col in TMK_CANDIDATES if col in parcel_intersection_geo.columns), None
    )
    if found_column is None:
        print("\n[WARNING] Standard TMK column not found. Available columns:")
        print(list(parcel_intersection_geo.columns))
        print("[FATAL] No TMK column identified.")
        raise SystemExit(1)

    raw_tmks          = parcel_intersection_geo[found_column].dropna().astype(str).unique()
    unique_tmk_sorted = sorted(raw_tmks)
    print(f"  -> Found {len(unique_tmk_sorted)} unique TMKs across the {len(target_golf_geo)} golf courses.")

    tmk_df = pd.DataFrame({"TMK": unique_tmk_sorted})
    tmk_df.to_csv(TMK_LIST_CSV, index=False)

    # [METHODOLOGY] .geometry.area - compute legal footprint area from intersection geometry
    total_area_m2   = parcel_intersection_geo.geometry.area.sum()
    total_acres     = total_area_m2 / 4046.86

    print("\n" + "=" * 60)
    print("PARCEL INTERSECTION COMPLETE")
    print("=" * 60)
    print(f"  Total Targeted Courses : {len(target_golf_geo)}")
    print(f"  Total Unique TMKs      : {len(unique_tmk_sorted)}")
    print(f"  Total Legal Footprint  : {total_acres:,.2f} Acres")
    print("-" * 60)
    print(f"[+] Exported TMK List (CSV) : {TMK_LIST_CSV.as_posix()}")
    print("\n[DONE] Step 2 Complete.")
    return total_acres


def run_step3(osm_derived_acres):
    print("\n" + "=" * 70)
    print("Phase 5 - Step 3: Economic Validation")
    print("=" * 70)
    print(f"\n  TMK list      : {TMK_LIST_CSV}")
    print(f"  Parcels GPKG  : {PARCELS_REPROJECTED}")
    print(f"  OSM Polygons  : {TARGET_GOLF_GPKG}")
    print(f"  Phase 3 dir   : {PHASE3_DATA_DIR}")
    print(f"  Output        : {COMPARISON_CSV}\n")

    required = {
        "TMK list (Step 2 output)"     : TMK_LIST_CSV,
        "Parcels GPKG (Step 1 output)" : PARCELS_REPROJECTED,
        "OSM Polygons (Step 1 output)" : TARGET_GOLF_GPKG,
    }
    for label, path in required.items():
        if not path.exists():
            print(f"[FATAL] {label} not found:\n  {path}")
            raise SystemExit(1)

    missing_imp = [p for p in IMPUTED_PATHS if not p.exists()]
    if missing_imp:
        print("[FATAL] Phase 3 imputed datasets not found:")
        for p in missing_imp:
            print(f"  {p}")
        raise SystemExit(1)

    print("-" * 70)
    print("[Step 3.1] Loading TMK list...")
    tmk_df = pd.read_csv(TMK_LIST_CSV)
    tmk_df.rename(columns={tmk_df.columns[0]: "tmk"}, inplace=True)
    tmk_df["tmk"] = tmk_df["tmk"].astype(str)
    print(f"  Loaded {len(tmk_df)} TMKs.")

    print("-" * 70)
    print("[Step 3.2] Loading parcel attributes from cadastre GPKG...")
    # [METHODOLOGY] gpd.read_file - spatial read of Step 1 parcel cadastre for attribute join
    parcels_attr = gpd.read_file(PARCELS_REPROJECTED).drop(columns="geometry")

    if "tmk" in parcels_attr.columns:
        parcels_attr["tmk"] = parcels_attr["tmk"].astype(str)
    else:
        found = next(
            (c for c in ["TMK", "tmk8num", "tmk9num", "taxpin", "parcel_uid"]
             if c in parcels_attr.columns),
            None,
        )
        if found is None:
            print("[FATAL] Cannot find a TMK join column in parcels GPKG.")
            raise SystemExit(1)
        parcels_attr = parcels_attr.rename(columns={found: "tmk"})
        parcels_attr["tmk"] = parcels_attr["tmk"].astype(str)

    matched_parcels = tmk_df.merge(parcels_attr, on="tmk", how="inner")
    print(f"  TMKs from Step 2:      {len(tmk_df)}")
    print(f"  Matched in cadastre:   {len(matched_parcels)}")

    area_col = next(
        (c for c in ["dpp_approved_area_acres", "dpp_stated_area", "rpa_stated_area"]
         if c in matched_parcels.columns and matched_parcels[c].notna().any()),
        None,
    )
    if area_col:
        official_area_acres = matched_parcels[area_col].sum(skipna=True)
        print(f"\n  Official area column used : {area_col}")
        print(f"  Total official area       : {official_area_acres:,.2f} acres")
    else:
        official_area_acres = float("nan")

    print(f"\n  OSM-derived legal footprint (Step 2 geometry): {osm_derived_acres:,.2f} acres")

    print("-" * 70)
    print("[Step 3.3] Loading Phase 3 imputations & applying spatial deduplication...")

    oahu_estimates = []
    for i, path in enumerate(IMPUTED_PATHS, start=1):
        df_i      = pd.read_csv(path)
        oahu_mask = (
            df_i["Longitude"].notna() & df_i["Latitude"].notna() &
            (df_i["Latitude"]  >= OAHU_LAT_MIN) & (df_i["Latitude"]  <= OAHU_LAT_MAX) &
            (df_i["Longitude"] >= OAHU_LON_MIN) & (df_i["Longitude"] <= OAHU_LON_MAX)
        )
        df_oahu   = df_i[oahu_mask].copy()
        df_oahu["Total_Opportunity_Cost"] = df_oahu["osm_acreage"] * df_oahu["Baseline_Value_Per_Acre"]
        df_oahu["imputation"] = i
        oahu_estimates.append(df_oahu)
        del df_i; gc.collect()

    oahu_all = pd.concat(oahu_estimates, ignore_index=True)
    sizes    = ", ".join(str(len(d)) for d in oahu_estimates)
    print(f"  Oahu courses before deduplication (per imputation): {sizes}")
    print("\n  Applying spatial deduplication using OSM polygons...")

    # [METHODOLOGY] gpd.read_file - spatial read of Oahu golf polygons for deduplication
    osm_polys_geo = gpd.read_file(TARGET_GOLF_GPKG)
    osm_polys_geo["poly_id"] = range(1, len(osm_polys_geo) + 1)

    unique_courses = (
        oahu_all.groupby(["Longitude", "Latitude"])
        .agg(Holes=("Holes", "max"))
        .reset_index()
    )

    # [METHODOLOGY] gpd.GeoDataFrame - convert deduplicated course coordinates to spatial points
    courses_geo = gpd.GeoDataFrame(
        unique_courses,
        geometry=gpd.points_from_xy(unique_courses["Longitude"], unique_courses["Latitude"]),
        crs=4326,
    )
    # [METHODOLOGY] .to_crs - reproject course points to match OSM CRS
    courses_geo = courses_geo.to_crs(osm_polys_geo.crs)

    # [METHODOLOGY] gpd.sjoin_nearest - nearest-neighbor match to OSM polygons;
    #               mirrors Phase 2's fallback matching logic
    joined_nearest = gpd.sjoin_nearest(
        courses_geo.reset_index(drop=True),
        osm_polys_geo[["poly_id", "geometry"]],
        how="left",
        distance_col="nearest_dist",
    ).groupby(level=0).first()

    # [METHODOLOGY] 500 m cap - only assign a polygon if within 500 m of the point;
    #               threshold mirrors Phase 2 spatial tolerance for point-to-polygon matching
    assigned_poly = np.where(
        joined_nearest["nearest_dist"] <= 500,
        joined_nearest["poly_id"],
        np.nan,
    )

    group_ids = [
        f"orphan_{i}" if np.isnan(p) else str(int(p))
        for i, p in enumerate(assigned_poly)
    ]

    dedup_df = unique_courses.copy()
    dedup_df["group_id"] = group_ids
    dedup_df["Holes"]    = joined_nearest["Holes"].values

    master_keep_list = (
        dedup_df
        .sort_values(["group_id", "Holes"], ascending=[True, False])
        .drop_duplicates(subset=["group_id"])
        [["Longitude", "Latitude", "Holes"]]
        .reset_index(drop=True)
    )
    print(f"  Unique Oahu courses after spatial deduplication: {len(master_keep_list)}")

    oahu_deduped_list = []
    for i in range(1, M + 1):
        df_i = oahu_all[oahu_all["imputation"] == i].merge(
            master_keep_list, on=["Longitude", "Latitude", "Holes"], how="inner"
        )
        oahu_deduped_list.append(df_i)

    oahu_per_course = (
        pd.concat(oahu_deduped_list, ignore_index=True)
        .groupby(["Longitude", "Latitude"])
        .agg(
            n_imputations         = ("imputation",             "count"),
            mean_final_acreage    = ("osm_acreage",            "mean"),
            mean_baseline_val     = ("Baseline_Value_Per_Acre","mean"),
            mean_opportunity_cost = ("Total_Opportunity_Cost",  "mean"),
            Holes                 = ("Holes",                  "first"),
            **({
                "county_type": ("county_type", "first")
            } if "county_type" in oahu_all.columns else {}),
        )
        .reset_index()
        .sort_values("Longitude")
    )

    # [METHODOLOGY] Rubin's Rules - pooling across M imputations; simplified formula
    #               using total-level aggregates (see Phase 4 for full coefficient pooling)
    oahu_agg_dedup = [d["Total_Opportunity_Cost"].sum() for d in oahu_deduped_list]
    q_bar = np.mean(oahu_agg_dedup)
    v_w   = np.mean([d["Total_Opportunity_Cost"].var(ddof=1) for d in oahu_deduped_list])
    v_b   = np.var(oahu_agg_dedup, ddof=1)
    v_t   = v_w + v_b + v_b / M
    se    = np.sqrt(v_t)
    ci_lo = q_bar - 2.576 * se
    ci_hi = q_bar + 2.576 * se

    print(
        f"\n  Deduplicated Pooled Oahu Opportunity Cost: "
        f"${q_bar/1e9:.3f}B (99% CI: ${ci_lo/1e9:.3f}B - ${ci_hi/1e9:.3f}B)"
    )

    print("-" * 70)
    print("[Step 3.4] Building and saving comparison table...")

    rows = []
    add_row(rows, "Total Golf Courses (Oahu, OSM polygons)", len(osm_polys_geo))
    add_row(rows, "Total Unique TMKs (Step 2)",             f"{len(tmk_df):,}")
    add_row(rows, "TMKs Matched in Cadastre",               f"{len(matched_parcels):,}")
    add_row(rows, "OSM-Derived Legal Footprint (acres)",    f"{osm_derived_acres:,.2f}")

    for i, val in enumerate(oahu_agg_dedup, start=1):
        add_row(rows, f"Oahu Opportunity Cost - Imputation {i} ($B)", f"{val/1e9:.3f}")

    add_row(rows, "Pooled Oahu Opportunity Cost - q_bar ($B)", f"{q_bar/1e9:.3f}")
    add_row(rows, "Standard Error ($B)",                        f"{se/1e9:.3f}")
    add_row(rows, "95% CI Lower ($B)",                          f"{ci_lo/1e9:.3f}")
    add_row(rows, "95% CI Upper ($B)",                          f"{ci_hi/1e9:.3f}")

    if not np.isnan(official_area_acres):
        add_row(rows, "Total Official Area (acres)", f"{official_area_acres:,.2f}")

    comparison_df = pd.DataFrame(rows)

    print("=" * 70)
    print("PHASE 5 ECONOMIC VALIDATION - RESULTS")
    print("=" * 70)
    for _, row in comparison_df.iterrows():
        print(f"  {row['Metric']:<55} {row['Value']}")
    print("=" * 70)

    comparison_df.to_csv(COMPARISON_CSV, index=False)
    print(f"\n[+] Comparison table saved -> {COMPARISON_CSV.as_posix()}")

    print(
        f"\n  Per-Course Summary ({len(oahu_per_course)} Oahu courses, "
        f"averaged across {M} imputations):"
    )
    print(f"  {'Latitude':<12} {'Longitude':<12} {'Holes':<10} {'Mean Acreage':<18} Mean Opp. Cost ($M)")
    print("-" * 70)
    for _, r in oahu_per_course.iterrows():
        print(
            f"  {r['Latitude']:<12.4f} {r['Longitude']:<12.4f} "
            f"{str(r['Holes']):<10} {r['mean_final_acreage']:<18.1f} "
            f"${r['mean_opportunity_cost']/1e6:.2f}M"
        )

    print("\n[DONE] Step 3 Complete.")


def run_step4():
    print("\n" + "=" * 70)
    print("Phase 5 - Step 4: Tax Assessment Merge (Diagnostic)")
    print("=" * 70 + "\n")

    if not TMK_LIST_CSV.exists():
        print(f"[FATAL] TMK list not found. Run Step 2 first.\n  {TMK_LIST_CSV}")
        raise SystemExit(1)

    tax_file_to_use = next((p for p in TAX_CSV_CANDIDATES if p.exists()), None)
    if tax_file_to_use is None:
        candidates = "\n  ".join(str(p) for p in TAX_CSV_CANDIDATES)
        print(f"[FATAL] No Honolulu cadastral CSV found. Expected:\n  {candidates}")
        raise SystemExit(1)

    tmk_df = pd.read_csv(TMK_LIST_CSV)
    tmk_df.rename(columns={tmk_df.columns[0]: "TMK"}, inplace=True)
    tmk_df["TMK_clean"] = tmk_df["TMK"].astype(str).str.replace(r"[^0-9]", "", regex=True)

    tax_data = pd.read_csv(tax_file_to_use)
    tmk_col  = next(
        (c for c in tax_data.columns if re.search(r"(?i)^tmk$|parcel_id|tax_map_key|pin", c)),
        None,
    )
    if tmk_col is None:
        print("[FATAL] No TMK column identified in cadastral CSV.")
        raise SystemExit(1)

    tax_data["TMK_clean"] = tax_data[tmk_col].astype(str).str.replace(r"[^0-9]", "", regex=True)

    step2_lens = tmk_df["TMK_clean"].str.len()
    csv_lens   = tax_data["TMK_clean"].dropna().str.len()

    # 8-digit format = Z S PPP QQQ  (3-digit parcel field)
    # 9-digit format = Z S PPP QQQQ (4-digit parcel field, trailing 0 for non-CPR parcels)
    if (step2_lens == 8).all() and (csv_lens == 9).all():
        print("[AUTO-FIX] Step 2 TMKs are 8-digit; appending '0' to match 9-digit CSV format...")
        tmk_df["TMK_clean"] = tmk_df["TMK_clean"] + "0"
    elif (step2_lens == 9).all() and (csv_lens == 8).all():
        print("[AUTO-FIX] CSV TMKs are 8-digit; appending '0' to match 9-digit Step 2 format...")
        tax_data["TMK_clean"] = tax_data["TMK_clean"] + "0"

    merged_data   = tmk_df.merge(tax_data, on="TMK_clean", how="inner")
    matched_count = len(merged_data)

    print(
        f"  Successfully matched {matched_count} out of {len(tmk_df)} TMKs "
        f"({matched_count / len(tmk_df) * 100:.1f}%)."
    )

    if matched_count == 0:
        print("\n[FAIL] 0 TMK matches. Check cadastral CSV format.")
        raise SystemExit(1)

    print(f"\n[SUCCESS] TMK format verified. Proceeding to Step 5.")
    print("\n[DONE] Step 4 Complete.")


def run_step5():
    print("\n" + "=" * 70)
    print("Phase 5 - Step 5: Geographic Concentration Breakdown")
    print("=" * 70 + "\n")

    if not TMK_LIST_CSV.exists():
        print(f"[FATAL] TMK list not found. Run Step 2 first.\n  {TMK_LIST_CSV}")
        raise SystemExit(1)

    tax_file_to_use = next((p for p in TAX_CSV_CANDIDATES if p.exists()), None)
    if tax_file_to_use is None:
        candidates = "\n  ".join(str(p) for p in TAX_CSV_CANDIDATES)
        print(f"[FATAL] No Honolulu cadastral CSV found. Expected:\n  {candidates}")
        raise SystemExit(1)

    tmk_df = pd.read_csv(TMK_LIST_CSV)
    tmk_df.rename(columns={tmk_df.columns[0]: "TMK"}, inplace=True)
    tmk_df["TMK_clean"] = tmk_df["TMK"].astype(str).str.replace(r"[^0-9]", "", regex=True)

    tax_data = pd.read_csv(tax_file_to_use)
    tmk_col  = next(
        (c for c in tax_data.columns if re.search(r"(?i)^tmk$", c)),
        None,
    )
    if tmk_col is None:
        print("[FATAL] No TMK column identified in cadastral CSV.")
        raise SystemExit(1)

    tax_data["TMK_clean"] = tax_data[tmk_col].astype(str).str.replace(r"[^0-9]", "", regex=True)

    step2_lens = tmk_df["TMK_clean"].str.len()
    csv_lens   = tax_data["TMK_clean"].dropna().str.len()

    # 8-digit format = Z S PPP QQQ  (3-digit parcel field)
    # 9-digit format = Z S PPP QQQQ (4-digit parcel field, trailing 0 for non-CPR parcels)
    if (step2_lens == 8).all() and (csv_lens == 9).all():
        tmk_df["TMK_clean"] = tmk_df["TMK_clean"] + "0"
    elif (step2_lens == 9).all() and (csv_lens == 8).all():
        tax_data["TMK_clean"] = tax_data["TMK_clean"] + "0"

    merged_data = tmk_df.merge(tax_data, on="TMK_clean", how="inner")
    merged_data = merged_data.dropna(subset=["Zone"])

    merged_data["Zone_Code"]    = merged_data["Zone"].astype(int).astype(str)
    merged_data["District_Name"] = merged_data["Zone_Code"].map(DISTRICT_MAP).fillna(
        "Zone " + merged_data["Zone_Code"]
    )

    geo_summary = (
        merged_data
        .groupby(["Zone_Code", "District_Name"], as_index=False)
        .agg(Parcel_Count=("Zone_Code", "count"))
        .assign(Pct_of_Total_Parcels=lambda df: df["Parcel_Count"] / df["Parcel_Count"].sum() * 100)
        .sort_values("Parcel_Count", ascending=False)
        .reset_index(drop=True)
    )

    print(f"{'Zone':<5} {'Geographic District':<35} {'Parcel Count':<15} {'% of Parcels':<15}")
    print("-" * 70)
    for _, row in geo_summary.iterrows():
        print(
            f"{row['Zone_Code']:<5} {row['District_Name']:<35} "
            f"{int(row['Parcel_Count']):<15} {row['Pct_of_Total_Parcels']:.1f}%"
        )
    print("-" * 70)
    print(f"{'':5} {'TOTAL':<35} {int(geo_summary['Parcel_Count'].sum()):<15} 100.0%")
    print("-" * 70)

    geo_summary.to_csv(GEO_BREAKDOWN_CSV, index=False)
    print(f"\n[+] Geographic Breakdown saved -> {GEO_BREAKDOWN_CSV.name}")
    print("\n[DONE] Step 5 Complete.")


def run_step6():
    print("\n" + "=" * 70)
    print("Phase 5 - Step 6: Zoning Intersection Analysis")
    print("=" * 70 + "\n")

    if not TARGET_GOLF_GPKG.exists():
        print(f"[FATAL] Target Golf Polygons not found. Run Step 1.\n  {TARGET_GOLF_GPKG}")
        raise SystemExit(1)
    if not ZONING_GPKG.exists():
        print(f"[FATAL] Zoning layer not found:\n  {ZONING_GPKG}")
        raise SystemExit(1)

    print("[Step 1] Loading spatial datasets...")
    # [METHODOLOGY] gpd.read_file - spatial read of Step 1 Oahu golf polygons for zoning overlay
    golf_gdf   = gpd.read_file(TARGET_GOLF_GPKG)
    # [METHODOLOGY] gpd.read_file - spatial read of Honolulu zoning layer
    zoning_gdf = gpd.read_file(ZONING_GPKG)
    print(f"  Golf polygons:  {len(golf_gdf):,} features  (CRS: EPSG {golf_gdf.crs.to_epsg()})")
    print(f"  Zoning layer:   {len(zoning_gdf):,} features  (CRS: EPSG {zoning_gdf.crs.to_epsg()})")

    # [METHODOLOGY] Zoning is in EPSG 3760 (ftUS); reprojected to match golf CRS
    #               (EPSG 5070, metres) so .geometry.area returns m², convertible to acres.
    if zoning_gdf.crs != golf_gdf.crs:
        print(
            f"\n[Step 2] Reprojecting zoning from EPSG {zoning_gdf.crs.to_epsg()}"
            f" -> EPSG {golf_gdf.crs.to_epsg()}..."
        )
        zoning_gdf = zoning_gdf.to_crs(golf_gdf.crs)
        print("  Reprojection complete.")

    county_zone_acres = (
        zoning_gdf
        .assign(zone_total_acres=zoning_gdf.geometry.area / M2_PER_ACRE)
        .groupby("zone_class", as_index=False)["zone_total_acres"]
        .sum()
        .rename(columns={"zone_total_acres": "county_total_acres"})
    )

    # [METHODOLOGY] gpd.overlay(how='intersection') - clips zoning polygons to golf
    #               boundaries, quantifying which zone classes cover the golf footprint.
    print("\n[Step 3] Performing spatial intersection (golf courses x zoning)...")
    golf_sub   = golf_gdf[[golf_gdf.geometry.name]]
    zoning_sub = zoning_gdf[["zone_class", "zoning_description", zoning_gdf.geometry.name]]
    intersection_gdf = gpd.overlay(golf_sub, zoning_sub, how="intersection", keep_geom_type=False)
    print(f"  Intersection produced {len(intersection_gdf):,} fragments.")

    print("\n[Step 4] Calculating fragment areas in acres...")
    intersection_gdf["area_acres"] = intersection_gdf.geometry.area / M2_PER_ACRE
    total_golf_acres = intersection_gdf["area_acres"].sum()
    print(f"  Total intersected golf footprint: {total_golf_acres:.1f} acres")

    zone_summary = (
        intersection_gdf
        .groupby(["zone_class", "zoning_description"], as_index=False)
        .agg(acres=("area_acres", "sum"), fragments=("area_acres", "count"))
        .assign(pct_of_total=lambda df: df["acres"] / total_golf_acres * 100)
        .sort_values("acres", ascending=False)
        .reset_index(drop=True)
    )

    zone_penetration = (
        zone_summary[["zone_class", "zoning_description", "acres"]]
        .rename(columns={"acres": "golf_acres"})
        .merge(county_zone_acres, on="zone_class", how="left")
        .assign(pct_zone_as_golf=lambda df: df["golf_acres"] / df["county_total_acres"] * 100)
        .sort_values("pct_zone_as_golf", ascending=False)
        .reset_index(drop=True)
    )

    zone_summary.to_csv(ZONING_PCT_CSV, index=False)
    print(f"\n[+] Zoning percentages saved  -> {ZONING_PCT_CSV.name}")
    zone_penetration.to_csv(ZONE_PENETRATION_CSV, index=False)
    print(f"[+] Zone penetration saved    -> {ZONE_PENETRATION_CSV.name}")
    print("\n[DONE] Step 6 Complete.")


def main():
    print("\n" + "=" * 70)
    print("PHASE 5 - HAWAII MICRO-CASE STUDY (MASTER PIPELINE)")
    print("=" * 70)

    run_step1()
    osm_derived_acres = run_step2()
    run_step3(osm_derived_acres)
    run_step4()
    run_step5()
    run_step6()

    print("\n" + "=" * 70)
    print("PHASE 5 COMPLETE")
    print("=" * 70 + "\n")


if __name__ == "__main__":
    main()
    provenance.record_provenance("Phase 5", "Phase_5.py", SCRIPT_DIR, PROV_START)
