# Purpose: Master pipeline - parse, spatial-join, proxy-merge, and classify
#          baseline land values for all US golf courses (Phase 1).
# Inputs:  00 - Data Sources/Original Data/Golf Courses-USA.csv
#          00 - Data Sources/Original Data/2022 - USDA County Data - Ag Use.csv
#          00 - Data Sources/Original Data/2024 - FHFA June 20 Land Prices.xlsx
#          00 - Data Sources/Original Data/tl_2022_us_county.shp (vendored 2026-07-27,
#            source https://www2.census.gov/geo/tiger/TIGER2022/COUNTY/tl_2022_us_county.zip)
#          00 - Data Sources/Secondary/2023-rural-urban-continuum-codes.csv (vendored
#            2026-07-27, source https://www.ers.usda.gov/media/5768/2023-rural-urban-continuum-codes.csv)
# Outputs: Phase 1 Parsing/Data/Python/Py_Phase1_Spatial_Joined_Golf_Courses.csv
#          Phase 1 Parsing/Data/Python/Py_Phase1_Valuation_Joined_Golf_Courses.csv
#          Phase 1 Parsing/Data/Python/Py_Phase1_Baseline_Golf_Valuation.csv


# === 1. LIBRARIES ===

import re
import sys
import time
from pathlib import Path

import numpy as np
import pandas as pd
import geopandas as gpd


# === 2. GLOBALS & PATHS ===

SCRIPT_DIR = Path(__file__).parent
ROOT_DIR   = SCRIPT_DIR.parent
DATA_DIR   = ROOT_DIR / "00 - Data Sources" / "Original Data"
OUTPUT_DIR = SCRIPT_DIR / "Data" / "python"

OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

PROV_START = time.time()
sys.path.insert(0, str(ROOT_DIR))
import provenance

RAW_CSV    = DATA_DIR / "Golf Courses-USA.csv"
USDA_IN    = DATA_DIR / "2022 - USDA County Data - Ag Use.csv"
FHFA_IN    = DATA_DIR / "2024 - FHFA June 20 Land Prices.xlsx"
# Vendored 2026-07-27 (X-08/Gate-3 policy: no master script performs a network fetch at
# run time). Source: https://www.ers.usda.gov/media/5768/2023-rural-urban-continuum-codes.csv
RUCC_CSV   = ROOT_DIR / "00 - Data Sources" / "Secondary" / "2023-rural-urban-continuum-codes.csv"
# Vendored 2026-07-27. Source: https://www2.census.gov/geo/tiger/TIGER2022/COUNTY/tl_2022_us_county.zip
COUNTY_SHP = DATA_DIR / "tl_2022_us_county.shp"

OUT_SPATIAL  = OUTPUT_DIR / "Py_Phase1_Spatial_Joined_Golf_Courses.csv"
OUT_VAL_JOIN = OUTPUT_DIR / "Py_Phase1_Valuation_Joined_Golf_Courses.csv"
OUT_BASELINE = OUTPUT_DIR / "Py_Phase1_Baseline_Golf_Valuation.csv"


STATE_FIPS_TO_ABBR = {
    "01": "AL", "02": "AK", "04": "AZ", "05": "AR", "06": "CA",
    "08": "CO", "09": "CT", "10": "DE", "11": "DC", "12": "FL",
    "13": "GA", "15": "HI", "16": "ID", "17": "IL", "18": "IN",
    "19": "IA", "20": "KS", "21": "KY", "22": "LA", "23": "ME",
    "24": "MD", "25": "MA", "26": "MI", "27": "MN", "28": "MS",
    "29": "MO", "30": "MT", "31": "NE", "32": "NV", "33": "NH",
    "34": "NJ", "35": "NM", "36": "NY", "37": "NC", "38": "ND",
    "39": "OH", "40": "OK", "41": "OR", "42": "PA", "44": "RI",
    "45": "SC", "46": "SD", "47": "TN", "48": "TX", "49": "UT",
    "50": "VT", "51": "VA", "53": "WA", "54": "WV", "55": "WI",
    "56": "WY", "60": "AS", "66": "GU", "69": "MP", "72": "PR",
    "78": "VI",
}


# === 3. FUNCTIONS ===

def extract_ownership(detail_str: str) -> str | None:
    # [METHODOLOGY] first-parenthetical extraction, matching R/Julia exactly (P1-05) —
    # searches anywhere in the string (not anchored to position 0) so a "CLOSED|" prefix
    # (e.g. Turtle Creek Golf Club, FL) doesn't defeat the match
    m = re.search(r"\(([^)]+)\)", str(detail_str))
    return m.group(1) if m else None

def extract_holes(detail_str: str) -> int | None:
    d = str(detail_str)
    # [METHODOLOGY] strict "(N Holes)" pattern first
    m = re.search(r"\((\d+)\s*Holes?\)", d, re.IGNORECASE)
    if m:
        return int(m.group(1))
    # [METHODOLOGY] combo courses, e.g. "(18 Holes & 9 Holes)" -> sum the two nines
    m = re.search(r"\((\d+)\s*Holes?\s*&\s*(\d+)\s*Holes?\)", d, re.IGNORECASE)
    if m:
        return int(m.group(1)) + int(m.group(2))
    # [METHODOLOGY] bare "(N)" with no "Holes" text, restricted to the prefix before the
    # first comma (the Ownership/Holes segment) so a phone area code or zip can't match
    m = re.search(r"\((\d+)\)", d.split(",", 1)[0])
    if m:
        return int(m.group(1))
    return 18


# === 4. EXECUTION ===

def main():
    for path in (RAW_CSV, USDA_IN, FHFA_IN):
        if not path.exists():
            raise FileNotFoundError(f"Input file not found: {path}")

    print(" 1  Loading raw Golf Courses CSV")
    courses_df = pd.read_csv(RAW_CSV, header=None,
                             names=["Longitude", "Latitude", "Course_Name", "Details"])
    print(f"    Rows loaded: {len(courses_df):,}")

    print(" 2  Extracting Ownership_Type & Holes via regex")
    courses_df["Ownership_Type"] = courses_df["Details"].apply(extract_ownership)
    courses_df["Holes"]          = courses_df["Details"].apply(extract_holes)

    print(" 3  Deduplicating (lat/lon-round, Course_Name; ties broken by max Holes)")
    # [METHODOLOGY] matches R's arrange(desc(Holes)) |> slice(1) and Julia's
    # sort!(g, :Holes, rev=true); first(g) on the same (round(lat,4), round(lon,4),
    # Course_Name) key -- P1-06/Decision 4: Python previously had no course-level
    # dedup at all (5 duplicate source rows survived uncaught, all Urban).
    original_n = len(courses_df)
    courses_df["Lat_Round"] = courses_df["Latitude"].round(4)
    courses_df["Lon_Round"] = courses_df["Longitude"].round(4)
    courses_df = (courses_df
                  .sort_values("Holes", ascending=False, kind="mergesort")
                  .drop_duplicates(subset=["Lat_Round", "Lon_Round", "Course_Name"], keep="first")
                  .drop(columns=["Lat_Round", "Lon_Round"])
                  .sort_index()
                  .reset_index(drop=True))
    print(f"    Original raw rows:   {original_n:,}")
    print(f"    Duplicates removed:  {original_n - len(courses_df):,}")
    print(f"    Final cleaned rows:  {len(courses_df):,}")

    print(" 4  Converting to GeoDataFrame (EPSG:4326)")
    courses_df = courses_df.dropna(subset=["Longitude", "Latitude"])
    courses_geo = gpd.GeoDataFrame(
        courses_df,
        geometry=gpd.points_from_xy(courses_df["Longitude"], courses_df["Latitude"]),
        crs="EPSG:4326"
    )

    print(" 5  Reading 2022 US County boundaries (vendored TIGER/Line)")
    # [METHODOLOGY] CRS: EPSG 4326 (WGS 84) - projects county boundaries to match golf course point CRS for spatial join
    county_geo = gpd.read_file(COUNTY_SHP).to_crs("EPSG:4326")

    county_geo["STUSPS"] = county_geo["STATEFP"].map(STATE_FIPS_TO_ABBR).fillna("")

    print(" 6  Spatial point-in-polygon join")
    courses_geo = gpd.sjoin(  # [METHODOLOGY]
        courses_geo,
        county_geo[["GEOID", "NAME", "STUSPS", "geometry"]],
        how="left",
        predicate="intersects"
    )
    courses_geo = courses_geo.rename(columns={
        "GEOID": "FIPS", "NAME": "County_Name", "STUSPS": "Tigris_State_Abbr"
    })
    if "index_right" in courses_geo.columns:
        courses_geo.drop(columns=["index_right"], inplace=True)

    courses_df = pd.DataFrame(courses_geo.drop(columns=["geometry"]))
    courses_df.to_csv(OUT_SPATIAL, index=False)
    print(f"    [OK] Spatial data saved -> {OUT_SPATIAL}")

    courses_df["FIPS"] = (courses_df["FIPS"].astype(str)
                          .str.replace(r"\.0$", "", regex=True).str.zfill(5))

    print(" 7  Processing USDA County Ag-Use data")
    usda_df = pd.read_csv(USDA_IN)
    usda_df = usda_df[
        usda_df["Data Item"] == "AG LAND, INCL BUILDINGS - ASSET VALUE, MEASURED IN $ / ACRE"
    ].copy()
    usda_df["State ANSI"]  = (usda_df["State ANSI"].astype(str)
                               .str.replace(r"\.0$", "", regex=True).str.zfill(2))
    usda_df["County ANSI"] = (usda_df["County ANSI"].astype(str)
                               .str.replace(r"\.0$", "", regex=True).str.zfill(3))
    usda_df["FIPS"] = usda_df["State ANSI"] + usda_df["County ANSI"]
    usda_df["USDA_Ag_Value_Per_Acre"] = pd.to_numeric(
        usda_df["Value"].astype(str).str.replace(",", ""), errors="coerce"
    )
    usda_df = (usda_df[["FIPS", "USDA_Ag_Value_Per_Acre"]]
               .dropna(subset=["USDA_Ag_Value_Per_Acre"])
               .drop_duplicates(subset=["FIPS"]))

    print(" 8  Processing FHFA Panel Counties data")
    fhfa_df = pd.read_excel(FHFA_IN, sheet_name="Panel Counties", skiprows=1)
    fhfa_df = fhfa_df[fhfa_df["Year"] == 2022].copy()
    fhfa_df["FIPS"] = (fhfa_df["FIPS"].astype(str)
                        .str.replace(r"\.0$", "", regex=True).str.zfill(5))
    # [METHODOLOGY] dynamic column match (B-9) — matches R's grep()/Julia's occursin(),
    # avoids a hardcoded embedded-newline column name breaking on any source refresh
    as_is_matches = [c for c in fhfa_df.columns if "Per Acre, As-Is" in str(c)]
    if not as_is_matches:
        raise ValueError("Could not find 'Per Acre, As-Is' column in FHFA data")
    as_is_col = as_is_matches[0]
    fhfa_df = (fhfa_df[["FIPS", as_is_col]]
               .rename(columns={as_is_col: "FHFA_Res_Value_Per_Acre"})
               .drop_duplicates(subset=["FIPS"]))
    fhfa_df["FHFA_Res_Value_Per_Acre"] = pd.to_numeric(
        fhfa_df["FHFA_Res_Value_Per_Acre"], errors="coerce"
    )

    print(" 9  Left-joining proxies onto golf courses")
    courses_df = (courses_df
                  .merge(usda_df, on="FIPS", how="left")
                  .merge(fhfa_df, on="FIPS", how="left"))
    courses_df.to_csv(OUT_VAL_JOIN, index=False)
    print(f"    [OK] Valuation data saved -> {OUT_VAL_JOIN}")

    print(" 10 Reading 2023 RUCC data (vendored)")
    rucc_df = pd.read_csv(RUCC_CSV, encoding="latin1")
    rucc_df = rucc_df[rucc_df["Attribute"] == "RUCC_2023"].copy()
    rucc_df["RUCC_2023"] = pd.to_numeric(rucc_df["Value"], errors="coerce")
    rucc_df["FIPS"] = (rucc_df["FIPS"].astype(str)
                       .str.replace(r"\.0$", "", regex=True).str.zfill(5))
    rucc_df = rucc_df[["FIPS", "RUCC_2023"]].drop_duplicates(subset=["FIPS"])

    print(" 11 Merging RUCC and Classifying Urban/Rural")
    courses_df = courses_df.merge(rucc_df, on="FIPS", how="left")
    courses_df["county_type"] = np.where(
        courses_df["RUCC_2023"].isin([1, 2, 3]), "Urban",
        np.where(courses_df["RUCC_2023"].between(4, 9), "Rural", None)
    )

    print(" 12 Building Baseline_Value_Per_Acre")
    courses_df["Baseline_Value_Per_Acre"] = np.where(
        courses_df["county_type"] == "Urban",  courses_df["FHFA_Res_Value_Per_Acre"],
        np.where(courses_df["county_type"] == "Rural", courses_df["USDA_Ag_Value_Per_Acre"], np.nan)
    )

    n            = len(courses_df)
    missing_fips = courses_df["FIPS"].replace("nan", np.nan).isna().sum()
    usda_hit     = courses_df["USDA_Ag_Value_Per_Acre"].notna().sum()
    fhfa_hit     = courses_df["FHFA_Res_Value_Per_Acre"].notna().sum()
    urban        = (courses_df["county_type"] == "Urban").sum()
    rural        = (courses_df["county_type"] == "Rural").sum()
    unclassified = courses_df["county_type"].isna().sum()
    missing_base = courses_df["Baseline_Value_Per_Acre"].isna().sum()
    bv           = courses_df["Baseline_Value_Per_Acre"].dropna()

    print("\n=== OUTPUT STATISTICS ===")
    print(f"  Total golf courses:       {n:,}")
    print(f"  Missing FIPS (no county): {missing_fips:,}")
    print(f"  USDA match rate:          {usda_hit:,} / {n:,}  ({usda_hit/n:.2%})")
    print(f"  FHFA match rate:          {fhfa_hit:,} / {n:,}  ({fhfa_hit/n:.2%})")
    print(f"  Urban courses:            {urban:,}")
    print(f"  Rural courses:            {rural:,}")
    print(f"  Unclassified (no RUCC):   {unclassified:,}")
    print(f"  Missing Baseline value:   {missing_base:,}  (MICE imputation target)")
    print(f"\n  Baseline_Value_Per_Acre summary:")
    print(f"    Min:    ${bv.min():>14,.2f}")
    print(f"    Median: ${bv.median():>14,.2f}")
    print(f"    Mean:   ${bv.mean():>14,.2f}")
    print(f"    Max:    ${bv.max():>14,.2f}")

    courses_df.to_csv(OUT_BASELINE, index=False)
    print(f"\n  [OK] Final Baseline saved -> {OUT_BASELINE}")


if __name__ == "__main__":
    main()
    provenance.record_provenance("Phase 1", "Phase_1.py", SCRIPT_DIR, PROV_START)
