# Purpose: Phase 2 master - OSM polygon extraction + spatial matching.
#          Step 1 streams the 11 GB PBF via pyosmium, reprojects to EPSG:5070,
#          computes true acreage, and saves a GeoPackage.
#          Step 2 matches polygons to Phase 1 points via spatial intersect
#          and 500 m nearest-neighbour fallback, then saves a flat CSV.
# Inputs:  00 - Data Sources/Original Data/us-260413.osm.pbf
#          Phase 1 Parsing/Data/Python/Py_Phase1_Baseline_Golf_Valuation.csv
# Outputs: Data/Python/Py_Phase2_OSM_Golf_Polygons.gpkg
#          Data/Python/Py_Phase2_Acreage_Matched.csv


# === 1. LIBRARIES ===

import sys
import time
from pathlib import Path
import pandas as pd
import geopandas as gpd
import shapely.wkb as wkblib
import osmium


# === 2. GLOBALS & PATHS ===

SCRIPT_DIR = Path(__file__).parent
ROOT_DIR   = SCRIPT_DIR.parent

PROV_START = time.time()
sys.path.insert(0, str(ROOT_DIR))
import provenance

PBF_FILE   = ROOT_DIR / "00 - Data Sources" / "Original Data" / "us-260413.osm.pbf"
OUT_GPKG   = SCRIPT_DIR / "Data" / "python" / "Py_Phase2_OSM_Golf_Polygons.gpkg"
PHASE1_CSV = ROOT_DIR / "Phase 1 Parsing" / "Data" / "python" / "Py_Phase1_Baseline_Golf_Valuation.csv"
OUT_CSV    = SCRIPT_DIR / "Data" / "python" / "Py_Phase2_Acreage_Matched.csv"

MIN_ACRES     = 5
MAX_ACRES     = 1500
SQ_M_PER_ACRE = 4046.8564224
MAX_NEAREST_M = 500


# === 3. FUNCTIONS ===

class GolfCourseHandler(osmium.SimpleHandler):
    """Stream through every OSM area; keep only leisure=golf_course."""

    def __init__(self):
        super().__init__()
        self.wkb_fab = osmium.geom.WKBFactory()
        self.records = []
        self.errors  = 0

    def area(self, a):
        if a.tags.get("leisure") != "golf_course":
            return
        try:
            wkb  = self.wkb_fab.create_multipolygon(a)
            geom = wkblib.loads(wkb, hex=True)
            self.records.append({
                "osm_id":   a.id,
                "name":     a.tags.get("name", "Unknown"),
                "geometry": geom,
            })
        except Exception:
            self.errors += 1


def extract_osm_polygons():
    """Step 1: Extract golf-course polygons from OSM PBF file."""
    print("=" * 60)
    print("STEP 1: Extracting OSM Golf Course Polygons")
    print("=" * 60)

    # 1. Stream the PBF
    print(f"1 Streaming {PBF_FILE.stat().st_size / 1e9:.1f} GB PBF through pyosmium handler")
    print(f"    File: {PBF_FILE}")
    t0 = time.time()

    handler = GolfCourseHandler()
    # sparse_mem_array allocates per node seen, not by max node ID - avoids the
    # multi-GB upfront allocation that flex_mem (the default) triggers on US-scale PBFs.
    handler.apply_file(str(PBF_FILE), locations=True, idx="sparse_mem_array")

    elapsed = time.time() - t0
    print(f"    Done in {elapsed/60:.1f} minutes.")
    print(f"    Raw polygons captured: {len(handler.records):,}")
    print(f"    Geometry build errors: {handler.errors}")

    if not handler.records:
        print("    ERROR: No polygons extracted. Aborting Step 1.")
        return None

    # 2. Build GeoDataFrame in EPSG:4326 then reproject
    print("2 Building GeoDataFrame & reprojecting to EPSG:5070")
    osm_golf_geo = gpd.GeoDataFrame(handler.records, crs="EPSG:4326")
    osm_golf_geo = osm_golf_geo.to_crs(epsg=5070)  # [METHODOLOGY] EPSG:5070 - equal-area CRS for accurate acreage

    # 3. Calculate area & convert to acres
    print("3 Calculating acreage")
    osm_golf_geo["osm_acreage"] = osm_golf_geo.geometry.area / SQ_M_PER_ACRE  # [METHODOLOGY]

    raw_count    = len(osm_golf_geo)
    osm_golf_geo = osm_golf_geo[
        (osm_golf_geo["osm_acreage"] >= MIN_ACRES) &
        (osm_golf_geo["osm_acreage"] <= MAX_ACRES)
    ].copy()
    filtered_count = len(osm_golf_geo)
    dropped        = raw_count - filtered_count

    # 4. Report
    print(f"\n=== STEP 1 OUTPUT STATISTICS ===")
    print(f"  Raw polygons before filter:   {raw_count:,}")
    print(f"  Dropped (< {MIN_ACRES} or > {MAX_ACRES} acres): {dropped:,}")
    print(f"  Final polygon count:          {filtered_count:,}")

    ac = osm_golf_geo["osm_acreage"]
    print(f"\n  osm_acreage summary:")
    print(f"    Min:    {ac.min():>10,.1f} acres")
    print(f"    Median: {ac.median():>10,.1f} acres")
    print(f"    Mean:   {ac.mean():>10,.1f} acres")
    print(f"    Max:    {ac.max():>10,.1f} acres")

    # 5. Save
    print(f"\nSaving to {OUT_GPKG}")
    OUT_GPKG.parent.mkdir(parents=True, exist_ok=True)
    osm_golf_geo.to_file(str(OUT_GPKG), driver="GPKG")  # [METHODOLOGY]
    print(f"  [OK] Saved -> {OUT_GPKG}")

    return osm_golf_geo


def match_osm_to_courses(osm_golf_geo):
    """Step 2: Match OSM polygons to Phase 1 points."""
    print("\n" + "=" * 60)
    print("STEP 2: Matching OSM Polygons to Phase 1 Points")
    print("=" * 60)

    # 1. Load Phase 1 points
    print("1 Loading Phase 1 baseline dataset")
    courses_df  = pd.read_csv(PHASE1_CSV)
    courses_df  = courses_df.dropna(subset=["Longitude", "Latitude"])
    courses_geo = gpd.GeoDataFrame(
        courses_df,
        geometry=gpd.points_from_xy(courses_df["Longitude"], courses_df["Latitude"]),
        crs="EPSG:4326",
    ).to_crs(epsg=5070)  # [METHODOLOGY] EPSG:5070 - equal-area CRS for distance accuracy
    print(f"    Phase 1 rows: {len(courses_geo):,}")

    # 2. Load OSM polygons (already in EPSG:5070)
    print("2 Loading OSM golf polygons")
    osm_golf_geo = osm_golf_geo.copy()
    if osm_golf_geo.crs.to_epsg() != 5070:
        osm_golf_geo = osm_golf_geo.to_crs(epsg=5070)  # [METHODOLOGY]
    print(f"    OSM polygons: {len(osm_golf_geo):,}")

    # 3a. Primary join: intersects
    print("3a Spatial join (intersects)")
    courses_geo = gpd.sjoin(  # [METHODOLOGY] point-in-polygon primary match
        courses_geo,
        osm_golf_geo[["osm_acreage", "geometry"]],
        how="left",
        predicate="intersects",
    )
    if "index_right" in courses_geo.columns:
        courses_geo.drop(columns=["index_right"], inplace=True)

    # [METHODOLOGY] tie-break: when a point falls inside >1 candidate polygon,
    # keep the largest-area match (matches R's explicit largest-area-wins rule)
    courses_geo = courses_geo.sort_values("osm_acreage", ascending=False)

    matched_mask = courses_geo["osm_acreage"].notna()
    n_hit  = matched_mask.sum()
    n_miss = (~matched_mask).sum()
    print(f"    Direct intersect hits:   {n_hit:,}")
    print(f"    Misses (need fallback):  {n_miss:,}")

    # 3b. Fallback: nearest within 500 m
    if n_miss > 0:
        print(f"3b Nearest-neighbor fallback (max {MAX_NEAREST_M} m)")
        miss_geo = courses_geo.loc[courses_geo["osm_acreage"].isna()].copy()

        nearest = gpd.sjoin_nearest(  # [METHODOLOGY] nearest-feature fallback for unmatched courses
            miss_geo[["geometry"]],
            osm_golf_geo[["osm_acreage", "geometry"]],
            how="left",
            max_distance=MAX_NEAREST_M,
            distance_col="_dist",
        )
        # [METHODOLOGY] tie-break: among equidistant candidates, keep the largest-area match
        nearest = nearest.sort_values(["_dist", "osm_acreage"], ascending=[True, False])
        nearest = nearest[~nearest.index.duplicated(keep="first")]
        courses_geo.loc[nearest.index, "osm_acreage"] = nearest["osm_acreage"].values

        n_recovered = nearest["osm_acreage"].notna().sum()
        print(f"    Recovered via nearest:   {n_recovered:,}")

    # 4. De-duplicate (a point inside overlapping polygons creates dupes)
    courses_geo = courses_geo[~courses_geo.index.duplicated(keep="first")]

    courses_geo["acreage_source"] = courses_geo["osm_acreage"].apply(
        lambda x: "MICE_Target" if pd.isna(x) else "OSM"
    )

    # 5. Final counts
    total     = len(courses_geo)
    has_acre  = courses_geo["osm_acreage"].notna().sum()
    miss_acre = courses_geo["osm_acreage"].isna().sum()

    print(f"\n=== STEP 2 OUTPUT STATISTICS ===")
    print(f"  Total courses:               {total:,}")
    print(f"  Matched with osm_acreage:    {has_acre:,}  ({has_acre/total:.1%})")
    print(f"  Missing osm_acreage:         {miss_acre:,}  (MICE target)")

    ac = courses_geo["osm_acreage"].dropna()
    if len(ac) > 0:
        print(f"\n  osm_acreage summary (matched only):")
        print(f"    Min:    {ac.min():>10,.1f} acres")
        print(f"    Median: {ac.median():>10,.1f} acres")
        print(f"    Mean:   {ac.mean():>10,.1f} acres")
        print(f"    Max:    {ac.max():>10,.1f} acres")

    # 6. Drop geometry, save
    OUT_CSV.parent.mkdir(parents=True, exist_ok=True)
    out = courses_geo.drop(columns=["geometry"])
    out.to_csv(str(OUT_CSV), index=False)
    print(f"\n  [OK] Saved -> {OUT_CSV}")

    return out


def main():
    if not PHASE1_CSV.exists():
        raise FileNotFoundError(f"Phase 1 CSV not found: {PHASE1_CSV}")

    if OUT_GPKG.exists():
        print(f"  [CACHE] GPKG found — skipping PBF parse.")
        print(f"          Loading: {OUT_GPKG}")
        osm_golf_geo = gpd.read_file(str(OUT_GPKG))
    else:
        if not PBF_FILE.exists():
            raise FileNotFoundError(f"PBF file not found: {PBF_FILE}")
        osm_golf_geo = extract_osm_polygons()
        if osm_golf_geo is None:
            print("\nStep 1 failed. Aborting pipeline.")
            return

    match_osm_to_courses(osm_golf_geo)

    print("\n" + "=" * 60)
    print("PHASE 2 PIPELINE COMPLETE")
    print("=" * 60)
    print(f"\nOutput files:")
    print(f"  Polygons: {OUT_GPKG}")
    print(f"  Matched:  {OUT_CSV}")


# === 4. EXECUTION ===

if __name__ == "__main__":
    main()
    provenance.record_provenance("Phase 2", "Phase_2.py", SCRIPT_DIR, PROV_START)
