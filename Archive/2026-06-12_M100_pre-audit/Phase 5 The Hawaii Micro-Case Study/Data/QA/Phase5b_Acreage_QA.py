"""
Phase5b_Acreage_QA.py
=====================
Read-only QA script that reproduces every figure in Phase5_Acreage_Verification.md
directly from the three language output CSVs.

Measurements produced
---------------------
(a) OSM polygon acreage        -- from *_Oahu_Comparison.csv
(b) Cadastre-intersected total -- sum of 'acres' column in *_Step6_Zoning_Percentages.csv
(c) Zoning-intersected total   -- identical to (b); same parcel set, partitioned by zone

Run from anywhere inside the repo; the script resolves all paths relative to its
own location so no edits are required.

Output: printed report + Phase5b_Acreage_QA_Results.csv written next to this script.
"""

import csv
import pathlib
import sys

# Force UTF-8 output on Windows (avoids cp1252 encoding errors)
if hasattr(sys.stdout, "reconfigure"):
    sys.stdout.reconfigure(encoding="utf-8")

# ---------------------------------------------------------------------------
# 1.  Path setup (all relative to this script's directory)
# ---------------------------------------------------------------------------
SCRIPT_DIR = pathlib.Path(__file__).resolve().parent   # .../Data/QA/
DATA_DIR   = SCRIPT_DIR.parent                         # .../Data/  (parent of QA/)

SOURCES = {
    "Python": {
        "comparison": DATA_DIR / "python" / "Py_Phase5_Oahu_Comparison.csv",
        "zoning":     DATA_DIR / "python" / "Py_Phase5_Step6_Zoning_Percentages.csv",
    },
    "R": {
        "comparison": DATA_DIR / "R" / "Phase5_Oahu_Comparison.csv",
        "zoning":     DATA_DIR / "R" / "Phase5_Step6_Zoning_Percentages.csv",
    },
    "Julia": {
        "comparison": DATA_DIR / "Julia" / "Jl_Phase5_Oahu_Comparison.csv",
        "zoning":     DATA_DIR / "Julia" / "Jl_Phase5_Step6_Zoning_Percentages.csv",
    },
}

# ---------------------------------------------------------------------------
# 2.  Helpers
# ---------------------------------------------------------------------------

def read_comparison(path: pathlib.Path) -> dict:
    """
    Parse a key-value *_Oahu_Comparison.csv and return a plain dict.
    Values that look numeric (after stripping commas) are cast to float.
    """
    result = {}
    with open(path, encoding="utf-8-sig", newline="") as fh:
        reader = csv.reader(fh)
        next(reader)          # skip header row  (Metric,Value)
        for row in reader:
            if len(row) < 2 or not row[0].strip():
                continue
            key = row[0].strip()
            raw = row[1].strip().replace(",", "")
            try:
                result[key] = float(raw)
            except ValueError:
                result[key] = raw
    return result


def read_zoning(path: pathlib.Path) -> list[dict]:
    """
    Parse a *_Step6_Zoning_Percentages.csv and return a list of row dicts.
    The 'acres' and 'pct_of_total' columns are cast to float.
    """
    rows = []
    with open(path, encoding="utf-8-sig", newline="") as fh:
        reader = csv.DictReader(fh)
        for row in reader:
            if not row.get("zone_class", "").strip():
                continue
            row["acres"]        = float(row["acres"].strip().replace(",", ""))
            row["pct_of_total"] = float(row["pct_of_total"].strip().replace(",", ""))
            rows.append(row)
    return rows


# Zone-class grouping rules
PRES_FEDERAL = {"P-1", "P-2", "F-1"}
AGRICULTURE  = {"AG-1", "AG-2"}
# everything else → Resort / Residential / Country / Other


def group_zone_rows(rows: list[dict]) -> tuple[float, float, float, float]:
    """Return (pres_fed_ac, ag_ac, other_ac, total_ac) for a zoning table."""
    pf = ag = other = 0.0
    for r in rows:
        zc = r["zone_class"].strip()
        ac = r["acres"]
        if zc in PRES_FEDERAL:
            pf += ac
        elif zc in AGRICULTURE:
            ag += ac
        else:
            other += ac
    return pf, ag, other, pf + ag + other


def fmt(val: float, decimals: int = 2) -> str:
    return f"{val:,.{decimals}f}"


# ---------------------------------------------------------------------------
# 3.  Load all data
# ---------------------------------------------------------------------------
print("\n" + "=" * 70)
print("  Phase 5b -- Oahu Golf Footprint Acreage QA")
print("=" * 70)

# Verify files exist before proceeding
missing = []
for lang, paths in SOURCES.items():
    for label, p in paths.items():
        if not p.exists():
            missing.append(str(p))
if missing:
    print("\n[ERROR] The following source files were not found:")
    for m in missing:
        print(f"  {m}")
    sys.exit(1)

comparisons = {lang: read_comparison(SOURCES[lang]["comparison"]) for lang in SOURCES}
zonings     = {lang: read_zoning(SOURCES[lang]["zoning"])         for lang in SOURCES}

OSM_KEY = "OSM-Derived Legal Footprint (acres)"

# ---------------------------------------------------------------------------
# 4.  Measurement (a): OSM polygon acreage
# ---------------------------------------------------------------------------
osm = {lang: comparisons[lang][OSM_KEY] for lang in SOURCES}
grand_osm = sum(osm.values()) / 3

print("\n-- (a) OSM Polygon Acreage  [Step 1 output] " + "-" * 28)
print(f"  {'Language':<10} {'Acres':>12}")
print(f"  {'-'*10} {'-'*12}")
for lang in ("Python", "R", "Julia"):
    print(f"  {lang:<10} {fmt(osm[lang]):>12}")
print(f"  {'Grand Mean':<10} {fmt(grand_osm):>12}")

# Also report course and TMK counts as a sanity check
for lang in ("Python", "R", "Julia"):
    cmp = comparisons[lang]
    course_key = next((k for k in cmp if "Golf Courses" in k), None)
    tmk_key    = next((k for k in cmp if "Total Unique TMKs" in k), None)
    courses = int(cmp[course_key]) if course_key else "N/A"
    tmks    = int(cmp[tmk_key])    if tmk_key    else "N/A"
    print(f"  [{lang}] Courses: {courses}  |  Unique TMKs: {tmks}")

# ---------------------------------------------------------------------------
# 5.  Measurements (b) and (c): cadastre / zoning-intersected totals
# ---------------------------------------------------------------------------
# Sum all zone acres per language → both (b) and (c) in one pass
zone_data = {}
for lang in SOURCES:
    pf, ag, other, total = group_zone_rows(zonings[lang])
    zone_data[lang] = {"pf": pf, "ag": ag, "other": other, "total": total}

cad_totals = {lang: zone_data[lang]["total"] for lang in SOURCES}
grand_cad  = sum(cad_totals.values()) / 3

print("\n-- (b) Cadastre-Intersected Acreage  [Step 2, recovered from Step 6 zone sum]")
print(f"  {'Language':<10} {'Acres':>12}")
print(f"  {'-'*10} {'-'*12}")
for lang in ("Python", "R", "Julia"):
    print(f"  {lang:<10} {fmt(cad_totals[lang]):>12}")
print(f"  {'Grand Mean':<10} {fmt(grand_cad):>12}")

print("\n-- (c) Zoning-Intersected Acreage  [Step 6 sum -- identical to (b)]")
print("  (b) and (c) are the same value: Step 6 partitions the exact")
print("  Step-2 parcel set by dominant zone class, so the column sum")
print("  recovers the cadastre-intersected total exactly.")
print(f"  Grand Mean (c) = {fmt(grand_cad)}")

# ---------------------------------------------------------------------------
# 6.  OSM-vs-cadastre divergence check
# ---------------------------------------------------------------------------
divergence_pct = abs(grand_osm - grand_cad) / grand_osm * 100
gap_ac         = grand_osm - grand_cad

print("\n-- OSM vs. Cadastre Divergence Check " + "-" * 34)
print(f"  Grand Mean (a) OSM polygons      : {fmt(grand_osm)} ac")
print(f"  Grand Mean (b)/(c) cadastre/zone : {fmt(grand_cad)} ac")
print(f"  Absolute gap                     : {fmt(gap_ac)} ac")
print(f"  Relative divergence              : {divergence_pct:.1f}%")

threshold = 5.0
if divergence_pct > threshold:
    print(f"\n  [FLAG] Divergence exceeds {threshold}% threshold.")
    print("  This is expected: OSM polygons include fairways/roughs that")
    print("  extend over un-titled land (public ROW, streambeds, military")
    print("  easements) with no TMK parcel. The cookie-cutter Step 2")
    print("  retains only OSM-golf overlap within titled cadastral parcels.")
    print("  → Canonical figure: cadastre/zoning-intersected total (b)/(c).")
else:
    print(f"\n  [OK] Divergence is within the {threshold}% flag threshold.")

# ---------------------------------------------------------------------------
# 7.  Per-language zoning group breakdown
# ---------------------------------------------------------------------------
print("\n-- Per-Language Zoning Group Breakdown " + "-" * 32)
print(f"  {'Language':<10} {'Pres+Fed':>11} {'Agriculture':>12} {'Other':>10} {'Total':>10}")
print(f"  {'-'*10} {'-'*11} {'-'*12} {'-'*10} {'-'*10}")
for lang in ("Python", "R", "Julia"):
    d = zone_data[lang]
    print(f"  {lang:<10} {fmt(d['pf']):>11} {fmt(d['ag']):>12} {fmt(d['other']):>10} {fmt(d['total']):>10}")

gm_pf    = sum(zone_data[lang]["pf"]    for lang in SOURCES) / 3
gm_ag    = sum(zone_data[lang]["ag"]    for lang in SOURCES) / 3
gm_other = sum(zone_data[lang]["other"] for lang in SOURCES) / 3

print(f"  {'Grand Mean':<10} {fmt(gm_pf):>11} {fmt(gm_ag):>12} {fmt(gm_other):>10} {fmt(grand_cad):>10}")

# ---------------------------------------------------------------------------
# 8.  Actual % shares from Grand Mean (c)
# ---------------------------------------------------------------------------
pf_pct    = gm_pf    / grand_cad * 100
ag_pct    = gm_ag    / grand_cad * 100
other_pct = gm_other / grand_cad * 100

print("\n-- Actual % Shares from Grand Mean (c) " + "-" * 31)
print(f"  {'Zone Group':<30} {'Acres':>10} {'%':>8}")
print(f"  {'-'*30} {'-'*10} {'-'*8}")
print(f"  {'Preservation + Federal':<30} {fmt(gm_pf):>10} {pf_pct:>7.1f}%")
print(f"  {'Agriculture':<30} {fmt(gm_ag):>10} {ag_pct:>7.1f}%")
print(f"  {'Resort / Res / Country / Other':<30} {fmt(gm_other):>10} {other_pct:>7.1f}%")
print(f"  {'TOTAL':<30} {fmt(grand_cad):>10} {'100.0':>8}%")

# ---------------------------------------------------------------------------
# 9.  Comparison against Phase 5 Summary stated figures
# ---------------------------------------------------------------------------
STATED_TOTAL   = 6067.0
STATED_PF_PCT  = 0.817
STATED_AG_PCT  = 0.138
STATED_OTH_PCT = 0.045

diff_total = grand_cad - STATED_TOTAL
diff_pct   = diff_total / STATED_TOTAL * 100

print("\n-- Comparison Against Phase 5 Summary Stated Figures " + "-" * 18)
print(f"  Summary states : ~{STATED_TOTAL:,.0f} ac")
print(f"  Grand Mean (c) : {fmt(grand_cad)} ac")
print(f"  Difference     : {diff_total:+.2f} ac  ({diff_pct:+.2f}%)")

if abs(diff_pct) < 0.5:
    print("  [OK] Stated total is consistent with computed Grand Mean (c).")

# Apply stated % shares to Grand Mean and compare to actual zone data
stated_pf  = STATED_PF_PCT  * grand_cad
stated_ag  = STATED_AG_PCT  * grand_cad
stated_oth = STATED_OTH_PCT * grand_cad

print(f"\n  Applying stated shares ({STATED_PF_PCT*100:.1f}% / {STATED_AG_PCT*100:.1f}% / {STATED_OTH_PCT*100:.1f}%) to Grand Mean (c):")
print(f"  {'Zone Group':<30} {'From stated %':>14} {'From zone data':>15} {'Diff':>8}")
print(f"  {'-'*30} {'-'*14} {'-'*15} {'-'*8}")
print(f"  {'Pres+Federal':<30} {fmt(stated_pf):>14} {fmt(gm_pf):>15} {stated_pf - gm_pf:>+8.2f}")
print(f"  {'Agriculture':<30} {fmt(stated_ag):>14} {fmt(gm_ag):>15} {stated_ag - gm_ag:>+8.2f}")
print(f"  {'Other':<30} {fmt(stated_oth):>14} {fmt(gm_other):>15} {stated_oth - gm_other:>+8.2f}")

# ---------------------------------------------------------------------------
# 10.  Per-language zone-class detail table
# ---------------------------------------------------------------------------
print("\n-- Per-Language Zone-Class Detail " + "-" * 37)
all_zones = []
for lang in SOURCES:
    for row in zonings[lang]:
        zc = row["zone_class"].strip()
        if zc not in all_zones:
            all_zones.append(zc)

header = f"  {'Zone':<10} {'Description':<44} {'Python':>10} {'R':>10} {'Julia':>10} {'MaxDiff':>8}"
print(header)
print("  " + "-" * (len(header) - 2))

for zc in all_zones:
    ac_by_lang = {}
    desc = ""
    for lang in ("Python", "R", "Julia"):
        match = next((r for r in zonings[lang] if r["zone_class"].strip() == zc), None)
        if match:
            ac_by_lang[lang] = match["acres"]
            desc = match["zoning_description"]
        else:
            ac_by_lang[lang] = float("nan")

    vals = [v for v in ac_by_lang.values() if v == v]  # exclude NaN
    max_diff = max(vals) - min(vals) if len(vals) > 1 else 0.0

    py_s  = fmt(ac_by_lang.get("Python", float("nan")))
    r_s   = fmt(ac_by_lang.get("R",      float("nan")))
    jl_s  = fmt(ac_by_lang.get("Julia",  float("nan")))
    diff_s = f"{max_diff:.4f}"
    flag   = "  *** DIVERGE ***" if max_diff > 1.0 else ""
    desc_t = desc[:43]
    print(f"  {zc:<10} {desc_t:<44} {py_s:>10} {r_s:>10} {jl_s:>10} {diff_s:>8}{flag}")

# ---------------------------------------------------------------------------
# 11.  Write CSV results file
# ---------------------------------------------------------------------------
OUT_CSV = SCRIPT_DIR / "Phase5b_Acreage_QA_Results.csv"
with open(OUT_CSV, "w", newline="", encoding="utf-8") as fh:
    w = csv.writer(fh)
    w.writerow(["measure", "language", "acres"])
    for lang in ("Python", "R", "Julia"):
        w.writerow([f"(a) OSM polygon total",       lang, round(osm[lang], 4)])
    w.writerow(["(a) OSM Grand Mean",              "Grand Mean", round(grand_osm, 4)])
    for lang in ("Python", "R", "Julia"):
        w.writerow([f"(b)/(c) Cadastre+Zone total", lang, round(cad_totals[lang], 4)])
    w.writerow(["(b)/(c) Grand Mean",              "Grand Mean", round(grand_cad, 4)])
    w.writerow([])
    w.writerow(["zone_group", "language", "acres", "pct_of_total"])
    for lang in ("Python", "R", "Julia"):
        d   = zone_data[lang]
        tot = d["total"]
        w.writerow(["Preservation+Federal", lang, round(d["pf"],    4), round(d["pf"]    / tot * 100, 2)])
        w.writerow(["Agriculture",          lang, round(d["ag"],    4), round(d["ag"]    / tot * 100, 2)])
        w.writerow(["Other",                lang, round(d["other"], 4), round(d["other"] / tot * 100, 2)])
    w.writerow(["Preservation+Federal", "Grand Mean", round(gm_pf,    4), round(pf_pct,    2)])
    w.writerow(["Agriculture",          "Grand Mean", round(gm_ag,    4), round(ag_pct,    2)])
    w.writerow(["Other",                "Grand Mean", round(gm_other, 4), round(other_pct, 2)])
    w.writerow(["TOTAL",                "Grand Mean", round(grand_cad, 4), 100.0])
    w.writerow([])
    w.writerow(["zone_class", "zoning_description",
                "acres_Python", "acres_R", "acres_Julia", "max_diff_ac"])
    for zc in all_zones:
        desc = ""
        ac   = {}
        for lang in ("Python", "R", "Julia"):
            match = next((r for r in zonings[lang] if r["zone_class"].strip() == zc), None)
            if match:
                ac[lang] = match["acres"]
                desc     = match["zoning_description"]
        vals     = list(ac.values())
        max_diff = round(max(vals) - min(vals), 6) if vals else ""
        w.writerow([zc, desc,
                    round(ac.get("Python", float("nan")), 4),
                    round(ac.get("R",      float("nan")), 4),
                    round(ac.get("Julia",  float("nan")), 4),
                    max_diff])

print(f"\n[OK] Results CSV written → {OUT_CSV.name}")

print("\n" + "=" * 70)
print("  QA complete -- no source files were modified.")
print("=" * 70 + "\n")
