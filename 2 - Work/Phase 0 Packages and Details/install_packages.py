# === 1. LIBRARIES ===

import subprocess
import sys
from pathlib import Path


# === 2. GLOBALS & PATHS ===

SCRIPT_DIR = Path(__file__).resolve().parent
PROJ_DIR = SCRIPT_DIR.parent
REQUIREMENTS_TXT = PROJ_DIR / "requirements.txt"

# Fallback package list -- used only when requirements.txt is absent (see
# EXECUTION). P0-01 (2026-07-28): dropped the standard-library entries
# (pathlib/time/re/warnings/multiprocessing are never actually installable --
# `pip install pathlib` in particular pulls a real, long-deprecated PyPI
# backport that can conflict with the Python 3.4+ built-in module of the same
# name) and added openpyxl, which pandas needs to read Phase 1's one .xlsx
# source file (Phase_1.py:183) but which this list never listed.
# Format: (install_name, import_name)
# These differ when the PyPI package name does not match the importable module name.
PACKAGES = [
    # Phase 1 Parsing
    ("pandas",          "pandas"),
    ("geopandas",       "geopandas"),
    ("shapely",         "shapely"),
    ("openpyxl",        "openpyxl"),
    # Phase 2 Spatial Polygons and True Acreage
    ("osmium",          "osmium"),
    # Note: pygeos is deprecated — its functionality is now in shapely >= 2.0.
    # If on older Shapely, run: pip install pygeos, or upgrade: pip install "shapely>=2.0"
    # Phase 3 Economic Merge and MICE Imputation
    ("miceforest",      "miceforest"),
    ("numpy",           "numpy"),
    # Phase 4 Econometric Modeling
    ("scipy",           "scipy"),
    ("statsmodels",     "statsmodels"),
    # Phase 5 Hawaii Micro-Case Study
    ("pygris",          "pygris"),
    # Phase 6 Visualization -- not required by any master script (no Python
    # arm; see P0-01), kept installed but flagged as unused, not removed here.
    ("matplotlib",      "matplotlib"),
    ("seaborn",         "seaborn"),
]


# === 3. FUNCTIONS ===

def find_missing(pkg_list):
    """
    Check which packages from a list cannot be imported.

    Parameters
    ----------
    pkg_list : list of tuple
        Each tuple is (install_name, import_name).

    Returns
    -------
    list of tuple
        Subset of pkg_list whose import_name cannot be imported.
    """
    missing = []
    for install_name, import_name in pkg_list:
        try:
            __import__(import_name)
        except ImportError:
            missing.append((install_name, import_name))
    return missing


def report_status(pkg_list):
    """
    Print the installation status of every package in pkg_list.

    Parameters
    ----------
    pkg_list : list of tuple
        Each tuple is (install_name, import_name).
    """
    print("Checking installed Python packages...")
    for install_name, import_name in pkg_list:
        try:
            __import__(import_name)
            print(f"  {install_name} is already installed")
        except ImportError:
            print(f"  {install_name} - MISSING")


def install_and_verify(pkg_list):
    """
    Report current status, install any missing packages, then verify.

    Parameters
    ----------
    pkg_list : list of tuple
        Each tuple is (install_name, import_name).
    """
    report_status(pkg_list)

    missing = find_missing(pkg_list)

    if not missing:
        print("\nNo missing packages owo")
        return

    print(f"\nFound {len(missing)} missing package(s). Installing...")

    for install_name, import_name in missing:
        try:
            subprocess.check_call(
                [sys.executable, "-m", "pip", "install", install_name],
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL,
            )
            print(f"  Successfully installed: {install_name}")
        except subprocess.CalledProcessError as e:
            print(f"  Failed to install: {install_name} ({e})")

    # Final verification pass after installation attempts
    print("\nVerifying installation...")
    still_missing = find_missing(pkg_list)

    if still_missing:
        names = [n for n, _ in still_missing]
        print(f"\nThe following packages failed to install or load: {names}")
    else:
        print("\nAll packages are now properly installed and ready to use!")


# === 4. EXECUTION ===

# P0-01 (2026-07-28): restore the pinned environment from requirements.txt
# when one exists, rather than resolving whatever's currently newest on PyPI
# for any package this script decides is missing. Falls back to the
# pre-existing ad hoc install-what's-missing behaviour when requirements.txt
# is absent.
if __name__ == "__main__":
    if REQUIREMENTS_TXT.exists():
        print(f"Found requirements.txt at {REQUIREMENTS_TXT} -- restoring pinned environment...")
        subprocess.check_call([sys.executable, "-m", "pip", "install", "-r", str(REQUIREMENTS_TXT)])
        print("\npip install -r requirements.txt complete -- environment matches the pinned versions.")
    else:
        print("No requirements.txt found -- falling back to ad hoc installation of the package list below.")
        print("(This does not pin versions -- run `pip freeze > requirements.txt` once installed to fix that.)")
        install_and_verify(PACKAGES)