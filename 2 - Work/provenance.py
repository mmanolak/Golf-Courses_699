# Purpose: Shared run-provenance capture for the Python master scripts (Roadmap Sec3.3).
#          Each phase script imports this module and calls record_provenance() once,
#          at the end of its run, to append one row to Run_Provenance_Python.csv.
# Not part of the analysis pipeline itself - instrumentation only.

import csv
import os
import platform
import subprocess
import sys
import time
from datetime import datetime, timezone
from importlib import metadata as _md

_KEY_PACKAGE_WHITELIST = [
    "pandas", "numpy", "geopandas", "miceforest", "scikit-learn",
    "statsmodels", "pyosmium", "shapely", "matplotlib",
]


def _git(repo_dir, *args):
    try:
        out = subprocess.run(
            ["git", "-C", str(repo_dir), *args],
            capture_output=True, text=True, timeout=10,
        )
        if out.returncode != 0:
            return None
        return out.stdout.strip()
    except Exception:
        return None


def _key_packages():
    parts = []
    for name in _KEY_PACKAGE_WHITELIST:
        try:
            parts.append(f"{name}={_md.version(name)}")
        except Exception:
            pass
    return ";".join(parts) if parts else None


def record_provenance(phase, script, script_dir, start_time,
                       M=None, maxit=None, n_workers=None, seed=None,
                       key_packages=None):
    try:
        repo_dir = os.path.abspath(script_dir)
        csv_path = os.path.abspath(os.path.join(script_dir, "..", "Run_Provenance_Python.csv"))

        git_sha = _git(repo_dir, "rev-parse", "HEAD")
        dirty_out = _git(repo_dir, "status", "--porcelain")
        git_dirty = (len(dirty_out) > 0) if dirty_out is not None else None

        row = {
            "timestamp_utc": datetime.now(timezone.utc).isoformat(),
            "phase": phase,
            "language": "Python",
            "script": script,
            "git_sha": git_sha,
            "git_dirty": git_dirty,
            "M": M,
            "maxit": maxit,
            "n_workers": n_workers,
            "seed": seed,
            "wall_time_sec": time.time() - start_time,
            "host": platform.node(),
            "os": f"{platform.system()} {platform.release()}",
            "lang_version": sys.version.split()[0],
            "key_packages": key_packages if key_packages is not None else _key_packages(),
        }

        file_exists = os.path.exists(csv_path)
        with open(csv_path, "a", newline="", encoding="utf-8") as f:
            writer = csv.DictWriter(f, fieldnames=list(row.keys()))
            if not file_exists:
                writer.writeheader()
            writer.writerow(row)
    except Exception as e:
        print(f"[provenance] recording failed, continuing run: {e}", file=sys.stderr)
