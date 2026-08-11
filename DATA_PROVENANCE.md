# Data & Environment Provenance

**Purpose.** `2 - Work/renv.lock` pins R package versions but not the system
libraries those packages compile against. A replicator starting from a bare
OS install hits the same native build chain this project needed — this file
records that chain, plus language versions and the frozen input data, so the
pipeline can be reproduced from scratch on a machine that has none of it yet.

**Canonical machine.** As of 2026-08-10, the canonical development machine
for this project is a Fedora Linux workstation (hostname `strix`). Prior
development used Windows; several packages in the R dependency chain
(`Rcpp`, `s2`, `units`, `sf`) would not compile there. All figures reported
in the thesis originate from the R implementation running on this machine
going forward.

---

## System

| Component | Version |
|---|---|
| OS | Fedora Linux 43 (Server Edition) |
| CPU / RAM | 32 cores / 124 GiB |
| R | 4.5.3 |
| Python | 3.14.6 |
| Julia | 1.12.6 |
| GDAL | 3.11.5 |
| GEOS | 3.14.1 |
| PROJ | 9.6.2 |
| `sf_use_s2()` | `TRUE` |

## System (native) dependencies

R's geospatial stack (`sf`, and transitively `s2`, `units`, `terra`) and
several tidyverse/rendering packages build native extensions against system
libraries. On Fedora 43, `dnf install` the following before `renv::restore()`:

```
gcc-c++ make cmake
gdal-devel geos-devel proj-devel udunits2-devel abseil-cpp-devel
libuv-devel libgit2-devel sqlite-devel libcurl-devel openssl-devel
libxml2-devel fontconfig-devel harfbuzz-devel fribidi-devel
libtiff-devel libjpeg-turbo-devel libpng-devel poppler-cpp-devel
ImageMagick-c++-devel librsvg2-devel
```

(`libjpeg-devel` is not a package name on Fedora — the equivalent is
`libjpeg-turbo-devel`.)

Versions installed on the canonical machine at time of writing:

| Package | Version |
|---|---|
| gcc-c++ | 15.3.1-1.fc43 |
| make | 4.4.1-11.fc43 |
| cmake | 3.31.11-1.fc43 |
| gdal-devel | 3.11.5-1.fc43 |
| geos-devel | 3.14.1-1.fc43 |
| proj-devel | 9.6.2-2.fc43 |
| udunits2-devel | 2.2.28-12.fc43 |
| abseil-cpp-devel | 20250814.2-2.fc43 |
| libuv-devel | 1.51.0-2.fc43 |
| libgit2-devel | 1.9.6-1.fc43 |
| sqlite-devel | 3.50.2-2.fc43 |
| libcurl-devel | 8.15.0-8.fc43 |
| openssl-devel | 3.5.7-2.fc43 |
| libxml2-devel | 2.12.10-5.fc43 |
| fontconfig-devel | 2.17.0-3.fc43 |
| harfbuzz-devel | 11.5.1-2.fc43 |
| fribidi-devel | 1.0.16-3.fc43 |
| libtiff-devel | 4.7.2-1.fc43 |
| libjpeg-turbo-devel | 3.1.3-1.fc43 |
| libpng-devel | 1.6.58-1.fc43 |
| poppler-cpp-devel | 25.07.0-5.fc43 |
| ImageMagick-c++-devel | 7.1.2.27-1.fc43 |
| librsvg2-devel | 2.61.4-1.fc43 |

`renv::restore()` against `2 - Work/renv.lock` installs 195 R packages
against this toolchain; log of a clean run is kept at `~/logs/renv_restore.log`
(outside the repo — local machine only).

**Known pitfall:** do not run `renv::snapshot(type = "implicit")` on this
project — it silently drops dispatch-loaded packages (`ranger`, `mice`'s real
random-forest backend, plus `VIM`/`ggmice`/`patchwork`) that no static
analysis of the scripts can see. See `Issue_Register.md`, P0-01 addendum.

## Language package managers

- **R** — `2 - Work/renv.lock` (`renv::restore()`)
- **Python** — `requirements.txt` (`pip install -r requirements.txt`)
- **Julia** — `2 - Work/Project.toml` / `Manifest.toml` (`Pkg.instantiate()`)

## Frozen input: OpenStreetMap extract

- **File:** `us-260413.osm.pbf`
- **Source:** Geofabrik North America / United States extract —
  https://download.geofabrik.de/north-america.html (also see
  `2 - Work/00 - Data Sources/Data Sources - Via HTML/Download OpenStreetMap for United States of America _ Geofabrik Download Server.url`)
- **SHA256:** `b9b4b31554b63c4d30ee2daf17d537dc983202212ab0e0592364c7da7a502bb4`

This hash should match across every machine that runs Phase 2 — verify with
`sha256sum` before trusting a Phase 2 re-run against a copy of this file
obtained any other way.
