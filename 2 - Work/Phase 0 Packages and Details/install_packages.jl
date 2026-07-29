# === 1. LIBRARIES ===

using Pkg


# === 2. GLOBALS & PATHS ===

# P0-01 (2026-07-28): the project root, one directory up from this script --
# same convention every Phase_1..6.jl master script uses to find Project.toml.
const PROJ_DIR      = normpath(joinpath(@__DIR__, ".."))
const MANIFEST_TOML = joinpath(PROJ_DIR, "Manifest.toml")

# Fallback package list -- used only when Manifest.toml is absent (see
# EXECUTION). Dropped the standard-library entries (Statistics/Printf/Random/
# Serialization/LinearAlgebra ship with Julia itself and were never really
# "installable"); added XLSX (Phase_1.jl's original X-08 crash) and BetaML
# (Mice.jl's real random-forest backend, Phase_3.jl, using BetaML -- added to
# Project.toml under X-10 but never added here).
PACKAGES = [
    # Phase 1 Parsing, Phase 2 Spatial Polygons (shared) & Phase 2 Spatial Polygons — parallel processing
    "CSV", "DataFrames", "GeoDataFrames", "ArchGDAL", "LibGEOS", "XLSX",
    # Phase 3 Economic Merge and MICE Imputation
    "CategoricalArrays", "Mice", "BetaML",
    # Phase 4 Econometric Modeling
    "GLM", "CovarianceMatrices", "Distributions",
    # Phase 6 Visualization
    "CairoMakie", "Latexify", "GeoInterfaceMakie", "StatsBase", "ZipFile", "Colors", "Plots"
]


# === 3. FUNCTIONS ===

"""
    find_missing(pkg_list)

Return the subset of pkg_list that are not currently installed.
Uses Pkg.project() to check against the active environment's dependencies,
with a fallback check against the full depot for packages installed globally.

# Arguments
- `pkg_list::Vector{String}`: package names to check.

# Returns
- `Vector{String}`: names of packages not found in the environment.
"""
function find_missing(pkg_list::Vector{String})::Vector{String}
    # Collect all installed package names from the full depot manifest
    # so globally installed packages are recognised even if not in the
    # local Project.toml.
    # Pkg.dependencies() returns Dict{UUID, PackageInfo} — iterate values()
    # directly to avoid UUID destructuring which errors in Julia 1.12
    installed_names = Set(v.name for v in values(Pkg.dependencies()))
    return [pkg for pkg in pkg_list if pkg ∉ installed_names]
end


"""
    report_status(pkg_list)

Print the installation status of every package in pkg_list.

# Arguments
- `pkg_list::Vector{String}`: package names to check.
"""
function report_status(pkg_list::Vector{String})
    println("Checking installed Julia packages...")
    installed = Set(v.name for v in values(Pkg.dependencies()))
    for pkg in pkg_list
        if pkg ∈ installed
            println("  $pkg is already installed")
        else
            println("  $pkg - MISSING")
        end
    end
end


"""
    install_and_verify(pkg_list)

Report current status, install any missing packages, then verify.

# Arguments
- `pkg_list::Vector{String}`: package names to check and install.
"""
function install_and_verify(pkg_list::Vector{String})
    report_status(pkg_list)

    missing_pkgs = find_missing(pkg_list)

    if isempty(missing_pkgs)
        println("\nNo missing packages :D")
        return
    end

    println("\nFound $(length(missing_pkgs)) missing package(s). Installing...")

    for pkg in missing_pkgs
        try
            Pkg.add(pkg)
            println("  Successfully installed: $pkg")
        catch e
            println("  Failed to install: $pkg ($(sprint(showerror, e)))")
        end
    end

    # Final verification pass after installation attempts
    println("\nVerifying installation...")
    still_missing = find_missing(pkg_list)

    if !isempty(still_missing)
        println("\nThe following packages failed to install or load:")
        for pkg in still_missing
            println("  - $pkg")
        end
    else
        println("\nAll packages are now properly installed and ready to use!")
    end
end


# === 4. EXECUTION ===

# P0-01 (2026-07-28): activate the project environment first -- this script
# previously never called Pkg.activate() at all, so it checked and installed
# against whichever environment happened to be ambient (typically the global
# default), never the project-local one every Phase_1..6.jl master script
# activates via this same call. That mismatch, not an incomplete PACKAGES
# list, is X-08's real root cause. Restore from Manifest.toml when present,
# rather than resolving whatever's currently newest in the General registry
# for any package this script decides is missing; falls back to the
# pre-existing ad hoc install-what's-missing behaviour otherwise.
Pkg.activate(PROJ_DIR; io = devnull)

if isfile(MANIFEST_TOML)
    println("Found Manifest.toml at $MANIFEST_TOML -- restoring pinned environment...")
    Pkg.instantiate()
    println("\nPkg.instantiate() complete -- environment matches Manifest.toml.")
else
    println("No Manifest.toml found -- falling back to ad hoc installation of the package list below.")
    println("(This does not pin versions -- run Pkg.resolve() then commit Manifest.toml once installed to fix that.)")
    install_and_verify(PACKAGES)
end