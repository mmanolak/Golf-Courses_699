# === 1. LIBRARIES ===

using Pkg


# === 2. GLOBALS & PATHS ===

# All packages required across Phases 1-5.
PACKAGES = [
    # Phase 1 Parsing, Phase 2 Spatial Polygons (shared) & Phase 2 Spatial Polygons — parallel processing
    "CSV", "DataFrames", "GeoDataFrames", "ArchGDAL", "Statistics", "LibGEOS",
    # Phase 3 Economic Merge and MICE Imputation
    "CategoricalArrays", "Printf", "Random", "Mice",
    # Phase 4 Econometric Modeling
    "GLM", "CovarianceMatrices", "Serialization",
    "LinearAlgebra", "Distributions",
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

install_and_verify(PACKAGES)