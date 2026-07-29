# Purpose: Shared run-provenance capture for the Julia master scripts (Roadmap Sec3.3).
#          Each phase script includes this file and calls record_provenance() once,
#          at the end of its run, to append one row to Run_Provenance_Julia.csv.
# Not part of the analysis pipeline itself - instrumentation only.

using Dates
using Pkg

# P0-01 (2026-07-28): BetaML replaces Plots -- BetaML is Mice.jl's actual
# random-forest imputation backend (Phase_3.jl, using BetaML) and was
# missing from this whitelist even after X-10 wired it into Project.toml;
# Plots was never a real dependency (CairoMakie is what Phase_6.jl uses).
const _KEY_PACKAGE_WHITELIST = [
    "DataFrames", "CSV", "Mice", "ArchGDAL", "GeoDataFrames", "CairoMakie", "BetaML",
]

function _git(repo_dir::AbstractString, args...)
    try
        cmd = Cmd(vcat(["git", "-C", repo_dir], collect(String, args)))
        # Issue_Register.md B-8 follow-up (2026-07-28): root cause confirmed on the
        # 2026-07-28 RF cascade - Phase 1/2's `using ArchGDAL, GeoDataFrames` export
        # their own `read` bindings, ambiguating the bare `read` call at `Main` scope.
        # Qualify explicitly rather than relying on whatever `read` resolves to here.
        out = Base.read(cmd, String)
        return strip(out)
    catch e
        @warn "[provenance] git command failed" repo_dir args exception=e
        return nothing
    end
end

function _key_packages()
    parts = String[]
    try
        deps = Pkg.dependencies()
        for (_, pkginfo) in deps
            if pkginfo.name in _KEY_PACKAGE_WHITELIST && pkginfo.version !== nothing
                push!(parts, string(pkginfo.name, "=", pkginfo.version))
            end
        end
    catch
        return nothing
    end
    return isempty(parts) ? nothing : join(parts, ";")
end

function record_provenance(phase::AbstractString, script::AbstractString, script_dir::AbstractString,
                            start_time::Float64;
                            M = missing, maxit = missing, n_workers = missing, seed = missing,
                            key_packages = nothing)
    try
        repo_dir = abspath(script_dir)
        csv_path = abspath(joinpath(script_dir, "..", "Run_Provenance_Julia.csv"))

        git_sha = _git(repo_dir, "rev-parse", "HEAD")
        dirty_out = _git(repo_dir, "status", "--porcelain")
        git_dirty = dirty_out === nothing ? missing : !isempty(dirty_out)

        kp = key_packages === nothing ? _key_packages() : key_packages
        nw = n_workers === missing ? Threads.nthreads() : n_workers

        header = "timestamp_utc,phase,language,script,git_sha,git_dirty,M,maxit,n_workers,seed,wall_time_sec,host,os,lang_version,key_packages\n"
        esc(x) = x === nothing || x === missing ? "" : "\"" * replace(string(x), "\"" => "\"\"") * "\""

        row = join([
            esc(Dates.format(Dates.now(Dates.UTC), dateformat"yyyy-mm-ddTHH:MM:SS") * "Z"),
            esc(phase), esc("Julia"), esc(script), esc(git_sha), esc(git_dirty),
            esc(M), esc(maxit), esc(nw), esc(seed),
            esc(time() - start_time),
            esc(gethostname()), esc(string(Sys.KERNEL, " ", Sys.MACHINE)),
            esc(string("Julia ", VERSION)), esc(kp),
        ], ",") * "\n"

        write_header = !isfile(csv_path)
        open(csv_path, "a") do io
            write_header && write(io, header)
            write(io, row)
        end
    catch e
        @warn "[provenance] recording failed, continuing run" exception=e
    end
    return nothing
end
