"""Compare PEPPER's speed with SPICELIB N0067's.

    pixi run compare-spicelib --kernels DIR [--runs 3]
    pixi run compare-spicelib --report      # the table again, from the last run

DIR holds the real kernels: naif0012.tls, pck00011.tpc, de421.bsp,
earth_latest_high_prec.bpc, moon_pa_de440_200625.bpc, moon_de440_250416.tf.

SPICELIB is compiled from NAIF's N0067 sources (the toolkit archive in
upstream/archives) with the same gfortran, twice:

- "naif": the flags of NAIF's build script, src/spicelib/mkprodct.csh
  (-std=legacy -Wno-character-truncation and no optimization, except -O2
  for MOVED and MOVEI; its -m64 is left out, as arm64 has no 32-bit mode):
  the library of the toolkit a user downloads;
- "o3": -O3 -funroll-loops, the optimization of PEPPER's release build.

test/core/bench_compare.f90 (PEPPER, release build) runs first and writes
the synthetic kernels; bench/bench_spicelib.f90 does the same operations
with each SPICELIB. Each program runs --runs times; the table has the
fastest time of each operation. The checksums (sums of the results) of
PEPPER and the "o3" SPICELIB must agree. The results go to
build/compare/results.md (and .json).
"""

import argparse
import glob
import json
import os
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "build", "compare")
ARCHIVE = os.path.join(ROOT, "upstream", "archives", "N0067", "MacIntel_OSX_gfortran_64bit_toolkit.tar.Z")

# key: (group, description)
OPERATIONS = {
    "load_set": ("Loading", "FURNSH of 6 real kernels (LSK, PCK, DE421, Earth and Moon binary PCKs, Moon FK), per set"),
    "spk_load": ("Loading", "FURNSH of 1000 SPKs, per file"),
    "spk_unload": ("Loading", "UNLOAD of 1000 SPKs, per file"),
    "pool_load": ("Loading", "FURNSH of a text kernel, per variable (20000 variables)"),
    "names_load": ("Loading", "FURNSH of 10000 body names and the first lookup, per name"),
    "frames_load": ("Loading", "FURNSH of 1000 TK frames, per frame"),
    "spkezr_moon": ("States", "SPKEZR Moon from Earth, J2000, no correction"),
    "spkezr_mars_lts": ("States", "SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S"),
    "spkpos_sun_cns": ("States", "SPKPOS Sun from Earth, J2000, CN+S"),
    "spkgeo_moon": ("States", "SPKGEO Moon from Earth, by code"),
    "spk_random_body": ("States", "SPKGEO of a random body among 1000 loaded SPKs"),
    "pxform_itrf93": ("Frames", "PXFORM J2000 to ITRF93 (binary PCK)"),
    "pxform_iau_mars": ("Frames", "PXFORM J2000 to IAU_MARS (text PCK)"),
    "pxform_moon_me": ("Frames", "PXFORM MOON_ME to J2000 (FK + binary PCK)"),
    "sxform_itrf93": ("Frames", "SXFORM J2000 to ITRF93"),
    "frames_pxform": ("Frames", "PXFORM of a random frame among 1000 TK frames"),
    "str2et": ("Time", "STR2ET of an ISO UTC string"),
    "et2utc": ("Time", "ET2UTC, calendar format"),
    "unitim_tdb_tai": ("Time", "UNITIM TDB to TAI"),
    "bodn2c": ("Names and constants", "BODN2C of a built-in name"),
    "names_bodn2c": ("Names and constants", "BODN2C of a random name among 10000 kernel names"),
    "bodvrd_radii": ("Names and constants", "BODVRD Earth radii"),
    "pool_gdpool": ("Names and constants", "GDPOOL of a random variable among 20000"),
}


def run(cmd, **kw):
    """Run a command, return its stdout; stop on failure."""
    r = subprocess.run(cmd, capture_output=True, text=True, **kw)
    if r.returncode != 0:
        sys.exit(f"failed: {' '.join(cmd)}\n{r.stdout}\n{r.stderr}")
    return r.stdout


def spicelib_sources():
    """NAIF's SPICELIB sources, extracted from the toolkit archive."""
    src = os.path.join(OUT, "naif", "toolkit", "src", "spicelib")
    if not os.path.isdir(src):
        # Python's tarfile cannot read compress (.Z) archives; tar can
        dest = os.path.join(OUT, "naif")
        os.makedirs(dest, exist_ok=True)
        run(["tar", "-xf", ARCHIVE, "-C", dest, "toolkit/src/spicelib"])
    return src


def build_spicelib(name, flags, flags_move):
    """Compile SPICELIB with `flags` (`flags_move` for MOVED and MOVEI)
    into build/compare/spicelib_<name>/libspicelib.a."""
    lib = os.path.join(OUT, f"spicelib_{name}", "libspicelib.a")
    if os.path.exists(lib):
        return lib
    src = spicelib_sources()
    obj = os.path.dirname(lib)
    os.makedirs(obj, exist_ok=True)
    files = sorted(glob.glob(os.path.join(src, "*.f")))
    print(f"compiling SPICELIB ({name}): {len(files)} files", flush=True)
    procs = []
    for i in range(0, len(files), 50):
        chunk = files[i:i + 50]
        regular = [f for f in chunk if os.path.basename(f) not in ("moved.f", "movei.f")]
        move = [f for f in chunk if os.path.basename(f) in ("moved.f", "movei.f")]
        for group, fl in ((regular, flags), (move, flags_move)):
            if group:
                procs.append(subprocess.Popen(["gfortran", "-c", *fl, "-w", "-I", src, *group], cwd=obj))
        if len(procs) >= os.cpu_count():
            for p in procs:
                if p.wait() != 0:
                    sys.exit("SPICELIB compilation failed")
            procs = []
    for p in procs:
        if p.wait() != 0:
            sys.exit("SPICELIB compilation failed")
    run(["ar", "crs", lib, *sorted(glob.glob(os.path.join(obj, "*.o")))])
    return lib


def parse(text):
    """{key: (n, us, checksum)} from a benchmark's output."""
    res = {}
    for line in text.splitlines():
        parts = line.split("|")
        if len(parts) == 4 and parts[0] in OPERATIONS:
            res[parts[0]] = (int(parts[1]), float(parts[2]), float(parts[3]))
        elif line.startswith("FAILED"):
            sys.exit(line)
    return res


def best(runs):
    """The fastest time of each operation over several runs."""
    out = {}
    for r in runs:
        for k, (n, us, c) in r.items():
            if k not in out or us < out[k][1]:
                out[k] = (n, us, c)
    return out


def fmt_us(us):
    return f"{us:.3g}" if us < 100 else f"{us:,.0f}"


def fmt_speedup(x):
    return f"{x:.1f}×" if x < 100 else f"{x:,.0f}×"

def report(res):
    """Check the checksums and write the table (results.md, results.json)."""
    # the same results: PEPPER and the -O3 SPICELIB (the same compiler optimizations)
    for key, (n, us, c) in res["pepper"].items():
        c3 = res["o3"][key][2]
        if abs(c - c3) > 1e-9 * max(1.0, abs(c3)):
            print(f"WARNING: {key}: checksum {c!r} (PEPPER) against {c3!r} (SPICELIB -O3)")

    lines = ["| Operation | SPICELIB, NAIF's flags (µs) | SPICELIB, -O3 (µs) | PEPPER (µs) "
             "| Speedup over NAIF's build | Speedup over -O3 |",
             "|---|--:|--:|--:|--:|--:|"]
    group = None
    for key, (grp, desc) in OPERATIONS.items():
        if key not in res["pepper"]:
            continue
        if grp != group:
            lines.append(f"| **{grp}** | | | | | |")
            group = grp
        p, o, n = res["pepper"][key][1], res["o3"][key][1], res["naif"][key][1]
        lines.append(f"| {desc} | {fmt_us(n)} | {fmt_us(o)} | {fmt_us(p)} | {fmt_speedup(n / p)} | {fmt_speedup(o / p)} |")
    table = "\n".join(lines)
    with open(os.path.join(OUT, "results.md"), "w") as f:
        f.write(table + "\n")
    with open(os.path.join(OUT, "results.json"), "w") as f:
        json.dump(res, f, indent=1)
    print(table)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--kernels", help="directory of the real kernels")
    ap.add_argument("--runs", type=int, default=3, help="runs of each program (the fastest time counts)")
    ap.add_argument("--report", action="store_true",
                    help="only write the table again from build/compare/results.json")
    args = ap.parse_args()
    if args.report:
        with open(os.path.join(OUT, "results.json")) as f:
            report({k: {op: tuple(v) for op, v in r.items()} for k, r in json.load(f).items()})
        return
    if not args.kernels:
        ap.error("--kernels is required")
    kernels = os.path.abspath(args.kernels)
    work = os.path.join(OUT, "work")
    os.makedirs(work, exist_ok=True)

    libs = {
        "naif": build_spicelib("naif", ["-std=legacy", "-Wno-character-truncation"],
                               ["-std=legacy", "-Wno-character-truncation", "-O2"]),
        "o3": build_spicelib("o3", ["-O3", "-funroll-loops", "-std=legacy", "-fallow-argument-mismatch"],
                             ["-O3", "-funroll-loops", "-std=legacy", "-fallow-argument-mismatch"]),
    }
    exes = {}
    for name, lib in libs.items():
        exes[name] = os.path.join(OUT, f"bench_spicelib_{name}")
        run(["gfortran", "-O3", "-funroll-loops", os.path.join(ROOT, "bench", "bench_spicelib.f90"),
             lib, "-o", exes[name]], cwd=OUT)

    results = {"pepper": [], "o3": [], "naif": []}
    for k in range(args.runs):
        print(f"run {k + 1} of {args.runs}", flush=True)
        out = run(["fpm", "test", "bench_compare", "--profile", "release", "--", kernels, work], cwd=ROOT)
        results["pepper"].append(parse(out))
        for name, exe in exes.items():
            results[name].append(parse(run([exe, kernels, work], cwd=OUT)))
    res = {name: best(r) for name, r in results.items()}

    report(res)


if __name__ == "__main__":
    main()
