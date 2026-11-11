"""Compare PEPPER's speed with ANISE's (the Rust library of nyx-space).

    pixi run -e anise compare-anise --kernels DIR [--runs 7]
    pixi run -e anise compare-anise --report      # the table again, from the last run

DIR holds the real kernels: naif0012.tls, pck00011.tpc, de421.bsp,
earth_latest_high_prec.bpc, moon_pa_de440_200625.bpc, moon_de440_250416.tf
(the kernels of bench/compare_spicelib.py).

PEPPER's side is test/core/bench_anise.f90 (release build); ANISE's side is
bench/anise (a Rust program on the anise crate, release build with LTO).
PEPPER's side runs every query twice: with the kernels read from their
files as needed (as SPICELIB), and with the binary kernels loaded in memory
(FURNSH's in_memory, as ANISE). Both make the same queries at the same
epochs: 100000 pseudo-random epochs
over 20 years from J2000, and 100000 sequential epochs one minute apart (the
"sequential" rows). Each program runs --runs times; the table has the
fastest time of each operation. The checksums (sums of the results) of the
two sides are compared. The results go to build/compare_anise/results.md
(and .json).

Differences in what the two sides do:

- ANISE does not read text kernels: pck00011.tpc and moon_de440_250416.tf
  are converted to ANISE's PCA and EPA data sets first (timed separately),
  and the converter keeps only bodies with a GM (the program supplies
  DE440's GMs of Mars, its barycenter, the Moon and the Earth-Moon
  barycenter, which do not enter the rotations).
- ANISE takes NAIF IDs: the SPKEZR rows pass names to PEPPER and IDs to
  ANISE (the SPKGEO rows pass IDs to both).
- ANISE 0.10.6 knows ECLIPJ2000 only from its own PCA file: its side applies
  SPICE's fixed obliquity rotation to the J2000 state.
- ANISE 0.10.6's FK converter maps the alias MOON_ME to the Moon's body
  frame; its side asks for MOON_ME_DE440_ME421, which MOON_ME stands for.
"""

import argparse
import json
import os
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "build", "compare_anise")
CARGO_TARGET = os.path.join(ROOT, "build", "anise", "target")
ANISE_EXE = os.path.join(CARGO_TARGET, "release", "bench_anise")

# key: (group, description)
OPERATIONS = {
    "load_set": ("Loading", "Loading the kernel set, per set (PEPPER: FURNSH of the 6 kernels; ANISE: the 3 binary kernels and the converted PCA and EPA)"),
    "anise_convert_text": ("Loading", "Converting the text PCK and the FK to ANISE's formats (ANISE only; once)"),
    "spkezr_moon": ("States, random epochs", "SPKEZR Moon from Earth, J2000, no correction"),
    "spkgeo_moon": ("States, random epochs", "SPKGEO Moon from Earth, by code"),
    "spkezr_mars_lts": ("States, random epochs", "SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S"),
    "spkpos_sun_cns": ("States, random epochs", "SPKPOS Sun from Earth, J2000, CN+S"),
    "spkezr_moon_seq": ("States, sequential epochs", "SPKEZR Moon from Earth, J2000, no correction"),
    "spkgeo_moon_seq": ("States, sequential epochs", "SPKGEO Moon from Earth, by code"),
    "spkezr_mars_lts_seq": ("States, sequential epochs", "SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S"),
    "spkpos_sun_cns_seq": ("States, sequential epochs", "SPKPOS Sun from Earth, J2000, CN+S"),
    "pxform_itrf93": ("Frames, random epochs", "PXFORM J2000 to ITRF93 (binary PCK)"),
    "sxform_itrf93": ("Frames, random epochs", "SXFORM J2000 to ITRF93"),
    "pxform_iau_mars": ("Frames, random epochs", "PXFORM J2000 to IAU_MARS (text PCK)"),
    "pxform_moon_me": ("Frames, random epochs", "PXFORM MOON_ME to J2000 (FK + binary PCK)"),
    "pxform_itrf93_seq": ("Frames, sequential epochs", "PXFORM J2000 to ITRF93 (binary PCK)"),
    "sxform_itrf93_seq": ("Frames, sequential epochs", "SXFORM J2000 to ITRF93"),
    "pxform_iau_mars_seq": ("Frames, sequential epochs", "PXFORM J2000 to IAU_MARS (text PCK)"),
    "pxform_moon_me_seq": ("Frames, sequential epochs", "PXFORM MOON_ME to J2000 (FK + binary PCK)"),
}


def mem_key(key):
    """PEPPER's key of the same operation with the kernels in memory."""
    if key.endswith("_seq"):
        return key[:-4] + "_mem_seq"
    return key + "_mem"


MEM_KEYS = {mem_key(k) for k in OPERATIONS}


def run(cmd, cwd):
    """Run a command; return its standard output (stop on failure)."""
    p = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True)
    if p.returncode != 0:
        sys.exit(f"failed: {' '.join(cmd)}\n{p.stdout}\n{p.stderr}")
    return p.stdout


def parse(out):
    """The key|n|us|checksum lines of a benchmark's output."""
    res = {}
    for line in out.splitlines():
        parts = line.split("|")
        if len(parts) == 4 and (parts[0] in OPERATIONS or parts[0] in MEM_KEYS):
            res[parts[0]] = (float(parts[2]), float(parts[3]))
    return res


def fmt(x):
    """Three significant digits."""
    if x is None:
        return "–"
    if x >= 100:
        return f"{x:,.0f}"
    return f"{x:.3g}"


def table(best):
    """The Markdown table of the fastest times: ANISE, PEPPER reading the
    files as needed, and PEPPER with the binary kernels in memory (FURNSH's
    in_memory)."""
    rows = ["| Operation | ANISE 0.10.6 (µs) | PEPPER (µs) | PEPPER, in memory (µs) | PEPPER in memory: speedup over ANISE | Checksums agree |",
            "|---|--:|--:|--:|--:|:-:|"]
    group = None
    for key, (g, desc) in OPERATIONS.items():
        a, p = best["anise"].get(key), best["pepper"].get(key)
        pm = best["pepper"].get(mem_key(key))
        if a is None and p is None:
            continue
        if g != group:
            rows.append(f"| **{g}** | | | | | |")
            group = g
        ta = a[0] if a else None
        tp = p[0] if p else None
        tm = pm[0] if pm else None
        speed = f"{ta / tm:.2g}×" if ta and tm else "–"
        agree = ""
        if a and p and not key.startswith("load_set"):
            sums = [a[1], p[1]] + ([pm[1]] if pm else [])
            ok = all(abs(x - p[1]) <= 1e-8 * max(abs(x), abs(p[1]), 1.0) for x in sums)
            agree = "yes" if ok else "no (" + " / ".join(f"{x:.15g}" for x in sums) + ")"
        rows.append(f"| {desc} | {fmt(ta)} | {fmt(tp)} | {fmt(tm)} | {speed} | {agree} |")
    return "\n".join(rows)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--kernels", help="directory of the real kernels")
    ap.add_argument("--runs", type=int, default=7, help="runs of each program (default 7)")
    ap.add_argument("--report", action="store_true", help="print the table of the last run")
    args = ap.parse_args()
    os.makedirs(OUT, exist_ok=True)
    jpath = os.path.join(OUT, "results.json")
    if args.report:
        print(table(json.load(open(jpath))))
        return
    if not args.kernels:
        ap.error("--kernels is required")
    kernels = os.path.abspath(args.kernels)

    print("building PEPPER's side (fpm, release) and ANISE's side (cargo, release)", flush=True)
    run(["fpm", "build", "--profile", "release", "--tests"], cwd=ROOT)
    run(["cargo", "build", "--release", "--quiet", "--target-dir", CARGO_TARGET], cwd=os.path.join(ROOT, "bench", "anise"))

    results = {"pepper": [], "anise": []}
    for r in range(args.runs):
        print(f"run {r + 1} of {args.runs}", flush=True)
        results["pepper"].append(parse(run(["fpm", "test", "bench_anise", "--profile", "release", "--", kernels], cwd=ROOT)))
        results["anise"].append(parse(run([ANISE_EXE, kernels], cwd=ROOT)))
    keys = list(OPERATIONS) + sorted(MEM_KEYS)
    best = {side: {k: min((r[k] for r in runs if k in r), key=lambda x: x[0])
                   for k in keys if any(k in r for r in runs)}
            for side, runs in results.items()}
    json.dump(best, open(jpath, "w"), indent=1)
    md = table(best)
    open(os.path.join(OUT, "results.md"), "w").write(md + "\n")
    print(md)


if __name__ == "__main__":
    main()
