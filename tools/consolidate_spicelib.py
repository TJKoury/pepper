#!/usr/bin/env python3
"""
Consolidate the NAIF SPICELIB (Fortran) sources from the per-platform
toolkit archives into a single codebase in ``src/spicelib``.

Steps:
  1. Download the toolkit archive for each platform/compiler (cached in
     ``upstream/archives``).
  2. Extract only ``toolkit/src/spicelib`` from each archive.
  3. Normalize every file: ``.for`` -> ``.f`` and CRLF -> LF.
  4. Files identical on all platforms are written once as ``<name>.f``.
     Files that differ between platforms are merged line-by-line into a
     single preprocessed source ``<name>.F``: each differing block is
     wrapped in ``#if`` / ``#elif`` / ``#else`` / ``#endif`` directives
     on the platform macros below.
  5. Verify, for every platform, that preprocessing the merged sources
     with that platform's macros reproduces its original files exactly.

Platform macros (set in fpm.toml via the ``platform`` feature):

    SPICE_OS_MACOS, SPICE_OS_LINUX, SPICE_OS_WINDOWS, SPICE_OS_CYGWIN
    SPICE_COMPILER_GFORTRAN, SPICE_COMPILER_INTEL

The ``#else`` branch of every block is always the code from the FALLBACK
platform (NAIF's Cygwin gfortran build), so combinations NAIF does not ship
(e.g. native Windows + gfortran) get a consistent, gfortran-compatible set.

The NAIF build scripts (mkprodct.csh / mkprodct.bat) are not copied.

Usage:
    python tools/consolidate_spicelib.py [--version N0067] [--force-download]

Requires ``tar`` on the PATH (to read the ``.tar.Z`` archives; both GNU tar
and bsdtar, including Windows' tar.exe, handle them).
"""

import argparse
import difflib
import re
import shutil
import subprocess
import sys
import tempfile
import urllib.request
import zipfile
from pathlib import Path

BASE_URL = "https://naif.jpl.nasa.gov/pub/naif/toolkit/FORTRAN"

# NAIF platform directory -> (archive name, OS macro, compiler macro).
# Only 64-bit x64 builds of gfortran and Intel Fortran are considered.
# (NAIF does not provide a Mac arm64 Fortran toolkit.)
PLATFORMS = {
    "MacIntel_OSX_gfortran_64bit": ("toolkit.tar.Z", "MACOS", "GFORTRAN"),
    "MacIntel_OSX_IFORT_64bit": ("toolkit.tar.Z", "MACOS", "INTEL"),
    "PC_Linux_gfortran_64bit": ("toolkit.tar.Z", "LINUX", "GFORTRAN"),
    "PC_Linux_IFORT_64bit": ("toolkit.tar.Z", "LINUX", "INTEL"),
    "PC_Cygwin_gfortran_64bit": ("toolkit.tar.gz", "CYGWIN", "GFORTRAN"),
    "PC_Windows_IFORT_64bit": ("toolkit.zip", "WINDOWS", "INTEL"),
}
FALLBACK = "PC_Cygwin_gfortran_64bit"

MEMBER_DIR = "toolkit/src/spicelib"
SKIP_FILES = {"mkprodct.csh", "mkprodct.bat"}

# Checked at the top of zzplatfm.F (compiled on every platform) so a build
# without the platform macros fails loudly instead of silently using the
# fallback code.
GUARD = """\
#if !defined(SPICE_OS_MACOS) && !defined(SPICE_OS_LINUX) && \\
    !defined(SPICE_OS_WINDOWS) && !defined(SPICE_OS_CYGWIN)
#error "No SPICE_OS_* macro defined (see the platform feature in fpm.toml)"
#endif
#if !defined(SPICE_COMPILER_GFORTRAN) && !defined(SPICE_COMPILER_INTEL)
#error "No SPICE_COMPILER_* macro defined (see the platform feature in fpm.toml)"
#endif
"""
GUARD_FILE = "zzplatfm.f"

ROOT = Path(__file__).resolve().parent.parent
ARCHIVE_DIR = ROOT / "upstream" / "archives"
OUT_DIR = ROOT / "src" / "spicelib"


# --------------------------------------------------------------------------
# Download / extract / normalize
# --------------------------------------------------------------------------

def download(platform: str, archive: str, version: str, force: bool) -> Path:
    dest = ARCHIVE_DIR / version / f"{platform}_{archive}"
    if dest.exists() and not force:
        print(f"  cached   {dest.relative_to(ROOT)}")
        return dest
    url = f"{BASE_URL}/{platform}/packages/{archive}"
    print(f"  download {url}")
    dest.parent.mkdir(parents=True, exist_ok=True)
    tmp = dest.with_suffix(dest.suffix + ".part")
    with urllib.request.urlopen(url) as r, open(tmp, "wb") as f:
        shutil.copyfileobj(r, f)
    tmp.replace(dest)
    return dest


def extract(archive: Path, workdir: Path) -> Path:
    """Extract only the spicelib source directory; return its path."""
    if archive.suffix == ".zip":
        with zipfile.ZipFile(archive) as z:
            members = [m for m in z.namelist() if m.startswith(MEMBER_DIR + "/")]
            z.extractall(workdir, members)
    else:
        subprocess.run(["tar", "-xf", str(archive), "-C", str(workdir), MEMBER_DIR],
                       check=True)
    return workdir / MEMBER_DIR


def normalize_name(name: str) -> str:
    return name[:-4] + ".f" if name.lower().endswith(".for") else name


def normalize_text(data: bytes) -> bytes:
    data = data.replace(b"\r\n", b"\n")
    if b"\r" in data:
        raise ValueError("stray carriage return found")
    return data


def load_platform(srcdir: Path) -> dict:
    files = {}
    for p in sorted(srcdir.iterdir()):
        if not p.is_file() or p.name in SKIP_FILES:
            continue
        name = normalize_name(p.name)
        if name in files:
            raise RuntimeError(f"name collision after normalization: {p}")
        try:
            files[name] = normalize_text(p.read_bytes())
        except ValueError as e:
            raise RuntimeError(f"{p}: {e}") from None
    return files


# --------------------------------------------------------------------------
# Preprocessor conditions
# --------------------------------------------------------------------------

def os_macro(p):
    return f"SPICE_OS_{PLATFORMS[p][1]}"


def cc_macro(p):
    return f"SPICE_COMPILER_{PLATFORMS[p][2]}"


def condition(group: set, universe: list) -> str:
    """Simplest condition on the platform macros that is true for exactly
    the platforms in ``group`` (out of ``universe``)."""
    def matches(pred):
        return {p for p in universe if pred(p)} == group

    for cc in sorted({cc_macro(p) for p in universe}):
        if matches(lambda p: cc_macro(p) == cc):
            return f"defined({cc})"
    if len(group) == 1:
        (p,) = group
        return f"defined({os_macro(p)}) && defined({cc_macro(p)})"
    for os_ in sorted({os_macro(p) for p in universe}):
        if matches(lambda p: os_macro(p) == os_):
            return f"defined({os_})"
    return " || ".join(f"(defined({os_macro(p)}) && defined({cc_macro(p)}))"
                       for p in universe if p in group)


# --------------------------------------------------------------------------
# N-way line merge
# --------------------------------------------------------------------------

def is_blank(line: str) -> bool:
    return not line.strip()


def units(lines: list):
    """Split lines into (blank-lines-before, non-blank line) units, plus the
    trailing blank lines."""
    out, blanks = [], []
    for line in lines:
        if is_blank(line):
            blanks.append(line)
        else:
            out.append((blanks, line))
            blanks = []
    return out, blanks


def blank_like_base(base: list, lines: list) -> list:
    """Return ``lines`` re-laid-out with the blank lines of ``base`` wherever
    the non-blank lines can be paired one-to-one with ``base``. Blank lines
    are insignificant in fixed-form Fortran; this keeps differing blank-line
    layouts from producing noisy #if blocks."""
    bu, btail = units(base)
    vu, _ = units(lines)
    out = []
    sm = difflib.SequenceMatcher(None, [u[1] for u in bu], [u[1] for u in vu],
                                 autojunk=False)
    for tag, i1, i2, j1, j2 in sm.get_opcodes():
        if tag == "equal" or (tag == "replace" and i2 - i1 == j2 - j1):
            for (blanks, _), (_, line) in zip(bu[i1:i2], vu[j1:j2]):
                out.extend(blanks)
                out.append(line)
        else:
            for blanks, line in vu[j1:j2]:
                out.extend(blanks)
                out.append(line)
    out.extend(btail)
    return out


def nonblank(text: bytes) -> list:
    return [l for l in text.splitlines() if l.strip()]


def merge_variants(texts: dict) -> str:
    """Merge per-platform versions of one file into a single source with
    #if blocks. ``texts`` maps platform -> file contents (str)."""
    platforms = list(texts)
    base = texts[FALLBACK].splitlines(keepends=True)
    lines = {p: blank_like_base(base, texts[p].splitlines(keepends=True))
             for p in platforms}

    # For each platform: base-gap -> platform-gap maps (only at gaps that lie
    # in/at the edges of equal blocks) and the changed base ranges. A gap
    # where the platform inserts lines maps to two positions: ``left`` is
    # before the insertion (used for region starts), ``right`` after it
    # (used for region ends).
    left, right, changed = {}, {}, []
    for p in platforms:
        lm, rm = {0: 0}, {}
        sm = difflib.SequenceMatcher(None, base, lines[p], autojunk=False)
        for tag, i1, i2, j1, j2 in sm.get_opcodes():
            if tag == "equal":
                for k in range(i2 - i1 + 1):
                    lm.setdefault(i1 + k, j1 + k)
                    rm[i1 + k] = j1 + k
            else:
                changed.append((i1, i2))
        lm.setdefault(len(base), len(lines[p]))
        rm[len(base)] = len(lines[p])
        left[p], right[p] = lm, rm

    # Union overlapping/touching changed ranges into regions.
    regions = []
    for s, e in sorted(changed):
        if regions and s <= regions[-1][1]:
            regions[-1][1] = max(regions[-1][1], e)
        else:
            regions.append([s, e])

    out, pos = [], 0
    for s, e in regions:
        out.extend(base[pos:s])
        # Text of this region for each platform, grouped by identical text.
        groups = {}
        for p in platforms:
            seg = "".join(lines[p][left[p][s]:right[p][e]])
            groups.setdefault(seg, set()).add(p)
        fallback_seg = "".join(base[s:e])
        others = [(seg, grp) for seg, grp in groups.items() if seg != fallback_seg]
        # Order the #if/#elif branches by first platform in PLATFORMS order.
        others.sort(key=lambda sg: min(platforms.index(p) for p in sg[1]))
        for k, (seg, grp) in enumerate(others):
            out.append(("#if " if k == 0 else "#elif ") + condition(grp, platforms) + "\n")
            out.append(seg)
        out.append("#else\n")
        out.append(fallback_seg)
        out.append("#endif\n")
        pos = e
    out.extend(base[pos:])
    return "".join(out)


def preprocess(text: str, defined: set) -> str:
    """Minimal evaluator for the directives emitted by merge_variants (and
    GUARD), used for verification."""
    def evaluate(expr):
        expr = re.sub(r"defined\((\w+)\)", lambda m: str(m.group(1) in defined), expr)
        expr = expr.replace("&&", " and ").replace("||", " or ").replace("!", " not ")
        return eval(expr, {"__builtins__": {}}, {})

    out, stack = [], []          # stack of (branch_taken, active)
    src = text.replace("\\\n", "").splitlines(keepends=True)
    for line in src:
        if line.startswith("#"):
            d, _, arg = line[1:].strip().partition(" ")
            if d == "if":
                c = evaluate(arg)
                stack.append([c, c])
            elif d == "elif":
                c = not stack[-1][0] and evaluate(arg)
                stack[-1] = [stack[-1][0] or c, c]
            elif d == "else":
                stack[-1] = [True, not stack[-1][0]]
            elif d == "endif":
                stack.pop()
            elif d == "error":
                if all(a for _, a in stack):
                    raise RuntimeError(line.strip())
            else:
                raise RuntimeError(f"unexpected directive: {line!r}")
        elif all(a for _, a in stack):
            out.append(line)
    assert not stack
    return "".join(out)


# --------------------------------------------------------------------------

def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--version", default="N0067",
                    help="toolkit version expected in the archives (default N0067)")
    ap.add_argument("--force-download", action="store_true")
    args = ap.parse_args()

    # Load all platforms.
    sources: dict[str, dict[str, bytes]] = {}
    with tempfile.TemporaryDirectory() as tmp:
        for platform, (archive, _, _) in PLATFORMS.items():
            print(platform)
            path = download(platform, archive, args.version, args.force_download)
            work = Path(tmp) / platform
            work.mkdir()
            sources[platform] = load_platform(extract(path, work))
            print(f"  {len(sources[platform])} files")

    # Check the version (tkvrsn.f contains the toolkit version string).
    for platform, files in sources.items():
        if f"VERSTR = '{args.version}'".encode() not in files.get("tkvrsn.f", b""):
            print(f"ERROR: {platform} tkvrsn.f does not mention {args.version}")
            return 1

    platforms = list(PLATFORMS)
    all_names = sorted(set().union(*(f.keys() for f in sources.values())))
    for name in all_names:
        missing = [p for p in platforms if name not in sources[p]]
        if missing:
            print(f"ERROR: {name} missing on {', '.join(missing)}")
            return 1

    # Write output (only the directory this script owns).
    if OUT_DIR.exists():
        shutil.rmtree(OUT_DIR)
    OUT_DIR.mkdir(parents=True)
    merged = []
    for name in all_names:
        versions = {p: sources[p][name] for p in platforms}
        if len(set(versions.values())) == 1 and name != GUARD_FILE:
            (OUT_DIR / name).write_bytes(versions[platforms[0]])
            continue
        if not name.endswith(".f"):
            print(f"ERROR: non-Fortran file {name} differs between platforms")
            return 1
        texts = {p: v.decode("ascii") for p, v in versions.items()}
        for p, t in texts.items():
            if any(line.startswith("#") for line in t.splitlines()):
                print(f"ERROR: {name} ({p}) already contains '#' lines")
                return 1
        text = merge_variants(texts)
        if name == GUARD_FILE:
            text = GUARD + text
        out_name = name[:-2] + ".F"
        (OUT_DIR / out_name).write_bytes(text.encode("ascii"))
        merged.append(out_name)

    # Verify: preprocessing for each platform reproduces its original files
    # (exactly for .f files; for merged .F files, exactly except possibly
    # for blank lines -- see blank_like_base).
    blank_only = set()
    for platform in platforms:
        defined = {os_macro(platform), cc_macro(platform)}
        for p in OUT_DIR.iterdir():
            data = p.read_bytes()
            if p.suffix == ".F":
                data = preprocess(data.decode("ascii"), defined).encode("ascii")
                orig = sources[platform][p.stem + ".f"]
                if data != orig and nonblank(data) == nonblank(orig):
                    blank_only.add(f"{p.name} ({platform})")
                    continue
            else:
                orig = sources[platform][p.name]
            if data != orig:
                print(f"ERROR: verification failed for {p.name} on {platform}")
                return 1
    for item in sorted(blank_only):
        print(f"note: blank lines differ from the original in {item}")

    print(f"\n{len(all_names) - len(merged)} common files and "
          f"{len(merged)} merged platform files (.F) -> {OUT_DIR.relative_to(ROOT)}")
    print("  " + " ".join(merged))
    print("verification passed for all platforms")
    return 0


if __name__ == "__main__":
    sys.exit(main())
