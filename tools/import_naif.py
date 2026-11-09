#!/usr/bin/env python3
"""
HISTORICAL: this tool did the original import of NAIF's sources (2026-11). The
layout has changed since: src/spicelib is gone (the routines are modules in src/,
the remaining legacy ones in src/legacy/spicelib/, the include files modules in
src/inc/), and tspice is the package tspice/ (tspice/src, tspice/app).

Import NAIF SPICE Fortran sources from the per-platform archives into a
single codebase.

Products:

  spicelib  The SPICELIB library, from the Fortran toolkit archives
            -> src/spicelib
  tspice    The NAIF SPICELIB test suite (tspice program plus its testutil
            and support libraries), from the tspice archives
            -> test/tspice, test/tspice/testutil, test/tspice/support

For each product:
  1. Download the archive for each platform/compiler (cached in
     ``upstream/archives/<version>``).
  2. Extract only the needed source directories.
  3. Normalize every file: ``.for`` -> ``.f``, ``.pgm`` -> ``.f`` (main
     programs) and CRLF -> LF.
  4. Files identical on all platforms are written once. Files that differ
     only in comments/blank lines are written once using the FALLBACK
     platform's copy. Files with real code differences are merged
     line-by-line into a single preprocessed source ``<name>.F``: each
     differing block is wrapped in ``#if`` / ``#elif`` / ``#else`` /
     ``#endif`` directives on the platform macros below.
  5. Verify, for every platform, that preprocessing the merged sources with
     that platform's macros reproduces its original files (up to blank
     lines in merged files, and up to comments in comment-only variants).

Platform macros (set in fpm.toml via the ``platform`` feature):

    SPICE_OS_MACOS, SPICE_OS_LINUX, SPICE_OS_WINDOWS, SPICE_OS_CYGWIN
    SPICE_COMPILER_GFORTRAN, SPICE_COMPILER_INTEL

The ``#else`` branch of every block is always the code from the product's
FALLBACK platform (a Unix-style gfortran build), so combinations NAIF does
not ship (e.g. native Windows + gfortran) get a consistent,
gfortran-compatible set.

Not imported: the NAIF build scripts (mkprodct.csh / mkprodct.bat), the
tspice archive's own copy of spicelib (identical to the toolkit's), include
files in the tspice directories that are identical to src/spicelib's (found
through fpm's include-dir), and twrk.pgm (an interactive progress-bar demo,
not a test).

Usage:
    python tools/import_naif.py [spicelib] [tspice] [--version N0067]
                                [--force-download]

With no product arguments, both are imported (spicelib first).

The imported sources are NAIF's original fixed-form code (.f/.F). To get the
free-form sources used in this repository, run tools/fixed2free.py
afterwards. Requires
``tar`` on the PATH (to read the ``.tar.Z`` archives; both GNU tar and
bsdtar, including Windows' tar.exe, handle them).
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

ROOT = Path(__file__).resolve().parent.parent
ARCHIVE_DIR = ROOT / "upstream" / "archives"
SPICELIB_DIR = ROOT / "src" / "spicelib"

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

PRODUCTS = {
    "spicelib": {
        # The toolkit URL always serves the latest release; the version is
        # checked against tkvrsn.f.
        "url": "https://naif.jpl.nasa.gov/pub/naif/toolkit/FORTRAN/{platform}/packages/{archive}",
        # NAIF platform -> (archive, OS, compiler). Only 64-bit x64 builds of
        # gfortran and Intel Fortran. (NAIF has no Mac arm64 Fortran toolkit.)
        "platforms": {
            "MacIntel_OSX_gfortran_64bit": ("toolkit.tar.Z", "MACOS", "GFORTRAN"),
            "MacIntel_OSX_IFORT_64bit": ("toolkit.tar.Z", "MACOS", "INTEL"),
            "PC_Linux_gfortran_64bit": ("toolkit.tar.Z", "LINUX", "GFORTRAN"),
            "PC_Linux_IFORT_64bit": ("toolkit.tar.Z", "LINUX", "INTEL"),
            "PC_Cygwin_gfortran_64bit": ("toolkit.tar.gz", "CYGWIN", "GFORTRAN"),
            "PC_Windows_IFORT_64bit": ("toolkit.zip", "WINDOWS", "INTEL"),
        },
        "fallback": "PC_Cygwin_gfortran_64bit",
        "members": {"toolkit/src/spicelib": SPICELIB_DIR},
        "version_check": ("toolkit/src/spicelib", "tkvrsn.f", "VERSTR = '{version}'"),
        "guard_file": "zzplatfm.f",
        "dedup_inc": False,
    },
    "tspice": {
        "url": "https://naif.jpl.nasa.gov/pub/naif/misc/tspice/{version}/{platform}/{archive}",
        # NAIF builds tspice with gfortran on Mac/Linux and Intel Fortran on
        # Windows.
        "platforms": {
            "MacIntel_OSX_64bit": ("tspice.tar", "MACOS", "GFORTRAN"),
            "PC_Linux_64bit": ("tspice.tar", "LINUX", "GFORTRAN"),
            "PC_Windows_64bit": ("tspice.tar", "WINDOWS", "INTEL"),
        },
        "fallback": "PC_Linux_64bit",
        "members": {
            "tspice/src/tspice": ROOT / "test" / "tspice",
            # fpm links non-module procedures into a test program only from
            # the program's own directory tree, so the libraries go below it.
            "tspice/src/testutil": ROOT / "test" / "tspice" / "testutil",
            "tspice/src/support": ROOT / "test" / "tspice" / "support",
        },
        "version_check": None,   # the version is part of the URL
        "guard_file": None,
        "dedup_inc": True,
    },
}

SKIP_FILES = {"mkprodct.csh", "mkprodct.bat", "twrk.pgm"}


# --------------------------------------------------------------------------
# Download / extract / normalize
# --------------------------------------------------------------------------

def download(url: str, dest: Path, force: bool) -> Path:
    if dest.exists() and not force:
        print(f"  cached   {dest.relative_to(ROOT)}")
        return dest
    print(f"  download {url}")
    dest.parent.mkdir(parents=True, exist_ok=True)
    tmp = dest.with_suffix(dest.suffix + ".part")
    with urllib.request.urlopen(url) as r, open(tmp, "wb") as f:
        shutil.copyfileobj(r, f)
    tmp.replace(dest)
    return dest


def extract(archive: Path, workdir: Path, members: list) -> None:
    """Extract only the given directories from the archive."""
    if archive.suffix == ".zip":
        with zipfile.ZipFile(archive) as z:
            names = [n for n in z.namelist()
                     if any(n.startswith(m + "/") for m in members)]
            z.extractall(workdir, names)
    else:
        subprocess.run(["tar", "-xf", str(archive), "-C", str(workdir), *members],
                       check=True)


def normalize_name(name: str) -> str:
    for ext in (".for", ".pgm"):
        if name.lower().endswith(ext):
            return name[:-len(ext)] + ".f"
    return name


def normalize_text(data: bytes) -> bytes:
    data = data.replace(b"\r\n", b"\n")
    if b"\r" in data:
        raise ValueError("stray carriage return found")
    return data


def load_dir(srcdir: Path) -> dict:
    files = {}
    for p in sorted(srcdir.iterdir()):
        if not p.is_file() or p.name in SKIP_FILES:
            continue
        name = normalize_name(p.name)
        if name in files:
            raise RuntimeError(f"name collision after normalization: {p}")
        try:
            data = normalize_text(p.read_bytes())
        except ValueError as e:
            raise RuntimeError(f"{p}: {e}") from None
        if p.suffix.lower() == ".pgm":
            data = add_program_statement(data, p.stem.upper())
        files[name] = data
    return files


def add_program_statement(data: bytes, name: str) -> bytes:
    """Insert a PROGRAM statement before the first non-comment line if the
    main program has none (legal Fortran, but fpm needs it to recognize the
    file as a program)."""
    lines = data.splitlines(keepends=True)
    for i, line in enumerate(lines):
        if not is_comment(line):
            if line.split()[0].upper() == b"PROGRAM":
                return data
            lines.insert(i, f"      PROGRAM {name}\n".encode())
            return b"".join(lines)
    raise RuntimeError(f"no code in main program {name}")


# --------------------------------------------------------------------------
# Preprocessor conditions
# --------------------------------------------------------------------------

class Macros:
    def __init__(self, platforms: dict):
        self.platforms = platforms

    def os(self, p):
        return f"SPICE_OS_{self.platforms[p][1]}"

    def cc(self, p):
        return f"SPICE_COMPILER_{self.platforms[p][2]}"

    def defined(self, p):
        return {self.os(p), self.cc(p)}

    def condition(self, group: set) -> str:
        """Simplest condition on the platform macros that is true for
        exactly the platforms in ``group`` (out of all platforms)."""
        universe = list(self.platforms)

        def matches(pred):
            return {p for p in universe if pred(p)} == group

        for cc in sorted({self.cc(p) for p in universe}):
            if matches(lambda p: self.cc(p) == cc):
                return f"defined({cc})"
        if len(group) == 1:
            (p,) = group
            return f"defined({self.os(p)}) && defined({self.cc(p)})"
        for os_ in sorted({self.os(p) for p in universe}):
            if matches(lambda p: self.os(p) == os_):
                return f"defined({os_})"
        return " || ".join(f"(defined({self.os(p)}) && defined({self.cc(p)}))"
                           for p in universe if p in group)


# --------------------------------------------------------------------------
# N-way line merge
# --------------------------------------------------------------------------

def is_blank(line: str) -> bool:
    return not line.strip()


def is_comment(line) -> bool:
    """Fixed-form comment or blank line (str or bytes)."""
    s = line.decode("ascii") if isinstance(line, bytes) else line
    return not s.strip() or s[0] in "Cc*!"


def nonblank(text: bytes) -> list:
    return [l for l in text.splitlines() if l.strip()]


def code_lines(text: bytes) -> list:
    return [l for l in text.splitlines() if not is_comment(l)]


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


def merge_variants(texts: dict, fallback: str, macros: Macros) -> str:
    """Merge per-platform versions of one file into a single source with
    #if blocks. ``texts`` maps platform -> file contents (str)."""
    platforms = list(texts)
    base = texts[fallback].splitlines(keepends=True)
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
        # Order the #if/#elif branches by first platform in table order.
        others.sort(key=lambda sg: min(platforms.index(p) for p in sg[1]))
        for k, (seg, grp) in enumerate(others):
            out.append(("#if " if k == 0 else "#elif ") + macros.condition(grp) + "\n")
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

    out, stack = [], []          # stack of [branch_taken, active]
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
# Product import
# --------------------------------------------------------------------------

def same_as_spicelib(spicelib: bytes, fixed: bytes) -> bool:
    """True if a (fixed-form) include file is the same as SPICELIB's copy,
    which may already have been converted to free form."""
    if spicelib is None:
        return False
    if spicelib == fixed:
        return True
    from fixed2free import convert_text   # lazy: fixed2free imports us
    return convert_text(fixed.decode("ascii")).encode("ascii") == spicelib


def import_product(name: str, cfg: dict, version: str, force: bool) -> bool:
    print(f"=== {name} ({version})")
    platforms = list(cfg["platforms"])
    fallback = cfg["fallback"]
    macros = Macros(cfg["platforms"])
    members = list(cfg["members"])

    # sources[member][platform][filename] = normalized bytes
    sources = {m: {} for m in members}
    with tempfile.TemporaryDirectory() as tmp:
        for platform, (archive, _, _) in cfg["platforms"].items():
            print(platform)
            url = cfg["url"].format(platform=platform, archive=archive, version=version)
            path = download(url, ARCHIVE_DIR / version / f"{platform}_{archive}", force)
            work = Path(tmp) / platform
            work.mkdir()
            extract(path, work, members)
            for m in members:
                sources[m][platform] = load_dir(work / m)
                print(f"  {m}: {len(sources[m][platform])} files")
            vc = cfg["version_check"]
            if vc:
                text = sources[vc[0]][platform].get(vc[1], b"")
                if vc[2].format(version=version).encode() not in text:
                    print(f"ERROR: {platform} {vc[1]} is not version {version}")
                    return False

    spicelib_inc = {}
    if cfg["dedup_inc"]:
        spicelib_inc = {p.name: p.read_bytes() for p in SPICELIB_DIR.glob("*.inc")}
        if not spicelib_inc:
            print(f"ERROR: no include files in {SPICELIB_DIR}; import spicelib first")
            return False

    for member, outdir in cfg["members"].items():
        src = sources[member]
        rel = outdir.relative_to(ROOT)
        all_names = sorted(set().union(*(f.keys() for f in src.values())))
        for fname in all_names:
            missing = [p for p in platforms if fname not in src[p]]
            if missing:
                print(f"ERROR: {member}/{fname} missing on {', '.join(missing)}")
                return False

        if outdir.exists():
            shutil.rmtree(outdir)
        outdir.mkdir(parents=True)
        merged, comment_only, deduped = [], [], []
        for fname in all_names:
            versions = {p: src[p][fname] for p in platforms}
            identical = len(set(versions.values())) == 1
            if fname.endswith(".inc") and identical and same_as_spicelib(
                    spicelib_inc.get(fname), versions[fallback]):
                deduped.append(fname)
                continue
            if identical and fname != cfg["guard_file"]:
                (outdir / fname).write_bytes(versions[fallback])
                continue
            if not fname.endswith(".f"):
                print(f"ERROR: non-Fortran file {fname} differs between platforms")
                return False
            if (fname != cfg["guard_file"]
                    and len({tuple(code_lines(v)) for v in versions.values()}) == 1):
                (outdir / fname).write_bytes(versions[fallback])
                comment_only.append(fname)
                continue
            texts = {p: v.decode("ascii") for p, v in versions.items()}
            for p, t in texts.items():
                if any(line.startswith("#") for line in t.splitlines()):
                    print(f"ERROR: {fname} ({p}) already contains '#' lines")
                    return False
            text = merge_variants(texts, fallback, macros)
            if fname == cfg["guard_file"]:
                text = GUARD + text
            out_name = fname[:-2] + ".F"
            (outdir / out_name).write_bytes(text.encode("ascii"))
            merged.append(out_name)

        # Verify: for each platform, the output reproduces the originals.
        notes = set()
        for platform in platforms:
            rebuilt = {}
            for p in outdir.iterdir():
                if not p.is_file():
                    continue
                data = p.read_bytes()
                if p.suffix == ".F":
                    data = preprocess(data.decode("ascii"),
                                      macros.defined(platform)).encode("ascii")
                    rebuilt[p.stem + ".f"] = data
                else:
                    rebuilt[p.name] = data
            for fname in deduped:
                rebuilt[fname] = spicelib_inc[fname]
            if set(rebuilt) != set(src[platform]):
                print(f"ERROR: file set mismatch for {member} on {platform}")
                return False
            for fname, data in rebuilt.items():
                orig = src[platform][fname]
                if data == orig:
                    continue
                if fname[:-2] + ".F" in merged and nonblank(data) == nonblank(orig):
                    notes.add(f"blank lines differ from the original in {fname} ({platform})")
                elif fname in comment_only and code_lines(data) == code_lines(orig):
                    notes.add(f"comments differ from the original in {fname} ({platform})")
                else:
                    print(f"ERROR: verification failed for {member}/{fname} on {platform}")
                    return False
        for n in sorted(notes):
            print(f"  note: {n}")

        n_written = len(all_names) - len(deduped)
        print(f"{rel}: {n_written} files ({len(merged)} merged platform files"
              + (f", {len(deduped)} include files shared with src/spicelib omitted"
                 if deduped else "") + ")")
        if merged:
            print("  merged: " + " ".join(merged))
    print("verification passed for all platforms\n")
    return True


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("products", nargs="*", metavar="product",
                    help=f"products to import: {', '.join(PRODUCTS)} (default: all)")
    ap.add_argument("--version", default="N0067",
                    help="toolkit version (default N0067)")
    ap.add_argument("--force-download", action="store_true")
    args = ap.parse_args()
    unknown = set(args.products) - set(PRODUCTS)
    if unknown:
        ap.error(f"unknown product(s): {', '.join(sorted(unknown))}")

    for name in args.products or list(PRODUCTS):
        if not import_product(name, PRODUCTS[name], args.version, args.force_download):
            return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
