#!/usr/bin/env python3
"""
Convert free-form Fortran code to lowercase, in place, leaving character
literals, comments and preprocessor directives unchanged.

    python tools/lowercase_code.py [DIR ...]      (default: src/spicelib)

Applies to *.f90, *.F90 and *.inc. Preprocessor lines (and their backslash
continuations) are not changed, since macro names are case-sensitive.
Character literals may continue across lines (``&`` ... ``&``).

Verification (before anything is written): every source file is compiled
with gfortran before and after (same file names, same flags for each
platform macro set), and the object files must be byte-identical. Use
--no-compile to skip. Run under ``pixi run`` so gfortran is on the PATH.
"""

import argparse
import concurrent.futures
import filecmp
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from fixed2free import gfortran_macro_sets  # noqa: E402

SUFFIXES = (".f90", ".F90", ".inc")
COMPILE_FLAGS = ["-c", "-fallow-argument-mismatch", "-w"]


def map_code(text: str, fn) -> str:
    """Apply fn to every run of code characters (outside character literals,
    comments and preprocessor directives) of each line; return the new text.
    fn gets and returns a string; a run never spans lines."""
    out, quote, directive = [], None, False
    for line in text.split("\n"):
        if directive or (quote is None and line.startswith("#")):
            out.append(line)
            directive = line.endswith("\\")
            continue
        pieces, start, k = [], 0, 0
        if quote is not None:
            # Continued character literal: resumes after the leading '&'.
            stripped = line.lstrip()
            if stripped.startswith("&"):
                k = len(line) - len(stripped) + 1
        code_from = None if quote is not None else 0
        while k < len(line):
            ch = line[k]
            if quote is not None:
                if ch == quote:
                    if k + 1 < len(line) and line[k + 1] == quote:
                        k += 1          # doubled quote inside the literal
                    else:
                        quote = None
                        code_from = k + 1
                elif ch == "&" and line[k + 1:].strip() == "":
                    break               # literal continues on the next line
            elif ch in "'\"" or ch == "!":
                pieces.append(line[start:code_from])
                pieces.append(fn(line[code_from:k]))
                start, code_from = k, None
                if ch == "!":
                    break               # comment to end of line
                quote = ch
            k += 1
        if code_from is not None:
            pieces.append(line[start:code_from])
            pieces.append(fn(line[code_from:]))
        else:
            pieces.append(line[start:])
        out.append("".join(pieces))
    if quote is not None:
        raise ValueError("unterminated character literal at end of file")
    return "\n".join(out)


def lowercase_text(text: str) -> str:
    return map_code(text, str.lower)


def compile_one(job):
    src, obj, flags = job
    r = subprocess.run(["gfortran", *COMPILE_FLAGS, *flags, src.name, "-o", str(obj)],
                       cwd=src.parent, capture_output=True, text=True)
    return src, r.returncode, r.stderr


def verify(files: dict, work: Path, extra_flags=(), old_texts=None) -> list:
    """files: {path: new text}. Compile old and new copies; return failures.
    extra_flags are added to every compilation (e.g. -I for module files).
    old_texts: {path: text} to compile instead of the file on disk as the
    old copy (e.g. with blank lines so that line numbers match)."""
    dirs = sorted({p.parent for p in files})
    # Copy only the outermost directories (e.g. src, not also src/legacy).
    tops = [d for d in dirs if not any(o != d and o in d.parents for o in dirs)]
    for tag in ("old", "new"):
        for d in tops:
            shutil.copytree(d, work / tag / d.relative_to(ROOT),
                            ignore=shutil.ignore_patterns("*.mod", "*.o", "*.smod"))
    for p, text in files.items():
        (work / "new" / p.relative_to(ROOT)).write_text(text, encoding="ascii")
    for p, text in (old_texts or {}).items():
        (work / "old" / p.relative_to(ROOT)).write_text(text, encoding="ascii")

    jobs = []
    for k, defined in enumerate(gfortran_macro_sets()):
        flags = [f"-D{m}" for m in sorted(defined)]
        for p in files:
            if p.suffix == ".inc" or (k > 0 and p.suffix != ".F90"):
                continue
            for tag in ("old", "new"):
                src = work / tag / p.relative_to(ROOT)
                obj = work / "obj" / str(k) / tag / (str(p.relative_to(ROOT)).replace(os.sep, "__") + ".o")
                obj.parent.mkdir(parents=True, exist_ok=True)
                inc = [f"-I{work / tag / d.relative_to(ROOT)}" for d in dirs]
                jobs.append((src, obj, flags + inc + list(extra_flags)))
    failures = []
    with concurrent.futures.ThreadPoolExecutor(os.cpu_count() or 4) as ex:
        for src, rc, err in ex.map(compile_one, jobs):
            if rc != 0:
                failures.append(f"compile failed: {src}\n{err}")
    if failures:
        return failures
    for odir in sorted((work / "obj").glob("*/old")):
        for obj in sorted(odir.iterdir()):
            if not filecmp.cmp(obj, odir.parent / "new" / obj.name, shallow=False):
                failures.append(f"object files differ: {obj.stem.replace('__', '/')} "
                                f"(macro set {odir.parent.name})")
    return failures


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("dirs", nargs="*", type=Path, default=[ROOT / "src" / "spicelib"])
    ap.add_argument("--no-compile", action="store_true",
                    help="skip the gfortran object-file comparison")
    args = ap.parse_args()
    if not args.no_compile and shutil.which("gfortran") is None:
        print("ERROR: gfortran not found (run under `pixi run`, or use --no-compile)")
        return 1

    files = {}
    for d in args.dirs:
        for p in sorted(d.resolve().iterdir()):
            if p.is_file() and p.suffix in SUFFIXES:
                old = p.read_text(encoding="ascii")
                try:
                    new = lowercase_text(old)
                except ValueError as e:
                    print(f"ERROR: {p}: {e}; nothing was changed")
                    return 1
                if new != old:
                    files[p] = new
    print(f"{len(files)} files to change")
    if not files:
        return 0

    if not args.no_compile:
        with tempfile.TemporaryDirectory() as tmp:
            failures = verify(files, Path(tmp))
        if failures:
            for f in failures[:20]:
                print("ERROR: " + f)
            print(f"{len(failures)} failure(s); nothing was changed")
            return 1
        print("object files identical before and after for all gfortran macro sets")

    for p, text in files.items():
        p.write_text(text, encoding="ascii")
    print(f"changed {len(files)} files")
    return 0


if __name__ == "__main__":
    sys.exit(main())
