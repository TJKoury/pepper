#!/usr/bin/env python3
"""
Convert fixed-form Fortran sources to free form, in place.

    *.f   -> *.f90
    *.F   -> *.F90   (preprocessed sources)
    *.inc -> *.inc   (include files, converted to free form)

Conversion rules (purely mechanical; code is otherwise unchanged):

  * Comment lines (C, c, * or ! in column 1) become ``!`` comments. A
    comment that would start with ``!$`` followed by a blank (e.g. NAIF's
    ``C$ Abstract`` header markers) is written as ``! $`` instead, since
    ``!$ `` is the OpenMP conditional-compilation sentinel in free form.
  * Statement labels stay in columns 1-5; column 6 is blanked; text past
    column 72 (ignored in fixed form) must be empty.
  * Blanks that fixed form ignores but free form does not are removed
    (outside character literals): inside numbers (``299 792 458.0D0``) and
    inside dot operators (``RE. LE. 0``). Blanks inside identifiers are
    fixed through the explicit TOKEN_FIXES table.
  * A code line is given a trailing `` &`` when the next code line (skipping
    comments, blank lines and preprocessor directives) is a continuation
    line. Continuation lines with no content become blank lines.
  * Preprocessor directives (lines starting with ``#``, with any
    backslash-continued lines) are copied unchanged.

Verification (before the originals are removed):

  1. For preprocessed files, for every platform macro set, preprocessing
     then converting must equal converting then preprocessing (this checks
     the ``&`` placement around ``#if`` blocks).
  2. Every file is compiled with gfortran both ways (fixed sources with the
     fixed includes, free sources with the free includes) and the object
     files must be byte-identical -- or, failing that, gfortran's parse trees
     (-fdump-tree-original) must be identical up to statement order and
     temporary numbering. (gfortran orders some independent setup statements
     by source position.) Use --no-compile to skip this.

Usage:
    python tools/fixed2free.py [DIR ...] [-I INCDIR ...] [--no-compile]

    Default DIRs: src/spicelib, test/tspice, test/tspice/testutil,
    test/tspice/support (converted together, since the tests include
    SPICELIB's include files). Default INCDIR: src/spicelib.

Run under ``pixi run`` so gfortran is on the PATH.
"""

import argparse
import concurrent.futures
import filecmp
import os
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from import_naif import PRODUCTS, Macros, preprocess  # noqa: E402

EXT = {".f": ".f90", ".F": ".F90", ".inc": ".inc"}
DEFAULT_DIRS = [ROOT / "src" / "spicelib",
                ROOT / "test" / "tspice",
                ROOT / "test" / "tspice" / "testutil",
                ROOT / "test" / "tspice" / "support"]
COMPILE_FLAGS = ["-c", "-fallow-argument-mismatch", "-w"]


def is_directive_block(lines):
    """Flags for lines that are preprocessor directives (incl. backslash
    continuations)."""
    flags, cont = [], False
    for line in lines:
        d = cont or line.startswith("#")
        flags.append(d)
        cont = d and line.endswith("\\")
    return flags


def is_comment(line: str) -> bool:
    return not line.strip() or line[0] in "Cc*!"


def is_continuation(line: str) -> bool:
    return len(line) > 5 and line[5] not in " 0"


def is_empty_continuation(line: str) -> bool:
    """A continuation line with no content (only the marker in column 6).
    It contributes nothing to the statement; in free form it would be a
    blank line, which cannot carry a continuation, so it is dropped from
    the statement."""
    return (is_continuation(line) and not line[:5].strip()
            and not line[6:72].strip())


def check_code(line: str, where: str) -> None:
    """Reject constructs the mechanical conversion does not handle."""
    if "\t" in line:
        raise ValueError(f"{where}: tab character")
    if line[72:].strip():
        raise ValueError(f"{where}: text beyond column 72")
    if line[:5].strip() and not line[:5].strip().isdigit():
        raise ValueError(f"{where}: invalid label field")
    quote = None
    for ch in line[6:72]:
        if quote:
            if ch == quote:
                quote = None
        elif ch in "'\"":
            quote = ch
        elif ch == "!":
            raise ValueError(f"{where}: inline comment")
    if quote:
        raise ValueError(f"{where}: character context continued across lines")


# Individual blanks inside identifiers (legal in fixed form, but not handled
# by squeeze_blanks): {file name: [(old code text, new code text), ...]}.
TOKEN_FIXES = {
    "f_sbf.f": [("CALL DELFIL( PCK 0 )", "CALL DELFIL( PCK0 )")],
}

DOT_OP = re.compile(r"\.\s*(EQ|NE|LT|LE|GT|GE|AND|OR|NOT|EQV|NEQV|TRUE|FALSE)\s*\.",
                    re.IGNORECASE)
DIGIT_BLANK_DIGIT = re.compile(r"(?<=\d) +(?=\d)")


def squeeze_blanks(body: str) -> str:
    """Remove blanks that fixed form ignores but free form does not, outside
    character literals: inside numbers (``299 792 458.0D0``) and inside dot
    operators (``RE. LE. 0``)."""
    parts, quote, start = [], None, 0
    for k, ch in enumerate(body):
        if quote:
            if ch == quote:
                quote = None
                parts.append(body[start:k + 1])
                start = k + 1
        elif ch in "'\"":
            parts.append(_squeeze(body[start:k]))
            quote, start = ch, k
    parts.append(body[start:] if quote else _squeeze(body[start:]))
    return "".join(parts)


def _squeeze(code: str) -> str:
    code = DOT_OP.sub(lambda m: "." + m.group(1) + ".", code)
    return DIGIT_BLANK_DIGIT.sub("", code)


def convert_comment(line: str) -> str:
    text = line[1:].rstrip() if line.strip() else ""
    if not line.strip():
        return ""
    if text.startswith("$") and (len(text) == 1 or text[1].isspace()):
        return "! " + text
    return "!" + text


def convert_text(text: str, name: str = "<text>") -> str:
    lines = text.split("\n")
    trailing_newline = lines and lines[-1] == ""
    if trailing_newline:
        lines = lines[:-1]
    fixes = TOKEN_FIXES.get(Path(name.split(":")[0]).name, [])
    for old, new in fixes:
        hits = [k for k, l in enumerate(lines) if old in l and not is_comment(l)]
        if len(hits) != 1:
            raise ValueError(f"{name}: token fix {old!r} matched {len(hits)} lines")
        lines[hits[0]] = lines[hits[0]].replace(old, new)
    directive = is_directive_block(lines)
    code = [i for i, l in enumerate(lines) if not directive[i] and not is_comment(l)
            and not is_empty_continuation(l)]
    next_code = {a: b for a, b in zip(code, code[1:])}

    out = []
    for i, line in enumerate(lines):
        if directive[i]:
            out.append(line)
        elif is_comment(line):
            out.append(convert_comment(line))
        elif is_empty_continuation(line):
            out.append("")
        else:
            check_code(line, f"{name}:{i + 1}")
            new = (line[:5] + " " + squeeze_blanks(line[6:72])).rstrip()
            j = next_code.get(i)
            if j is not None and is_continuation(lines[j]):
                new += " &"
            out.append(new)
    return "\n".join(out) + ("\n" if trailing_newline else "")


# --------------------------------------------------------------------------
# Verification
# --------------------------------------------------------------------------

def macro_sets():
    sets = set()
    for cfg in PRODUCTS.values():
        m = Macros(cfg["platforms"])
        sets |= {frozenset(m.defined(p)) for p in cfg["platforms"]}
    return sorted(sets, key=sorted)


def gfortran_macro_sets():
    return [s for s in macro_sets() if "SPICE_COMPILER_GFORTRAN" in s]


def verify_directives(name: str, fixed: str, free: str) -> None:
    for defined in macro_sets():
        a = convert_text(preprocess(fixed, set(defined)), name)
        b = preprocess(free, set(defined))
        if a != b:
            raise ValueError(f"{name}: directive/continuation mismatch for "
                             f"{' '.join(sorted(defined))}")


def compile_one(args):
    src, obj, flags = args
    r = subprocess.run(["gfortran", *COMPILE_FLAGS, *flags, src.name, "-o", str(obj)],
                       cwd=src.parent, capture_output=True, text=True)
    return src, r.returncode, r.stderr


def verify_compile(pairs, incdirs, workdir: Path, notes: list) -> list:
    """pairs: list of (fixed_path, free_path). incdirs: {"fixed": [...],
    "free": [...]} include directories. Returns a list of failure messages."""
    failures, jobs = [], []
    for k, defined in enumerate(gfortran_macro_sets()):
        flags = [f"-D{m}" for m in sorted(defined)]
        for n, (fixed, free) in enumerate(pairs):
            if fixed.suffix == ".inc":
                continue
            if k > 0 and fixed.suffix != ".F90":
                continue  # macros only matter for preprocessed files
            for src, tag in ((fixed, "fixed"), (free, "free")):
                obj = workdir / "obj" / str(k) / tag / f"{n}.o"
                obj.parent.mkdir(parents=True, exist_ok=True)
                inc = [f"-I{d}" for d in incdirs[tag]]
                jobs.append((src, obj, flags + inc + [f"-f{tag}-form"]))
    with concurrent.futures.ThreadPoolExecutor(os.cpu_count() or 4) as ex:
        for src, rc, err in ex.map(compile_one, jobs):
            if rc != 0:
                failures.append(f"compile failed: {src}\n{err}")
    if failures:
        return failures
    jobs_by_obj = {str(obj): (src, flags) for src, obj, flags in jobs}
    for k in range(len(gfortran_macro_sets())):
        fdir = workdir / "obj" / str(k) / "fixed"
        if not fdir.exists():
            continue
        for obj in sorted(fdir.iterdir()):
            other = workdir / "obj" / str(k) / "free" / obj.name
            if filecmp.cmp(obj, other, shallow=False):
                continue
            # gfortran may order independent setup statements (e.g. for
            # assumed-length character dummies) differently depending on
            # source positions. Accept if the parse trees are identical up to
            # statement order and temporary numbering.
            dumps = [tree_dump(*jobs_by_obj[str(o)], o.with_suffix(".tree"))
                     for o in (obj, other)]
            if dumps[0] == dumps[1]:
                notes.append(f"{pairs[int(obj.stem)][1].name}: objects differ only "
                             f"in statement order")
            else:
                failures.append(f"object files differ: {pairs[int(obj.stem)][0].name} "
                                f"(macro set {k})")
    return failures


def tree_dump(src: Path, flags: list, out: Path) -> list:
    """gfortran's -fdump-tree-original output, with temporaries renumbered
    and lines sorted."""
    subprocess.run(["gfortran", *COMPILE_FLAGS, *flags, src.name,
                    "-o", str(out.with_suffix(".o2")),
                    f"-fdump-tree-original={out}"],
                   cwd=src.parent, check=True, capture_output=True)
    text = re.sub(r"\bD\.\d+", "D.#", out.read_text())
    return sorted(text.splitlines())


# --------------------------------------------------------------------------

def convert_dirs(dirs: list, incdirs: list, compile_check: bool) -> bool:
    """Convert all fixed-form files in ``dirs`` together. ``incdirs`` are
    extra include directories (e.g. src/spicelib for the tests); those that
    are also being converted are compiled against their fixed/free copies."""
    converted = {}
    for d in dirs:
        srcs = sorted(p for p in d.iterdir() if p.is_file() and p.suffix in EXT)
        print(f"{d.relative_to(ROOT)}: {len(srcs)} files")
        for p in srcs:
            fixed = p.read_text(encoding="ascii")
            free = convert_text(fixed, str(p.relative_to(ROOT)))
            if p.suffix == ".F":
                verify_directives(p.name, fixed, free)
            converted[p] = free
    if not converted:
        print("nothing to convert")
        return True
    print("directive checks passed")

    if compile_check:
        with tempfile.TemporaryDirectory() as tmp:
            work = Path(tmp)
            def copy_of(d, tag):
                return work / tag / str(d.relative_to(ROOT))
            pairs = []
            for p, free in converted.items():
                # Same file name for both (gfortran embeds it in I/O calls);
                # the source form is selected with -ffixed-form/-ffree-form.
                f1 = copy_of(p.parent, "fixed") / (p.stem + EXT[p.suffix])
                f2 = copy_of(p.parent, "free") / (p.stem + EXT[p.suffix])
                f1.parent.mkdir(parents=True, exist_ok=True)
                f2.parent.mkdir(parents=True, exist_ok=True)
                shutil.copyfile(p, f1)
                f2.write_text(free, encoding="ascii")
                pairs.append((f1, f2))
            inc = {tag: [copy_of(d, tag) if d in dirs else d for d in incdirs]
                   for tag in ("fixed", "free")}
            notes = []
            failures = verify_compile(pairs, inc, work, notes)
            for n in sorted(set(notes)):
                print("  note: " + n)
            if failures:
                for f in failures[:20]:
                    print("ERROR: " + f)
                print(f"{len(failures)} failure(s); nothing was changed")
                return False
        print("object files identical (fixed vs free) for all gfortran macro sets")

    for p, free in converted.items():
        new = p.with_name(p.stem + EXT[p.suffix])
        if new != p:
            p.unlink()
        new.write_text(free, encoding="ascii")
    print(f"converted {len(converted)} files")
    return True


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("dirs", nargs="*", type=Path, default=DEFAULT_DIRS,
                    help="directories to convert together (default: src/spicelib "
                         "and the tspice test suite)")
    ap.add_argument("-I", dest="incdirs", action="append", type=Path,
                    default=None, help="include directory for the compile check "
                                       "(default: src/spicelib)")
    ap.add_argument("--no-compile", action="store_true",
                    help="skip the gfortran object-file comparison")
    args = ap.parse_args()
    if not args.no_compile and shutil.which("gfortran") is None:
        print("ERROR: gfortran not found (run under `pixi run`, or use --no-compile)")
        return 1
    dirs = [d.resolve() for d in args.dirs]
    incdirs = [d.resolve() for d in (args.incdirs or [ROOT / "src" / "spicelib"])]
    try:
        return 0 if convert_dirs(dirs, incdirs, not args.no_compile) else 1
    except ValueError as e:
        print(f"ERROR: {e}; nothing was changed")
        return 1


if __name__ == "__main__":
    sys.exit(main())
