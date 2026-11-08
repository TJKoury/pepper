#!/usr/bin/env python3
"""
Switch SPICELIB code to the PEPPER kinds (module pepper_kinds), in place:

    integer           ->  integer(ip)
    double precision  ->  real(wp)
    1.5d0, 1.d-3, 2D5 ->  1.5_wp, 1.0e-3_wp, 2.0e5_wp
    dble(x)           ->  real(x, wp)

and add ``use pepper_kinds, only: ...`` after the procedure statement of each
file that needs it (or update it). Running the script again changes nothing.

    python tools/convert_kinds.py [DIR ...]      (default: src/spicelib)

Applies to *.f90 and *.F90 (one procedure per file, as in SPICELIB). The
.inc files are left alone: they are also included by the tspice tests, and
they become module parameters later. Character literals, comments and
preprocessor directives are not changed. Column alignment of declarations is
kept where there is room. Local names that would clash with ``ip`` or ``wp``
are renamed (RENAMES; the renamed target must be unused).

Verification (before anything is written): every file is compiled with
gfortran before and after (same file names, same flags, every platform
macro set) and the object files must be byte-identical. The "before" copy
gets a blank line where the use statement goes, since gfortran embeds source
line numbers in I/O statements. Use --no-compile to
skip. Run under ``pixi run`` so gfortran is on the PATH.
"""

import argparse
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from lowercase_code import map_code, verify  # noqa: E402

KINDS_MODULE = ROOT / "src" / "pepper_kinds.f90"
SUFFIXES = (".f90", ".F90")

# Local variables whose names clash with the kind parameters.
RENAMES = {"zzekad06.f90": {"wp": "wpad"}}   # "words of padding"; sorts like wp

INTEGER = re.compile(r"\binteger\b(?!\s*[(*])( {4,})?", re.I)
DOUBLE = re.compile(r"\bdouble\s+precision\b( {2,})?", re.I)
DLITERAL = re.compile(r"(?<![\w.])(\d+\.?\d*|\.\d+)[dD]([+-]?\d+)(?![\w])"
                      r"|(?<=\.)(\d+\.?\d*)[dD]([+-]?\d+)(?![\w])")
HEADER = re.compile(r"^(\s*)(?:[a-z*()\s]*?\s)?(subroutine|function)\s+\w+", re.I)


def literal(mant: str, exp: str) -> str:
    if mant.startswith("."):
        mant = "0" + mant
    if "." not in mant:
        mant += ".0"
    elif mant.endswith("."):
        mant += "0"
    exp = exp.lstrip("+")
    return f"{mant}_wp" if int(exp) == 0 else f"{mant}e{exp}_wp"


def rename(code: str, renames: dict) -> str:
    for old, new in renames.items():
        code = re.sub(rf"\b{old}\b", new, code, flags=re.I)
    return code


DBLE = re.compile(r"\bdble\s*\(", re.I)


def convert_dble(code: str) -> str:
    out, k = [], 0
    for m in DBLE.finditer(code):
        if m.start() < k:
            continue
        depth, j = 1, m.end()
        while j < len(code) and depth:
            depth += {"(": 1, ")": -1}.get(code[j], 0)
            j += 1
        if depth:
            raise ValueError(f"dble( ... ) not closed on its line: {code!r}")
        inner = code[m.end():j - 1]
        lead, arg, trail = re.match(r"(\s*)(.*?)(\s*)$", inner, re.S).groups()
        out.append(code[k:m.start()] + f"real({lead}{convert_dble(arg)}, wp{trail})")
        k = j
    out.append(code[k:])
    return "".join(out)


def convert_code(code: str, renames: dict) -> str:
    code = rename(code, renames)
    code = convert_dble(code)
    code = INTEGER.sub(lambda m: "integer(ip)" + (m.group(1)[4:] if m.group(1) else ""), code)
    code = DOUBLE.sub(lambda m: "real(wp)" + (m.group(1) + " " * 8 if m.group(1) else ""), code)

    def lit(m):
        if m.group(1) is not None:
            return literal(m.group(1), m.group(2))
        return literal(m.group(3), m.group(4))
    return DLITERAL.sub(lit, code)


def code_only(text: str) -> str:
    """The text with literals, comments and directives blanked out."""
    parts = []
    map_code(text, lambda s: parts.append(s) or s)
    return " ".join(parts)


def add_use(text: str, path: Path) -> str:
    code = code_only(text)
    names = [n for n in ("ip", "wp") if re.search(rf"(?:\b|_){n}\b", code)]
    if not names:
        return text, None
    lines = text.split("\n")
    for i, line in enumerate(lines):
        if re.match(r"\s*use pepper_kinds\b", line):
            indent = line[:len(line) - len(line.lstrip())]
            lines[i] = f"{indent}use pepper_kinds, only: {', '.join(names)}"
            return "\n".join(lines), None
    for i, line in enumerate(lines):
        if line.lstrip().startswith(("!", "#")) or not line.strip():
            continue
        m = HEADER.match(line)
        if not m:
            raise ValueError(f"first statement is not a procedure statement: {line!r}")
        j = i
        while lines[j].split("!")[0].rstrip().endswith("&"):
            j += 1
        indent = m.group(1)
        lines.insert(j + 1, f"{indent}use pepper_kinds, only: {', '.join(names)}")
        return "\n".join(lines), j + 1
    raise ValueError("no procedure statement")


def convert_text(text: str, path: Path) -> str:
    return convert(text, path)[0]


def convert(text: str, path: Path):
    """Return (new text, index of the inserted use line or None)."""
    renames = RENAMES.get(path.name, {})
    return add_use(map_code(text, lambda s: convert_code(s, renames)), path)


def with_blank_line(text: str, index) -> str:
    """text with an empty line inserted at index (to keep line numbers equal
    to the converted text's when comparing object files)."""
    if index is None:
        return text
    lines = text.split("\n")
    lines.insert(index, "")
    return "\n".join(lines)


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

    files, old_texts = {}, {}
    for d in args.dirs:
        for p in sorted(d.resolve().iterdir()):
            if p.is_file() and p.suffix in SUFFIXES:
                old = p.read_text(encoding="ascii")
                try:
                    new, at = convert(old, p)
                except ValueError as e:
                    print(f"ERROR: {p}: {e}; nothing was changed")
                    return 1
                if new != old:
                    files[p] = new
                    # Static locals keep their names in the object file, so
                    # the old copy gets the renames too (checked unused below).
                    renames = RENAMES.get(p.name, {})
                    for name in renames.values():
                        if re.search(rf"\b{name}\b", code_only(old), re.I):
                            print(f"ERROR: {p}: rename target {name} already used")
                            return 1
                    renamed = map_code(old, lambda s: rename(s, renames))
                    old_texts[p] = with_blank_line(renamed, at)
    print(f"{len(files)} files to change")
    if not files:
        return 0

    if not args.no_compile:
        with tempfile.TemporaryDirectory() as tmp:
            mods = Path(tmp) / "mods"
            mods.mkdir()
            r = subprocess.run(["gfortran", "-c", str(KINDS_MODULE), "-J", str(mods),
                                "-o", str(mods / "pepper_kinds.o")],
                               capture_output=True, text=True)
            if r.returncode != 0:
                print("ERROR: cannot compile pepper_kinds:\n" + r.stderr)
                return 1
            failures = verify(files, Path(tmp), extra_flags=[f"-I{mods}"],
                              old_texts=old_texts)
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
