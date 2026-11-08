#!/usr/bin/env python3
"""
Mark the procedures of PEPPER module files ``pure`` or ``elemental``, as far
as the compiler accepts it.

    python tools/mark_pure.py FILE ...       (run under pixi; uses fpm build)

Each procedure without a prefix yet is marked ``elemental`` if all its
arguments are scalars (and it has arguments), otherwise ``pure``. Then the
library is built with fpm; a procedure in which the compiler reports an
error is downgraded (elemental -> pure -> no prefix) and the build is
repeated until it succeeds. The compiler checks all the rules for pure
procedures (no saved state, no I/O, only pure calls, intent(in) arguments
for functions, ...), so what remains marked is pure.
"""

import argparse
import re
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from add_intents import procedures  # noqa: E402
from modularize import HEADER, parse_decl, raw_statements, entity_name, SPEC_START  # noqa: E402

ROOT = Path(__file__).resolve().parent.parent
PREFIXES = ("elemental", "pure", "")
ERROR = re.compile(r"^\./(?:\./)?(src/\S+?\.[fF]90):(\d+):\d+:\s*$")


def all_scalar(lines: list, first: int, last: int) -> bool:
    stmts = raw_statements("\n".join(lines[first:last + 1]))
    head = HEADER.match(stmts[0].text)
    args = [a.strip().lower() for a in (head.group("args") or "").split(",") if a.strip()]
    if not args:
        return False
    for s in stmts[1:]:
        if not (SPEC_START.match(s.text) or parse_decl(s.text)):
            break
        if re.match(r"\s*external\b", s.text, re.I):
            if any(entity_name(e) in args for e in s.text.split()[1:]):
                return False
        decl = parse_decl(s.text)
        if decl:
            if decl[1] and "dimension" in decl[1].lower():
                return False
            for e in decl[2]:
                if entity_name(e) in args and "(" in e.split("=")[0]:
                    return False
    return True


def set_prefix(lines: list, first: int, prefix: str) -> None:
    line = lines[first]
    body = re.sub(r"^(\s*)(?:(?:elemental|pure)\s+)", r"\1", line)
    indent = body[:len(body) - len(body.lstrip())]
    lines[first] = indent + (prefix + " " if prefix else "") + body.lstrip()


def current_prefix(line: str) -> str:
    m = re.match(r"\s*(elemental|pure)\b", line)
    return m.group(1) if m else ""


def build() -> str:
    r = subprocess.run(["fpm", "build"], cwd=ROOT, capture_output=True, text=True)
    return "" if r.returncode == 0 else r.stdout + r.stderr


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("files", nargs="+", type=Path)
    args = ap.parse_args()
    files = {p.resolve(): p.read_text(encoding="ascii").split("\n") for p in args.files}

    # Initial marking.
    for path, lines in files.items():
        for first, last in procedures(lines):
            if not current_prefix(lines[first]):
                set_prefix(lines, first, "elemental" if all_scalar(lines, first, last) else "pure")
        path.write_text("\n".join(lines), encoding="ascii")

    for round_ in range(1, 100):
        out = build()
        if not out:
            break
        hits = {}
        for line in out.splitlines():
            m = ERROR.match(line)
            if m:
                hits.setdefault((ROOT / m.group(1)).resolve(), set()).add(int(m.group(2)))
        if not any(p in files for p in hits):
            print(out[-3000:])
            print("ERROR: build fails outside the marked files")
            return 1
        n = 0
        for path, nums in hits.items():
            if path not in files:
                continue
            lines = files[path]
            procs = list(procedures(lines))
            for first, last in procs:
                if any(first <= k - 1 <= last for k in nums):
                    p = current_prefix(lines[first])
                    if p:
                        set_prefix(lines, first, PREFIXES[PREFIXES.index(p) + 1])
                        n += 1
            path.write_text("\n".join(lines), encoding="ascii")
        print(f"round {round_}: downgraded {n} procedures", flush=True)
        if n == 0:
            print(out[-3000:])
            print("ERROR: errors that downgrading does not fix")
            return 1

    for path, lines in files.items():
        counts = {p: 0 for p in PREFIXES}
        for first, _ in procedures(lines):
            counts[current_prefix(lines[first])] += 1
        print(f"{path.name}: elemental {counts['elemental']}, pure {counts['pure']}, "
              f"neither {counts['']}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
