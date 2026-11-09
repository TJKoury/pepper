#!/usr/bin/env python3
"""
Fix the rank mismatches between actual and dummy arguments that
tools/fix_scalar_args.py leaves, using the error positions.

    pixi run python tools/fix_rank_args.py

SPICELIB relied on sequence association, which explicit interfaces do not
allow in two more forms:

- an array passed to a scalar dummy argument ("scalar and rank-1"): the
  routine gets the array's first element; the actual argument x becomes x(1)
  (x(1, 1) for a rank-2 array);
- a scalar passed to an array dummy argument that is an input ("rank-1 and
  scalar") outside a plain CALL statement (e.g. in a function reference in an
  IF): the actual argument x becomes [ x ].

The argument must be a plain name or literal at the position the compiler
reports; anything else is reported. Repeats until no such errors remain.
"""

import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
ERR = re.compile(r"^\./(?:\./)?(src/\S+?):(\d+):(\d+):\s*\n(?:.*\n){1,4}?Error: Rank mismatch in "
                 r"argument '(\w+)' at \(1\) \((scalar and rank-(\d)|rank-1 and scalar)\)", re.M)


def build() -> str:
    subprocess.run([sys.executable, str(ROOT / "tools" / "clean_stale.py")], cwd=ROOT,
                   capture_output=True)
    r = subprocess.run(["fpm", "build"], cwd=ROOT, capture_output=True, text=True)
    return r.stdout + r.stderr


def main() -> int:
    notes = []
    for _ in range(30):
        hits = ERR.findall(build())
        if not hits:
            break
        edits = {}
        for f, line, col, dummy, kind, rank in hits:
            edits.setdefault(f, set()).add((int(line), int(col), kind, rank))
        changed = 0
        for f, items in edits.items():
            p = ROOT / f
            lines = p.read_text().split("\n")
            # right to left within a line, so the columns stay valid
            for line, col, kind, rank in sorted(items, key=lambda x: (x[0], -x[1])):
                text = lines[line - 1]
                # (the reported column can be that of a blank before the argument)
                lead = len(text[col - 1:]) - len(text[col - 1:].lstrip())
                col += lead
                m = re.match(r"(\w+|-?[\d.]+(?:_wp)?|'[^']*')", text[col - 1:])
                if not m:
                    notes.append(f"{f}:{line}:{col}: not a name or literal: {text.strip()[:60]}")
                    continue
                tok = m.group(1)
                if kind.startswith("scalar"):
                    new = f"{tok}(1)" if rank == "1" else f"{tok}(" + ", ".join(["1"] * int(rank)) + ")"
                else:
                    new = f"[ {tok} ]"
                lines[line - 1] = text[:col - 1] + new + text[col - 1 + len(tok):]
                changed += 1
            p.write_text("\n".join(lines))
        if not changed:
            break
    for n in notes:
        print("NOTE: " + n)
    remaining = ERR.findall(build())
    print(f"{len(remaining)} rank-mismatch errors remain")
    return 0 if not remaining else 1


if __name__ == "__main__":
    sys.exit(main())
