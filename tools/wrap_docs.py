#!/usr/bin/env python3
"""
Wrap long FORD documentation comments, in place:

    python tools/wrap_docs.py [--width N] FILE_OR_DIR ...     (default width 100)

- A ``!>`` comment line longer than the width is split into several ``!>``
  lines with the same indentation.
- A trailing ``!!`` comment (after a declaration) that makes the line
  longer than the width continues on following lines, as ``!!`` comments
  aligned under the first one.

Only comment text is moved; code is not changed. Running it again changes
nothing.
"""

import argparse
import sys
import textwrap
from pathlib import Path

SUFFIXES = (".f90", ".F90")


def wrap_text(text: str, width: int) -> str:
    out = []
    for line in text.split("\n"):
        if len(line) <= width:
            out.append(line)
            continue
        stripped = line.lstrip()
        indent = line[:len(line) - len(stripped)]
        if stripped.startswith("!>"):
            body = stripped[2:].strip()
            out += [f"{indent}!> {t}" for t in
                    textwrap.wrap(body, max(20, width - len(indent) - 3))]
            continue
        k = line.find("  !! ")
        if k > 0 and "'" not in line[:k] and '"' not in line[:k]:
            code, doc = line[:k], line[k + 5:].strip()
            col = len(code) + 2
            parts = textwrap.wrap(doc, max(20, width - col - 3))
            if parts:
                out.append(f"{code}  !! {parts[0]}")
                out += [" " * col + f"!! {p}" for p in parts[1:]]
                continue
        out.append(line)
    return "\n".join(out)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("paths", nargs="+", type=Path)
    ap.add_argument("--width", type=int, default=100)
    args = ap.parse_args()
    n = 0
    for p in args.paths:
        files = [f for f in sorted(p.rglob("*")) if f.suffix in SUFFIXES] if p.is_dir() else [p]
        for f in files:
            old = f.read_text(encoding="ascii")
            new = wrap_text(old, args.width)
            if new != old:
                f.write_text(new, encoding="ascii")
                n += 1
    print(f"wrapped comments in {n} files")
    return 0


if __name__ == "__main__":
    sys.exit(main())
