#!/usr/bin/env python3
"""
Replace old Fortran syntax with the modern forms, in place:

    character*(*)    ->  character(*)        (obsolescent form)
    character*(n)    ->  character(n)
    character*80     ->  character(80)
    .eq. .ne. .lt. .le. .gt. .ge.  ->  ==  /=  <  <=  >  >=

    python tools/modernize_syntax.py [DIR ...]     (default: src)

Applies to *.f90, *.F90 and *.inc, recursively. Character literals,
comments and preprocessor directives are not changed. The column of what
follows is kept where the blanks allow it. Running it again changes nothing.

Verify by building before and after: the object files must be identical
(the change keeps line numbers).
"""

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from lowercase_code import map_code  # noqa: E402

SUFFIXES = (".f90", ".F90", ".inc")
RELATIONAL = {"eq": "==", "ne": "/=", "lt": "<", "le": "<=", "gt": ">", "ge": ">="}
RELOP = re.compile(r"\.(eq|ne|lt|le|gt|ge)\.", re.I)
STAR = re.compile(r"\b(character)\s*\*\s*(\((?:[^()]|\([^()]*\))*\)|\d+)( *)", re.I)


def fix(code: str) -> str:
    def sub(m):
        length = m.group(2)
        new = f"{m.group(1)}({length[1:-1].strip() if length[0] == '(' else length})"
        old_len = m.end(2) - m.start(1)
        blanks = m.group(3)
        if len(blanks) >= 2:
            # Keep the column of what follows (at least one blank).
            blanks = " " * max(1, len(blanks) + old_len - len(new))
        return new + blanks
    code = STAR.sub(sub, code)
    return RELOP.sub(lambda m: RELATIONAL[m.group(1).lower()], code)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("dirs", nargs="*", type=Path, default=[ROOT / "src"])
    args = ap.parse_args()
    n = 0
    for d in args.dirs:
        for p in sorted(d.rglob("*")):
            if p.is_file() and p.suffix in SUFFIXES:
                old = p.read_text(encoding="ascii")
                new = map_code(old, fix)
                if new != old:
                    p.write_text(new, encoding="ascii")
                    n += 1
    print(f"changed {n} files")
    return 0


if __name__ == "__main__":
    sys.exit(main())
