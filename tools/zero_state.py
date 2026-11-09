#!/usr/bin/env python3
"""
Make the init routines of the state types set every component first.

    python tools/zero_state.py [FILE ...]   (default: src/pepper_state_*.f90)

SPICELIB keeps saved variables in static storage, which is zero in practice
(.bss), and some of its code relies on that (e.g. ZZNRDDP's PE0, PINC0, ...,
never assigned). The state types are allocated, and allocated memory is not
zeroed; so each init_<name>_state sets every component first (numbers to
zero, logicals to .false., characters to blanks) and then the initial values
of SPICELIB's DATA statements. Idempotent (the zeroing block is marked).
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MARK = "      ! (every component first: SPICELIB's saved variables start as zero)"


def zeroing(types_text: str) -> list:
    out = [MARK]
    for line in types_text.split("\n"):
        m = re.match(r"\s*(integer|real|logical|character)\b[^:]*::\s*(\w+)", line, re.I)
        if not m:
            continue
        kind, name = m.group(1).lower(), m.group(2)
        value = {"integer": "0", "real": "0.0_wp", "logical": ".false.", "character": "' '"}[kind]
        out.append(f"      state%{name} = {value}")
    return out


def process(path: Path) -> bool:
    t = path.read_text(encoding="ascii")
    if MARK in t:
        return False
    changed = False
    for m in re.finditer(r"   type, public :: (\w+)\n(.*?)   end type \1\n", t, re.S):
        tn, body = m.group(1), m.group(2)
        hm = re.search(rf"(   (?:pure )?subroutine init_{tn}\(state\)\n      type\({tn}\), intent\(inout\) :: state  !! the state\n)", t)
        if not hm:
            continue
        t = t.replace(hm.group(1), hm.group(1) + "\n".join(zeroing(body)) + "\n", 1)
        changed = True
    if changed:
        path.write_text(t, encoding="ascii")
    return changed


def main() -> int:
    files = [Path(f) for f in sys.argv[1:]] or sorted((ROOT / "src").glob("pepper_state_*.f90"))
    n = sum(process(f) for f in files)
    print(f"zero_state: {n} files changed")
    return 0


if __name__ == "__main__":
    sys.exit(main())
