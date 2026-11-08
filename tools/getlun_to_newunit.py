#!/usr/bin/env python3
"""
Replace SPICELIB's unit bookkeeping before an OPEN by NEWUNIT=, in place:

    call getlun ( unit )                   open ( newunit = unit, &
    open ( unit = unit, &           ->            file = ..., ... )
           file = ..., ... )

    python tools/getlun_to_newunit.py FILE ...

A ``call getlun ( X )`` is replaced only if the next statement is an OPEN
with ``unit = X`` (``unit=X``); the OPEN gets ``newunit = X`` and the GETLUN
call is removed (a PEPPER comment notes it). NEWUNIT= gives a unit that no
other open file uses, atomically, so it is also safe when several threads
open files. Other uses of GETLUN are reported and left alone.
"""

import argparse
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from modularize import raw_statements  # noqa: E402


def convert(path: Path) -> list:
    text = path.read_text(encoding="ascii")
    lines = text.split("\n")
    stmts = raw_statements(text)
    notes, drop, n = [], set(), 0
    for k, s in enumerate(stmts):
        m = re.match(r"call\s+getlun\s*\(\s*(\w+(?:\s*\([^()]*\))?)\s*\)$", s.text, re.I)
        if not m:
            continue
        var = re.sub(r"\s+", "", m.group(1)).lower()
        nxt = stmts[k + 1] if k + 1 < len(stmts) else None
        pat = re.compile(r"(\bopen\s*\(\s*)unit(\s*=\s*)" + re.escape(var).replace(r"\(", r"\s*\(\s*")
                         + r"(\s*,)", re.I)
        if nxt is None or not re.match(r"open\b", nxt.text, re.I):
            notes.append(f"{path.name}:{s.first + 1}: GETLUN not followed by OPEN; left alone")
            continue
        first = lines[nxt.first]
        squeezed = re.sub(r"\s+", "", first).lower()
        if not re.match(rf"open\(unit={re.escape(var)},", squeezed):
            notes.append(f"{path.name}:{s.first + 1}: OPEN does not use unit = {var}; left alone")
            continue
        new = re.sub(r"(?i)\bunit(\s*)=", r"newunit\1=", first, count=1)
        lines[nxt.first] = new
        indent = lines[s.first][:len(lines[s.first]) - len(lines[s.first].lstrip())]
        lines[s.first] = "!     PEPPER: the unit comes from NEWUNIT= in the OPEN below."
        for i in range(s.first + 1, s.last + 1):
            drop.add(i)
        n += 1
    if n:
        path.write_text("\n".join(l for i, l in enumerate(lines) if i not in drop), encoding="ascii")
        notes.insert(0, f"{path.name}: {n} GETLUN calls replaced")
    return notes


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("files", nargs="+", type=Path)
    args = ap.parse_args()
    for p in args.files:
        for note in convert(p):
            print(note)
    return 0


if __name__ == "__main__":
    sys.exit(main())
