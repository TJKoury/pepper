#!/usr/bin/env python3
"""
Turn read-only tables initialized by an implied-DO DATA statement into named
constants, in place.

    python tools/implied_do_to_parameter.py FILE ...

A statement such as

    data ( names(i), lb(i), ub(i), i = 1, n ) / 'A', 1, 2,  'B', 3, 4 /

initializes several arrays row by row. Each array becomes a parameter whose
values are the corresponding column:

    character(8), parameter :: names ( n ) = [ character(8) :: 'A', 'B' ]
    integer(ip), parameter :: lb ( n ) = [ 1, 3 ]
    ...

The arrays must be declared on lines of their own (``type name ( dims )``)
and saved with SAVE statements of their own, which are removed. An array is
converted only if it is never assigned or read into (``name(...) =``,
``read``); being passed to a procedure is not checked (check by hand that
the procedures only read it). Arrays that do not qualify are reported, and
the statement is left alone.
"""

import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from modularize import split_top  # noqa: E402

DATA = re.compile(r"^      data\s*\((?P<items>.*?),\s*(?P<var>\w+)\s*=\s*1\s*,\s*(?P<n>\w+)\s*\)\s*/"
                  r"(?P<values>.*?)/\s*$", re.S | re.M | re.I)


def statement_at(lines, k):
    """The (first, last) line index of the statement starting at line k."""
    e = k
    while lines[e].rstrip().endswith("&"):
        e += 1
        # blank and comment lines may come between continuation lines
        while not lines[e].strip() or lines[e].lstrip().startswith("!"):
            e += 1
    return k, e


def joined(lines, a, b) -> str:
    return " ".join(l.rstrip().rstrip("&").strip() for l in lines[a:b + 1]
                    if not l.lstrip().startswith("!"))


def convert(path: Path) -> list:
    lines = path.read_text(encoding="ascii").split("\n")
    notes = []
    k = 0
    while k < len(lines):
        if not re.match(r"\s*data\s*\(", lines[k], re.I):
            k += 1
            continue
        a, b = statement_at(lines, k)
        stmt = joined(lines, a, b)
        m = re.match(r"\s*data\s*\((.*),\s*(\w+)\s*=\s*1\s*,\s*(\w+)\s*\)\s*/(.*)/\s*$", stmt, re.I)
        if not m:
            notes.append(f"{path.name}:{a + 1}: not a simple implied-DO DATA statement")
            k = b + 1
            continue
        var, n = m.group(2), m.group(3)
        names = []
        for item in split_top(m.group(1)):
            im = re.match(rf"\s*(\w+)\s*\(\s*{var}\s*\)\s*$", item, re.I)
            if not im:
                names = None
                break
            names.append(im.group(1))
        if not names:
            notes.append(f"{path.name}:{a + 1}: items are not name({var})")
            k = b + 1
            continue
        values = [v.strip() for v in split_top(m.group(4))]
        if len(values) % len(names):
            notes.append(f"{path.name}:{a + 1}: {len(values)} values for {len(names)} arrays")
            k = b + 1
            continue
        text = "\n".join(l for l in lines if not l.lstrip().startswith("!"))
        bad = [x for x in names if re.search(rf"^\s*{x}\s*(\([^=]*\))?\s*=(?!=)", text, re.M | re.I)
               or re.search(rf"^\s*read\b.*\b{x}\b", text, re.M | re.I)]
        decls = {}
        for x in names:
            for i, l in enumerate(lines):
                dm = re.match(rf"^(\s*)(character\s*\(\s*\w+\s*\)|integer\(ip\)|real\(wp\)|logical)"
                              rf"\s+{x}\s*(\([^)]*\))\s*$", l, re.I)
                if dm:
                    decls[x] = (i, dm.group(2), dm.group(3))
        missing = [x for x in names if x not in decls]
        if bad or missing:
            notes.append(f"{path.name}:{a + 1}: left alone (assigned: {bad}, no own declaration: "
                         f"{missing})")
            k = b + 1
            continue
        cols = {x: values[j::len(names)] for j, x in enumerate(names)}
        # replace the declarations (last first, so indices stay valid)
        new = {}
        for x in names:
            i, typ, dims = decls[x]
            spec = f"[ {typ} :: " if typ.lower().startswith("character") else "[ "
            vals = cols[x]
            rows = [", ".join(vals[r:r + 4]) for r in range(0, len(vals), 4)]
            new[i] = ([f"      {typ}, parameter :: {x} {dims} = {spec}&"] +
                      [f"         {r}" + (",&" if j < len(rows) - 1 else " ]")
                       for j, r in enumerate(rows)])
        drop = set(range(a, b + 1))
        for i, l in enumerate(lines):
            sm = re.match(r"^\s*save\s+(\w+)\s*$", l, re.I)
            if sm and sm.group(1).lower() in [x.lower() for x in names]:
                drop.add(i)
        out = []
        for i, l in enumerate(lines):
            if i in new:
                out += new[i]
            elif i not in drop:
                out.append(l)
        lines = out
        notes.append(f"{path.name}: converted {', '.join(names)}")
        k = 0
    path.write_text("\n".join(lines), encoding="ascii")
    return notes


def main() -> int:
    for f in sys.argv[1:]:
        for n in convert(Path(f)):
            print(n)
    return 0


if __name__ == "__main__":
    sys.exit(main())
