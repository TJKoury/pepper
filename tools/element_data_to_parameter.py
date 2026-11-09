#!/usr/bin/env python3
"""
Turn read-only tables initialized element by element in DATA statements into
named constants, in place.

    python tools/element_data_to_parameter.py FILE NAME ...

For each array NAME (declared on a line of its own, ``type name ( n )``),
the statements ``data name ( k ) / value /`` (k a literal or a parameter
plus a literal, e.g. ``ang + 3``, values possibly continued over lines) must
give every element exactly once. The declaration becomes a parameter array
with those values, and the DATA statements and the array's SAVE statement
are removed. Comments between the DATA statements stay where they are (they
often document the table's entries); a note before the first removed
statement points to the parameters. The arrays must not be assigned (this
is checked) and may only be read by the procedures they are passed to
(check by hand).
"""

import re
import sys
from pathlib import Path


def statement(lines, k):
    e = k
    while lines[e].rstrip().endswith("&"):
        e += 1
        while not lines[e].strip() or lines[e].lstrip().startswith("!"):
            e += 1
    return e


def value_of(expr: str, params: dict) -> int:
    expr = expr.strip()
    m = re.match(r"^(\w+)\s*\+\s*(\d+)$", expr)
    if m:
        return params[m.group(1).lower()] + int(m.group(2))
    if re.match(r"^\d+$", expr):
        return int(expr)
    return params[expr.lower()]


def convert(path: Path, names: list) -> None:
    lines = path.read_text(encoding="ascii").split("\n")
    params = {}
    # the parameters of the included files (old-style PARAMETER statements)
    for l in lines:
        im = re.match(r"^\s*include\s+'([^']+)'", l, re.I)
        if im and (path.parent / im.group(1)).exists():
            for il in (path.parent / im.group(1)).read_text().split("\n"):
                pm = re.match(r"^\s*parameter\s*\(\s*(\w+)\s*=\s*(\d+)\s*\)", il, re.I)
                if pm:
                    params[pm.group(1).lower()] = int(pm.group(2))
    for l in lines:
        m = re.match(r"^\s*integer(?:\(ip\))?\s*,\s*parameter\s*::\s*(\w+)\s*=\s*(.+?)\s*$", l, re.I)
        if m:
            try:
                params[m.group(1).lower()] = int(eval(re.sub(r"\b([a-z_]\w*)\b",
                                                             lambda x: str(params[x.group(1).lower()]),
                                                             m.group(2).lower())))
            except (KeyError, SyntaxError):
                pass
    text = "\n".join(l for l in lines if not l.lstrip().startswith("!"))
    first_data = None
    for name in names:
        if re.search(rf"^\s*{name}\s*\([^=]*\)\s*=(?!=)", text, re.M | re.I):
            sys.exit(f"element_data_to_parameter: {name} is assigned")
        dk = [i for i, l in enumerate(lines)
              if re.match(rf"^(\s*)(character\s*\(\s*\w+\s*\)|integer\(ip\)|real\(wp\)|logical)"
                          rf"\s+{name}\s*\(\s*(\w+)\s*\)\s*$", l, re.I)]
        if len(dk) != 1:
            sys.exit(f"element_data_to_parameter: {name}: no declaration of its own")
        dm = re.match(rf"^(\s*)(character\s*\(\s*\w+\s*\)|integer\(ip\)|real\(wp\)|logical)"
                      rf"\s+{name}\s*\(\s*(\w+)\s*\)\s*$", lines[dk[0]], re.I)
        typ, size = dm.group(2), dm.group(3)
        n = value_of(size, params)
        values, drop = {}, set()
        k = 0
        while k < len(lines):
            m = re.match(rf"^\s*data\s+{name}\s*\(([^)]*)\)\s*(/|&)", lines[k], re.I)
            if m:
                e = statement(lines, k)
                stmt = " ".join(l.rstrip().rstrip("&").strip() for l in lines[k:e + 1]
                                if not l.lstrip().startswith("!") and l.strip())
                vm = re.match(rf"^\s*data\s+{name}\s*\(([^)]*)\)\s*/(.*)/\s*$", stmt, re.I)
                idx = value_of(vm.group(1), params)
                if idx in values:
                    sys.exit(f"element_data_to_parameter: {name}({idx}) twice")
                values[idx] = vm.group(2).strip()
                drop |= {i for i in range(k, e + 1) if not lines[i].lstrip().startswith("!")}
                first_data = k if first_data is None else min(first_data, k)
                k = e + 1
            else:
                k += 1
        if sorted(values) != list(range(1, n + 1)):
            sys.exit(f"element_data_to_parameter: {name}: elements {sorted(values)} of 1:{n}")
        spec = f"[ {typ} :: " if typ.lower().startswith("character") else "[ "
        vals = [values[i] for i in range(1, n + 1)]
        new = [f"      {typ}, parameter :: {name} ( {size} ) = {spec}&"]
        for j, v in enumerate(vals):
            new.append(f"         {v}" + (",&" if j < n - 1 else " ]"))
        for i, l in enumerate(lines):
            if re.match(rf"^\s*save\s+{name}\s*$", l, re.I):
                drop.add(i)
        out = []
        for i, l in enumerate(lines):
            if i == dk[0]:
                out += new
            elif i == first_data and i in drop and name == names[-1]:
                out += ["!     PEPPER: the tables " + ", ".join(x.upper() for x in names) +
                        " are parameters (declared above);",
                        "!     the comments on their entries follow."]
            elif i not in drop:
                out.append(l)
        lines = out
        first_data = None if name != names[-1] else first_data
        text = "\n".join(l for l in lines if not l.lstrip().startswith("!"))
    path.write_text("\n".join(lines), encoding="ascii")


if __name__ == "__main__":
    convert(Path(sys.argv[1]), [n.lower() for n in sys.argv[2:]])
