#!/usr/bin/env python3
"""
Turn read-only tables into named constants, in place: a variable that is
initialized by DATA (and saved) but never changed becomes a parameter.

    python tools/data_to_parameter.py FILE ...

For each variable initialized in a DATA statement of the form
``data name / v1, v2, ... /`` (repeat counts ``n*v`` allowed), the variable
is converted if it is never assigned (``name =``, ``name(...) =``), never
read into, and never passed as an argument (a call could change it). Then

    real(wp)   power (0:3)                 real(wp), parameter :: power (0:3) = &
    save       power                  ->      [ 1.0_wp, 10.0_wp, 100.0_wp, 1000.0_wp ]
    data       power / 1.0_wp, ... /

Variables that do not qualify are reported and left alone. A variable that
is passed to procedures but only read (checked by hand) can be converted
with --force NAME. Character
values keep their declared length (``[character(n) :: ...]``).
"""

import argparse
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from modularize import raw_statements, parse_decl, split_top, entity_name  # noqa: E402


class SymbolicRepeat(str):
    """A DATA value repeated a number of times given by a named constant
    (``n*value``): the value, marked."""


def values_of(text: str) -> list:
    out = []
    for v in split_top(text):
        m = re.match(r"^(\d+)\s*\*\s*(.+)$", v)
        s = re.match(r"^([a-z_]\w*)\s*\*\s*(.+)$", v, re.I)
        if m:
            out.extend([m.group(2).strip()] * int(m.group(1)))
        elif s:
            out.append(SymbolicRepeat(s.group(2).strip()))
        else:
            out.append(v.strip())
    return out


def data_groups(text: str):
    """(names, values) pairs of 'data a / ... /, b / ... /'."""
    body = re.match(r"\s*data\s+(.*)$", text, re.I | re.S).group(1)
    pos, out = 0, []
    while pos < len(body):
        m = re.match(r"\s*,?\s*([^/]+?)\s*/", body[pos:])
        if not m:
            break
        names = [n.strip() for n in split_top(m.group(1))]
        start = pos + m.end()
        end = body.index("/", start)
        while body[start:end].count("'") % 2:      # a '/' inside a literal
            end = body.index("/", end + 1)
        out.append((names, values_of(body[start:end])))
        pos = end + 1
    return out


def modified(stmts, name: str) -> bool:
    for s in stmts:
        t = s.low
        if re.match(rf"\s*{name}\s*(\([^=]*\))?\s*=(?!=)", t):
            return True
        if re.match(r"\s*(call|read)\b", t) and re.search(rf"\b{name}\b", t):
            return True
        if re.search(rf"\b\w+\s*\([^()]*\b{name}\b", t) and not re.match(r"\s*(if|do|data|save|\w.*::)", t):
            # appears inside a function reference's argument list
            if re.search(rf"[(,]\s*{name}\s*[,)]", t):
                return True
    return False


def convert(path: Path, force=()) -> list:
    text = path.read_text(encoding="ascii")
    lines = text.split("\n")
    stmts = raw_statements(text)
    notes, decls, saves, datas = [], {}, {}, []
    for s in stmts:
        d = parse_decl(s.text)
        if d and d[1] is None:
            for e in d[2]:
                decls[entity_name(e)] = (s, d[0], e)
        elif re.match(r"\s*save\s+\w", s.text, re.I):
            for n in split_top(s.text.split(None, 1)[1]):
                saves[n.lower()] = s
        elif re.match(r"\s*data\b", s.text, re.I) and not re.match(r"\s*data\s*\(.*\)\s*(\(.*\))?\s*=", s.text, re.I):
            datas.append(s)       # (not an assignment to an array named DATA)
    edits = {}
    for s in datas:
        groups = data_groups(s.text)
        ok = True
        for names, vals in groups:
            if any(isinstance(v, SymbolicRepeat) for v in vals):
                ok = False         # the array length is not known here
                break
            if len(names) != 1 or not re.match(r"^\w+$", names[0]):
                ok = False
                break
            n = names[0].lower()
            if n not in decls or (n not in force and modified(stmts, n)):
                ok = False
                break
            ds, spec, ent = decls[n]
            if ds.first != ds.last or len(parse_decl(ds.text)[2]) != 1:
                ok = False
                break
        if not ok:
            notes.append(f"{path.name}: left alone: {s.text[:70]}")
            continue
        for names, vals in groups:
            n = names[0].lower()
            ds, spec, ent = decls[n]
            line = lines[ds.first]
            indent = line[:len(line) - len(line.lstrip())]
            is_array = "(" in ent
            if is_array:
                head = ""
                if spec.lower().startswith("character"):
                    length = re.match(r"character\s*(\(.*\))?", spec, re.I).group(1) or "(1)"
                    head = f"character{length} :: "
                rows, cur = [], ""
                for v in vals:
                    if cur and len(cur) + len(v) > 60:
                        rows.append(cur)
                        cur = ""
                    cur += v + ", "
                rows.append(cur.rstrip(", "))
                body = (", &\n" + indent + "     ").join(r.rstrip(", ") for r in rows)
                new = f"{indent}{spec}, parameter :: {ent} = &\n{indent}   [ {head}{body} ]"
            else:
                new = f"{indent}{spec}, parameter :: {ent} = {vals[0]}"
            edits[ds.first] = new
            if n in saves:
                ss = saves[n]
                rest = [x for x in split_top(ss.text.split(None, 1)[1]) if x.lower() != n]
                for i in range(ss.first, ss.last + 1):
                    edits[i] = None
                if rest:
                    edits[ss.first] = lines[ss.first][:len(lines[ss.first]) - len(lines[ss.first].lstrip())] \
                        + "save " + ", ".join(rest)
                saves = {k: (v if v is not ss else None) for k, v in saves.items()}
                saves = {k: v for k, v in saves.items() if v is not None}
        for i in range(s.first, s.last + 1):
            edits[i] = None
    if edits:
        out = []
        for i, l in enumerate(lines):
            if i in edits:
                if edits[i] is not None:
                    out.append(edits[i])
            else:
                out.append(l)
        path.write_text("\n".join(out), encoding="ascii")
        notes.insert(0, f"{path.name}: {sum(1 for v in edits.values() if v and 'parameter' in v)} "
                        "tables converted")
    return notes


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("files", nargs="+", type=Path)
    ap.add_argument("--force", action="append", default=[], metavar="NAME",
                    help="convert NAME although it is passed to procedures (checked by hand "
                         "to be read-only); repeatable")
    args = ap.parse_args()
    for p in args.files:
        for n in convert(p, {f.lower() for f in args.force}):
            print(n)
    return 0


if __name__ == "__main__":
    sys.exit(main())
