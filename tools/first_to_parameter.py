#!/usr/bin/env python3
"""
Replace constants computed on the first call with named constants, in
place:

    integer(ip) icomma                     integer(ip), parameter :: icomma = ichar( ',' )
    logical first                     ->
    save / data first / .true. /           (the IF block and FIRST are gone)
    if ( first ) then
       first  = .false.
       icomma = ichar( ',' )
    end if

    python tools/first_to_parameter.py [--dry-run] FILE ...

A FIRST block qualifies when its condition is a saved logical that is
initialized to .true. by DATA and set only to .false. (inside such
blocks), and every other statement of the block is an assignment
``name = expression`` where

- name is a local scalar variable (not an argument) assigned nowhere else
  (and the same expression if several blocks, e.g. in several ENTRY
  points, assign it), and
- the expression uses only literals, ``ichar('c')``/``char(n)``, the
  constant functions of pepper_constants (``intmax()`` -> ``intmax``),
  parameters, and names assigned earlier in the block.

Blocks with anything else (loops, calls, array elements) are reported and
left alone. The new parameters are declared at the end of the
specification part, in the order of the block.
"""

import argparse
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from modularize import raw_statements, parse_decl, split_top, entity_name, SPEC_START  # noqa: E402
from modules import CONSTANTS  # noqa: E402

INTRINSICS = {"ichar", "char", "iachar", "achar", "huge", "tiny", "epsilon", "real", "int",
              "dble", "sqrt", "acos", "asin", "atan", "abs", "mod", "max", "min", "len"}


def block_end(stmts, k):
    depth = 0
    for j in range(k, len(stmts)):
        t = stmts[j].low
        if re.match(r"if\s*\(.*\)\s*then$", t) or re.match(r"(do|select)\b", t):
            depth += 1
        elif re.match(r"end\s*(if|do|select)$", t):
            depth -= 1
            if depth == 0:
                return j
    return None


def convert(path: Path, dry_run: bool) -> list:
    text = path.read_text(encoding="ascii")
    lines = text.split("\n")
    stmts = raw_statements(text)
    head = re.match(r"^(?:[\w*()\s,]*?\s)?(subroutine|function)\s+(\w+)\s*(?:\(([^()]*)\))?",
                    stmts[0].text, re.I)
    if not head:
        return []
    args = {a.strip().lower() for a in (head.group(3) or "").split(",") if a.strip()}
    entries = set()
    for s in stmts:
        m = re.match(r"entry\s+\w+\s*\(([^()]*)\)", s.text, re.I)
        if m:
            entries |= {a.strip().lower() for a in m.group(1).split(",") if a.strip()}
    args |= entries
    spec_end = 0
    decls, params, saved_all, saves, datas = {}, set(), False, {}, []
    for k, s in enumerate(stmts[1:], 1):
        if not (SPEC_START.match(s.text) or parse_decl(s.text)
                or re.match(r"\s*(save|data|entry)\b", s.text, re.I)):
            if not re.match(r"\s*entry\b", s.text, re.I):
                break
        spec_end = k
        d = parse_decl(s.text)
        if d:
            for e in d[2]:
                decls[entity_name(e)] = (s, d, e)
            if d[1] and "parameter" in d[1].lower():
                params |= {entity_name(e) for e in d[2]}
        if re.match(r"\s*parameter\b", s.text, re.I):
            m = re.match(r"\s*parameter\s*\((.*)\)\s*$", s.text, re.I | re.S)
            params |= {entity_name(e) for e in split_top(m.group(1))}
        if re.match(r"\s*save\s*$", s.text, re.I):
            saved_all = True
        elif re.match(r"\s*save\b", s.text, re.I):
            for n in split_top(s.text.split(None, 1)[1]):
                saves[n.lower()] = s
        if re.match(r"\s*data\b", s.text, re.I):
            datas.append(s)
    # Find the FIRST flags and their blocks.
    blocks, notes = [], []
    for k, s in enumerate(stmts):
        m = re.match(r"if\s*\(\s*(\w+)\s*\)\s*then$", s.text, re.I)
        if not m:
            continue
        flag = m.group(1).lower()
        if flag not in decls or not decls[flag][1][0].lower().startswith("logical"):
            continue
        if not (saved_all or flag in saves):
            continue
        if not any(re.search(rf"\b{flag}\s*/\s*\.true\.\s*/", d.low) for d in datas):
            continue
        e = block_end(stmts, k)
        if e is None:
            continue
        blocks.append((flag, k, e))
    if not blocks:
        return []
    flags = {b[0] for b in blocks}
    if len(flags) != 1:
        return [f"{path.name}: several first flags; left alone"]
    flag = flags.pop()
    # The flag must only be set to .false. inside the blocks, and only tested
    # by them.
    inside = {j for _, k, e in blocks for j in range(k, e + 1)}
    def code(s):
        return re.sub(r"'[^']*'|\"[^\"]*\"", "''", s.low)

    for j, s in enumerate(stmts):
        if j in inside or not re.search(rf"\b{flag}\b", code(s)):
            continue
        if parse_decl(s.text) or re.match(r"\s*(save|data)\b", s.text, re.I):
            continue
        return [f"{path.name}: {flag} is used outside its blocks; left alone"]
    assigned, order = {}, []
    for _, k, e in blocks:
        for j in range(k + 1, e):
            t = stmts[j].text.strip()
            if re.match(rf"{flag}\s*=\s*\.false\.$", t, re.I):
                continue
            m = re.match(r"(\w+)\s*=\s*(.+)$", t)
            if not m:
                return [f"{path.name}: block has '{t[:50]}'; left alone"]
            name, expr = m.group(1).lower(), m.group(2).strip()
            if name in args or name not in decls or "(" in decls[name][2]:
                return [f"{path.name}: block assigns {name} (not a local scalar); left alone"]
            words = set(re.findall(r"[a-z_]\w*", re.sub(r"'[^']*'", "''", expr.lower())))
            words -= {"wp", "ip"}
            ok = all(w in INTRINSICS or w in CONSTANTS or w in params or w in assigned
                     for w in words)
            if not ok:
                return [f"{path.name}: {name} = {expr[:40]} is not constant; left alone"]
            if name in assigned and assigned[name] != expr:
                return [f"{path.name}: {name} gets different values; left alone"]
            if name not in assigned:
                assigned[name] = expr
                order.append(name)
    # Assigned nowhere else?
    for j, s in enumerate(stmts):
        if j in inside:
            continue
        for n in order:
            if re.match(rf"\s*{n}\s*=(?!=)", code(s)) or re.match(r"\s*(call|read)\b", code(s)) and \
                    re.search(rf"[(,]\s*{n}\s*[,)]", code(s)):
                return [f"{path.name}: {n} is also set outside the first block; left alone"]
    if dry_run:
        return [f"{path.name}: would convert {', '.join(order)}"]
    consts = set()
    drop = set()                       # line indices to delete
    for _, k, e in blocks:
        drop |= set(range(stmts[k].first, stmts[e].last + 1))
    edits = {}
    for n in order + [flag]:
        s, d, ent = decls[n]
        if len(d[2]) == 1:
            drop |= set(range(s.first, s.last + 1))
        else:
            if s.first != s.last:
                return [f"{path.name}: continued declaration of {n}; left alone"]
            keep = [x for x in d[2] if entity_name(x) not in set(order) | {flag}]
            line = lines[s.first]
            indent = line[:len(line) - len(line.lstrip())]
            edits[s.first] = f"{indent}{d[0]} " + ", ".join(keep)
    for n, s in saves.items():
        if n in set(order) | {flag}:
            rest = [x for x in split_top(s.text.split(None, 1)[1])
                    if x.lower() not in set(order) | {flag}]
            if rest:
                line = lines[s.first]
                edits[s.first] = line[:len(line) - len(line.lstrip())] + "save " + ", ".join(rest)
            else:
                drop |= set(range(s.first, s.last + 1))
    gone = set(order) | {flag}
    for s in datas:
        groups = [g.lower() for g in re.findall(r"(\w+)\s*/[^/]*/", s.text)]
        if groups and all(g in gone for g in groups):
            drop |= set(range(s.first, s.last + 1))
        elif any(g in gone for g in groups):
            return [f"{path.name}: a DATA statement mixes converted and other variables; left alone"]
    new_decls = []
    last_spec = stmts[spec_end].last
    indent = "      "
    for n in order:
        expr = assigned[n]
        for c in CONSTANTS:
            if re.search(rf"\b{c}\s*\(\s*\)", expr, re.I):
                consts.add(c)
                expr = re.sub(rf"\b{c}\s*\(\s*\)", c, expr, flags=re.I)
        spec = decls[n][1][0]
        new_decls.append(f"{indent}{spec}, parameter :: {n} = {expr}")
    # The constant functions used are now module constants: remove their
    # declarations (type declarations and EXTERNAL statements).
    for s in stmts[1:spec_end + 1]:
        kw = re.match(r"\s*external\b", s.text, re.I)
        d = None if kw else parse_decl(s.text)
        ents = split_top(s.text[kw.end():]) if kw else (d[2] if d else [])
        if not ents or not any(entity_name(e) in consts for e in ents):
            continue
        keep = [e for e in ents if entity_name(e) not in consts]
        if not keep:
            drop |= set(range(s.first, s.last + 1))
        elif s.first == s.last:
            line = lines[s.first]
            ind = line[:len(line) - len(line.lstrip())]
            edits[s.first] = f"{ind}external " + ", ".join(keep) if kw else f"{ind}{d[0]} " + ", ".join(keep)
        else:
            return [f"{path.name}: continued declaration of a constant function; left alone"]
    out = []
    use_done = False
    for i, line in enumerate(lines):
        if i not in drop:
            out.append(edits.get(i, line))
            if not use_done and consts and re.match(r"\s*use pepper_kinds\b", line, re.I):
                out.append(f"{indent}use pepper_constants, only: {', '.join(sorted(consts))}")
                use_done = True
        if i == last_spec:
            out.append("!")
            out.append("!     Constants (SPICELIB computed them on the first call).")
            out.append("!")
            out += new_decls
    if consts and not use_done:
        return [f"{path.name}: no 'use pepper_kinds' line for the constants; left alone"]
    path.write_text("\n".join(out), encoding="ascii")
    return [f"{path.name}: converted {', '.join(order)}"]


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("files", nargs="+", type=Path)
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()
    for p in args.files:
        for n in convert(p, args.dry_run):
            print(n)
    return 0


if __name__ == "__main__":
    sys.exit(main())
