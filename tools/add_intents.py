#!/usr/bin/env python3
"""
Add INTENT attributes to the dummy arguments of the procedures in PEPPER
module files, taken from each procedure's NAIF "$ Brief_I/O" table
(I -> intent(in), O -> intent(out), I/O -> intent(inout)).

    python tools/add_intents.py [--dry-run] FILE ...

A declaration such as ``real(wp)   v1 ( 3 )`` of an argument becomes
``real(wp), intent(in) :: v1 ( 3 )``. Declarations that also declare
non-arguments are split. An argument without a table entry, or without a
declaration, is an error (fix the table or the declaration by hand). Run it
it again: procedures whose arguments all have intents are left alone.
"""

import argparse
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from modularize import HEADER, parse_decl, raw_statements, entity_name, SPEC_START  # noqa: E402

ROW = re.compile(r"^!\s{2,}([A-Za-z]\w*(?:\s*,\s*[A-Za-z]\w*)*)\s*,?\s+(I/O|I-O|I,O|I|O)(\s|$)")
PENDING = re.compile(r"^!\s{2,}([A-Za-z]\w*(?:\s*,\s*[A-Za-z]\w*)*)\s*,\s*$")
INTENT = {"I": "in", "O": "out", "I/O": "inout", "I-O": "inout", "I,O": "inout"}


def procedures(lines: list):
    """(first, last) line indices of each module procedure."""
    i = 0
    while i < len(lines):
        line = lines[i]
        m = None if line.lstrip().startswith("!") else HEADER.match(line)
        if m and m.group("name") and not line.lstrip().lower().startswith("end"):
            name = m.group("name").lower()
            k = i
            end = re.compile(rf"\s*end\s+{m.group('kind')}\s+{name}\s*$", re.I)
            while not end.match(lines[k]):
                k += 1
            yield i, k
            i = k + 1
        else:
            i += 1


def has_brief_io(body: list) -> bool:
    """The header has a Brief_I/O table with at least one row."""
    inside = False
    for line in body:
        if line.startswith("! $ "):
            if inside:
                return False
            inside = line.startswith("! $ Brief_I/O")
            continue
        if inside and ROW.match(line):
            return True
    return False


DETAIL = re.compile(r"^!\s{1,8}([A-Za-z]\w*)(?:\s{2,}(\S.*))?\s*$")


def detailed(body: list) -> dict:
    """For a header without a Brief_I/O table: argument name (lowercase) ->
    (intent, description) from the Detailed_Input and Detailed_Output
    sections (an entry is a name at the start of a comment line, followed by
    its text; the description is the entry's first paragraph). A name in
    both sections is intent(inout)."""
    found = {}
    for title, intent in (("Detailed_Input", "in"), ("Detailed_Output", "out")):
        inside, name, col = False, None, None
        for line in body:
            if line.startswith("! $ "):
                if inside:
                    break
                inside = line.startswith("! $ " + title)
                continue
            if not inside:
                continue
            m = DETAIL.match(line)
            if m and m.group(1).isupper():
                name = m.group(1).lower()
                prev = found.get(name)
                desc = [m.group(2).strip()] if m.group(2) else []
                found[name] = ("inout" if prev and prev[0] != intent else intent, prev[1] if prev else desc)
                col = line.index(m.group(2)) if m.group(2) else None
                continue
            if name and line.strip() == "!":
                if found[name][1]:
                    name = None
                continue
            if name:
                text = line[1:].strip()
                if found[name][1] is not None and text:
                    found[name][1].append(text)
    return {n: (i, " ".join(d)) for n, (i, d) in found.items()}


def table(body: list) -> dict:
    if not has_brief_io(body):
        return {n: i for n, (i, d) in detailed(body).items()}
    out, pending, inside = {}, [], False
    for line in body:
        if line.startswith("! $ "):
            if inside:
                break
            inside = line.startswith("! $ Brief_I/O")
            continue
        if not inside:
            continue
        r = ROW.match(line)
        if r:
            for n in pending + re.split(r"\s*,\s*", r.group(1)):
                out[n.lower()] = INTENT[r.group(2)]
            pending = []
            continue
        p = PENDING.match(line)
        pending = pending + re.split(r"\s*,\s*", p.group(1)) if p else []
    return out


def add_intents(lines: list, first: int, last: int, errors: list) -> list:
    body = lines[first:last + 1]
    stmts = raw_statements("\n".join(body))
    head = HEADER.match(stmts[0].text)
    name = head.group("name").lower()
    args = [a.strip().lower() for a in (head.group("args") or "").split(",") if a.strip()]
    if not args:
        return body
    have = set()
    for s in stmts[1:]:
        d = parse_decl(s.text)
        if d and d[1] and "intent" in d[1].lower():
            have.update(entity_name(e) for e in d[2])
    if all(a in have for a in args):
        return body          # done already
    intents = table(body)
    missing = [a for a in args if a not in intents]
    if missing:
        errors.append(f"{name}: no Brief_I/O entry for {', '.join(missing)}")
        return body
    declared = set()
    out = list(body)
    for s in stmts[1:]:
        if not (SPEC_START.match(s.text) or parse_decl(s.text)):
            break
        decl = parse_decl(s.text)
        if not decl:
            continue
        spec, attrs, ents = decl
        if attrs and "intent" in attrs.lower():
            declared.update(entity_name(e) for e in ents)
            continue
        dummies = [e for e in ents if entity_name(e) in args]
        if not dummies:
            continue
        if s.first != s.last:
            errors.append(f"{name}: continued declaration of arguments: {s.text}")
            return body
        line = out[s.first]
        indent = line[:len(line) - len(line.lstrip())]
        new = []
        others = [e for e in ents if entity_name(e) not in args]
        if others:
            a = f"{spec}, {attrs} :: " if attrs else (f"{spec} :: " if attrs is not None else f"{spec} ")
            new.append(indent + a + ", ".join(others))
        groups = {}
        for e in dummies:
            it = intents[entity_name(e)]
            # A cell's control area (its size) is an input even if the cell
            # is an output: intent(inout), not intent(out).
            if it == "out" and re.search(r"\(\s*lbcell\s*:", e, re.I):
                it = "inout"
            groups.setdefault(it, []).append(e)
            declared.add(entity_name(e))
        for intent in ("in", "inout", "out"):
            if intent in groups:
                a = f"{spec}, {attrs}, intent({intent}) :: " if attrs else f"{spec}, intent({intent}) :: "
                new.append(indent + a + ", ".join(groups[intent]))
        out[s.first] = "\n".join(new)
    undeclared = [a for a in args if a not in declared]
    if undeclared:
        errors.append(f"{name}: arguments without a declaration: {', '.join(undeclared)}")
        return body
    return out


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("files", nargs="+", type=Path)
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()
    errors = []
    for path in args.files:
        lines = path.read_text(encoding="ascii").split("\n")
        out, pos = [], 0
        for first, last in procedures(lines):
            out.extend(lines[pos:first])
            out.extend(add_intents(lines, first, last, errors))
            pos = last + 1
        out.extend(lines[pos:])
        if not args.dry_run:
            path.write_text("\n".join(out), encoding="ascii")
    for e in errors:
        print("ERROR: " + e)
    return 1 if errors else 0


if __name__ == "__main__":
    sys.exit(main())
