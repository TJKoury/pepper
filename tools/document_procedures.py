#!/usr/bin/env python3
"""
Add FORD documentation to the procedures moved from SPICELIB, taken from
their NAIF headers:

- a ``!>`` docstring before each procedure: the first paragraph of its
  "$ Abstract" section;
- a ``!!`` comment after each argument declaration: the argument's
  description in the "$ Brief_I/O" table. A declaration of several
  arguments is split into one declaration per argument.

    python tools/document_procedures.py MODULE_FILE [LEGACY_FILE]

With --remove-brief-io, the "$ Brief_I/O" section of a module procedure is
then deleted, since its content is inline: every argument must have its
``!!`` comment, and any other text of the section (e.g. "The function
returns ...") is added to the docstring.

With LEGACY_FILE (the legacy wrappers of the module, e.g.
src/legacy/pepper_legacy_math.f90), its wrappers get the same
documentation, plus a line naming the module procedure they call.
Procedures that already have a ``!>`` docstring, and declarations that
already have a ``!!`` comment, are left alone, so running it again changes
nothing.
"""

import argparse
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from add_intents import procedures  # noqa: E402
from modularize import HEADER, parse_decl, raw_statements, entity_name, SPEC_START  # noqa: E402

ROW = re.compile(r"^!(\s{2,})([A-Za-z]\w*(?:\s*,\s*[A-Za-z]\w*)*)(\s+)(I/O|I-O|I|O)(\s+)(\S.*)$")
PENDING = re.compile(r"^!\s{2,}([A-Za-z]\w*(?:\s*,\s*[A-Za-z]\w*)*)\s*,\s*$")


def section(body: list, title: str) -> list:
    out, inside = [], False
    for line in body:
        if line.startswith("! $ "):
            if inside:
                break
            inside = line.startswith("! $ " + title)
            continue
        if inside:
            out.append(line)
    return out


def abstract(body: list) -> list:
    """First paragraph of the Abstract section, as text lines."""
    text, started = [], False
    for line in section(body, "Abstract"):
        s = line[1:].strip() if line.startswith("!") else line.strip()
        if not s:
            if started:
                break
            continue
        started = True
        text.append(s)
    return text


def descriptions(body: list) -> dict:
    """Argument name (lowercase) -> description from the Brief_I/O table."""
    out, pending, current, col = {}, [], None, None
    for line in section(body, "Brief_I/O"):
        r = ROW.match(line)
        if r:
            names = pending + re.split(r"\s*,\s*", r.group(2))
            col = line.index(r.group(6))
            current = [r.group(6).strip()]
            for n in names:
                out[n.lower()] = current
            pending = []
            continue
        p = PENDING.match(line)
        if p:
            pending += re.split(r"\s*,\s*", p.group(1))
            current = None
            continue
        if current is not None and col and len(line) > col and not line[1:col].strip():
            current.append(line[col:].strip())
            continue
        current = None
    return {k: " ".join(v) for k, v in out.items()}


def leftover_text(body: list) -> list:
    """The lines of the Brief_I/O section that are not part of the table
    (header, rows, row continuations), as paragraphs of text lines."""
    out, current, col = [], False, None
    for line in section(body, "Brief_I/O"):
        r = ROW.match(line)
        if r or PENDING.match(line) or re.match(r"^!\s+(VARIABLE\s|--------)", line):
            current = bool(r)
            col = line.index(r.group(6)) if r else col
            continue
        if current and col and len(line) > col and not line[1:col].strip():
            continue
        current = False
        s = line[1:].strip() if line.startswith("!") else line.strip()
        if s:
            out.append(s)
        elif out and out[-1] != "":
            out.append("")
    while out and out[-1] == "":
        out.pop()
    return out


def remove_brief_io(lines: list, first: int, last: int, errors: list) -> list:
    """Procedure lines without the Brief_I/O section (its other text moved
    into the docstring), if all arguments have inline comments."""
    body = lines[first:last + 1]
    stmts = raw_statements("\n".join(body))
    head = HEADER.match(stmts[0].text)
    name = head.group("name").lower()
    args = [a.strip().lower() for a in (head.group("args") or "").split(",") if a.strip()]
    starts = [k for k, l in enumerate(body) if l.startswith("! $ Brief_I/O")]
    if not starts:
        return body
    documented = set()
    for l in body:
        if "!!" in l:
            d = parse_decl(l.split("!!")[0].strip())
            if d:
                documented |= {entity_name(e) for e in d[2]}
    missing = [a for a in args if a not in documented]
    if missing:
        errors.append(f"{name}: Brief_I/O kept (no inline comment for {', '.join(missing)})")
        return body
    s = starts[0]
    e = next(k for k in range(s + 1, len(body)) if body[k].startswith("! $ "))
    extra = leftover_text(body)
    out = body[:s] + body[e:]
    if extra:
        h = stmts[0].first          # the procedure statement (after any docstring)
        indent = out[h][:len(out[h]) - len(out[h].lstrip())]
        out[h:h] = [f"{indent}!>"] + [f"{indent}!> {x}" if x else f"{indent}!>" for x in extra]
    return out


def document(lines: list, first: int, last: int, names_doc: dict, extra=None) -> list:
    """Return the procedure lines with a docstring and argument comments."""
    body = lines[first:last + 1]
    stmts = raw_statements("\n".join(body))
    head = HEADER.match(stmts[0].text)
    args = [a.strip().lower() for a in (head.group("args") or "").split(",") if a.strip()]
    docs = names_doc.get("args", {})
    out = list(body)
    for s in stmts[1:]:
        if not (SPEC_START.match(s.text) or parse_decl(s.text)):
            break
        decl = parse_decl(s.text)
        if not decl or s.first != s.last or "!!" in out[s.first]:
            continue
        dummies = [e for e in decl[2] if entity_name(e) in args]
        if not dummies:
            continue
        line = out[s.first]
        indent = line[:len(line) - len(line.lstrip())]
        spec, attrs, ents = decl
        prefix = f"{spec}, {attrs} :: " if attrs else (f"{spec} :: " if attrs is not None else f"{spec} ")
        new = []
        others = [e for e in ents if entity_name(e) not in args]
        if others:
            new.append(indent + prefix + ", ".join(others))
        if len(dummies) == 1 and not others:
            d = docs.get(entity_name(dummies[0]))
            new.append(line.rstrip() + (f"  !! {d}" if d else ""))
        else:
            for e in dummies:
                d = docs.get(entity_name(e))
                new.append(indent + prefix + e + (f"  !! {d}" if d else ""))
        out[s.first] = "\n".join(new)
    header_line = out[0]
    indent = header_line[:len(header_line) - len(header_line.lstrip())]
    doc = names_doc.get("abstract", [])
    if doc or extra:
        block = [f"{indent}!> {t}" for t in doc]
        if extra:
            block += ([f"{indent}!>"] if doc else []) + [f"{indent}!> {t}" for t in extra]
        out[0] = "\n".join(block + [header_line])
    return out


def has_docstring(lines: list, first: int) -> bool:
    k = first - 1
    while k >= 0 and not lines[k].strip():
        k -= 1
    return k >= 0 and lines[k].lstrip().startswith("!>")


def rewrite(path: Path, info: dict, module=None, remove=False, errors=None) -> int:
    lines = path.read_text(encoding="ascii").split("\n")
    out, pos, n = [], 0, 0
    for first, last in procedures(lines):
        name = HEADER.match(raw_statements("\n".join(lines[first:last + 1]))[0].text).group("name").lower()
        out.extend(lines[pos:first])
        doc = dict(info.get(name, {}))
        if has_docstring(lines, first):
            doc.pop("abstract", None)
        extra = None
        if module and not has_docstring(lines, first):
            extra = [f"Legacy interface: calls {name} of module {module}."]
        new = document(lines, first, last, doc, extra)
        if remove:
            joined = "\n".join(new).split("\n")
            new = remove_brief_io(joined, 0, len(joined) - 1, errors)
        n += new != lines[first:last + 1]
        out.extend(new)
        pos = last + 1
    out.extend(lines[pos:])
    path.write_text("\n".join(out), encoding="ascii")
    return n


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("module_file", type=Path)
    ap.add_argument("legacy_file", type=Path, nargs="?")
    ap.add_argument("--remove-brief-io", action="store_true",
                    help="delete the Brief_I/O sections of the module procedures")
    args = ap.parse_args()
    lines = args.module_file.read_text(encoding="ascii").split("\n")
    info = {}
    for first, last in procedures(lines):
        body = lines[first:last + 1]
        name = HEADER.match(raw_statements("\n".join(body))[0].text).group("name").lower()
        info[name] = {"abstract": abstract(body), "args": descriptions(body)}
    module = re.search(r"^module (\w+)", "\n".join(lines), re.M).group(1)
    errors = []
    n = rewrite(args.module_file, info, remove=args.remove_brief_io, errors=errors)
    print(f"{args.module_file}: {n} procedures changed")
    for e in errors:
        print("NOTE: " + e)
    if args.legacy_file:
        print(f"{args.legacy_file}: {rewrite(args.legacy_file, info, module)} wrappers documented")
    return 0


if __name__ == "__main__":
    sys.exit(main())
