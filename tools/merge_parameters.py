#!/usr/bin/env python3
"""
Merge old-style two-statement named constants into one declaration, in
place:

    integer(ip)           mrklen
    parameter           ( mrklen = 10 )      ->   integer(ip), parameter :: mrklen = 10

    python tools/merge_parameters.py [--no-compile] [DIR ...]   (default: src)

Applies to *.f90, *.F90 and *.inc (recursively). The merged declaration is
put where the PARAMETER statement was (its value may use constants defined
in between); the type declaration is removed (or the name is removed from
it, if it declares several names). A PARAMETER statement with several
constants becomes one declaration per constant. Constants without their own
type declaration, and statements inside preprocessor conditionals, are left
alone. Comments are kept.

Verification (before anything is written): every changed file, and every
file that includes a changed .inc file, is compiled with gfortran before
and after for every platform macro set, and the object files must be
byte-identical. For this check the removed lines are replaced by blank
lines, so that source line numbers (which gfortran embeds in I/O
statements) are unchanged; the files written do not have them. Module
files are compiled against the module files of the current fpm build
(--modules DIR, default: the newest build directory). Run under pixi.
"""

import argparse
import re
import shutil
import sys
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from lowercase_code import verify  # noqa: E402
from modularize import raw_statements, parse_decl, split_top, entity_name  # noqa: E402

SUFFIXES = (".f90", ".F90", ".inc")


def in_conditional(lines: list) -> set:
    """Line indices inside #if ... #endif blocks."""
    inside, depth = set(), 0
    for i, line in enumerate(lines):
        s = line.strip()
        if s.startswith("#if"):
            depth += 1
        elif s.startswith("#endif"):
            depth -= 1
        elif depth > 0:
            inside.add(i)
    return inside


def merge(text: str):
    """Return (merged text, text with blank lines instead of removed lines,
    number of constants merged)."""
    lines = text.split("\n")
    stmts = raw_statements(text)
    cond = in_conditional(lines)
    decls = {}                      # name -> (stmt, decl)
    for s in stmts:
        d = parse_decl(s.text)
        if d and d[1] is None and not (set(range(s.first, s.last + 1)) & cond):
            for e in d[2]:
                if "=" not in e:
                    decls.setdefault(entity_name(e), []).append((s, d, e))
    edits, removed, n = {}, {}, 0       # line -> replacement lines / entities removed
    for s in stmts:
        m = re.match(r"\s*parameter\s*\((.*)\)\s*$", s.text, re.I | re.S)
        if not m or set(range(s.first, s.last + 1)) & cond:
            continue
        items = split_top(m.group(1))
        new = []
        for item in items:
            name = entity_name(item)
            value = item.split("=", 1)[1].strip()
            cands = decls.get(name, [])
            if len(cands) != 1 or cands[0][0].first > s.first:
                new = None
                break
            ds, d, ent = cands[0]
            new.append((ds, d, ent, value))
        if not new:
            continue
        indent = lines[s.first][:len(lines[s.first]) - len(lines[s.first].lstrip())]
        out = []
        for ds, d, ent, value in new:
            line = f"{indent}{d[0]}, parameter :: {ent} = {value}"
            if len(line) > 132 or "\n" in value:
                out = None
                break
            out.append(line)
            removed.setdefault(ds.first, (ds, d, []))[2].append(entity_name(ent))
        if out is None:
            for ds, d, ent, value in new:
                if ds.first in removed and entity_name(ent) in removed[ds.first][2]:
                    removed[ds.first][2].remove(entity_name(ent))
            continue
        edits[s.first] = ("\n".join(out), s.last)
        n += len(new)
    # Remove the names from their type declarations.
    for first, (ds, d, names) in removed.items():
        if not names:
            continue
        keep = [e for e in d[2] if entity_name(e) not in names]
        if keep:
            if ds.first != ds.last:
                raise ValueError(f"continued declaration: {ds.text[:60]}")
            line = lines[ds.first]
            ind = line[:len(line) - len(line.lstrip())]
            edits[ds.first] = (f"{ind}{d[0]} " + ", ".join(keep), ds.last)
        else:
            edits[ds.first] = (None, ds.last)
    merged, blanked = [], []
    i = 0
    while i < len(lines):
        if i in edits:
            new, last = edits[i]
            span = last - i + 1
            if new is not None:
                merged += new.split("\n")
                blanked += new.split("\n")
                # keep the line count for the check
                blanked += [""] * max(0, span - len(new.split("\n")))
                if len(new.split("\n")) > span:
                    raise ValueError("a merged declaration needs more lines than it replaces")
            else:
                blanked += [""] * span
            # A comment at the end of a removed declaration would be lost;
            # keep it as a comment line.
            for j in range(i, last + 1):
                c = lines[j].find("!")
                if new is None and c >= 0 and "'" not in lines[j][:c] and '"' not in lines[j][:c]:
                    merged.append(lines[j][:len(lines[j]) - len(lines[j].lstrip())] + lines[j][c:])
            i = last + 1
            continue
        merged.append(lines[i])
        blanked.append(lines[i])
        i += 1
    return "\n".join(merged), "\n".join(blanked), n


def newest_build() -> Path:
    mods = sorted(ROOT.glob("build/gfortran_*/pepper_instance_module.mod"), key=lambda p: p.stat().st_mtime)
    return mods[-1].parent if mods else None


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("dirs", nargs="*", type=Path, default=[ROOT / "src"])
    ap.add_argument("--no-compile", action="store_true",
                    help="write the changes without the object-file check")
    ap.add_argument("--modules", type=Path, default=None,
                    help="directory with the .mod/.smod files of the current build")
    args = ap.parse_args()
    if not args.no_compile and shutil.which("gfortran") is None:
        print("ERROR: gfortran not found (run under `pixi run`, or use --no-compile)")
        return 1
    final, check, total = {}, {}, 0
    for d in args.dirs:
        for p in sorted(d.resolve().rglob("*")):
            if p.is_file() and p.suffix in SUFFIXES:
                old = p.read_text(encoding="ascii")
                try:
                    new, blank, n = merge(old)
                except ValueError as e:
                    print(f"ERROR: {p}: {e}; nothing was changed")
                    return 1
                if new != old:
                    final[p], check[p] = new, blank
                    total += n
    print(f"{total} constants in {len(final)} files to merge")
    if not final:
        return 0
    if not args.no_compile:
        # Files that include a changed .inc file must be compiled too.
        incs = {p.name for p in final if p.suffix == ".inc"}
        if incs:
            for d in args.dirs:
                for p in sorted(d.resolve().rglob("*.[fF]90")):
                    if p not in check:
                        t = p.read_text(encoding="ascii")
                        if any(re.search(rf"include\s+'{re.escape(i)}'", t, re.I) for i in incs):
                            check[p] = t
        mods = args.modules or newest_build()
        with tempfile.TemporaryDirectory() as tmp:
            failures = verify(check, Path(tmp), extra_flags=[f"-I{mods}"] if mods else [])
        if failures:
            for f in failures[:20]:
                print("ERROR: " + f)
            print(f"{len(failures)} failure(s); nothing was changed")
            return 1
        print("object files identical before and after for all gfortran macro sets")
    for p, text in final.items():
        p.write_text(text, encoding="ascii")
    print(f"changed {len(final)} files")
    return 0


if __name__ == "__main__":
    sys.exit(main())
