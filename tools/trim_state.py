#!/usr/bin/env python3
"""
Turn the temporaries of an umbrella's state type back into local variables.

    python tools/trim_state.py [--dry-run] NAME ...

NAME is an umbrella whose state was taken as "all" its local variables
(modules.UMBRELLAS, e.g. zztime). A component is a temporary, and becomes a
local variable of the procedures that use it, if in every procedure that
mentions it, the first statement that does is

- an assignment of the whole variable (``x = ...``, not an element) whose
  right-hand side does not use it, outside any IF/DO/SELECT block, or
- a DO statement with it as the loop variable, outside any block.

Then no call can see a value left by an earlier call, so saving it made no
difference. (The test is conservative: a variable that is set in some
branches only, or passed to a procedure first, stays state.)

The component is removed from the type (src/pepper_state_<name>.f90) and its
initialization, and from the ASSOCIATE of each procedure, which declares it
locally instead. Character variables keep their declared length.
"""

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src"
sys.path.insert(0, str(Path(__file__).resolve().parent))
from modularize import raw_statements, parse_decl, entity_name  # noqa: E402
from modules import UMBRELLAS  # noqa: E402


def code(s) -> str:
    return re.sub(r"'[^']*'|\"[^\"]*\"", "''", s.low)


def procedures(text: str, comp_path: str):
    """(start, end) line indices of the procedures that associate names with
    comp_path (e.g. ctx%zztime)."""
    lines = text.split("\n")
    out, start = [], None
    for i, l in enumerate(lines):
        if re.match(r"\s*module procedure \w+\s*$", l) or re.match(r"\s*subroutine \w+", l):
            start = i
        elif re.match(r"\s*end (procedure|subroutine)\b", l) and start is not None:
            body = "\n".join(lines[start:i + 1])
            if comp_path + "%" in body:
                out.append((start, i))
            start = None
    return out


BLOCK_START = re.compile(r"(\w+\s*:\s*)?(if\s*\(.*\)\s*then|do\b|select\s+case)")
BLOCK_MID = re.compile(r"(else\s*if\b.*then|else|case\b.*)$")
BLOCK_END = re.compile(r"end\s*(if|do|select)\b")


def parse_blocks(stmts: list) -> list:
    """A statement list as a tree: items are ('stmt', text) or ('block',
    header text, [branch, ...]) where a branch is a list of items."""
    root, stack = [], []
    cur = root
    for s in stmts:
        t = code(s).strip()
        if BLOCK_START.match(t):
            node = ("block", t, [[]])
            cur.append(node)
            stack.append((cur, node))
            cur = node[2][0]
        elif BLOCK_MID.match(t) and stack:
            node = stack[-1][1]
            node[2].append([("stmt", t)])     # the ELSE IF/CASE condition starts the branch
            cur = node[2][-1]
        elif BLOCK_END.match(t) and stack:
            cur, _ = stack.pop()
        else:
            cur.append(("stmt", t))
    return root


def mentions(item, name) -> bool:
    pat = re.compile(rf"(?<![%\w]){name}\b")
    if item[0] == "stmt":
        return bool(pat.search(item[1]))
    return bool(pat.search(item[1])) or any(mentions(x, name) for br in item[2] for x in br)


_INTENTS = None


def intents() -> dict:
    """Procedure name -> list of the intents of its arguments (in order),
    from the interfaces in pepper_instance and the procedures of the
    stateless modules."""
    global _INTENTS
    if _INTENTS is not None:
        return _INTENTS
    _INTENTS = {}
    files = [SRC / "pepper_instance.f90"] + sorted(SRC.glob("pepper_[ams]*.f90")) + \
        [SRC / "pepper_errors.f90", SRC / "pepper_hash.f90"]
    for f in files:
        text = f.read_text(encoding="ascii")
        for m in re.finditer(r"^\s*(?:module\s+|pure\s+|elemental\s+)*(?:[\w()*]+\s+)?"
                             r"(subroutine|function)\s+(\w+)\s*\(([^)]*)\)(.*?)^\s*end\s+\1\s+\2",
                             text, re.M | re.S | re.I):
            args = [a.strip().lower() for a in m.group(3).replace("&", " ").split(",") if a.strip()]
            decl = {}
            for d in re.finditer(r"intent\((in|out|inout)\)\s*::\s*(\w+)", m.group(4), re.I):
                decl[d.group(2).lower()] = d.group(1).lower()
            _INTENTS[m.group(2).lower()] = [decl.get(a, "?") for a in args]
    return _INTENTS


def defines_by_call(stmt: str, name: str) -> bool:
    """True if stmt is a CALL that passes name (alone) to an INTENT(OUT)
    argument and does not use name otherwise."""
    m = re.match(r"call\s+(\w+)\s*\((.*)\)\s*$", stmt)
    if not m or m.group(1) not in intents():
        return False
    from modularize import split_top
    actual = [a.strip() for a in split_top(m.group(2))]
    ints = intents()[m.group(1)]
    if len(actual) != len(ints):
        return False
    hits = [i for i, a in enumerate(actual) if re.search(rf"(?<![%\w]){name}\b", a)]
    return len(hits) == 1 and actual[hits[0]] == name and ints[hits[0]] == "out"


DEFINED, PARTIAL, UNSAFE = "defined", "partial", "unsafe"


def flow(items: list, name: str) -> str:
    """For a statement list: DEFINED if every path defines name before any
    use, PARTIAL if no path uses it before defining it (but some paths may
    not define it), UNSAFE if a path may use it before defining it."""
    pat = re.compile(rf"(?<![%\w]){name}\b")
    for item in items:
        if not mentions(item, name):
            continue
        if item[0] == "stmt":
            m = re.match(rf"{name}\s*=(?!=)(.*)$", item[1])
            if (m and not pat.search(m.group(1))) or defines_by_call(item[1], name):
                return DEFINED
            return UNSAFE
        header, branches = item[1], item[2]
        if re.match(rf"(\w+\s*:\s*)?do\s+{name}\s*=", header):
            return DEFINED
        if pat.search(header):
            return UNSAFE
        results = [flow(br, name) for br in branches]
        if UNSAFE in results:
            return UNSAFE
        is_if = header.startswith("if")
        has_else = any(br and br[0][0] == "stmt" and re.match(r"else$", br[0][1]) for br in branches[1:])
        has_default = any(br and br[0][0] == "stmt" and re.match(r"case\s+default", br[0][1])
                          for br in branches[1:])
        complete = (is_if and has_else) or (header.startswith("select") and has_default)
        if complete and all(r == DEFINED for r in results):
            return DEFINED
        # some path may not define it: keep checking the following statements
    return PARTIAL


def safe(items: list, name: str) -> bool:
    return flow(items, name) != UNSAFE


def is_temporary(name: str, bodies: list) -> bool:
    for body in bodies:
        stmts = raw_statements(body)
        if any(re.match(r"go\s*to\b|goto\b", code(s)) or re.search(r"\bgo\s*to\b", code(s))
               for s in stmts):
            return False
        k = next((i for i, s in enumerate(stmts) if re.match(r"associate\b", code(s))), None)
        if k is None:
            return False
        rest = stmts[k + 1:]
        if not safe(parse_blocks(rest), name):
            return False
    return True


def trim(name: str, dry_run: bool) -> list:
    cfg = UMBRELLAS[name]
    comp = cfg["component"]
    path_expr = f"ctx%{comp}" if cfg.get("where") == "context" else f"ctx%spice%{comp}"
    smod = SRC / f"pepper_state_{name}.f90"
    stext = smod.read_text(encoding="ascii")
    # Components of the type: name -> declaration line
    tblock = re.search(rf"   type, public :: {name}_state\n(.*?)   end type {name}_state\n", stext, re.S)
    comps = {}
    for line in tblock.group(1).split("\n"):
        m = re.match(r"\s*(.*?)\s*::\s*(\w+)(.*?)\s*(!!.*)?$", line)
        if m:
            comps[m.group(2).lower()] = (line, m.group(1), m.group(2) + m.group(3).rstrip())
    files = [f for f in sorted(SRC.glob("pepper_*.[fF]90")) if path_expr + "%" in f.read_text()]
    # Bodies of all procedures (in all files) that use the component.
    texts = {f: f.read_text(encoding="ascii") for f in files}
    bodies = {n: [] for n in comps}
    for f, t in texts.items():
        lines = t.split("\n")
        for a, b in procedures(t, path_expr):
            body = "\n".join(lines[a:b + 1])
            for n in comps:
                if f"{n} => {path_expr}%{n}" in body:
                    bodies[n].append(body)
    # A SPICE cell (lower bound LBCELL) is never a temporary here: its size
    # in the control area is set once (SSIZE*) and is an input of the calls
    # that "return" the cell.
    cells = {n for n, c in comps.items() if re.search(r"\(\s*lbcell\s*:", c[2], re.I)}
    temps = [n for n in comps if bodies[n] and n not in cells and is_temporary(n, bodies[n])]
    alltext = "\n".join(texts.values())
    unused = [n for n in comps if not bodies[n] and f"{path_expr}%{n}" not in alltext]
    if dry_run:
        return [f"{name}: {len(temps)} of {len(comps)} components are temporaries: {' '.join(temps)}"
                + (f"; unused: {' '.join(unused)}" if unused else "")]
    gone = set(temps) | set(unused)
    # The type and its init routine.
    new = stext
    for n in gone:
        new = new.replace(comps[n][0] + "\n", "", 1)
        new = re.sub(rf"\n      state%{n} = [^\n]*", "", new)
    smod.write_text(new, encoding="ascii")
    # The procedures: drop the association, declare the variable locally.
    for f, t in texts.items():
        lines = t.split("\n")
        for a, b in reversed(procedures(t, path_expr)):
            body = lines[a:b + 1]
            joined = "\n".join(body)
            mine = [n for n in gone if f"{n} => {path_expr}%{n}" in joined]
            if not mine:
                continue
            # the ASSOCIATE statement (possibly continued)
            s = next(i for i, l in enumerate(body) if re.match(r"\s*associate \(", l))
            e = s
            while body[e].rstrip().endswith("&"):
                e += 1
            assoc = " ".join(x.strip().rstrip("&").strip() for x in body[s:e + 1])
            items = [x.strip() for x in re.match(r"associate \((.*)\)\s*$", assoc).group(1).split(",")]
            keep = [x for x in items if x.split("=>")[0].strip().lower() not in mine]
            indent = body[s][:len(body[s]) - len(body[s].lstrip())]
            if keep:
                new_assoc = [f"{indent}associate ( " + f", &\n{indent}            ".join(keep) + " )"]
            else:
                new_assoc = []
            # local declarations at the end of the specification part: just
            # before the first executable statement (the allocation of the
            # state, or the PEPPER comment and call before it)
            k = next(i for i, l in enumerate(body) if re.match(r"\s*if \( \.not\. allocated \(", l)
                     or re.match(r"\s*call sync_boddefs", l))
            while k > 0 and body[k - 1].lstrip().startswith("!") and "PEPPER" in body[k - 1]:
                k -= 1
            k -= 1
            decls = [f"      {comps[n][1]} :: {comps[n][2]}" for n in mine]
            if not keep:
                # no ASSOCIATE left: remove its END ASSOCIATE (the last one)
                j = max(i for i, l in enumerate(body) if re.match(r"\s*end associate\s*$", l))
                body = body[:j] + body[j + 1:]
            body = body[:s] + new_assoc + body[e + 1:]
            body = body[:k + 1] + decls + body[k + 1:]
            lines[a:b + 1] = body
        texts[f] = "\n".join(lines)
    for f, t in texts.items():
        f.write_text(t, encoding="ascii")
    return [f"{name}: {len(gone)} components are local variables now: {' '.join(sorted(gone))}"]


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("names", nargs="+")
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()
    for n in args.names:
        for line in trim(n, args.dry_run):
            print(line)
    return 0


if __name__ == "__main__":
    sys.exit(main())
