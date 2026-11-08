#!/usr/bin/env python3
"""
Inventory of the global state in the SPICELIB sources: what each routine
saves, where the ENTRY umbrellas are, and which routines depend on state
through the routines they call. It drives and tracks the thread-safety work
(plan.md, step 3).

    python tools/state_inventory.py [options] [DIR ...]   (default: src/spicelib)

For every file (one procedure, possibly with ENTRY points) it finds:

- ``save`` statements (a bare ``save`` saves every local variable),
- ``data`` statements and initialized declarations (implicit SAVE),
- ``entry`` points,
- the "first call" pattern (a saved logical named first or similar),
- the routines it calls (CALL statements, and references to known functions).

Each file gets an own-state class:

    umbrella      ENTRY points (shared state between entry points)
    constants     saved values that are constants computed on the first call
                  (heuristic: a saved first flag, or no arguments; in both
                  cases no calls except to the error system)
    saved         other saved variables (caches, real shared data)
    none          no saved variables

and, through the call graph, a dependency class (the worst one reached):

    stateless       neither it nor anything it calls has state
    constants-only  only "constants" routines have state
    errors-only     the error system (chkin/sigerr/failed/...) has state
    stateful        it, or something it calls apart from the error system,
                    has other state

Output: a summary (default); --list for one line per file; --json PATH for
everything (including saved names and callees). --fail-above N exits with
status 1 if more than N files have own state (to keep the count from growing
once the work is under way).
"""

import argparse
import json
import re
import sys
from collections import Counter
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from lowercase_code import map_code  # noqa: E402

SUFFIXES = (".f90", ".F90")

# The public interface of the error system. Everything these reach is part
# of the error system.
ERROR_ROOTS = {
    "chkin", "chkout", "failed", "return", "sigerr", "setmsg", "errch", "errdp",
    "errint", "errfnm", "errhan", "getmsg", "erract", "errdev", "errprt",
    "reset", "qcktrc", "trcdep", "trcnam", "trcmxd", "trcoff", "freeze",
}

# Routines the "constants" heuristic gets wrong: they store real data.
NOT_CONSTANTS = {"zzholdd.f90"}   # get/put store of values by ID

TYPE = r"(?:integer|real|double\s*precision|logical|character|complex|type)\b"
HEADER = re.compile(r"^(?:[\w*()\s,]*?\s)?(subroutine|function)\s+(\w+)")
FIRST_NAMES = re.compile(r"^(first|frst|init|pass1|nodata|first\w*)$")
IDENT = re.compile(r"[a-z_]\w*")


def statements(text: str) -> list:
    """Code statements (lowercase), with continuations joined and character
    literals replaced by ''. Lines of both branches of #if blocks count."""
    marked = map_code(text, lambda s: "\x01" + s + "\x02")
    stmts, cur = [], ""
    for line in marked.split("\n"):
        code = "''".join(re.findall(r"\x01([^\x02]*)\x02", line)).strip()
        if not code:
            continue
        if cur and code.startswith("&"):
            code = code[1:]
        if code.endswith("&"):
            cur += code[:-1] + " "
            continue
        stmts.extend(s.strip() for s in (cur + code).split(";") if s.strip())
        cur = ""
    return [s.lower() for s in stmts]


def names_in(s: str) -> list:
    """Variable names in a list such as 'a, b(3), /c/' (array bounds dropped)."""
    depth, out = 0, []
    for tok in re.findall(r"[a-z_]\w*|[()]", s):
        if tok == "(":
            depth += 1
        elif tok == ")":
            depth -= 1
        elif depth == 0:
            out.append(tok)
    return out


def data_names(s: str) -> list:
    """Variables initialized by a DATA statement body 'a, b / 1, 2 /, c / 3 /'."""
    out = []
    for part in re.split(r"/[^/]*/", s):
        part = re.sub(r"\(([^()]*,\s*\w+\s*=[^()]*)\)", lambda m: m.group(1).split(",")[0], part)
        out.extend(n for n in names_in(part) if n not in out)
    return out


def scan(path: Path) -> dict:
    stmts = statements(path.read_text(encoding="ascii"))
    m = HEADER.match(stmts[0]) if stmts else None
    if not m:
        raise ValueError(f"{path}: first statement is not a procedure statement")
    args = re.search(r"\(([^()]*)\)", stmts[0][m.end():])
    info = {
        "file": path.name, "name": m.group(2), "kind": m.group(1),
        "nargs": len(names_in(args.group(1))) if args else 0,
        "entries": [], "save_all": False, "saved": [], "data": [],
        "initialized": [], "logicals": set(), "calls": set(), "refs": set(),
    }
    for s in stmts[1:]:
        if re.match(r"entry\s+\w+", s):
            info["entries"].append(re.match(r"entry\s+(\w+)", s).group(1))
        elif s == "save":
            info["save_all"] = True
        elif re.match(r"save\b", s):
            info["saved"] += names_in(re.sub(r"^save\s*(::)?", "", s))
        elif re.match(r"data\b\s*[a-z(]", s):
            info["data"] += data_names(s[4:])
        elif re.match(TYPE, s):
            if re.match(r"logical\b", s):
                info["logicals"].update(names_in(re.sub(r"^logical(\s*\(\w+\))?\s*(::)?", "", s)))
            if "::" in s and "parameter" not in s.split("::")[0]:
                body = s.split("::", 1)[1]
                info["initialized"] += [n.split("=")[0].strip()
                                        for n in re.split(r",(?![^()]*\))", body)
                                        if "=" in n and "=>" not in n]
        for c in re.findall(r"\bcall\s+(\w+)", s):
            info["calls"].add(c)
        info["refs"].update(re.findall(r"\b([a-z_]\w*)\s*\(", s))
    return info


def classify(infos: list) -> None:
    by_name = {}
    for info in infos:
        for n in [info["name"], *info["entries"]]:
            by_name[n] = info["file"]
    functions = {n for i in infos if i["kind"] == "function" for n in [i["name"], *i["entries"]]}
    files = {i["file"]: i for i in infos}

    for info in infos:
        own_names = {info["name"], *info["entries"]}
        callees = {c for c in info["calls"] if c in by_name}
        callees |= {r for r in info["refs"] if r in functions}
        info["callees"] = sorted({by_name[c] for c in callees - own_names} - {info["file"]})
        saved = set(info["saved"]) | set(info["data"]) | set(info["initialized"])
        info["saved_names"] = sorted(saved)
        if info["entries"]:
            own = "umbrella"
        elif info["save_all"] or saved:
            # Heuristic for constants computed on the first call: a saved
            # logical first flag, and no calls except to the error system
            # (so the saved values cannot depend on loaded data).
            firsts = {n for n in saved if FIRST_NAMES.match(n) and n in info["logicals"]}
            only_errors = all(by_name.get(c) is None or c in ERROR_ROOTS for c in info["calls"])
            constant = ((firsts or info["nargs"] == 0) and not info["save_all"] and only_errors
                        and info["file"] not in NOT_CONSTANTS)
            own = "constants" if constant else "saved"
        else:
            own = "none"
        info["own"] = own

    # The error system: everything reachable from its public interface.
    errors, todo = set(), [by_name[n] for n in ERROR_ROOTS if n in by_name]
    while todo:
        f = todo.pop()
        if f not in errors:
            errors.add(f)
            todo.extend(files[f]["callees"])
    for f in errors:
        files[f]["error_system"] = True

    # Dependency classes by fixed-point iteration over the call graph.
    rank = {"stateless": 0, "constants-only": 1, "errors-only": 2, "stateful": 3}
    for info in infos:
        if info["own"] == "none":
            info["depends"] = "stateless"
        else:
            info["depends"] = ("errors-only" if info.get("error_system")
                               else "constants-only" if own_is_constant(info) else "stateful")
    changed = True
    while changed:
        changed = False
        for info in infos:
            best = info["depends"]
            for c in info["callees"]:
                d = files[c]["depends"]
                if rank[d] > rank[best]:
                    best = d
            if best != info["depends"]:
                info["depends"], changed = best, True


def own_is_constant(info) -> bool:
    return info["own"] == "constants"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("dirs", nargs="*", type=Path, default=[ROOT / "src" / "spicelib"])
    ap.add_argument("--list", action="store_true", help="one line per file")
    ap.add_argument("--json", type=Path, metavar="PATH", help="write everything as JSON")
    ap.add_argument("--fail-above", type=int, metavar="N",
                    help="exit with status 1 if more than N files have own state")
    args = ap.parse_args()

    infos = []
    for d in args.dirs:
        for p in sorted(d.resolve().iterdir()):
            if p.is_file() and p.suffix in SUFFIXES:
                infos.append(scan(p))
    classify(infos)

    if args.list:
        for i in infos:
            extra = ""
            if i["entries"]:
                extra += f" entries={len(i['entries'])}"
            if i["save_all"]:
                extra += " save-all"
            if i["saved_names"]:
                extra += " saved=" + ",".join(i["saved_names"][:8]) + (
                    ",..." if len(i["saved_names"]) > 8 else "")
            print(f"{i['file']:16s} {i['own']:10s} {i['depends']:14s}"
                  f"{' E' if i.get('error_system') else '  '}{extra}")

    if args.json:
        out = [{k: (sorted(v) if isinstance(v, set) else v) for k, v in i.items()
                if k not in ("logicals", "refs", "calls")} for i in infos]
        args.json.parent.mkdir(parents=True, exist_ok=True)
        args.json.write_text(json.dumps(out, indent=1) + "\n")

    own = Counter(i["own"] for i in infos)
    dep = Counter(i["depends"] for i in infos)
    stateful_own = len(infos) - own["none"]
    print(f"{len(infos)} files, {sum(len(i['entries']) for i in infos)} entry points")
    print("own state:  " + ", ".join(f"{k} {own[k]}" for k in
                                     ("umbrella", "saved", "constants", "none")))
    print("depends on: " + ", ".join(f"{k} {dep[k]}" for k in
                                     ("stateful", "errors-only", "constants-only", "stateless")))
    print(f"error system: {sum(1 for i in infos if i.get('error_system'))} files")
    print(f"files with own state: {stateful_own}")
    if args.fail_above is not None and stateful_own > args.fail_above:
        print(f"ERROR: more than {args.fail_above} files have own state")
        return 1
    return 0


if __name__ == "__main__":
    sys.exit(main())
