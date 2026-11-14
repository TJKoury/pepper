#!/usr/bin/env python3
"""
Fix calls that pass a scalar where the (now explicit) interface has an
array dummy argument, in place. SPICELIB relied on the implicit interface
for this (sequence association of a scalar with a one-element array).

    pixi run python tools/fix_scalar_args.py

Builds with fpm, reads the "Rank mismatch in argument ... (rank-1 and
scalar)" errors, and fixes each call according to the dummy argument's
intent (from the interfaces, see tools/trim_state.py):

- intent(in): the actual argument x becomes the array constructor [ x ];
- intent(out) or intent(inout): a one-element local array x_1 of x's type
  is declared in the procedure; for inout it is set from x before the call,
  and x is set from it after the call.

Only CALL statements that are statements of their own are changed; any
other case is reported. Repeats until the build has no such errors.
"""

import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from modularize import split_top, parse_decl, raw_statements, entity_name  # noqa: E402
from trim_state import intents  # noqa: E402

ERR = re.compile(r"^\./(?:\./)?(src/\S+?):(\d+):(\d+):\s*\n(?:.*\n){1,4}?Error: Rank mismatch in "
                 r"argument '(\w+)' at \(1\) \(rank-1 and scalar\)", re.M)


def build() -> str:
    r = subprocess.run(["fpm", "build"], cwd=ROOT, capture_output=True, text=True)
    return r.stdout + r.stderr


def procedure_bounds(lines, k):
    a = k
    while a > 0 and not re.match(r"\s*(module procedure \w+|(\w[\w()* ]*\s)?(subroutine|function) \w+)",
                                 lines[a]):
        a -= 1
    b = k
    while b < len(lines) and not re.match(r"\s*end (procedure|subroutine|function)\b", lines[b]):
        b += 1
    return a, b


def fix_one(path: Path, line_no: int, dummy: str, notes: list) -> bool:
    lines = path.read_text(encoding="ascii").split("\n")
    k = line_no - 1
    # the start of the statement (continued lines end with &)
    s = k
    while s > 0 and lines[s - 1].rstrip().endswith("&"):
        s -= 1
    e = s
    while lines[e].rstrip().endswith("&"):
        e += 1
    stmt = " ".join(l.strip().rstrip("&").strip() for l in lines[s:e + 1])
    m = re.match(r"call\s+(\w+)\s*\((.*)\)\s*$", stmt, re.I)
    if not m:
        notes.append(f"{path.name}:{line_no}: not a plain CALL statement: {stmt[:60]}")
        return False
    proc = m.group(1).lower()
    actual = [a.strip() for a in split_top(m.group(2))]
    ints = intents().get(proc)
    # the interface's argument names, to find the position of the dummy
    text = (ROOT / "src" / "pepper_instance_module.f90").read_text() + "".join(
        f.read_text() for f in (ROOT / "src").glob("pepper_[ams]*.f90"))
    hm = re.search(rf"(?:subroutine|function)\s+{proc}\s*\(([^)]*)\)", text, re.I)
    if not ints or not hm:
        notes.append(f"{path.name}:{line_no}: no interface for {proc}")
        return False
    names = [a.strip().lower() for a in hm.group(1).replace("&", " ").split(",")]
    pos = names.index(dummy.lower())
    x = actual[pos]
    intent = ints[pos]
    a, b = procedure_bounds(lines, k)
    body = lines[a:b + 1]
    indent = lines[s][:len(lines[s]) - len(lines[s].lstrip())]
    if intent == "in" or not re.match(r"^\w+$", x):
        if intent != "in":
            notes.append(f"{path.name}:{line_no}: output argument is not a variable: {x}")
            return False
        actual[pos] = f"[ {x} ]"
        new = [f"{indent}call {m.group(1)} ( " + ", ".join(actual) + " )"]
    else:
        # the declaration of x in the procedure
        decl = None
        for st in raw_statements("\n".join(body)):
            d = parse_decl(st.text)
            if d and x.lower() in [entity_name(en) for en in d[2]]:
                decl = d[0]
        if decl is None and re.search(rf"\b{x}\s*=>", "\n".join(body), re.I):
            # an associate name: the type of the state component
            for sf in (ROOT / "src").glob("pepper_state_*.f90"):
                mm = re.search(rf"^\s*(.*?)\s*::\s*{x}\b", sf.read_text(), re.M | re.I)
                if mm and re.search(rf"{x}\s*=>\s*ctx%[\w%]*\b{x}\b", "\n".join(body), re.I):
                    decl = mm.group(1)
                    break
        if decl is None:
            # an argument of a "module procedure": declared in its interface
            mp = re.match(r"\s*module procedure (\w+)", lines[a])
            if mp:
                inst = (ROOT / "src" / "pepper_instance_module.f90").read_text()
                im = re.search(rf"module (?:\S+ )?(?:subroutine|function) {mp.group(1)} \((.*?)end "
                               rf"(?:subroutine|function) {mp.group(1)}", inst, re.S | re.I)
                if im:
                    dm = re.search(rf"^\s*(.*?)(?:,\s*intent\(\w+\))?\s*::\s*{x}\b", im.group(1),
                                   re.M | re.I)
                    if dm:
                        decl = dm.group(1)
        if decl is None:
            notes.append(f"{path.name}:{line_no}: no declaration of {x} (state?)")
            return False
        tmp = f"{x}_1"
        actual[pos] = tmp
        new = []
        if intent == "inout":
            new.append(f"{indent}{tmp}(1) = {x}")
        new.append(f"{indent}call {m.group(1)} ( " + ", ".join(actual) + " )")
        new.append(f"{indent}{x} = {tmp}(1)")
        if not any(re.search(rf"::\s*{tmp}\s*\(", l) for l in body):
            # declare it after the last declaration statement before code
            j = next((i for i, l in enumerate(body) if re.match(r"\s*implicit none\s*$", l)), None)
            if j is None:
                # (no IMPLICIT NONE: after the last USE line, which may come
                # after the documentation)
                uses = [i for i, l in enumerate(body) if re.match(r"\s*use\b", l, re.I)]
                j = uses[-1] if uses else 0
            body_decl = f"      {decl} :: {tmp}(1)  !! one-element copy of {x} (array argument)"
            lines.insert(a + j + 1, body_decl)
            s += 1
            e += 1
    lines[s:e + 1] = [x2 for x2 in new]
    path.write_text("\n".join(lines), encoding="ascii")
    return True


def main() -> int:
    notes = []
    for round_ in range(200):
        out = build()
        hits = ERR.findall(out)
        if not hits:
            break
        # one fix per file per round (line numbers change)
        done = set()
        for f, line, col, dummy in hits:
            if f in done:
                continue
            if fix_one(ROOT / f, int(line), dummy, notes):
                done.add(f)
        if not done:
            break
    for n in notes:
        print("NOTE: " + n)
    remaining = ERR.findall(build())
    print(f"{len(remaining)} rank-mismatch errors remain")
    return 0 if not remaining else 1


if __name__ == "__main__":
    sys.exit(main())
