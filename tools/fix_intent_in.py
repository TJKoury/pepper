#!/usr/bin/env python3
"""
Make arguments that are documented as inputs but modified intent(inout),
from the compiler's errors.

    pixi run python tools/fix_intent_in.py

Builds with fpm and reads the errors "Dummy argument 'X' with INTENT(IN) in
variable definition context" (an assignment to X, or X passed to an output
argument). SPICELIB's Brief_I/O tables call some arguments inputs that the
routine changes (e.g. the EK segment descriptors, updated in place by the
column writers): the code is right and the table is not. For each error the
argument becomes intent(inout) in the procedure's interface in
src/pepper_instance_module.f90 (and in the procedure itself if it declares its
arguments, the full MODULE SUBROUTINE form), with a note in its !! comment.
Repeats until the build has no such errors; prints what it changed.
"""

import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
ERR = re.compile(r"^\./(?:\./)?(src/\S+?):(\d+):(\d+):\s*\n(?:.*\n){1,4}?Error: Dummy argument "
                 r"'(\w+)' with INTENT\(IN\) in variable definition context", re.M)
NOTE = " (PEPPER: changed by the routine, an input in SPICELIB's table)"


def build() -> str:
    subprocess.run([sys.executable, str(ROOT / "tools" / "clean_stale.py")], cwd=ROOT,
                   capture_output=True)
    r = subprocess.run(["fpm", "build"], cwd=ROOT, capture_output=True, text=True)
    return r.stdout + r.stderr


def procedure_at(path: Path, line: int) -> str:
    name = None
    for i, l in enumerate(path.read_text().split("\n")[:line]):
        m = re.match(r"\s*module (?:procedure (\w+)\s*$|(?:\S+ )*(?:subroutine|function) (\w+)\b)", l)
        if m:
            name = m.group(1) or m.group(2)
    return name


def make_inout(text: str, proc: str, arg: str, indent: str) -> tuple:
    m = re.search(rf"\n {indent}module (?:\S+ )*(?:subroutine|function) {proc} \(.*?\n {indent}end "
                  rf"(?:subroutine|function) {proc}\n", text, re.S)
    if not m:
        return text, False
    block = m.group(0)
    new, n = re.subn(rf"(, )intent\(in\)( :: {arg}\b[^\n]*)", lambda x: x.group(1) + "intent(inout)" +
                     x.group(2) + ("" if NOTE in x.group(2) else NOTE), block)
    return text.replace(block, new), n > 0


def main() -> int:
    inst = ROOT / "src" / "pepper_instance_module.f90"
    done = set()
    for _ in range(50):
        hits = ERR.findall(build())
        todo = set()
        for f, line, col, arg in hits:
            proc = procedure_at(ROOT / f, int(line))
            if proc and (proc, arg.lower()) not in done:
                todo.add((f, proc, arg.lower()))
        if not todo:
            break
        for f, proc, arg in sorted(todo):
            t = inst.read_text()
            t, ok = make_inout(t, proc, arg, "     ")
            inst.write_text(t)
            src = ROOT / f
            s = src.read_text()
            s, ok2 = make_inout(s, proc, arg, "")
            if ok2:
                src.write_text(s)
            print(f"{proc}: {arg} -> intent(inout)" + ("" if ok else " (interface not found)"))
            done.add((proc, arg))
    remaining = ERR.findall(build())
    print(f"{len(remaining)} INTENT(IN) errors remain")
    return 0 if not remaining else 1


if __name__ == "__main__":
    sys.exit(main())
