#!/usr/bin/env python3
"""
Replace SPICELIB's include files (parameters only) by modules.

    python tools/inc_to_modules.py [--dry-run]

For each src/spicelib/<name>.inc this writes src/inc/pepper_inc_<name>.f90:
a module with the include file's parameter declarations (and its comments,
which document them) and nothing else. Then every INCLUDE line in src/,
src/legacy/ and test/ becomes a USE of that module, placed in the USE part of
the scoping unit that contained the INCLUDE (USE statements come before
IMPLICIT NONE and the declarations; an INCLUDE may stand anywhere in the
specification part). A unit that already uses the module gets no second USE.

Scoping units: program, module, (module) subroutine/function, MODULE
PROCEDURE bodies and the interface bodies of pepper_instance_module. Statement
continuations (&) of the unit's header and of USE statements are respected.
The .inc files themselves are left for the caller to delete.
"""

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
INC = ROOT / "src" / "spicelib"
OUT = ROOT / "src" / "inc"

INCLUDE = re.compile(r"^\s*include\s+['\"]([\w.]+)['\"]\s*(?:!.*)?$", re.I)
HEADER = re.compile(r"^\s*(?:(?:pure|elemental|recursive|impure|module)\s+)*"
                    r"(?:(?:integer|real|logical|character|double\s+precision|type)\b[^!]*?\s+)?"
                    r"(?:subroutine|function)\s+\w+|^\s*module\s+procedure\s+\w+\s*$|"
                    r"^\s*program\s+\w+|^\s*module\s+(?!procedure\b)\w+\s*$|^\s*submodule\b", re.I)
USE = re.compile(r"^\s*use\b", re.I)


def module_name(inc: str) -> str:
    return "pepper_inc_" + inc.lower().removesuffix(".inc").replace("-", "_")


def module_text(path: Path) -> str:
    text = path.read_text(errors="replace")
    name = module_name(path.name)
    body = []
    for line in text.split("\n"):
        if re.match(r"^[cC*]", line) and not re.match(r"^\s*\w", line[1:2] or " "):
            line = "!" + line[1:]
        body.append(("   " + line.strip()) if line.strip() and not line.lstrip().startswith("!")
                    else line.rstrip())
    while body and not body[-1].strip():
        body.pop()
    return (f"!> The parameters of SPICELIB's include file {path.name} (an include\n"
            f"!> file until PEPPER; tools/inc_to_modules.py). Its comments follow.\n"
            f"module {name}\n\n   implicit none\n\n   public\n\n" + "\n".join(body) +
            f"\n\nend module {name}\n")


def unit_start(lines: list, k: int) -> int:
    """Index of the last line of the header of the scoping unit containing
    line k (headers may be continued)."""
    i = k
    while i >= 0:
        if HEADER.match(lines[i]) and not lines[i].lstrip().startswith("!"):
            j = i
            while lines[j].rstrip().endswith("&"):
                j += 1
            return j
        i -= 1
    return -1


def convert_file(path: Path, dry: bool) -> int:
    lines = path.read_text(errors="replace").split("\n")
    n = 0
    k = 0
    while k < len(lines):
        m = INCLUDE.match(lines[k])
        if not m:
            k += 1
            continue
        if not (INC / m.group(1).lower()).exists() and \
                not (OUT / (module_name(m.group(1)) + ".f90")).exists():
            k += 1          # not one of SPICELIB's include files (e.g. tspice's own)
            continue
        mod = module_name(m.group(1))
        h = unit_start(lines, k)
        del lines[k]
        # the unit's USE statements between its header and the include
        last = h
        j = h + 1
        while j < k:
            if USE.match(lines[j]):
                e = j
                while lines[e].rstrip().endswith("&"):
                    e += 1
                last = e
                j = e + 1
            else:
                j += 1
        uses = "\n".join(lines[h + 1:k])
        if not re.search(rf"^\s*use\s+{mod}\b", uses, re.M | re.I):
            ind = "      "
            lines.insert(last + 1, f"{ind}use {mod}")
            k += 1
        n += 1
    if n and not dry:
        path.write_text("\n".join(lines))
    return n


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()
    incs = sorted(INC.glob("*.inc"))
    if not args.dry_run:
        OUT.mkdir(exist_ok=True)
        for p in incs:
            (OUT / (module_name(p.name) + ".f90")).write_text(module_text(p))
    total = 0
    files = [p for d in ("src", "test") for p in (ROOT / d).rglob("*") if p.suffix.lower() in (".f90",)
             and "/inc/" not in str(p)]
    for p in sorted(files):
        total += convert_file(p, args.dry_run)
    print(f"inc_to_modules: {len(incs)} modules, {total} INCLUDE lines replaced")
    return 0


if __name__ == "__main__":
    sys.exit(main())
