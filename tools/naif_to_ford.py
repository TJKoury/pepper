#!/usr/bin/env python3
"""
Turn the NAIF headers of the module procedures into FORD documentation.

    python tools/naif_to_ford.py [FILE ...]   (default: the submodules in src/)

In the modules and submodules, each procedure (MODULE PROCEDURE, (MODULE)
SUBROUTINE or FUNCTION) begins with NAIF's header: comment lines with the sections
"! $ Abstract", "! $ Particulars", "! $ Examples", ... This makes the header
the procedure's FORD documentation: the comment lines become docmark lines
(!!) right after the procedure statement, and each section title becomes a
Markdown heading ("## Detailed Input"). The text keeps its indentation, so
FORD shows it preformatted, as NAIF wrote it (tables, code examples and
formulas included). Only the header (the comments before the procedure's
first statement) changes, and the USE and IMPLICIT statements before it move
after it; nothing else does. A header without NAIF sections
is left alone. Idempotent.
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
START = re.compile(r"^\s*(?:module\s+procedure\s+\w+\s*$|(?:(?!end\b)\S+\s+)*(?:subroutine|function)\s+\w+\s*\()", re.I)
SECTION = re.compile(r"^!\s*\$\s+(\w[\w/ -]*)\s*$")


def convert(path: Path) -> int:
    lines = path.read_text().split("\n")
    out, i, n = [], 0, 0
    while i < len(lines):
        out.append(lines[i])
        if not START.match(lines[i]) or lines[i].lstrip().startswith("!"):
            i += 1
            continue
        # the end of the procedure statement (continued with &)
        while lines[i].rstrip().endswith("&"):
            i += 1
            out.append(lines[i])
        j = i + 1
        # USE statements may come before the header (they move after it)
        uses = []
        def pre(x):
            return bool(re.match(r"\s*(use\b|implicit none)", x, re.I)) or "!! one-element copy" in x
        while j < len(lines):
            if pre(lines[j]) or (uses and uses[-1].rstrip().endswith("&")):
                uses.append(lines[j])
                j += 1
            elif not lines[j].strip() and uses:
                # a blank line between them: only if more of them follow
                n2 = j
                while n2 < len(lines) and not lines[n2].strip():
                    n2 += 1
                if n2 < len(lines) and pre(lines[n2]):
                    uses += lines[j:n2]
                    j = n2
                else:
                    break
            else:
                break
        # the header: blank and comment lines up to the first statement
        k = j
        while k < len(lines) and (not lines[k].strip() or lines[k].lstrip().startswith("!")):
            k += 1
        block = lines[j:k]
        if not any(SECTION.match(b.strip()) for b in block) or any(b.lstrip().startswith("!!") for b in block):
            out += uses
            i = j
            continue
        doc = []
        for b in block:
            s = b.strip()
            if not s:
                continue              # (docmark lines must be contiguous)
            m = SECTION.match(s)
            if m:
                title = m.group(1).replace("_", " ").strip()
                doc += ["!!", f"!! ## {title}", "!!"]
            elif s == "!" or s.startswith("!-&"):
                doc.append("!!")
            else:
                doc.append("!!" + b.lstrip()[1:])
        while doc and doc[0] == "!!":
            doc.pop(0)
        # (no runs of empty doc lines)
        doc = [d for x, d in enumerate(doc) if not (d == "!!" and x > 0 and doc[x - 1] == "!!")]
        out += doc + uses
        n += 1
        i = k
    path.write_text("\n".join(out))
    return n


def main() -> int:
    files = [Path(f) for f in sys.argv[1:]] or sorted(
        p for p in (ROOT / "src").glob("pepper_*.[fF]90") if not p.name.startswith("pepper_state_"))
    total = sum(convert(p) for p in files)
    print(f"naif_to_ford: {total} procedure headers converted in {len(files)} files")
    return 0


if __name__ == "__main__":
    sys.exit(main())
