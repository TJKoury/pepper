#!/usr/bin/env python3
"""
Remove SPICELIB's traceback mechanism (CHKIN/CHKOUT) from the sources, in
place:

    python tools/remove_traceback.py [DIR ...]      (default: src/spicelib)

- Deletes every ``call chkin (...)``, ``call chkout (...)`` and
  ``call freeze`` statement (trcpkg itself is removed separately).
- Removes the traceback output block in outmsg.
- Tidies the blocks that become trivial (only where no comment lines are in
  between):
      if (c) then / return / else / end if    ->  if (c) return
      if (c) then / return / end if           ->  if (c) return
      else / end if                           ->  end if
- Reports blocks that became empty (``if (c) then / end if``); they are
  left for review.

The legacy layer keeps no-op CHKIN/CHKOUT/... for existing callers.
"""

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from modularize import raw_statements  # noqa: E402

DELETE = re.compile(r"call\s+(chkin|chkout)\s*\(\s*('[^']*'|\w+)\s*\)$|call\s+freeze$", re.I)
TRACE_BLOCK = re.compile(r"if\s*\(\s*trace\s+\.and\.\s+msgsel\s*\(\s*'TRACEBACK'\s*\)\s*\)\s*then$", re.I)


def block_end(stmts, k):
    """Index of the END IF matching the IF ... THEN at stmts[k]."""
    depth = 0
    for j in range(k, len(stmts)):
        t = stmts[j].text.lower()
        if re.match(r"if\s*\(.*\)\s*then$", t):
            depth += 1
        elif re.match(r"end\s*if$", t):
            depth -= 1
            if depth == 0:
                return j
    raise ValueError("unmatched IF block")


def only_blank_between(lines, a, b):
    """True if the lines strictly between line indices a and b are blank."""
    return all(not lines[i].strip() for i in range(a + 1, b))


def tidy(lines: list) -> tuple:
    """One pass of block tidying; returns (new lines, changed?)."""
    stmts = raw_statements("\n".join(lines))
    low = [s.text.lower() for s in stmts]
    for k, s in enumerate(stmts):
        m = re.match(r"if\s*(\(.*\))\s*then$", low[k])
        if not m:
            continue
        cond = re.match(r"if\s*(\(.*\))\s*then$", s.text, re.I).group(1)
        indent = lines[s.first][:len(lines[s.first]) - len(lines[s.first].lstrip())]
        pats = [["return", "else", "end if"], ["return", "end if"]]
        for pat in pats:
            seq = low[k + 1:k + 1 + len(pat)]
            if [re.sub(r"\s+", " ", x) for x in seq] == pat or \
               [x.replace("endif", "end if") for x in seq] == pat:
                group = stmts[k:k + 1 + len(pat)]
                if all(only_blank_between(lines, a.last, b.first) for a, b in zip(group, group[1:])):
                    new = lines[:s.first] + [f"{indent}if {cond} return"] + lines[group[-1].last + 1:]
                    return new, True
    for k in range(len(stmts) - 1):
        if low[k] == "else" and re.match(r"end\s*if$", low[k + 1]) and \
                only_blank_between(lines, stmts[k].last, stmts[k + 1].first):
            return lines[:stmts[k].first] + lines[stmts[k].last + 1:], True
    return lines, False


def process(text: str, name: str, report: list) -> str:
    lines = text.split("\n")
    stmts = raw_statements(text)
    drop = set()
    for k, s in enumerate(stmts):
        if DELETE.match(s.text):
            drop.update(range(s.first, s.last + 1))
        elif TRACE_BLOCK.match(s.text):
            e = block_end(stmts, k)
            drop.update(range(s.first, stmts[e].last + 1))
    lines = [l for i, l in enumerate(lines) if i not in drop]
    changed = True
    while changed:
        lines, changed = tidy(lines)
    stmts = raw_statements("\n".join(lines))
    for a, b in zip(stmts, stmts[1:]):
        if re.match(r"if\s*\(.*\)\s*then$", a.text, re.I) and re.match(r"end\s*if$", b.text, re.I):
            report.append(f"{name}:{a.first + 1}: empty block: {a.text}")
    return "\n".join(lines)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("dirs", nargs="*", type=Path, default=[ROOT / "src" / "spicelib"])
    args = ap.parse_args()
    report, n = [], 0
    for d in args.dirs:
        for p in sorted(d.glob("*.[fF]90")):
            if p.name == "trcpkg.F90":
                continue
            old = p.read_text(encoding="ascii")
            new = process(old, p.name, report)
            if new != old:
                p.write_text(new, encoding="ascii")
                n += 1
    print(f"changed {n} files")
    for r in report:
        print(r)
    return 0


if __name__ == "__main__":
    sys.exit(main())
