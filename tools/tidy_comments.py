#!/usr/bin/env python3
"""
Tidy the comments and blank lines of the sources, in place (code is not
changed):

    python tools/tidy_comments.py [DIR_OR_FILE ...]       (default: src)

- Comments about SPICELIB's traceback, which PEPPER does not have, are
  removed or shortened: "Use discovery check-in." goes; "..., then check
  out and return." becomes "..., then return."; "... and check out." loses
  the check-out.
- A comment block left without text is removed.
- NAIF header sections with no content are removed (e.g. "$ Declarations",
  whose declarations moved into the interface).
- Runs of blank lines, and runs of empty comment lines ("!"), become one.

Comment lines are recognized after the code's literals (map_code), so text
in strings is never touched.
"""

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(Path(__file__).resolve().parent))
from lowercase_code import map_code  # noqa: E402

SUFFIXES = (".f90", ".F90")

REMOVE = [
    re.compile(r"\s*This routine uses discovery check-in\.", re.I),
    re.compile(r"\s*Use discovery check-in(?: for speed)?(?:\.|;)?", re.I),
    re.compile(r"\s*Check in only if an error is detected\.", re.I),
    re.compile(r"\s*Discovery check-in(?: is used)?\.", re.I),
]
SHORTEN = [
    (re.compile(r"\bthen\s+check\s*-?\s*out\s+and\s+return\b", re.I), "then return"),
    (re.compile(r"\bcheck\s*-?\s*out\s+and\s+return\b", re.I), "return"),
    (re.compile(r"\bgive up and check out\b", re.I), "give up and return"),
    (re.compile(r"(?<=\S)\s+and\s+check\s+out\.", re.I), "."),
]
ONLY_CHECKOUT = re.compile(r"^\s*and\s+check\s+out\.\s*$", re.I)


def comment_lines(text: str) -> set:
    """Indices of the lines that are comment lines (only a comment)."""
    out = set()
    for i, line in enumerate(map_code(text, lambda s: "\x01" + s + "\x02").split("\n")):
        if line.startswith("\x01\x02!") or (line.lstrip().startswith("!") and "\x01" not in line
                                              and not line.startswith(("'", '"'))):
            out.add(i)
    return out


def tidy(text: str) -> str:
    lines = text.split("\n")
    comments = comment_lines(text)
    # The NAIF header sections (from a "! $ " line to the next code line, and
    # the revision history up to "!-&"): their text is never edited.
    header, inside = set(), False
    for i, line in enumerate(lines):
        if re.match(r"! \$ \w", line) or line.startswith("!-"):
            inside = True
        elif i not in comments and line.strip():
            inside = False
        if inside:
            header.add(i)
    # 1. traceback comments (in the code only)
    for i in sorted(comments - header):
        line = lines[i]
        body = line.lstrip()[1:]
        if ONLY_CHECKOUT.match(body):
            # "...signal an error" / "and check out." -> "...signal an error."
            k = i - 1
            if k in comments and k not in header and lines[k].lstrip()[1:].strip():
                lines[k] = lines[k].rstrip() + "."
                lines[i] = line[:len(line) - len(line.lstrip())] + "!"
                continue
        new = body
        for pat in REMOVE:
            new = pat.sub("", new)
        for pat, rep in SHORTEN:
            new = pat.sub(rep, new)
        if new != body:
            prefix = line[:len(line) - len(line.lstrip())] + "!"
            lines[i] = prefix + new.rstrip() if new.strip() else prefix
    # 2. comment blocks without text (bounded by non-comment lines)
    out, i = [], 0
    while i < len(lines):
        if i in comments and i not in header:
            j = i
            while j in comments:
                j += 1
            block = lines[i:j]
            if any(l.lstrip()[1:].strip() for l in block):
                out.extend(block)
            i = j
        else:
            out.append(lines[i])
            i += 1
    lines = out
    # 3. empty NAIF sections ("! $ Name" followed only by "!" lines up to the
    #    next section or the end of the comment block)
    out, i = [], 0
    while i < len(lines):
        if re.match(r"! \$ \w", lines[i]):
            j = i + 1
            while j < len(lines) and lines[j].strip() == "!":
                j += 1
            nxt = lines[j] if j < len(lines) else ""
            if re.match(r"! \$ \w", nxt) or re.match(r"!-&", nxt):
                i = j            # the section is empty
                continue
        out.append(lines[i])
        i += 1
    lines = out
    # 4. runs of blank lines / empty comment lines
    out = []
    for line in lines:
        if out and not line.strip() and not out[-1].strip():
            continue
        if out and line.strip() == "!" and out[-1].strip() == "!":
            continue
        out.append(line)
    return "\n".join(out)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("paths", nargs="*", type=Path, default=[ROOT / "src"])
    args = ap.parse_args()
    n = 0
    for p in args.paths:
        files = [f for f in sorted(p.rglob("*")) if f.suffix in SUFFIXES] if p.is_dir() else [p]
        for f in files:
            old = f.read_text(encoding="ascii")
            new = tidy(old)
            if new != old:
                f.write_text(new, encoding="ascii")
                n += 1
    print(f"tidied {n} files")
    return 0


if __name__ == "__main__":
    sys.exit(main())
