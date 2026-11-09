#!/usr/bin/env python3
"""
Inline statement functions, in place.

    python tools/inline_statement_functions.py FILE NAME ...

Each statement function NAME of FILE (a line ``name ( a, b ) = expression``
in the specification part) is removed, with its type declaration, and every
reference ``name ( x, y )`` in the executable code is replaced by the
expression with the dummy arguments replaced by the parenthesized actual
arguments.

A statement function that uses saved variables cannot stay a statement
function once the saved variables are components of a state type (reached
through ASSOCIATE, which a statement function cannot see); SC01 has such
functions. A reference that is the target of an assignment would be an
array element, not a function reference: none may exist.
"""

import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from modularize import split_top  # noqa: E402


def matching(s: str, k: int) -> int:
    """Index of the parenthesis closing the one at s[k]."""
    depth = 0
    for i in range(k, len(s)):
        if s[i] == "(":
            depth += 1
        elif s[i] == ")":
            depth -= 1
            if depth == 0:
                return i
    raise ValueError("unbalanced parentheses: " + s)


def inline(path: Path, names: list) -> None:
    lines = path.read_text(encoding="ascii").split("\n")
    defs = {}
    for n in names:
        pat = re.compile(rf"^\s*{n}\s*\(([^)]*)\)\s*=\s*(.*?)\s*$", re.I)
        hits = [i for i, l in enumerate(lines) if pat.match(l)]
        if len(hits) != 1:
            sys.exit(f"inline_statement_functions: {n}: {len(hits)} definitions")
        m = pat.match(lines[hits[0]])
        defs[n] = ([a.strip().lower() for a in m.group(1).split(",")], m.group(2))
        del lines[hits[0]]
        # its type declaration (a line of its own)
        decl = [i for i, l in enumerate(lines)
                if re.match(rf"^\s*(real\(wp\)|integer\(ip\)|logical|character\S*)\s+{n}\s*$", l, re.I)]
        for i in reversed(decl):
            del lines[i]
    for n, (dummies, expr) in defs.items():
        ref = re.compile(rf"\b{n}\s*\(", re.I)
        for i, l in enumerate(lines):
            if l.lstrip().startswith("!"):
                continue
            while True:
                m = ref.search(l)
                if not m:
                    break
                if re.match(rf"^\s*{n}\s*\(.*\)\s*=[^=]", l, re.I):
                    sys.exit(f"inline_statement_functions: {path.name}:{i + 1}: assignment to {n}")
                k = m.end() - 1
                e = matching(l, k)
                actual = [a.strip() for a in split_top(l[k + 1:e])]
                body = expr
                for d, a in zip(dummies, actual):
                    body = re.sub(rf"\b{d}\b", f"({a})" if not re.match(r"^\w+$", a) else a,
                                  body, flags=re.I)
                l = l[:m.start()] + f"( {body} )" + l[e + 1:]
            lines[i] = l
    path.write_text("\n".join(lines), encoding="ascii")


if __name__ == "__main__":
    inline(Path(sys.argv[1]), [n.lower() for n in sys.argv[2:]])
