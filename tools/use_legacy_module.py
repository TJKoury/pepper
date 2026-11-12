#!/usr/bin/env python3
"""Make tspice's program units use the legacy module (pepper_legacy) instead
of SPICELIB's routines as external procedures: add `use pepper_legacy` to
each program unit, and remove the unit's declarations of the module's
procedures (EXTERNAL statements, type declarations of functions).

A name of the module the unit uses otherwise (a variable, a dummy
argument, its own name or an ENTRY name, a routine of its own) is left out
of the `use` (renamed to pepper_legacy_<name>).

Run once (the migration):
    python tools/use_legacy_module.py [files...]   (default: tspice/src, tspice/app)
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
MODULE = ROOT / 'src' / 'legacy' / 'pepper_legacy.f90'

UNIT = re.compile(r'^\s*(?:(?:recursive|pure|elemental)\s+)*(?:(?:integer|logical|real|double\s+precision|'
                  r'character\s*(?:\*\s*\(?\s*[\w*]*\s*\)?|\(\s*[\w*]*\s*\))?|integer\s*\(\s*\w+\s*\)|'
                  r'real\s*\(\s*\w+\s*\))\s+)?(program|subroutine|function|block\s+data)\s+(\w+)\s*(\(([^)]*)\))?',
                  re.I)
END_UNIT = re.compile(r'^\s*end(\s+(program|subroutine|function|block\s*data)(\s+\w+)?)?\s*$', re.I)
TYPES = re.compile(r'^(\s*)(integer(?:\s*\(\s*\w+\s*\))?|logical|real(?:\s*\(\s*\w+\s*\))?|double\s+precision|'
                   r'character\s*(?:\*\s*(?:\(\s*[\w*+\-\s]+\s*\)|\d+))?(?:\(\s*(?:len\s*=\s*)?[\w*]+\s*\))?)'
                   r'(\s*(?:,\s*[\w\s(),:]+?)?\s*(?:::)?\s+|\s*::\s*)(.*)$', re.I)


def module_names():
    text = MODULE.read_text()
    names = set()
    funcs = set()
    for m in re.finditer(r'^\s*(?:(?:recursive|pure|elemental)\s+)*(?:([\w()*]+(?:\s+precision)?)\s+)?'
                         r'(subroutine|function)\s+(\w+)', text, re.M | re.I):
        if m.group(3).lower() in ('legacy_context', 'legacy_spice'):
            continue
        names.add(m.group(3).lower())
        if m.group(2).lower() == 'function':
            funcs.add(m.group(3).lower())
    private = set()
    for m in re.finditer(r'^\s*private\s*::\s*(.*)$', text, re.M | re.I):
        private |= {x.strip().lower() for x in m.group(1).split(',')}
    # the abstract interfaces are not procedures of the module
    for m in re.finditer(r'abstract interface(.*?)end interface', text, re.S | re.I):
        for n in re.findall(r'(?:subroutine|function)\s+(\w+)', m.group(1), re.I):
            names.discard(n.lower())
            funcs.discard(n.lower())
    return names - private, funcs - private


def logical_lines(lines):
    """[(first index, last index, joined text)] of the statements."""
    out = []
    i = 0
    while i < len(lines):
        j = i
        text = lines[i]
        while re.search(r'&\s*(!.*)?$', strip_comment(lines[j])) and j + 1 < len(lines):
            j += 1
            text = re.sub(r'&\s*(!.*)?$', '', strip_comment(text)) + ' ' + lines[j].lstrip().lstrip('&')
        out.append((i, j, text))
        i = j + 1
    return out


def strip_comment(s):
    q = None
    for k, ch in enumerate(s):
        if q:
            if ch == q:
                q = None
        elif ch in '\'"':
            q = ch
        elif ch == '!':
            return s[:k]
    return s


def split_entities(s):
    out, depth, cur = [], 0, ''
    for ch in s:
        if ch == '(':
            depth += 1
        elif ch == ')':
            depth -= 1
        if ch == ',' and depth == 0:
            out.append(cur)
            cur = ''
        else:
            cur += ch
    if cur.strip():
        out.append(cur)
    return [x.strip() for x in out]


def convert(path, names, funcs):
    lines = path.read_text().split('\n')
    stmts = logical_lines(lines)
    # program units: (header statement index, end statement index)
    units = []
    k = 0
    while k < len(stmts):
        i, j, t = stmts[k]
        code = strip_comment(t)
        m = UNIT.match(code)
        if m and not code.lstrip().lower().startswith('end'):
            e = k + 1
            while e < len(stmts) and not END_UNIT.match(strip_comment(stmts[e][2])):
                e += 1
            units.append((k, e, m.group(2).lower(), [a.strip().lower() for a in (m.group(4) or '').split(',')
                                                     if a.strip()]))
            k = e
        k += 1
    if not units:
        return False
    own_routines = {u[2] for u in units}
    edits = {}      # line index -> replacement lines (None: delete)
    inserts = {}    # line index -> lines inserted after it
    for k0, k1, uname, dummies in units:
        body = stmts[k0 + 1:k1]
        code = '\n'.join(strip_comment(t) for _, _, t in body).lower()
        entries = set(re.findall(r'^\s*entry\s+(\w+)', code, re.M))
        externals = {x.strip().lower() for st in re.findall(r'^\s*external\s*(?:::)?\s*(.*)$', code, re.M)
                     for x in st.split(',')}
        exclude = set()
        for n in {uname} | entries | set(dummies):
            if n in names:
                exclude.add(n)
        for i, j, t in body:
            c = strip_comment(t)
            low = c.lower()
            if re.match(r'^\s*(use|implicit|external|parameter|data|save|common|equivalence|intrinsic)\b', low):
                if re.match(r'^\s*external\b', low):
                    ents = re.sub(r'^\s*external\s*(::)?', '', c, flags=re.I)
                    keep = []
                    for x in split_entities(ents):
                        n = x.lower()
                        if n in names and n not in exclude and n not in own_routines:
                            continue
                        keep.append(x)
                    indent = re.match(r'\s*', t).group(0)
                    edits[(i, j)] = [f'{indent}EXTERNAL {", ".join(keep)}'] if keep else None
                continue
            m = TYPES.match(c)
            if not m or re.match(r'^\s*(character|integer|logical|real|double)\s*(\*\s*\(?[\w*]*\)?)?\s*function\b',
                                 low):
                continue
            ents = split_entities(m.group(4))
            keep = []
            for x in ents:
                mm = re.match(r'(\w+)\s*(\*\s*\(?[\w*]+\)?)?\s*(\(.*\))?\s*(=.*)?$', x)
                if not mm:
                    keep.append(x)
                    continue
                n = mm.group(1).lower()
                if n not in names:
                    keep.append(x)
                    continue
                is_array = mm.group(3) is not None or mm.group(4) is not None
                called = re.search(rf'(?<![\w%]){n}\s*\(', code) is not None
                if n in funcs and not is_array and n not in exclude and n not in own_routines and \
                        (called or n in externals or
                         not re.search(rf'(?<![\w%]){n}\b', code.replace(x.lower(), '', 1))):
                    continue        # the module's function: its declaration goes
                exclude.add(n)      # a variable of the unit (or its own routine)
                keep.append(x)
            if len(keep) != len(ents):
                indent = m.group(1)
                edits[(i, j)] = [f'{indent}{m.group(2)}{m.group(3)}{", ".join(keep)}'] if keep else None
        # names of the module the unit calls or references as variables
        # without declaring (implicit none: none), and its own routines
        for n in own_routines - {uname}:
            if n in names and re.search(rf'(?<![\w%]){n}\b', code):
                pass    # the file's own routine of a module's name: the unit calls its own
        for n in own_routines:
            if n in names:
                exclude.add(n)
        use = '      use pepper_legacy'
        if exclude:
            use += ', ' + ', '.join(f'pepper_legacy_{n} => {n}' for n in sorted(exclude))
        hi, hj, _ = stmts[k0]
        inserts[hj] = wrap(use)
    out = []
    skip_until = -1
    for idx, line in enumerate(lines):
        if idx <= skip_until:
            continue
        rep = next(((a, b, v) for (a, b), v in edits.items() if a == idx), None)
        if rep:
            a, b, v = rep
            if v:
                out += [wrap1 for w in v for wrap1 in wrap(w)]
            skip_until = b
            if b in inserts:
                out += inserts[b]
            continue
        out.append(line)
        if idx in inserts:
            out += inserts[idx]
    path.write_text('\n'.join(out))
    return True


def wrap(line, width=130):
    if len(line) <= width:
        return [line]
    parts = []
    cur = line
    while len(cur) > width:
        cut = cur.rfind(', ', 0, width - 2)
        if cut < 0:
            break
        parts.append(cur[:cut + 1] + ' &')
        cur = '         ' + cur[cut + 2:]
    parts.append(cur)
    return parts


def main():
    names, funcs = module_names()
    files = [Path(a) for a in sys.argv[1:]] or sorted((ROOT / 'tspice' / 'src').rglob('*.f90')) + \
        sorted((ROOT / 'tspice' / 'app').glob('*.f90'))
    n = 0
    for f in files:
        if 'legacy' in f.parts:
            continue
        if convert(f, names, funcs):
            n += 1
    print(f'{n} files; the module has {len(names)} routines ({len(funcs)} functions)')


if __name__ == '__main__':
    main()
