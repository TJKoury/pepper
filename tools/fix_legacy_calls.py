#!/usr/bin/env python3
"""Fix the argument mismatches of tspice's calls that the legacy module's
explicit interfaces report (pepper_legacy): F77 code passed a scalar where
SPICELIB's routine declares an array (sequence association).

For each "Rank mismatch ... (rank-N and scalar)":
- an expression, or a variable the routine only reads (intent(in) in the
  module API): passed as a one-element array, [x];
- a variable the routine writes: a one-element array equivalenced with it
  (declared after it, `<name>_A1`), passed instead.

The other errors are listed for fixing by hand. Run until it fixes nothing:
    python tools/fix_legacy_calls.py
"""

import re
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
TSPICE = ROOT / 'tspice'
sys.path.insert(0, str(ROOT / 'tools'))
import gen_capi  # noqa: E402

ERR = re.compile(r'^(\S+?\.f90):(\d+):(\d+):\n\n.*?\n.*?\nError: (.*)$', re.M)


def intents():
    """routine -> {dummy: intent} from the module API's interfaces."""
    out = {}
    procs = dict(gen_capi.parse_context_interfaces())
    for mod in gen_capi.PURE_MODULES:
        for n, p in gen_capi.parse_math(mod).items():
            procs.setdefault(n, p)
    for n, p in procs.items():
        out[n] = {a.name: a.intent for a in p.args}
    return out


def mod_dir():
    mods = sorted(TSPICE.glob('build/gfortran_*/pepper_legacy.mod'), key=lambda p: p.stat().st_mtime)
    return mods[-1].parent


def compile_errors(files):
    md = mod_dir()
    errs = []
    for f in files:
        r = subprocess.run(['gfortran', '-fsyntax-only', '-frecursive', '-fallow-argument-mismatch', f'-I{md}',
                            '-J', '/tmp', '-ffree-line-length-none', str(f)], capture_output=True, text=True,
                           cwd=TSPICE)
        for m in ERR.finditer(r.stderr):
            errs.append((TSPICE / m.group(1), int(m.group(2)), int(m.group(3)), m.group(4)))
    return errs


def arg_extent(line, col):
    """The actual argument starting at col (1-based): (start, end) indices."""
    i = col - 1
    while i < len(line) and line[i] == ' ':
        i += 1
    start = i
    depth = 0
    q = None
    while i < len(line):
        ch = line[i]
        if q:
            if ch == q:
                q = None
        elif ch in '\'"':
            q = ch
        elif ch in '([':
            depth += 1
        elif ch in ')]':
            if depth == 0:
                break
            depth -= 1
        elif ch == ',' and depth == 0:
            break
        elif ch == '&' and depth == 0:
            break
        i += 1
    end = i
    while end > start and line[end - 1] == ' ':
        end -= 1
    return start, end


def routine_at(lines, l, col):
    """The routine called by the argument list containing (l, col)."""
    text = lines[l - 1][:col - 1]
    k = l - 2
    while k >= 0 and lines[k].split('!')[0].rstrip().endswith('&'):
        text = lines[k].split('!')[0].rstrip()[:-1] + ' ' + text.lstrip().lstrip('&')
        k -= 1
    depth = 0
    for idx in range(len(text) - 1, -1, -1):
        ch = text[idx]
        if ch == ')':
            depth += 1
        elif ch == '(':
            if depth == 0:
                m = re.search(r'(\w+)\s*$', text[:idx])
                return m.group(1).lower() if m else None
            depth -= 1
    return None


def unit_bounds(lines, l):
    """(header line index, end line index) of the program unit around line l."""
    hdr = re.compile(r'^\s*(?:(?:recursive)\s+)?(?:[\w*()]+\s+)?(?:precision\s+)?(subroutine|function|program)\s+\w+',
                     re.I)
    a = l - 1
    while a > 0 and not (hdr.match(lines[a]) and not lines[a].lstrip().lower().startswith('end')):
        a -= 1
    b = l - 1
    while b < len(lines) - 1 and not re.match(r'^\s*end\s*(subroutine|function|program)?\b\s*\w*\s*$', lines[b],
                                              re.I):
        b += 1
    return a, b


def declaration(lines, a, b, name):
    """(line index, type spec) of the declaration of a local variable."""
    tre = re.compile(r'^\s*(integer(?:\s*\(\s*\w+\s*\))?|logical|real(?:\s*\(\s*\w+\s*\))?|double\s+precision|'
                     r'character\s*(?:\*\s*(?:\(\s*[^)]*\)|\d+))?)\s*(::)?\s*(.*)$', re.I)
    for k in range(a + 1, b):
        m = tre.match(lines[k].split('!')[0])
        if not m:
            continue
        ents = re.split(r',(?![^(]*\))', m.group(3))
        for e in ents:
            mm = re.match(r'\s*(\w+)\s*(\*\s*\(?[\w*]+\)?)?\s*(\(.*\))?\s*$', e)
            if mm and mm.group(1).lower() == name.lower():
                if mm.group(3):
                    return None     # an array
                spec = m.group(1)
                if mm.group(2):
                    spec = 'CHARACTER' + mm.group(2)
                return k, spec
    return None


def first_element(lines, a, b, name):
    """The subscripts of the first element of an array declared in the unit
    (its lower bounds), or None."""
    if not re.fullmatch(r'[A-Za-z]\w*', name):
        return None
    pat = re.compile(rf'(?<![\w%]){name}\s*(\*\s*\(?[\w*]+\)?)?\s*\(([^()]*(?:\([^()]*\)[^()]*)*)\)', re.I)
    for k in range(a + 1, b):
        code = lines[k].split('!')[0]
        if not re.match(r'^\s*(integer|logical|real|double|character|dimension)', code, re.I):
            continue
        m = pat.search(code)
        if m:
            dims = re.split(r',(?![^(]*\))', m.group(2))
            lbs = []
            for d in dims:
                lbs.append(d.split(':')[0].strip() if ':' in d else '1')
            return ', '.join(lbs)
    return None


def is_dummy(lines, a, name):
    head = lines[a]
    k = a
    while lines[k].rstrip().endswith('&'):
        k += 1
        head += lines[k]
    m = re.search(r'\((.*)\)', head)
    return bool(m) and name.lower() in [x.strip().lstrip('&').strip().lower() for x in m.group(1).split(',')]


def main():
    files = [p for p in sorted((TSPICE / 'src').rglob('*.f90')) + sorted((TSPICE / 'app').glob('*.f90'))]
    if len(sys.argv) > 1:
        files = [Path(a).resolve() for a in sys.argv[1:]]
    known = intents()
    total = 0
    for round_ in range(6):
        errs = compile_errors(files)
        rank = [e for e in errs if re.search(r'Rank mismatch in argument .(\w+). at \(1\) \((rank-\d+ and scalar|scalar and rank-\d+)\)', e[3])]
        if not rank:
            break
        byfile = {}
        for e in rank:
            byfile.setdefault(e[0], []).append(e)
        fixed = 0
        for f, es in byfile.items():
            lines = f.read_text().split('\n')
            equivs = {}     # (decl line) -> [lines to add]
            # right to left on each line, so the columns stay valid
            for _, l, c, msg in sorted(es, key=lambda e: (e[1], e[2]), reverse=True):
                dummy = re.search(r"argument '(\w+)'", msg).group(1).lower()
                line = lines[l - 1]
                s, e = arg_extent(line, c)
                actual = line[s:e]
                if 'scalar and rank' in msg:
                    # an array where a scalar is declared: its first element
                    a, b = unit_bounds(lines, l)
                    lb = first_element(lines, a, b, actual)
                    if lb is None:
                        print(f'  by hand: {f.name}:{l}: array {actual} to a scalar ({dummy})')
                        continue
                    lines[l - 1] = line[:s] + f'{actual}({lb})' + line[e:]
                    fixed += 1
                    continue
                if not actual or actual.startswith('['):
                    continue
                routine = routine_at(lines, l, s + 1)
                intent = known.get(routine, {}).get(dummy, '')
                var = re.fullmatch(r'[A-Za-z]\w*', actual)
                if not var or intent == 'in':
                    lines[l - 1] = line[:s] + '[' + actual + ']' + line[e:]
                    fixed += 1
                    continue
                a, b = unit_bounds(lines, l)
                unit_text = '\n'.join(lines[a:b]).lower()
                if re.search(rf'parameter\s*\(\s*{actual.lower()}\s*=', unit_text):
                    lines[l - 1] = line[:s] + '[' + actual + ']' + line[e:]
                    fixed += 1
                    continue
                if is_dummy(lines, a, actual):
                    print(f'  by hand: {f.name}:{l}: {actual} (a dummy argument) to {routine}({dummy}, {intent or "?"})')
                    continue
                d = declaration(lines, a, b, actual)
                if d is None:
                    print(f'  by hand: {f.name}:{l}: {actual} to {routine}({dummy}, {intent or "?"})')
                    continue
                k, spec = d
                while lines[k].split('!')[0].rstrip().endswith('&'):
                    k += 1
                alias = f'{actual}_A1'
                if (k, alias) not in equivs:
                    equivs[(k, alias)] = [f'      {spec} {alias} ( 1 )', f'      EQUIVALENCE ( {actual}, {alias} )']
                lines[l - 1] = line[:s] + alias + line[e:]
                fixed += 1
            # the equivalences after their declarations (bottom first)
            for (k, alias), add in sorted(equivs.items(), reverse=True):
                lines[k + 1:k + 1] = add
            f.write_text('\n'.join(lines))
        total += fixed
        print(f'round {round_ + 1}: {fixed} fixed')
        if not fixed:
            break
    errs = compile_errors(files)
    print(f'{total} fixed; {len(errs)} errors left:')
    for f, l, c, msg in errs:
        print(f'  {f.relative_to(ROOT)}:{l}:{c}: {msg}')


if __name__ == '__main__':
    main()
