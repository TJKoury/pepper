#!/usr/bin/env python3
"""Merge the legacy interface (tspice/src/legacy/*.f90: SPICELIB's routines
as external procedures, and the modules pepper_legacy_context and
pepper_legacy_callbacks) into one module, pepper_legacy, in the library
(src/legacy/pepper_legacy.f90).

A program gets SPICELIB's routines with `use pepper_legacy` (and no
declarations of their own of SPICELIB's functions: the module gives their
interfaces).

In each procedure:
- the `use` of the two legacy modules goes (their entities are the host's);
- EXTERNAL statements and type declarations of the module's procedures go
  (they are host procedures now), except for dummy arguments.

Run once (the migration); src/legacy/pepper_legacy.f90 is then the source.
    python tools/make_legacy_module.py [legacy source directory]
"""

import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = Path(sys.argv[1]) if len(sys.argv) > 1 else ROOT / 'tspice' / 'src' / 'legacy'
OUT = ROOT / 'src' / 'legacy' / 'pepper_legacy.f90'

HEAD = re.compile(r'^\s*(?:(?:recursive|pure|elemental)\s+)*'
                  r'(?:(?:integer\(ip\)|integer|real\(wp\)|double precision|logical|character\(\*\)|'
                  r'character\(\d+\)|character)\s+)?(subroutine|function)\s+(\w+)\s*(?:\(([^)]*)\))?', re.I)
END = re.compile(r'^\s*end\s+(subroutine|function)\b', re.I)
TYPE_DECL = re.compile(r'^(\s*)(integer(?:\(ip\))?|real\(wp\)|double precision|logical|character(?:\(\*\)|\*?\(?\d*\)?)?)'
                       r'(\s*(?:,\s*external\s*)?(?:::)?\s*)(.*)$', re.I)


def split_module(text):
    """The specification part and the contained procedures of a module."""
    lines = text.split('\n')
    i0 = next(i for i, l in enumerate(lines) if re.match(r'\s*implicit none', l))
    ic = next(i for i, l in enumerate(lines) if re.match(r'\s*contains\s*$', l))
    ie = next(i for i, l in enumerate(lines) if re.match(r'\s*end module', l))
    spec = [l for l in lines[i0 + 1:ic] if not re.match(r'\s*(private|public)\b', l)]
    publics = []
    for l in lines[i0 + 1:ic]:
        m = re.match(r'\s*public\s*::\s*(.*)', l)
        if m:
            publics += [x.strip() for x in m.group(1).split(',')]
    uses = [l for l in lines[:i0] if re.match(r'\s*use\b', l)]
    head_doc = [l for l in lines[:i0] if l.strip().startswith('!')]
    return uses, spec, lines[ic + 1:ie], publics, head_doc


def procedures(lines):
    """[(name, kind, args, start, end)] of the procedures in a list of lines."""
    out = []
    i = 0
    while i < len(lines):
        m = HEAD.match(lines[i])
        if m and not lines[i].lstrip().startswith('!') and not re.match(r'\s*end\b', lines[i], re.I):
            j = i + 1
            while not END.match(lines[j]):
                j += 1
            args = [a.strip().lower() for a in (m.group(3) or '').split(',') if a.strip()]
            out.append((m.group(2).lower(), m.group(1).lower(), args, i, j))
            i = j
        i += 1
    return out


def clean(lines, start, end, name, args, procs, funcs):
    """The cleaned lines of one procedure."""
    body = lines[start:end + 1]
    text = '\n'.join(body[1:]).lower()
    # an assumed-length result is for external functions only: ANA's is
    # the module API's, two characters
    out = [re.sub(r'character\(\*\) function ana\b', 'character(2) function ana', body[0])]
    for line in body[1:]:
        low = line.lower()
        if re.match(r'\s*use pepper_legacy_(context|callbacks)\b', low):
            continue
        m = re.match(r'^(\s*)external\s*(?:::)?\s*(.*?)\s*(!.*)?$', line, re.I)
        if m:
            names = [x.strip() for x in m.group(2).split(',') if x.strip()]
            keep = [x for x in names if x.lower() not in procs or x.lower() in args]
            if keep:
                out.append(f'{m.group(1)}external :: {", ".join(keep)}')
            continue
        m = TYPE_DECL.match(line)
        if m and '(' not in m.group(4).split('!')[0].replace('(ip)', '') or (m and 'external' in m.group(3).lower()):
            ents, _, comment = m.group(4).partition('!')
            names = [x.strip() for x in re.split(r',(?![^(]*\))', ents) if x.strip()]
            keep = []
            for x in names:
                n = re.match(r'(\w+)', x).group(1).lower()
                called = re.search(rf'(?<![\w%]){n}\s*\(', text) is not None
                others = len(re.findall(rf'(?<![\w%]){n}\b', text)) > 1
                if n in funcs and n not in args and n != name and (called or not others):
                    continue
                keep.append(x)
            if not keep:
                continue
            if len(keep) != len(names):
                line = f'{m.group(1)}{m.group(2)}{m.group(3)}{", ".join(keep)}' + (f'  !{comment}' if comment else '')
        out.append(line)
    return out


def main():
    files = sorted(SRC.glob('*.f90'))
    mod_ctx = (SRC / 'pepper_legacy_context.f90').read_text()
    mod_cb = (SRC / 'pepper_legacy_callbacks.f90').read_text()
    ctx_uses, ctx_spec, ctx_body, ctx_pub, ctx_doc = split_module(mod_ctx)
    cb_uses, cb_spec, cb_body, cb_pub, _ = split_module(mod_cb)

    # all the procedures: names, and which are functions
    chunks = []
    procs, funcs = set(), set()
    for f in files:
        if f.name in ('pepper_legacy_context.f90', 'pepper_legacy_callbacks.f90'):
            continue
        lines = f.read_text().split('\n')
        ps = procedures(lines)
        chunks.append((f, lines, ps))
        for n, kind, args, s, e in ps:
            procs.add(n)
            if kind == 'function':
                funcs.add(n)

    out = [
        '!> SPICELIB\'s interface to PEPPER: SPICELIB\'s routines, with their names and',
        '!> arguments, on a default instance and context. A program written for',
        '!> SPICELIB uses this module (`use pepper_legacy`) instead of declaring',
        '!> SPICELIB\'s functions itself; NAIF\'s test suite (tspice) runs through it.',
        '!>',
        '!> Each routine calls the module API (pepper_instance) with the default',
        '!> context, [[legacy_context]], or for the routines that change the kernels',
        '!> (FURNSH, LDPOOL, BODDEF, ...) the default instance, [[legacy_spice]],',
        '!> whose errors are signaled in the same context. The error routines (SIGERR,',
        '!> FAILED, ERRACT, ...) act on that context, which starts with SPICELIB\'s',
        '!> settings (action DEFAULT, output to the screen). Like SPICELIB itself, this',
        '!> interface is not thread-safe: threads use the module API, a context each.',
        '!>',
        '!> Generated by tools/make_legacy_module.py from the legacy interface of',
        '!> tspice (one file per subsystem, generated by tools/modularize_ctx.py and',
        '!> tools/callbacks.py), merged into one module.',
        'module pepper_legacy',
        '',
    ]
    seen = set()
    for u in ctx_uses + cb_uses:
        key = u.strip()
        if key not in seen:
            seen.add(key)
            out.append('   ' + key)
    out += ['', '   implicit none', '', '   ! The module API\'s entities this module uses are not part of its interface.',
            '   private :: pepper_spice, pepper_context, ip, wp']
    imported = re.search(r'only:\s*(.*)', next(u for u in cb_uses if 'pepper_instance' in u)).group(1)
    names = [x.strip() for x in imported.split(',') if x.strip() and x.strip() != 'pepper_context']
    out.append('   private :: ' + ', '.join(names))
    out.append('   ! The callbacks\' bridges (pepper_legacy_callbacks, merged here)')
    out.append('   private :: ' + ', '.join(x for x in cb_pub if not x.startswith('legacy_')))
    out.append('')
    out.append('   ! ---- the default instance (pepper_legacy_context, merged here)')
    out += ctx_spec
    out.append('   ! ---- the callbacks (pepper_legacy_callbacks, merged here)')
    out += cb_spec
    out += ['', 'contains', '']
    out += ctx_body
    out += cb_body
    count = 0
    for f, lines, ps in chunks:
        out.append('')
        out.append(f'   ! ==== {f.stem.replace("pepper_legacy_", "")} (from tspice/src/legacy/{f.name})')
        out.append('')
        prev = 0
        for n, kind, args, s, e in ps:
            # the comments before the procedure (its documentation)
            k = s - 1
            while k >= prev and lines[k].lstrip().startswith('!'):
                k -= 1
            doc = [l for l in lines[k + 1:s]]
            out += doc
            out += clean(lines, s, e, n, args, procs, funcs)
            out.append('')
            prev = e + 1
            count += 1
    out.append('end module pepper_legacy')
    OUT.parent.mkdir(exist_ok=True)
    OUT.write_text('\n'.join(out) + '\n')
    print(f'{OUT}: {count} procedures ({len(funcs)} functions)')


if __name__ == '__main__':
    main()
