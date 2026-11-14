#!/usr/bin/env python3
"""Split the legacy interface (src/legacy/pepper_legacy.f90, one module of
37,000 lines that took gigabytes of RAM to compile with ifx on Windows) into
one module per subsystem, and keep pepper_legacy as the module that uses
them all (so programs still `use pepper_legacy`):

- pepper_legacy_context: the default instance and its context
  (legacy_context, legacy_spice), the only global state;
- pepper_legacy_callbacks: the callbacks in SPICELIB's form (the abstract
  interfaces legacy_*) and their bridges to the module API's callbacks;
- pepper_legacy_<subsystem>: the routines of a subsystem (the sections
  "! ==== <subsystem>" of the merged module). The routines of one subsystem
  call no routine of another (each calls the module API with the default
  context), except for the error routines (SETMSG, SIGERR of errsys) the
  umbrellas' entries call;
- pepper_legacy: uses all of them, with the same public entities as the
  merged module (minus its two module variables, which are internal now).

Run once (the migration); the files in src/legacy are then the source.
    python tools/split_legacy_module.py
"""

import re
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
DIR = ROOT / 'src' / 'legacy'
MERGED = DIR / 'pepper_legacy.f90'

HEAD = re.compile(r'^\s*(?:(?:recursive|pure|elemental)\s+)*'
                  r'(?:(?:integer\(ip\)|integer|real\(wp\)|double precision|logical|character\(\*\)|'
                  r'character\(\d+\)|character)\s+)?(subroutine|function)\s+(\w+)', re.I)
END = re.compile(r'^\s*end\s+(subroutine|function)\b', re.I)


def top_level(lines):
    """The names of the procedures in LINES that are not inside another
    (internal procedures, interface bodies)."""
    names, depth = [], 0
    for l in lines:
        if l.lstrip().startswith('!'):
            continue
        if END.match(l):
            depth -= 1
        elif m := HEAD.match(l):
            if depth == 0:
                names.append(m.group(2).lower())
            depth += 1
    return names


def hoist_instance_use(body):
    """BODY without the routines' USE of pepper_instance_module, and the entities
    they import ([local name => ]name), for one USE of the module: the
    compiler reads pepper_instance_module's module file (its context type has
    hundreds of type-bound procedures) for every USE of it, so a USE in each
    of a thousand routines took gigabytes of RAM (ifx: more than 16 GB)."""
    out, items, i = [], {}, 0
    while i < len(body):
        m = re.match(r'\s*use\s+pepper_instance_module\s*,\s*only\s*:(.*)$', body[i], re.I)
        if not m:
            out.append(body[i])
            i += 1
            continue
        text = m.group(1)
        while text.rstrip().endswith('&'):
            i += 1
            text = text.rstrip()[:-1] + ' ' + body[i].strip().lstrip('&')
        for x in text.split('!')[0].split(','):
            x = re.sub(r'\s+', ' ', x.strip().lower())
            if x:
                local = x.split('=>')[0].strip()
                if items.get(local, x) != x:
                    raise SystemExit(f'pepper_instance_module: {local} is {items[local]} and {x}')
                items[local] = x
        i += 1
    return out, list(items.values())


def wrap(prefix, names, indent='   '):
    """A declaration `prefix name, name, ...`, continued over lines of at
    most 100 characters."""
    out, line = [], indent + prefix
    for i, n in enumerate(names):
        item = n + (', ' if i < len(names) - 1 else '')
        if len(line) + len(item) > 98:
            out.append(line.rstrip() + ' &')
            line = indent + '   '
        line += item
    out.append(line.rstrip())
    return out


def find(lines, pattern, start=0):
    """The index of the first line from START that matches PATTERN."""
    return next(i for i in range(start, len(lines)) if re.match(pattern, lines[i]))


def strip_blank(lines):
    """LINES without leading and trailing blank lines, and with runs of blank
    lines reduced to one."""
    out = []
    for l in lines:
        if l.strip() or (out and out[-1].strip()):
            out.append(l)
    while out and not out[-1].strip():
        out.pop()
    return out


def main():
    lines = MERGED.read_text().split('\n')
    i_ctx = find(lines, r'   ! ---- the default instance')
    i_cb = find(lines, r'   ! ---- the callbacks')
    i_contains = find(lines, r'contains\s*$')
    i_bridges = find(lines, r'\s*(logical\s+)?(function|subroutine)\s+bridge_', i_contains)
    while lines[i_bridges - 1].lstrip().startswith('!'):
        i_bridges -= 1
    i_sections = find(lines, r'   ! ==== ')
    i_end = find(lines, r'end module pepper_legacy')

    ctx_spec = strip_blank(lines[i_ctx + 1:i_cb])
    cb_spec = strip_blank(lines[i_cb + 1:i_contains])
    ctx_body = strip_blank(lines[i_contains + 1:i_bridges])
    cb_body = strip_blank(lines[i_bridges:i_sections])
    instance_use = next(l for l in lines[:i_ctx] if 'use pepper_instance_module, only: pepper_context,' in l)
    callbacks = [x.strip() for x in instance_use.split('only:')[1].split(',') if x.strip() != 'pepper_context']

    # the sections: [(subsystem, lines)]
    sections = []
    for i in range(i_sections, i_end):
        m = re.match(r'   ! ==== (\w+)', lines[i])
        if m:
            sections.append((m.group(1), []))
        elif sections:
            sections[-1][1].append(lines[i])

    files = {}
    files['pepper_legacy_context'] = [
        '!> The default instance of the legacy interface ([[pepper_legacy]]) and its',
        '!> context, the only global state of PEPPER: [[legacy_context]] (with',
        '!> SPICELIB\'s error settings, created on the first call) and',
        '!> [[legacy_spice]], for the routines that change the kernels.',
        'module pepper_legacy_context',
        '',
        '   use pepper_instance_module, only: pepper_instance, pepper_context',
        '',
        '   implicit none',
        '',
        '   private',
        '   public :: legacy_context, legacy_spice',
        '',
        *ctx_spec,
        '',
        'contains',
        '',
        *ctx_body,
        '',
        'end module pepper_legacy_context',
    ]
    bridge_names = [m.group(2) for l in cb_body if (m := HEAD.match(l)) and not l.lstrip().startswith('!')
                    and not re.match(r'\s*end\b', l)]
    current_names = re.findall(r'::\s*(current_\w+)', '\n'.join(cb_spec))
    abstract_names = re.findall(r'^\s*(?:logical\s+)?(?:function|subroutine)\s+(legacy_\w+)', '\n'.join(cb_spec), re.M)
    files['pepper_legacy_callbacks'] = [
        '!> The callbacks of the legacy interface ([[pepper_legacy]]): the',
        '!> interfaces of the routines a SPICELIB program passes (legacy_*, without',
        '!> the context), and the bridges (bridge_*), with the module API\'s',
        '!> interfaces, that call the routine a legacy routine was given (current_*).',
        'module pepper_legacy_callbacks',
        '',
        '   use pepper_kinds, only: ip, wp',
        '   use pepper_instance_module, only: pepper_context',
        *wrap('use pepper_instance_module, only: ', callbacks),
        '',
        '   implicit none',
        '',
        '   private',
        *wrap('public :: ', abstract_names),
        *wrap('public :: ', current_names),
        *wrap('public :: ', bridge_names),
        '',
        *cb_spec,
        '',
        'contains',
        '',
        *cb_body,
        '',
        'end module pepper_legacy_callbacks',
    ]

    # the routines of each subsystem, and the routines a subsystem calls of
    # another (the error routines: SETMSG and SIGERR of errsys); a function
    # of another subsystem would not compile (implicit none), a called
    # subroutine would only fail to link
    owner = {n: name for name, body in sections for n in top_level(body)}
    public = []
    for name, body in sections:
        procs = top_level(body)
        body, instance = hoist_instance_use(body)
        called = {}
        # names the routines take from the library's modules (USE in the
        # routine) are not the legacy routines
        local = {x.split('=>')[0].strip().lower() for l in body
                 if (m := re.match(r'\s*use\s+\w+\s*,\s*only\s*:(.*)', l, re.I))
                 for x in m.group(1).split('!')[0].split(',')}
        for l in body:
            for n in re.findall(r'\bcall\s+(\w+)', l.split('!')[0], re.I):
                o = owner.get(n.lower())
                if o and o != name and n.lower() not in local:
                    called.setdefault(o, set()).add(n.lower())
        uses = wrap('use pepper_instance_module, only: ', instance) if instance else []
        text = '\n'.join(x.split('!')[0] for x in body).lower()
        if re.search(r'\blegacy_(context|spice)\b', text):
            uses.append('   use pepper_legacy_context, only: legacy_context, legacy_spice')
        if re.search(r'\b(bridge|current)_\w+|\blegacy_(?!context\b|spice\b)\w+', text):
            uses.append('   use pepper_legacy_callbacks')
        for o in sorted(called):
            uses.append(f'   use pepper_legacy_{o}, only: {", ".join(sorted(called[o]))}')
        module = f'pepper_legacy_{name}'
        files[module] = [
            f'!> The legacy interface\'s routines of the subsystem {name} ({len(procs)} routines):',
            '!> SPICELIB\'s names and arguments, on the default context. Programs use',
            '!> them through [[pepper_legacy]].',
            f'module {module}',
            '',
            *uses,
            *([''] if uses else []),
            '   implicit none',
            '',
            '   private',
            *wrap('public :: ', procs),
            '',
            'contains',
            '',
            *strip_blank(body),
            '',
            f'end module {module}',
        ]
        public.append(module)

    doc = [l for l in lines[:find(lines, r'module pepper_legacy')] if l.startswith('!>')]
    doc = doc[:doc.index('!> Generated by tools/make_legacy_module.py from the legacy interface of')]
    files['pepper_legacy'] = [
        *doc,
        '!> The routines are in one module per subsystem (pepper_legacy_<subsystem>),',
        '!> the default instance in pepper_legacy_context and the callbacks in',
        '!> pepper_legacy_callbacks (tools/split_legacy_module.py split the merged',
        '!> module of tools/make_legacy_module.py: one module took gigabytes of RAM',
        '!> to compile with ifx); this module uses them all.',
        'module pepper_legacy',
        '',
        '   use pepper_legacy_context, only: legacy_context, legacy_spice',
        *wrap('use pepper_legacy_callbacks, only: ', abstract_names),
        *[f'   use {m}' for m in public],
        '',
        '   implicit none',
        '',
        '   public',
        '',
        'end module pepper_legacy',
    ]

    for module, text in files.items():
        (DIR / f'{module}.f90').write_text('\n'.join(text) + '\n')
    print(f'{len(files)} files in {DIR}: {len(sections)} subsystems')


if __name__ == '__main__':
    main()
