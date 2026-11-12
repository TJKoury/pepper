#!/usr/bin/env python3
"""Convert a SPICE text kernel to a JSON kernel, or (--reverse) a JSON
kernel to a text kernel, for SPICELIB and other toolkits.

    python tools/tk2json.py pck00011.tpc                 # writes pck00011.json
    python tools/tk2json.py pck00011.tpc -o out.json
    python tools/tk2json.py --reverse masses.json        # writes masses.tpc

Text kernel -> JSON keeps the assignments in their order (`+=` becomes a
"+NAME" key), the `@` times as "@..." strings, and the comment blocks as
`//` lines where they were. Numbers are written so that they read back
exactly (1.0D3 becomes 1000.0).

JSON -> text kernel writes one `NAME = ( ... )` per key ("+NAME": `+=`) in a
\\begindata block, after the leading `//` comment lines (as the kernel's
comments). The JSON's other comments are dropped. A time with a blank
("@2000-01-01 12:00") cannot be written: a text kernel's `@` value is one
token (write "@2000-01-01T12:00"). Text kernel lines are at most 132
characters; a longer string is an error.

The format is PEPPER's JSON kernel (src/pepper_json.f90; the README's
"JSON kernels").
"""

import argparse
import json
import math
import re
import sys
from pathlib import Path


# ---- text kernel -> JSON

def _tokens(data):
    """The tokens of a \\begindata block's text: names, '=', '+=', '(',
    ')', strings (('str', value)), and values (numbers, @times)."""
    out = []
    i = 0
    n = len(data)
    while i < n:
        ch = data[i]
        if ch in ' \t\r\n,':
            i += 1
        elif ch == "'":
            j = i + 1
            s = ''
            while True:
                k = data.find("'", j)
                if k < 0:
                    raise ValueError('a string has no closing quote')
                s += data[j:k]
                if data[k + 1:k + 2] == "'":       # '' is a quote
                    s += "'"
                    j = k + 2
                else:
                    i = k + 1
                    break
            out.append(('str', s))
        elif ch in '()':
            out.append(ch)
            i += 1
        elif data.startswith('+=', i):
            out.append('+=')
            i += 2
        elif ch == '=':
            out.append('=')
            i += 1
        else:
            m = re.compile(r"[^\s,()=']+").match(data, i)
            tok = m.group(0)
            # a name followed by += (no blank): NAME+=
            if tok.endswith('+') and data.startswith('=', m.end()):
                tok = tok[:-1]
                out.append(tok)
                i = m.end() - 1
                continue
            out.append(tok)
            i = m.end()
    return out


def _number(tok):
    """A text kernel number as a JSON value (exact)."""
    t = tok.upper().replace('D', 'E')
    if re.fullmatch(r'[+-]?\d+', t):
        return int(t)
    x = float(t)
    if math.isinf(x) or math.isnan(x):
        raise ValueError(f'the number {tok} has no JSON form')
    return x


def _assignments(data):
    """[(name, append, [values])] of a \\begindata block's text; a value
    is a float/int, a str, or ('@', time)."""
    toks = _tokens(data)
    out = []
    i = 0
    while i < len(toks):
        name = toks[i]
        if not isinstance(name, str) or name in ('=', '+=', '(', ')'):
            raise ValueError(f'expected a variable name, found {name!r}')
        if i + 1 >= len(toks) or toks[i + 1] not in ('=', '+='):
            raise ValueError(f'expected = or += after {name}')
        append = toks[i + 1] == '+='
        i += 2
        vals = []
        if i < len(toks) and toks[i] == '(':
            i += 1
            while i < len(toks) and toks[i] != ')':
                vals.append(toks[i])
                i += 1
            if i >= len(toks):
                raise ValueError(f'the values of {name} have no closing parenthesis')
            i += 1
        else:
            vals.append(toks[i])
            i += 1
        conv = []
        for v in vals:
            if isinstance(v, tuple):
                conv.append(v[1])
            elif v.startswith('@'):
                conv.append(('@', v[1:]))
            else:
                conv.append(_number(v))
        out.append((name, append, conv))
    return out


def _json_value(v):
    if isinstance(v, tuple):
        return json.dumps('@' + v[1])
    if isinstance(v, float):
        return repr(v)
    return json.dumps(v)


def tk_to_json(text):
    """A text kernel's text as a JSON kernel's."""
    blocks = []         # ('comment', text) and ('data', text), in order
    mode = 'comment'
    cur = []
    for line in text.splitlines():
        s = line.strip()
        if s == '\\begindata':
            blocks.append((mode, '\n'.join(cur)))
            mode, cur = 'data', []
        elif s == '\\begintext':
            blocks.append((mode, '\n'.join(cur)))
            mode, cur = 'comment', []
        else:
            cur.append(line.rstrip())
    blocks.append((mode, '\n'.join(cur)))

    lines = []
    entries = []        # (kind, payload), with comments where they were
    started = False
    for kind, body in blocks:
        if kind == 'comment':
            c = body.strip('\n')
            if c.strip():
                target = entries if started else None
                cl = ['// ' + x if x else '//' for x in c.split('\n')]
                if target is None:
                    lines += cl
                else:
                    target.append(('comment', cl))
        else:
            started = True
            for name, append, vals in _assignments(body):
                entries.append(('var', (name, append, vals)))
    lines.append('{')
    nvars = sum(1 for k, _ in entries if k == 'var')
    seen = 0
    for kind, payload in entries:
        if kind == 'comment':
            lines += ['    ' + x for x in payload]
            continue
        name, append, vals = payload
        seen += 1
        key = json.dumps(('+' if append else '') + name)
        if len(vals) == 1:
            value = _json_value(vals[0])
        else:
            value = '[' + ', '.join(_json_value(v) for v in vals) + ']'
            if len(value) > 100:
                value = '[\n        ' + ',\n        '.join(_json_value(v) for v in vals) + '\n    ]'
        lines.append(f'    {key}: {value}' + (',' if seen < nvars else ''))
    lines.append('}')
    return '\n'.join(lines) + '\n'


# ---- JSON -> text kernel

def _strip_json(text):
    """A JSON kernel's text without its // comments and trailing commas
    (standard JSON), and its leading comment lines."""
    if text.startswith('﻿'):
        text = text[1:]
    out = []
    lead = []
    i = 0
    n = len(text)
    in_lead = True
    while i < n:
        ch = text[i]
        if ch == '"':
            j = i + 1
            while j < n and text[j] != '"':
                j += 2 if text[j] == '\\' else 1
            out.append(text[i:j + 1])
            i = j + 1
            in_lead = False
        elif text.startswith('//', i):
            k = text.find('\n', i)
            k = n if k < 0 else k
            if in_lead:
                lead.append(text[i + 2:k].removeprefix(' '))
            i = k
        elif text.startswith('/*', i):
            raise ValueError('block comments (/* */) are not supported')
        else:
            if not ch.isspace():
                in_lead = False
            out.append(ch)
            i += 1
    s = ''.join(out)
    s = re.sub(r',(\s*[}\]])', r'\1', s)
    return s, lead


def _tk_value(name, v):
    if isinstance(v, bool):
        return '1' if v else '0'
    if isinstance(v, (int, float)):
        r = repr(float(v)) if isinstance(v, float) else str(v)
        return r.upper()
    if isinstance(v, str):
        if v.startswith('@'):
            if not v[1:] or re.search(r'[\s,()]', v[1:]):
                raise ValueError(f'{name}: the time "{v}" has a blank or a separator; a text kernel '
                                 'cannot hold it (use no blanks, e.g. 2000-01-01T12:00)')
            return v
        return "'" + v.replace("'", "''") + "'"
    raise ValueError(f'{name}: the value {v!r} is not a number, string or time')


def json_to_tk(text, kpl='PCK'):
    """A JSON kernel's text as a text kernel's."""
    body, lead = _strip_json(text)
    pairs = json.loads(body, object_pairs_hook=lambda p: p)
    if not isinstance(pairs, list):
        raise ValueError('the kernel must be one JSON object')
    lines = [f'KPL/{kpl}', ''] + lead + ['', '\\begindata', '']
    for key, value in pairs:
        append = key.startswith('+')
        name = key[1:] if append else key
        vals = value if isinstance(value, list) else [value]
        if isinstance(value, list) and any(isinstance(x, list) for x in value) or not vals:
            raise ValueError(f'{name}: nested or empty arrays are not supported')
        tv = [_tk_value(name, v) for v in vals]
        op = '+=' if append else '='
        one = f'{name} {op} ( ' + ' '.join(tv) + ' )' if len(tv) > 1 else f'{name} {op} {tv[0]}'
        if len(one) <= 132:
            lines.append(one)
        else:
            lines.append(f'{name} {op} (')
            for t in tv:
                if len(t) + 6 > 132:
                    raise ValueError(f'{name}: a value is longer than a text kernel line (132 characters)')
                lines.append('      ' + t)
            lines.append('      )')
    lines += ['', '\\begintext', '']
    return '\n'.join(lines)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('input', type=Path, help='the kernel to convert')
    ap.add_argument('-o', '--output', type=Path, help='the converted kernel (default: the input with the '
                    'other extension, .json or .tpc)')
    ap.add_argument('--reverse', action='store_true', help='JSON kernel -> text kernel')
    ap.add_argument('--kpl', default='PCK', help='the text kernel\'s KPL/ type (--reverse; default PCK)')
    args = ap.parse_args()
    text = args.input.read_text()
    try:
        if args.reverse:
            out = json_to_tk(text, args.kpl)
            dest = args.output or args.input.with_suffix('.tpc')
        else:
            out = tk_to_json(text)
            dest = args.output or args.input.with_suffix('.json')
    except ValueError as e:
        sys.exit(f'{args.input}: {e}')
    if dest.resolve() == args.input.resolve():
        sys.exit(f'{dest}: the output would overwrite the input')
    dest.write_text(out)
    print(dest)


if __name__ == '__main__':
    main()
