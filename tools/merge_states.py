#!/usr/bin/env python3
"""
Merge the generated state modules (src/pepper_state_*.f90) into one module,
src/pepper_states.f90, with default initialization in the small types.

    python tools/merge_states.py BUILD_DIR

BUILD_DIR is an fpm build directory with the .mod files of the state modules
(e.g. build/gfortran_XXXX): the script compiles a small program there to get
the size of each type.

A one-time conversion (a record, like the other tools here). Each state
module had a type (sometimes two: the instance part and the per-context
part) and a routine init_<name>_state that set the initial values after
allocate: every component first to zero (SPICELIB relies on saved storage
starting as zero), then SPICELIB's DATA values. In the merged module:

* A type of at most SMALL bytes has default initialization (the final value
  of each component in the old routine), and its routine is gone: the
  users' "if (.not. allocated(x)) then; allocate(x); call init_..(x); end
  if" becomes "if (.not. allocated(x)) allocate(x)".
* A larger type keeps its routine (now one assignment per component): for a
  type with default initialization gfortran builds a temporary of the
  whole type on the stack in allocate, and a template of it in the
  executable.
* The parameters of a state stay private. Only those the types and
  initial values use are kept. A name that several states define with
  different values gets the state's name as a prefix (wdsize becomes
  ana_wdsize, ...); one defined identically (a literal) by several states
  is declared once. The include modules are used with only-lists; a name
  that two include modules define with different values is imported under
  an alias (zzsrftrn_nroom => nroom).

It also replaces the generated block of use statements in pepper_instance
and the allocate/init blocks in the submodules. The old modules are then
removed with git rm.
"""

import re
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src"
SMALL = 16384  # bytes: the largest type with default initialization
WIDTH = 110    # wrap a declaration whose code is longer than this

IDENT = re.compile(r"[A-Za-z_]\w*")
NUMBER = re.compile(r"(\d+\.?\d*|\.\d+)([dDeE][+-]?\d+)?(_\w+)?")


# ----------------------------------------------------------------- lexing

def split_comment(line):
    """(code, comment) of a free-form line: the comment starts at the
    first '!' outside a string."""
    q = None
    for i, ch in enumerate(line):
        if q:
            if ch == q:
                q = None
        elif ch in "'\"":
            q = ch
        elif ch == "!":
            return line[:i], line[i:]
    return line, ""


def tokens(code):
    """(kind, text) tokens of code: 'str' (a string), 'id' (a name),
    'comp' (a name after %), 'other' (numbers, operators, blanks)."""
    out = []
    i = 0
    n = len(code)
    while i < n:
        ch = code[i]
        if ch in "'\"":
            j = i + 1
            while j < n:
                if code[j] == ch:
                    if j + 1 < n and code[j + 1] == ch:
                        j += 2
                        continue
                    break
                j += 1
            out.append(("str", code[i:j + 1]))
            i = j + 1
        elif ch.isdigit() or (ch == "." and i + 1 < n and code[i + 1].isdigit()):
            # a number, with its exponent and kind (1.d-13, 0.0_wp)
            m = NUMBER.match(code, i)
            out.append(("other", m.group()))
            i = m.end()
        elif ch.isalpha() or ch == "_":
            m = IDENT.match(code, i)
            j = len(out) - 1
            while j >= 0 and out[j] == ("other", " "):
                j -= 1
            kind = "comp" if j >= 0 and out[j] == ("other", "%") else "id"
            out.append((kind, m.group()))
            i = m.end()
        else:
            out.append(("other", ch))
            i += 1
    return out


def idents(code):
    """The names (lower case) referenced in code (not components, not in
    strings)."""
    return {t.lower() for k, t in tokens(code) if k == "id"}


def rename(code, mapping):
    """Rename the names of code (not components, not in strings)."""
    if not mapping:
        return code
    return "".join(mapping.get(t.lower(), t) if k == "id" else t for k, t in tokens(code))


def split_top(text, sep=","):
    """Split at the separators outside strings and parentheses/brackets."""
    parts, depth, cur = [], 0, ""
    for k, t in tokens(text):
        if k == "other" and t in "([":
            depth += 1
        elif k == "other" and t in ")]":
            depth -= 1
        if k == "other" and t == sep and depth == 0:
            parts.append(cur)
            cur = ""
        else:
            cur += t
    parts.append(cur)
    return parts


def split_commas_anywhere(text):
    """Pieces of text that end with a comma outside strings (any depth)."""
    parts, cur = [], ""
    for k, t in tokens(text):
        cur += t
        if k == "other" and t == ",":
            parts.append(cur)
            cur = ""
    parts.append(cur)
    return parts


def wrap(first, rest, indent):
    """first + rest as free-form lines of at most about WIDTH characters,
    broken after commas."""
    lines, cur = [], first
    for p in split_commas_anywhere(rest):
        if len(cur) + len(p) > WIDTH and cur != first:
            lines.append(cur.rstrip() + " &")
            cur = " " * indent + p.lstrip()
        else:
            cur += p
    lines.append(cur.rstrip())
    return lines


def reflow(text, width=72, prefix="   !> "):
    """text as comment lines of at most width characters (after prefix)."""
    lines, cur = [], ""
    for w in text.split():
        if cur and len(cur) + 1 + len(w) > width:
            lines.append(prefix + cur)
            cur = w
        else:
            cur = f"{cur} {w}" if cur else w
    if cur:
        lines.append(prefix + cur)
    return lines


# ------------------------------------------------------------- the modules

def inc_exports():
    """name -> {include module: value}, for the parameters of src/inc."""
    out = defaultdict(dict)
    for f in sorted((SRC / "inc").glob("pepper_inc_*.f90")):
        mod = f.stem
        code = "\n".join(split_comment(l)[0] for l in f.read_text().splitlines())
        code = re.sub(r"&\s*\n\s*&?", " ", code)
        stmts = [m.group(1) for m in re.finditer(r"\bparameter\s*\((.*?)\)\s*$", code, re.M | re.I)]
        stmts += [m.group(1) for m in re.finditer(r",\s*parameter\s*::(.*)$", code, re.M | re.I)]
        for s in stmts:
            for part in split_top(s):
                n = re.match(r"\s*(\w+)\s*=(.*)$", part, re.S)
                if n:
                    out[n.group(1).lower()][mod] = n.group(2).replace(" ", "").lower()
    return out


class Block:
    """One old state module."""

    def __init__(self, path):
        self.path = path
        self.base = path.stem[len("pepper_state_"):]
        text = path.read_text()
        head, rest = text.split("\nmodule ", 1)
        self.header = [l[2:].lstrip() if l.startswith("!>") else l for l in head.splitlines()]
        self.incs = re.findall(r"^\s*use (pepper_inc_\w+)\s*$", rest, re.M)
        decl = rest.split("\n   private\n", 1)[1].split("\ncontains\n")[0]
        body = rest.split("\ncontains\n")[1]
        # the types
        type_re = r"((?:   !>[^\n]*\n)*)   type, public :: (\w+)\n(.*?)   end type \2\n"
        self.types = []
        for m in re.finditer(type_re, decl, re.S):
            doc = [l.strip()[2:].lstrip() for l in m.group(1).splitlines()]
            comps = []
            for l in m.group(3).splitlines():
                code, comment = split_comment(l)
                if code.strip():
                    cm = re.match(r"^\s*(.*?)::\s*(\w+)\s*(\(.*\))?\s*$", code)
                    comps.append({"spec": cm.group(1).strip(), "name": cm.group(2),
                                  "dims": (cm.group(3) or "").strip(), "doc": [comment.strip()]})
                else:
                    comps[-1]["doc"].append(comment.strip())
            self.types.append({"name": m.group(2), "doc": doc, "comps": comps})
        decl = re.sub(type_re, "", decl, flags=re.S)
        # the parameters, in order (with the comment lines just before)
        self.params = []
        pending = []
        typed = {}
        for l in decl.splitlines():
            s = l.strip()
            if not s or s.startswith("public ::"):
                continue
            if s.startswith("!"):
                pending.append(s)
                continue
            m = re.match(r"^(.*?),\s*parameter\s*::\s*(\w+)\s*=\s*(.*)$", s, re.I)
            if m:
                self.params.append({"type": m.group(1), "name": m.group(2).lower(),
                                    "expr": m.group(3).strip(), "comments": pending})
                pending = []
                continue
            m = re.match(r"^(.*?)\s*::\s*(\w+)$", s)
            if m:
                typed[m.group(2).lower()] = m.group(1)
                continue
            m = re.match(r"^parameter\s*\(\s*(\w+)\s*=\s*(.*)\)$", s, re.I)
            if m:
                self.params.append({"type": typed[m.group(1).lower()], "name": m.group(1).lower(),
                                    "expr": m.group(2).strip(), "comments": pending})
                pending = []
                continue
            raise SystemExit(f"{path}: unexpected declaration: {s}")
        # the initial values (the last assignment of each component)
        for t in self.types:
            m = re.search(r"subroutine init_%s\(state\)\n(.*?)end subroutine" % t["name"], body, re.S)
            values = {}
            for l in m.group(1).splitlines():
                code = split_comment(l)[0].strip()
                a = re.match(r"^state%(\w+)\s*=\s*(.*)$", code)
                if a:
                    values[a.group(1).lower()] = a.group(2).strip()
            for c in t["comps"]:
                c["value"] = values[c["name"].lower()]
        self.map = {}

    def refs(self):
        """The names the types, initial values and kept parameters use."""
        r = set()
        for t in self.types:
            for c in t["comps"]:
                r |= idents(c["spec"]) | idents(c["dims"]) | idents(c["value"])
        for p in getattr(self, "kept", []):
            r |= idents(p["expr"])
        return r

    def purpose_and_where(self):
        """The NAIF purpose lines and the 'state of ...' paragraph."""
        h = self.header
        i = next(k for k, l in enumerate(h) if l.startswith("The state of SPICELIB"))
        purpose = h[:i]
        while purpose and not purpose[-1].strip():
            purpose.pop()
        where = " ".join(l.strip() for l in h[i:])
        where = re.sub(r"\s*Generated by\s+tools/modularize_ctx\.py from the\s+saved variables of \w+\.f90\.",
                       "", where)
        where = where.replace("The state of SPICELIB's", "The saved variables of SPICELIB's")
        return purpose, where


def type_sizes(build, blocks):
    """The storage size in bytes of every type, from a program compiled
    against the .mod files in build."""
    lines = ["program sizes"]
    names = []
    for b in blocks:
        for t in b.types:
            lines.append(f"   use pepper_state_{b.base}, only: {t['name']}")
            names.append(t["name"])
    lines.append("   implicit none")
    for i, n in enumerate(names):
        lines.append(f"   type({n}), allocatable :: v{i}")
    for i, n in enumerate(names):
        lines.append(f"   print '(a,1x,i0)', '{n}', storage_size(v{i})/8")
    lines.append("end program sizes")
    work = ROOT / "build" / "merge_states"
    work.mkdir(parents=True, exist_ok=True)
    (work / "sizes.f90").write_text("\n".join(lines) + "\n")
    subprocess.run(["gfortran", "-I", str(build), "sizes.f90", "-o", "sizes"], cwd=work, check=True)
    out = subprocess.run(["./sizes"], cwd=work, check=True, capture_output=True, text=True).stdout
    return {l.split()[0]: int(l.split()[1]) for l in out.splitlines()}


def default_value(value, comps, mapping):
    """The default initialization of a component from its value in the old
    routine (len(state%x) and shape(state%x) become constants)."""
    def length(m):
        c = comps[m.group(1).lower()]
        lm = re.match(r"character\((.*)\)$", c["spec"].replace(" ", ""), re.I)
        return f"character({lm.group(1)})"

    value = re.sub(r"character\(\s*len\(\s*state%(\w+)\s*\)\s*\)", length, value)

    def shape(m):
        dims = comps[m.group(1).lower()]["dims"].strip()[1:-1]
        ext = []
        for d in split_top(dims):
            if ":" in d:
                lo, hi = d.split(":")
                ext.append(f"{hi.strip()} - ({lo.strip()}) + 1")
            else:
                ext.append(d.strip())
        return "[ " + ", ".join(ext) + " ]"

    value = re.sub(r"shape\s*\(\s*state%(\w+)\s*\)", shape, value)
    if "state%" in value:
        raise SystemExit(f"unexpected value: {value}")
    return rename(value, mapping)


# ------------------------------------------------------------- generation

def main():
    build = Path(sys.argv[1]).resolve()
    blocks = [Block(p) for p in sorted(SRC.glob("pepper_state_*.f90"))]
    sizes = type_sizes(build, blocks)
    exports = inc_exports()
    type_names = {t["name"].lower() for b in blocks for t in b.types}

    # the parameters each block needs (closure over the expressions)
    for b in blocks:
        byname = {p["name"]: p for p in b.params}
        need, todo = set(), [r for r in b.refs() if r in byname]
        while todo:
            n = todo.pop()
            if n in need:
                continue
            need.add(n)
            todo += [r for r in idents(byname[n]["expr"]) if r in byname]
        b.kept = [p for p in b.params if p["name"] in need]

    # shared (identical literal) and conflicting names
    defs = defaultdict(list)
    for b in blocks:
        for p in b.kept:
            defs[p["name"]].append((b, p))
    shared, prefixed = {}, set()
    for name, ds in defs.items():
        exprs = {(p["type"].replace(" ", "").lower(), p["expr"].replace(" ", "").lower()) for _, p in ds}
        literal = all(not (idents(p["expr"]) - {"ip", "wp"}) for _, p in ds)
        clash = name in exports or name in type_names or name in ("ip", "wp")
        if len(ds) > 1 and len(exprs) == 1 and literal and not clash:
            shared[name] = ds[0][1]
        elif len(ds) > 1 or clash:
            prefixed.add(name)
    for b in blocks:
        b.map = {p["name"]: f"{b.base}_{p['name']}" for p in b.kept if p["name"] in prefixed}
        b.own = [p for p in b.kept if p["name"] not in shared]

    # the names taken from the include modules
    imports = defaultdict(set)
    for b in blocks:
        b.inc_of = {}
        local = {p["name"] for p in b.kept}
        for r in sorted(b.refs() - local):
            mods = [m for m in b.incs if m in exports.get(r, {})]
            if mods:
                imports[mods[0]].add(r)
                b.inc_of[r] = mods[0]
    taken = defaultdict(set)
    for m, names in imports.items():
        for n in names:
            taken[n].add(m)
    for n, ms in sorted(taken.items()):
        if len(ms) == 1:
            continue
        ms = sorted(ms)
        if len({exports[n][m] for m in ms}) == 1:
            # the same value in several include modules: take it from one
            for m in ms[1:]:
                imports[m].discard(n)
            print(f"note: {n} is in {ms} (same value); taken from {ms[0]}")
        else:
            # different values: import each under an alias
            for m in ms:
                alias = f"{m[len('pepper_inc_'):]}_{n}"
                imports[m].discard(n)
                imports[m].add(f"{alias} => {n}")
                for b in blocks:
                    if b.inc_of.get(n) == m:
                        b.map[n] = alias
            print(f"note: {n} has different values in {ms}; imported with aliases")

    big = [t["name"] for b in blocks for t in b.types if sizes[t["name"]] > SMALL]
    out = []
    w = out.append
    w("!> The saved state of SPICELIB's routines: one derived type for each")
    w("!> routine (or umbrella routine with its entry points) whose saved")
    w("!> variables are real state. Each is an allocatable component of")
    w("!> pepper_spice (the state of the instance, e.g. the kernel pool) or of")
    w("!> pepper_context (the state of a computation in progress), allocated on")
    w("!> first use.")
    w("!>")
    w("!> The components start with SPICELIB's DATA values, and the others as")
    w("!> zero (SPICELIB relies on saved storage starting as zero). A type of")
    w(f"!> at most {SMALL // 1024} KiB has default initialization. A larger one has a")
    w("!> routine init_<name>_state that sets the initial values after")
    w("!> allocate: for a type with default initialization, gfortran builds a")
    w("!> temporary of the whole type on the stack in allocate (and a template")
    w("!> of it in the executable).")
    w("!>")
    w("!> The parameters are private. Those that several states define with")
    w("!> different values have the name of the state as a prefix.")
    w("!> (tools/merge_states.py merged the modules pepper_state_<name> that")
    w("!> tools/modularize_ctx.py had generated.)")
    w("module pepper_states")
    w("")
    w("   use pepper_kinds, only: ip, wp")
    for m in sorted(imports):
        names = sorted(imports[m])
        if names:
            out.extend(wrap(f"   use {m}, only: ", ", ".join(names), 9))
    w("")
    w("   implicit none")
    w("")
    w("   private")
    w("")
    out.extend(wrap("   public :: ", ", ".join(f"init_{n}" for n in big), 7))
    w("")
    if shared:
        w("   ! parameters of several states")
        for name in sorted(shared):
            p = shared[name]
            w(f"   {p['type']}, parameter :: {name} = {p['expr']}")
        w("")

    for b in blocks:
        purpose, where = b.purpose_and_where()
        w(f"   ! {'-' * 66} {b.base.upper()}")
        w("")
        for p in b.own:
            for c in p["comments"]:
                w(f"   {c}")
            w(f"   {p['type']}, parameter :: {b.map.get(p['name'], p['name'])} = {rename(p['expr'], b.map)}")
        if b.own:
            w("")
        for k, t in enumerate(b.types):
            if k == 0:
                for l in purpose:
                    w(f"   !> {l}".rstrip())
                w("   !>")
                out.extend(reflow(where))
                if t["doc"] and not t["doc"][0].startswith("The saved variables of"):
                    w("   !>")
                    for l in t["doc"]:
                        w(f"   !> {l}".rstrip())
            else:
                for l in t["doc"]:
                    w(f"   !> {l}".rstrip())
            small = sizes[t["name"]] <= SMALL
            if not small:
                w("   !>")
                w(f"   !> No default initialization ({sizes[t['name']]} bytes): see init_{t['name']}.")
            w(f"   type, public :: {t['name']}")
            lens = {c["name"].lower(): c for c in t["comps"]}
            for c in t["comps"]:
                spec = rename(c["spec"], b.map)
                dims = rename(c["dims"], b.map)
                code = f"      {spec} :: {c['name']}" + (f" {dims}" if dims else "")
                if small:
                    code += f" = {default_value(c['value'], lens, b.map)}"
                if len(code) <= WIDTH:
                    w(f"{code}  {c['doc'][0]}")
                    pad = len(code) + 2
                    for d in c["doc"][1:]:
                        w(" " * pad + d)
                else:
                    head, value = code.split(" = ", 1)
                    for d in c["doc"]:
                        w("      !> " + d[2:].lstrip())
                    out.extend(wrap(head + " = ", value, 9))
            w(f"   end type {t['name']}")
            w("")

    w("contains")
    w("")
    for b in blocks:
        for t in b.types:
            if sizes[t["name"]] <= SMALL:
                continue
            w(f"   !> Set the initial values of a new {t['name']}: SPICELIB's DATA")
            w("   !> values, and zero for the other components.")
            w(f"   pure subroutine init_{t['name']}(state)")
            w(f"      type({t['name']}), intent(inout) :: state  !! the state")
            for c in t["comps"]:
                out.extend(wrap(f"      state%{c['name']} = ", rename(c["value"], b.map), 9))
            w(f"   end subroutine init_{t['name']}")
            w("")
    w("end module pepper_states")
    (SRC / "pepper_states.f90").write_text("\n".join(out) + "\n")

    # pepper_instance: one use statement
    inst = SRC / "pepper_instance.f90"
    text = inst.read_text()
    first = "   ! The state types of the subsystems (generated modules pepper_state_*).\n"
    last = "   ! <<< generated state modules\n"
    if first in text:
        a = text.index(first)
        z = text.index(last) + len(last)
        text = text[:a] + ("   ! The state types of the subsystems, and the routines that set the\n"
                           "   ! initial values of the large ones.\n"
                           "   use pepper_states\n") + text[z:]
        inst.write_text(text)

    # the users: no init call for the small types
    small = {t["name"] for b in blocks for t in b.types if sizes[t["name"]] <= SMALL}
    pat = re.compile(r"^(\s*)if \( \.not\. allocated \( ([\w%]+) \) \) then\n"
                     r"\s*allocate \( \2 \)\n"
                     r"\s*call init_(\w+) \( \2 \)\n"
                     r"\s*end if\n", re.M)
    total = 0
    for f in sorted([*SRC.glob("*.f90"), *SRC.glob("*.F90")]):
        text = f.read_text()

        def repl(m):
            if m.group(3) in small:
                return f"{m.group(1)}if ( .not. allocated ( {m.group(2)} ) ) allocate ( {m.group(2)} )\n"
            return m.group(0)

        new, n = pat.subn(repl, text)
        if new != text:
            f.write_text(new)
        total += sum(1 for m in pat.finditer(text) if m.group(3) in small)
    left = subprocess.run(["grep", "-rn", r"init_\w*_state", str(SRC)], capture_output=True, text=True).stdout
    left = [l for l in left.splitlines()
            if not any(re.search(rf"\binit_{n}\b", l) for n in big) and "/pepper_state_" not in l]
    print(f"{len(blocks)} modules, {sum(len(b.types) for b in blocks)} types "
          f"({len(big)} with init routines); {total} allocate blocks; "
          f"{len(shared)} shared and {len(prefixed)} prefixed parameter names")
    for l in left:
        print("left:", l)
    print("now remove the old modules: git rm src/pepper_state_*.f90")


if __name__ == "__main__":
    main()
