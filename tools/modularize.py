#!/usr/bin/env python3
"""
Move SPICELIB routines from src/spicelib into PEPPER modules, and generate
the legacy wrappers that keep the original external procedures.

    python tools/modularize.py [--dry-run] MODULE ...

For each MODULE (a key of modules.MODULES, e.g. pepper_math):

- src/<MODULE>.f90 is written: the module, with each routine of its list as
  a module procedure. The routine's text is kept, except that
    * calls of constant functions (``pi()``) become the named constants of
      pepper_constants (``pi``),
    * declarations (type declarations, ``external``) of the module procedures
      and constants it uses are removed,
    * routines and constants from other modules are imported with
      ``use <module>, only: ...``,
    * the final ``end`` becomes ``end subroutine <name>`` / ``end function``.
- src/legacy/pepper_legacy_<name>.f90 is written: for each routine an external
  procedure with the original name and argument declarations that calls the
  module procedure (SPICELIB's interface, for tspice and existing callers).
- The routine's file in src/spicelib is deleted.

pepper_constants is written by hand (constants as parameters); its legacy
wrappers (functions such as ``pi()``) are generated.

If src/<MODULE>.f90 exists already, the routines of its list that are
still in src/spicelib are added to it (and their wrappers to the legacy
file).

Modules must be given in dependency order of modules.MODULES; a module may
use only modules that come before it. A call to a routine that is in a
module that has not been generated yet is an error.
"""

import argparse
import re
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
SRC = ROOT / "src"
SPICELIB = SRC / "spicelib"
sys.path.insert(0, str(Path(__file__).resolve().parent))
from lowercase_code import map_code  # noqa: E402
from modules import MODULES, CONSTANTS  # noqa: E402

TYPESPEC = re.compile(
    r"(integer\s*\(\s*ip\s*\)|real\s*\(\s*wp\s*\)|logical|"
    r"character\s*(?:\*\s*\([^()]*\)|\*\s*\d+|\([^()]*\))?)", re.I)
HEADER = re.compile(r"^\s*(?:(?P<prefix>[\w*()\s,]*?)\s+)?(?P<kind>subroutine|function)"
                    r"\s+(?P<name>\w+)\s*(?:\((?P<args>[^()]*)\))?", re.I)
SPEC_START = re.compile(r"\s*(use|implicit|include|parameter|external|intrinsic)\b", re.I)


# ---------------------------------------------------------------------------
# Source parsing

def code_lines(text: str) -> list:
    """Per line: the code with character literals kept and comments and
    preprocessor directives removed (literal-aware across continuations)."""
    out = []
    for line in map_code(text, lambda s: "\x01" + s + "\x02").split("\n"):
        if line.startswith("#"):
            out.append("")
            continue
        cut = line.find("\x02!")
        if cut >= 0:
            line = line[:cut]
        out.append(line.replace("\x01", "").replace("\x02", "").rstrip())
    return out


class Stmt:
    def __init__(self, first, last, text):
        self.first, self.last, self.text = first, last, text   # line indices

    @property
    def low(self):
        return self.text.lower()


def raw_statements(text: str) -> list:
    """Code statements as Stmt(first line, last line, joined text)."""
    out, start, buf = [], None, ""
    for i, line in enumerate(code_lines(text)):
        code = line.strip()
        if not code:
            continue
        if start is None:
            start = i
        if buf and code.startswith("&"):
            code = code[1:].lstrip()
        if code.endswith("&"):
            buf += code[:-1].rstrip() + " "
            continue
        out.append(Stmt(start, i, (buf + code).strip()))
        start, buf = None, ""
    return out


def balanced(s: str, k: int) -> str:
    """The text inside the parentheses that open at s[k]."""
    depth = 0
    for j in range(k, len(s)):
        depth += {"(": 1, ")": -1}.get(s[j], 0)
        if depth == 0:
            return s[k + 1:j]
    return s[k + 1:]


def split_top(s: str) -> list:
    """Split s at commas outside parentheses."""
    parts, depth, cur, quote = [], 0, "", None
    for ch in s:
        if quote:
            if ch == quote:
                quote = None
            cur += ch
            continue
        if ch in "'\"":
            quote = ch
        elif ch in "([":
            depth += 1
        elif ch in ")]":
            depth -= 1
        if ch == "," and depth == 0:
            parts.append(cur)
            cur = ""
        else:
            cur += ch
    parts.append(cur)
    return [p.strip() for p in parts if p.strip()]


def parse_decl(text: str):
    """(typespec, attributes or None, [entities]) for a type declaration,
    else None."""
    m = TYPESPEC.match(text)
    if not m:
        return None
    rest = text[m.end():]
    if rest and not (rest[0].isspace() or rest[0] in ",:"):
        return None          # e.g. 'integer_x = 1' (not a declaration)
    if "::" in rest:
        attrs, ents = rest.split("::", 1)
        return m.group(1), attrs.strip().lstrip(",").strip(), split_top(ents)
    if rest.lstrip().startswith(","):
        return None
    if re.match(r"\s*function\b", rest, re.I):
        return None          # function statement prefix
    return m.group(1), None, split_top(rest)


def entity_name(ent: str) -> str:
    return re.match(r"\s*(\w+)", ent).group(1).lower()


class Routine:
    def __init__(self, name: str, preprocessed_ok: bool = False):
        paths = sorted(SPICELIB.glob(name + ".[fF]90"))
        if len(paths) != 1:
            raise SystemExit(f"modularize: no unique source file for {name}")
        self.path = paths[0]
        if self.path.suffix == ".F90" and not preprocessed_ok:
            raise SystemExit(f"modularize: {self.path.name} is preprocessed (not supported)")
        self.name = name
        text = self.path.read_text(encoding="ascii")
        self.lines = text.split("\n")
        self.stmts = raw_statements(text)
        h = HEADER.match(self.stmts[0].text)
        if not h or h.group("name").lower() != name:
            raise SystemExit(f"modularize: {self.path.name}: unexpected first statement")
        self.kind = h.group("kind").lower()
        self.args = [a.strip().lower() for a in (h.group("args") or "").split(",") if a.strip()]
        self.prefix = (h.group("prefix") or "").strip()
        # The specification part: statements up to the first executable one.
        self.spec = []
        for s in self.stmts[1:]:
            if SPEC_START.match(s.text) or parse_decl(s.text):
                self.spec.append(s)
            elif re.match(r"\s*(save|data|dimension|equivalence|common|entry)\b", s.text, re.I):
                raise SystemExit(f"modularize: {self.path.name}: unsupported statement: {s.text}")
            else:
                break
        last = self.stmts[-1]
        if last.low != "end":
            raise SystemExit(f"modularize: {self.path.name}: last statement is not END")
        self.end = last

    def references(self) -> set:
        """Names called or referenced as functions (lowercase). Names that
        are variables here (arguments, arrays, character variables used
        with substrings) are not references."""
        variables = set(self.args)
        chars = set()
        for s in self.spec:
            decl = parse_decl(s.text)
            if decl:
                for e in decl[2]:
                    if "(" in e.split("=")[0]:
                        variables.add(entity_name(e))
                    elif decl[0].lower().startswith("character"):
                        chars.add(entity_name(e))
        names = set()
        for s in self.stmts[1:]:
            code = re.sub(r"'[^']*'|\"[^\"]*\"", "''", s.low)
            names.update(re.findall(r"\bcall\s+(\w+)", code))
            for m in re.finditer(r"\b([a-z_]\w*)\s*\(", code):
                n = m.group(1)
                if n in chars and ":" in balanced(code, m.end() - 1):
                    variables.add(n)
                names.add(n)
        return names - variables


# ---------------------------------------------------------------------------
# Generation

def module_of() -> dict:
    """Procedure name -> stateless module (generated and hand-written)."""
    from modules import HANDWRITTEN
    out = {r: m for m, rs in MODULES.items() for r in rs}
    out.update({r: m for m, rs in HANDWRITTEN.items() for r in rs})
    return out


def remove_entities(r: Routine, drop: set, out_lines: list) -> None:
    """Remove the declarations of the names in drop from the routine's
    specification part (edits out_lines, a copy of r.lines, in place)."""
    for s in r.spec:
        kw = re.match(r"\s*(external)\b", s.text, re.I)
        decl = None if kw else parse_decl(s.text)
        if kw:
            ents = split_top(s.text[kw.end():])
        elif decl:
            ents = decl[2]
        else:
            continue
        keep = [e for e in ents if entity_name(e) not in drop]
        if len(keep) == len(ents):
            continue
        if s.first != s.last and keep:
            raise SystemExit(f"modularize: {r.path.name}: cannot edit a continued "
                             f"declaration: {s.text}")
        if not keep:
            for i in range(s.first, s.last + 1):
                out_lines[i] = None
            continue
        line = out_lines[s.first]
        indent = line[:len(line) - len(line.lstrip())]
        if kw:
            out_lines[s.first] = f"{indent}external {', '.join(keep)}"
        else:
            spec, attrs, _ = decl
            head = spec + (f", {attrs}" if attrs else "") + (" :: " if attrs is not None else "")
            m = re.match(r"(\s*\S.*?\S?)(\s+)" + re.escape(ents[0]), line)
            pad = " " if attrs is not None else (m.group(2) if m else " ")
            sep = "" if attrs is not None else pad
            out_lines[s.first] = f"{indent}{head}{sep}{', '.join(keep)}"


def module_procedure(r: Routine, mod: str, where: dict) -> str:
    refs = r.references()
    from modules import CONTEXT_MODULES
    needs_ctx = refs & {n for rs in CONTEXT_MODULES.values() for n in rs}
    if needs_ctx:
        raise SystemExit(f"modularize: {r.name} calls {', '.join(sorted(needs_ctx))}, which "
                         "need the context (move it with modularize_ctx.py instead)")
    own = set(MODULES[mod])
    consts = sorted(refs & set(CONSTANTS))
    calls = sorted(n for n in refs if n in where and n != r.name)
    for n in calls:
        if MODULES_ORDER.index(where[n]) > MODULES_ORDER.index(mod):
            raise SystemExit(f"modularize: {r.name} ({mod}) calls {n} ({where[n]}), "
                             "which comes later")
    imports = {}
    for n in calls:
        if where[n] != mod:
            imports.setdefault(where[n], []).append(n)
    if consts:
        imports.setdefault("pepper_constants", []).extend(consts)

    lines = list(r.lines)
    remove_entities(r, set(calls) | set(consts), lines)
    # Constant functions become constants.
    if consts:
        pat = re.compile(r"\b(" + "|".join(consts) + r")\s*\(\s*\)", re.I)
        for i, line in enumerate(lines):
            if line is not None:
                lines[i] = map_code(line, lambda s: pat.sub(lambda m: m.group(1), s))
    # Imports after the existing use statement (or the procedure statement).
    uses = [s for s in r.spec if re.match(r"\s*use\b", s.text, re.I)]
    after = uses[-1].last if uses else r.stmts[0].last
    ref_line = lines[uses[-1].first] if uses else lines[r.stmts[0].first]
    indent = ref_line[:len(ref_line) - len(ref_line.lstrip())]
    new_uses = [f"{indent}use {m}, only: {', '.join(sorted(set(ns)))}"
                for m, ns in sorted(imports.items(), key=lambda kv: MODULES_ORDER.index(kv[0]))]
    end = r.end.first
    lines[end] = re.sub(r"(?i)\bend\s*$", f"end {r.kind} {r.name}", lines[end])
    out = []
    for i, line in enumerate(lines):
        if line is not None:
            out.append(line)
        if i == after:
            out.extend(new_uses)
    while out and not out[-1].strip():
        out.pop()
    return "\n".join(out)


def wrapper(r: Routine, mod: str) -> str:
    """External procedure with the routine's original interface."""
    keep = set(r.args) | {r.name}
    # The parameters that the argument declarations need (directly or
    # through other parameters).
    values = {}
    for s in r.spec:
        if re.search(r"\bparameter\b", s.low):
            m = re.match(r"\s*parameter\s*\((.*)\)\s*$", s.text, re.I | re.S)
            body = m.group(1) if m else s.text.split("::", 1)[-1]
            for e in split_top(body):
                values[entity_name(e)] = set(re.findall(r"[a-z_]\w*", e.split("=", 1)[-1].lower()))
    need = set()
    for s in r.spec:
        decl = parse_decl(s.text)
        if decl and any(entity_name(e) in keep for e in decl[2]):
            need |= set(re.findall(r"[a-z_]\w*", (decl[0] + " " + " ".join(
                e for e in decl[2] if entity_name(e) in keep)).lower()))
    params, todo = set(), [n for n in need if n in values]
    while todo:
        n = todo.pop()
        if n not in params:
            params.add(n)
            todo.extend(v for v in values[n] if v in values)
    keep |= params
    out = [r.lines[i] for i in range(r.stmts[0].first, r.stmts[0].last + 1)]
    indent = "      "
    out.append(f"{indent}use {mod}, only: pepper_{r.name} => {r.name}")
    for s in r.spec:
        text = "\n".join(r.lines[s.first:s.last + 1])
        if re.match(r"\s*(use|implicit|include)\b", s.text, re.I):
            out.append(text)
        elif re.match(r"\s*parameter\b", s.text, re.I):
            m = re.match(r"\s*parameter\s*\((.*)\)\s*$", s.text, re.I | re.S)
            ents = [e for e in split_top(m.group(1)) if entity_name(e) in params]
            if ents:
                out.append(f"{indent}parameter ( {', '.join(ents)} )")
        elif re.match(r"\s*(external|intrinsic)\b", s.text, re.I):
            kw = re.match(r"\s*(\w+)", s.text).group(1)
            ents = [e for e in split_top(s.text[len(kw) + s.text.index(kw):])
                    if entity_name(e) in r.args]
            if ents:
                out.append(f"{indent}{kw.lower()} {', '.join(ents)}")
        else:
            decl = parse_decl(s.text)
            ents = [e for e in decl[2] if entity_name(e) in keep]
            if not ents:
                continue
            if len(ents) == len(decl[2]):
                out.append(text)
            else:
                spec, attrs, _ = decl
                head = spec + (f", {attrs} ::" if attrs else (" ::" if attrs is not None else ""))
                out.append(f"{indent}{head} {', '.join(ents)}")
    args = ", ".join(r.args)
    if r.kind == "function":
        out.append(f"{indent}{r.name} = pepper_{r.name} ( {args} )")
    else:
        out.append(f"{indent}call pepper_{r.name} ( {args} )")
    out.append(f"{indent}end {r.kind} {r.name}")
    return "\n".join(out)


def constant_wrapper(name: str, kind: str) -> str:
    typ = {"wp": "real(wp)", "ip": "integer(ip)"}[kind]
    return "\n".join([
        f"      {typ} function {name} ( )",
        f"      use pepper_kinds, only: {kind}",
        f"      use pepper_constants, only: pepper_{name} => {name}",
        "      implicit none",
        f"      {name} = pepper_{name}",
        f"      end function {name}",
    ])


LEGACY_HEADER = """\
! Legacy interface to {mod}: external procedures with SPICELIB's names
! and argument lists, calling the module procedures. Generated by
! tools/modularize.py when the routines moved to {src}.
!
! These keep the original SPICELIB interface for existing callers (such as
! the tspice tests). New code should use {mod} directly.
"""

MODULE_HEADER = """\
!> {doc}
!>
!> The procedures keep SPICELIB's names, arguments and documentation. They
!> have no saved state and do not signal SPICE errors. Moved here from
!> src/spicelib by tools/modularize.py; the external procedures with the
!> original interface are in src/legacy/{legacy}.
module {mod}

{uses}   implicit none

   private

   public :: {publics}

contains

"""


def legacy_name(mod: str) -> str:
    return "pepper_legacy_" + mod.removeprefix("pepper_") + ".f90"


def generate(mod: str, where: dict, dry_run: bool) -> list:
    written = []
    legacy = SRC / "legacy" / legacy_name(mod)
    if mod == "pepper_constants":
        wrappers = [constant_wrapper(n, CONSTANTS[n]) for n in MODULES[mod]]
        sources = [SPICELIB / f"{n}.f90" for n in MODULES[mod]]
        src = "src/pepper_constants.f90"
    else:
        target = SRC / f"{mod}.f90"
        # Adding to an existing module: only the routines still in src/spicelib.
        existing = target.exists()
        names = [n for n in MODULES[mod]
                 if not existing or list(SPICELIB.glob(n + ".[fF]90"))]
        if not names:
            raise SystemExit(f"modularize: nothing to move for {mod}")
        routines = [Routine(n) for n in names]
        procs = [module_procedure(r, mod, where) for r in routines]
        wrappers = [wrapper(r, mod) for r in routines]
        sources = [r.path for r in routines]
        src = f"src/{mod}.f90"
        publics = ", ".join(sorted(MODULES[mod]))
        publics = "&\n      ".join(_wrap(publics, 70))
        if existing:
            body = target.read_text(encoding="ascii")
            body = re.sub(r"   public :: .*?\n\n", f"   public :: {publics}\n\n", body,
                          count=1, flags=re.S)
            body = body.replace(f"\nend module {mod}\n",
                                "\n" + "\n\n".join(procs) + f"\n\nend module {mod}\n")
        else:
            body = MODULE_HEADER.format(doc=DOCS[mod], legacy=legacy.name, mod=mod,
                                        uses="", publics=publics)
            body += "\n\n".join(procs) + f"\n\nend module {mod}\n"
        if not dry_run:
            target.write_text(body, encoding="ascii")
        written.append(target)
    if mod != "pepper_constants" and legacy.exists():
        text = legacy.read_text(encoding="ascii").rstrip("\n") + "\n\n" + "\n\n".join(wrappers) + "\n"
    else:
        text = LEGACY_HEADER.format(mod=mod, src=src) + "\n" + "\n\n".join(wrappers) + "\n"
    if not dry_run:
        legacy.write_text(text, encoding="ascii")
        for p in sources:
            p.unlink()
    written.append(legacy)
    return written


def _wrap(s: str, width: int) -> list:
    out, cur = [], ""
    for word in s.split(" "):
        if cur and len(cur) + len(word) + 1 > width:
            out.append(cur.rstrip())
            cur = ""
        cur += word + " "
    out.append(cur.rstrip())
    return out


DOCS = {
    "pepper_arrays": "Searching, sorting and other operations on arrays.",
    "pepper_math": "Vectors, matrices, rotations, quaternions, coordinate systems, "
                   "planes, ellipses and other numerical routines.",
    "pepper_strings": "Character string operations.",
    "pepper_errors": "Error message texts (the explanations of SPICE short error messages).",
}

MODULES_ORDER = list(MODULES)


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("modules", nargs="+")
    ap.add_argument("--dry-run", action="store_true", help="check, but write nothing")
    args = ap.parse_args()
    where = module_of()
    for mod in args.modules:
        if mod not in MODULES:
            sys.exit(f"modularize: unknown module {mod}")
        for p in generate(mod, where, args.dry_run):
            print(("would write " if args.dry_run else "wrote ") + str(p.relative_to(ROOT)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
