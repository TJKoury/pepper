"""
Umbrella routines (SPICELIB routines with ENTRY points that share saved
variables), for tools/modularize_ctx.py.

An umbrella is split into one procedure per entry point. Each gets a copy
of the umbrella's declarations (minus its arguments, which are declared in
the interface). The saved variables that are real state (modules.UMBRELLAS)
become components of a derived type held in pepper_spice; the procedure
accesses them through an ASSOCIATE block, so its code is unchanged. All
other saved variables become ordinary local variables.
"""

import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from modularize import (SPICELIB, HEADER, SPEC_START, raw_statements, parse_decl,  # noqa: E402
                        split_top, entity_name, balanced)
from data_to_parameter import data_groups  # noqa: E402

STFUN = re.compile(r"^(\w+)\s*\(([\w\s,]*)\)\s*=(?!=)")


class Entry:
    """One entry point, with the attributes of modularize.Routine that the
    generators use (name, kind, prefix, args, spec, lines, references())."""

    def __init__(self, umb, k, nxt, main=False):
        self.umbrella = umb
        s = umb.stmts[k]
        if main:
            # The umbrella routine itself: its body runs from the end of the
            # specification part to the first entry.
            self.name, self.args = umb.name, list(umb.args)
        else:
            m = re.match(r"entry\s+(\w+)\s*(?:\(([^()]*)\))?", s.text, re.I)
            self.name = m.group(1).lower()
            self.args = [a.strip().lower() for a in (m.group(2) or "").split(",") if a.strip()]
        self.kind = umb.kind
        self.prefix = ""
        if umb.kind == "function":
            for d in umb.spec:
                decl = parse_decl(d.text)
                if decl and any(entity_name(e) == self.name for e in decl[2]):
                    self.prefix = decl[0]
            if not self.prefix:
                self.prefix = umb.prefix
        self.spec = umb.spec
        self.lines = umb.lines
        self.path = umb.path
        self.body = umb.stmts[umb.body_start if main else k + 1:nxt]
        # Lines of the entry: its documentation and code, up to the next
        # entry (whose "!$Procedure" line and blank lines are dropped).
        end = umb.stmts[nxt].first if nxt < len(umb.stmts) else len(umb.lines)
        seg = list(range(s.last + 1, end))
        while seg and (not umb.lines[seg[-1]].strip() or umb.lines[seg[-1]].startswith("!$Procedure")):
            seg.pop()
        self.segment = seg
        code_start = self.body[0].first if self.body else end
        self.doc_lines = [umb.lines[i] for i in seg if i < code_start
                          and (not umb.lines[i].strip() or umb.lines[i].lstrip().startswith("!"))]
        self.code_lines = [i for i in seg if i >= code_start]

    def references(self) -> set:
        variables = set(self.umbrella.args) | set(self.umbrella.variables)
        names = set()
        for s in self.body + self.umbrella.stfuns:
            code = re.sub(r"'[^']*'|\"[^\"]*\"", "''", s.low)
            names.update(re.findall(r"\bcall\s+(\w+)", code))
            for m in re.finditer(r"\b([a-z_]\w*)\s*\(", code):
                n = m.group(1)
                if n in self.umbrella.chars and ":" in balanced(code, m.end() - 1):
                    variables.add(n)
                names.add(n)
        return names - variables

    def mentions(self) -> set:
        words = set()
        for s in self.body:
            words |= set(re.findall(r"[a-z_]\w*", re.sub(r"'[^']*'", "''", s.low)))
        return words


class Umbrella:
    def __init__(self, name: str, state, component, where="instance", functions=(), docs=None):
        paths = sorted(SPICELIB.glob(name + ".[fF]90"))
        if len(paths) != 1:
            raise SystemExit(f"umbrella: no unique source file for {name}")
        self.path = paths[0]
        self.name = name
        self.component = component
        self.where = where
        self.docs = docs or {}
        text = self.path.read_text(encoding="ascii")
        self.lines = text.split("\n")
        self.state = []
        self.stmts = raw_statements(text)
        h = HEADER.match(self.stmts[0].text)
        self.kind = h.group("kind").lower()
        self.prefix = (h.group("prefix") or "").strip()
        self.args = [a.strip().lower() for a in (h.group("args") or "").split(",") if a.strip()]
        self.spec, self.stfuns = [], []
        self.variables, self.chars, scalars = set(), set(), set()
        k = 1
        while k < len(self.stmts):
            s = self.stmts[k]
            decl = parse_decl(s.text)
            if SPEC_START.match(s.text) or decl or re.match(r"\s*(save|data)\b", s.text, re.I):
                self.spec.append(s)
                if decl:
                    for e in decl[2]:
                        n = entity_name(e)
                        if "(" in e.split("=")[0]:
                            self.variables.add(n)
                        else:
                            scalars.add(n)
                            if decl[0].lower().startswith("character"):
                                self.chars.add(n)
                k += 1
                continue
            m = STFUN.match(s.text)
            if m and m.group(1).lower() in scalars:
                self.stfuns.append(s)
                k += 1
                continue
            break
        self.body_start = k
        ents = [i for i, s in enumerate(self.stmts) if re.match(r"entry\b", s.text, re.I)]
        last = len(self.stmts) - 1
        if self.stmts[last].low != "end":
            raise SystemExit(f"umbrella: {name}: last statement is not END")
        self.entries = [Entry(self, k, nxt) for k, nxt in zip(ents, ents[1:] + [last])]
        # The umbrella routine itself, unless it only signals BOGUSENTRY. (A
        # routine without ENTRY statements, e.g. one with a saved cache, is
        # just this procedure.)
        main = Entry(self, 0, ents[0] if ents else last, main=True)
        if not ents or not any("bogusentry" in s.low for s in main.body):
            self.entries.insert(0, main)
        if state == "saved":
            # The saved variables: named in SAVE statements or initialized
            # by DATA (a bare SAVE is "all").
            if any(re.match(r"\s*save\s*$", s.text, re.I) for s in self.spec):
                state = "all"
            else:
                state = []
                for s in self.spec:
                    if re.match(r"\s*save\b", s.text, re.I):
                        state += [entity_name(e) for e in split_top(s.text.split(None, 1)[1])]
                    elif re.match(r"\s*data\b", s.text, re.I):
                        state += [g[0][0].lower() for g in data_groups(s.text) if len(g[0]) == 1]
                state = list(dict.fromkeys(x.lower() for x in state))
        if state == "all":
            # Every local variable (all of them are saved in SPICELIB): exactly
            # SPICELIB's behavior, per context.
            params = set()
            for s in self.spec:
                d = parse_decl(s.text)
                if d and d[1] and "parameter" in d[1].lower():
                    params |= {entity_name(e) for e in d[2]}
                if re.match(r"\s*parameter\b", s.text, re.I):
                    m = re.match(r"\s*parameter\s*\((.*)\)\s*$", s.text, re.I | re.S)
                    params |= {entity_name(e) for e in split_top(m.group(1))}
            names = []
            for s in self.spec:
                d = parse_decl(s.text)
                if d:
                    names += [entity_name(e) for e in d[2]]
            entry_names = {e.name for e in self.entries} | {self.name}
            state = [n for n in names if n not in params | set(self.args) | entry_names
                     | set(functions)]
        self.state = [s.lower() for s in state]
        unknown = [s for s in self.state if s not in scalars | self.variables]
        if unknown:
            raise SystemExit(f"umbrella: {name}: state variables not declared: {unknown}")

    # ------------------------------------------------------------------
    # The state type

    def state_module(self) -> str:
        """Source of module pepper_state_<name> with type <name>_state."""
        values, decls, defaults = {}, {}, {}
        for s in self.spec:
            if re.search(r"\bparameter\b", s.low):
                m = re.match(r"\s*parameter\s*\((.*)\)\s*$", s.text, re.I | re.S)
                body = m.group(1) if m else s.text.split("::", 1)[-1]
                for e in split_top(body):
                    values[entity_name(e)] = set(re.findall(r"[a-z_]\w*", e.split("=", 1)[-1].lower()))
            decl = parse_decl(s.text)
            if decl:
                for e in decl[2]:
                    if entity_name(e) in self.state:
                        decls[entity_name(e)] = (decl[0], e)
            if re.match(r"\s*data\b", s.text, re.I):
                for names, vals in data_groups(s.text):
                    if len(names) == 1 and names[0].lower() in self.state:
                        defaults[names[0].lower()] = vals
        need = set()
        for spec, e in decls.values():
            need |= set(re.findall(r"[a-z_]\w*", (spec + " " + e).lower()))
        todo = [n for n in need if n in values]
        params = set()
        while todo:
            n = todo.pop()
            if n not in params:
                params.add(n)
                todo.extend(v for v in values[n] if v in values)
        holder = "pepper_context (one per thread)" if self.where == "context" else "pepper_spice"
        out = [f"!> The state of SPICELIB's {self.name.upper()} routines (entry points "
               + ", ".join(e.name for e in self.entries) + "),",
               f"!> held in {holder}. Generated by tools/modularize_ctx.py from the",
               f"!> saved variables of {self.path.name}.",
               f"module pepper_state_{self.name}", "",
               "   use pepper_kinds, only: ip, wp", "", "   implicit none", "", "   private", ""]
        for s in self.spec:
            text = "\n".join(self.lines[s.first:s.last + 1]).strip()
            if re.match(r"\s*include\b", s.text, re.I):
                out.append("   " + text)
            elif re.match(r"\s*parameter\b", s.text, re.I):
                m = re.match(r"\s*parameter\s*\((.*)\)\s*$", s.text, re.I | re.S)
                ents = [e for e in split_top(m.group(1)) if entity_name(e) in params]
                if ents:
                    out.append(f"   parameter ( {', '.join(ents)} )")
            else:
                decl = parse_decl(s.text)
                if decl:
                    for e in decl[2]:
                        if entity_name(e) in params:
                            attrs = f", {decl[1]}" if decl[1] else ""
                            out.append(f"   {decl[0]}{attrs} :: {e}")
        # No default initialization in the type: gfortran builds a temporary
        # of the whole (large) type on the stack to ALLOCATE a type with
        # default initialization. init_<name>_state sets the initial values.
        out += ["", f"   public :: init_{self.name}_state", "",
                f"   !> The saved variables of {self.name.upper()}.",
                f"   type, public :: {self.name}_state"]
        inits = []
        for n in self.state:
            spec, e = decls[n]
            if n in defaults:
                vals = defaults[n]
                value = vals[0] if len(set(vals)) == 1 else f"[ {', '.join(vals)} ]"
                inits.append(f"      state%{n} = {value}")
            doc = self.docs.get(n, f"{self.name.upper()}'s saved variable {n.upper()}")
            out.append(f"      {spec} :: {e}  !! {doc}")
        out += [f"   end type {self.name}_state", "", "contains", "",
                f"   !> Set the initial values (SPICELIB's DATA statements) of a new state.",
                f"   pure subroutine init_{self.name}_state(state)",
                f"      type({self.name}_state), intent(inout) :: state  !! the state"]
        out += inits
        out += [f"   end subroutine init_{self.name}_state", "",
                f"end module pepper_state_{self.name}", ""]
        return "\n".join(out)

    # ------------------------------------------------------------------
    # The procedures

    def procedure(self, e: Entry, drop_functions: set, imports: list, rewrite) -> str:
        """Source of the module procedure for entry e. drop_functions: names
        whose declarations are removed (functions that are now module
        procedures or constants); rewrite(code) rewrites a run of code."""
        from lowercase_code import map_code
        mentioned = e.mentions()
        entry_names = {x.name for x in self.entries}
        params = set()
        for s in self.spec:
            d = parse_decl(s.text)
            if d and d[1] and "parameter" in d[1].lower():
                params |= {entity_name(x) for x in d[2]}
            if re.match(r"\s*parameter\b", s.text, re.I):
                m = re.match(r"\s*parameter\s*\((.*)\)\s*$", s.text, re.I | re.S)
                params |= {entity_name(x) for x in split_top(m.group(1))}
        drop = set(self.args) | set(self.state) | entry_names | \
            {n for n in drop_functions - params if n in e.references() or n not in mentioned}
        out = [f"      module procedure {e.name}"]
        # Documentation (without the Brief_I/O section, which is in the
        # interface now).
        skip = False
        for line in e.doc_lines:
            if line.startswith("! $ "):
                skip = line.startswith("! $ Brief_I/O")
            if not skip:
                out.append(line)
        for s in self.spec:
            text = self.lines[s.first:s.last + 1]
            if re.match(r"\s*(save)\b", s.text, re.I):
                continue
            if re.match(r"\s*data\b", s.text, re.I):
                groups = data_groups(s.text)
                if all(len(g[0]) == 1 and g[0][0].lower() in self.state for g in groups):
                    continue           # defaults of the state type
                raise SystemExit(f"umbrella: {self.name}: DATA of a variable that is not "
                                 f"state: {s.text[:60]}")
            if re.match(r"\s*use\b", s.text, re.I):
                out += text
                if s is [x for x in self.spec if re.match(r"\s*use\b", x.text, re.I)][-1]:
                    out += imports
                continue
            kw = re.match(r"\s*external\b", s.text, re.I)
            decl = None if kw else parse_decl(s.text)
            if kw or decl:
                ents = split_top(s.text[kw.end():]) if kw else decl[2]
                keep = [x for x in ents if entity_name(x) not in drop]
                if len(keep) == len(ents):
                    out += text
                elif keep:
                    ind = text[0][:len(text[0]) - len(text[0].lstrip())]
                    if kw:
                        out.append(f"{ind}external {', '.join(keep)}")
                    else:
                        attrs = f", {decl[1]} ::" if decl[1] else (" ::" if decl[1] is not None else "")
                        out.append(f"{ind}{decl[0]}{attrs} {', '.join(keep)}")
                continue
            out += text
        for s in self.stfuns:
            words = set(re.findall(r"[a-z_]\w*", s.low))
            if words & set(self.state):
                raise SystemExit(f"umbrella: {self.name}: statement function uses state: {s.text}")
            out += self.lines[s.first:s.last + 1]
        used = [n for n in self.state if n in mentioned]
        body = [map_code(self.lines[i], rewrite) for i in e.code_lines]
        if used:
            comp = f"ctx%{self.component}" if self.where == "context" \
                else f"ctx%spice%{self.component}"
            out.append(f"      if ( .not. allocated ( {comp} ) ) then")
            out.append(f"         allocate ( {comp} )")
            out.append(f"         call init_{self.name}_state ( {comp} )")
            out.append("      end if")
            assoc = [f"{n} => {comp}%{n}" for n in used]
            out.append("      associate ( " + ", &\n                  ".join(assoc) + " )")
            out += body
            out.append("      end associate")
        else:
            out += body
        out.append(f"      end procedure {e.name}")
        return dedupe_uses("\n".join(out))


def dedupe_uses(text: str) -> str:
    """Remove names imported twice from the same module by USE ... ONLY
    statements (on single lines) of one procedure."""
    seen, out = {}, []
    for line in text.split("\n"):
        m = re.match(r"^(\s*)use\s+(\w+)\s*,\s*only\s*:\s*(.*?)\s*$", line, re.I)
        if not m:
            out.append(line)
            continue
        mod = m.group(2).lower()
        names = [n.strip() for n in m.group(3).split(",") if n.strip()]
        new = [n for n in names if n.lower() not in seen.setdefault(mod, set())]
        seen[mod] |= {n.lower() for n in new}
        if new:
            out.append(f"{m.group(1)}use {m.group(2)}, only: {', '.join(new)}")
    return "\n".join(out)
