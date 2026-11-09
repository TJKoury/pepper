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
    def __init__(self, name: str, state, component, where="instance", functions=(), docs=None,
                 context_state=(), context_component=None, path=None, context_epoch=False,
                 context_init=(), epoch_entries=()):
        paths = [path] if path else sorted(SPICELIB.glob(name + ".[fF]90"))
        if len(paths) != 1:
            raise SystemExit(f"umbrella: no unique source file for {name}")
        self.path = paths[0]
        self.name = name
        self.component = component
        self.where = where
        self.docs = docs or {}
        # Variables held in the context although the others are instance
        # state (e.g. the units of the files in the handle manager).
        self.cstate = [s.lower() for s in context_state]
        self.ccomponent = context_component
        # A per-context cache of data derived from the instance's loaded
        # files: it is discarded (and set up again) when the instance's
        # segment_epoch changed; context_init are the statements that set up
        # a new cache ({c} is the cache); the epoch_entries (loads, unloads)
        # advance segment_epoch.
        self.context_epoch = context_epoch
        self.context_init = list(context_init)
        self.epoch_entries = [e.lower() for e in epoch_entries]
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
        # The umbrella routine is dropped if it only signals BOGUSENTRY, or
        # does nothing at all (only RETURN); legacy_main() replaces it.
        self.main = main
        self.main_bogus = any("bogusentry" in s.low for s in main.body)
        noop = all(re.match(r"\s*(return|end)\b", s.text, re.I) for s in main.body
                   if not re.match(r"\s*(data|save)\b", s.text, re.I))
        if not ents or not (self.main_bogus or noop):
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
            # Statement functions and their dummy arguments are not state
            # (the dummies keep their declarations, which give their types;
            # a variable of the same name is set before it is used anyway).
            stfun = set()
            for s in self.stfuns:
                head = s.low.partition("=")[0]
                stfun |= set(re.findall(r"[a-z_]\w*", head))
            # A declared name that is also the name of a routine is a function
            # only if it is used as one (or declared EXTERNAL): a variable
            # may have the name of a routine (e.g. TKFRAM's POOL).
            code = " ".join(s.low for e in self.entries for s in e.body)
            externals = set()
            for s in self.spec:
                if re.match(r"\s*external\b", s.text, re.I):
                    externals |= {x.strip().lower() for x in s.text.split(None, 1)[1].split(",")}
            dims = set()
            for s in self.spec:
                d = parse_decl(s.text)
                if d:
                    dims |= {entity_name(e) for e in d[2] if "(" in e}
            called = {f for f in set(functions) if f not in dims and
                      (f in externals or re.search(rf"\b{f}\s*\(", code))}
            state = [n for n in names if n not in params | set(self.args) | entry_names
                     | called | stfun]
        self.state = [s.lower() for s in state]
        unknown = [s for s in self.state + self.cstate if s not in scalars | self.variables]
        for s in self.spec:
            if re.match(r"\s*equivalence\b", s.text, re.I):
                names = set(re.findall(r"[a-z_]\w*", s.low)) - {"equivalence"}
                if names & set(self.state + self.cstate):
                    raise SystemExit(f"umbrella: {name}: EQUIVALENCE of state variables: {s.text}")
        if unknown:
            raise SystemExit(f"umbrella: {name}: state variables not declared: {unknown}")

    # ------------------------------------------------------------------
    # The state type

    def state_module(self) -> str:
        """Source of module pepper_state_<name>: type <name>_state (and, with
        context_state, <name>_context_state), with init routines."""
        values, decls, defaults = {}, {}, {}
        every = self.state + self.cstate
        for s in self.spec:
            if re.search(r"\bparameter\b", s.low):
                m = re.match(r"\s*parameter\s*\((.*)\)\s*$", s.text, re.I | re.S)
                body = m.group(1) if m else s.text.split("::", 1)[-1]
                for e in split_top(body):
                    values[entity_name(e)] = set(re.findall(r"[a-z_]\w*", e.split("=", 1)[-1].lower()))
            decl = parse_decl(s.text)
            if decl:
                for e in decl[2]:
                    if entity_name(e) in every:
                        decls[entity_name(e)] = (decl[0], e)
            if re.match(r"\s*data\b", s.text, re.I):
                for names, vals in data_groups(s.text):
                    if len(names) == 1 and names[0].lower() in every:
                        defaults[names[0].lower()] = vals
        need = set()
        for spec, e in decls.values():
            need |= set(re.findall(r"[a-z_]\w*", (spec + " " + e).lower()))
        for vals in defaults.values():       # initial values may use parameters
            for v in vals:
                need |= set(re.findall(r"[a-z_]\w*", re.sub(r"'[^']*'", "", v.lower())))
        todo = [n for n in need if n in values]
        params = set()
        while todo:
            n = todo.pop()
            if n not in params:
                params.add(n)
                todo.extend(v for v in values[n] if v in values)
        holder = "pepper_context (one per thread)" if self.where == "context" else "pepper_spice"
        from document_procedures import abstract
        purpose = abstract(self.lines)
        out = ([f"!> {line}" for line in purpose] + ["!>"] if purpose else []) + \
              [f"!> The state of SPICELIB's {self.name.upper()} routines (entry points "
               + ", ".join(e.name for e in self.entries) + "),",
               f"!> held in {holder}"
               + (" (and, for some variables, in pepper_context)" if self.cstate else "")
               + ". Generated by tools/modularize_ctx.py from the",
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
        # No default initialization in the types: gfortran builds a temporary
        # of the whole (large) type on the stack to ALLOCATE a type with
        # default initialization. The init routines set the initial values.
        types = [(f"{self.name}_state", self.state, f"The saved variables of {self.name.upper()}")]
        if self.cstate:
            types.append((f"{self.name}_context_state", self.cstate,
                          f"The saved variables of {self.name.upper()} that are per context"))
        epoch_type = f"{self.name}_context_state" if self.cstate else (
            f"{self.name}_state" if self.where == "context" else None)
        out += [""] + [f"   public :: init_{tn}" for tn, _, _ in types] + [""]
        bodies = []
        for tn, names, doc in types:
            out += [f"   !> {doc}.", f"   type, public :: {tn}"]
            inits = []
            for n in names:
                spec, e = decls[n]
                if n in defaults:
                    vals = defaults[n]
                    if len(set(vals)) == 1:
                        value = vals[0]
                    elif spec.lower().startswith("character"):
                        value = f"[ character(len(state%{n})) :: {', '.join(vals)} ]"
                    else:
                        value = f"[ {', '.join(vals)} ]"
                    # an array of rank > 1 takes its values in array element order
                    dims = e.partition("(")[2]
                    if len(set(vals)) > 1 and len(split_top(dims.rpartition(")")[0])) > 1:
                        value = f"reshape ( {value}, shape ( state%{n} ) )"
                    inits.append(f"      state%{n} = {value}")
                d = self.docs.get(n) or self.comment_of(n) or \
                    f"saved between calls by {self.name.upper()}"
                out.append(f"      {spec} :: {e}  !! {d}")
            if self.context_epoch and tn == epoch_type:
                out.append("      integer(ip) :: epoch  !! the instance's segment_epoch this cache reflects")
                inits.append("      state%epoch = 0")
            out += [f"   end type {tn}", ""]
            bodies += [f"   !> Set the initial values (SPICELIB's DATA statements) of a new state.",
                       f"   pure subroutine init_{tn}(state)",
                       f"      type({tn}), intent(inout) :: state  !! the state"] + inits + \
                      [f"   end subroutine init_{tn}", ""]
        out += ["contains", ""] + bodies + [f"end module pepper_state_{self.name}", ""]
        return "\n".join(out)

    def header_docs(self) -> list:
        """The documentation of the umbrella routine itself (its NAIF header:
        the comment lines from the start of the file to the end of its
        specification part), without the Brief_I/O section (each entry
        point has its own) and the code. For an umbrella whose routine only
        signals BOGUSENTRY (so it is not a procedure any more), this is kept
        as a comment block before the procedures of its entry points."""
        end = self.stmts[self.body_start].first if self.body_start < len(self.stmts) else len(self.lines)
        out, skip = [], False
        for i, line in enumerate(self.lines[:end]):
            s = line.lstrip()
            if line.startswith("! $ "):
                skip = line.startswith("! $ Brief_I/O")
            if skip or not (s.startswith("!") or not s):
                continue
            if s.startswith("#"):
                continue
            out.append(line)
        while out and not out[-1].strip("! \t"):
            out.pop()
        name = self.name.upper()
        intro = [f"!     PEPPER: SPICELIB's umbrella routine {name} is not a procedure any more:",
                 f"!     its entry points are the module procedures below, and its saved",
                 f"!     variables are in pepper_state_{self.name}. Its documentation follows.",
                 "!"]
        if out and out[0].startswith("!$Procedure"):
            return [out[0], "!"] + intro + out[1:]
        return intro + out

    def legacy_main(self, spec_lines: list) -> str:
        """The legacy external procedure that replaces a dropped umbrella
        routine: it signals SPICE(BOGUSENTRY) as SPICELIB's did, or does
        nothing as SPICELIB's did. spec_lines declare its arguments."""
        what = ("calling it directly signals SPICE(BOGUSENTRY), as in SPICELIB."
                if self.main_bogus else "it does nothing, as in SPICELIB.")
        out = [f"      !> Umbrella of the {self.name.upper()} routines: {what}",
               f"      subroutine {self.name} ( {', '.join(self.args)} )",
               "      use pepper_kinds, only: ip, wp",
               "      implicit none"] + spec_lines
        # arguments declared only by EXTERNAL in SPICELIB (dummy procedures)
        undeclared = [a for a in self.args if not any(re.search(rf"::\s*{a}\b", l) for l in spec_lines)]
        if undeclared:
            out.append(f"      external :: {', '.join(undeclared)}")
        if self.main_bogus:
            # SPICELIB's own error calls (the short message differs, e.g.
            # SPICE(DSKBOGUSENTRY)), through the legacy error routines
            calls = [s.text.strip() for s in self.main.body
                     if re.match(r"\s*call\s+(setmsg|errch|errint|sigerr)\b", s.text, re.I)]
            used = sorted({re.match(r"\s*call\s+(\w+)", c, re.I).group(1).lower() for c in calls})
            out += [f"      external :: {', '.join(used)}"]
            for c in calls:
                parts = re.split(r"\s*//\s*", c)
                out.append("      " + (" &\n      // ".join(parts) if len(c) > 100 else c))
        out.append(f"      end subroutine {self.name}")
        return "\n".join(out)

    def main_dropped(self) -> bool:
        return not any(e.name == self.name for e in self.entries)

    def comment_of(self, name: str):
        """The comment just above the declaration of a variable in the
        source, if it is short text (not a diagram), as one line."""
        for s in self.spec:
            d = parse_decl(s.text)
            if not d or name not in [entity_name(e) for e in d[2]]:
                continue
            k, block = s.first - 1, []
            while k >= 0 and self.lines[k].lstrip().startswith("!"):
                text = self.lines[k].lstrip()[1:].strip()
                if text:
                    block.insert(0, text)
                elif block:
                    break
                k -= 1
            if not block or len(block) > 4 or any(c in " ".join(block) for c in "|+") \
                    or block[0].startswith("$"):
                return None
            text = " ".join(block)
            if text.lower() in ("local variables", "saved variables", "local parameters",
                                "parameters", "saved values", "local variables.") or len(text) < 8:
                return None
            return text[0].lower() + text[1:] if not text[:2].isupper() else text
        return None

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
        # (a module procedure passed as an argument is declared EXTERNAL in
        # SPICELIB: that declaration would hide the module procedure)
        externals = set()
        for s in self.spec:
            if re.match(r"\s*external\b", s.text, re.I):
                externals |= {x.strip().lower() for x in s.text.split(None, 1)[1].split(",")}
        drop = set(self.args) | set(self.state) | set(self.cstate) | entry_names | \
            {n for n in drop_functions - params
             if n in e.references() or n not in mentioned or n in externals}
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
                if all(len(g[0]) == 1 and g[0][0].lower() in self.state + self.cstate for g in groups):
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
            # (the dummy arguments of the statement function are its own)
            head, _, body = s.low.partition("=")
            dummies = set(re.findall(r"[a-z_]\w*", head.partition("(")[2]))
            words = set(re.findall(r"[a-z_]\w*", body)) - dummies
            if words & set(self.state + self.cstate):
                raise SystemExit(f"umbrella: {self.name}: statement function uses state: {s.text}")
        if self.stfuns:
            # The lines from the first to the last statement function, with
            # any preprocessor directives between them (platform variants).
            a, b = self.stfuns[0].first, self.stfuns[-1].last
            while a > 0 and self.lines[a - 1].startswith("#"):
                a -= 1
            while b + 1 < len(self.lines) and self.lines[b + 1].startswith("#"):
                b += 1
            out += self.lines[a:b + 1]
        used = [n for n in self.state if n in mentioned]
        cused = [n for n in self.cstate if n in mentioned]
        body = [map_code(self.lines[i], rewrite) for i in e.code_lines]
        if used or cused:
            comp = f"ctx%{self.component}" if self.where == "context" \
                else f"ctx%spice%{self.component}"
            ccomp = f"ctx%{self.ccomponent}"
            assoc = []
            for c, tn, names in ((comp, f"{self.name}_state", used),
                                 (ccomp, f"{self.name}_context_state", cused)):
                if names:
                    cache = self.context_epoch and (c == ccomp or self.where == "context")
                    if cache:
                        out.append(f"!     PEPPER: discard the cache if files were loaded or unloaded")
                        out.append(f"!     (through any context) since it was filled.")
                        out.append(f"      if ( allocated ( {c} ) ) then")
                        out.append(f"         if ( {c}%epoch /= ctx%spice%segment_epoch ) deallocate ( {c} )")
                        out.append("      end if")
                    out.append(f"      if ( .not. allocated ( {c} ) ) then")
                    out.append(f"         allocate ( {c} )")
                    out.append(f"         call init_{tn} ( {c} )")
                    if cache:
                        out.append(f"         {c}%epoch = ctx%spice%segment_epoch")
                        out += [f"         {s.format(c=c)}" for s in self.context_init]
                    out.append("      end if")
                    if cache and e.name in self.epoch_entries:
                        out.append("!     PEPPER: a load or unload: other contexts discard their caches;")
                        out.append("!     this context's cache is updated by the code below.")
                        out.append("      ctx%spice%segment_epoch = ctx%spice%segment_epoch + 1")
                        out.append(f"      {c}%epoch = ctx%spice%segment_epoch")
                    assoc += [f"{n} => {c}%{n}" for n in names]
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
