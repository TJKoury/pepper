#!/usr/bin/env python3
"""
Move SPICELIB routines that need the context (they signal errors) from
src/spicelib into a submodule of pepper_instance, with the legacy wrappers
that keep the original external procedures.

    python tools/modularize_ctx.py [--dry-run] MODULE ...

For each MODULE (a key of modules.CONTEXT_MODULES, e.g. pepper_cells):

- src/<MODULE>.f90 (.F90 if a routine is preprocessed) is written:
  ``submodule (pepper_instance) <MODULE>``, with each routine as a
  ``module procedure``. Its text is kept, except that
    * the context is the new first argument ``ctx``: calls of SPICELIB's
      error routines become ``ctx%sigerr(...)`` etc., ``return()`` and
      ``failed()`` become ``ctx%failed()``, and calls of other routines
      that need the context get ``ctx`` as their first argument;
    * routines of the stateless modules (pepper_math, ...) are imported
      with ``use``, and constant functions become constants (``pi``);
    * the declarations of the arguments (now in the interface) and of the
      called functions are removed, as is the "$ Brief_I/O" section.
- In src/pepper_instance.f90, between the "generated" markers: the
  interface of each routine (``module subroutine/function``, with intents
  and ``!!`` comments from the Brief_I/O table and a ``!>`` docstring from
  the Abstract); a type-bound procedure of pepper_context for each routine
  whose name does not start with ``zz`` (``ctx%cardd(cell)``); the zz
  routines are public module procedures instead.
- src/legacy/pepper_legacy_<name>.f90 is written: external procedures with
  SPICELIB's names and arguments that call the routines with
  legacy_context().
- The routines' files in src/spicelib are deleted.

Umbrella routines (modules.UMBRELLAS; see tools/umbrella.py): each entry
point (and the umbrella routine itself, unless it only signals BOGUSENTRY)
becomes a module procedure. The saved variables listed as state become the
components of a type in a generated module src/pepper_state_<name>.f90,
held in pepper_spice (or pepper_context) as an allocatable component, and
the procedures access them through ASSOCIATE.

A routine may call only: the error routines, routines of earlier (or the
same) context modules, and routines of the stateless modules. Anything else
(including ENTRY points of routines still in src/spicelib) is an error, so
the module assignment has to follow the call graph.
"""

import argparse
import re
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
from lowercase_code import map_code  # noqa: E402
from modularize import (Routine, SRC, SPICELIB, parse_decl, split_top,  # noqa: E402
                        entity_name, remove_entities, module_of, _wrap)
from modules import MODULES, CONSTANTS, CONTEXT_MODULES, UMBRELLAS, ALLOWED_LEGACY  # noqa: E402
from add_intents import table as io_table, has_brief_io  # noqa: E402
from document_procedures import abstract, descriptions, leftover_text  # noqa: E402

INSTANCE = SRC / "pepper_instance.f90"
ERROR_CALLS = ["sigerr", "setmsg", "errch", "errdp", "errint", "errfnm", "errhan", "reset",
               "getmsg", "erract"]
ERROR_FUNCS = ["failed", "return"]
ORDER = list(CONTEXT_MODULES)

DOCS = {
    "pepper_cells": "Cells, sets and windows: SPICELIB's arrays with a control area "
                    "(size and cardinality), sets (ordered cells without duplicates) "
                    "and windows (sets of intervals).",
    "pepper_numerics": "Numerical, geometric and coordinate routines that check their "
                       "inputs (signal errors): inverse functions, Jacobians, planes, "
                       "ellipses, interpolation, Kepler's equation, aberration, ...",
    "pepper_array_ops": "Array operations that check their inputs (signal errors): "
                        "cycling, inserting, removing, swapping and packing elements; "
                        "doubly linked lists in an integer pool (lnk*).",
    "pepper_text": "String operations that check their inputs (signal errors): "
                   "replacing markers with numbers, inserting and removing "
                   "substrings, conversions, ...",
    "pepper_pool": "The kernel pool: variables loaded from text kernels or set by the "
                   "program (ldpool, gdpool, pdpool, ...), reading text kernels, and "
                   "the watchers that tell subsystems when variables change.",
    "pepper_bodies": "Body names and NAIF ID codes (built-in, and defined in the kernel "
                     "pool or by boddef), and body constants from the pool (bodvrd, "
                     "bodvcd); hash sets of names and integers.",
    "pepper_files": "The handle manager of binary kernel files (DAF, DAS): the file "
                    "table of the instance, the units of each context, file formats, "
                    "platform attributes and the FTP corruption check.",
    "pepper_daf": "DAF (Double precision Array File) files: opening and closing, "
                  "reading and writing records, arrays and summaries, searching the "
                  "arrays, comments, and the transfer format.",
    "pepper_das": "DAS (Direct Access Segregated) files: opening and closing, the file "
                  "summary, reading, writing and updating character, double precision "
                  "and integer data through record buffers, comments, and the transfer "
                  "format.",
    "pepper_segments": "Segment buffers of SPK, PCK, CK and DSK files: the tables of loaded "
                       "files (per instance) and the per-context caches of the segments found "
                       "for bodies and instruments.",
    "pepper_time": "Time: calendars and time scales (TTRANS, UNITIM, ETCAL, JUL2GR), "
                   "parsing (STR2ET), the defaults (TIMDEF), number formatting, unit "
                   "conversion, and the aberration correction strings.",
    "pepper_sclk": "Spacecraft clocks of type 1: the SCLK kernel data, conversion between "
                   "ticks, encoded clock, strings and ephemeris time.",
    "pepper_rotations": "Rotations and Euler angles, frames defined by two vectors, the "
                        "built-in inertial frames (CHGIRF), and the precession, nutation "
                        "and obliquity models used by dynamic frames.",
    "pepper_records": "The segments of SPK, PCK and CK files: reading their records and "
                      "evaluating them (all supported types), the generic segment "
                      "routines, two-body propagation and the SGP4 model.",
    "pepper_frames": "Reference frames: names, IDs and classes (FRAMEX, ZZFDAT), and the "
                     "parameters of dynamic frames.",
    "pepper_orientation": "Orientation data: body orientation from PCK files and text kernels "
                          "(TISBOD, TIPBOD), C-kernel and text-kernel frames (CKFROT, CKFXFM, "
                          "TKFRAM), switch frames (ZZSWFXFM), the TEME frame, and frame and "
                          "SCLK lookups.",
    "pepper_dynamic": "Dynamic frames (ZZDYNFRM, ZZDYNROT) and the copies of the frame and SPK "
                      "routines they use (ZZ*0, ZZ*1): SPICELIB has no recursion, so a dynamic "
                      "frame defined by states of bodies calls copies one level down.",
    "pepper_transforms": "Frame transformations: FRMGET and ROTGET (one frame to its base), "
                         "FRMCHG and REFCHG (between any two frames), PXFORM and SXFORM.",
    "pepper_time_api": "Time conversion and output: ET2UTC, UTC2ET, TIMOUT, TPICTR, DELTET, "
                       "and the scanner they use (SCANIT).",
    "pepper_sclk_api": "Spacecraft clock conversions: SCS2E, SCE2S, SCT2E, SCE2T, SCENCD, "
                       "SCDECD, SCTIKS, SCFMT, SCPART.",
    "pepper_ck_api": "C-kernel pointing (CKGP, CKGPAV) and coverage (CKCOV, CKOBJ).",
    "pepper_pck_api": "PCK coverage and frames (PCKCOV, PCKFRM), and Euler angles of bodies "
                      "(BODEUL, PCKEUL).",
    "pepper_dsk_write": "Writing type 2 DSK segments (DSKW02).",
    "pepper_spk_api": "The SPK API: states and positions of targets relative to observers with "
                      "aberration corrections (SPKEZR, SPKPOS, SPKEZ, SPKEZP, SPKGEO, SPKGPS, "
                      "SPKSSB, SPKAPO, SPKAPP, SPKACS, SPKAPS, SPKLTC, SPKCPO, SPKCPT, SPKCVO, "
                      "SPKCVT), coverage, segment subsetting (SPKSUB, SPKS*), and the generic "
                      "segment writer (SGSEQW).",
    "pepper_keeper": "The kernel manager: loading (FURNSH), tracking (KTOTAL, KDATA, KINFO) "
                     "and unloading (UNLOAD, KCLEAR) of SPICE kernels, including meta-kernels, "
                     "and the routines it needs.",
    "pepper_dsk_files": "DLA (DAS linked array) files and DSK files: segment lists, opening "
                        "and closing, reading and writing type 2 segments (plates, vertices, "
                        "voxels), their descriptors, and the DSK tolerances.",
}


def entry_names(name: str) -> list:
    """The procedure names of a context-module item: the routine, or the
    entry points of an umbrella (from its source, or from UMBRELLAS once it
    has moved)."""
    if name not in UMBRELLAS:
        return [name]
    if list(SPICELIB.glob(name + ".[fF]90")):
        return [e.name for e in make_umbrella(name).entries]
    return UMBRELLAS[name]["entries"]


def make_umbrella(name: str):
    from umbrella import Umbrella
    cfg = UMBRELLAS[name]
    functions = set(module_of()) | set(CONSTANTS) | set(ERROR_FUNCS) | set(ERROR_CALLS)
    functions |= {r for rs in CONTEXT_MODULES.values() for r in rs}
    from modules import STATE_DOCS
    return Umbrella(name, cfg["state"], cfg["component"], cfg.get("where", "instance"), functions,
                    cfg.get("docs") or STATE_DOCS.get(name), cfg.get("context_state", ()),
                    cfg.get("context_component"), context_epoch=cfg.get("context_epoch", False),
                    context_init=cfg.get("context_init", ()),
                    epoch_entries=cfg.get("epoch_entries", ()))


def where_ctx() -> dict:
    return {e: m for m, rs in CONTEXT_MODULES.items() for r in rs for e in entry_names(r)}


_LEGACY = None


def legacy_names() -> set:
    """Names of the procedures still in src/spicelib: the files' routines and
    their ENTRY points."""
    global _LEGACY
    if _LEGACY is None:
        _LEGACY = {}
        for p in SPICELIB.glob("*.[fF]90"):
            names = {p.stem.lower()}
            for m in re.finditer(r"^\s*entry\s+(\w+)", p.read_text(encoding="ascii"), re.M | re.I):
                names.add(m.group(1).lower())
            _LEGACY[p.stem.lower()] = names
    return _LEGACY


def check_calls(r, mod: str, ctx_where: dict, stateless: dict) -> None:
    moving = set(CONTEXT_MODULES[mod])
    legacy = {n for f, ns in legacy_names().items() if f not in moving for n in ns}
    for n in sorted(r.references()):
        if n in ctx_where:
            if ORDER.index(ctx_where[n]) > ORDER.index(mod):
                raise SystemExit(f"modularize_ctx: {r.name} calls {n} ({ctx_where[n]}), "
                                 "which comes later")
        elif n in ERROR_CALLS or n in ERROR_FUNCS or n in stateless or n in CONSTANTS \
                or n in ALLOWED_LEGACY:
            continue
        elif n in legacy and n != r.name:
            raise SystemExit(f"modularize_ctx: {r.name} calls {n}, which is still in "
                             "src/spicelib (add it to a context module first)")


def rewrite_code(code: str, ctx_names: set, consts: set) -> str:
    code = re.sub(r"\bcall\s+(" + "|".join(ERROR_CALLS) + r")\b", r"call ctx%\1", code, flags=re.I)
    code = re.sub(r"(?<![%\w])(failed|return)\s*\(\s*\)", "ctx%failed()", code, flags=re.I)
    if consts:
        code = re.sub(r"\b(" + "|".join(sorted(consts)) + r")\s*\(\s*\)", r"\1", code, flags=re.I)
    if ctx_names:
        alt = "|".join(sorted(ctx_names))
        # call x  /  call x ( )  ->  call x ( ctx )
        code = re.sub(rf"\b(call\s+)({alt})\s*(\(\s*\))?\s*$", r"\1\2 ( ctx )", code, flags=re.I)
        # x ( )  ->  x ( ctx );  x ( a  ->  x ( ctx, a
        code = re.sub(rf"(?<![%\w])({alt})(\s*)\(\s*\)", r"\1\2( ctx )", code, flags=re.I)
        code = re.sub(rf"(?<![%\w])({alt})(\s*)\((?!\s*ctx\b)", r"\1\2( ctx,", code, flags=re.I)
    return code


def drop_section(lines: list, title: str) -> list:
    out, skip = [], False
    for line in lines:
        if line.startswith("! $ "):
            skip = line.startswith("! $ " + title)
        if not skip:
            out.append(line)
    return out


def module_procedure(r: Routine, ctx_where: dict, stateless: dict) -> str:
    refs = r.references()
    # a moved routine passed as an argument is declared EXTERNAL in SPICELIB:
    # that declaration would hide the module procedure
    for s in r.spec:
        if re.match(r"\s*external\b", s.text, re.I):
            refs = refs | {x.strip().lower() for x in s.text.split(None, 1)[1].split(",")}
    ctx_names = {n for n in refs if n in ctx_where and n != r.name}
    consts = refs & set(CONSTANTS)
    imports = {}
    for n in sorted(refs):
        if n in stateless:
            imports.setdefault(stateless[n], []).append(n)
    if consts:
        imports.setdefault("pepper_constants", []).extend(sorted(consts))
    lines = list(r.lines)
    drop = set(r.args) | ctx_names | set(stateless) & refs | consts | set(ERROR_FUNCS)
    remove_entities(r, drop, lines)
    # Code rewrite (after the specification part only matters, but the
    # specification part has no calls).
    for i in range(r.stmts[0].last + 1, len(lines)):
        if lines[i] is not None:
            lines[i] = map_code(lines[i], lambda s: rewrite_code(s, ctx_names, consts))
    # The procedure statement becomes 'module procedure name'.
    first = r.stmts[0]
    indent = lines[first.first][:len(lines[first.first]) - len(lines[first.first].lstrip())]
    lines[first.first] = f"{indent}module procedure {r.name}"
    for i in range(first.first + 1, first.last + 1):
        lines[i] = None
    uses = [s for s in r.spec if re.match(r"\s*use\b", s.text, re.I)]
    after = uses[-1].last if uses else first.last
    new_uses = use_lines(imports, indent)
    lines[r.end.first] = re.sub(r"(?i)\bend\s*$", f"end procedure {r.name}", lines[r.end.first])
    out = []
    for i, line in enumerate(lines):
        if line is not None:
            out.append(line)
        if i == after:
            out.extend(new_uses)
    out = drop_section(out, "Brief_I/O")
    while out and not out[-1].strip():
        out.pop()
    from umbrella import dedupe_uses
    return dedupe_uses("\n".join(out))


def use_lines(imports: dict, indent: str) -> list:
    from modules import HANDWRITTEN
    order = list(MODULES) + list(HANDWRITTEN)
    return [f"{indent}use {m}, only: {', '.join(sorted(set(ns)))}"
            for m, ns in sorted(imports.items(), key=lambda kv: order.index(kv[0]))]


from modules import PROCEDURE_ARGS  # noqa: E402


def spec_for_args(r: Routine, with_intent: dict, docs: dict, indent: str) -> list:
    """Declarations needed for the arguments: includes, parameters (with
    their type declarations) and the argument declarations."""
    # Parameters: name -> names in its value; keep those the argument
    # declarations need (directly or through other parameters).
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
        if decl and any(entity_name(e) in r.args for e in decl[2]):
            words = decl[0] + " " + " ".join(e for e in decl[2] if entity_name(e) in r.args)
            need |= set(re.findall(r"[a-z_]\w*", words.lower()))
    todo = [n for n in need if n in values]
    params = set()
    while todo:
        n = todo.pop()
        if n not in params:
            params.add(n)
            todo.extend(v for v in values[n] if v in values)
    out = []
    for s in r.spec:
        text = "\n".join(r.lines[s.first:s.last + 1])
        if re.match(r"\s*include\b", s.text, re.I):
            out.append(indent + text.strip())
            continue
        if re.match(r"\s*parameter\b", s.text, re.I):
            m = re.match(r"\s*parameter\s*\((.*)\)\s*$", s.text, re.I | re.S)
            ents = [e for e in split_top(m.group(1)) if entity_name(e) in params]
            if ents:
                out.append(f"{indent}parameter ( {', '.join(ents)} )")
            continue
        decl = parse_decl(s.text)
        if not decl:
            continue
        spec, attrs, ents = decl
        for e in ents:
            n = entity_name(e)
            a = f", {attrs}" if attrs else ""
            if n in r.args and n in PROCEDURE_ARGS.get(r.name, {}):
                # a dummy procedure: declared by its abstract interface
                doc = f"  !! {docs[n]}" if docs.get(n) else ""
                out.append(f"{indent}procedure({PROCEDURE_ARGS[r.name][n]}) :: {n}{doc}")
            elif n in r.args:
                it = with_intent.get(n) if with_intent else None
                # A cell's control area (its size) is an input even if the
                # cell is an output: intent(inout), not intent(out).
                if it == "out" and re.search(r"\(\s*lbcell\s*:", e, re.I):
                    it = "inout"
                intent = f", intent({it})" if it else ""
                doc = f"  !! {docs[n]}" if docs.get(n) else ""
                out.append(f"{indent}{spec}{a}{intent} :: {e}{doc}")
            elif n in params:
                out.append(f"{indent}{spec}{a} :: {e}" if attrs else f"{indent}{spec} :: {e}")
    return out


def interface(r) -> str:
    body = getattr(r, "doc_lines", r.lines)
    intents = io_table(body)
    missing = [a for a in r.args if a not in intents]
    if missing and not has_brief_io(body):
        # documented by Detailed_Input/Output only, and not all arguments:
        # the others may be read and written
        print(f"modularize_ctx: {r.name}: not documented, intent(inout): {', '.join(missing)}")
        intents.update({a: "inout" for a in missing})
    elif missing:
        raise SystemExit(f"modularize_ctx: {r.name}: no Brief_I/O entry for {', '.join(missing)}")
    docs = descriptions(body)
    doc = abstract(body)
    extra = leftover_text(body)
    lines = [f"      !> {t}" for t in doc]
    if extra:
        lines += ["      !>"] + [f"      !> {x}" if x else "      !>" for x in extra]
    args = ", ".join(["ctx"] + r.args)
    prefix = (r.prefix + " ") if r.prefix else ""
    lines.append(f"      module {prefix}{r.kind} {r.name} ( {args} )")
    lines.append("         class(pepper_context), intent(inout) :: ctx  !! the context")
    lines += spec_for_args(r, intents, docs, "         ")
    lines.append(f"      end {r.kind} {r.name}")
    return "\n".join(lines)


def wrapper(r) -> str:
    doc_lines = getattr(r, "doc_lines", r.lines)
    docs = descriptions(doc_lines)
    out = [f"      !> {t}" for t in abstract(doc_lines)]
    target = f"zz routine {r.name}" if r.name.startswith("zz") else f"ctx%{r.name}"
    out += ["      !>", f"      !> Legacy interface: calls {target} with the legacy context."]
    if hasattr(r, "umbrella"):
        prefix = (r.prefix + " ") if r.prefix else ""
        out.append(f"      {prefix}{r.kind} {r.name} ( {', '.join(r.args)} )")
    else:
        out += [r.lines[i] for i in range(r.stmts[0].first, r.stmts[0].last + 1)]
    procs = sorted(set(PROCEDURE_ARGS.get(r.name, {}).values()))
    if r.name.startswith("zz"):
        out.append(f"      use pepper_instance, only: pepper_context, pepper_{r.name} => {r.name}"
                   + "".join(f", {x}" for x in procs))
    else:
        out.append("      use pepper_instance, only: pepper_context"
                   + "".join(f", {x}" for x in procs))
    out.append("      use pepper_legacy_context, only: legacy_context")
    for s in r.spec:
        if re.match(r"\s*(use|implicit)\b", s.text, re.I):
            out.append("\n".join(r.lines[s.first:s.last + 1]))
    out += spec_for_args(r, {}, docs, "      ")
    out.append("      type(pepper_context), pointer :: ctx")
    out.append("      ctx => legacy_context()")
    args = ", ".join(r.args)
    if r.name.startswith("zz"):
        call = f"pepper_{r.name} ( {', '.join(['ctx'] + r.args)} )"
    else:
        call = f"ctx%{r.name} ( {args} )"
    out.append(f"      {r.name} = {call}" if r.kind == "function" else f"      call {call}")
    out.append(f"      end {r.kind} {r.name}")
    return "\n".join(out)


def replace_region(text: str, marker: str, mod: str, content: str) -> str:
    begin = text.index(f"! >>> generated {marker}")
    end = text.index(f"! <<< generated {marker}")
    region = text[begin:end]
    start_tag, end_tag = f"! --- {mod}\n", f"! --- end {mod}\n"
    if start_tag in region:
        a = region.index(start_tag)
        b = region.index(end_tag) + len(end_tag)
        line_start = region.rfind("\n", 0, a) + 1
        region = region[:line_start] + region[b:]
    indent = text[text.rfind("\n", 0, end) + 1:end]
    block = f"{indent}{start_tag}{content}\n{indent}{end_tag}"
    region = region + block[len(indent):] + indent if region.endswith("\n" + indent) else region + block
    # Keep the end marker on its own line with its indentation.
    region = region.rstrip(" ")
    if not region.endswith("\n"):
        region += "\n"
    return text[:begin] + region + indent + text[end:]


LEGACY_HEADER = """\
! Legacy interface to {mod}: external procedures with SPICELIB's names
! and argument lists, calling the module procedures with the legacy
! context. Generated by tools/modularize_ctx.py when the routines moved to
! {src}.
!
! These keep the original SPICELIB interface for existing callers (such as
! the tspice tests). New code should use the type-bound procedures of
! pepper_context (e.g. ctx%cardd) instead.
"""


def entry_procedure(e, ctx_where: dict, stateless: dict) -> str:
    refs = e.references()
    ctx_names = {n for n in refs if n in ctx_where and n != e.name}
    consts = refs & set(CONSTANTS)
    imports = {}
    for n in sorted(refs):
        if n in stateless:
            imports.setdefault(stateless[n], []).append(n)
    if consts:
        imports.setdefault("pepper_constants", []).extend(sorted(consts))
    drop = set(ctx_where) | set(stateless) | set(CONSTANTS) | set(ERROR_FUNCS)
    return e.umbrella.procedure(e, drop, use_lines(imports, "      "),
                                lambda s: rewrite_code(s, ctx_names, consts))


def generate(mod: str, dry_run: bool) -> list:
    from umbrella import Umbrella
    stateless = module_of()
    ctx_where = where_ctx()
    routines, umbrellas, sources = [], [], []
    for n in CONTEXT_MODULES[mod]:
        if n in UMBRELLAS:
            u = make_umbrella(n)
            umbrellas.append(u)
            routines += u.entries
            sources.append(u.path)
        else:
            r = Routine(n, preprocessed_ok=True)
            routines.append(r)
            sources.append(r.path)
    for r in routines:
        if "ctx" in {entity_name(e) for s in r.spec if parse_decl(s.text)
                     for e in parse_decl(s.text)[2]} | set(r.args):
            raise SystemExit(f"modularize_ctx: {r.name} already has a variable named ctx")
        check_calls(r, mod, ctx_where, stateless)
    procs = []
    for r in routines:
        if hasattr(r, "umbrella"):
            text = entry_procedure(r, ctx_where, stateless)
            u = r.umbrella
            if u.main_dropped() and r is u.entries[0]:
                # The umbrella routine's own documentation, before its entries.
                text = "\n".join(u.header_docs()) + "\n\n" + text
            procs.append(text)
        else:
            procs.append(module_procedure(r, ctx_where, stateless))
    ext = ".F90" if any(p.suffix == ".F90" for p in sources) else ".f90"
    sub = SRC / f"{mod}{ext}"
    body = (f"!> {DOCS[mod]}\n!>\n"
            "!> The procedures keep SPICELIB's names and documentation; the context is\n"
            "!> their first argument (interfaces and type-bound procedures in\n"
            f"!> pepper_instance). Moved here from src/spicelib by\n"
            f"!> tools/modularize_ctx.py; the external procedures with the original\n"
            f"!> interface are in src/legacy/{legacy_name(mod)}.\n"
            f"submodule (pepper_instance) {mod}\n\n   implicit none\n\ncontains\n\n"
            + "\n\n".join(procs) + f"\n\nend submodule {mod}\n")
    text = INSTANCE.read_text(encoding="ascii")
    publics = [r.name for r in routines if r.name.startswith("zz")]
    binds = "\n".join(f"      procedure, non_overridable :: {r.name}"
                      for r in routines if not r.name.startswith("zz"))
    ifaces = "   interface\n\n" + "\n\n".join(interface(r) for r in routines) + "\n\n   end interface"
    if publics:
        ifaces = "   public :: " + "&\n      ".join(_wrap(", ".join(publics), 70)) + "\n\n" + ifaces
    text = replace_region(text, "bindings", mod, binds)
    text = replace_region(text, "interfaces", mod, ifaces)
    with_state = [u for u in umbrellas if u.state or u.cstate]
    if with_state:
        uses = []
        for u in with_state:
            names = [f"{u.name}_state", f"init_{u.name}_state"]
            if u.cstate:
                names += [f"{u.name}_context_state", f"init_{u.name}_context_state"]
            uses.append(f"   use pepper_state_{u.name}, only: {', '.join(names)}")
        text = replace_region(text, "state modules", mod, "\n".join(uses))
        comps = {"instance": [], "context": []}
        for u in with_state:
            if u.state:
                comps[u.where].append(f"      type({u.name}_state), allocatable :: {u.component}  !! "
                                      f"state of {u.name.upper()}")
            if u.cstate:
                comps["context"].append(f"      type({u.name}_context_state), allocatable :: "
                                        f"{u.ccomponent}  !! per-context state of {u.name.upper()}")
        for where, region in (("instance", "state components"),
                              ("context", "context state components")):
            if comps[where]:
                text = replace_region(text, region, mod, "\n".join(comps[where]))
    legacy = SRC / "legacy" / legacy_name(mod)
    mains = [u.legacy_main(spec_for_args(u.main, {}, {}, "      ")) for u in umbrellas
             if u.main_dropped()]
    wrappers = LEGACY_HEADER.format(mod=mod, src=f"src/{sub.name}") + "\n" + \
        "\n\n".join([wrapper(r) for r in routines] + mains) + "\n"
    if not dry_run:
        sub.write_text(body, encoding="ascii")
        INSTANCE.write_text(text, encoding="ascii")
        legacy.write_text(wrappers, encoding="ascii")
        for u in with_state:
            (SRC / f"pepper_state_{u.name}.f90").write_text(u.state_module(), encoding="ascii")
        for p in sources:
            p.unlink()
    return [sub, INSTANCE, legacy] + [SRC / f"pepper_state_{u.name}.f90" for u in with_state]


def legacy_name(mod: str) -> str:
    return "pepper_legacy_" + mod.removeprefix("pepper_") + ".f90"


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("modules", nargs="+")
    ap.add_argument("--dry-run", action="store_true", help="check, but write nothing")
    args = ap.parse_args()
    for mod in args.modules:
        if mod not in CONTEXT_MODULES:
            sys.exit(f"modularize_ctx: unknown module {mod}")
        for p in generate(mod, args.dry_run):
            print(("would write " if args.dry_run else "wrote ") + str(p.relative_to(SRC.parent)))
    return 0


if __name__ == "__main__":
    sys.exit(main())
