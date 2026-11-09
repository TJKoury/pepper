#!/usr/bin/env python3
"""
Remove the build products of deleted source files.

    python tools/clean_stale.py

fpm keeps the object file of a source file that was deleted (moved into a
module), and its library archive keeps the object as a member. The linker
then takes the first definition of a symbol it finds in the archive, so an
old external procedure can silently replace the new legacy wrapper of the
same name in the test executables. This removes, in every build/gfortran_*
directory, the objects (and their .log/.digest files) whose source no longer
exists, and then the library archives, which fpm rebuilds from the current
objects. It also removes the test and example executables, and their
objects, that are older than the newest library source, because fpm neither
relinks nor recompiles them when only the library changed. Run by the pixi
build and test tasks.
"""

import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent


def package_root(d: Path) -> Path:
    """The root of the package whose objects are in build directory d
    (build/gfortran_*/<package>): the tspice package's own objects are named
    relative to tspice/, the library's relative to the repository."""
    return ROOT / "tspice" if d.name == "tspice" else ROOT


def source_of(obj: Path, base: Path = ROOT):
    """The source path of an fpm object name such as src_spicelib_pool.f90.o."""
    name = obj.name[:-2] if obj.name.endswith(".o") else None
    if not name:
        return None
    # a dependency's sources, named relative to the package: "../src/x.f90"
    # becomes ".._src_x.f90"
    while name.startswith(".._"):
        base = base.parent
        name = name[3:]
    # fpm joins the path components with "_": try every split
    parts = name.split("_")
    for i in range(1, len(parts)):
        cand = base / "/".join(parts[:i]) / "_".join(parts[i:])
        if cand.exists():
            return cand
    for i in range(1, len(parts)):
        for j in range(i + 1, len(parts)):
            cand = base / "/".join(parts[:i]) / "/".join(parts[i:j]) / "_".join(parts[j:])
            if cand.exists():
                return cand
    return None


def main() -> int:
    removed = 0
    for d in sorted([*(ROOT / "build").glob("gfortran_*/*"), *(ROOT / "tspice" / "build").glob("gfortran_*/*")]):
        if not d.is_dir():
            continue
        stale = []
        for obj in d.glob("*.o"):
            if not obj.name.startswith(("src_", "test_", "app_", "example_", ".._")):
                continue
            if source_of(obj, package_root(d)) is None:
                stale.append(obj)
        for obj in stale:
            for p in [obj, Path(str(obj) + ".log"), Path(str(obj) + ".digest")]:
                if p.exists():
                    p.unlink()
            removed += 1
        if stale:
            for a in d.glob("*.a"):
                a.unlink()
    if removed:
        print(f"clean_stale: removed {removed} objects of deleted sources (and the archives)")
    # An archive can keep a member whose object file is gone already (e.g.
    # removed by an earlier run while the archive was rebuilt by appending):
    # check the members of every archive.
    import subprocess
    for a in [*(ROOT / "build").glob("gfortran_*/*/*.a"), *(ROOT / "tspice" / "build").glob("gfortran_*/*/*.a")]:
        r = subprocess.run(["ar", "t", str(a)], capture_output=True, text=True)
        members = [m for m in r.stdout.split() if m.endswith(".o")]
        bad = [m for m in members if m.startswith(("src_", "test_", "app_", "example_", ".._"))
               and source_of(Path(m), package_root(a.parent)) is None]
        if bad:
            a.unlink()
            print(f"clean_stale: removed {a.relative_to(ROOT)} ({len(bad)} members of deleted "
                  f"sources, e.g. {bad[0]})")
    # A module that uses pepper_instance keeps an old .mod after its own
    # source is unchanged and pepper_instance changed (seen after failed
    # builds): its users then see an old layout of pepper_context. Remove the
    # build products of the modules (other than pepper_instance) that use
    # it and whose .mod is older than pepper_instance.mod.
    import re as _re
    users = {}
    for src in (ROOT / "src").rglob("*.[fF]90"):
        text = src.read_text(errors="ignore")
        if _re.search(r"^\s*use\s+pepper_instance\b", text, _re.M | _re.I):
            for m in _re.finditer(r"^\s*module\s+(?!procedure|subroutine|function)(\w+)\s*$", text, _re.M | _re.I):
                users[m.group(1).lower()] = src
    stale_mods = 0
    for d in [*(ROOT / "build").glob("gfortran_*"), *(ROOT / "tspice" / "build").glob("gfortran_*")]:
        inst = d / "pepper_instance.mod"
        if not inst.exists():
            continue
        for name, src in users.items():
            mod = d / f"{name}.mod"
            if mod.exists() and mod.stat().st_mtime < inst.stat().st_mtime:
                rel = "_".join(src.relative_to(ROOT).parts)
                for f in [mod, d / f"{name}.smod"] + list(d.glob(f"*/{rel}.o*")):
                    if f.exists():
                        f.unlink()
                stale_mods += 1
    if stale_mods:
        print(f"clean_stale: removed {stale_mods} module files older than pepper_instance.mod")
    # fpm does not relink the executables (tests, examples) when only the
    # library changed: remove those older than the newest library source, so
    # that it links them again.
    newest = max((f.stat().st_mtime for f in (ROOT / "src").rglob("*")
                  if f.suffix in (".f90", ".F90", ".inc")), default=0)
    # Nor does it recompile the objects of tests and examples when only a
    # module of the library changed (e.g. a derived type got a component):
    # a test then works with the old layout of the type. Remove those
    # objects too.
    recompile = 0
    for obj in [*(ROOT / "build").glob("gfortran_*/*/*.o"), *(ROOT / "tspice" / "build").glob("gfortran_*/*/*.o")]:
        if obj.name.startswith(("test_", "example_", "app_")) and obj.stat().st_mtime < newest:
            for p in [obj, Path(str(obj) + ".log"), Path(str(obj) + ".digest")]:
                if p.exists():
                    p.unlink()
            recompile += 1
    if recompile:
        print(f"clean_stale: removed {recompile} test/example objects older than the library sources")
    relink = 0
    for exe in [*(ROOT / "build").glob("gfortran_*/*/*"), *(ROOT / "tspice" / "build").glob("gfortran_*/*/*")]:
        if exe.parent.name in ("test", "example", "app") and exe.is_file() and \
                exe.stat().st_mtime < newest:
            exe.unlink()
            relink += 1
    if relink:
        print(f"clean_stale: removed {relink} executables older than the library sources")
    return 0


if __name__ == "__main__":
    sys.exit(main())
