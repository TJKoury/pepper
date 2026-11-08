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


def source_of(obj: Path):
    """The source path of an fpm object name such as src_spicelib_pool.f90.o."""
    name = obj.name[:-2] if obj.name.endswith(".o") else None
    if not name:
        return None
    # fpm joins the path components with "_": try every split
    parts = name.split("_")
    for i in range(1, len(parts)):
        cand = ROOT / "/".join(parts[:i]) / "_".join(parts[i:])
        if cand.exists():
            return cand
    for i in range(1, len(parts)):
        for j in range(i + 1, len(parts)):
            cand = ROOT / "/".join(parts[:i]) / "/".join(parts[i:j]) / "_".join(parts[j:])
            if cand.exists():
                return cand
    return None


def main() -> int:
    removed = 0
    for d in sorted((ROOT / "build").glob("gfortran_*/*")):
        if not d.is_dir():
            continue
        stale = []
        for obj in d.glob("*.o"):
            if not obj.name.startswith(("src_", "test_", "app_", "example_")):
                continue
            if source_of(obj) is None:
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
    for obj in (ROOT / "build").glob("gfortran_*/*/*.o"):
        if obj.name.startswith(("test_", "example_", "app_")) and obj.stat().st_mtime < newest:
            for p in [obj, Path(str(obj) + ".log"), Path(str(obj) + ".digest")]:
                if p.exists():
                    p.unlink()
            recompile += 1
    if recompile:
        print(f"clean_stale: removed {recompile} test/example objects older than the library sources")
    relink = 0
    for exe in (ROOT / "build").glob("gfortran_*/*/*"):
        if exe.parent.name in ("test", "example", "app") and exe.is_file() and \
                exe.stat().st_mtime < newest:
            exe.unlink()
            relink += 1
    if relink:
        print(f"clean_stale: removed {relink} executables older than the library sources")
    return 0


if __name__ == "__main__":
    sys.exit(main())
