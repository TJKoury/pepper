"""Check the library for code that gfortran makes thread-unsafe.

gfortran (at least up to 16.2) keeps the length of a deferred-length
character function result (`character(:), allocatable` result) in a
*static* variable at each call site (`static integer(kind=8) slen.N` in
-fdump-tree-original), even with -fopenmp or -frecursive. Threads share it:
concurrent calls get each other's lengths, strings come out wrong and the
heap is corrupted. PEPPER's contexts are used from several threads at once,
so the library must not have such functions.

    python tools/check_threads.py          # fast: scan the sources
    python tools/check_threads.py --ir     # also build with
                                           # -fdump-tree-original and list
                                           # every uninitialized static

The source scan finds functions (and interface bodies) whose result is
declared `character(:), allocatable` (or `character(len=:)`). The --ir check
is the ground truth: it lists any static variable without an initializer in
the code gfortran generates for the library (initialized statics are the
constant tables of DATA statements). Exit status 1 if anything is found.
"""

import glob
import os
import re
import shutil
import subprocess
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))

FUNCTION = re.compile(
    r"^\s*(?:(?:pure|elemental|impure|recursive|module)\s+)*function\s+(\w+)\s*\(([^)]*)\)\s*"
    r"(?:result\s*\(\s*(\w+)\s*\))?",
    re.IGNORECASE,
)
END_FUNCTION = re.compile(r"^\s*end\s+function\b", re.IGNORECASE)


def deferred_result(lines, start, result):
    """True if the function starting at line `start` declares `result` as a
    deferred-length character."""
    decl = re.compile(
        r"^\s*character\s*\(\s*(?:len\s*=\s*)?:\s*\)[^!]*::[^!]*\b" + result + r"\b",
        re.IGNORECASE,
    )
    for line in lines[start + 1:]:
        if END_FUNCTION.match(line) or FUNCTION.match(line):
            return False
        if decl.match(line):
            return True
    return False


def scan_sources():
    """Functions with a deferred-length character result in src/."""
    found = []
    for path in sorted(glob.glob(os.path.join(ROOT, "src", "**", "*.[fF]90"), recursive=True)):
        with open(path, encoding="utf-8", errors="replace") as f:
            lines = f.read().split("\n")
        for i, line in enumerate(lines):
            if line.lstrip().startswith("!"):
                continue
            m = FUNCTION.match(line)
            if not m:
                continue
            result = m.group(3) or m.group(1)
            if deferred_result(lines, i, result):
                found.append(f"{os.path.relpath(path, ROOT)}:{i + 1}: {m.group(1)}")
    return found


def scan_ir():
    """Uninitialized statics in the library's generated code.

    The flags give this check a build directory of its own; the dumps stay
    there next to the objects, so that a later run, which recompiles only the
    changed sources, still scans every file (dumps of deleted sources are
    skipped)."""
    flags = "-fopenmp -fdump-tree-original"
    subprocess.run(["fpm", "build", "--flag", flags], cwd=ROOT, check=True,
                   stdout=subprocess.DEVNULL)
    # fpm names the objects (and so the dumps) after the source paths with
    # "/" replaced by "_": src/inc/x.f90 -> src_inc_x.f90.f90.006t.original
    sources = {os.path.relpath(p, ROOT).replace(os.sep, "_")
               for p in glob.glob(os.path.join(ROOT, "src", "**", "*.[fF]90"), recursive=True)}
    dumps = []
    for path in glob.glob(os.path.join(ROOT, "build", "*", "pepper", "src_*.original")):
        m = re.match(r"(src_.*?\.[fF]90)\.", os.path.basename(path))
        if m and m.group(1) in sources:
            dumps.append(path)
    found = []
    static = re.compile(r"^\s*static\s+(?!void\b|struct\b|const\b)([^=;]*);")
    for path in sorted(dumps):
        function = "?"
        with open(path, encoding="utf-8", errors="replace") as f:
            for line in f:
                if re.match(r"^\S.*\(.*\)$", line):
                    function = line.split("(")[0].split()[-1]
                m = static.match(line)
                if m:
                    found.append(f"{os.path.basename(path)}: {function}: static {m.group(1).strip()}")
    return found, len(dumps)


def main():
    problems = scan_sources()
    for p in problems:
        print(f"deferred-length character result: {p}")
    if "--ir" in sys.argv[1:]:
        if shutil.which("fpm") is None:
            print("check_threads: fpm not found", file=sys.stderr)
            return 2
        statics, n = scan_ir()
        for s in statics:
            print(f"static variable: {s}")
        problems += statics
        print(f"check_threads: {n} files of generated code scanned")
    if problems:
        print(f"check_threads: {len(problems)} problem(s) (see tools/check_threads.py)")
        return 1
    print("check_threads: no thread-unsafe constructs found")
    return 0


if __name__ == "__main__":
    sys.exit(main())
