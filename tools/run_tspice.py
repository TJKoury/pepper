#!/usr/bin/env python3
"""
Run the tspice test families in parallel.

Each test family runs as a separate tspice process in its own working
directory (the families create and delete kernel files with fixed names, so
they cannot share one). A family passes if its process exits with status 0.

Usage (normally through fpm, which supplies the executable):

    fpm test tspice --runner "python tools/run_tspice.py" -- [options] [FAMILY ...]
    pixi run tspice [options] [FAMILY ...]

FAMILY arguments select families by name, case-insensitive, with shell-style
wildcards; the "F_" prefix is optional (e.g. ``spk*`` matches F_SPKEZR,
F_SPK18, ...). With no FAMILY arguments, all families are run.

Work directories are created under build/tspice/<FAMILY>. The directory of a
family that passed is deleted (unless --keep); a failed family's directory is
kept with its output (output.txt) and tspice logs, and the failure is
appended to build/tspice/failures.log. Family timings are saved in
build/tspice/timings.json and used to start the slowest families first on the
next run. Families with timing comparisons (TIMING_SENSITIVE) run one at a
time after all the others.

Exit status: 0 if all selected families passed, 1 otherwise, 2 for usage
errors.
"""

import argparse
import concurrent.futures
import fnmatch
import json
import os
import shutil
import signal
import subprocess
import sys
import threading
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
WORK = ROOT / "build" / "tspice"

# Families with test cases that compare run times. They fail spuriously when
# other families compete for the CPUs, so they run one at a time after the
# parallel batch.
TIMING_SENSITIVE = {"F_SCLK1", "F_ZZHSC", "F_ZZHSI"}
TIMINGS = WORK / "timings.json"
FAILURES = WORK / "failures.log"   # failures of all runs, appended

_procs = set()
_lock = threading.Lock()
_stop = threading.Event()


def list_families(exe: Path) -> list:
    r = subprocess.run([str(exe), "--list"], capture_output=True, text=True, check=True)
    return [line.strip() for line in r.stdout.splitlines() if line.strip()]


def select(families: list, patterns: list) -> list:
    if not patterns:
        return list(families)
    chosen, unmatched = set(), []
    for pat in patterns:
        pat = pat.upper()
        pats = [pat] if pat.startswith("F_") else [pat, "F_" + pat]
        hits = {f for f in families for p in pats if fnmatch.fnmatchcase(f, p)}
        if not hits:
            unmatched.append(pat)
        chosen |= hits
    if unmatched:
        sys.exit(f"run_tspice: no test family matches: {' '.join(unmatched)}"
                 " (use --list to show the families)")
    return [f for f in families if f in chosen]   # keep the standard order


def run_family(exe: Path, family: str, tspice_args: list, timeout):
    """Run one family in its own directory; return (family, status, seconds).
    status is 'PASS', 'FAIL', 'TIMEOUT' or 'SKIP' (interrupted)."""
    if _stop.is_set():
        return family, "SKIP", 0.0
    wd = WORK / family
    shutil.rmtree(wd, ignore_errors=True)
    wd.mkdir(parents=True)
    t0 = time.monotonic()
    with open(wd / "output.txt", "wb") as out:
        p = subprocess.Popen([str(exe), *tspice_args, family], cwd=wd,
                             stdout=out, stderr=subprocess.STDOUT,
                             stdin=subprocess.DEVNULL)
        with _lock:
            _procs.add(p)
        try:
            rc = p.wait(timeout=timeout)
            status = "PASS" if rc == 0 else ("SKIP" if _stop.is_set() else "FAIL")
        except subprocess.TimeoutExpired:
            p.kill()
            p.wait()
            status = "TIMEOUT"
        finally:
            with _lock:
                _procs.discard(p)
    return family, status, time.monotonic() - t0


def tail(path: Path, n: int) -> str:
    try:
        lines = path.read_text(errors="replace").splitlines()
    except OSError:
        return ""
    return "\n".join(lines[-n:])


def main() -> int:
    ap = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("exe", type=Path, help="tspice executable (supplied by fpm)")
    ap.add_argument("families", nargs="*", metavar="FAMILY",
                    help="families to run (wildcards allowed; default: all)")
    ap.add_argument("-j", "--jobs", type=int, default=os.cpu_count() or 4,
                    help="number of families to run at once (default: CPU count)")
    ap.add_argument("--list", action="store_true",
                    help="list the selected families and exit")
    ap.add_argument("--keep", action="store_true",
                    help="keep the work directories of passing families too")
    ap.add_argument("--timeout", type=float, default=None, metavar="SECONDS",
                    help="time limit per family")
    ap.add_argument("--tail", type=int, default=30, metavar="N",
                    help="lines of output to show for each failure (default 30)")
    ap.add_argument("--tspice-args", default="", metavar="ARGS",
                    help="extra options for tspice, e.g. '-c' to log each test case")
    ap.add_argument("--slowest", type=int, default=5, metavar="N",
                    help="report the N slowest families (default 5)")
    args = ap.parse_args()

    exe = args.exe.resolve()
    families = select(list_families(exe), args.families)
    if args.list:
        print("\n".join(families))
        return 0

    WORK.mkdir(parents=True, exist_ok=True)
    try:
        timings = json.loads(TIMINGS.read_text())
    except (OSError, ValueError):
        timings = {}
    # Slowest first (unknown families first of all) for better packing.
    queue = sorted((f for f in families if f not in TIMING_SENSITIVE),
                   key=lambda f: -timings.get(f, float("inf")))
    serial = [f for f in families if f in TIMING_SENSITIVE]

    jobs = max(1, min(args.jobs, len(queue)))
    print(f"Running {len(families)} tspice test families with {jobs} parallel jobs"
          + (f" ({len(serial)} timing-sensitive families afterwards, one at a time)"
             if serial else "")
          + f" (work directories: {WORK.relative_to(ROOT)})", flush=True)

    def interrupt(signum, frame):
        _stop.set()
        with _lock:
            for p in _procs:
                p.terminate()
    signal.signal(signal.SIGINT, interrupt)

    results, width, n = {}, len(str(len(families))), 0
    t0 = time.monotonic()

    def finished():
        with concurrent.futures.ThreadPoolExecutor(jobs) as ex:
            futs = [ex.submit(run_family, exe, f, args.tspice_args.split(), args.timeout)
                    for f in queue]
            for fut in concurrent.futures.as_completed(futs):
                yield fut.result()
        for f in serial:
            yield run_family(exe, f, args.tspice_args.split(), args.timeout)

    for family, status, secs in finished():
        n += 1
        results[family] = (status, secs)
        if status == "SKIP":
            continue
        print(f"[{n:{width}}/{len(families)}] {status:7s} {family:24s} {secs:7.1f}s",
              flush=True)
        if status == "PASS":
            timings[family] = round(secs, 2)
            if not args.keep:
                shutil.rmtree(WORK / family, ignore_errors=True)
        else:
            print(tail(WORK / family / "output.txt", args.tail), flush=True)
    wall = time.monotonic() - t0

    try:
        TIMINGS.write_text(json.dumps(dict(sorted(timings.items())), indent=1) + "\n")
    except OSError:
        pass

    failed = [f for f in families if results[f][0] in ("FAIL", "TIMEOUT")]
    skipped = [f for f in families if results[f][0] == "SKIP"]
    passed = [f for f in families if results[f][0] == "PASS"]
    total = sum(s for _, s in results.values())

    print()
    if args.slowest:
        ran = sorted((f for f in families if results[f][0] != "SKIP"),
                     key=lambda f: -results[f][1])
        print("Slowest: " + ", ".join(f"{f} {results[f][1]:.1f}s"
                                      for f in ran[:args.slowest]))
    print(f"{len(passed)} passed, {len(failed)} failed"
          + (f", {len(skipped)} not run (interrupted)" if skipped else "")
          + f" in {wall:.1f}s (sum of family times {total:.1f}s)")
    if failed:
        print("Failed families (work directories kept):")
        for f in failed:
            print(f"  {f:24s} {results[f][0]:7s} {(WORK / f).relative_to(ROOT)}")
        # Keep a record across runs (a rerun replaces the work directories).
        try:
            with open(FAILURES, "a") as log:
                stamp = time.strftime("%Y-%m-%d %H:%M:%S")
                for f in failed:
                    log.write(f"{stamp} {f} {results[f][0]}\n")
                    log.write("    " + tail(WORK / f / "output.txt", 15).replace("\n", "\n    ") + "\n")
        except OSError:
            pass
    return 1 if failed or skipped else 0


if __name__ == "__main__":
    sys.exit(main())
