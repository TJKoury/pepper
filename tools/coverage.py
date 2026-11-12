#!/usr/bin/env python3
"""Test coverage of PEPPER's sources (src/): build with gfortran's
--coverage (a build directory of its own: fpm keys it on the flags), run
the tests, and write an lcov report.

    pixi run -e coverage coverage            # the unit tests (test/core)
    pixi run -e coverage coverage-all        # and NAIF's test suite (tspice)
    python tools/coverage.py [--tspice] [--no-tests] [--families 'spk*' ...] [--report-only]

The report: build/coverage/html/index.html (and build/coverage/coverage.info,
lcov's tracefile). Counters from earlier runs are cleared first.
"""

import argparse
import shutil
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
OUT = ROOT / 'build' / 'coverage'
COV = '--coverage'
# lcov 2 is strict: gfortran's notes have a few of what it calls errors
# (inconsistent line counts for multi-line statements, unused lines)
IGNORE = 'inconsistent,mismatch,unused,negative,empty,format,source,gcov,corrupt,count'


def run(cmd, cwd=ROOT, check=True):
    print('+', ' '.join(cmd), flush=True)
    return subprocess.run(cmd, cwd=cwd, check=check)


def build_dirs(base):
    """fpm's build directories (one per set of flags) that have coverage notes."""
    return sorted({p.parent for p in base.glob('build/gfortran_*/**/*.gcno')})


def clear_counters(base):
    for p in base.glob('build/gfortran_*/**/*.gcda'):
        p.unlink()


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--tspice', action='store_true', help="also run NAIF's test suite (the tspice package)")
    ap.add_argument('--no-tests', action='store_true', help='do not run the unit tests (test/core)')
    ap.add_argument('--families', nargs='*', default=[], help="tspice families to run (default: all)")
    ap.add_argument('--report-only', action='store_true', help='write the report from the counters of the last run')
    args = ap.parse_args()

    gcov = shutil.which('gcov')
    if not gcov or not shutil.which('lcov'):
        sys.exit('coverage: gcov and lcov are needed (pixi run -e coverage ...)')

    if not args.report_only:
        clear_counters(ROOT)
        clear_counters(ROOT / 'tspice')

    # unit tests: fpm runs them all (a failing test still leaves its counters)
    if not args.no_tests and not args.report_only:
        run(['fpm', 'test', '--flag', COV, '--link-flag', COV], check=False)
    if args.tspice and not args.report_only:
        run(['fpm', 'run', '--flag', COV, '--link-flag', COV, '--runner', 'python ../tools/run_tspice.py', '--']
            + args.families, cwd=ROOT / 'tspice', check=False)

    OUT.mkdir(parents=True, exist_ok=True)
    infos = []
    for base in ([ROOT] if not args.no_tests else []) + ([ROOT / 'tspice'] if args.tspice else []):
        dirs = [d for d in build_dirs(base) if any(d.glob('*.gcda'))]
        if not dirs:
            continue
        info = OUT / f'{base.name}.info'
        cmd = ['lcov', '--capture', '--gcov-tool', gcov, '--output-file', str(info), '--ignore-errors', IGNORE,
               '--rc', 'geninfo_unexecuted_blocks=1', '--quiet']
        for d in dirs:
            cmd += ['--directory', str(d)]
        # the sources are relative to the directory fpm compiled from
        cmd += ['--base-directory', str(base)]
        run(cmd)
        infos.append(info)
        # the baseline: every compiled line at zero, so that files no test
        # reaches are in the report too
        base_info = OUT / f'{base.name}-baseline.info'
        cmd = ['lcov', '--capture', '--initial', '--gcov-tool', gcov, '--output-file', str(base_info),
               '--ignore-errors', IGNORE, '--quiet']
        for d in dirs:     # the coverage build's (not older builds' notes)
            cmd += ['--directory', str(d)]
        run(cmd + ['--base-directory', str(base)])
        infos.append(base_info)
    if not infos:
        sys.exit('coverage: no counters (did the tests run?)')

    total = OUT / 'all.info'
    cmd = ['lcov', '--output-file', str(total), '--ignore-errors', IGNORE, '--quiet']
    for i in infos:
        cmd += ['--add-tracefile', str(i)]
    run(cmd)
    # PEPPER's sources only (not the tests, not the toolchain's)
    run(['lcov', '--extract', str(total), f'{ROOT}/src/*', '--output-file', str(OUT / 'coverage.info'),
         '--ignore-errors', IGNORE, '--quiet'])
    run(['genhtml', str(OUT / 'coverage.info'), '--output-directory', str(OUT / 'html'), '--title',
         'PEPPER' + (' (unit tests and tspice)' if args.tspice else ' (unit tests)'),
         '--prefix', str(ROOT), '--ignore-errors', IGNORE, '--quiet', '--legend'])
    run(['lcov', '--summary', str(OUT / 'coverage.info'), '--ignore-errors', IGNORE])
    print(f'coverage: {OUT / "html" / "index.html"}')


if __name__ == '__main__':
    main()
