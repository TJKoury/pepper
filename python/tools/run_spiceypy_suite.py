"""Run SpiceyPy's own test suite (test_wrapper.py of the installed SpiceyPy)
against PepperyPy, and summarize: the compatibility measure of
plans/pepperypy_plan.md.

    pixi run -e python py-test-spiceypy [-- pytest arguments]

The test file is copied to a temporary directory with `spiceypy` replaced by
`pepperypy` (the kernels still come from SpiceyPy's downloader). Results are
printed by kind: passed, failed for a function PepperyPy lacks
(COVERAGE.md), and other failures.
"""

import os
import re
import subprocess
import sys
import tempfile
from pathlib import Path

import spiceypy.tests


def main():
    src = Path(spiceypy.tests.__file__).parent / 'test_wrapper.py'
    text = src.read_text()
    text = text.replace('import spiceypy as spice', 'import pepperypy as spice')
    text = text.replace('import spiceypy.utils.callbacks', 'import pepperypy.utils.callbacks')
    text = re.sub(r'\bspiceypy\.utils\.', 'pepperypy.utils.', text)
    text = text.replace('from spiceypy.utils.support_types import', 'from pepperypy.utils.support_types import')
    with tempfile.TemporaryDirectory() as d:
        (Path(d) / 'test_spiceypy_wrapper.py').write_text(text)
        cmd = [sys.executable, '-m', 'pytest', '-q', '-p', 'no:cacheprovider', '-rf', '--tb=no',
               '--rootdir', d, str(Path(d) / 'test_spiceypy_wrapper.py')] + sys.argv[1:]
        env = dict(os.environ, COLUMNS='400')
        out = subprocess.run(cmd, capture_output=True, text=True, cwd=d, env=env).stdout
    fails = re.findall(r'^FAILED \S+::(\w+) - (.*)$', out, re.M)
    missing = sorted({m.group(1) for t, msg in fails for m in [re.search(r"has no attribute '(\w+)'", msg)] if m})
    other = [(t, msg) for t, msg in fails if 'has no attribute' not in msg]
    summary = next((l for l in reversed(out.splitlines()) if ' passed' in l or ' failed' in l), out[-300:])
    print(summary)
    print(f'{len(fails) - len(other)} failed for a missing function ({len(missing)} functions: '
          f'{", ".join(missing)})')
    print(f'{len(other)} other failures:')
    for t, msg in other:
        print(f'  {t}: {msg[:150]}')

if __name__ == '__main__':
    main()
