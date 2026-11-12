"""JSON kernels (files named *.json): jsonspice's tests, on files, with
PEPPER's differences (a bad "@" time is an error; meta-kernels are FURNSH's),
and the converter tools/tk2json.py on SpiceyPy's test kernels."""

import sys
from pathlib import Path

import pytest

import pepperypy
from pepperypy import Spice
from pepperypy.exceptions import SpiceyError

ROOT = Path(__file__).resolve().parents[2]
sys.path.insert(0, str(ROOT / 'tools'))
import tk2json  # noqa: E402


@pytest.fixture
def spice():
    s = Spice()
    yield s
    s.close()


def write(path, text):
    path.write_text(text)
    return str(path)


def test_jsonspice(spice, tmp_path):
    """jsonspice's test (tests/test.py), with files instead of dicts."""
    c = spice.context()
    spice.furnsh(write(tmp_path / 'a.json', '{"abc": [1, 2, 3]}'))
    assert list(c.pool_values('abc')) == [1, 2, 3]
    spice.furnsh(write(tmp_path / 'b.json', '{"+abc": [4, 5, 6]}'))
    assert list(c.pool_values('abc')) == [1, 2, 3, 4, 5, 6]
    spice.furnsh(write(tmp_path / 'c.json', '{"time": "@1972-JAN-1"}'))
    assert list(c.pool_values('time')) == [-8.83656e+08]
    spice.furnsh(write(tmp_path / 'd.json', '{"+time": "@1972-JAN-2"}'))
    assert list(c.pool_values('time')) == [-8.836560e+08, -8.835696e+08]
    spice.furnsh(write(tmp_path / 'test.json', '''{
    "time3": "@1972-JAN-1",   // example "@" time kernel variable
    "aaa" : 1,
    "+aaa" : 2,   // example "+=" append variable
    "VARIABLE": [1.0, 2.0, 3.0], // example variable with array value
    "VARIABLE2": 42.0 // example variable with scalar value
}'''))
    assert c.pool_values('VARIABLE2')[0] == 42.0
    assert list(c.pool_values('aaa')) == [1, 2]


def test_bad_time_is_an_error(spice, tmp_path):
    """jsonspice keeps "@SYNTAX-ERROR" as a string; PEPPER signals
    SPICE(BADTIMESPEC), and loads nothing of the kernel."""
    with pytest.raises(SpiceyError, match='BADTIMESPEC'):
        spice.furnsh(write(tmp_path / 'e.json', '{"ok": 1, "time2": "@SYNTAX-ERROR"}'))
    spice.context().reset()
    assert spice.context().expool('ok') is False


def test_meta(spice, tmp_path):
    """jsonspice's meta-kernel test: a JSON meta-kernel with path symbols,
    listing a text kernel and a JSON kernel."""
    write(tmp_path / 'kernel1.tpc', 'KPL/PCK\n\\begindata\nVAR1 = 1\n\\begintext\n')
    write(tmp_path / 'kernel2.json', '{\n    "VAR2": 2\n}\n')
    meta = write(tmp_path / 'meta.json', f'''{{
   // meta-kernel variables:
   "PATH_VALUES"  : "{tmp_path}",
   "PATH_SYMBOLS" : "TEST",
   "KERNELS_TO_LOAD" : ["$TEST/kernel1.tpc",
                        "$TEST/kernel2.json"],
   // other random variables:
   "HELLO" : 1,
   "STUFF" : "A STRING"
}}''')
    spice.furnsh(meta)
    c = spice.context()
    assert c.pool_values('VAR1')[0] == 1.0
    assert c.pool_values('VAR2')[0] == 2.0
    assert c.pool_values('HELLO')[0] == 1.0
    assert c.pool_strings('STUFF')[0] == 'A STRING'
    assert c.ktotal('ALL') == 3
    spice.unload(meta)
    assert c.ktotal('ALL') == 0


def test_module_functions(tmp_path):
    """The module-level furnsh (SpiceyPy's API) reads JSON kernels."""
    pepperypy.kclear()
    pepperypy.furnsh(write(tmp_path / 'm.json', '{"BODY8_GM": 6836534.064}'))
    assert pepperypy.bodvrd('8', 'GM', 1)[1][0] == 6836534.064
    pepperypy.kclear()


def pool(spice):
    """{name: (type, values)} of an instance's kernel pool."""
    c = spice.context()
    out = {}
    for name in c.pool_names('*'):
        n, t = c.dtpool(name)
        out[name] = (t, list(c.pool_values(name)) if t == 'N' else list(c.pool_strings(name)))
    return out


@pytest.mark.parametrize('which', ['pck', 'gm_pck', 'lsk'])
def test_converter(kernels, tmp_path, which):
    """tools/tk2json.py: SpiceyPy's text kernels and their JSON forms (and
    those converted back) give the same kernel pool."""
    tpc = Path(getattr(kernels.CoreKernels, which))
    js = tmp_path / (tpc.stem + '.json')
    js.write_text(tk2json.tk_to_json(tpc.read_text()))
    back = tmp_path / (tpc.stem + '_back.tpc')
    back.write_text(tk2json.json_to_tk(js.read_text()))
    pools = []
    for f in (tpc, js, back):
        with Spice() as s:
            s.furnsh(str(f))
            pools.append(pool(s))
    assert len(pools[0]) > 3
    # exactly the same: both formats' numbers are correctly rounded (NPARSD
    # reads with Fortran's READ in PEPPER; SPICELIB's could be a few ulps
    # off: spicelib_bugs.md)
    assert pools[1] == pools[0]
    assert pools[2] == pools[0]
