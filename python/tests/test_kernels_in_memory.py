"""Kernels built in memory from NumPy arrays (KernelBuilder,
Spice.load_kernel, write_kernel, kernel_format): the same states as an SPK
written by SPKW13/SPKW09 from the same arrays, bitwise; the kernel written to
an SPK file that SpiceyPy (CSPICE) reads; builders filled from threads."""

import subprocess
import sys
import threading

import numpy as np
import pytest

import pepperypy
from pepperypy import KernelBuilder, Spice
from pepperypy.exceptions import SpiceyError


@pytest.fixture
def spice():
    s = Spice()
    yield s
    s.close()


def circle(n=150, step=60.0, r=7000.0, w=1e-3):
    """Epochs and (n, 6) states on a circle."""
    et = step * np.arange(n)
    a = w * et
    states = np.column_stack([r * np.cos(a), r * np.sin(a), np.zeros(n),
                              -r * w * np.sin(a), r * w * np.cos(a), np.zeros(n)])
    return et, states


def states_of(ctx, body, ets):
    return np.array([ctx.spkgeo(body, t, 'J2000', 399)[0] for t in ets])


def test_builder_matches_spkw(spice, tmp_path):
    et, st = circle()
    c = spice.context()
    kb = KernelBuilder()
    kb.spk_hermite(-10001, 399, 'J2000', et, st, 7, ident='PY13')
    kb.spk_lagrange(-10002, 399, 'J2000', et, st, 8)
    assert len(kb) == 2
    spice.load_kernel('py_kernel', kb)
    assert kb.nsegments() == 0

    # the reference: SPKW13 and SPKW09 from the same arrays, in another instance
    ref = Spice()
    rc = ref.context()
    path = str(tmp_path / 'ref.bsp')
    h = rc.spkopn(path, 'REF', 0)
    rc.spkw13(h, -10001, 399, 'J2000', et[0], et[-1], 'REF13', 7, len(et), st, et)
    rc.spkw09(h, -10002, 399, 'J2000', et[0], et[-1], 'REF09', 8, len(et), st, et)
    rc.spkcls(h)
    ref.furnsh(path)
    ets = np.linspace(et[0], et[-1], 777)
    for body in (-10001, -10002):
        assert np.array_equal(states_of(c, body, ets), states_of(rc, body, ets))
    ref.close()

    assert c.kernel_format('py_kernel') == ('', '(programmatic)')
    assert c.ktotal('SPK') == 1
    assert c.kdata(0, 'SPK')[:2] == ('py_kernel', 'SPK')
    cover = c.spk_coverage('py_kernel', -10001)
    assert cover.shape == (1, 2) and cover[0, 0] == et[0] and cover[0, 1] == et[-1]
    spice.unload('py_kernel')
    assert c.ktotal('ALL') == 0


def test_written_kernel_read_by_spiceypy(spice, tmp_path):
    """write_kernel gives an SPK file that NAIF's CSPICE reads. (SpiceyPy
    runs in a subprocess: loading a file into CSPICE moves its global handle
    counter, which other tests compare with PEPPER's.)"""
    et, st = circle()
    kb = KernelBuilder()
    kb.set_format('pytest')
    kb.spk_hermite(-10001, 399, 'J2000', et, st, 7, tbeg=600.0, tend=6000.0)
    spice.load_kernel('k', kb)
    c = spice.context()
    assert c.kernel_format('k') == ('', 'pytest')
    path = str(tmp_path / 'written.bsp')
    spice.write_kernel('k', path)
    ets = np.linspace(600.0, 6000.0, 301)
    out = tmp_path / 'cspice.npy'
    script = (
        'import sys, numpy as np, spiceypy\n'
        'path, out = sys.argv[1], sys.argv[2]\n'
        'spiceypy.furnsh(path)\n'
        'ets = np.linspace(600.0, 6000.0, 301)\n'
        'states = [spiceypy.spkgeo(-10001, t, "J2000", 399)[0] for t in ets]\n'
        'cover = spiceypy.wnfetd(spiceypy.spkcov(path, -10001), 0)\n'
        'np.save(out, np.vstack([np.array(states).ravel(), np.r_[cover, np.zeros(6 * len(ets) - 2)]]))\n')
    subprocess.run([sys.executable, '-c', script, path, str(out)], check=True)
    theirs = np.load(out)
    ours = states_of(c, -10001, ets)
    assert np.allclose(ours.ravel(), theirs[0], rtol=1e-14, atol=1e-11)
    assert tuple(theirs[1][:2]) == (600.0, 6000.0)


def test_builder_errors_and_adapt(spice):
    et, st = circle(n=3)
    kb = KernelBuilder()
    with pytest.raises(SpiceyError, match='TOOFEWSTATES'):
        kb.spk_lagrange(-10001, 399, 'J2000', et[:2], st[:2], 8)
    kb.spk_lagrange(-10001, 399, 'J2000', et[:2], st[:2], 8, adapt=True)
    kb.spk_lagrange(-10001, 399, 'J2000', et[:1], st[:1], 8, adapt=True)
    assert len(kb) == 1
    with pytest.raises(SpiceyError, match='INVALIDDEGREE'):
        kb.spk_hermite(-10001, 399, 'J2000', et, st, 6)
    with pytest.raises(ValueError):
        kb.spk_hermite(-10001, 399, 'J2000', et, st[:, :3], 3)
    kb.spk_hermite(-10001, 399, 'NOSUCHFRAME', et, st, 3)
    with pytest.raises(SpiceyError, match='INVALIDREFFRAME'):
        spice.load_kernel('bad', kb)
    assert len(kb) == 2   # unchanged by the failed load
    assert spice.context().ktotal('ALL') == 0


def test_replace_keep_and_module_level(spice, tmp_path):
    et, st = circle()
    kb = KernelBuilder()
    kb.spk_hermite(-10001, 399, 'J2000', et, st, 7)
    spice.load_kernel('k', kb, keep=True)
    assert len(kb) == 1
    other = Spice()
    other.load_kernel('k', kb)
    t = 1234.5
    assert np.array_equal(spice.context().spkgeo(-10001, t, 'J2000', 399)[0],
                          other.context().spkgeo(-10001, t, 'J2000', 399)[0])
    other.close()
    kb.spk_hermite(-10001, 399, 'J2000', et, 2.0 * st, 7)
    spice.load_kernel('k', kb)   # replaces the kernel
    c = spice.context()
    assert c.ktotal('SPK') == 1
    assert np.allclose(c.spkgeo(-10001, et[10], 'J2000', 399)[0], 2.0 * st[10], rtol=1e-13)
    # the module-level function, on the current instance
    with spice.as_current():
        kb.spk_hermite(-10003, 399, 'J2000', et, st, 7)
        pepperypy.load_kernel('m', kb)
        assert pepperypy.ktotal('SPK') == 2


def test_builders_filled_by_threads(spice):
    """Builders belong to no instance: threads fill them at once."""
    et, st = circle(n=200)
    builders = [KernelBuilder() for _ in range(4)]
    errors = []

    def fill(k):
        try:
            for _ in range(20):
                builders[k].clear()
                builders[k].spk_hermite(-10010 - k, 399, 'J2000', et, (1 + k) * st, 7, ctx=spice.context())
        except Exception as e:   # pragma: no cover
            errors.append(e)

    threads = [threading.Thread(target=fill, args=(k,)) for k in range(4)]
    for t in threads:
        t.start()
    for t in threads:
        t.join()
    assert not errors
    for k, kb in enumerate(builders):
        spice.load_kernel(f'thread_{k}', kb)
    c = spice.context()
    for k in range(4):
        assert np.allclose(c.spkgeo(-10010 - k, et[5], 'J2000', 399)[0], (1 + k) * st[5], rtol=1e-13)


def test_define_body(spice):
    """A kernel's body names: usable while it is loaded, removed with it."""
    et, st = circle()
    c = spice.context()
    spice.boddef('PY_SC', -5)
    kb = KernelBuilder()
    kb.define_body('PY_SC', -10001)
    kb.spk_hermite(-10001, 399, 'J2000', et, st, 7)
    spice.load_kernel('named', kb)
    assert c.bodn2c('PY_SC') == -10001
    state, lt = c.spkezr('PY_SC', et[10], 'J2000', 'NONE', 'EARTH')
    assert np.allclose(state, st[10], rtol=1e-12)
    spice.unload('named')
    assert c.bodn2c('PY_SC') == -5          # the boddef again
    with pytest.raises(SpiceyError, match='BLANKNAMEASSIGNED'):
        kb.define_body(' ', -10002)
