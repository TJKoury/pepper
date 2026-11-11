"""Fixtures of PepperyPy's tests: SpiceyPy's test kernels (downloaded once,
by SpiceyPy's own downloader, into the temporary directory), loaded into
both SpiceyPy (the reference, CSPICE) and PepperyPy."""

import numpy as np
import pytest

import pepperypy
import spiceypy
from spiceypy.tests import gettestkernels as gtk


@pytest.fixture(scope='session')
def kernels():
    """The test kernels' paths (downloaded if missing)."""
    gtk.download_kernels()
    return gtk


def _load(paths):
    for m in (spiceypy, pepperypy):
        m.kclear()
        for p in paths:
            m.furnsh(p)


@pytest.fixture
def core(kernels):
    """The core kernels (LSK, PCK, DE405 subset, GM) in both libraries."""
    _load(kernels.CoreKernels.standardKernelList)
    yield kernels
    for m in (spiceypy, pepperypy):
        m.kclear()


@pytest.fixture
def cassini(kernels):
    """The core kernels and Cassini's (CK, FK, IK, PCK, SCLK, SPKs)."""
    c = kernels.CassiniKernels
    _load(kernels.CoreKernels.standardKernelList +
          [c.cassPck, c.satSpk, c.cassTourSpk, c.cassFk, c.cassCk, c.cassSclk, c.cassIk])
    yield kernels
    for m in (spiceypy, pepperypy):
        m.kclear()


def same(got, expected, rtol=1e-12, atol=1e-9, path='result'):
    """Assert two results are equal: numbers to a relative 1e-12 (and an
    absolute 1e-9 for values near zero), strings exactly, cells by their
    elements."""
    if hasattr(expected, 'is_double') and hasattr(expected, 'card'):
        expected = [x for x in expected]
    if hasattr(got, 'is_double') and hasattr(got, 'card'):
        got = [x for x in got]
    if isinstance(expected, str):
        assert isinstance(got, str), f'{path}: {got!r} is not a string'
        assert got.rstrip() == expected.rstrip(), f'{path}: {got!r} != {expected!r}'
        return
    if isinstance(expected, (tuple, list)) and expected and all(isinstance(x, str) for x in expected):
        assert [g.rstrip() for g in got] == [e.rstrip() for e in expected], f'{path}: {got!r} != {expected!r}'
        return
    if isinstance(expected, tuple):
        assert isinstance(got, tuple) and len(got) == len(expected), f'{path}: {got!r} vs {expected!r}'
        for k, (g, e) in enumerate(zip(got, expected)):
            same(g, e, rtol, atol, f'{path}[{k}]')
        return
    if isinstance(expected, (bool, np.bool_)):
        assert bool(got) == bool(expected), f'{path}: {got!r} != {expected!r}'
        return
    e = np.asarray(expected)
    g = np.asarray(got)
    if e.dtype.kind == 'U':
        assert g.dtype.kind == 'U' and g.shape == e.shape and all(
            x.rstrip() == y.rstrip() for x, y in zip(g.ravel(), e.ravel())), f'{path}: {g} != {e}'
        return
    assert g.shape == e.shape, f'{path}: shape {g.shape} != {e.shape}'
    if e.dtype.kind in 'iub':
        assert np.array_equal(g, e), f'{path}: {g} != {e}'
    else:
        np.testing.assert_allclose(g, e, rtol=rtol, atol=atol, err_msg=path)
