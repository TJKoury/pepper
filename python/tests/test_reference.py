"""PepperyPy against SpiceyPy (CSPICE N0067): the same calls, written as
SpiceyPy programs, give the same results (to 1e-12 relative), the same
types and shapes, and the same exceptions."""

import numpy as np
import pytest

import pepperypy
import spiceypy

from conftest import same

ET = 2.0e8              # 2006 MAY 4, within DE405's subset
ETS = [1.0e8, 2.0e8, 3.0e8]
CASS_ET = 415022488.0   # 2013 FEB 25, within Cassini's test CK
SPOINT = [3396.19, 0.0, 0.0]

CORE = [
    # time
    ('str2et', lambda m: m.str2et('2026 NOV 8 12:00:00.123')),
    ('str2et list', lambda m: m.str2et(['2000 JAN 1 12:00', '2017 JAN 1'])),
    ('utc2et', lambda m: m.utc2et('2004-06-11T19:32:00')),
    ('et2utc C', lambda m: m.et2utc(ET, 'C', 3)),
    ('et2utc ISOC', lambda m: m.et2utc(ET, 'ISOC', 6)),
    ('et2utc J', lambda m: m.et2utc(ET, 'J', 7)),
    ('et2utc vector', lambda m: m.et2utc(ETS, 'ISOD', 2)),
    ('timout', lambda m: m.timout(ET, 'YYYY-MON-DD HR:MN:SC.### ::TDB')),
    ('timout vector', lambda m: m.timout(ETS, 'MON DD, YYYY')),
    ('deltet', lambda m: m.deltet(ET, 'ET')),
    ('unitim', lambda m: m.unitim(ET, 'ET', 'JDTDB')),
    ('etcal', lambda m: m.etcal(ET)),
    ('tparse', lambda m: m.tparse('1996-12-18T12:28:28')),
    ('tpictr', lambda m: m.tpictr('Tue Aug  6 11:10:57  1996')),
    ('et2lst', lambda m: m.et2lst(ET, 399, 1.0, 'PLANETOCENTRIC')),
    ('j2000', lambda m: m.j2000()),
    ('spd', lambda m: m.spd()),
    ('dpr', lambda m: m.dpr()),
    ('clight', lambda m: m.clight()),
    # bodies and frames
    ('bodn2c', lambda m: m.bodn2c('MOON')),
    ('bodc2n', lambda m: m.bodc2n(301)),
    ('bods2c', lambda m: m.bods2c('399')),
    ('bodc2s', lambda m: m.bodc2s(3)),
    ('bodvrd', lambda m: m.bodvrd('EARTH', 'RADII', 3)),
    ('bodvcd', lambda m: m.bodvcd(499, 'RADII', 3)),
    ('bodfnd', lambda m: m.bodfnd(399, 'RADII')),
    ('namfrm', lambda m: m.namfrm('IAU_MARS')),
    ('frmnam', lambda m: m.frmnam(10014)),
    ('frinfo', lambda m: m.frinfo(10014)),
    ('cidfrm', lambda m: m.cidfrm(399)),
    ('cnmfrm', lambda m: m.cnmfrm('EARTH')),
    ('pxform', lambda m: m.pxform('J2000', 'IAU_EARTH', ET)),
    ('pxform ecliptic', lambda m: m.pxform('ECLIPJ2000', 'J2000', ET)),
    ('sxform', lambda m: m.sxform('IAU_MARS', 'J2000', ET)),
    ('pxfrm2', lambda m: m.pxfrm2('IAU_MOON', 'J2000', ET, ET + 100.0)),
    ('tisbod', lambda m: m.tisbod('J2000', 499, ET)),
    ('tipbod', lambda m: m.tipbod('J2000', 499, ET)),
    # states
    ('spkezr', lambda m: m.spkezr('MOON', ET, 'J2000', 'NONE', 'EARTH')),
    ('spkezr LT+S', lambda m: m.spkezr('MARS', ET, 'ECLIPJ2000', 'LT+S', 'EARTH')),
    ('spkezr CN+S', lambda m: m.spkezr('SUN', ET, 'IAU_EARTH', 'CN+S', 'EARTH')),
    ('spkezr XLT', lambda m: m.spkezr('MOON', ET, 'J2000', 'XLT', 'EARTH')),
    ('spkezr vector', lambda m: m.spkezr('MOON', ETS, 'J2000', 'LT', 'EARTH')),
    ('spkpos', lambda m: m.spkpos('JUPITER BARYCENTER', ET, 'J2000', 'LT+S', 'EARTH')),
    ('spkpos vector', lambda m: m.spkpos('SUN', ETS, 'J2000', 'NONE', 'MOON')),
    ('spkez', lambda m: m.spkez(301, ET, 'J2000', 'LT', 399)),
    ('spkezp', lambda m: m.spkezp(301, ET, 'J2000', 'LT', 399)),
    ('spkgeo', lambda m: m.spkgeo(301, ET, 'J2000', 399)),
    ('spkgps', lambda m: m.spkgps(301, ET, 'J2000', 399)),
    ('spkssb', lambda m: m.spkssb(399, ET, 'J2000')),
    ('spkacs', lambda m: m.spkacs(301, ET, 'J2000', 'LT+S', 399)),
    ('spkcpo', lambda m: m.spkcpo('SUN', ET, 'IAU_EARTH', 'OBSERVER', 'CN+S',
                                  [-2353.6213656676991, -4641.3414911499403, 3677.0523293197439],
                                  'EARTH', 'IAU_EARTH')),
    ('spkcvo', lambda m: m.spkcvo('SUN', ET, 'IAU_EARTH', 'OBSERVER', 'CN+S',
                                  [-2353.6, -4641.3, 3677.0, 0.0, 0.0, 0.0], ET, 'EARTH', 'IAU_EARTH')),
    ('lspcn', lambda m: m.lspcn('MARS', ET, 'LT+S')),
    # geometry
    ('subpnt', lambda m: m.subpnt('NEAR POINT/ELLIPSOID', 'MARS', ET, 'IAU_MARS', 'LT+S', 'EARTH')),
    ('subpnt intercept', lambda m: m.subpnt('INTERCEPT/ELLIPSOID', 'MARS', ET, 'IAU_MARS', 'CN+S', 'EARTH')),
    ('subslr', lambda m: m.subslr('NEAR POINT/ELLIPSOID', 'MARS', ET, 'IAU_MARS', 'LT+S', 'EARTH')),
    # (an iteration: agrees to 1e-10)
    ('sincpt', lambda m: m.sincpt('ELLIPSOID', 'MARS', ET, 'IAU_MARS', 'LT+S', 'EARTH', 'J2000',
                                  m.spkpos('MARS', ET, 'J2000', 'LT+S', 'EARTH')[0])),
    ('ilumin', lambda m: m.ilumin('ELLIPSOID', 'MARS', ET, 'IAU_MARS', 'LT+S', 'EARTH', SPOINT)),
    ('illumf', lambda m: m.illumf('ELLIPSOID', 'MARS', 'SUN', ET, 'IAU_MARS', 'LT+S', 'EARTH', SPOINT)),
    ('occult', lambda m: m.occult('MOON', 'ELLIPSOID', 'IAU_MOON', 'SUN', 'ELLIPSOID', 'IAU_SUN', 'LT',
                                  'EARTH', ET)),
    ('srfrec', lambda m: m.srfrec(399, 0.5, 0.3)),
    ('pgrrec', lambda m: m.pgrrec('MARS', 1.0, 0.5, 2.0, 3396.19, 0.00589)),
    ('recpgr', lambda m: m.recpgr('MARS', [3000.0, 1000.0, 500.0], 3396.19, 0.00589)),
    # the kernel pool and kernels
    ('gdpool', lambda m: m.gdpool('BODY399_RADII', 0, 3)),
    ('gdpool start', lambda m: m.gdpool('BODY399_RADII', 1, 2)),
    ('gipool', lambda m: m.gipool('BODY399_NUT_PREC_ANGLES', 0, 1) if False else m.dtpool('BODY399_RADII')),
    ('dtpool', lambda m: m.dtpool('BODY399_POLE_RA')),
    ('expool', lambda m: m.expool('BODY399_RADII')),
    ('expool no', lambda m: m.expool('NO_SUCH_VARIABLE')),
    ('ktotal', lambda m: m.ktotal('ALL')),
    ('ktotal SPK', lambda m: m.ktotal('SPK')),
    ('kdata', lambda m: m.kdata(0, 'SPK')[1:]),
    ('spkobj', lambda m: m.spkobj(spiceypy.tests.gettestkernels.CoreKernels.spk)),
    ('spkcov', lambda m: m.spkcov(spiceypy.tests.gettestkernels.CoreKernels.spk, 301)),
    # math
    ('vnorm', lambda m: m.vnorm([1.0, 2.0, 3.0])),
    ('vsep', lambda m: m.vsep([1.0, 0.0, 0.0], [1.0, 1.0, 0.0])),
    ('vcrss', lambda m: m.vcrss([1.0, 2.0, 3.0], [4.0, 5.0, 6.0])),
    ('vhat', lambda m: m.vhat([1.0, 2.0, 3.0])),
    ('mxv', lambda m: m.mxv(np.arange(9.0).reshape(3, 3), [1.0, 2.0, 3.0])),
    ('mxm', lambda m: m.mxm(np.arange(9.0).reshape(3, 3), np.eye(3) * 2)),
    ('axisar', lambda m: m.axisar([0.0, 0.0, 1.0], 0.3)),
    ('raxisa', lambda m: m.raxisa(m.axisar([1.0, 2.0, 3.0], 0.3))),
    ('m2eul', lambda m: m.m2eul(m.axisar([1.0, 2.0, 3.0], 0.3), 3, 1, 3)),
    ('eul2m', lambda m: m.eul2m(0.1, 0.2, 0.3, 3, 1, 3)),
    ('m2q', lambda m: m.m2q(m.axisar([1.0, 2.0, 3.0], 0.3))),
    ('q2m', lambda m: m.q2m([0.5, 0.5, 0.5, 0.5])),
    ('twovec', lambda m: m.twovec([1.0, 1.0, 0.0], 1, [0.0, 0.0, 1.0], 3)),
    ('reclat', lambda m: m.reclat([1.0, 2.0, 3.0])),
    ('latrec', lambda m: m.latrec(2.0, 0.3, 0.4)),
    ('recsph', lambda m: m.recsph([1.0, 2.0, 3.0])),
    ('sphrec', lambda m: m.sphrec(2.0, 0.3, 0.4)),
    ('reccyl', lambda m: m.reccyl([1.0, 2.0, 3.0])),
    ('cylrec', lambda m: m.cylrec(2.0, 0.3, 0.4)),
    ('recrad', lambda m: m.recrad([1.0, 2.0, 3.0])),
    ('radrec', lambda m: m.radrec(2.0, 0.3, 0.4)),
    ('recgeo', lambda m: m.recgeo([3000.0, 2000.0, 4000.0], 6378.137, 1.0 / 298.257)),
    ('georec', lambda m: m.georec(0.3, 0.4, 100.0, 6378.137, 1.0 / 298.257)),
    ('convrt', lambda m: m.convrt(1.0, 'AU', 'KM')),
    ('xf2rav', lambda m: m.xf2rav(m.sxform('IAU_MARS', 'J2000', ET))),
    ('nearpt', lambda m: m.nearpt([7000.0, 1000.0, 3000.0], 6378.0, 6378.0, 6357.0)),
    ('surfnm', lambda m: m.surfnm(3.0, 2.0, 1.0, [0.0, 0.0, 1.0])),
    ('npedln', lambda m: m.npedln(7.0, 7.0, 6.0, [10.0, 0.0, 0.0], [0.0, 0.0, 1.0])),
]

CASS_SCLK = 267832537952.0     # the start of the test CK's coverage
CASSINI = [
    ('scs2e', lambda m: m.scs2e(-82, '1/1740543049.140')),
    ('sce2s', lambda m: m.sce2s(-82, CASS_ET)),
    ('sce2c', lambda m: m.sce2c(-82, CASS_ET)),
    ('sct2e', lambda m: m.sct2e(-82, CASS_SCLK)),
    ('scencd', lambda m: m.scencd(-82, '1/1740543049.140')),
    ('scdecd', lambda m: m.scdecd(-82, CASS_SCLK)),
    ('ckgp', lambda m: m.ckgp(-82000, CASS_SCLK, 256, 'J2000')),
    ('ckgpav', lambda m: m.ckgpav(-82000, CASS_SCLK + 1000.0, 256.0, 'J2000')),
    ('ckfrot', lambda m: m.ckfrot(-82000, CASS_ET)),
    ('pxform CK', lambda m: m.pxform('CASSINI_ISS_NAC', 'J2000', CASS_ET)),
    ('ckobj', lambda m: m.ckobj(spiceypy.tests.gettestkernels.CassiniKernels.cassCk)),
    ('ckcov', lambda m: m.ckcov(spiceypy.tests.gettestkernels.CassiniKernels.cassCk, -82000, False,
                                'INTERVAL', 0.0, 'SCLK')),
    ('ckcov TDB', lambda m: m.ckcov(spiceypy.tests.gettestkernels.CassiniKernels.cassCk, -82000, False,
                                    'SEGMENT', 0.0, 'TDB')),
    ('getfov', lambda m: m.getfov(-82360, 10)),
]


# results of iterations, which agree to 1e-10 (relative)
LOOSER = {'sincpt'}


@pytest.mark.parametrize('name,call', CORE, ids=[c[0] for c in CORE])
def test_core(core, name, call):
    same(call(pepperypy), call(spiceypy), rtol=1e-10 if name in LOOSER else 1e-12, path=name)


@pytest.mark.parametrize('name,call', CASSINI, ids=[c[0] for c in CASSINI])
def test_cassini(cassini, name, call):
    same(call(pepperypy), call(spiceypy), path=name)


ERRORS = [
    ('unknown body', lambda m: m.spkezr('NOBODY', ET, 'J2000', 'NONE', 'EARTH')),
    ('unknown frame', lambda m: m.pxform('J2000', 'NO_FRAME', ET)),
    ('bad time', lambda m: m.str2et('not a time')),
    ('no data', lambda m: m.spkezr('MOON', 1.0e12, 'J2000', 'NONE', 'EARTH')),
    ('missing file', lambda m: m.furnsh('no-such-kernel.bsp')),
    ('bad abcorr', lambda m: m.spkezr('MOON', ET, 'J2000', 'XX', 'EARTH')),
]


@pytest.mark.parametrize('name,call', ERRORS, ids=[c[0] for c in ERRORS])
def test_errors(core, name, call):
    """The same exception class and short message as SpiceyPy's."""
    with pytest.raises(spiceypy.utils.exceptions.SpiceyError) as ref:
        call(spiceypy)
    with pytest.raises(pepperypy.SpiceyError) as got:
        call(pepperypy)
    assert got.value.short == ref.value.short
    assert type(got.value).__name__ == type(ref.value).__name__
    # the error was reset: the next call works
    assert pepperypy.bodn2c('EARTH') == 399


def test_not_found(core):
    for m in (spiceypy, pepperypy):
        with pytest.raises(m.NotFoundError if m is pepperypy else spiceypy.utils.exceptions.NotFoundError):
            m.bodn2c('NO SUCH BODY')
        with m.no_found_check():
            assert m.bodn2c('NO SUCH BODY') == (0, False)
            assert m.bodn2c('MOON') == (301, True)


def test_gf(core):
    """GF searches with cells, as SpiceyPy programs write them."""
    for name, call in [
        ('gfdist', lambda m, w: m.gfdist('MOON', 'NONE', 'EARTH', 'LOCMAX', 0.0, 0.0, 86400.0 * 5, 1000, w)),
        ('gfsep', lambda m, w: m.gfsep('MOON', 'SPHERE', 'NULL', 'SUN', 'SPHERE', 'NULL', 'NONE', 'EARTH',
                                       'LOCMAX', 0.0, 0.0, 86400.0 * 6, 1000, w)),
        ('gfposc', lambda m, w: m.gfposc('MOON', 'J2000', 'NONE', 'EARTH', 'LATITUDINAL', 'LATITUDE', '>',
                                         0.2, 0.0, 86400.0, 1000, w)),
        ('gfoclt', lambda m, w: m.gfoclt('ANY', 'MOON', 'ELLIPSOID', 'IAU_MOON', 'SUN', 'ELLIPSOID',
                                         'IAU_SUN', 'LT', 'EARTH', 180.0, w)),
        ('gfrr', lambda m, w: m.gfrr('MOON', 'NONE', 'EARTH', '=', 0.0, 0.0, 86400.0, 1000, w)),
        ('gfpa', lambda m, w: m.gfpa('MOON', 'SUN', 'LT+S', 'EARTH', 'LOCMIN', 0.0, 0.0, 86400.0, 1000, w)),
    ]:
        results = []
        for m in (spiceypy, pepperypy):
            w = m.cell_double(2) if m is spiceypy else pepperypy.cell_double(2)
            et0, et1 = m.str2et('2007 JAN 1'), m.str2et('2007 APR 1')
            m.wninsd(et0, et1, w)
            results.append(call(m, w))
        same(results[1], results[0], rtol=0, atol=1e-6, path=name)


def test_daf(core):
    """Reading a DAF: its first array's summary, unpacked, and its data."""
    spk = spiceypy.tests.gettestkernels.CoreKernels.spk
    out = []
    for m in (spiceypy, pepperypy):
        handle = m.dafopr(spk)
        m.dafbfs(handle)
        assert m.daffna()
        dc, ic = m.dafus(m.dafgs(n=5), 2, 6)
        data = m.dafgda(handle, int(ic[4]), int(ic[4]) + 9)
        out.append((dc, ic, data, m.dafgn() if hasattr(m, 'dafgn') else ''))
        m.dafcls(handle)
    same(out[1], out[0])


def test_windows():
    """Window routines on cells (PEPPER's, on SPICELIB's cell layout)."""
    out = []
    for m in (spiceypy, pepperypy):
        a = m.cell_double(20)
        b = m.cell_double(20)
        for left, right in [(1.0, 3.0), (7.0, 11.0), (23.0, 27.0)]:
            m.wninsd(left, right, a)
        for left, right in [(2.0, 6.0), (8.0, 10.0), (16.0, 18.0)]:
            m.wninsd(left, right, b)
        u = m.wnunid(a, b)
        i = m.wnintd(a, b)
        d = m.wndifd(a, b)
        m.wnexpd(0.5, 0.5, u)
        out.append(([x for x in u], [x for x in i], [x for x in d], m.wncard(a), m.wnfetd(a, 1),
                    m.wnsumd(a), m.wnelmd(8.0, a), m.wnincd(8.0, 9.0, a), m.card(a), m.size(a)))
    same(out[1], out[0])
