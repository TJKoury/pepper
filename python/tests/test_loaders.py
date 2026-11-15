"""Kernel loaders written in Python (KernelLoader, Spice.add_loader, furnsh's
loader=), and the builder's providers and frames (add_ephemeris, add_frame,
frame_rotations) from Python: the C interface's callbacks, end to end."""

import numpy as np
import pytest

import pepperypy
from pepperypy import KernelBuilder, KernelLoader, Spice
from pepperypy.exceptions import SpiceyError


@pytest.fixture
def spice():
    s = Spice()
    yield s
    s.close()


def circle(n=150, step=60.0, r=7000.0, w=1e-3):
    et = step * np.arange(n)
    a = w * et
    states = np.column_stack([r * np.cos(a), r * np.sin(a), np.zeros(n),
                              -r * w * np.sin(a), r * w * np.cos(a), np.zeros(n)])
    return et, states


def spin(w, t):
    """The matrix from the base to a frame turning by w*t about Z."""
    c, s = np.cos(w * t), np.sin(w * t)
    return np.array([[c, s, 0.0], [-s, c, 0.0], [0.0, 0.0, 1.0]])


class CsvLoader(KernelLoader):
    """A first line 'CSVEPH body center frame', then rows 'et,x,y,z,vx,vy,vz';
    rows of 'ROT et q0 q1 q2 q3' make a frame CSV_ROT; kept: the rows read."""

    def __init__(self):
        self.rows = {}
        self.unloaded = []

    def load(self, ctx, kb, file):
        with open(file) as f:
            tag, body, center, frame = f.readline().split()
            if tag != 'CSVEPH':
                raise ValueError(f'{file} is not a CSV ephemeris')
            states, rots = [], []
            for line in f:
                if line.startswith('ROT'):
                    rots.append([float(v) for v in line.split()[1:]])
                elif line.strip():
                    states.append([float(v) for v in line.split(',')])
        a = np.array(states)
        kb.spk_hermite(int(body), int(center), frame, a[:, 0], a[:, 1:], 7)
        kb.define_body('CSV_SC', int(body))
        if rots:
            r = np.array(rots)
            kb.frame_rotations('CSV_ROT', 1400700, 399, 'J2000', r[:, 0], r[:, 1:], 7)
        kb.set_format('CSV ephemeris')
        self.rows[file] = len(a)

    def detect(self, file, head):
        return head.startswith(b'CSVEPH')

    def unload(self, file):
        self.unloaded.append(file)


def write_csv(path, body=-10001, rotations=False):
    et, st = circle()
    with open(path, 'w') as f:
        f.write(f'CSVEPH {body} 399 J2000\n')
        for t, s in zip(et, st):
            f.write(','.join(repr(float(v)) for v in (t, *s)) + '\n')
        if rotations:
            c = Spice().context()
            for t in et:
                f.write('ROT ' + ' '.join(repr(float(v)) for v in (t, *c.m2q(spin(1e-3, t)))) + '\n')
    return str(path), et, st


def test_python_loader(spice, tmp_path):
    ld = CsvLoader()
    spice.add_loader('CSV', ld, extensions=['.csveph'])
    assert spice.loaders() == ['CSV']
    path, et, st = write_csv(tmp_path / 'a.csveph', rotations=True)
    spice.furnsh(path)                                   # by extension
    c = spice.context()
    assert np.allclose(c.spkgeo(-10001, et[10], 'J2000', 399)[0], st[10], rtol=1e-12)
    assert c.bodn2c('CSV_SC') == -10001                  # a name the file defines
    assert c.kernel_format(path) == ('CSV', 'CSV ephemeris')
    assert ld.rows[path] == len(et)                      # the loader's kept data
    rot = c.pxform('J2000', 'CSV_ROT', 1234.5)
    assert np.abs(rot - spin(1e-3, 1234.5)).max() < 1e-8
    cover = c.ck_coverage(path, 1400700, False, 'SEGMENT', 0.0, 'TDB')
    assert cover.shape == (1, 2)
    spice.unload(path)
    assert ld.unloaded == [path]
    assert c.namfrm('CSV_ROT') == 0

    # by detect (no loader has the extension), and by name
    other, _, _ = write_csv(tmp_path / 'b.txt', body=-10002)
    spice.furnsh(other)
    assert c.kernel_format(other)[0] == 'CSV'
    spice.unload(other)
    odd, _, _ = write_csv(tmp_path / 'c.dat', body=-10003)
    spice.add_loader('QUIET', CsvLoader())               # (its detect accepts too; loader= decides)
    spice.furnsh(odd, loader='quiet')
    assert c.kernel_format(odd)[0] == 'QUIET'
    spice.unload(odd)
    spice.remove_loader('QUIET')
    assert spice.loaders() == ['CSV']


def test_python_loader_errors(spice, tmp_path):
    spice.add_loader('CSV', CsvLoader(), extensions=['.csveph'])
    bad = tmp_path / 'bad.csveph'
    bad.write_text('NOTCSV 1 2 3\n')
    with pytest.raises(pepperypy.CallbackError) as err:
        spice.furnsh(str(bad))
    assert isinstance(err.value.__cause__, ValueError)
    assert spice.context().ktotal('ALL') == 0             # nothing loaded
    with pytest.raises(SpiceyError, match='UNKNOWNLOADER'):
        spice.furnsh(str(bad), loader='NOPE')
    with pytest.raises(SpiceyError, match='LOADERCONFLICT'):
        spice.add_loader('CSV', CsvLoader())


def test_builder_models_and_frames(spice):
    et, st = circle()
    c = spice.context()
    kb = KernelBuilder()

    def orbit(ctx, body, t):
        a = 1e-3 * t
        return [7000.0 * np.cos(a), 7000.0 * np.sin(a), 0.0, -7.0 * np.sin(a), 7.0 * np.cos(a), 0.0]

    def model_frame(ctx, frame, t):
        r = spin(2e-3, t).T                              # from the frame to J2000
        x = np.zeros((6, 6))
        x[:3, :3] = r
        x[3:, 3:] = r
        return x, 'J2000'

    kb.add_ephemeris(-10005, 399, 'J2000', 0.0, 1e4, orbit)
    kb.add_frame('PY_MODEL', 1400701, 399, model_frame)
    quats = np.array([c.m2q(spin(1e-3, t)) for t in et])
    mats = np.array([spin(1e-3, t) for t in et])
    kb.frame_rotations('PY_Q', 1400702, 399, 'J2000', et, quats, 7)
    kb.frame_rotations('PY_M', 1400703, 399, 'J2000', et, mats, 7)
    av = np.tile([0.0, 0.0, 1e-3], (len(et), 1))
    kb.frame_rotations('PY_H', 1400704, 399, 'J2000', et, quats, 7, av=av, method='HERMITE')
    spice.load_kernel('py_models', kb)

    assert np.allclose(c.spkgeo(-10005, 100.0, 'J2000', 399)[0], orbit(None, 0, 100.0), rtol=1e-14)
    assert np.abs(c.pxform('J2000', 'PY_MODEL', 100.0) - spin(2e-3, 100.0)).max() < 1e-14
    t = 1234.5
    assert np.abs(c.pxform('J2000', 'PY_Q', t) - spin(1e-3, t)).max() < 1e-8
    assert np.array_equal(c.pxform('J2000', 'PY_Q', t), c.pxform('J2000', 'PY_M', t))
    x = c.sxform('J2000', 'PY_H', t)
    assert np.abs(x[:3, :3] - spin(1e-3, t)).max() < 1e-12
    with pytest.raises(SpiceyError, match='NOTWRITABLE'):
        spice.write_kernel('py_models', 'unused.bsp')
    with pytest.raises(ValueError):
        kb.frame_rotations('PY_X', 1400705, 399, 'J2000', et, quats[:, :3], 7)
    spice.unload('py_models')
    assert c.namfrm('PY_Q') == 0
    with pytest.raises(SpiceyError):
        c.spkgeo(-10005, 100.0, 'J2000', 399)


def test_frames_written_and_read_back(spice, tmp_path):
    et, _ = circle()
    c = spice.context()
    kb = KernelBuilder()
    kb.frame_rotations('PY_W', 1400710, 399, 'J2000', et, np.array([spin(1e-3, t) for t in et]), 7)
    spice.load_kernel('py_w', kb)
    bc, tf = str(tmp_path / 'w.bc'), str(tmp_path / 'w.tf')
    spice.write_kernel('py_w', '', ck_file=bc, text_file=tf)
    other = Spice()
    other.furnsh([tf, bc])
    t = 2222.2
    assert np.array_equal(other.context().pxform('J2000', 'PY_W', t), c.pxform('J2000', 'PY_W', t))
    other.close()
