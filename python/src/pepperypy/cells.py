"""SPICE cells (SpiceyPy's SpiceCell, without ctypes).

A double precision or integer cell keeps SPICELIB's layout in a NumPy
array: six control elements (the size at index 4, SPICELIB's ``cell(-1)``;
the cardinality at index 5, ``cell(0)``), then the elements. PEPPER's cell,
set and window routines (wninsd, wnunid, spkcov, ...) work on it directly.
A character cell is a list of strings (handled in Python).

A window, a double precision cell of intervals, converts to and from an
``(n, 2)`` array (``to_intervals``, ``window``), the form PEPPER's routines of
any size take (``gf_distance``, ``spk_coverage``, ...).

Derived from SpiceyPy's ``support_types.SpiceCell`` (MIT; see LICENSE), which
derives from spiceminer's (MIT, Philipp Rasch).
"""

import numpy as np

# SpiceyPy's type codes
CHAR, DOUBLE, INT, TIME, BOOL = 0, 1, 2, 3, 4
CTRLBLOCK = 6

_NAMES = {CHAR: 'char', DOUBLE: 'double', INT: 'int', TIME: 'time', BOOL: 'bool'}


class SpiceCell:
    """A SPICE cell: a set, a window, or a list of values, of fixed size."""

    def __init__(self, dtype=DOUBLE, size=0, length=0):
        self.dtype = dtype
        self.length = length
        if dtype == CHAR:
            self._strings = []
            self._size = int(size)
            self._buf = None
        else:
            npt = np.int32 if dtype in (INT, BOOL) else np.float64
            self._buf = np.zeros(CTRLBLOCK + int(size), dtype=npt)
            self._buf[4] = size

    # ---- SpiceyPy's attributes

    @property
    def size(self):
        return self._size if self.dtype == CHAR else int(self._buf[4])

    @property
    def card(self):
        return len(self._strings) if self.dtype == CHAR else int(self._buf[5])

    @card.setter
    def card(self, n):
        n = int(n)
        if n < 0 or n > self.size:
            raise ValueError(f'cardinality {n} out of range (size {self.size})')
        if self.dtype == CHAR:
            del self._strings[n:]
            self._strings += [''] * (n - len(self._strings))
        else:
            self._buf[5] = n

    @property
    def data(self):
        """The elements (a view of the cell for numbers)."""
        if self.dtype == CHAR:
            return list(self._strings)
        return self._buf[CTRLBLOCK:CTRLBLOCK + self.card]

    def is_char(self):
        return self.dtype == CHAR

    def is_double(self):
        return self.dtype == DOUBLE

    def is_int(self):
        return self.dtype == INT

    def is_time(self):
        return self.dtype == TIME

    def is_bool(self):
        return self.dtype == BOOL

    def is_set(self):
        """Whether the elements are a set: sorted, without duplicates."""
        d = list(self)
        return all(d[k] < d[k + 1] for k in range(len(d) - 1))

    def reset(self):
        """Empty the cell."""
        self.card = 0

    def append(self, value):
        """Append a value (APPNDD, APPNDI, APPNDC)."""
        if self.card >= self.size:
            from .exceptions import dynamically_instantiate_spiceyerror
            raise dynamically_instantiate_spiceyerror(
                short='SPICE(CELLTOOSMALL)', long=f'The cell cannot hold more than {self.size} elements.')
        if self.dtype == CHAR:
            self._strings.append(str(value))
        else:
            self._buf[CTRLBLOCK + self.card] = value
            self._buf[5] += 1

    def resize(self, size):
        """Change the size (SSIZED: the cell is emptied)."""
        if self.dtype == CHAR:
            self._size = int(size)
            self._strings = []
            return
        buf = np.zeros(CTRLBLOCK + int(size), dtype=self._buf.dtype)
        buf[4] = size
        self._buf = buf

    # ---- windows

    def to_intervals(self):
        """A window's intervals as an (n, 2) array."""
        return self.data.reshape(-1, 2).copy()

    # ---- the Python protocols

    def __len__(self):
        return self.card

    def __iter__(self):
        return iter(self.data.tolist() if self.dtype != CHAR else self._strings)

    def __getitem__(self, i):
        d = self.data
        if isinstance(i, slice):
            return d[i].tolist() if self.dtype != CHAR else d[i]
        if i < 0:
            i += self.card
        if not 0 <= i < self.card:
            raise IndexError('SpiceCell index out of range')
        return d[i].item() if self.dtype != CHAR else d[i]

    def __contains__(self, value):
        return value in list(self)

    def __eq__(self, other):
        if not isinstance(other, SpiceCell):
            return NotImplemented
        return self.dtype == other.dtype and list(self) == list(other)

    def __repr__(self):
        return f'<SpiceCell {_NAMES.get(self.dtype, self.dtype)} card={self.card} size={self.size} {list(self)!r}>'


def SPICEDOUBLE_CELL(size):
    """A double precision cell."""
    return SpiceCell(DOUBLE, size)


def SPICEINT_CELL(size):
    """An integer cell."""
    return SpiceCell(INT, size)


def SPICECHAR_CELL(size, length):
    """A character cell."""
    return SpiceCell(CHAR, size, length)


def SPICEBOOL_CELL(size):
    """A boolean cell."""
    return SpiceCell(BOOL, size)


def SPICETIME_CELL(size):
    """A time cell (double precision)."""
    return SpiceCell(TIME, size)


def Cell_Double(cell_size):
    return SpiceCell(DOUBLE, cell_size)


def Cell_Int(cell_size):
    return SpiceCell(INT, cell_size)


def Cell_Char(cell_size, length):
    return SpiceCell(CHAR, cell_size, length)


def Cell_Bool(cell_size):
    return SpiceCell(BOOL, cell_size)


def Cell_Time(cell_size):
    return SpiceCell(TIME, cell_size)


def window(intervals, size=None):
    """A window (a double precision cell) from an (n, 2) array of
    intervals, sorted and disjoint."""
    a = np.asarray(intervals, dtype=np.float64).reshape(-1)
    c = SpiceCell(DOUBLE, max(a.shape[0], size or 0))
    c._buf[CTRLBLOCK:CTRLBLOCK + a.shape[0]] = a
    c._buf[5] = a.shape[0]
    return c


def from_values(values, dtype=DOUBLE, size=None):
    """A cell holding the values."""
    if dtype == CHAR:
        vals = [str(v) for v in values]
        c = SpiceCell(CHAR, max(len(vals), size or 0), max([len(v) for v in vals] + [1]))
        c._strings = vals
        return c
    a = np.asarray(values).reshape(-1)
    c = SpiceCell(dtype, max(a.shape[0], size or 0))
    c._buf[CTRLBLOCK:CTRLBLOCK + a.shape[0]] = a
    c._buf[5] = a.shape[0]
    return c
