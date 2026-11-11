"""SpiceyPy's functions whose signatures differ from PEPPER's procedures:
written on PEPPER's results of any size (pool_values, gf_distance, ...),
cells, and error settings. The module-level functions use the current
instance and the calling thread's context, as the generated ones do.

The signatures and results follow SpiceyPy's (MIT; see LICENSE).
"""

import numpy as np

from . import _core
from ._core import _found_public as _found, current_context as _ctx
from ._version import toolkit_version
from .cells import CHAR, DOUBLE, INT, SpiceCell, from_values, window as _window
from .exceptions import dynamically_instantiate_spiceyerror
from .found_catcher import no_found_check as _no_found


def _intervals(cnfine):
    """A window (cell or (n, 2) array) as an (n, 2) array."""
    if isinstance(cnfine, SpiceCell):
        return cnfine.to_intervals()
    return np.asarray(cnfine, dtype=np.float64).reshape(-1, 2)


def _fill(result, intervals):
    """Put a GF result into a window cell (made if None)."""
    a = np.ascontiguousarray(intervals, dtype=np.float64).reshape(-1)
    if result is None:
        return _window(intervals, size=max(2000, a.shape[0]))
    if not isinstance(result, SpiceCell) or not result.is_double():
        raise TypeError('result must be a double precision SpiceCell')
    if a.shape[0] > result.size:
        raise dynamically_instantiate_spiceyerror(
            short='SPICE(WINDOWEXCESS)',
            long=f'The result has {a.shape[0] // 2} intervals; the window holds {result.size // 2}.')
    result._buf[6:6 + a.shape[0]] = a
    result._buf[5] = a.shape[0]
    return result


# ---- the kernel pool


def gdpool(name, start, room):
    """Return the d.p. value of a kernel variable from the kernel pool.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gdpool_c.html"""
    with _no_found():
        values, found = _ctx().pool_values(name)
    return _found('gdpool', (values[start:start + room],), found)


def gipool(name, start, room):
    """Return the integer value of a kernel variable from the kernel pool.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gipool_c.html"""
    with _no_found():
        values, found = _ctx().pool_integers(name)
    return _found('gipool', (values[start:start + room],), found)


def gcpool(name, start, room, lenout=None):
    """Return the character value of a kernel variable from the kernel pool.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gcpool_c.html"""
    with _no_found():
        values, found = _ctx().pool_strings(name)
    return _found('gcpool', (list(values[start:start + room]),), found)


def gnpool(name, start, room, lenout=None):
    """Return names of kernel variables matching a specified template.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gnpool_c.html"""
    names = list(_ctx().pool_names(name))
    return _found('gnpool', (names[start:start + room],), bool(names))


def bodvrd(bodynm, item, maxn):
    """Fetch from the kernel pool the double precision values of an item
    associated with a body.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvrd_c.html"""
    values = _ctx().body_values_by_name(bodynm, item)
    return len(values), values


def bodvcd(bodyid, item, maxn):
    """Fetch from the kernel pool the double precision values of an item
    associated with a body, by its ID code.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/bodvcd_c.html"""
    values = _ctx().body_values_by_code(bodyid, item)
    return len(values), values


def getfov(instid, room, shapelen=None, framelen=None):
    """Return the field-of-view (FOV) parameters of an instrument.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getfov_c.html"""
    shape, frame, bsight, bounds = _ctx().fov_by_code(instid)
    return shape, frame, bsight, len(bounds), bounds


# ---- DAF


def dafgs(n=125):
    """Return (get) the summary for the current array in the current DAF
    (its first n values).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/dafgs_c.html"""
    return _ctx().dafgs()[:n]


# ---- cells and windows


def card(cell):
    """The cardinality of a cell.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/card_c.html"""
    return cell.card


def size(cell):
    """The size of a cell.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/size_c.html"""
    return cell.size


def scard(incard, cell):
    """Set the cardinality of a cell.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/scard_c.html"""
    cell.card = incard
    return cell


def ssize(newsize, cell):
    """Set the size of a cell (it is emptied).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/ssize_c.html"""
    cell.resize(newsize)
    return cell


def copy(cell):
    """A copy of a cell (character, double precision or integer).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/copy_c.html"""
    if cell.dtype not in (CHAR, DOUBLE, INT):
        raise NotImplementedError('copy: character, double precision and integer cells only')
    if cell.is_char():
        c = SpiceCell(CHAR, cell.size, cell.length)
        c._strings = list(cell._strings)
        return c
    c = SpiceCell(cell.dtype, 0)
    c._buf = cell._buf.copy()
    return c


def _append(item, cell):
    if hasattr(item, '__iter__') and not isinstance(item, (str, bytes)):
        for x in item:
            cell.append(x)
    else:
        cell.append(item)


def appndd(item, cell):
    """Append item(s) to a double precision cell.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/appndd_c.html"""
    _append(item, cell)


def appndi(item, cell):
    """Append item(s) to an integer cell.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/appndi_c.html"""
    _append(item, cell)


def appndc(item, cell):
    """Append item(s) to a character cell.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/appndc_c.html"""
    _append(item, cell)


def cell_double(cell_size):
    """A double precision cell."""
    return SpiceCell(DOUBLE, cell_size)


def cell_int(cell_size):
    """An integer cell."""
    return SpiceCell(INT, cell_size)


def cell_char(cell_size, length):
    """A character cell."""
    return SpiceCell(CHAR, cell_size, length)


def cell_bool(cell_size):
    """A boolean cell."""
    from .cells import BOOL
    return SpiceCell(BOOL, cell_size)


def cell_time(cell_size):
    """A time cell."""
    from .cells import TIME
    return SpiceCell(TIME, cell_size)


# ---- sets (any type of cell: SPICELIB's routines for numbers, Python for
# strings)


def _set_values(cell):
    if cell.dtype not in (CHAR, DOUBLE, INT):
        raise NotImplementedError('sets of character, double precision and integer cells only')
    return list(cell)


def _new_like(cell, values, size=None):
    values = sorted(set(values))
    if cell.is_char():
        return from_values(values, CHAR, size or max(cell.size, len(values)))
    return from_values(np.asarray(values, dtype=cell._buf.dtype), cell.dtype, size or max(cell.size, len(values)))


def union(a, b):
    """The union of two sets (cells of the same type).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/union_c.html"""
    return _new_like(a, _set_values(a) + _set_values(b), a.size + b.size)


def inter(a, b):
    """The intersection of two sets.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/inter_c.html"""
    sb = set(_set_values(b))
    return _new_like(a, [x for x in _set_values(a) if x in sb], max(a.size, b.size))


def diff(a, b):
    """The difference of two sets (elements of a not in b).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/diff_c.html"""
    sb = set(_set_values(b))
    return _new_like(a, [x for x in _set_values(a) if x not in sb], max(a.size, b.size))


def sdiff(a, b):
    """The symmetric difference of two sets.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/sdiff_c.html"""
    sa, sb = set(_set_values(a)), set(_set_values(b))
    return _new_like(a, list(sa ^ sb), a.size + b.size)


def set_c(a, op, b):
    """Compare two sets: op is '=', '<>', '<=', '<', '>=', '>', '&'
    (a common element) or '~' (none).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/set_c.html"""
    sa, sb = set(_set_values(a)), set(_set_values(b))
    ops = {'=': sa == sb, '<>': sa != sb, '<=': sa <= sb, '<': sa < sb, '>=': sa >= sb,
           '>': sa > sb, '&': bool(sa & sb), '~': not (sa & sb)}
    key = op.strip()
    if key not in ops:
        raise dynamically_instantiate_spiceyerror(short='SPICE(INVALIDOPERATION)',
                                                  long=f'The operation "{op}" is not recognized.')
    return ops[key]


def _insert(item, cell):
    items = list(item) if hasattr(item, '__iter__') and not isinstance(item, (str, bytes)) else [item]
    values = sorted(set(_set_values(cell)) | set(items))
    if len(values) > cell.size:
        raise dynamically_instantiate_spiceyerror(short='SPICE(SETEXCESS)',
                                                  long='The set cannot hold the inserted elements.')
    if cell.is_char():
        cell._strings = [str(v) for v in values]
    else:
        cell._buf[6:6 + len(values)] = values
        cell._buf[5] = len(values)


def insrtc(item, inset):
    """Insert an item (or items) into a character set.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/insrtc_c.html"""
    _insert(item, inset)


def elemc(item, inset):
    """Whether an item is an element of a character set.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/elemc_c.html"""
    return item in _set_values(inset)


def removc(item, inset):
    """Remove an item from a character set.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/removc_c.html"""
    inset._strings = [x for x in inset._strings if x != item]


def valid(insize, n, inset):
    """Make a set of the first n elements of a cell: sorted, without
    duplicates, of size insize.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/valid_c.html"""
    values = sorted(set(list(inset)[:n]))
    if inset.is_char():
        inset._size = insize
        inset._strings = values
    else:
        buf = np.zeros(6 + insize, dtype=inset._buf.dtype)
        buf[4] = insize
        buf[6:6 + len(values)] = values
        buf[5] = len(values)
        inset._buf = buf
    return inset


# ---- arrays (CSPICE's order vectors count from 0)


def shellc(ndim, lenvals, array):
    """Sort an array of strings (ASCII order).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/shellc_c.html"""
    return sorted(list(array)[:ndim])


def shelld(ndim, array):
    """Sort a double precision array.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/shelld_c.html"""
    return np.sort(np.asarray(array, dtype=np.float64)[:ndim])


def shelli(ndim, array):
    """Sort an integer array.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/shelli_c.html"""
    return np.sort(np.asarray(array, dtype=np.int32)[:ndim])


def _order(array, ndim):
    a = list(array)[: (len(array) if ndim is None else ndim)]
    return np.array(sorted(range(len(a)), key=lambda k: a[k]), dtype=np.int32)


def orderc(array, ndim=None):
    """The order vector of an array of strings (indices from 0).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/orderc_c.html"""
    return _order(array, ndim)


def orderd(array, ndim=None):
    """The order vector of a double precision array (indices from 0).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/orderd_c.html"""
    return _order(array, ndim)


def orderi(array, ndim=None):
    """The order vector of an integer array (indices from 0).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/orderi_c.html"""
    return _order(array, ndim)


def reordc(iorder, ndim, lenvals, array):
    """Reorder an array of strings by an order vector.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/reordc_c.html"""
    a = list(array)
    return [a[k] for k in list(iorder)[:ndim]]


def reordd(iorder, ndim, array):
    """Reorder a double precision array by an order vector.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/reordd_c.html"""
    return np.asarray(array, dtype=np.float64)[np.asarray(iorder)[:ndim]]


def reordi(iorder, ndim, array):
    """Reorder an integer array by an order vector.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/reordi_c.html"""
    return np.asarray(array, dtype=np.int32)[np.asarray(iorder)[:ndim]]


def reordl(iorder, ndim, array):
    """Reorder a logical array by an order vector.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/reordl_c.html"""
    return np.asarray(array, dtype=bool)[np.asarray(iorder)[:ndim]]


def isordv(array, n):
    """Whether an array of n integers is an order vector (a permutation of
    0 to n - 1).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/isordv_c.html"""
    a = list(array)[:n]
    return sorted(a) == list(range(n))


def intmax():
    """The largest integer.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/intmax_c.html"""
    return 2 ** 31 - 1


def intmin():
    """The smallest integer.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/intmin_c.html"""
    return -(2 ** 31)


def xposeg(matrix):
    """The transpose of a matrix.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/xposeg_c.html"""
    return np.ascontiguousarray(np.asarray(matrix, dtype=np.float64).T)


def xpose6(m):
    """The transpose of a 6x6 matrix.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/xpose6_c.html"""
    return np.ascontiguousarray(np.asarray(m, dtype=np.float64).reshape(6, 6).T)


# ---- strings


def iswhsp(string):
    """Whether a string is empty or white space only.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/iswhsp_c.html"""
    return string.strip() == ''


def lparse(inlist, delim, nmax):
    """Parse a list of items delimited by a character (the items
    left-justified, trailing blanks removed).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/lparse_c.html"""
    return [x.strip() for x in inlist.split(delim)][:nmax]


def lparsm(inlist, delims, nmax, lenout=None):
    """Parse a list of items separated by any of several delimiters.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/lparsm_c.html"""
    import re
    if not delims:
        return [inlist.strip()][:nmax]
    others = delims.replace(' ', '')
    text = inlist.strip()
    if ' ' in delims:
        # a run of blanks, with or without another delimiter, separates once
        pattern = r'\s*[' + re.escape(others) + r']\s*|\s+' if others else r'\s+'
    else:
        pattern = '[' + re.escape(others) + ']'
    return [x.strip() for x in re.split(pattern, text)][:nmax]


def lparss(inlist, delims, nmax=20, length=50):
    """Parse a list of items into a character set (sorted, unique).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/lparss_c.html"""
    items = sorted(set(lparsm(inlist, delims, 10 ** 9)))
    return from_values(items, CHAR, nmax)


def hx2dp(string):
    """Convert a string in base 16 'scientific notation' to a double
    precision number (the error message if it cannot).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/hx2dp_c.html"""
    number, error, errmsg = _ctx().hx2dp(string)
    return errmsg if error else number


# ---- datetime (SpiceyPy's helpers)


def et2datetime(et):
    """A datetime (UTC) of an epoch, or an array of them.

    SpiceyPy's helper."""
    from datetime import datetime, timezone
    def one(e):
        s = _ctx().et2utc(e, 'ISOC', 6)
        return datetime.strptime(s, '%Y-%m-%dT%H:%M:%S.%f').replace(tzinfo=timezone.utc)
    if hasattr(et, '__iter__'):
        return np.array([one(e) for e in et])
    return one(et)


def datetime2et(dt):
    """The epoch of a datetime (UTC if naive), or of an array of them.

    SpiceyPy's helper."""
    from datetime import timezone
    def one(d):
        if d.tzinfo is not None:
            d = d.astimezone(timezone.utc).replace(tzinfo=None)
        return _ctx().str2et(d.isoformat())
    if hasattr(dt, '__iter__'):
        return np.array([one(d) for d in dt])
    return one(dt)


# ---- GF searches (SpiceyPy's signatures, with cells; PEPPER's searches
# take and return (n, 2) windows: gf_distance, ...)


def gfdist(target, abcorr, obsrvr, relate, refval, adjust, step, nintvls, cnfine, result=None):
    """Return the time window over which a specified constraint on observer-
    target distance is met.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfdist_c.html"""
    return _fill(result, _ctx().gf_distance(target, abcorr, obsrvr, relate, refval, adjust, step,
                                            _intervals(cnfine)))


def gfposc(target, inframe, abcorr, obsrvr, crdsys, coord, relate, refval, adjust, step, nintvls, cnfine,
           result=None):
    """Determine time intervals for which a coordinate of an observer-target
    position vector satisfies a numerical constraint.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfposc_c.html"""
    return _fill(result, _ctx().gf_position(target, inframe, abcorr, obsrvr, crdsys, coord, relate, refval,
                                            adjust, step, _intervals(cnfine)))


def gfsubc(target, fixref, method, abcorr, obsrvr, crdsys, coord, relate, refval, adjust, step, nintvls,
           cnfine, result=None):
    """Determine time intervals for which a coordinate of a subpoint position
    vector satisfies a numerical constraint.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfsubc_c.html"""
    return _fill(result, _ctx().gf_subpoint(target, fixref, method, abcorr, obsrvr, crdsys, coord, relate,
                                            refval, adjust, step, _intervals(cnfine)))


def gfsntc(target, fixref, method, abcorr, obsrvr, dref, dvec, crdsys, coord, relate, refval, adjust, step,
           nintvls, cnfine, result=None):
    """Determine time intervals for which a coordinate of a surface intercept
    position vector satisfies a numerical constraint.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfsntc_c.html"""
    return _fill(result, _ctx().gf_intercept(target, fixref, method, abcorr, obsrvr, dref, dvec, crdsys,
                                             coord, relate, refval, adjust, step, _intervals(cnfine)))


def gfsep(targ1, shape1, inframe1, targ2, shape2, inframe2, abcorr, obsrvr, relate, refval, adjust, step,
          nintvls, cnfine, result=None):
    """Determine time intervals when the angular separation between the
    position vectors of two target bodies relative to an observer satisfies
    a numerical relationship.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfsep_c.html"""
    return _fill(result, _ctx().gf_separation(targ1, shape1, inframe1, targ2, shape2, inframe2, abcorr,
                                              obsrvr, relate, refval, adjust, step, _intervals(cnfine)))


def gfoclt(occtyp, front, fshape, fframe, back, bshape, bframe, abcorr, obsrvr, step, cnfine, result=None):
    """Determine time intervals when an observer sees one target occulted by,
    or in transit across, another.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfoclt_c.html"""
    return _fill(result, _ctx().gf_occultation(occtyp, front, fshape, fframe, back, bshape, bframe, abcorr,
                                               obsrvr, step, _intervals(cnfine)))


def gfilum(method, angtyp, target, illumn, fixref, abcorr, obsrvr, spoint, relate, refval, adjust, step,
           nintvls, cnfine, result=None):
    """Return the time window over which a specified constraint on the
    observed phase, solar incidence, or emission angle at a specified target
    body surface point is met.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfilum_c.html"""
    return _fill(result, _ctx().gf_illumination(method, angtyp, target, illumn, fixref, abcorr, obsrvr,
                                                spoint, relate, refval, adjust, step, _intervals(cnfine)))


def gfpa(target, illmin, abcorr, obsrvr, relate, refval, adjust, step, nintvls, cnfine, result=None):
    """Determine time intervals for which a specified constraint on the phase
    angle between an illumination source, a target, and observer body
    centers is met.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfpa_c.html"""
    return _fill(result, _ctx().gf_phase(target, illmin, abcorr, obsrvr, relate, refval, adjust, step,
                                         _intervals(cnfine)))


def gfrr(target, abcorr, obsrvr, relate, refval, adjust, step, nintvls, cnfine, result=None):
    """Determine time intervals for which a specified constraint on the
    observer-target range rate is met.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfrr_c.html"""
    return _fill(result, _ctx().gf_range_rate(target, abcorr, obsrvr, relate, refval, adjust, step,
                                              _intervals(cnfine)))


def gftfov(inst, target, tshape, tframe, abcorr, obsrvr, step, cnfine, result=None):
    """Determine time intervals when a specified ephemeris object intersects
    the space bounded by the field-of-view (FOV) of a specified instrument.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gftfov_c.html"""
    return _fill(result, _ctx().gf_target_in_fov(inst, target, tshape, tframe, abcorr, obsrvr, step,
                                                 _intervals(cnfine)))


def gfrfov(inst, raydir, rframe, abcorr, obsrvr, step, cnfine, result=None):
    """Determine time intervals when a specified ray intersects the space
    bounded by the field-of-view (FOV) of a specified instrument.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfrfov_c.html"""
    return _fill(result, _ctx().gf_ray_in_fov(inst, raydir, rframe, abcorr, obsrvr, step,
                                              _intervals(cnfine)))


def gfuds(udfuns, udqdec, relate, refval, adjust, step, nintvls, cnfine, result=None):
    """Perform a GF search on a user defined scalar quantity: `udfuns(et)`
    gives it; `udqdec(udfuns, et)` whether it is decreasing (for instance
    `uddc(udfuns, et, dt)`).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfuds_c.html"""
    return _fill(result, _ctx().gf_user_scalar(udfuns, relate, refval, adjust, step, _intervals(cnfine),
                                               udqdec))


def gfudb(udfuns, udfunb, step, cnfine, result=None):
    """Perform a GF search on a user defined boolean quantity:
    `udfunb(udfuns, et)`.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/gfudb_c.html"""
    return _fill(result, _ctx().gf_user_boolean(udfuns, udfunb, step, _intervals(cnfine)))


def uddc(udfunc, x, dx):
    """Whether a scalar function of time is decreasing at x (its derivative
    from its values at x - dx and x + dx).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/uddc_c.html"""
    return _ctx().uddc(udfunc, x, dx)


# ---- error handling (per context: the calling thread's)


def failed():
    """Whether an error is pending in this thread's context (errors are
    raised as exceptions).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/failed_c.html"""
    return _ctx().failed()


def reset():
    """Clear the error status of this thread's context.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/reset_c.html"""
    _ctx().reset()


def erract(op, lenout, action=None):
    """The error action: always RETURN in PepperyPy (errors are raised as
    exceptions); setting it is accepted and ignored.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/erract_c.html"""
    return 'RETURN'


def errdev(op, lenout, device=None):
    """The error output device: always NULL in PepperyPy (errors are raised
    as exceptions); setting it is accepted and ignored.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/errdev_c.html"""
    return 'NULL'


def errprt(op, lislen, inlist=None):
    """The error output items: SPICELIB's default list (setting it is
    accepted and ignored: errors are raised as exceptions).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/errprt_c.html"""
    return 'SHORT, LONG, EXPLAIN, TRACEBACK, DEFAULT'


_errors = __import__('threading').local()


def setmsg(message):
    """Set the long error message of the error sigerr signals next.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/setmsg_c.html"""
    _errors.long = message


def _replace_marker(marker, text):
    long = getattr(_errors, 'long', '')
    _errors.long = long.replace(marker, text, 1)


def errch(marker, string):
    """Replace a marker in the long error message setmsg set by a string.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/errch_c.html"""
    _replace_marker(marker, string)


def errdp(marker, number):
    """Replace a marker in the long error message by a number (SPICELIB's
    format, as DPSTR with 14 significant digits).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/errdp_c.html"""
    _replace_marker(marker, _core.dpstr(number, 14).strip())


def errint(marker, number):
    """Replace a marker in the long error message by an integer.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/errint_c.html"""
    _replace_marker(marker, str(int(number)))


def sigerr(message):
    """Signal an error in this thread's context, as CSPICE does: the error
    is pending (failed, getmsg) until reset, and the next query raises it.
    The long message is the one setmsg set.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/sigerr_c.html"""
    long = getattr(_errors, 'long', '')
    _errors.long = ''
    _ctx().signal(message, long)


def getmsg(option, lenout=None):
    """A message of the error pending in this thread's context: 'SHORT',
    'EXPLAIN' or 'LONG'.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/getmsg_c.html"""
    return _ctx().getmsg(option)


def dasadc(handle, n, bpos, epos, datlen, data):
    """Add character data to a DAS file (data padded to datlen characters).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/dasadc_c.html"""
    data = [data] if isinstance(data, str) else list(data)
    _ctx().dasadc(handle, n, bpos, epos, datlen, [str(x).ljust(datlen) for x in data])


def return_c():
    """Whether routines should return at once: never pending in PepperyPy
    (errors are raised as exceptions).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/return_c.html"""
    return False


# The traceback: PEPPER keeps none of its own routines; these keep the
# modules the program checks in, per thread.


def _trace():
    if not hasattr(_errors, 'trace'):
        _errors.trace = []
    return _errors.trace


def chkin(module):
    """Check in a module (the program's traceback, per thread).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/chkin_c.html"""
    _trace().append(module)


def chkout(module):
    """Check out a module.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/chkout_c.html"""
    t = _trace()
    if t and t[-1] == module:
        t.pop()


def trcdep():
    """The depth of the traceback (the program's modules checked in).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/trcdep_c.html"""
    return len(_trace())


def trcnam(index, namlen=None):
    """The module at a depth of the traceback (from 0).

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/trcnam_c.html"""
    return _trace()[index]


def trcoff():
    """Turn tracing off: does nothing."""


def qcktrc(lenout=None):
    """The traceback of the modules checked in ('a --> b').

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/qcktrc_c.html"""
    return ' --> '.join(_trace())


def tkvrsn(item='TOOLKIT'):
    """The toolkit version: PEPPER's.

    https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/C/cspice/tkvrsn_c.html"""
    return toolkit_version


class KernelPool:
    """Within the block, the module-level functions use a new instance with
    only these kernels; the current instance (its kernels and kernel pool)
    is unchanged and in use again after the block.

        with KernelPool(['naif0012.tls', 'de440s.bsp']):
            et = spice.str2et('2026 NOV 8')
    """

    def __init__(self, kernel_paths):
        if isinstance(kernel_paths, (str, bytes)) or not hasattr(kernel_paths, '__iter__'):
            kernel_paths = [kernel_paths]
        self.kernel_paths = [str(k) for k in kernel_paths]
        self._spice = None
        self._cm = None

    def __enter__(self):
        self._spice = _core.Spice()
        self._spice.furnsh(self.kernel_paths)
        self._cm = self._spice.as_current()
        self._cm.__enter__()
        return self

    def __exit__(self, *exc):
        self._cm.__exit__(*exc)
        self._spice.close()
        self._spice = None
        return False
