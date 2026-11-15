# cython: language_level=3, boundscheck=False, wraparound=False, cdivision=True
# cython: embedsignature=True, freethreading_compatible=True
"""PepperyPy's core: the instance (Spice), the contexts (Context), the
conversions between Python and PEPPER's C interface, callbacks,
programmatic providers and prepared queries. The routines themselves are
generated (tools/gen_capi.py) into the included .pxi files.

See plans/pepperypy_plan.md for the design.
"""

cimport numpy as cnp
from numpy cimport NPY_DOUBLE, NPY_INT32
from cpython.pythread cimport PyThread_get_thread_ident
from libc.string cimport memcpy, strlen

from ._pepper cimport *

import threading
import weakref
from contextlib import contextmanager
from contextvars import ContextVar

import numpy as np

from . import config
from .cells import SpiceCell, DOUBLE as _CDOUBLE, INT as _CINT, from_values as _from_values
from .exceptions import (NotFoundError, SpiceyError, dynamically_instantiate_spiceyerror)

cnp.import_array()

cdef extern from "pepper.h" nogil:
    void pepper_runtime_init()

# The Fortran runtime set as a Fortran main program sets it (see pepper.h).
pepper_runtime_init()

cdef extern from "_guard.h" nogil:
    int pp_enter(volatile unsigned long *owner, long *depth, unsigned long me)
    void pp_leave(volatile unsigned long *owner, long *depth)


class ContextInUseError(RuntimeError):
    """A context was used by a thread while another thread was in it.

    A context is for one thread at a time: use a context per thread
    (`Spice.context()` gives each thread its own)."""


class CallbackError(SpiceyError):
    """An exception was raised by a Python callback (a GF function or a
    programmatic provider). The original exception is its cause."""


# ----------------------------------------------------------------------
# Conversions

cdef inline bytes _b(object s):
    """A string argument as bytes."""
    if isinstance(s, str):
        return (<str>s).encode('latin-1')
    if isinstance(s, bytes):
        return <bytes>s
    if isinstance(s, (int, np.integer)):
        return str(s).encode('ascii')
    if isinstance(s, np.str_):
        return str(s).encode('latin-1')
    raise TypeError(f'expected a string, got {type(s).__name__}')


cdef inline str _s(const char *p):
    """A NUL-terminated C string."""
    return p[:strlen(p)].decode('latin-1')


cdef inline str _sn(const char *p, int n):
    """A C string of length n."""
    if p == NULL or n <= 0:
        return ''
    return p[:n].decode('latin-1')


cdef list _slist(const char *p, int n):
    """n NUL-terminated strings, one after the other."""
    cdef list out = []
    cdef int i
    cdef size_t k
    for i in range(n):
        k = strlen(p)
        out.append(p[:k].decode('latin-1'))
        p += k + 1
    return out


cdef tuple _strblock(object strings):
    """Strings as a block of fixed-width NUL-padded strings: (bytes, n, width)."""
    if isinstance(strings, (str, bytes)):
        strings = [strings]
    cdef list bs = [_b(x) for x in strings]
    cdef int width = max([len(x) for x in bs] + [1]) + 1
    return b''.join([x.ljust(width, b'\0') for x in bs]), len(bs), width


cdef cnp.ndarray _empty(int dt, int d1, int d2):
    """An array for a result: (d1,), or a d1 x d2 matrix (Fortran order)."""
    cdef cnp.npy_intp dims[2]
    dims[0] = d1
    dims[1] = d2
    if d2 < 0:
        return cnp.PyArray_EMPTY(1, dims, dt, 0)
    return cnp.PyArray_EMPTY(2, dims, dt, 1)


cdef cnp.ndarray _input(object x, int dt, int d1, int d2, bint copy):
    """An array argument of fixed shape: (d1,), or a d1 x d2 matrix (passed in
    Fortran order). A copy if the routine changes it."""
    cdef cnp.ndarray a
    dtype = np.float64 if dt == NPY_DOUBLE else np.int32
    if d2 < 0:
        a = np.array(x, dtype=dtype, copy=True) if copy else np.ascontiguousarray(x, dtype=dtype)
        if a.ndim != 1:
            a = a.reshape(-1)
        if a.shape[0] < d1:
            raise ValueError(f'expected an array of {d1} values, got {a.shape[0]}')
    else:
        a = np.array(x, dtype=dtype, order='F', copy=True) if copy else np.asfortranarray(x, dtype=dtype)
        if a.ndim != 2 or a.shape[0] != d1 or a.shape[1] != d2:
            raise ValueError(f'expected a {d1}x{d2} array, got shape {np.shape(x)}')
    return a


cdef cnp.ndarray _vrows(object x, int dt, int k, int n, bint copy):
    """A list of n vectors of k values: rows (n, k), the memory of Fortran's
    (k, n)."""
    dtype = np.float64 if dt == NPY_DOUBLE else np.int32
    cdef cnp.ndarray a = np.array(x, dtype=dtype, copy=True) if copy else np.ascontiguousarray(x, dtype=dtype)
    if a.ndim == 1 and a.shape[0] == k * n:
        a = a.reshape(n, k)
    if a.ndim != 2 or a.shape[0] < n or a.shape[1] != k:
        raise ValueError(f'expected {n} vectors of {k} values, got shape {np.shape(x)}')
    return a


cdef cnp.ndarray _rows(object x, int dt, int rank):
    """An array argument of any size: a vector, or a list of vectors (rows)."""
    dtype = np.float64 if dt == NPY_DOUBLE else np.int32
    cdef cnp.ndarray a = np.ascontiguousarray(x, dtype=dtype)
    if rank == 1:
        if a.ndim != 1:
            a = a.reshape(-1)
    elif a.ndim == 1:
        a = a.reshape(1, -1) if a.shape[0] else a.reshape(0, 0)
    elif a.ndim != 2:
        raise ValueError(f'expected a 2-dimensional array, got shape {np.shape(x)}')
    return a


cdef cnp.ndarray _copy1(int dt, const void *p, int n):
    """A result of any size: a vector."""
    cdef cnp.npy_intp dims[1]
    dims[0] = n
    cdef cnp.ndarray a = cnp.PyArray_EMPTY(1, dims, dt, 0)
    if n > 0:
        memcpy(cnp.PyArray_DATA(a), p, n * (8 if dt == NPY_DOUBLE else 4))
    return a


cdef cnp.ndarray _copy2(int dt, const void *p, int rows, int cols):
    """A result of any size: a list of vectors (rows)."""
    cdef cnp.npy_intp dims[2]
    dims[0] = rows
    dims[1] = cols
    cdef cnp.ndarray a = cnp.PyArray_EMPTY(2, dims, dt, 0)
    if rows > 0 and cols > 0:
        memcpy(cnp.PyArray_DATA(a), p, rows * cols * (8 if dt == NPY_DOUBLE else 4))
    return a


cdef object _autocell(int dt, list others):
    """A cell for a routine to fill, when the caller gives none: of the size
    of the cells given, at least 2000 (SpiceyPy's default)."""
    cdef long total = 0
    for o in others:
        if isinstance(o, SpiceCell):
            total += (<object>o).size
    return SpiceCell(_CDOUBLE if dt == NPY_DOUBLE else _CINT, max(2000, total))


cdef cnp.ndarray _cellbuf(object c, int dt, bint input_only):
    """The buffer of a cell (SPICELIB's layout). An input may also be given as
    values (for a window: an (n, 2) array of intervals)."""
    if isinstance(c, SpiceCell):
        buf = (<object>c)._buf
        if buf is None or (dt == NPY_DOUBLE) != (buf.dtype == np.float64):
            raise TypeError(f'expected a {"double precision" if dt == NPY_DOUBLE else "integer"} cell')
        return buf
    if input_only:
        return _from_values(np.asarray(c), _CDOUBLE if dt == NPY_DOUBLE else _CINT)._buf
    raise TypeError(f'expected a SpiceCell, got {type(c).__name__}')


cdef inline bint _iterable(object x):
    return hasattr(x, '__iter__') and not isinstance(x, (str, bytes))


cdef object _stack(list results):
    """The results of a vectorized call: each output stacked."""
    if not results:
        return np.array([])
    first = results[0]
    if isinstance(first, tuple):
        return tuple(_stack1([r[k] for r in results]) for k in range(len(first)))
    return _stack1(results)


cdef object _stack1(list values):
    return np.array(values)


# Whether a false found flag raises NotFoundError: per thread (and asyncio
# task); None follows config.catch_false_founds.
_found_state = ContextVar('pepperypy_catch_false_founds', default=None)


cpdef bint catch_false_founds():
    """Whether a false found flag raises NotFoundError here (see
    found_check and no_found_check)."""
    s = _found_state.get()
    return config.catch_false_founds if s is None else s


cdef object _found(str name, tuple outs, bint found):
    """SpiceyPy's found handling: NotFoundError, or the found flag returned
    last (no_found_check)."""
    if catch_false_founds():
        if not found:
            raise NotFoundError(f'Spice returns not found for function: {name}', found=found)
        if len(outs) == 1:
            return outs[0]
        return outs
    return outs + (found,)


def _found_public(name, outs, found):
    """SpiceyPy's found handling, for the hand-written functions."""
    return _found(name, tuple(outs), found)


# ----------------------------------------------------------------------
# Errors

cdef dict _messages(void *ctx, bint instance):
    cdef char buf[2048]
    out = {}
    for option in ('SHORT', 'EXPLAIN', 'LONG'):
        if instance:
            pepper_spice_getmsg(ctx, option.encode(), 2048, buf)
        else:
            pepper_getmsg(ctx, option.encode(), 2048, buf)
        out[option] = _s(buf)
    if instance:
        pepper_spice_reset(ctx)
    else:
        pepper_reset(ctx)
    return out


_pending = threading.local()   # exceptions raised by callbacks, per thread


cdef object _spice_error(dict m):
    exc = dynamically_instantiate_spiceyerror(short=m['SHORT'], explain=m['EXPLAIN'], long=m['LONG'])
    pending = getattr(_pending, 'exc', None)
    if pending is not None:
        _pending.exc = None
        if m['SHORT'] == 'SPICE(PYTHONCALLBACK)':
            err = CallbackError(short=m['SHORT'], explain=m['EXPLAIN'], long=m['LONG'])
            err.__cause__ = pending
            return err
    return exc


cdef void _callback_failed(void *ctx, object exc) noexcept:
    """A callback raised: keep the exception for the thread, signal an error
    in the context so that the search or query stops."""
    try:
        _pending.exc = exc
        if ctx != NULL and not pepper_failed(ctx):
            msg = f'A Python callback raised {type(exc).__name__}: {exc}'.encode('latin-1', 'replace')
            pepper_sigerr(ctx, b'SPICE(PYTHONCALLBACK)', msg[:1800])
    except BaseException:
        pass


# ----------------------------------------------------------------------
# The instance

_current = ContextVar('pepperypy_instance', default=None)


cdef class _SpiceBase:
    """The state of an instance (see Spice)."""

    cdef void *_ptr
    cdef object _lock
    cdef object _tls
    cdef object _contexts
    cdef dict _bodies
    cdef dict _frames
    cdef object __weakref__

    cdef int _check(self) except -1:
        if self._ptr == NULL:
            raise ValueError('the instance is closed')
        return 0

    cdef int _raise(self) except -1:
        raise _spice_error(_messages(self._ptr, True))


cdef class _ContextBase:
    """The state of a context (see Context)."""

    cdef void *_ptr
    cdef readonly _SpiceBase spice
    cdef volatile unsigned long _owner
    cdef long _depth
    cdef object __weakref__

    cdef inline int _enter(self) except -1:
        if self._ptr == NULL:
            raise ValueError('the context is closed')
        if pp_enter(&self._owner, &self._depth, PyThread_get_thread_ident()) != 0:
            raise ContextInUseError('another thread is using this context; use a context per thread '
                                    '(Spice.context() gives each thread its own)')
        return 0

    cdef inline void _leave(self) noexcept:
        pp_leave(&self._owner, &self._depth)

    cdef int _raise(self) except -1:
        raise _spice_error(_messages(self._ptr, False))

    cdef void _free(self) noexcept:
        if self._ptr != NULL:
            pepper_context_free(self._ptr)
            self._ptr = NULL


# ----------------------------------------------------------------------
# Kernels built in memory


cdef class KernelBuilder:
    """SPK segments made from arrays, loaded as a kernel with no file
    (`Spice.load_kernel`). The interpolation is SPICE's: Lagrange (SPK type
    9) or Hermite (type 13, odd degree). A builder belongs to no instance:
    each thread can fill its own while others query. Frames are resolved
    when it is loaded.

        kb = KernelBuilder()
        kb.spk_hermite(-10001, 399, 'J2000', ets, states, 7)
        spice.load_kernel('my_trajectory', kb)
        spice.write_kernel('my_trajectory', 'my_trajectory.bsp')
    """

    cdef void *_ptr

    def __cinit__(self):
        self._ptr = pepper_builder_new()

    def __dealloc__(self):
        if self._ptr != NULL:
            pepper_builder_free(self._ptr)
            self._ptr = NULL

    def _add(self, int type, int body, int center, frame, epochs, states, int degree,
             tbeg, tend, ident, adapt, ctx):
        cdef cnp.ndarray e = np.ascontiguousarray(np.atleast_1d(epochs), dtype=np.float64).reshape(-1)
        cdef cnp.ndarray s = np.ascontiguousarray(states, dtype=np.float64)
        if s.ndim == 1 and s.shape[0] == 6:
            s = s.reshape(1, 6)
        if s.ndim != 2 or s.shape[1] != 6 or s.shape[0] != e.shape[0]:
            raise ValueError(f'states must be an (n, 6) array for n = {e.shape[0]} epochs, '
                             f'not {np.shape(states)}')
        cdef int bounds = 0 if (tbeg is None and tend is None) else 1
        cdef double tb = 0.0, te = 0.0
        if bounds:
            tb = float(tbeg) if tbeg is not None else float(e[0])
            te = float(tend) if tend is not None else float(e[e.shape[0] - 1])
        cdef _ContextBase c = (current_context() if ctx is None else ctx)
        cdef bytes bf = _b(frame), bi = _b(ident)
        c._enter()
        try:
            if pepper_builder_spk(c._ptr, self._ptr, type, body, center, bf, <int>e.shape[0],
                                  <const double*>cnp.PyArray_DATA(e), <const double*>cnp.PyArray_DATA(s),
                                  degree, bounds, tb, te, bi, 1 if adapt else 0):
                c._raise()
        finally:
            c._leave()

    def spk_lagrange(self, int body, int center, frame, epochs, states, int degree,
                     tbeg=None, tend=None, ident='', adapt=False, ctx=None):
        """Add an SPK segment of type 9 (Lagrange interpolation): `states`
        is an (n, 6) array (km, km/s) at the n `epochs` (TDB seconds past
        J2000, increasing). `tbeg`, `tend`: the coverage, inside the epochs
        (default: their span). `adapt`: a segment too short for its degree
        has its degree lowered (two states make a Hermite cubic, one state is
        dropped). Errors are raised through `ctx` (default: the calling
        thread's context of the current instance)."""
        self._add(9, body, center, frame, epochs, states, degree, tbeg, tend, ident, adapt, ctx)

    def spk_hermite(self, int body, int center, frame, epochs, states, int degree,
                    tbeg=None, tend=None, ident='', adapt=False, ctx=None):
        """Add an SPK segment of type 13 (Hermite interpolation, odd
        degree); the arguments are those of `spk_lagrange`."""
        self._add(13, body, center, frame, epochs, states, degree, tbeg, tend, ident, adapt, ctx)

    def define_body(self, name, int code, ctx=None):
        """Define a body name of the kernel: while it is loaded, `name`
        translates to `code` (bodn2c, spkezr, ...); when it is unloaded the
        definition is removed, and an earlier one of the name (boddef's,
        another kernel's) is in effect again. Names from the kernel pool take
        precedence, as over boddef's."""
        cdef _ContextBase c = (current_context() if ctx is None else ctx)
        cdef bytes b = _b(name)
        c._enter()
        try:
            if pepper_builder_define_body(c._ptr, self._ptr, b, code):
                c._raise()
        finally:
            c._leave()

    def set_format(self, text):
        """The kernel's description (`kernel_format`)."""
        cdef bytes b = _b(text)
        pepper_builder_set_format(self._ptr, b)

    def clear(self):
        """Remove the segments."""
        pepper_builder_clear(self._ptr)

    def nsegments(self):
        """The number of segments."""
        return pepper_builder_nsegments(self._ptr)

    def __len__(self):
        return pepper_builder_nsegments(self._ptr)



include "_spice_methods.pxi"


cdef class Spice(_SpiceRoutines):
    """A SPICE instance: loaded kernels, the kernel pool, names defined with
    boddef, programmatic bodies and frames. Kernel changes are methods of
    the instance; queries are made through contexts (`context()`).

    Kernels must not be changed while other threads query. The module-level
    functions (`pepperypy.furnsh`, `pepperypy.spkezr`, ...) use the current
    instance (`as_current`; by default the one made at import)."""

    def __cinit__(self):
        self._ptr = pepper_spice_new()
        self._lock = threading.RLock()
        self._tls = threading.local()
        self._contexts = weakref.WeakSet()
        self._bodies = {}
        self._frames = {}

    def __dealloc__(self):
        if self._ptr != NULL:
            pepper_spice_free(self._ptr)
            self._ptr = NULL

    def close(self):
        """Unload the kernels and free the instance (its contexts first)."""
        for c in list(self._contexts):
            (<_ContextBase>c)._free()
        if self._ptr != NULL:
            pepper_spice_free(self._ptr)
            self._ptr = NULL
        self._bodies.clear()
        self._frames.clear()

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        self.close()

    cpdef object context(self):
        """The calling thread's context of this instance (made on first use,
        kept until the thread ends). The module-level functions use it."""
        c = getattr(self._tls, 'ctx', None)
        if c is None or (<_ContextBase>c)._ptr == NULL:
            self._check()
            c = Context(self)
            self._tls.ctx = c
        return c

    def new_context(self):
        """A new context, owned by the caller (one thread at a time)."""
        self._check()
        return Context(self)

    @contextmanager
    def as_current(self):
        """Make this instance the one the module-level functions use, in this
        block (for this thread or asyncio task)."""
        token = _current.set(self)
        try:
            yield self
        finally:
            _current.reset(token)

    # ---- kernels

    def furnsh(self, path, in_memory=None):
        """Load kernels (a file, a meta-kernel, or a list of them).
        `in_memory`: read binary kernels whole into memory (default: the
        instance's choice, `set_in_memory`).

        https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/furnsh.html"""
        self._check()
        paths = [path] if not _iterable(path) else list(path)
        cdef int mem = -1 if in_memory is None else (1 if in_memory else 0)
        cdef bytes b
        cdef const char *p
        cdef int st
        with self._lock:
            for one in paths:
                b = _b(str(one) if not isinstance(one, (str, bytes)) else one)
                p = b
                with nogil:
                    st = pepper_spice_furnsh(self._ptr, p, mem)
                if st:
                    self._raise()

    def kclear(self):
        """Unload all kernels, clear the kernel pool, remove the programmatic
        bodies and frames.

        https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/kclear.html"""
        self._check()
        cdef int st
        with self._lock:
            with nogil:
                st = pepper_spice_kclear(self._ptr)
            self._bodies.clear()
            self._frames.clear()
            if st:
                self._raise()

    def pdpool(self, name, values):
        """Insert real values into the kernel pool.

        https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/pdpool.html"""
        self._check()
        cdef cnp.ndarray a = np.ascontiguousarray(np.atleast_1d(values), dtype=np.float64).reshape(-1)
        cdef bytes b = _b(name)
        with self._lock:
            if pepper_spice_pdpool(self._ptr, b, <int>a.shape[0], <const double*>cnp.PyArray_DATA(a)):
                self._raise()

    def pipool(self, name, ivals):
        """Insert integer values into the kernel pool.

        https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/pipool.html"""
        self._check()
        cdef cnp.ndarray a = np.ascontiguousarray(np.atleast_1d(ivals), dtype=np.int32).reshape(-1)
        cdef bytes b = _b(name)
        with self._lock:
            if pepper_spice_pipool(self._ptr, b, <int>a.shape[0], <const int*>cnp.PyArray_DATA(a)):
                self._raise()

    def pcpool(self, name, cvals):
        """Insert strings into the kernel pool.

        https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/pcpool.html"""
        self._check()
        block, n, width = _strblock(cvals)
        cdef bytes b = _b(name)
        with self._lock:
            if pepper_spice_pcpool(self._ptr, b, n, width, block):
                self._raise()

    def lmpool(self, cvals):
        """Load kernel pool assignments from lines of a text kernel.

        https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/lmpool.html"""
        self._check()
        block, n, width = _strblock(cvals)
        with self._lock:
            if pepper_spice_lmpool(self._ptr, n, width, block):
                self._raise()

    def write_kernel(self, name, file, ck_file=None, text_file=None):
        """Write a kernel built in memory to SPICE files: an SPK (`file`;
        '' for none) and, for its rotation tables, a CK (`ck_file`) and a text
        kernel (`text_file`) with their clocks, frame definitions and the
        kernel's body names. The files must not exist."""
        self._check()
        cdef bytes bn = _b(name), bf = _b(file)
        cdef bytes bc = _b(ck_file) if ck_file is not None else b''
        cdef bytes bt = _b(text_file) if text_file is not None else b''
        with self._lock:
            if pepper_spice_write_kernel(self._ptr, bn, bf, bc, bt):
                self._raise()

    def load_kernel(self, name, KernelBuilder builder, keep=False):
        """Load a KernelBuilder as a kernel named `name`, with no file: an
        SPK in memory, used like an SPK file (queries, spkcov and spkobj by
        its name, kdata, unload). A kernel loaded under the same name is
        replaced. The builder is emptied, unless `keep`. `write_kernel`
        writes it to an SPK file."""
        self._check()
        cdef bytes b = _b(name)
        with self._lock:
            if pepper_spice_load_kernel(self._ptr, b, builder._ptr, 1 if keep else 0):
                self._raise()

    def set_in_memory(self, flag):
        """Read binary kernels (SPK, CK, binary PCK) whole into memory when
        they are loaded from now on."""
        self._check()
        pepper_spice_set_in_memory(self._ptr, 1 if flag else 0)

    def kernel_memory(self):
        """The memory used by the binary kernels read into memory (bytes)."""
        self._check()
        return pepper_spice_kernel_memory(self._ptr)

    def set_max_open_files(self, int n):
        """The number of files each context keeps open at a time (before the
        contexts open files)."""
        self._check()
        pepper_spice_set_max_open_files(self._ptr, n)

    def max_open_files(self):
        """The number of files each context keeps open at a time."""
        self._check()
        return pepper_spice_max_open_files(self._ptr)

    # ---- programmatic ephemerides and frames

    def add_ephemeris(self, int body, int center, frame, double tbeg, double tend, provider):
        """Add a programmatic body: `provider(ctx, body, et)` returns its state
        (6 values: km, km/s) relative to `center` in `frame` at `et`, or None
        if it has none. `ctx` is the Context of the query (to make queries of
        its own), or None.

        The body takes part in the search for SPK segments as a file loaded
        now (it overrides files loaded before, within [tbeg, tend]).
        Providers are called from the querying threads (under the GIL)."""
        self._check()
        cdef bytes b = _b(frame)
        with self._lock:
            st = pepper_spice_add_ephemeris(self._ptr, body, center, b, tbeg, tend, _state_cb, <void*>provider)
            if st:
                self._raise()
            self._bodies[body] = provider

    def remove_ephemeris(self, int body):
        """Remove a programmatic body."""
        self._check()
        with self._lock:
            if pepper_spice_remove_ephemeris(self._ptr, body):
                self._raise()
            self._bodies.pop(body, None)

    def add_frame(self, name, int code, int center, provider):
        """Add a programmatic frame (no frame kernel needed):
        `provider(ctx, frame, et)` returns `(xform, relative)`, the 6x6 state
        transformation from this frame to the frame `relative` (a name or ID
        code), or None if it has none at `et`."""
        self._check()
        cdef bytes b = _b(name)
        with self._lock:
            st = pepper_spice_add_frame(self._ptr, b, code, center, _xform_cb, <void*>provider)
            if st:
                self._raise()
            self._frames[code] = provider
            self._frames[str(name).upper()] = code

    def remove_frame(self, name):
        """Remove a programmatic frame."""
        self._check()
        cdef bytes b = _b(name)
        with self._lock:
            if pepper_spice_remove_frame(self._ptr, b):
                self._raise()
            code = self._frames.pop(str(name).upper(), None)
            if code is not None:
                self._frames.pop(code, None)


# ----------------------------------------------------------------------
# Contexts

include "_context_methods.pxi"


cdef class Context(_ContextRoutines):
    """A context of an instance: the queries of one thread at a time (every
    SPICELIB query is a method). Errors are the context's own: other threads
    are not affected. Get one with `Spice.context()` (the calling thread's)
    or `Spice.new_context()`."""

    def __cinit__(self, _SpiceBase spice):
        spice._check()
        self.spice = spice
        self._ptr = pepper_context_new(spice._ptr)
        pepper_context_set_user(self._ptr, <void*>self)
        self._owner = 0
        self._depth = 0
        spice._contexts.add(self)

    def __dealloc__(self):
        self._free()

    def close(self):
        """Free the context (closes its files)."""
        self._free()

    def failed(self):
        """Whether an error is pending (errors are raised as exceptions, so
        only after a callback swallowed one)."""
        return self._ptr != NULL and pepper_failed(self._ptr) != 0

    def reset(self):
        """Clear the error status."""
        if self._ptr != NULL:
            pepper_reset(self._ptr)

    def signal(self, short_msg, long_msg=''):
        """Signal an error in the context (SIGERR, with SETMSG's message): it
        is pending until reset; the next query raises it."""
        cdef bytes bs = _b(short_msg), bl = _b(long_msg)
        pepper_sigerr(self._ptr, bs, bl)

    def getmsg(self, option):
        """A message of the pending error: 'SHORT', 'EXPLAIN' or 'LONG'."""
        cdef char buf[2048]
        cdef bytes bo = _b(option)
        pepper_getmsg(self._ptr, bo, 2048, buf)
        return _s(buf)

    # ---- prepared queries

    def prepare_state(self, target, observer, frame, abcorr):
        """A prepared state query: SPKEZR's arguments but the epoch, resolved
        once. Call it with an epoch or an array of epochs."""
        self._enter()
        cdef bytes bt = _b(target), bo = _b(observer), bf = _b(frame), ba = _b(abcorr)
        cdef void *q = NULL
        try:
            if pepper_prepare_state(self._ptr, bt, bo, bf, ba, &q):
                self._raise()
        finally:
            self._leave()
        return StateQuery._make(self, q)

    def prepare_rotation(self, from_, to):
        """A prepared frame transformation query: PXFORM's and SXFORM's
        arguments but the epoch. Call it (or `rotation`) for rotations,
        `transform` for state transformations."""
        self._enter()
        cdef bytes bf = _b(from_), bt = _b(to)
        cdef void *q = NULL
        try:
            if pepper_prepare_rotation(self._ptr, bf, bt, &q):
                self._raise()
        finally:
            self._leave()
        return RotationQuery._make(self, q)

    # ---- GF searches with Python functions

    def gf_user_scalar(self, udfuns, relate, double refval, double adjust, double step, cnfine, udqdec=None):
        """Times when a scalar function of time `udfuns(et)` satisfies a
        condition (GFUDS). `cnfine` is a window, an (n, 2) array of
        intervals; so is the result. `udqdec(udfuns, et)`: whether the
        function is decreasing at et (default: its derivative over one
        second, UDDC)."""
        holder = _GFHolder(udfuns, udqdec)
        cdef cnp.ndarray w = _rows(cnfine, NPY_DOUBLE, 2)
        cdef bytes br = _b(relate)
        cdef const char *pr = br
        cdef void *user = <void*>holder
        cdef int nw = <int>w.shape[0]
        cdef const double *pw = <const double*>cnp.PyArray_DATA(w)
        cdef const double *res = NULL
        cdef int nres = 0, st
        cdef pepper_bool_fn decr = NULL
        if udqdec is not None:
            decr = _bool_cb
        self._enter()
        try:
            with nogil:
                st = pepper_gf_user_scalar(self._ptr, _scalar_cb, decr, user, pr, refval, adjust, step,
                                           nw, pw, &res, &nres)
            if st:
                self._raise()
            return _copy2(NPY_DOUBLE, res, nres, 2)
        finally:
            self._leave()

    def gf_user_boolean(self, udfuns, udfunb, double step, cnfine):
        """Times when a boolean function of time is true (GFUDB):
        `udfunb(udfuns, et)`. `cnfine` and the result are (n, 2) windows."""
        holder = _GFHolder(udfuns, udfunb)
        cdef cnp.ndarray w = _rows(cnfine, NPY_DOUBLE, 2)
        cdef void *user = <void*>holder
        cdef int nw = <int>w.shape[0]
        cdef const double *pw = <const double*>cnp.PyArray_DATA(w)
        cdef const double *res = NULL
        cdef int nres = 0, st
        self._enter()
        try:
            with nogil:
                st = pepper_gf_user_boolean(self._ptr, _scalar_cb, _bool_cb, user, step, nw, pw, &res, &nres)
            if st:
                self._raise()
            return _copy2(NPY_DOUBLE, res, nres, 2)
        finally:
            self._leave()

    def uddc(self, udfuns, double x, double dx):
        """Whether a scalar function of time is decreasing at x: its
        derivative from its values at x - dx and x + dx (UDDC)."""
        holder = _GFHolder(udfuns, None)
        cdef int isdecr = 0, st
        cdef void *user = <void*>holder
        self._enter()
        try:
            with nogil:
                st = pepper_uddc(self._ptr, _scalar_cb, user, x, dx, &isdecr)
            if st:
                self._raise()
            return isdecr != 0
        finally:
            self._leave()


# ----------------------------------------------------------------------
# Callbacks

cdef class _GFHolder:
    cdef object udfuns
    cdef object other

    def __cinit__(self, udfuns, other):
        self.udfuns = udfuns
        self.other = other


cdef void _scalar_cb(void *ctx, void *user, double et, double *value) noexcept with gil:
    value[0] = 0.0
    if pepper_failed(ctx):
        return
    try:
        value[0] = float((<_GFHolder>user).udfuns(et))
    except BaseException as e:
        _callback_failed(ctx, e)


cdef void _bool_cb(void *ctx, void *user, double et, int *value) noexcept with gil:
    value[0] = 0
    if pepper_failed(ctx):
        return
    cdef _GFHolder h = <_GFHolder>user
    try:
        value[0] = 1 if h.other(h.udfuns, et) else 0
    except BaseException as e:
        _callback_failed(ctx, e)


cdef inline object _context_object(void *ctx):
    if ctx == NULL:
        return None
    cdef void *u = pepper_context_user(ctx)
    if u == NULL:
        return None
    return <object>u


cdef int _state_cb(void *ctx, void *user, int body, double et, double *state) noexcept with gil:
    cdef int i
    if ctx != NULL and pepper_failed(ctx):
        return 0
    try:
        r = (<object>user)(_context_object(ctx), body, et)
        if r is None:
            return 0
        a = np.asarray(r, dtype=np.float64).reshape(-1)
        if a.shape[0] != 6:
            raise ValueError(f'a provider returned {a.shape[0]} values for a state (6 expected)')
        for i in range(6):
            state[i] = a[i]
        return 1
    except BaseException as e:
        _callback_failed(ctx, e)
        return 1     # the error, not a missing state, is reported


cdef int _xform_cb(void *ctx, void *user, int frame, double et, double *xform, int *relative) noexcept with gil:
    cdef int i, j
    if ctx != NULL and pepper_failed(ctx):
        return 0
    try:
        r = (<object>user)(_context_object(ctx), frame, et)
        if r is None:
            return 0
        x, rel = r
        a = np.asarray(x, dtype=np.float64)
        if a.shape != (6, 6):
            raise ValueError(f'a frame provider returned a transformation of shape {a.shape} (6x6 expected)')
        for i in range(6):
            for j in range(6):
                xform[i + 6 * j] = a[i, j]
        if isinstance(rel, str):
            c = _context_object(ctx)
            relative[0] = c.namfrm(rel) if c is not None else 0
            if relative[0] == 0:
                raise ValueError(f'unknown frame {rel!r}')
        else:
            relative[0] = int(rel)
        return 1
    except BaseException as e:
        _callback_failed(ctx, e)
        return 1


# ----------------------------------------------------------------------
# Prepared queries

cdef class StateQuery:
    """A prepared state query (`Context.prepare_state`): `q(et)` gives
    `(state, lt)`; `q(ets)` gives `(states (n, 6), lts (n,))`, one call for
    all the epochs. A query belongs to its context (one thread at a time)."""

    cdef void *_q
    cdef readonly Context context

    @staticmethod
    cdef StateQuery _make(Context ctx, void *q):
        cdef StateQuery s = StateQuery.__new__(StateQuery)
        s._q = q
        s.context = ctx
        return s

    def __dealloc__(self):
        if self._q != NULL:
            pepper_state_query_free(self._q)
            self._q = NULL

    def __call__(self, et):
        cdef Context c = self.context
        cdef cnp.ndarray ets, states, lts
        cdef double e, lt
        cdef cnp.ndarray state
        cdef int st, n
        c._enter()
        try:
            if _iterable(et) or (isinstance(et, np.ndarray) and (<cnp.ndarray>et).ndim > 0):
                ets = np.ascontiguousarray(et, dtype=np.float64).reshape(-1)
                n = <int>ets.shape[0]
                states = np.empty((n, 6))
                lts = np.empty(n)
                with nogil:     # a batch: worth releasing the GIL
                    st = pepper_query_state(c._ptr, self._q, n, <const double*>cnp.PyArray_DATA(ets),
                                            <double*>cnp.PyArray_DATA(states), <double*>cnp.PyArray_DATA(lts))
                if st:
                    c._raise()
                return states, lts
            e = et
            state = _empty(NPY_DOUBLE, 6, -1)
            st = pepper_query_state(c._ptr, self._q, 1, &e, <double*>cnp.PyArray_DATA(state), &lt)
            if st:
                c._raise()
            return state, lt
        finally:
            c._leave()


cdef class RotationQuery:
    """A prepared frame transformation query (`Context.prepare_rotation`):
    `q(et)` or `q.rotation(et)` gives PXFORM's 3x3 rotation; `q.transform(et)`
    SXFORM's 6x6 state transformation. With an array of epochs, an array of
    matrices (n, 3, 3) or (n, 6, 6)."""

    cdef void *_q
    cdef readonly Context context

    @staticmethod
    cdef RotationQuery _make(Context ctx, void *q):
        cdef RotationQuery s = RotationQuery.__new__(RotationQuery)
        s._q = q
        s.context = ctx
        return s

    def __dealloc__(self):
        if self._q != NULL:
            pepper_rotation_query_free(self._q)
            self._q = NULL

    cdef object _eval(self, object et, int k):
        cdef Context c = self.context
        cdef cnp.ndarray ets, out
        cdef double e
        cdef int st, n
        c._enter()
        try:
            if _iterable(et) or (isinstance(et, np.ndarray) and (<cnp.ndarray>et).ndim > 0):
                ets = np.ascontiguousarray(et, dtype=np.float64).reshape(-1)
                n = <int>ets.shape[0]
                # n column-major k x k matrices: the transpose of each in C order
                out = np.empty((n, k, k))
                with nogil:
                    if k == 3:
                        st = pepper_query_rotation(c._ptr, self._q, n, <const double*>cnp.PyArray_DATA(ets),
                                                   <double*>cnp.PyArray_DATA(out))
                    else:
                        st = pepper_query_transform(c._ptr, self._q, n, <const double*>cnp.PyArray_DATA(ets),
                                                    <double*>cnp.PyArray_DATA(out))
                if st:
                    c._raise()
                return out.transpose(0, 2, 1)
            e = et
            out = _empty(NPY_DOUBLE, k, k)
            if k == 3:
                st = pepper_query_rotation(c._ptr, self._q, 1, &e, <double*>cnp.PyArray_DATA(out))
            else:
                st = pepper_query_transform(c._ptr, self._q, 1, &e, <double*>cnp.PyArray_DATA(out))
            if st:
                c._raise()
            return out
        finally:
            c._leave()

    def __call__(self, et):
        return self._eval(et, 3)

    def rotation(self, et):
        """PXFORM's rotation at et (or at each of an array of epochs)."""
        return self._eval(et, 3)

    def transform(self, et):
        """SXFORM's state transformation at et (or at each epoch)."""
        return self._eval(et, 6)


# ----------------------------------------------------------------------
# The module-level functions

cdef Spice _default = Spice()


def default_instance():
    """The instance made at import (the current one unless `as_current` is
    in effect)."""
    return _default


cpdef Spice current_instance():
    """The instance the module-level functions use: the one of the innermost
    `Spice.as_current()` block, or the default one."""
    s = _current.get()
    return _default if s is None else <Spice>s


cpdef Context current_context():
    """The calling thread's context of the current instance."""
    return current_instance().context()


cdef inline Spice _instance():
    s = _current.get()
    return _default if s is None else <Spice>s


cdef inline Context _context():
    return <Context>_instance().context()


def furnsh(path, in_memory=None):
    """Load kernels into the current instance (see Spice.furnsh)."""
    _instance().furnsh(path, in_memory)


def write_kernel(name, file, ck_file=None, text_file=None):
    """Write a kernel built in memory of the current instance to SPICE files
    (see Spice.write_kernel)."""
    _instance().write_kernel(name, file, ck_file, text_file)


def load_kernel(name, builder, keep=False):
    """Load a KernelBuilder as a kernel into the current instance (see
    Spice.load_kernel)."""
    _instance().load_kernel(name, builder, keep)


def kclear():
    """Unload all kernels of the current instance (see Spice.kclear)."""
    _instance().kclear()


def pdpool(name, values):
    """Insert real values into the kernel pool (see Spice.pdpool)."""
    _instance().pdpool(name, values)


def pipool(name, ivals):
    """Insert integer values into the kernel pool (see Spice.pipool)."""
    _instance().pipool(name, ivals)


def pcpool(name, cvals):
    """Insert strings into the kernel pool (see Spice.pcpool)."""
    _instance().pcpool(name, cvals)


def lmpool(cvals):
    """Load kernel pool assignments from lines (see Spice.lmpool)."""
    _instance().lmpool(cvals)


include "_functions.pxi"
