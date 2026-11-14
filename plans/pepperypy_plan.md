# PepperyPy: a Python interface to PEPPER

PepperyPy is to PEPPER what [SpiceyPy](https://github.com/AndrewAnnex/SpiceyPy)
is to CSPICE: SpiceyPy's Python API (names, arguments, results, exceptions,
docstrings), with PEPPER underneath instead of CSPICE. What PEPPER adds shows
through: instances and contexts (thread-safe queries, several kernel sets in
one process), prepared and batch queries, results of any size, programmatic
ephemerides and frames.

Status: implemented (phases 1 to 5; phase 6, wheels, not yet). See
"Implementation" at the end for what differs from the design below.

## SpiceyPy as it is (8.x)

- **Binding:** `ctypes` over CSPICE (`libcspice`, loaded by
  `utils/libspicehelper.py`: `CSPICE_SHARED_LIB`, `find_library`, or the copy
  in the wheel), `argtypes`/`restype` set per `*_c` function. Each wrapper in
  `spiceypy.py` converts its arguments by hand (`stypes.string_to_char_p`,
  `to_double_vector`, `empty_double_vector`, ...) and copies the results
  into NumPy arrays.
- **Errors:** at import, `erract("set", 10, "return")` and
  `errdev("set", 10, "null")`; the `spice_error_check` decorator calls
  `failed_c` after every call, reads the messages, calls `reset_c` and raises
  a `SpiceyError` subclass chosen from the short message
  (`SpiceNOSUCHFILE(SpiceyPyIOError)`, ... several hundred, in
  `utils/exceptions.py`). `spice_found_exception_thrower` turns `found =
  False` into `NotFoundError` (`config.catch_false_founds`, `found_check()`,
  `no_found_check()`).
- **Cells:** `SpiceCell` mirrors CSPICE's C struct (`utils/support_types.py`;
  that code derives from spiceminer, MIT, Philipp Rasch 2013).
- **Callbacks:** GF user functions as `CFUNCTYPE` objects (`utils/callbacks.py`:
  `SpiceUDFUNS`, `SpiceUDFUNB`, ...).
- **Vectorization:** a few wrappers (`spkezr`, `spkpos`, `et2utc`, `convrt`,
  ...) accept a list of epochs and loop in Python; `cyice` (Cython, about 90
  functions) has `_v` variants that loop in C.
- **State:** CSPICE's, global to the process: one kernel pool, one error
  status. Not thread-safe; nothing in SpiceyPy says otherwise.
- **Build:** scikit-build-core + CMake builds CSPICE (from a CMake port) into
  the wheel; cibuildwheel for CPython 3.10–3.15 on Linux, macOS, Windows;
  Pyodide.
- **License:** MIT, `Copyright (c) [2015-2024] [Andrew Annex]` (the source
  headers say 2015-2025).

## Architecture

```
Python      pepperypy  (SpiceyPy's API; Spice and Context classes)
              │ Cython extension modules (cdef extern from "pepper.h")
C ABI       pepper_capi  (bind(c) shims: handles, C strings, error codes)
              │
Fortran     PEPPER  (pepper_instance, pepper_context)
```

### The C interface (`capi/`, in this repository)

PEPPER has no C interface yet. It belongs in this repository, not in
PepperyPy: C, C++, Julia, Rust or MATLAB programs would use the same one.

- **Handles:** an instance and a context are opaque pointers. The instance
  is allocated in Fortran (`allocate(s)`, `c_loc(s)`: a nonpolymorphic
  target, F2008), so the contexts' pointers to it stay valid.

  ```c
  void *pepper_spice_new(void);
  void  pepper_spice_free(void *spice);       /* after its contexts */
  void *pepper_context_new(void *spice);
  void  pepper_context_free(void *ctx);
  ```

- **CSPICE's signatures, with the context first:** each query is a shim with
  CSPICE's arguments (NUL-terminated input strings, `lenout` and a buffer for
  output strings, arrays as pointers) and a context (or, for the kernel
  changes, an instance) before them; it returns 1 if an error was signaled.

  ```c
  int pepper_spkezr(void *ctx, const char *targ, double et, const char *ref,
                    const char *abcorr, const char *obs,
                    double starg[6], double *lt);
  int pepper_furnsh(void *spice, const char *file);
  ```

  The Cython wrappers call them directly (`pepper_spkezr(self.ptr, ...)`)
  and check the returned flag in C, instead of calling `failed_c` after
  every call as SpiceyPy does.

  ```fortran
  integer(c_int) function pepper_spkezr ( ctx, targ, et, ref, abcorr, obs, starg, lt ) &
        bind(c, name='pepper_spkezr')
     type(c_ptr), value :: ctx
     character(kind=c_char), intent(in) :: targ(*), ref(*), abcorr(*), obs(*)
     real(c_double), value :: et
     real(c_double), intent(out) :: starg(6), lt
     type(capi_context), pointer :: c
     call c_f_pointer(ctx, c)
     call c%spkezr(f_string(targ), et, f_string(ref), f_string(abcorr), f_string(obs), starg, lt)
     pepper_spkezr = merge(1_c_int, 0_c_int, c%failed())
  end function
  ```

- **The context of the C interface:** `type, extends(pepper_context) ::
  capi_context` adds what the shims need per thread: the result buffers, and
  the callback slots of a GF search (below). Created as
  `c%pepper_context = spice%new_context()`.
- **Results of any size** (`spk_coverage`, `gf_distance`, `pool_values`,
  `body_values`, ...; see "Results of any size" in the README): the shim
  keeps the allocatable result in the context's buffer and returns its
  shape. `pepper_take_reals(ctx, buf, n)` (and `_ints`, `_strings`) copies
  it out. The buffer is the context's, so this is thread-safe. No `maxn`,
  no work space, no `SpiceCell` in the C interface; windows are `(n, 2)`
  arrays.
- **Batches:** shims for the prepared queries and for the routines that
  Python would call in a loop take arrays of epochs. The loop runs in
  Fortran, behind one call: `pepper_query_state_batch(ctx, q, n, ets,
  states, lts)`, `pepper_spkezr_batch`, `pepper_pxform_batch`,
  `pepper_str2et_batch`, `pepper_et2utc_batch`.
- **Errors:** `pepper_getmsg(ctx, which, lenout, msg)` and
  `pepper_reset(ctx)`. A context's error action is RETURN from the start
  (PEPPER's default), so nothing is set at import.
- **Generated:** the shims come from a generator (`tools/gen_capi.py`), not
  by hand. It reads the procedures' interfaces in `pepper_instance_module.f90`
  (names, intents, kinds, `!!` docs), and a table of the C signatures for
  what Fortran does not say: the sizes of assumed-size arguments, output
  string lengths. A hand-written shim is the exception (callbacks,
  handles, buffers). PEPPER binds about 940 procedures; the generator also
  writes `capi/pepper.h`.
- **Build:** fpm, as an additional library target. fpm's static library is
  enough for C programs and is what PepperyPy's extension modules link
  (below); a shared one (`libpepper_capi.{so,dylib,dll}`) can be built for
  other languages.

### Python (`python/`, in this repository)

A `python/` directory here rather than a separate repository first, while the
C interface changes often. It can be split off later.

```
python/
  pyproject.toml
  LICENSE                 PepperyPy (MIT) + SpiceyPy's and spiceminer's
                          notices + PEPPER's LICENSE (NAIF disclaimer, rules)
  src/pepperypy/
    __init__.py           the SpiceyPy-compatible functions (module level)
    pepper.pxd            cdef extern from "pepper.h" (generated)
    _core.pyx             Spice, Context, StateQuery, RotationQuery (cdef classes)
    _wrappers.pxi         the wrappers, as methods of Context/Spice (generated;
                          from SpiceyPy's signatures and docstrings)
    _callbacks.pyx        GF and provider callbacks (cdef functions, `with gil`)
    exceptions.py         SpiceyPy's (unchanged)
    found_catcher.py      SpiceyPy's (unchanged)
    support_types.py      SpiceyPy's SpiceCell (kept for compatibility)
    config.py
    meson.build
    tests/
```

From SpiceyPy, kept: the API (names, arguments, return values),
docstrings (with their NAIF links), `exceptions.py`, `found_catcher.py`,
`config.py`, the conversion helpers, `SpiceCell`, the tests and their
kernels (`gettestkernels.py`). From `cyice` (SpiceyPy's Cython module),
its patterns: string conversion, typed memoryviews, `nogil` loops, scalar
and vectorized forms behind one function. Replaced: ctypes entirely
(`libspicehelper.py`, `argtypes`, `CFUNCTYPE`; PepperyPy does not use
ctypes), the bodies of the wrappers, the error check, the build (no
CSPICE).

### Why Cython

- **Overhead:** a call costs about 0.1 to 0.3 µs in Cython, against about
  1 µs for a ctypes call and 3 to 5 µs for a SpiceyPy wrapper (argument
  conversion in Python, ctypes arrays, copies into NumPy). PEPPER's query
  from memory takes about 0.2 µs, so ctypes would make the binding cost
  ten times the query. (Estimates, measured in phase 2.)
- **The GIL:** `with nogil:` around each call into PEPPER, so threads
  query in parallel; free-threaded Python is supported (Cython 3.1+).
- **Arrays:** typed memoryviews take NumPy arrays without copies; loops
  over epochs run in C.
- **Errors:** the flag is checked in C; Python objects (the exception) are
  made only when an error was signaled.
- **Not f2py:** f2py cannot wrap derived types or type-bound procedures
  (all of PEPPER's API), handles strings poorly and has a weak callback
  model; it would need the `bind(c)` layer anyway.
- **Not nanobind:** as fast, but C++ and a style unlike SpiceyPy's
  (Cython carries over `cyice`'s code).

## Contexts in Python

PEPPER's rule is: one instance holds the kernels; each thread queries
through its own context; the kernels change only while no thread queries.
PepperyPy keeps that model and makes it invisible to code written for
SpiceyPy.

### Two levels

**1. SpiceyPy's functions (module level).** Existing code runs unchanged:

```python
import pepperypy as spice

spice.furnsh('kernels.tm')
et = spice.str2et('2026 NOV 8 12:00:00')
state, lt = spice.spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH')
```

- The kernel changes (`furnsh`, `unload`, `kclear`, `ldpool`, `pdpool`,
  `boddef`, ...) go to the **current instance**: a process-wide default
  `Spice()`, created at import.
- The queries go to the **calling thread's context** of the current
  instance. It is created on the thread's first query and kept in a
  `threading.local` map (by instance; a `weakref` so that it goes when the
  instance does).
- So SpiceyPy code that was single-threaded is unchanged, and code that
  queries from several threads becomes correct. With SpiceyPy (CSPICE's
  global state) it was not.

**2. Explicit objects.** These are for programs that manage threads, or need
several kernel sets.

```python
from pepperypy import Spice

de440 = Spice()
de440.furnsh('de440.tm', in_memory=True)

def work(ets):                       # in a worker thread
    ctx = de440.context()            # this thread's context
    q = ctx.prepare_state('MOON', 'EARTH', 'J2000', 'LT+S')
    return q(ets)                    # (n, 6) states, (n,) light times; one call

with ThreadPoolExecutor(8) as pool:
    results = list(pool.map(work, np.array_split(ets, 8)))
```

- `Spice`: kernel changes (methods named as SpiceyPy's functions),
  `add_ephemeris`, `add_frame`, `set_in_memory`, `kernel_memory`,
  `set_max_open_files`; `Spice.context()` returns the calling thread's
  context (the same one the module-level functions use); `Spice.new_context()`
  returns a new one that the caller owns.
- `Context`: every query, with SpiceyPy's names, arguments and results, as
  methods: `ctx.spkezr(...)`, `ctx.pxform(...)`, `ctx.gfdist(...)`.
  Plus PEPPER's own: `prepare_state`, `prepare_rotation`,
  `spk_coverage`, `body_values`, ...
- **Several instances:** independent kernel sets in one process, such as
  comparing DE440 with DE441, or a mission's reconstructed kernels with its
  predicted ones. SpiceyPy cannot do this (only `KernelPool`'s swap).
  `with de441.as_current(): ...` makes the module-level functions use
  another instance within a block. It is a `contextvars.ContextVar`, so it
  follows asyncio tasks and threads.

### The rules, enforced where it is cheap

- **A context is used by one thread at a time.** Each `Context` has a
  `threading.Lock` that a call acquires without waiting. If the lock is
  held, another thread is inside the same context, and the call raises
  `ContextInUseError` instead of corrupting it. Handing a context from one
  thread to another between calls is allowed. The flag is a C atomic in
  the `cdef class` (a compare-and-swap, a few nanoseconds), not a Python
  lock.
- **Prepared queries belong to their context's thread** (they keep segment
  records): a `StateQuery` holds its `Context` and takes the same lock.
- **Kernel changes while other threads query:** not detected, as in PEPPER
  (no library locks). A per-instance readers-writer lock would cost every
  query; it could be a debug option (`config.check_kernel_changes`).
  Documented: load first, then query.
- **Lifetimes:** a `Context` holds a reference to its `Spice`, so an
  instance is freed (`pepper_spice_free`) only after its contexts.
  `Spice.close()` and `Context.close()` free them explicitly; `__del__`
  does it otherwise.
- **Processes:** `Spice` is not picklable (its state is native memory).
  With `multiprocessing` or `ProcessPoolExecutor`, each worker loads its
  kernels in an `initializer`. Threads are the better choice with PEPPER:
  one copy of the kernels.

### Threads really run in parallel

The extension modules release the GIL (`with nogil:`) for the duration of
each call into PEPPER, so queries from several threads run at the same time
in Fortran. Free-threaded Python
(3.13t, 3.14t) removes the GIL completely. With SpiceyPy, threads would
corrupt CSPICE's state. With PepperyPy, a `ThreadPoolExecutor` scales with
the cores for work dominated by queries, especially batches.

### Errors are per context

Each context has its own error status, so an exception is raised in the
thread that caused it, and other threads are not affected. The wrapper
checks the shim's return flag, reads the messages from that context,
resets it, and raises SpiceyPy's exception class for the short message.
The exceptions are unchanged, except that the traceback is empty because
PEPPER keeps none. `erract`, `errdev` and `errprt` work per context (on the
calling thread's context, at module level).

`config.catch_false_founds` stays process-wide, as in SpiceyPy;
`found_check()` and `no_found_check()` become per-thread, through a
`ContextVar`.

## Callbacks: GF user functions and programmatic providers

There is one trampoline per kind of callback: a `cdef ... noexcept nogil`
function in `_callbacks.pyx` whose address is passed to the C interface,
and which takes the GIL (`with gil:`) to call Python. A callback carries an
integer key. The trampoline looks the key up in a registry of Python
callables, and the registry keeps the callables alive.

- **GF user functions** (`gfuds`, `gfudb`, `gf_user_scalar`, ...): PEPPER's
  `gf_scalar` interface is `(ctx, et, value)`, with no user data. The
  context is a `capi_context`, though, so the Fortran side of the trampoline
  finds the search's callback key in it (`select type`). One GF search runs
  per context at a time, so one slot per context is enough. The Python
  function receives `et`. A form that also receives the context would let
  it make queries.
- **Providers** (`Spice.add_ephemeris`, `Spice.add_frame`): the C
  interface's provider is `type, extends(ephemeris_provider) ::
  capi_ephemeris_provider` (and the same for frames), which holds the
  trampoline and the key. The Python side is a callable:

  ```python
  def orbit(ctx, body, et):            # ctx: the querying thread's Context
      return state_or_None             # None: no data at et (SPICE(NOEPHEMERISDATA))

  spice.add_ephemeris(-999, 399, 'J2000', t0, t1, orbit)
  spice.add_frame('MY_FRAME', 1400000, 399, transform)   # (ctx, frame, et) -> (xform 6x6, relative)
  ```

  The `ctx` passed in is a non-owning `Context` for the Fortran context of
  the query, so a provider can make queries, as PEPPER allows.
- **Exceptions in callbacks:** an exception raised by a callback is stored
  per context. The context signals SPICE(PYTHONCALLBACK), and the outer
  call re-raises the original exception, chained to the SPICE error.
- **Cost:** each call takes the GIL and calls Python (about 1 µs), which
  serializes threads.
  This is fine for a GF search, but slow for a provider called millions of
  times. So that the common case needs no Python callback, PEPPER gets a
  built-in **table provider** in Fortran (offered earlier, not built yet):
  `Spice.add_ephemeris_table(body, center, frame, ets, states)` does
  Hermite interpolation of a state history held in NumPy arrays. A frame
  equivalent takes quaternions with angular velocities.
- Providers and their keys are released by `remove_ephemeris`,
  `remove_frame` and `kclear`.

## Vectorization and NumPy

- Wrappers return NumPy arrays (as SpiceyPy does) and accept array-likes.
- Where SpiceyPy accepts a sequence of epochs (`spkezr`, `spkpos`,
  `et2utc`, `str2et`, `pxform`, ...), PepperyPy passes the whole array to a
  batch shim: one call, with the loop in Fortran. Results have the
  same shapes as SpiceyPy's.
- Prepared queries: `ctx.prepare_state(target, observer, frame, abcorr)`
  returns a `StateQuery`. Called with a scalar `et` it returns `(state, lt)`;
  called with an array it returns `(states (n, 6), lts (n,))`. NumPy arrays
  are passed without copies (C-contiguous float64, transposed on the Fortran
  side, or `order='F'` buffers). `RotationQuery` works the same way, with
  `rotation` and `transform`.
- Windows (coverage, GF results): `(n, 2)` arrays by default.
  `SpiceCell` arguments and results stay accepted where SpiceyPy takes
  them, converted at the boundary.
- A rough expectation, to be measured: a prepared batch costs about 0.1 to
  0.5 µs per epoch (PEPPER's own numbers), a Cython call per epoch 0.1 to
  0.3 µs more, while a SpiceyPy loop costs 3 to 5 µs per epoch in Python
  and ctypes overhead.

## Compatibility with SpiceyPy

- **The same:** the names, arguments, return values, exceptions and found
  handling. The goal is SpiceyPy's test suite passing against PepperyPy
  with `import pepperypy as spiceypy`. Running that suite also compares
  PEPPER with CSPICE through a second, independent test suite.
- **Different:**
  - `tkvrsn` reports PEPPER's version.
  - Tracebacks are empty (`trcoff` and `qcktrc` do nothing useful).
  - Error settings are per thread.
  - `KernelPool` swaps the kernels of the current instance.
  - Limits that PEPPER removed (string lengths, the sizes of output cells)
    are gone; arguments such as `lenout` and `size` are accepted and
    ignored.
- **Missing:** CSPICE functions that have no SPICELIB counterpart, such as
  the C-only `*_c` helpers. Each one gets either a Python implementation or
  a `NotImplementedError`, decided from a coverage table that the generator
  writes (SpiceyPy's names against PEPPER's procedures).

## Build and distribution

- **Wheels:** meson-python. Meson builds Fortran well and SciPy uses it
  with gfortran and Cython together. It builds PEPPER and `capi/` from
  this repository, with `-frecursive`, into a static library, then the
  Cython extension modules linked to it. fpm remains the developer build
  of PEPPER; a meson build file is the price of wheels.
- **cibuildwheel:** Linux (manylinux, gfortran), macOS (arm64, x86_64,
  gfortran from Homebrew or conda-forge), Windows (MinGW-w64 gfortran, or
  ifx). The Fortran runtime (`libgfortran`, `libquadmath`) is bundled by
  auditwheel, delocate or delvewheel.
- **Wheels per Python version:** the extension modules are built for each
  CPython version (3.10+) and for the free-threaded builds, as SpiceyPy's
  `cyice` is. Cython's support of the stable ABI (abi3) could reduce this
  later.
- **PEPPER linked statically** into the extension modules (the fpm or
  meson static library), so a wheel contains no separate PEPPER library;
  only the Fortran runtime is bundled.
- **conda-forge:** a recipe once the wheels work. The pixi environment here
  can build it from the start.
- **Pyodide:** out of scope (no reliable Fortran-to-WebAssembly toolchain).
- **Development:** `pip install -e . --no-build-isolation` with
  meson-python rebuilds the extension modules on import after a change
  (Fortran, C interface or `.pyx`).

## License

- PepperyPy: MIT, Copyright (c) 2026 Jacob Williams.
- The code taken from SpiceyPy keeps its notice. `python/LICENSE` includes
  SpiceyPy's LICENSE verbatim (`Copyright (c) [2015-2024] [Andrew Annex]`),
  and the files derived from it keep their headers (`Copyright (c)
  [2015-2025] [Andrew Annex]`). `support_types.py` keeps the spiceminer
  notice (`Copyright (c) 2013 Philipp Rasch`).
- The wheels contain PEPPER, which is derived from SPICELIB, so
  `python/LICENSE` includes PEPPER's LICENSE: NAIF's disclaimer and the
  rules. Under the rules ("Toolkit Redistribution"), shipping SPICE modules
  as part of a package supporting a SPICE-based tool is allowed; PEPPER is
  modified code, and NAIF does not support it.
- The README says that PepperyPy is not SpiceyPy, and that SpiceyPy's
  maintainers do not support it.

## Tests

- SpiceyPy's suite (`test_wrapper.py` and the others), with its kernels
  (`gettestkernels.py`, from SpiceyPyTestKernels). It is the compatibility
  target.
- **Threads:** queries from a `ThreadPoolExecutor` match the serial results
  to the last bit, as in `test_parallel`. Exceptions reach the right
  thread. `ContextInUseError` is raised. Several instances hold different
  kernels.
- **Callbacks:** providers (with a query inside), GF user functions,
  exceptions in callbacks, and `kclear` releasing them.
- **Batches:** batch and prepared results equal the one-epoch results.
- **Benchmarks:** against SpiceyPy (one at a time, a loop, a batch, threads)
  for the README's table.

## Phases

1. **C interface core:** handles, strings, errors, result buffers, and the
   generator, covering time, SPK, frames, bodies, the pool and kernel
   loading (about 60 functions). `capi/pepper.h`. A C test program.
2. **PepperyPy core:** `Spice` and `Context`, the module-level functions
   (current instance, per-thread contexts), SpiceyPy's exceptions and
   found handling, and the Cython wrappers for phase 1. Builds with
   meson-python in the pixi environment. Measure the cost of a call
   (`spkezr`, `pxform`, from memory) against PEPPER's own and against
   SpiceyPy's, to confirm the estimates in "Why Cython".
3. **Batches and prepared queries:** NumPy throughout.
4. **The rest of the API**, generated: GF (with callbacks), coverage, DSK,
   CK, SCLK, EK, writers, cells. SpiceyPy's suite passing.
5. **Providers:** Python callbacks, and the Fortran table provider.
6. **Wheels:** cibuildwheel, PyPI; documentation.

## Decisions

- **Binding:** Cython extension modules over the C interface. No ctypes
  anywhere in PepperyPy (see "Why Cython").
- **Location:** `capi/` and `python/` in this repository, not a separate
  `pepperypy` repository (it can be split off later, once the C interface
  is stable).
- **Module-level functions:** each thread gets its own context of the
  current instance, made automatically on its first query (not one shared
  context, which would give up the threading).
- **The `ContextInUseError` check:** always on (not only in a debug mode).
- **Name:** PepperyPy as the project, `pepperypy` as the import name.

## Implementation

What was built, and where it differs from the design above.

- **The C interface** is in `src/capi` (`pepper_capi_core.f90`, by hand:
  handles, strings, buffers, errors, GF callbacks, providers, prepared
  queries; `pepper_capi.f90`, generated), so fpm builds and checks it with
  the library; the header is `include/pepper.h`; `capi/test_capi.c` is its C
  test (`pixi run -e python test-capi`).
- **The generator** (`tools/gen_capi.py`) reads the interfaces themselves
  (`pepper_instance_module.f90`; the pure procedures of `pepper_math`,
  `pepper_strings`, `pepper_arrays`; the constants) rather than a table of
  signatures: 831 procedures (618 of contexts, 16 of instances, 197
  pure), with the kinds of arguments it knows (scalars, strings, fixed and
  argument-sized arrays, assumed-size inputs, arrays of strings, cells,
  results of any size). Tables cover what Fortran does not say: sizes of
  assumed-size results, CSPICE's 0-based indices, found flags SpiceyPy
  returns, arrays CSPICE writes as rows. The Python parameters follow
  SpiceyPy's (read from the installed SpiceyPy): by name, then by position;
  SpiceyPy's size parameters are accepted and ignored; dimensions it omits
  are taken from the arrays.
- **The build** is at the repository's root (`pyproject.toml`,
  `meson.build`), not in `python/`: meson needs PEPPER's sources in its
  source tree. PEPPER is a static library linked into one extension module
  (`pepperypy._core`); the Python sources are in `python/src/pepperypy`.
- **Classes:** Cython cannot include methods into a class body, so the
  generated methods are base classes: `_ContextBase` → `_ContextRoutines`
  (generated) → `Context`, and the same for `Spice`.
- **The GIL** is released only around long calls (GF, geometry, DSK, EK,
  files, coverage, batches): around a short call, releasing and taking
  back the GIL cost more than the call when threads contend for it, and
  one-call-per-epoch loops got slower with more threads (1.2 µs per call
  on 1 thread, 5.3 µs on 8). Batches scale: 0.62 µs per epoch on 1 thread,
  0.16 µs on 8.
- **Vectorized calls** loop in Cython over the scalar call (no batch shims
  but the prepared queries'): the loop costs less than the Python around
  it.
- **The gfortran runtime:** a program whose main is not Fortran must set
  gfortran's runtime options as gfortran's main does, or a file cannot be
  opened on a second unit (IOSTAT 5004), which every second context does:
  `pepper_runtime_init()` (`pepper.h`), called at import.
- **PEPPER fixes found by the tests:** loading a file again after
  UNLOAD/KCLEAR while a context that had read it was not used since
  (SPICE(EXTERNALOPEN)), and loading into a second instance a file the
  first had open: GETFAT and ZZDDHOPN accept, for reading, a file open on
  PEPPER's units (`opened_by_pepper`; `test/core/test_context.f90`).
- **Tests:** `python/tests` (144): ~130 calls compared with SpiceyPy
  (CSPICE) to 1e-12, errors and found flags, GF searches with cells, DAF
  reading, windows; threads (serial results to the last bit, per-thread
  errors, `ContextInUseError`), instances, `KernelPool`, prepared queries,
  results of any size, GF and provider callbacks (with exceptions).
  SpiceyPy's own suite (`py-test-spiceypy`): 524 of 638 pass.
- **Not done:** wheels (phase 6); the Fortran table provider (phase 5's
  second half); SpiceyPy's functions on its ctypes structures (planes,
  ellipses, DLA and DSK descriptors, EK queries) — see `python/COVERAGE.md`.
