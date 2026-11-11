PEPPER: Planetary Ephemerides, Positions, Pointing, Events & Rotations. A modern Fortran reimagining of SPICELIB.

## Overview

PEPPER provides tools for computing planetary ephemerides, positions, pointing, events, and rotations, leveraging modern Fortran features for high performance and maintainability.

## Features

- Compute planetary ephemerides with high precision.
- Determine positions and orientations of celestial bodies.
- Calculate pointing directions for observational instruments.
- Identify and analyze astronomical events.
- Handle rotations and transformations efficiently.

How PEPPER differs from SPICELIB (thread safety, the limits it removes, speed
comparisons): [pepper_vs_spicelib.md](pepper_vs_spicelib.md).

## SPICE documentation

PEPPER keeps SPICELIB's routines, their names, arguments and behavior
(`call spkezr(...)` becomes `call ctx%spkezr(...)`; see "Usage"), so NAIF's
documentation of the SPICE Toolkit for Fortran (N0067) applies. What
PEPPER changes is in [pepper_vs_spicelib.md](pepper_vs_spicelib.md).

- [NAIF](https://naif.jpl.nasa.gov/naif/), the Navigation and Ancillary
  Information Facility of NASA's Jet Propulsion Laboratory, which develops
  SPICE.
- [The SPICE Toolkit for Fortran](https://naif.jpl.nasa.gov/naif/toolkit_FORTRAN.html):
  the packages of SPICELIB N0067 that PEPPER started from.
- [Toolkit documentation](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/index.html),
  and the [SPICELIB routines](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/index.html)
  (each routine's header; PEPPER's FORD documentation, `pixi run docs`, has
  them too).
- [Required reading](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/index.html),
  the documents on each subsystem, among them:
  [kernels](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/kernel.html),
  [SPK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/spk.html) (ephemerides),
  [frames](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/frames.html),
  [time](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/time.html),
  [aberration corrections](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/abcorr.html),
  [NAIF IDs](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/naif_ids.html),
  [PCK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/pck.html),
  [CK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/ck.html),
  [SCLK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/sclk.html),
  [DSK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/dsk.html),
  [GF](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/gf.html) (geometry finder),
  [EK](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/ek.html),
  [cells](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/cells.html),
  [windows](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/windows.html),
  [error handling](https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/req/error.html)
  (PEPPER: per context; see "Usage").
- [Tutorials](https://naif.jpl.nasa.gov/naif/tutorials.html): NAIF's
  introductions to SPICE and each subsystem.
- [Data](https://naif.jpl.nasa.gov/naif/data.html) and the
  [generic kernels](https://naif.jpl.nasa.gov/pub/naif/generic_kernels/)
  (planetary ephemerides, leap seconds, planetary constants, frames).

## Building and testing

PEPPER builds with [fpm](https://fpm.fortran-lang.org); the development
environment (gfortran, fpm, Python, FORD) is managed with [pixi](https://pixi.sh):

```
pixi run build          # the library
pixi run test           # the unit tests (test/core)
pixi run tspice         # NAIF's test suite (the tspice package), in parallel
pixi run test-parallel  # queries from several OpenMP threads
pixi run check-threads  # no thread-unsafe constructs (also run by `pixi run test`)
pixi run bench-parallel # timings from 1, 2, 4 and 8 threads
pixi run docs           # FORD documentation in doc/
```

## Usage

All state belongs to an instance (`type(pepper_spice)`): loaded kernels, the
kernel pool, file tables, the names defined with `boddef`. The kernels are
changed through the instance: `spice%furnsh`, `unload`, `kclear`, `ldpool`,
`clpool`, the pool's setters (`pcpool`, `pdpool`, `pipool`, `lmpool`,
`dvpool`), `boddef` and the low-level loaders (`spklef`, `pcklof`, `cklpf`,
`eklef` and their unloaders). Their errors are reported through the
instance's own context (`spice%context()`; `spice%failed()`).

Each thread queries the instance through a context (`type(pepper_context)`,
from `spice%new_context()`); the procedures are type-bound procedures of the
context with SPICELIB's names and arguments, and errors are reported through
the context. A context finds the changes of the kernels on its next call. A
serial program can also query through the instance's own context.

```fortran
use pepper_kinds, only: wp
use pepper_instance, only: pepper_spice, pepper_context

type(pepper_spice), target :: spice
type(pepper_context) :: ctx
real(wp) :: et, state(6), lt

call spice%furnsh('kernels.tm')
if (spice%failed()) ...
ctx = spice%new_context()
call ctx%str2et('2026 NOV 8 12:00:00 UTC', et)
call ctx%spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH', state, lt)
if (ctx%failed()) ...
```

Load kernels first, then query from any number of threads, one context per
thread (see `test/core/test_parallel.f90`). The kernels must not be changed
while other threads use contexts of the instance (nothing checks this).

Binary kernels (SPK, CK, binary PCK) are read record by record as queries
need them, as in SPICELIB. They can instead be read whole into memory when
they are loaded: `call spice%furnsh('de440.bsp', in_memory=.true.)` (for a
meta-kernel: every binary kernel it lists), or `call
spice%set_in_memory(.true.)` for all later loads. Queries at scattered epochs
then never read the file (DE421 at random epochs: 9.6 → 0.22 µs per state),
at the cost of memory equal to the file sizes, once per instance
(`spice%kernel_memory()`).

gfortran (at least up to 16.2) keeps the length of a deferred-length
character function result (a `character(:), allocatable` result) in a static
variable at each call site, which threads share. The library therefore has no
such functions: they return strings of a length computed from their
arguments, or are subroutines with an allocatable argument
(`tools/check_threads.py` checks the sources; `pixi run check-threads-ir`
checks the generated code for any static variable). The same applies to
programs that call PEPPER from several threads.

Each context keeps up to 23 files open at a time, as SPICELIB does, and
reopens a file when it needs it again. A program that queries many kernels in
turn can raise this before creating its contexts, for example
`call spice%set_max_open_files(500)`; each context may then use that many file
descriptors, within the operating system's limit (`ulimit -n`).

The library has no SPICELIB-style interface. One lives in the `tspice`
package (`tspice/src/legacy/`), where NAIF's test suite uses it: SPICELIB's
routines as external procedures (`call spkezr(...)`). They are thin wrappers
that call the library with a default instance and context, whose
error handling starts with SPICELIB's settings (`erract`, `errprt` and
`errdev` work as in SPICELIB).

Bugs and inconsistencies found in SPICELIB N0067 are listed in
`spicelib_bugs.md`; the design and its history are in `plan.md`.

## Prepared queries

A state or frame query can be resolved once (names, aberration correction,
frame) and then evaluated at any number of epochs, one at a time or as a
batch. The results are SPKEZR's, PXFORM's and SXFORM's, to the last bit, and
states are 1.5 to 4 times faster (`pepper_vs_spicelib.md`, "Prepared
queries"):

```fortran
use pepper_instance, only: state_query, rotation_query

type(state_query) :: q
type(rotation_query) :: r
real(wp) :: state(6), lt, states(6, 1000), lts(1000), ets(1000), rot(3, 3)

call ctx%prepare_state(q, 'MOON', 'EARTH', 'J2000', 'LT+S')
call ctx%query_state(q, et, state, lt)         ! one epoch
call ctx%query_state(q, ets, states, lts)      ! a batch (lts is optional)

call ctx%prepare_rotation(r, 'J2000', 'ITRF93')
call ctx%query_rotation(r, et, rot)            ! PXFORM; query_transform: SXFORM
```

A query keeps the segments it used and their current records, so a thread
uses its own queries (as its own context).

## Programmatic ephemerides and frames

Bodies and frames can come from the program instead of kernel files: a
provider computes the states or the orientation, and the body or frame is
then used everywhere one from a kernel is (states with any correction,
frame transformations, GF searches, prepared queries). See
`programmatic_plan.md`.

```fortran
use pepper_instance, only: ephemeris_provider, frame_provider, pepper_context

type, extends(ephemeris_provider) :: my_orbit     ! the program's data and code
   real(wp) :: r0(3), v(3)
contains
   procedure :: state => my_orbit_state           ! (self, ctx, body, et, state, found)
end type

call spice%add_ephemeris(-999, 399, 'J2000', t0, t1, my_orbit(r0, v))
call ctx%spkezr('-999', et, 'J2000', 'LT+S', 'MOON', state, lt)

call spice%add_frame('MY_FRAME', 1400000, 399, my_frame_provider)  ! no frame kernel
call ctx%pxform('MY_FRAME', 'J2000', et, rot)
```

- A provider's `state` gives the body's state relative to its registered
  center in the registered frame. A frame provider's `transform` gives the
  state transformation from the frame to the frame it is relative to (any
  frame, programmatic ones included), as a TK frame's rotation does.
- `ephemeris_procedure` and `frame_procedure` wrap plain procedures.
- A programmatic body takes part in the segment search as a file loaded when
  it was added: it overrides files loaded before and is overridden by files
  loaded after, within its coverage.
- `remove_ephemeris` and `remove_frame` remove one; `kclear` removes all
  (`clpool` keeps the frames, as it keeps the loaded files).
- Providers are called from any context, so from several threads at once:
  they must not change shared data.
- A provider may make queries through the context it is given (states,
  frame transformations, names, kernel variables), which may call other
  providers: a frame defined by bodies' states (a two-body frame), a body
  defined from other bodies. It must not change the kernels or run a GF
  search. A provider whose result depends on itself (more than 50 nested
  provider calls) gives SPICE(RECURSIONTOODEEP). A provider procedure that
  can be nested (its queries reach bodies or frames of the same provider
  type) must be `recursive`.

## Results of any size

SPICELIB makes the caller size every output array, and fails (or truncates)
when the result is larger. For these routines PEPPER has variants that return
allocatable results of the size of the data. The SPICELIB forms stay, with
their arguments unchanged.

| SPICELIB | PEPPER |
|---|---|
| `gdpool`, `gipool`, `gcpool` | `pool_values(name, values, found)`, `pool_integers`, `pool_strings` |
| `bodvrd`, `bodvcd` | `body_values(body, item, values)` (body by name or code) |
| `getfov`, `getfvn` | `fov(inst, shape, frame, bsight, bounds)` (instrument by code or name) |
| `bodc2n`, `frmnam`, `srfc2s` | `body_name(code, name, found)`, `frame_name(code, name, found)`, `surface_name(code, body, name, found)` |
| `kdata`, `kinfo` | `kernel_data(which, kind, file, filtyp, source, handle, found)`, `kernel_info(file, filtyp, source, handle, found)` |
| `spkcov`, `ckcov`, `pckcov` | `spk_coverage(file, body, intervals)`, `ck_coverage(file, inst, needav, level, tol, timsys, intervals)`, `pck_coverage(file, frame, intervals)` |
| `spkobj`, `ckobj`, `pckfrm`, `dskobj`, `dsksrf` | `spk_objects(file, ids)`, `ck_objects`, `pck_frames`, `dsk_objects`, `dsk_surfaces(file, body, ids)` |
| `dskp02`, `dskv02` | `dsk_plates(handle, dladsc, plates)`, `dsk_vertices(handle, dladsc, vertices)` |
| `dafec`, `dasec` | `daf_comments(handle, lines)`, `das_comments(handle, lines)` |
| `ekrced`, `ekrcei`, `ekrcec` | `ek_values(handle, segno, recno, column, values, isnull)` (real, integer or string values) |
| `limbpt`, `termpt` | `limb_points(...)`, `terminator_points(...)`: the inputs without `maxn` |
| `gfdist`, `gfposc`, `gfsubc`, `gfsntc`, `gfsep`, `gfoclt`, `gfilum`, `gfpa`, `gfrr`, `gftfov`, `gfrfov`, `gfuds`, `gfudb`, `gfevnt` | `gf_distance`, `gf_position`, `gf_subpoint`, `gf_intercept`, `gf_separation`, `gf_occultation`, `gf_illumination`, `gf_phase`, `gf_range_rate`, `gf_target_in_fov`, `gf_ray_in_fov`, `gf_user_scalar`, `gf_user_boolean`, `gf_event`: the inputs without `mw`, `nw` and `work` |

Strings are `character(:), allocatable`, lists of strings `type(string)`
arrays (`pepper_containers`). A window (a coverage, a GF result or
confinement window) is an array `intervals(2, n)`, each column an interval
[start, stop], sorted and disjoint; a set of IDs is a sorted integer array.
`pepper_windows` converts them to and from SPICELIB's cells
(`window_to_cell`, `cell_to_window`, `set_to_cell`, `cell_to_set`) for the
cell, set and window routines. After an error the results are empty (size 0).

```fortran
real(wp), allocatable :: radii(:), cover(:,:), result(:,:)

call ctx%body_values('EARTH', 'RADII', radii)
call ctx%spk_coverage('de440.bsp', 301, cover)
call ctx%gf_distance('MOON', 'NONE', 'EARTH', '<', 4.0e5_wp, 0.0_wp, 3600.0_wp, cover, result)
do i = 1, size(result, 2)
   print *, result(:, i)
end do
```
