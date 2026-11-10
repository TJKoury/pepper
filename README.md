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