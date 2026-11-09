PEPPER: Planetary Ephemerides, Positions, Pointing, Events & Rotations. A modern Fortran reimagining of SPICELIB.

## Overview

PEPPER provides tools for computing planetary ephemerides, positions, pointing, events, and rotations, leveraging modern Fortran features for high performance and maintainability.

## Features

- Compute planetary ephemerides with high precision.
- Determine positions and orientations of celestial bodies.
- Calculate pointing directions for observational instruments.
- Identify and analyze astronomical events.
- Handle rotations and transformations efficiently.

## Building and testing

PEPPER builds with [fpm](https://fpm.fortran-lang.org); the development
environment (gfortran, fpm, Python, FORD) is managed with [pixi](https://pixi.sh):

```
pixi run build          # the library
pixi run test           # the unit tests (test/core)
pixi run tspice         # NAIF's test suite (the tspice package), in parallel
pixi run test-parallel  # queries from several OpenMP threads
pixi run bench-parallel # timings from 1, 2, 4 and 8 threads
pixi run docs           # FORD documentation in doc/
```

## Usage

All state belongs to an instance (`type(pepper_spice)`): loaded kernels, the
kernel pool, file tables. Each thread uses a context (`type(pepper_context)`)
of the instance; the procedures are type-bound procedures of the context with
SPICELIB's names and arguments, and errors are reported through the context.

```fortran
use pepper_kinds, only: wp
use pepper_instance, only: pepper_spice, pepper_context

type(pepper_spice), target :: spice
type(pepper_context) :: ctx
real(wp) :: et, state(6), lt

ctx = spice%new_context()
call ctx%furnsh('kernels.tm')
call ctx%str2et('2026 NOV 8 12:00:00 UTC', et)
call ctx%spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH', state, lt)
if (ctx%failed()) ...
```

Load kernels first, then query from any number of threads, one context per
thread (see `test/core/test_parallel.f90`).

The library has no SPICELIB-style interface. One lives in the `tspice`
package (`tspice/src/legacy/`), where NAIF's test suite uses it: SPICELIB's
routines as external procedures (`call spkezr(...)`). They are thin wrappers
that call the library with a default instance and context, whose
error handling starts with SPICELIB's settings (`erract`, `errprt` and
`errdev` work as in SPICELIB).

Bugs and inconsistencies found in SPICELIB N0067 are listed in
`spicelib_bugs.md`; the design and its history are in `plan.md`.