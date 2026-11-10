# Phase 7: outputs sized by PEPPER (design)

Phase 7 of [`limits.md`](limits.md): routines whose outputs the caller must
size get variants that return allocatable results. This document proposes
the API and the order of the work; the decisions marked **Decide** need your
choice before any code.

## The problem

Fortran 77 has no dynamic memory, so SPICELIB makes the caller supply every
output array, with its size. The caller has to know the size of a result
before computing it:

| What the caller must guess | Example | When the guess is too small |
|---|---|---|
| The number of values of a kernel variable | `gdpool(name, start, room, n, values, found)` | Only `room` values are returned; the caller pages with `start` |
| The number of values of a body constant | `bodvrd(body, item, maxn, dim, values)` | SPICE(ARRAYTOOSMALL) |
| The number of FOV boundary vectors | `getfov(instid, room, shape, frame, bsight, n, bounds)` | SPICE(BOUNDARYTOOBIG) |
| The number of coverage intervals | `spkcov(file, idcode, cover)` (a window cell) | SPICE(WINDOWEXCESS) |
| The number of GF result intervals, and workspace | `gfposc(..., cnfine, mw, nw, work, result)` | SPICE(WINDOWEXCESS) when a window fills; SPICE(INVALIDDIMENSION), SPICE(TOOFEWWINDOWS) for MW, NW |
| The number of limb or terminator points | `limbpt(..., maxn, npts, points, epochs, tangts)` | SPICE(OUTOFROOM) |
| The number of plates, vertices, comment lines | `dskp02(handle, dladsc, start, room, n, plates)`, `dafec(handle, bufsiz, n, buffer, done)` | Paging with `start`, or `done` |
| The length of a name or file name | `bodc2n(code, name, found)`, `kdata(..., file, ...)` | Silently cut to the caller's variable |

Some routines have no size argument at all and write past the end of a
short array: BODVAR, and the EK column readers EKRCEC, EKRCED, EKRCEI.

PEPPER has removed its own limits (names, files, pool values of any length
and number), so these interfaces are now the place where a program still hits
a fixed size: its own.

## Goals

- Every result whose size depends on the data can be returned without the
  caller guessing a size: allocatable arrays, and strings of any length.
- The SPICELIB interfaces stay, unchanged: SPICELIB programs, the legacy
  interface and tspice use them.
- The new routines are thread-safe like the rest: subroutines with
  `allocatable, intent(out)` arguments, never functions with deferred-length
  character results (gfortran keeps their length in a static variable; see
  `tools/check_threads.py`).
- Only for outputs. The cell, set, window and symbol-table routines (the
  caller's own data structures), and the many routines whose outputs have a
  fixed size (state vectors, descriptors, records), are out of scope.

## Decisions

### D1. Names (**Decide**)

**A. New names** that say what they return, as PEPPER's existing
extensions do (`ctx%pool_string`, `ctx%pool_names`, `ctx%message`):
`ctx%pool_values('BODY399_RADII', radii, found)`,
`ctx%spk_coverage(file, body, intervals)`.

**B. SPICELIB's names, overloaded** with a shorter argument list:
`ctx%gdpool('BODY399_RADII', radii, found)`,
`ctx%spkcov(file, body, intervals)`. Fortran resolves a generic by the
number, types and ranks of the arguments, so this works where the argument
lists differ (`gdpool` has 6 arguments against 3, a window cell is rank 1
and an interval array rank 2). It cannot work for strings:
`bodc2n(code, name, found)` with an allocatable `name` has the same arguments
as the SPICELIB form, so those need new names anyway.

| | A. New names | B. Overloaded SPICELIB names |
|---|---|---|
| Discoverable from NAIF's documentation | Through a table in the docs | Yes, for the routines that can be overloaded |
| Consistent | One rule for all, as the existing extensions | Two rules: overloads, plus new names for strings |
| Clear that the arguments differ | Yes | The same name means two argument lists |
| Compiler errors on a wrong call | Plain | "No specific procedure for the generic" |
| Changes to existing bindings | None | Each overloaded binding becomes a generic over two renamed specifics |

**Recommendation: A**, with a table in the documentation from each SPICELIB
routine to its new form. The names below follow A.

### D2. Windows and sets (**Decide**)

**A. Plain arrays:** a window is `real(wp), allocatable :: intervals(:,:)`
of shape (2, n), each column a [start, stop] interval, sorted and disjoint;
a set of IDs is `integer(ip), allocatable :: ids(:)`, sorted. Programs use
them directly (`size(intervals, 2)`, loops), and convert them to cells only
to call the SPICELIB window routines.

**B. A window type** (`type(pepper_window)`) with methods (union,
intersection, contains, measure, ...), the window routines of SPICELIB
rewritten for it.

**Recommendation: A.** Plain arrays need no new library and are what a
modern Fortran program can use without learning one. The SPICELIB window
routines remain for the operations; two converters (`window_to_cell`,
`cell_to_window`) bridge them. A type could be added later over the same
arrays.

### D3. Strings

Strings of any length are `character(:), allocatable, intent(out)`
arguments, lists of strings `type(string), allocatable, intent(out) :: x(:)`
(the `string` type of `pepper_containers`, as `pool_names` has now).

### D4. How the variants are built

Two kinds:

- **Size first, then the SPICELIB routine.** Where the size is cheap to get
  in advance, the variant gets it, allocates, and calls the existing routine
  with exactly that room: kernel variables (DTPOOL), body constants, FOV
  boundaries (the pool), DSK plates and vertices (DSKZ02), EK column entries
  (their size, ZZEKESIZ), comment lines (counted first). No change to the existing code.
- **A bound, then the SPICELIB routine.** Where the size is only known after
  the computation, the variant allocates from an upper bound:
  - *Coverage* (SPKCOV, CKCOV, PCKCOV): no more intervals than the file has
    segments for the object (a count over the segments).
  - *GF searches:* the solver samples each interval of the confinement
    window at STEP and refines one transition where the state differs
    between two samples, so it finds at most one transition per step. The
    result then has at most `n_cnfine + measure(cnfine)/step` intervals,
    plus one per confinement interval. A year searched in steps of one hour:
    about 8800 intervals, 140 KB for the result. (To confirm in step 7e for
    each search, and for the work windows, whose contents differ by
    search.)
  - *LIMBPT, TERMPT:* to be checked per method (an ellipsoid gives one point
    per cut; a DSK shape many); if no bound is available, the internal arrays
    of these routines become growable, as phase 6 did for their half-plane
    buffers.

A bound can be large for unusual inputs (a GF search over years in steps of
seconds); the variant then signals SPICE(INSUFFICIENTMEMORY) from a failed
ALLOCATE, which is clearer than SPICELIB's WINDOWEXCESS for a size the caller
never chose.

### D5. Errors

The variants keep their SPICELIB routine's errors and FOUND flags, without
the "too small" ones (ARRAYTOOSMALL, BOUNDARYTOOBIG, OUTOFROOM,
WINDOWEXCESS, CELLTOOSMALL, and the checks of MW and NW). On an error, allocatable outputs are allocated with
size 0 (or length 0), so a caller can use them without checking allocation.

## Proposed routines (with D1 A, D2 A)

In the order of the steps below. `ctx%` methods, except where noted.

| Step | New routine | SPICELIB routines |
|---|---|---|
| 7a | `pool_values(name, values, found)` (real), `pool_integers(name, values, found)`, `pool_strings(name, values, found)` | GDPOOL, GIPOOL, GCPOOL (`pool_string` and `pool_names` exist) |
| 7a | `body_values(body, item, values)`, `body` a name or a code (generic) | BODVRD, BODVCD, BODVAR |
| 7a | `fov(instid, shape, frame, bsight, bounds)`: `shape`, `frame` allocatable strings, `bounds(3, n)` | GETFOV, GETFVN (by name) |
| 7a | `body_name(code, name, found)`, `frame_name(code, name)`, `surface_name(code, body, name, found)` | BODC2N, FRMNAM, SRFC2S (`body_name_of` and `frame_name_of` exist internally) |
| 7a | `kernel_data(which, kind, file, filtyp, source, handle, found)`, `kernel_info(file, filtyp, source, handle, found)`: allocatable strings | KDATA, KINFO |
| 7b | `spk_coverage(file, body, intervals)`, `ck_coverage(file, inst, needav, level, tol, timsys, intervals)`, `pck_coverage(file, frame, intervals)` | SPKCOV, CKCOV, PCKCOV |
| 7b | `spk_objects(file, ids)`, `ck_objects(file, ids)`, `pck_frames(file, ids)`, `dsk_objects(file, ids)`, `dsk_surfaces(file, body, ids)` | SPKOBJ, CKOBJ, PCKFRM, DSKOBJ, DSKSRF |
| 7b | `window_to_cell`, `cell_to_window` (module procedures) | |
| 7c | `dsk_plates(handle, dladsc, plates)` (3, n), `dsk_vertices(handle, dladsc, vertices)` (3, n) | DSKP02, DSKV02 |
| 7c | `daf_comments(handle, lines)`, `das_comments(handle, lines)` | DAFEC, DASEC |
| 7c | `ek_values(handle, segno, recno, column, values, isnull)`, generic over real, integer and string values | EKRCED, EKRCEI, EKRCEC |
| 7d | `limb_points(..., npts, points, epochs, tangts)`, `terminator_points(...)`: the inputs of LIMBPT/TERMPT without MAXN | LIMBPT, TERMPT |
| 7e | One variant per GF search, without MW, NW, WORK; `cnfine` and `result` as interval arrays: `gf_distance`, `gf_position`, `gf_subpoint`, `gf_intercept`, `gf_separation`, `gf_occultation`, `gf_illumination`, `gf_phase`, `gf_range_rate`, `gf_target_in_fov`, `gf_ray_in_fov`, `gf_user_scalar`, `gf_user_boolean`, `gf_event` | GFDIST, GFPOSC, GFSUBC, GFSNTC, GFSEP, GFOCLT, GFILUM, GFPA, GFRR, GFTFOV, GFRFOV, GFUDS, GFUDB, GFEVNT (GFOCCE, GFFOVE with progress reporting: the same, later) |

Examples:

```fortran
real(wp), allocatable :: radii(:), cover(:,:), result(:,:)
logical :: found

call ctx%body_values('EARTH', 'RADII', radii)
call ctx%spk_coverage('de421.bsp', 301, cover)
do i = 1, size(cover, 2)
   print *, cover(:, i)
end do
call ctx%gf_distance('MOON', 'NONE', 'EARTH', '<', 400000.0_wp, 0.0_wp, &
                     86400.0_wp, cover, result)
```

## Steps

Each step: the routines, FORD documentation (`!>`, `!!` on every argument),
tests in `test/core` (each new routine against its SPICELIB form, and past
the size where the SPICELIB form fails), an entry in `limits.md`, the table
of routines in the user documentation. The legacy interface and tspice are
unchanged.

1. **7a. Pool, constants, FOV, names, kernel data:** wrappers (size first);
   the simplest and most used.
2. **7b. Coverage and objects:** a bound from the segment count, the window
   converters.
3. **7c. DSK, comments, EK readers:** wrappers (size first).
4. **7d. LIMBPT, TERMPT:** find a bound per method, or growable internal
   arrays; the step with the most unknowns.
5. **7e. GF searches:** the bound from the confinement window and STEP; a
   check of how many work windows each search uses (NW) and whether their
   size follows the same bound.

## Questions for you

1. **D1:** new names (A, recommended) or SPICELIB names overloaded (B)?
2. **D2:** plain arrays for windows and sets (A, recommended), or a window
   type (B)?
3. **Names of the new routines:** are names like `pool_values`,
   `spk_coverage`, `gf_distance` the style you want, or would you prefer
   something closer to SPICELIB (`gdpool_all`, `spkcov_all`, ...)?
4. **Scope:** are the cell, set, window and symbol-table routines right to
   leave out? (A program can keep using them with the converters.)
