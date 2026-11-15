# FreeFlyer ephemeris files: a reader and a PEPPER loader (plan)

**Status:** proposed (2026-11-15). No code yet. A separate library, not
part of PEPPER. It uses PEPPER's loader mechanism
(`external_loaders_plan.md`), and is the first real loader for it.

Read (and later write) FreeFlyer's ephemeris files, and load them into
PEPPER like SPKs:

```fortran
use freeflyer_pepper, only: freeflyer_loader

type(freeflyer_loader) :: ff
call ff%map_body('Spacecraft1', -10001)
call spice%add_loader('FREEFLYER', ff, extensions=['.FFephem', '.ephem'])
call spice%furnsh('naif0012.tls')
call spice%furnsh('sc.FFephem')
call ctx%spkezr('Spacecraft1', et, 'J2000', 'NONE', 'EARTH', state, lt)
```

## 1. The library

Two parts:
- **The reader** (module `freeflyer_ephemeris`). It parses a file of
  version 1, 2 or 3 into a derived type: header, columns, rows, and
  discontinuities. It has no dependency, not even PEPPER, so programs that
  don't use SPICE can use it too. The writer is a later phase.
- **The loader** (module `freeflyer_pepper`). It extends PEPPER's
  `kernel_loader`, maps the reader's type onto the kernel builder, and
  depends on PEPPER.

Packaging (decision F1): one repository, `freeflyer-fortran`, holding both
parts as two fpm packages: the reader at the root, and the loader in
`pepper/`, depending on the root package and on PEPPER. The alternative is
a shared `pepper-loaders` library holding the FreeFlyer, OEM (with
ndm-fortran) and STK loaders together.

Style: PEPPER's (as ndm-fortran's plan, section 2):
- fpm and pixi, `implicit none` and no implicit interfaces;
- the `ip`/`wp` kinds, fdoc docs;
- no saved state, a pure parse of an in-memory string, no deferred-length
  function results;
- errors as a status with the line;
- CI with gfortran and ifx.

## 2. The format

[FreeFlyer's help page](https://ai-solutions.com/_help_Files/freeflyer_ephemeris_file.htm)
(a.i. solutions' FreeFlyer) is the only specification. The format is text,
with three versions, set by `FormatVersion` in the header (absent in
version 1). The layout differs by version: the header, the title section,
the epochs and the numbers. The reader reads all three. The page has
inconsistencies (section 6), so real files of each version are needed
before the reader is final.

Version 3, the current one:

```
HEADER_START
FreeFlyer 7.9 Ephemeris
FormatVersion = 3
Spacecraft = "Spacecraft1"
StartTime = 2492596837.000000000 TAI GSFC MJD (Jan 01 2020 00:00:00.000000000 UTC)
StopTime = 2492640007.000000000 TAI GSFC MJD (Jan 01 2020 11:59:30.000000000 UTC)
CentralBody = Earth
ReferenceFrame = ICRF
PrincipalPlane = Equatorial
ColumnDelimiter = ","
DiscontinuityDelimiter = "|"
...
HEADER_END
TITLE_START
COLUMN_LABELS = "ElapsedTime","X","Y","Z","VX","VY","VZ"
COLUMN_UNIT_LABELS = "s","km","km","km","km/s","km/s","km/s"
COLUMN_INTERPOLATORS = "Independent Variable","8th Order Lagrange",...
COLUMN_TYPES = TimeSpan,Variable,Variable,...
TITLE_END
DATA_START
   0.000000000,  -6.64807999999999993e+03,   1.60991000000000014e+02, ...
  30.000000000,  ...
DATA_END
```

The versions:

| | Version 1 | Version 2 | Version 3 |
|---|---|---|---|
| Header | no `HEADER_START`/`HEADER_END`; first line `FreeFlyer 7.9 Ephemeris`; no `FormatVersion`; no delimiter keys | `HEADER_START`…`HEADER_END`, `FormatVersion = 2`, `ColumnDelimiter`, `DiscontinuityDelimiter` | as v2, `FormatVersion = 3` |
| StartTime / StopTime | `Jan 01 2020 00:00:00.000 UTC (Julian TAI:    28849.500428241)` | as v1 | `2492596837.000000000 TAI GSFC MJD (Jan 01 2020 00:00:00.000000000 UTC)` |
| Columns | fixed by flags (`UsePositionVelocity = ON`; the other sets, accelerations, quaternions, angular velocity and acceleration, have no file keys given). The page's template: `Epoch (UTC Calendar)  X Y Z VX VY VZ  Mass  Epoch (TAI Julian)` | `TITLE_START`…`TITLE_END`: `COLUMN_LABELS`, `COLUMN_UNIT_LABELS`, `SPACECRAFT_PROPERTIES`, `COLUMN_INTERPOLATORS` | as v2, plus `COLUMN_TYPES` (`TimeSpan`, `String`, `Variable`) |
| Data block | no markers (v2 added them); no v1 data example | markers: `DATA_START`…`DATA_END` in the examples, `BODY_START`…`BODY_END` in the prose | as v2 |
| Epoch column | UTC calendar (and, per the template, TAI Julian days at the end) | UTC calendar, milliseconds: `Jan 01 2020 00:00:00.000` | `ElapsedTime`, `seconds.nanoseconds` (the page: seconds as a `long long`, nanoseconds as a `long`) |
| Numbers | not shown | fixed point: `-6648.08000000000` | scientific: `-6.64807999999999993e+03` |
| Discontinuities | none | `before|after` per column | as v2 |
| Interpolation | not in the file | per column, `COLUMN_INTERPOLATORS` | as v2 |

- **Times.** "Julian TAI" (v1, v2) is days of TAI since the GSFC modified
  Julian date's origin, JD 2430000.0 (1941 Jan 5 12h). "TAI GSFC MJD" (v3) is
  seconds of TAI since the same origin. The example shows both:
  2492596837 s = 28849.500428241 days, and JD 2458849.5 (2020 Jan 1 0h UTC)
  − 28849.5 = JD 2430000.0. The 0.000428241 day is TAI−UTC, 37 s.
- **Version 3's elapsed time counts from StartTime.** The page doesn't say
  so, but its example is consistent: StopTime − StartTime =
  2492640007 − 2492596837 = 43170 s = 11:59:30, the span from the first
  row to the last.
- **Columns** (v2, v3) are chosen by `COLUMN_LABELS`, with units in
  `COLUMN_UNIT_LABELS`. Other columns (mass, custom ones, `NO_MAPPING`) can
  be present. A comment may follow the last column (`...,Burn 1`).
- **Interpolators** named in FreeFlyer's help: `8th Order Lagrange`,
  `10th Order Lagrange`, `5th Order Spline`, `0th Order Interpolation`. The
  algorithms are not defined.
- **Extensions**: `.FFephem` and `.ephem` (the help's list of formats).

## 3. The reader

### One reader for the three versions

The reader normalizes each version to the same description, then reads the
rows with one routine:

1. **The header** (key = value lines, up to `HEADER_END`, or in v1 up to
   the first line that is not a key): the version (`FormatVersion`, else
   1), the spacecraft, the central body, the frame and plane, the start and
   stop times, the delimiters (v1: commas or blanks).
2. **The columns**: from the title (v2, v3), or for v1 from its fixed
   template and the `Use*` flags. The result is the same either way: which
   column is the epoch, which are X…VZ, their units, their interpolator,
   and the delimiters.
3. **The epochs**, decoded by version, and kept as TAI seconds since the
   GSFC origin, as an `int64` and nanoseconds (an `int32`), so nothing is
   lost:
   - v1: the TAI Julian column if present (no leap-second conversion, and
     finer than milliseconds), else the UTC calendar date. The reader has
     no leap seconds, so a UTC epoch is kept as its calendar fields and
     converted by the caller (the loader, with PEPPER).
   - v2: the UTC calendar date, kept as fields, likewise.
   - v3: StartTime + elapsed time, in integers.
4. **The rows**, between `DATA_START` (or `BODY_START`) and `DATA_END` (or
   `BODY_END`). In v1, which has no markers, the rows run from the end of
   the header to the end of the file. Values are split at the column
   delimiter, and a value with the discontinuity delimiter becomes two. Any
   number format is accepted (Fortran's READ reads fixed point and
   scientific alike).
5. **The discontinuities**: for each row, which columns have two values.

Being lenient where the versions or the page differ costs nothing: a v3
file never has `BODY_START`, and accepting it does no harm.

### The types

```fortran
type :: ff_epoch          ! TAI seconds since JD 2430000.0 (int64) + nanoseconds (int32),
                          ! or UTC calendar fields (v1/v2 without the Julian column)
type :: ff_column         ! label, unit, interpolator, type (TimeSpan/String/Variable)
type :: ff_ephemeris      ! version, the header's keys (spacecraft, central body,
                          ! frame, plane, start, stop, delimiters, the rest as key/value),
                          ! columns(:), epochs(:), values(ncol, nrow), after(ncol, nrow)
                          ! with has_after(ncol, nrow) for discontinuities, comments(:)
```

`ff_read(file, eph, status)` reads the file. `ff_parse(text, eph, status)`
is pure and does no I/O.

## 4. The loader

`freeflyer_loader` extends `kernel_loader`:
- `detect`: the first non-blank line is either `HEADER_START` followed by
  `FreeFlyer <version> Ephemeris` (v2, v3), or `FreeFlyer <version>
  Ephemeris` itself (v1).
- `load`: `ff_read`, then the mapping below, through the builder.
- `unload`: frees what it kept for the file.

Settings, on the loader object:
- `map_body(name, code)`, for spacecraft and central bodies;
- `map_frame(frame, plane, pepper_frame)`;
- whether to keep the extra columns.

The interpolation is not a setting: it is the file's, or an error.

### Mapping to SPK segments

| FreeFlyer | PEPPER | Notes |
|---|---|---|
| `Spacecraft` | body code | The loader's map, then `bodn2c`. Otherwise an error that says how to map it. With a map, `define_body` makes the name usable in queries |
| `CentralBody` | center code | `bodn2c` (`Earth`, `Moon`, `Sun`, `Mars`, ...), and the loader's map |
| `ReferenceFrame` + `PrincipalPlane` | frame name | `ICRF`/`MJ2000` + `Equatorial` → `J2000` (ICRF→J2000 ignores the 23 mas frame bias, documented); `Ecliptic` → `ECLIPJ2000`; others through `map_frame`, else an error |
| v3 epochs, v1 TAI Julian | ET | TAI seconds since JD 2430000.0 → TAI seconds past J2000. The 21545 days are subtracted in integers, before converting to `real(wp)`, so the nanoseconds survive. Then TAI → TDB with `unitim` (the LSK's constants) |
| v1/v2 UTC epochs | ET | `str2et` of the fields + ` UTC` (needs the LSK) |
| X, Y, Z, VX, VY, VZ | states | Units from `COLUMN_UNIT_LABELS` (km, km/s; m and m/s converted). A missing column is an error |
| `N`th Order Lagrange | SPK type 9, degree N | The same interpolation, except at the edges, where SPICE and FreeFlyer may pick the window differently |
| `N`th Order Spline, any other interpolator | error | SPICE(UNSUPPORTEDINTERP), naming the interpolator, the column and the file. No substitute method (PEPPER's D8). Supported once PEPPER adds the method |
| v1 (no interpolator in the file) | SPK type 9 | FreeFlyer's own default for v1, once a real file or FreeFlyer's docs confirm it (8th order Lagrange is likely). It is not a user setting |
| X…VZ with different interpolators | error | One SPK segment interpolates all six components the same way |
| a discontinuity | a new segment | Below |
| StartTime/StopTime | segment bounds | The first and last segments' outer bounds |
| mass, other columns, comments | kept by the loader | Queried through `spice%loader('FREEFLYER', ptr)`; freed by `unload` |
| attitude columns | CK, once PEPPER's builder has it | Until then skipped. A file with no position and velocity is an error |
| `FormatVersion` other than 1–3 | error | SPICE(BADFREEFLYERFILE) |

### Discontinuities

Each discontinuity ends one SPK segment and starts the next at the same
epoch:
- The segment before it ends with the `before` values. The next starts with
  the `after` values.
- Interpolation never crosses the discontinuity: each segment holds only
  its own side's data, with one-sided windows near the ends.
- At the epoch itself, the later segment of the file wins (SPICE's priority
  within a file), so the state there is the one after the maneuver.
- SPKCOV merges the touching intervals, so the coverage stays one
  interval.

The cases to handle:
- **A mark on any of X…VZ splits the segment.** Unmarked columns take the
  same value on both sides. Marks only on other columns (mass) do not split
  it.
- **The epoch column.** One value, or `t|t`: an ordinary split. `ta|tb`
  with ta < tb: a gap (two coverage intervals).
- **Short arcs** between close discontinuities: the builder's `adapt`. With
  too few states for the degree, the degree is lowered. With two states,
  the segment becomes Hermite. A single state is dropped, since it covers
  no interval.
- **Segment identifiers** carry the row's comment where there is one, e.g.
  `Spacecraft1 (after Burn 1)`.

Version 1 has no discontinuities; a v1 file is one segment.

## 5. Phases

Each phase with its tests and docs.

1. **The reader, v2 and v3** (the help page has full examples of both).
   Tests: the page's examples, typed in; errors with their lines.
2. **The loader** (needs PEPPER's loader phases 1–2). Tests:
   - round trips: a FreeFlyer file written by the test from a known
     trajectory (sampled from a test SPK). Loaded, its states equal the
     samples at the rows, and between them equal an SPK written by SPKW09
     from the same samples, bitwise;
   - discontinuities, gaps, short arcs;
   - v3 nanosecond epochs: the same file at two StartTimes gives states
     that differ by exactly the shift;
   - errors: an unknown spacecraft or frame, no LSK, missing columns, an
     unknown version;
   - meta-kernels, `test_parallel`.
3. **Version 1**, once a real v1 file has settled its data rows (section
   6). Tests: the same trajectory as v1, v2 and v3 gives the same states,
   within each version's epoch precision (milliseconds for v2's calendar
   dates).
4. **Attitude** (needs PEPPER's loader phase 5): quaternions and angular
   velocities to a CK, in the same file as the SPK.
5. **The writer**: a FreeFlyer file (v2 or v3) from an `ff_ephemeris`, or
   from SPK segments through PEPPER. This makes the library a converter
   both ways.

## 6. What the help page leaves open

These need real files of each version (from FreeFlyer, or a.i. solutions'
examples) to settle. The reader accepts both readings where there are two:
- **The block names.** The v1 section ends with what v2 changed: "Lastly,
  the v2 ephemeris format uses BODY_START and BODY_END to indicate the start
  and end of the ephemeris' data". So v1's data has no markers, and v2 and
  later have them. But every v2 and v3 example names them
  `DATA_START`/`DATA_END`, so either the sentence or the examples have the
  wrong name. The reader accepts both names in v2/v3, and no marker in v1.
- **Version 1's data rows.** The page has no example: the delimiter, the
  epoch format, and whether the template's `Mass` and `Epoch (TAI Julian)`
  columns are really written. Nor does it give the file keys of the other
  column sets (accelerations, quaternions, ...) or their order.
- **Discontinuities.** The prose says each column gets two values, but in
  the example the epoch column has one.
- **Quoting and escaping** in String columns; comments; line endings.
- **Whether a file can hold several spacecraft** or several header/data
  groups.
- **The full list of interpolators**, frames and central bodies. The help
  pages disagree on the interpolators: `10th Order Lagrange` is on one
  page, `0th Order Interpolation` only on another.
