# PEPPER compared with SPICELIB

PEPPER is NAIF's SPICELIB (toolkit N0067) moved into modern Fortran
modules. It keeps SPICELIB's routines, names, arguments and results, and it
changes three things:

- **Thread safety.** All of SPICELIB's global state now belongs to objects.
  One program can query one set of kernels from many threads at once, or keep
  several independent sets of kernels.
- **No arbitrary limits.** SPICELIB sizes nearly every table at compile time,
  because Fortran 77 has no dynamic memory. PEPPER's tables grow as needed:
  there is no maximum number of loaded files, pool variables, body names,
  frames, BODDEF definitions or EK query terms, and no maximum file name
  length.
- **Speed.** Several of SPICELIB's algorithms scale badly with the amount of
  loaded data; PEPPER replaces them. Typical queries are about as fast as
  SPICELIB built with full optimization, and faster than the SPICELIB that
  NAIF's build scripts produce.

PEPPER passes NAIF's own test suite, tspice: all 399 test families, through a
legacy interface with SPICELIB's names that sits on top of PEPPER.

In short, compared with SPICELIB N0067 built from NAIF's sources on the same
machine (details in [Speed](#speed)):

| | Speedup over SPICELIB built with NAIF's flags | Speedup over SPICELIB built with -O3 |
|---|--:|--:|
| Loading kernels (FURNSH: SPKs, text kernels, names, frames) | 1.5–4.4× | 1.1–2.4× |
| States (SPKEZR, SPKPOS, SPKGEO; one body, DE421) | 1.4–1.6× | 1.0–1.1× |
| Frame transformations (PXFORM, SXFORM) | 1.6–2.3× | 1.1–1.4× |
| Time conversions (STR2ET, ET2UTC, UNITIM) | 3.8–5.1× | 1.2–3.3× |
| A random body among 1000 loaded SPKs | 8.1× | 6.3× |
| A random frame among 1000 TK frames | 64× | 39× |
| Unloading one of 1000 SPKs | 2,074× | 1,814× |
| Queries from 4 threads (PEPPER only) | | 3.9× over one thread |

[Compared with ANISE](#compared-with-anise), the Rust rewrite of SPICE's
core, PEPPER with its binary kernels in memory (an option of FURNSH) is
1.3–2 times faster for states and Earth rotations and 17 times faster for
frames from text PCKs. It is slower only for LT+S states, where it computes
more than ANISE (the rate of the stellar aberration in the velocity). With the kernels read from their files as needed (the default, as
SPICELIB), queries at scattered epochs are 8–27 times slower than ANISE's.

## Thread safety

SPICELIB keeps its state in `SAVE` variables: the loaded kernels, the kernel
pool, the file tables, the error status, and the caches of every subsystem.
Two threads that call SPICELIB at the same time corrupt that state, so a
SPICELIB program queries from one thread only.

PEPPER puts that state into two kinds of objects:

| Object | Holds | Shared by |
|---|---|---|
| Instance, `type(pepper_spice)` | The loaded kernels, the kernel pool, the file tables, the BODDEF names | All the threads that use it |
| Context, `type(pepper_context)` | The error status and messages, the caches, the open files | One thread |

The instance changes the kernels (`spice%furnsh`, `spice%unload`,
`spice%ldpool`, `spice%boddef`, ...). Each thread queries through its own
context (`ctx%spkezr`, `ctx%pxform`, `ctx%str2et`, ...), and the contexts find
the changes of the kernels on their next call. The rule is to load kernels
first and then query from any number of threads; the kernels must not change
while other threads query.

```fortran
type(pepper_spice), target :: spice
type(pepper_context) :: ctx

call spice%furnsh('kernels.tm')
if (spice%failed()) ...

!$omp parallel private(ctx)
ctx = spice%new_context()
!$omp do
do i = 1, n
   call ctx%spkezr('MOON', et(i), 'J2000', 'LT+S', 'EARTH', state(:,i), lt(i))
end do
!$omp end parallel
```

Other consequences:

- **Errors per context.** Each context has its own error status, messages and
  settings (SPICELIB's `ERRACT`, `ERRDEV`, `ERRPRT`). A new context returns on
  errors and prints nothing; the caller checks `ctx%failed()`.
- **Several instances.** A program can keep several independent sets of
  kernels, one instance each.
- **No OpenMP in the library.** The library has no locks and no OpenMP; only
  the program that calls it decides how to use threads.
- **Checked.** `pixi run test-parallel` compares, bit for bit, queries from
  several threads with the same queries from one thread. `pixi run
  check-threads` checks the sources for constructs that gfortran makes
  thread-unsafe.

The measured scaling is under [Queries from several threads](#queries-from-several-threads).

## Limits that PEPPER removes

The limits below are fixed sizes of SPICELIB's tables. When one is exceeded,
SPICELIB signals an error, or worse, silently truncates data. In PEPPER the
tables grow, and each removed limit has a test that goes past it
(`test/core/test_limits.f90`).

### Loading kernels

| What | SPICELIB | When exceeded |
|---|---|---|
| Loaded binary kernels (SPK, CK, PCK, DSK, EK; DAF and DAS files together) | 5000 | SPICE(FTFULL), SPICE(DAFFTFULL), SPICE(DASFTFULL) |
| Files loaded through FURNSH (KEEPER) | 5000, and 300 text kernels | SPICE(NOMOREROOM) |
| Loaded EK files | 20 | SPICE(EKFILETABLEFULL) |
| EK segments, tables and columns of the loaded EKs | 200 segments, 100 tables, 500 columns, 10000 column descriptors | SPICE(EKSEGTABLEFULL), SPICE(EKTABLELISTFULL), ... |
| File and path names | 255 characters | SPICE(FILENAMETOOLONG) in FURNSH; DAFOPR, DASOPR and others called directly silently cut the name and open the wrong file |
| Segments of one body in loaded DSKs | 10000 | SPICE(SEGMENTTABLEFULL) |

### Kernel pool

| What | SPICELIB | When exceeded |
|---|---|---|
| Variables | 26003 | SPICE(KERNELPOOLFULL) |
| Numeric values (all variables together) | 400000 | SPICE(KERNELPOOLFULL), SPICE(NOMOREROOM) |
| String values (all variables together) | 15000 | SPICE(KERNELPOOLFULL), SPICE(NOMOREROOM) |
| Length of a string value | 80 characters | Silently cut (and written past its slot by LMPOOL) |
| Length of a variable name | 32 characters | SPICE(BADVARNAME) |
| Leap seconds | 200 | SPICE(BUFFERTOOSMALL) |

Because each TK frame takes 10 pool variables, the 26003 variables also limit
a kernel set to about 2600 TK frames.

Text kernels load whatever their line terminators: LF or CR-LF on every
platform (and, with gfortran, a lone CR). SPICELIB's FURNSH rejected a text
kernel whose terminators were not the platform's (SPICE(INCOMPATIBLEEOL)),
although the Fortran run-time library could read it; CSPICE never made that
check (`test/core/test_pool.f90`).

### Names, frames and clocks

| What | SPICELIB | When exceeded |
|---|---|---|
| Body names in the kernel pool (`NAIF_BODY_NAME`) | 14983 | SPICE(KERVARTOOBIG) |
| Body names defined with BODDEF | 161 | SPICE(TOOMANYPAIRS) |
| Length of a body or surface name | 36 characters | Silently cut |
| Length of a frame name | 32 characters | Silently cut, or SPICE(VARNAMETOOLONG) for dynamic frames |
| Kernel frames looked up between two changes of the pool | 5209 | SPICE(HASHISFULL) (a cache that fails instead of evicting) |
| Surface names (`NAIF_SURFACE_NAME`) | 2000 | SPICE(TOOMANYSURFACES) |
| Surfaces in a DSK method string (`SURFACES = ...`) | 100 | SPICE(TOOMANYSURFACES) |
| Base frames of a switch frame | 15000 | SPICE(TOOMANYBASEFRAMES) |
| Nesting of dynamic frames (a dynamic frame defined by vectors in another dynamic frame, or an SPK or CK segment in one) | 2 levels | SPICE(RECURSIONTOODEEP) |
| SCLK partitions, and coefficients of one clock | 9999 partitions, 300000 coefficients | SPICE(TOOMANYPARTITIONS), SPICE(TOOMANYCOEFFS) |
| Nutation and precession angles of a body (PCK) | 200 | Error from BODVCD |

### Geometry and searches

| What | SPICELIB | When exceeded |
|---|---|---|
| DSK segments that contain a point | 1000 | SPICE(TOOMANYHITS) |
| Voxels on a ray, plates in a voxel group (DSK type 2 intercepts) | 50000 voxels, 256000 plates | SPICE(ARRAYTOOSMALL) |
| Tangent points per half-plane (LIMBPT, TERMPT) | 2000 | SPICE(ARRAYTOOSMALL), SPICE(WINDOWEXCESS) |
| FOV vertices (GF FOV searches) | 10000 | SPICE(BOUNDARYTOOBIG) |
| Length of a shape/method string in OCCULT and the GF searches | 500 characters (200 in GFSUBC, GFSNTC and GFILUM) | Silently cut: a long DSK surface list gives a syntax error or the wrong surfaces |

### EK queries

| What | SPICELIB | When exceeded |
|---|---|---|
| Query length | 2000 characters | Silently cut |
| Tokens in a query; numbers in a query | 500; 100 | Parse error |
| Tables in FROM (and in a join) | 10 | Parse error |
| SELECT columns; ORDER BY columns | 50; 10 | Parse error |
| Constraints in the normalized WHERE clause | 1000 | Written past the end of an array (no error) |
| Join constraints in one conjunction; conjunctions | 100; 200 | Errors |
| String comparisons (=, <, LIKE, ORDER BY, joins) | First 1024 characters | Strings that agree that far compare equal |

### Error messages

SPICELIB's short error messages have 25 characters, and its long messages 1840;
longer messages are cut (ET2LST's own 26-character short message is cut).
PEPPER keeps whole messages: `ctx%message(option)` returns the whole text,
and `getmsg` fills the caller's variable as before.

### What stays

Some fixed sizes are part of a file format or of SPICE's conventions, and
PEPPER keeps them: the DAF and DAS record and segment formats, the 40-character
DAF segment IDs, the 132-character lines of text kernels (NAIF's documented
rule), the 10 fields of an SCLK string, and guards against nonsense input (a dynamic
frame whose definition depends on itself is an error). Caches whose size affects only speed (record buffers, the 23
open files per context, which can be set with `spice%set_max_open_files`) keep
a size.

## Bugs fixed along the way

Moving SPICELIB into modules with explicit interfaces exposed 71 bugs and
inconsistencies in N0067. Each is listed, with what PEPPER does, in
[`spicelib_bugs.md`](spicelib_bugs.md). Most are harmless (arguments passed
against their declarations, documentation errors), but some give wrong
results without an error:

- ZZDDHOPN copies a file name into a 255-character variable without a check:
  DAFOPR, DASOPR and others open the wrong file when the path is longer.
- ZZRVBF (LMPOOL) writes a string value longer than 80 characters past its
  slot, into the next value.
- ZZEKNRML writes past the end of its constraint arrays when the normalized
  WHERE clause has more than 1000 relations.
- FRAMEX's cache of kernel frames signals SPICE(HASHISFULL) after 5209
  distinct frames, instead of evicting.
- ZZEKJSRT fails (SPICE(STRINGTRUNCATED)) to order EK rows by a fixed-count
  string column longer than 32 characters.

## Speed

### Method

SPICELIB is compiled from NAIF's N0067 sources (the toolkit archive), with the
same compiler as PEPPER, in two ways:

- **NAIF's flags.** NAIF's build script (`src/spicelib/mkprodct.csh`)
  compiles with `-std=legacy -Wno-character-truncation` and no optimization
  flag, so gfortran's default, `-O0`, applies; only MOVED and MOVEI get
  `-O2`. This is the library of the toolkit a user downloads from NAIF. (The
  script also has `-m64`, which has no meaning on arm64.)
- **-O3.** `-O3 -funroll-loops`, the optimization of PEPPER's release build.
  This compares the code, not the compiler flags.

PEPPER is its release build (`fpm --profile release`). Two programs do the
same operations in the same order with the same pseudo-random inputs:
`test/core/bench_compare.f90` (PEPPER) and `bench/bench_spicelib.f90`
(SPICELIB). Each operation also sums its results; PEPPER's sums agree with
those of the -O3 SPICELIB to the last bit (the build with NAIF's flags
differs in the last digits, because `-O3` fuses multiplications and
additions).

- **Real kernels:** naif0012.tls (leap seconds), pck00011.tpc (text PCK),
  de421.bsp (planets, Moon), earth_latest_high_prec.bpc (ITRF93),
  moon_pa_de440_200625.bpc and moon_de440_250416.tf (Moon frames). The query
  epochs are random over 20 years from 2000.
- **Synthetic kernels:** 1000 SPKs of one body each, a text kernel of 20000
  variables (4 values each), 10000 body names and 1000 TK frames.
- **Counts:** 100000 queries of each kind, 20 loads of the real kernel set.
- **Machine:** Apple M5 (4 performance and 6 efficiency cores), macOS,
  gfortran 16.2. Each time is the fastest of 7 runs.

To repeat it: `pixi run compare-spicelib --kernels DIR`, where DIR holds the
real kernels above (`bench/compare_spicelib.py`).

### Results

Microseconds per operation, and PEPPER's speedup over each SPICELIB build:

| Operation | SPICELIB, NAIF's flags (µs) | SPICELIB, -O3 (µs) | PEPPER (µs) | Speedup over NAIF's build | Speedup over -O3 |
|---|--:|--:|--:|--:|--:|
| **Loading** | | | | | |
| FURNSH of 6 real kernels (LSK, PCK, DE421, Earth and Moon binary PCKs, Moon FK), per set | 4,040 | 2,538 | 2,255 | 1.8× | 1.1× |
| FURNSH of 1000 SPKs, per file | 67.4 | 67 | 45.2 | 1.5× | 1.5× |
| UNLOAD of 1000 SPKs, per file | 9,080 | 7,941 | 4.38 | 2,074× | 1,814× |
| FURNSH of a text kernel, per variable (20000 variables) | 3.2 | 1.48 | 1.24 | 2.6× | 1.2× |
| FURNSH of 10000 body names and the first lookup, per name | 4.14 | 2.26 | 0.945 | 4.4× | 2.4× |
| FURNSH of 1000 TK frames, per frame | 13.4 | 9.48 | 6.07 | 2.2× | 1.6× |
| **States** | | | | | |
| SPKEZR Moon from Earth, J2000, no correction | 13.6 | 10.2 | 9.8 | 1.4× | 1.0× |
| SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S | 15.3 | 10.5 | 9.74 | 1.6× | 1.1× |
| SPKPOS Sun from Earth, J2000, CN+S | 14.6 | 10.5 | 9.57 | 1.5× | 1.1× |
| SPKGEO Moon from Earth, by code | 13.5 | 10.1 | 9.71 | 1.4× | 1.0× |
| SPKGEO of a random body among 1000 loaded SPKs | 110 | 85 | 13.5 | 8.1× | 6.3× |
| **Frames** | | | | | |
| PXFORM J2000 to ITRF93 (binary PCK) | 6.09 | 4.14 | 3.77 | 1.6× | 1.1× |
| PXFORM J2000 to IAU_MARS (text PCK) | 1.38 | 0.855 | 0.589 | 2.3× | 1.4× |
| PXFORM MOON_ME to J2000 (FK + binary PCK) | 3.52 | 2.31 | 1.78 | 2.0× | 1.3× |
| SXFORM J2000 to ITRF93 | 6.12 | 4.05 | 3.73 | 1.6× | 1.1× |
| PXFORM of a random frame among 1000 TK frames | 234 | 143 | 3.67 | 63.8× | 39.1× |
| **Time** | | | | | |
| STR2ET of an ISO UTC string | 6.71 | 2.18 | 1.77 | 3.8× | 1.2× |
| ET2UTC, calendar format | 2.26 | 1.28 | 0.481 | 4.7× | 2.7× |
| UNITIM TDB to TAI | 0.832 | 0.547 | 0.163 | 5.1× | 3.3× |
| **Names and constants** | | | | | |
| BODN2C of a built-in name | 0.137 | 0.106 | 0.0695 | 2.0× | 1.5× |
| BODN2C of a random name among 10000 kernel names | 0.495 | 0.43 | 0.359 | 1.4× | 1.2× |
| BODVRD Earth radii | 0.303 | 0.191 | 0.0859 | 3.5× | 2.2× |
| GDPOOL of a random variable among 20000 | 0.406 | 0.355 | 0.301 | 1.3× | 1.2× |

The loading times depend on the file system (each load opens files) and
vary between runs by up to a factor of 2 for all three builds; the table has
the fastest of 7 runs. The query times vary by a few percent.

### Where the differences come from

- **NAIF's build is not optimized.** Compiler optimization alone makes
  SPICELIB up to 3 times faster (compare its two columns). A downloaded
  toolkit is the slower one.
- **Single-body queries cost the same code in both.** States and frame
  transformations of one body evaluate the same SPICELIB algorithms; against
  the -O3 build, PEPPER is within about 10 percent, and up to 1.4 times
  faster for frames defined by text kernels.
- **Algorithms that scale with the loaded data:**
  - *UNLOAD:* after unloading a binary kernel, SPICELIB reloads every
    remaining kernel of that type to restore their priority order, and each
    reload opens the file: unloading N files takes O(N²) file opens. PEPPER
    reloads only when the unloaded file was loaded twice.
  - *Many SPKs:* SPICELIB's segment search for a body not seen before reads
    the summaries of the loaded files from the newest down, reopening files
    (it keeps 23 open). PEPPER keeps an index of the segments of all loaded
    files.
  - *Many TK frames:* SPICELIB keeps the last 200 TK frames and reparses a
    frame from the pool when it is not among them; its cache of kernel frame
    names never hits (a bug: names are stored under one key and looked up
    under another). PEPPER's caches have no cap and hit.
- **Time conversions:** SPICELIB's DELTET reads its five kernel variables
  and computes the ET of every leap second on every call. PEPPER reads them
  when the pool changes. ET2UTC and UNITIM go through DELTET.
- **Body names and constants:** lookups through hash maps, and BODVRD's
  pool lookup by a map.
- **Loading binary kernels:** SPICELIB opens each file four times during
  FURNSH (it checks twice whether the file is loaded already, opening it
  each time, and opens it again to read its type); PEPPER keeps the result
  of the first check for the rest of the load and opens the file twice.
- **Loading text kernels:** the kernel pool keeps its variables in a hash
  map that adds a new name with one lookup, and keeps its capacity when
  kernels are unloaded.

### Queries from several threads

Queries from several threads (`pixi run bench-parallel`, the same machine
as below; synthetic kernels; each epoch is an SPKEZR with CN+S, an SPKPOS with
LT, two PXFORMs, an ET2UTC and an STR2ET; 200000 epochs):

| Threads | µs per epoch | Speedup |
|--:|--:|--:|
| 1 | 10.17 | 1.00 |
| 2 | 5.10 | 1.99 |
| 4 | 2.60 | 3.91 |
| 8 | 1.87 | 5.43 |

The machine has 4 performance cores and 6 efficiency cores, so the scaling
flattens past 4 threads. SPICELIB cannot run these queries in parallel at
all.

## Prepared queries

PEPPER can resolve a state or frame query once and then evaluate it at any
number of epochs (`ctx%prepare_state` and `ctx%query_state`,
`ctx%prepare_rotation`, `ctx%query_rotation` and `ctx%query_transform`; see
the README). The results are SPKEZR's, PXFORM's and SXFORM's, to the last
bit (`test/core/test_queries.f90` compares 30,240 states over every
combination of target, observer, frame and correction). A state query does
less work per epoch:

- the names, the aberration correction and the frame are resolved once;
- the target's chain of segments is found from the segment descriptors
  before any is evaluated, and only the links that the result needs are
  evaluated (SPKGEO evaluates the target's whole chain to the solar system
  barycenter: for the Moon from the Earth, the Earth-Moon barycenter's
  segment, whose state it does not use);
- for SPK types 2 and 3 (the planetary ephemerides) the query keeps each
  segment's current Chebyshev record and reads a record only when the epoch
  moves to another.

`pixi run bench-queries` (`test/core/bench_queries.f90`, release build, the
same machine and kernels as the ANISE comparison; 200,000 epochs per
operation, random epochs over 20 years or sequential epochs 60 s apart):

With the binary kernels in memory:

| Operation | Epochs | SPICELIB routine (µs) | Prepared query (µs) | Batch (µs) | Speedup (query) | Speedup (batch) |
|---|---|--:|--:|--:|--:|--:|
| SPKEZR Moon from Earth, J2000, NONE | random | 0.268 | 0.121 | 0.120 | 2.2× | 2.2× |
| SPKEZR Moon from Earth, J2000, NONE | sequential | 0.250 | 0.098 | 0.093 | 2.5× | 2.7× |
| SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S | random | 0.858 | 0.535 | 0.550 | 1.6× | 1.6× |
| SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S | sequential | 0.851 | 0.502 | 0.522 | 1.7× | 1.6× |
| SPKEZR Mars barycenter from Earth, J2000, LT | random | 0.428 | 0.238 | 0.232 | 1.8× | 1.8× |
| SPKEZR Mars barycenter from Earth, J2000, LT | sequential | 0.421 | 0.204 | 0.201 | 2.1× | 2.1× |
| SPKEZR Sun from Earth, J2000, CN+S | random | 0.924 | 0.525 | 0.530 | 1.8× | 1.7× |
| SPKEZR Sun from Earth, J2000, CN+S | sequential | 0.914 | 0.489 | 0.493 | 1.9× | 1.9× |
| SPKEZR Moon from Earth, ITRF93, LT+S | random | 1.286 | 0.869 | 0.869 | 1.5× | 1.5× |
| SPKEZR Moon from Earth, ITRF93, LT+S | sequential | 1.211 | 0.777 | 0.774 | 1.6× | 1.6× |
| PXFORM J2000 to ITRF93 | random | 0.274 | 0.255 | 0.256 | 1.1× | 1.1× |
| PXFORM J2000 to ITRF93 | sequential | 0.232 | 0.215 | 0.214 | 1.1× | 1.1× |
| PXFORM J2000 to IAU_MARS | random | 0.585 | 0.562 | 0.569 | 1.0× | 1.0× |
| PXFORM J2000 to IAU_MARS | sequential | 0.531 | 0.502 | 0.508 | 1.1× | 1.0× |
| PXFORM MOON_ME to J2000 | random | 0.595 | 0.581 | 0.574 | 1.0× | 1.0× |
| PXFORM MOON_ME to J2000 | sequential | 0.570 | 0.561 | 0.556 | 1.0× | 1.0× |
| PXFORM J2000 to ECLIPJ2000 | random | 0.047 | 0.003 | 0.004 | 14.4× | 12.0× |
| PXFORM J2000 to ECLIPJ2000 | sequential | 0.047 | 0.003 | 0.004 | 14.3× | 11.6× |
| SXFORM J2000 to ITRF93 | random | 0.307 | 0.281 | 0.293 | 1.1× | 1.0× |
| SXFORM J2000 to ITRF93 | sequential | 0.264 | 0.241 | 0.239 | 1.1× | 1.1× |

With the binary kernels read from their files as needed:

| Operation | Epochs | SPICELIB routine (µs) | Prepared query (µs) | Batch (µs) | Speedup (query) | Speedup (batch) |
|---|---|--:|--:|--:|--:|--:|
| SPKEZR Moon from Earth, J2000, NONE | random | 9.811 | 6.546 | 6.806 | 1.5× | 1.4× |
| SPKEZR Moon from Earth, J2000, NONE | sequential | 0.378 | 0.098 | 0.093 | 3.9× | 4.1× |
| SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S | random | 9.439 | 8.728 | 8.649 | 1.1× | 1.1× |
| SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S | sequential | 1.201 | 0.498 | 0.516 | 2.4× | 2.3× |
| SPKEZR Mars barycenter from Earth, J2000, LT | random | 8.649 | 8.389 | 8.297 | 1.0× | 1.0× |
| SPKEZR Mars barycenter from Earth, J2000, LT | sequential | 0.576 | 0.201 | 0.199 | 2.9× | 2.9× |
| SPKEZR Sun from Earth, J2000, CN+S | random | 11.138 | 9.730 | 9.305 | 1.1× | 1.2× |
| SPKEZR Sun from Earth, J2000, CN+S | sequential | 1.378 | 0.486 | 0.491 | 2.8× | 2.8× |
| SPKEZR Moon from Earth, ITRF93, LT+S | random | 15.988 | 15.033 | 15.019 | 1.1× | 1.1× |
| SPKEZR Moon from Earth, ITRF93, LT+S | sequential | 1.717 | 0.826 | 0.823 | 2.1× | 2.1× |
| PXFORM J2000 to ITRF93 | random | 3.722 | 3.697 | 3.692 | 1.0× | 1.0× |
| PXFORM J2000 to ITRF93 | sequential | 0.269 | 0.250 | 0.244 | 1.1× | 1.1× |
| PXFORM J2000 to IAU_MARS | random | 0.598 | 0.567 | 0.570 | 1.1× | 1.0× |
| PXFORM J2000 to IAU_MARS | sequential | 0.523 | 0.499 | 0.497 | 1.0× | 1.1× |
| PXFORM MOON_ME to J2000 | random | 1.746 | 1.715 | 1.707 | 1.0× | 1.0× |
| PXFORM MOON_ME to J2000 | sequential | 0.604 | 0.593 | 0.586 | 1.0× | 1.0× |
| PXFORM J2000 to ECLIPJ2000 | random | 0.046 | 0.003 | 0.004 | 14.1× | 11.7× |
| PXFORM J2000 to ECLIPJ2000 | sequential | 0.046 | 0.003 | 0.004 | 14.2× | 11.7× |
| SXFORM J2000 to ITRF93 | random | 3.758 | 3.736 | 3.744 | 1.0× | 1.0× |
| SXFORM J2000 to ITRF93 | sequential | 0.296 | 0.273 | 0.275 | 1.1× | 1.1× |

- **States:** 1.5 to 2.5 times faster in memory. Read from the files, the
  gain is largest at sequential epochs (2 to 4 times: the query reads a
  record only when the epoch leaves it); at random epochs the reading of
  the files dominates.
- **Frames:** about 1.1 times, from the names only. The time is in
  evaluating the orientation itself (for ITRF93, the binary PCK; for
  IAU_MARS, the text PCK's polynomials): rotations that do not depend on
  time (inertial and fixed TK frames, such as J2000 to ECLIPJ2000) are
  computed once and are 14 times faster.
- **Against ANISE** (next section): ANISE takes 0.35 µs for the Moon from
  the Earth and 0.78 µs for Mars from the Earth with LT+S; prepared queries
  take 0.12 and 0.54 µs.
- **Batches** give the speed of the single queries: the gain is in the
  preparation. A batch is the place for further work (see
  `modernization_options.md`).

## Compared with ANISE

[ANISE](https://github.com/nyx-space/anise) (nyx-space) is a rewrite of
SPICE's core in Rust: it reads SPK and binary PCK files and computes states
and frame transformations, with its own data formats for planetary constants
and frame definitions. It has a new API, not SPICELIB's. It is thread-safe.

### Method

Two programs make the same queries at the same epochs:
`test/core/bench_anise.f90` (PEPPER, release build) and `bench/anise` (a Rust
program on the `anise` crate 0.10.6, the latest stable release; release build
with `opt-level = 3`, fat LTO and one codegen unit; Rust 1.99). The kernels
are the real kernels of the SPICELIB comparison. Each operation runs at two
sets of epochs:

- **Random epochs:** 100000 pseudo-random epochs over 20 years from 2000, as
  in the SPICELIB comparison. Almost every query needs data from a different
  part of the file.
- **Sequential epochs:** 100000 epochs one minute apart (69 days). Successive
  queries use the same records.

Each operation also sums its results, and the sums of the two programs
agree (to 10 significant digits or better: the two libraries convert epochs
and compose rotations in different orders). Each time is the fastest of 7
runs, on the machine of the SPICELIB comparison.

The two libraries do not take quite the same inputs:

- **Text kernels.** ANISE does not read text kernels. The text PCK and the
  FK are converted to ANISE's formats first (PCA and EPA data sets), which is
  timed separately. ANISE's converter keeps only bodies with a GM, and
  pck00011.tpc has none: the program supplies DE440's GMs of Mars, its
  barycenter, the Moon and the Earth-Moon barycenter (they do not enter the
  rotations).
- **Names.** ANISE takes NAIF IDs. The SPKEZR rows pass names to PEPPER and
  IDs to ANISE; the SPKGEO rows pass IDs to both.
- **ECLIPJ2000.** ANISE 0.10.6 knows this frame only from its own planetary
  constants file. Its side applies SPICE's fixed rotation by the obliquity of
  J2000 to the J2000 state.
- **MOON_ME.** In moon_de440_250416.tf, MOON_ME is an alias of
  MOON_ME_DE440_ME421. ANISE 0.10.6's FK converter maps the alias to the
  Moon's body ID with an identity rotation, which is wrong, so its side asks
  for MOON_ME_DE440_ME421 directly.

To repeat it: `pixi run -e anise compare-anise --kernels DIR`
(`bench/compare_anise.py`). The `anise` environment adds Rust; the default
environment does not have it.

### Results

Microseconds per operation. PEPPER runs every query twice:
- **Read as needed:** the binary kernels are read from their files record by
  record, as in SPICELIB (the default).
- **In memory:** the binary kernels are loaded with
  `spice%furnsh(file, in_memory=.true.)`, which reads each file whole into
  memory, as ANISE does.

The last column is the in-memory speedup over ANISE (below 1× when ANISE is
faster). All three give the same sums; PEPPER's two give the same results
to the last bit.

| Operation | ANISE 0.10.6 (µs) | PEPPER, read as needed (µs) | PEPPER, in memory (µs) | In memory: speedup over ANISE |
|---|--:|--:|--:|--:|
| **Loading** | | | | |
| Loading the kernel set (PEPPER: FURNSH of the 6 kernels; ANISE: the 3 binary kernels and the converted text PCK and FK), per set | 3,775 | 1,922 | 4,389 | 0.86× |
| Converting the text PCK and the FK to ANISE's formats (ANISE only; once) | 938 | – | – | – |
| **States, random epochs** | | | | |
| SPKEZR Moon from Earth, J2000, no correction | 0.352 | 10 | 0.262 | 1.3× |
| SPKGEO Moon from Earth, by code | 0.376 | 9.79 | 0.217 | 1.7× |
| SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S ¹ | 0.781 | 9.47 | 0.854 | 0.91× |
| SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT | 0.756 | 8.88 | 0.473 | 1.6× |
| SPKPOS Mars barycenter from Earth, ECLIPJ2000, LT+S | 0.777 | 8.78 | 0.468 | 1.7× |
| SPKPOS Sun from Earth, J2000, CN+S | 1.17 | 9.87 | 0.591 | 2.0× |
| **States, sequential epochs** | | | | |
| SPKEZR Moon from Earth, J2000, no correction | 0.34 | 0.374 | 0.248 | 1.4× |
| SPKGEO Moon from Earth, by code | 0.364 | 0.328 | 0.202 | 1.8× |
| SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT+S ¹ | 0.772 | 1.12 | 0.84 | 0.92× |
| SPKEZR Mars barycenter from Earth, ECLIPJ2000, LT | 0.744 | 0.583 | 0.464 | 1.6× |
| SPKPOS Mars barycenter from Earth, ECLIPJ2000, LT+S | 0.768 | 0.572 | 0.457 | 1.7× |
| SPKPOS Sun from Earth, J2000, CN+S | 1.16 | 0.864 | 0.583 | 2.0× |
| **Frames, random epochs** | | | | |
| PXFORM J2000 to ITRF93 (binary PCK) | 0.47 | 3.74 | 0.279 | 1.7× |
| SXFORM J2000 to ITRF93 | 0.466 | 3.77 | 0.301 | 1.5× |
| PXFORM J2000 to IAU_MARS (text PCK) | 9.92 | 0.578 | 0.592 | 17× |
| PXFORM MOON_ME to J2000 (FK + binary PCK) | 0.619 | 1.8 | 0.598 | 1.0× |
| **Frames, sequential epochs** | | | | |
| PXFORM J2000 to ITRF93 (binary PCK) | 0.44 | 0.265 | 0.238 | 1.8× |
| SXFORM J2000 to ITRF93 | 0.433 | 0.293 | 0.265 | 1.6× |
| PXFORM J2000 to IAU_MARS (text PCK) | 9.49 | 0.514 | 0.52 | 18× |
| PXFORM MOON_ME to J2000 (FK + binary PCK) | 0.623 | 0.618 | 0.574 | 1.1× |

¹ Not the same quantity: SPICE's LT+S velocity (PEPPER's) is the velocity of
the apparent position, including the rate of change of the stellar
aberration correction, which needs the observer's acceleration; ANISE's
applies the correction to the position only. The LT and SPKPOS rows compare
the same quantities.

### Where the differences come from

- **Reading records (random epochs).** By default, PEPPER reads a binary
  kernel record by record and keeps recent records in a buffer, as SPICELIB
  does. A query whose record is not in the buffer reads it from the file. At
  random epochs nearly every query does that, and the read costs about 9 µs
  of the 10; SPICELIB -O3 takes 10.2 µs for the same query. ANISE reads each
  binary kernel whole into memory when it loads it, and so can PEPPER, with
  `in_memory`. Then the query costs the same at random and at sequential
  epochs, 0.22 µs for SPKGEO, 45 times less than when read as needed.
- **In memory, the read is a copy.** PEPPER's in-memory reads skip the record
  buffer altogether (one array copy per read), so they are faster than reads
  from the buffer even at sequential epochs: SPKGEO 0.31 → 0.21 µs.
- **The computation itself.** With the data in memory, PEPPER is 1.3–2
  times faster than ANISE for geometric states, light time (LT), the LT+S and
  CN+S positions and ITRF93, and about equal for MOON_ME.
- **LT+S states.** The one row where ANISE is faster (0.78 against 0.85 µs)
  computes more in PEPPER: SPICE's velocity includes the rate of change of
  the stellar aberration, which needs the observer's acceleration
  (SPKACS differentiates the observer's velocity numerically, with two more
  ephemeris evaluations) and the derivative of the correction (ZZSTELAB).
  ANISE leaves that rate out of the velocity. For the same quantities, the
  LT state and the LT+S position, PEPPER takes 0.46–0.47 µs against ANISE's
  0.74–0.78. (This comparison also found that inertial frames other than
  J2000 cost PEPPER 0.33 µs per corrected state: SPKACS, SPKAPS and SPKLTC
  each looked up the frame name with a case-insensitive search of SPICELIB's
  21 inertial frames. A context now keeps the last name it found; the
  ECLIPJ2000 LT+S state went from 1.17 to 0.85 µs.)
- **Frames from text PCKs.** ANISE evaluates IAU_MARS in about 10 µs, and
  PEPPER in about 0.5–0.6 µs: PEPPER is 17–18 times faster.
- **Loading.** Read as needed, PEPPER loads the kernel set in half ANISE's
  time. In memory, it reads the same 35 MB of files as ANISE and takes a
  little longer (4.6 against 3.8 ms); ANISE also has to convert the text
  kernels to its own formats first (1.0 ms).
- **Memory.** In memory, each instance holds the files once, shared by all
  its contexts (threads). The choice is per file, so a program can keep a
  planetary ephemeris in memory and read a large CK as needed.

## Compatibility

- **The same routines.** The module API keeps SPICELIB's names and argument
  order: `call spkezr(...)` becomes `call ctx%spkezr(...)`, and `call
  furnsh(...)` becomes `call spice%furnsh(...)`.
- **The same results.** In the comparison above, every query of PEPPER gives
  the same result, to the last bit, as SPICELIB compiled with the same
  optimization.
- **SPICELIB programs.** The legacy interface (`tspice/src/legacy`) has
  SPICELIB's external routines with SPICELIB's names and arguments, on a
  default instance; NAIF's test suite runs through it. Like SPICELIB, it is
  not thread-safe.
- **The same files.** PEPPER reads and writes the same kernel files.
