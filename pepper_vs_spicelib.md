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
