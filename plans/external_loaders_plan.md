# Kernel loaders for other file formats: plan

**Status:** phases 1 and 2 implemented (2026-11-15), programmatic
kernels in the C interface and PepperyPy, and phase 3's `define_body`
(body names owned by a kernel: BODDEF records with an owner, removed when
the kernel is unloaded, every context rebuilding its name tables), and
phase 4 as `kb%add_ephemeris` (a provider owned by the kernel: the
registration of `add_ephemeris` with an owner, not replacing another of the
same body, removed with the kernel; SPKCOV and SPKOBJ by the kernel's name;
`write_kernel` refuses it). Frames (2026-11-15): `kb%frame_rotations`
(rotations at ET epochs, quaternions or matrices, as a CK type 5 segment
with ET tags in a DAF built in memory, evaluated by CKR05/CKE05 through a
`rotation_table` frame provider: no clock) and `kb%add_frame` (a frame
provider owned by the kernel); programmatic frames have owners, kernels of
one frame coexist (the last that has data answers). CKCOV/CKOBJ by a
kernel's name report its rotation tables; `write_kernel(name, file,
ck_file, text_file)` writes them as a CK (ticks = ET, shifted to be
non-negative if an epoch is before J2000) and a text kernel (a type 1 SCLK
per frame, the CK frame definitions, the body names), checked with CSPICE.
Not yet: the binary test format (phase 3); frame definitions owned by a
kernel (4c); the frame features in the C interface and PepperyPy (except
`write_kernel`'s new arguments, which they have).

What was built:
- `src/pepper_loaders.f90` (a submodule of `pepper_instance_module`): the
  builder (`kernel_builder`: `spk_lagrange`, `spk_hermite`, `set_format`,
  `clear`, `nsegments`; `adapt`), programmatic kernels (`load_kernel`,
  `write_kernel`), the registry (`add_loader`, `remove_loader`, `loader`),
  `kernel_format`, and the choice of a loader for FURNSH;
- `src/pepper_spk_packing.f90`: the data of SPK types 9 and 13, used by the
  builder and by SPKW09 and SPKW13;
- DAFs built in memory: `ddh_file%synthetic`; ZZDDHOPN finds them by name
  (the most recent, if several), ZZDDHF2H skips them, ZZDDHHLU refuses a
  unit for them (SPICE(NOFILEFORKERNEL)); ZZDAFGFR, ZZDAFGSR and DAFRCR
  read their file, summary and name records from the image; DAFEC and
  `daf_comments` give no comments; GETFAT reports them as DAFs of their
  type (so SPKCOV, SPKOBJ, CKCOV work by name);
- FURNSH's `loader` argument, ZZLDKER's resolution, `KERNEL_LOADERS`, the
  unload callbacks in UNLOAD and KCLEAR; `keeper_kernel%loader` and
  `%format`;
- the tests: `test/core/test_loaders.f90` with a made-up format
  (`test/core/pepper_test_loader.f90`: a header of `KEY = value` lines,
  blocks of `et x y z vx vy vz` rows), including the round trip format ->
  kernel -> .bsp -> format -> kernel; builders filled by threads and
  kernels queried by threads in `test_parallel`.

Differences from the plan below:
- The test format is the simple text one (with `OBJECT_CODE` for a name the
  file defines); its binary form (phase 3) is not done.
- One reserved name, `PEPPER` (2026-11-15): `loader='PEPPER'` is PEPPER's
  own loading (as without loaders; registered loaders not asked); it
  cannot be registered. The former reserved names (SPK, CK, PCK, DSK, EK,
  TEXT, JSON, META) are ordinary names.
- `load_kernel` copies the builder's data into the DAF image (one copy),
  then empties the builder; with `keep` it is not emptied.
- A file a loader read, loaded again, is read again (a second kernel, with
  its own handle); UNLOAD removes the last one, as for any file loaded
  twice. FURNSH reads the file on disk even when a kernel built in memory
  has its name (`zzddhman%files_only`, set while ZZLDKER loads from
  disk): with `loader='PEPPER'`, after the loader is removed, or for an
  SPK written under the kernel's name.

Let a program attach **loaders** to an instance: objects that read a file
format SPICE does not know and turn it into ordinary kernel data at load
time. After `furnsh`, such a file is used exactly like an SPK: SPKEZR and
the other state routines, light time and aberration, GF searches, prepared
queries, SPKCOV and SPKOBJ, KDATA and KINFO, UNLOAD and KCLEAR, priority by
load order, meta-kernels, every context at once.

The same machinery serves **programmatic kernels**: a program fills a
builder with its own arrays (no file at all) and loads it under a name. A
file loader is then just "read the file, fill a builder". The builder is the
core of the feature (phase 1), and loaders are a thin layer on it (phase 2).

```fortran
type(kernel_builder) :: kb
call kb%spk_hermite(ctx, -10001, 399, 'J2000', epochs, states, 7, tbeg, tend, 'MY TRAJECTORY')
call spice%load_kernel('my_trajectory', kb)      ! a kernel with no file
```

PEPPER provides only the mechanism: the builder, the registry, and
FURNSH's choice of a loader. It contains no loader for any real format. Loaders live in other libraries, for example:
- a FreeFlyer ephemeris loader (`freeflyer_loader_plan.md`);
- a CCSDS OEM/OMM/AEM loader, joining PEPPER and
  [ndm-fortran](https://github.com/jacobwilliams/ndm-fortran);
- STK `.e`, SP3, or a mission's own formats.

PEPPER's tests use a made-up format (section 5) designed to exercise
everything those loaders need (section 2).

```fortran
type(my_format_loader) :: ld                     ! a loader, from another library

call ld%map_body('Spacecraft1', -10001)          ! its own settings
call spice%add_loader('MYFORMAT', ld, extensions=['.myeph'])

call spice%furnsh('naif0012.tls')
call spice%furnsh('sc.myeph')                    ! chosen by extension
call spice%furnsh('sc_ephem.txt', loader='MYFORMAT')   ! chosen by name
call ctx%spkezr('-10001', et, 'J2000', 'NONE', 'EARTH', state, lt)
call spice%unload('sc.myeph')
```

## 1. What exists

- **Binary kernels in memory** (`in_memory_kernels_design.md`): a DAF can
  be held whole in `ddh_file%image`. DAFGDA and ZZDAFGDR read from the
  image, contexts never open a unit for it, and any number of contexts read
  it at once with no lock.
- **Programmatic ephemerides** (`programmatic_plan.md`,
  `src/pepper_programmatic.f90`): an `ephemeris_provider` registered with
  `spice%add_ephemeris` becomes a "virtual SPK file". This is a one-segment
  file of type 100 with a negative handle, in the SPK segment index, and
  SPKPVN asks the provider for the state. It is not a kernel: KDATA, SPKCOV
  and UNLOAD do not know it.
- **The loader** (ZZLDKER, `src/pepper_keeper.f90`). GETFAT gives the
  architecture: DAF goes to SPKLEF/CKLPF/PCKLOF, DAS to EKLEF/ZZDSKLSF,
  anything else to LDPOOL (a text kernel, or a JSON kernel by its `.json`
  extension). The keeper's table (`keeper_kernel`: name, type, handle,
  source) is what KDATA, KINFO, UNLOAD and KCLEAR work from.
- **The rule**: the instance loads, contexts only query. Loaders fit it,
  since they run only inside `furnsh` (and unload callbacks inside
  `unload`/`kclear`), on the instance.

## 2. What loaders need

The requirements, from the formats in view. Each one is in the design
(section 3) and in the test format (section 5).

| # | Requirement | Needed by |
|---|---|---|
| R1 | Choose the loader by name, by extension, or by the file's content; work in meta-kernels and through the legacy FURNSH | all; extensions are not reliable for OEM (`.txt`, `.xml`) |
| R2 | Text and binary files: the loader does its own I/O | all; Code-500 and other binary formats |
| R3 | Several segments per file, in priority order | OEM segments, FreeFlyer discontinuities (maneuvers) |
| R4 | Segments that share an epoch (the later one wins there) and gaps (coverage in several intervals) | FreeFlyer discontinuities, OEM segments |
| R5 | Lagrange and Hermite interpolation, of a degree per segment; any other method is an error (D8) | OEM (`INTERPOLATION`), FreeFlyer (`N`th Order Lagrange; its Spline is an error for now) |
| R6 | Segment bounds inside the data (extra points at the ends for the interpolation) | OEM `USEABLE_START/STOP_TIME`, FreeFlyer StartTime/StopTime |
| R7 | Short segments: too few states for the degree (lower it), or one state (drop it) | FreeFlyer arcs between close maneuvers, short OEM segments |
| R8 | Several bodies per file, and several centers and frames | SP3, STK, OEM files of several objects |
| R9 | Names from the file mapped to NAIF codes and frames, by the loader's settings or PEPPER's own lookups (`bodn2c`, `namfrm`) | all |
| R10 | Epochs in several time systems (UTC, TAI, TT, TDB, GPS), absolute or relative to an origin in the file, with precision kept (integer seconds + nanoseconds) | FreeFlyer v3, OEM (`TIME_SYSTEM`), SP3 (GPS) |
| R11 | Several layouts of one format (versions) in one loader | FreeFlyer v1/v2/v3, OEM v1/v2/v3, KVN and XML |
| R12 | Data with no place in an SPK, kept by the loader, queried later, freed at unload | OEM covariances, FreeFlyer mass and comments |
| R13 | Atomic loads: an error anywhere loads nothing, with the file and line in the message | all |
| R14 | A body name the file defines becomes usable in queries (`spkezr('SPACECRAFT1', ...)`) | FreeFlyer `Spacecraft`, OEM `OBJECT_NAME` |
| R15 | Attitude: CK segments, and the SCLK they need | OEM's sibling AEM, FreeFlyer attitude columns |
| R16 | States from a model rather than a table | OMM (SGP4) |
| R17 | One file making both an SPK and a CK | FreeFlyer files with orbit and attitude |
| R18 | Converting the loaded data to a real SPK file | every format (a converter for free) |

R1–R14 and R18 are phases 1–3. R15–R17 are phases 4–5.

## 3. The design

### What a loader makes: SPK segments in a DAF built in memory

A loader does not evaluate states itself. It hands PEPPER arrays (epochs,
states, the interpolation), and PEPPER builds a DAF image in memory with
those segments, laid out exactly as SPKW09 or SPKW13 would write them. The
file's kernel is that image.

Why this rather than providers (virtual segments):
- **Everything already works on DAF handles**: the segment search, the
  evaluators (SPKR09/SPKE09, SPKR13/SPKE13), SPKCOV, SPKOBJ, segment
  identifiers, DAF listings, priority.
- **The data is plain arrays** once loaded: no user code runs during
  queries, so there is nothing new for thread safety.
- **It converts to a real SPK for free** (R18): writing the image to disk
  gives a `.bsp` (`spice%write_kernel`). Any loader is then a converter to
  SPK with no extra code.

Provider segments (R16: the loader supplies the evaluation, e.g. SGP4)
come in a later phase (phase 4), tied to the kernel so that UNLOAD removes
them.

### Registering a loader

```fortran
call spice%add_loader(name, loader, extensions, replace)
call spice%remove_loader(name)
call spice%loader(name, ptr)          ! a pointer to the instance's copy (class(kernel_loader))
```

- `name`: what `furnsh(..., loader=)` and meta-kernels refer to. It is
  case-insensitive (stored in upper case). `PEPPER` is reserved (D3).
- `loader`: any extension of `kernel_loader`. The instance keeps a copy
  (`class(kernel_loader), allocatable`, as it keeps providers), so the
  loader can keep data from its loads (R12).
- `extensions` (optional): file-name endings, case-insensitive, matched
  against the end of the name (`.myeph`, `.oem`, `.oem.xml`). The longest
  match wins.
- An extension or name already registered is an error
  (SPICE(LOADERCONFLICT)) unless `replace = .true.`.
- Removing a loader does not unload its kernels. Their data is PEPPER's.
  Later unload callbacks for them are skipped.

### The loader type

```fortran
type, abstract, public :: kernel_loader
contains
   procedure(kernel_loader_load), deferred :: load       ! (self, ctx, kb, file)
   procedure :: detect => kernel_loader_detect           ! (self, file, head) result(accept): default .false.
   procedure :: unload => kernel_loader_unload           ! (self, file): default nothing
end type kernel_loader
```

- **`load(self, ctx, kb, file)`**: read the file and add its segments to
  the builder `kb`, an empty builder from FURNSH. The loader does its own
  I/O, so binary formats are possible (R2). `ctx` is the instance's
  context, for lookups (`bodn2c`, `str2et`, `unitim`, the pool) and errors.
  Errors are signaled in `ctx` with the loader's own short messages, with
  the file name and line (R13). `self` is `intent(inout)`, so it can keep
  data (R12). FURNSH then loads the builder under the file's name.
- **`detect(self, file, head)`**: optional content sniffing (R1). `head` is
  the first 4 KB of the file, read by PEPPER. It must be cheap and have no
  side effects.
- **`unload(self, file)`**: called when the file's kernels are unloaded
  (UNLOAD, KCLEAR, loading the same file again, the instance destroyed), so
  the loader can free what it kept.

### The builder

`kernel_builder` collects what one kernel defines. It is an ordinary
object that the program (or a loader) owns, and it does not depend on any
instance (D9):
- **Its segments are packed when they are added**, in the DAF layout,
  with the checks that need no instance.
- **The checks that do need one wait for the load.** Frames are kept as
  names and resolved when the builder is loaded, and the body names it
  defines are added then too.
- **Builders can be filled in any thread**, several at once, each its own.
  Only loading one touches the instance.

Nothing reaches the instance until the builder is loaded, and a load is
atomic (R13): on an error (in a loader's `load`, or in `load_kernel`),
nothing of the kernel is registered, as for JSON kernels.

```fortran
type, public :: kernel_builder
   private
   ...
contains
   ! ctx: any context, for the errors (the instance is not used)
   procedure :: spk_lagrange   ! (ctx, body, center, frame, epochs, states, degree, tbeg, tend, ident)  SPK type 9
   procedure :: spk_hermite    ! (ctx, body, center, frame, epochs, states, degree, tbeg, tend, ident)  SPK type 13
   procedure :: set_format     ! e.g. 'MYFORMAT version 3' (shown by kernel_format)
   procedure :: clear          ! empty it, to reuse
   ! phase 3: define_body (R14)
   ! phase 4: spk_provider (R16)
   ! phase 5: ck_quaternions, sclk (R15, R17)
   ! later: spk_chebyshev (types 2/3), spk_elements (type 10)
end type kernel_builder
```

The instance's side:

```fortran
call spice%load_kernel(name, kb)           ! resolve, register under name; kb is emptied (no copy)
call spice%write_kernel(name, file)        ! a loaded kernel to a real SPK file (R18)
```

`load_kernel` moves the builder's packed data into the instance
(`move_alloc`), so a large kernel is never copied.

- **Only SPICE's interpolations** (D8): a segment is one of the SPK types
  the builder has, with the degrees their writers accept: Lagrange (type
  9, degree 1 to SPKW09's maximum) or Hermite (type 13, odd degree, as
  SPKW13). A file that names another method, or a degree these do not
  allow, is an error in the loader. A loader must not substitute a method
  the file did not name; `adapt` is the one exception, and only for short
  segments.
- **Bodies are integer codes, frames are names**, as `add_ephemeris` takes
  them. Mapping a file's names (object, central body, frame, time system)
  is the loader's job (R9), with `ctx` for `bodn2c`, `str2et`, `unitim`,
  and the loader's own settings. An unknown frame is an error at the load.
- **Segments are kept in the order added; later ones win** (R3). Segments
  may share an epoch, where the later one wins (R4), and the gaps between
  segments are gaps in coverage.
- **Bounds inside the data** (R6): `tbeg`/`tend` are the segment's bounds
  and may lie inside the epochs. Both types allow it, so the extra points
  at the ends still serve the interpolation.
- **Short segments** (R7): with `adapt = .true.`, the builder lowers the
  degree of a segment with too few states to what they allow. With two
  states, a Lagrange segment becomes Hermite: a cubic from the
  velocities, better than a line. A segment of one state is dropped,
  since it covers no interval. Without `adapt`, these are the writers'
  errors.
- **The writers' checks, with their short messages**: epochs increasing,
  frame known, body not its own center, the identifier's length and
  characters.
- **Many bodies per file** (R8): segments of any body, center and frame.
- **Epochs** (R10) are ET, computed by the loader. Precision matters for
  relative times (seconds since an origin, with nanoseconds). The builder's
  docs show the pattern, and the test format exercises it: subtract the
  origins in integer arithmetic (`int64` seconds, nanoseconds apart) before
  converting to `real(wp)`.

### Programmatic kernels

A builder loaded by the program, with no file:
- **The name** is any non-blank string. It is what KDATA reports and what
  UNLOAD takes, and it is not looked up on disk. A kernel's name is unique
  among the loaded kernels, files included. Loading under a name in use
  replaces that kernel: it is unloaded first, and the new one gets the
  highest priority, as FURNSH does for a file loaded again.
- **Kernels are immutable** (thread safety: contexts read them with no
  lock). A program whose trajectory grows, a simulation or a filter, keeps
  its builder, adds the new segment or rebuilds the last, and loads it
  again under the same name. `load_kernel(name, kb, keep=.true.)` copies
  instead of moving, so the builder can be extended.
- **KCLEAR removes them**, as it removes programmatic ephemerides. Meta-kernels
  cannot name them, since there is no file.
- **`kernel_format`** answers loader `' '` and the builder's `set_format`
  text, or `(programmatic)` without one.
- **Body names and, later, pool assignments** defined by the builder have
  the kernel's name as their source.

Compared with programmatic ephemerides (`add_ephemeris`, a provider):

| | Programmatic kernel (builder) | Programmatic ephemeris (provider) |
|---|---|---|
| The program gives | states at epochs | a procedure |
| Interpolation | PEPPER's (SPK types 9, 13) | the program's own, or a model |
| Queries call user code | no | yes, from every thread |
| KDATA, SPKCOV, UNLOAD, `write_kernel` | yes | no (until phase 4) |
| For | trajectories already computed: simulations, other tools, files | analytic motion, models, live data |

Phase 4 (provider segments owned by a kernel) brings the two together.
`add_ephemeris` could then become a one-segment kernel with a provider
segment, and gain KDATA, SPKCOV and UNLOAD; that is a decision for then.

**PepperyPy and the C interface.** The builder takes plain arrays, so it
maps directly onto NumPy. A Python program can make an in-memory SPK from
its arrays with no file and no callback, e.g.
`spice.load_kernel('traj', [spk_hermite(...)])`.

### FURNSH: which loader reads a file

`spice%furnsh(file, in_memory, loader)` gets a new optional `loader`.
Resolution order (R1):

1. **`loader=` given**: the registered loader with that name reads the
   file, overriding everything else: no check that the file exists, of
   its type (even a DAF), its extension, or detection; the loader reports
   its own errors. `loader='PEPPER'` loads the file as without loaders
   (steps 2 and 5; registered loaders not asked). Any other name no loader
   has: SPICE(UNKNOWNLOADER).
2. **A DAF or DAS file** (GETFAT's ID word): the built-in loaders. A real
   SPICE binary file is never handed to a registered loader by extension or
   detection; its ID word is unambiguous.
3. **A registered extension**, the longest match.
4. **Detection**: the loaders' `detect`, the most recently registered
   first. The first that accepts reads the file.
5. **The built-in text readers**: a `*.json` file as a JSON kernel, else a
   text kernel or meta-kernel (as now).

`in_memory` does not apply: a loader's kernels are always in memory.

**Meta-kernels.** Steps 2–5 apply to each file of `KERNELS_TO_LOAD`, so
loaders registered with extensions or detection work in meta-kernels and
through the legacy `FURNSH` (and PepperyPy) unchanged. For an explicit
loader there is a new meta-kernel variable (D2):

```
\begindata
KERNELS_TO_LOAD = ( '$K/naif0012.tls', '$K/sc_ephem.txt' )
KERNEL_LOADERS  = ( '$K/sc_ephem.txt', 'MYFORMAT' )
```

`KERNEL_LOADERS` holds pairs (file as written in `KERNELS_TO_LOAD`, loader
name). A loader named there but not registered is an error at that file,
not a silent fallback.

### The kernel table, KDATA, UNLOAD

- **Entries.** Each DAF a file produces gets one `keeper_kernel` entry, all
  with the file's name: an SPK, and with phase 5 also a CK (R17).
  `keeper_kernel` gains `loader` (the loader's name).
- **KDATA and KINFO** report the type `SPK` (or `CK`), so KTOTAL('SPK'),
  lists and filters include them. Their handle is the image's handle.
- **`kernel_format(file, loader, format)`**, a new query: the loader's name
  and the description from `set_format`. It answers `' '` for SPICE's own
  kernels.
- **UNLOAD** of the file removes all its entries and images and calls the
  loader's `unload`. KCLEAR does the same for all. Loading the same file
  again unloads it first, as FURNSH does for binary kernels.
- **Body names** (R14, phase 3): `kb%define_body(name, code)` adds a
  name/code pair owned by the kernel (in BODDEF's table, with the kernel as
  its source), removed by UNLOAD.
- **Pool assignments** (phase 5): their source is the file, so
  `pool_sources`/`pool_defined_by(file)` list them. They are stored with
  the kernel and replayed when the keeper rebuilds the pool after a text
  kernel is unloaded.

### DAF images without a file

The core of phase 1. Today an in-memory DAF is still a file. It is opened
by name, and its handle-manager entry has a unit and is found by name. A
built image has no file on disk, so its `ddh_file` entry gets a flag
(`synthetic`), its file record in the image, the loaded file's name as its
name, a unique `mnm`, and `method = READ`. The audit, every routine that
may open, inquire or read by name or unit:
- ZZDDHHLU (the unit of a handle), ZZDDHCLS (closing), ZZDDHF2H and
  ZZDDHOPN (finding a file by name);
- GETFAT;
- the file record reads (DAFRFR, ZZDAFGFR) and summary/name records
  (ZZDAFGSR);
- DAFOPR, DAFCLS;
- `file_closed`;
- the contexts' lazy unit tables.

The routines that take a file name, SPKCOV and SPKOBJ (DAFOPR on a loaded
file returns its handle), must find a synthetic entry by its name, without
looking at the disk.

The segment data is built by the same code as the writers. The packing of
SPKW09/SPKW13 (records, epoch directory every 100 epochs, the trailer) is
moved into pure routines that both the writers and the builder call, so
the layouts cannot drift. A test checks that the image of a segment is
bitwise the data SPKW09/SPKW13 writes.

### Thread safety

- Loaders run only inside `furnsh`, `unload`, `kclear` and `destroy`, on
  the instance, while no queries run (the rule of the instance).
- Builders are independent objects: any thread may fill its own while
  others query. `load_kernel`, like `furnsh`, is an instance method and
  runs while no queries run.
- After loading, the images are read-only arrays, read by every context
  like any in-memory DAF.
- A loader's kept data may be read from contexts only through `intent(in)`
  access. It changes only in `load`/`unload`.
- Provider segments (phase 4) follow the provider rules.

### Errors

New short messages:
- SPICE(UNKNOWNLOADER): `loader=` or `KERNEL_LOADERS` names no registered
  loader;
- SPICE(LOADERCONFLICT): a name or extension registered twice without
  `replace`.

- SPICE(RESERVEDLOADERNAME): `add_loader` with the name PEPPER (FURNSH's
  name for PEPPER's own loading).

Each loader has its own, documented in its library. The builder's are those
of the SPK writers.

## 4. Decisions

- **D1. Loaders make DAF data, not providers** (above). Providers come
  later, for formats that need their own evaluation.
- **D2. Explicit loaders in meta-kernels**: the `KERNEL_LOADERS` pairs. The
  alternative is a prefix inside `KERNELS_TO_LOAD` (`'MYFORMAT:file'`),
  which clashes with Windows drive letters and with existing names. Or
  none at all, relying on extensions and detection.
- **D3. Reserved names** for the built-in loaders. As built (2026-11-15):
  one name, `PEPPER`, PEPPER's own loading (as without loaders: SPICE's
  kernels by their ID word, JSON kernels by their extension, text kernels
  and meta-kernels), not registrable. It exists for a SPICE text kernel or
  meta-kernel whose extension a registered loader also has.
- **D4. SPICE binary files are never claimed** by registered extensions or
  detection (resolution step 2).
- **D5. The loader object is copied into the instance**, so it can keep
  data, and its settings at registration are what it uses. To change them,
  register again with `replace`, or change them through `spice%loader(name,
  ptr)`.
- **D6. No real format in PEPPER.** Loaders for real formats are separate
  libraries that depend on PEPPER. PEPPER's tests have their own made-up
  format (section 5), so the mechanism is tested without any of them.
- **D7. Names, frames and times are resolved at load.** A leap-seconds
  kernel or a body name loaded after the file does not change it (as `@`
  times in text kernels). Loaders' errors should name what is missing
  ("load a leap-seconds kernel first", "map the object 'X' to a NAIF ID").
- **D8. The interpolation is the file's, and one PEPPER supports.** A
  loader maps the file's method to a builder segment type, or signals an
  error (e.g. SPICE(UNSUPPORTEDINTERP), naming the method and the file). It
  never approximates one method by another. That way, states from a loaded
  file are the file's own interpolation or nothing. More methods (a spline,
  for FreeFlyer) are added to PEPPER later as new segment types. A type
  that is not SPICE's would make `write_kernel` files readable only by
  PEPPER; that is a decision for when one is added.
- **D9. The builder does not depend on an instance.** Building a kernel
  and loading it are separate: building is the costly part (packing), and
  it can run in any thread, for programmatic kernels and loaders alike.
  Only `load_kernel` (and FURNSH) touch the instance. The cost: errors that
  need the instance, such as an unknown frame, come at the load rather
  than when the segment is added.

## 5. The test format

A made-up format, the "PEPPER test trajectory", read by a loader in the
tests (`test/core/pepper_test_loader.f90`). It is small, but each feature
stands for something a real format has, so that a loader for FreeFlyer, OEM
or SP3 needs nothing the tests have not covered. It is not a format for
users, and it is not documented outside the tests.

### Text form (`.ptrj`)

```
PEPPER_TEST_TRAJECTORY 2                      # signature + version (detection, R1, R11)
# comments anywhere after the first line
OBJECT      = SC_ONE                          # a name: mapped by the loader or bodn2c (R9, R14)
OBJECT_CODE = -10001                          # optional: the code; with it, define_body (R14)
CENTER      = EARTH
FRAME       = EME2000                         # mapped by the loader's table: EME2000 -> J2000 (R9)
TIME_SYSTEM = TAI                             # UTC, TAI, TT, TDB, GPS (R10)
ORIGIN      = 2492596837 000000000            # v2: integer seconds + nanoseconds, TAI, since JD 2430000.0 (R10)
INTERPOLATION = LAGRANGE 8                    # LAGRANGE n or HERMITE n (R5)
BOUNDS      = 30 0 43140 0                    # optional: segment bounds inside the data (R6)
UNITS       = km                              # km or m (converted)
BEGIN_DATA
0 000000000   -6648.08  160.991  ...  0.481415 | mass=500.0    # v2: seconds nanoseconds, 6 values, extras (R12)
30 000000000  ...
3600 000000000  948.3|948.3  398.5|398.5  ... -2.1|-2.4  ...  # a discontinuity: before|after (R3, R4)
...
GAP                                           # the next row starts a new segment after a gap (R4)
...
END_DATA
OBJECT = SC_TWO                               # a second object in the same file (R8)
...
```

- **Version 1** is the other layout (R11): absolute epochs as UTC calendar
  strings (`2020-01-01T00:00:00.000`, through `str2et`), no `ORIGIN`, no
  extras. One loader reads both versions, as a FreeFlyer loader must read
  v1–v3 and an OEM loader v1–v3, KVN and XML.
- **Extras** after `|` (here a mass) have no place in an SPK. The loader
  keeps them per object and epoch (R12). The test reads them back through
  `spice%loader('PTRJ', ptr)` and checks that `unload` freed them.
- **Short arcs** between close discontinuities test `adapt` (R7).
- **Unsupported interpolation** (D8): `INTERPOLATION = SPLINE 5`, and
  `HERMITE 6` (an even degree), are errors.
- **Errors** (R13) come from deliberately broken files: a bad signature, an
  unknown version, an unknown object or frame, a missing LSK for UTC,
  epochs out of order, a short line, a bad number. Each must name the file
  and line, and leave nothing loaded.

### Binary form (`.ptrb`)

The same content as version 2, in a stream-access file: a signature, the
header as fixed fields, then the rows as `int64` seconds, `int32`
nanoseconds and six `real64`. It tests a loader doing its own binary I/O
(R2) and detection on bytes rather than text.

### What the tests check

- **States.** At the rows, equal to the file's values. Between them,
  bitwise equal to an SPK written by SPKW09/SPKW13 from the same data. The
  test writes both the `.ptrj` file and the reference SPK from one
  trajectory sampled from a test kernel.
- **Discontinuities and gaps** (R3, R4): the state just before, at and
  just after a discontinuity epoch (the later segment at it); SPKCOV
  giving one interval across a discontinuity and two across a gap; a query
  in the gap failing with SPICE's usual error.
- **Bounds** (R6): coverage equal to `BOUNDS`; the extra rows used by the
  interpolation near the bounds.
- **Short arcs** (R7): a two-row arc as Hermite, a one-row arc dropped,
  and with `adapt = .false.`, the writers' errors.
- **Several objects** (R8), each with its own center and frame.
- **Times** (R10): the same trajectory with epochs in UTC (v1), TAI and
  GPS (v2) gives the same ET. Relative epochs keep the nanoseconds: the
  same file with origins one second apart gives states shifted by exactly
  one second.
- **Names** (R14): `spkezr('SC_ONE', ...)` works while the file is loaded
  and not after UNLOAD.
- **Resolution** (R1): by `loader=`, by extension, by detection with an
  unrelated extension (`.txt`), `loader='PEPPER'` keeping the loaders off,
  a DAF never claimed, conflicts and replacement; in meta-kernels
  (extensions and `KERNEL_LOADERS`) and through the legacy FURNSH.
- **Kernel table**: KDATA/KINFO type `SPK`, `kernel_format`, KTOTAL,
  UNLOAD and KCLEAR (with the unload callbacks), loading the same file
  twice, priority against real SPKs loaded before and after.
- **Conversion** (R18): `write_kernel` gives an SPK that, loaded on its
  own, gives the same states bitwise.
- **Threads**: `test_parallel` queries a loaded test-format kernel from
  every thread, many runs.
- **Later phases**: `ATTITUDE` blocks (quaternion rows) making a CK and its
  SCLK in the same file (R15, R17), and a `MODEL = CIRCULAR r period`
  object making a provider segment (R16).

## 6. Phases

Each phase with its tests (`pixi run test`, `test-parallel` many times,
`tspice`), docs (fdoc, README, `pepper_vs_spicelib.md`) and a commit.

### Phase 1: SPKs built in memory

- The synthetic DAF entry and the audit above.
- The packing of SPK types 9 and 13 factored out of SPKW09/SPKW13.
- The builder and programmatic kernels, the first user-visible feature:

  ```fortran
  type(kernel_builder) :: kb
  call kb%spk_hermite(ctx, -10001, 399, 'J2000', epochs, states, 7, tbeg, tend, 'MY TRAJECTORY')
  call spice%load_kernel('my_trajectory', kb)            ! a kernel named 'my_trajectory'
  call spice%write_kernel('my_trajectory', 'my_trajectory.bsp')   ! optionally, a real SPK
  ```

- KDATA/KINFO/UNLOAD/KCLEAR/SPKCOV/SPKOBJ on it; replacing by name;
  `keep`; `adapt` for short segments.
- Tests, on the builder directly:
  - states, bounds, short segments, several bodies, conversion, priority;
  - builders filled in parallel threads while other threads query, then
    loaded;
  - one builder loaded (with `keep`) into two instances;
  - replacing a kernel by name (the new data, the highest priority);
  - a name used by a loaded file;
  - an unknown frame found at the load, leaving nothing loaded.

### Phase 2: the loader registry

- `kernel_loader`, `add_loader`/`remove_loader`/`loader`.
- FURNSH's `loader=` and the resolution order, `KERNEL_LOADERS`.
- The `unload` callback, `kernel_format`.
- The test format's loader (text form, versions 1 and 2) and the tests of
  section 5, except the later phases.

### Phase 3: names and binary files

- `kb%define_body` (R14).
- The test format's binary form (R2).

### Phase 4: provider segments (R16)

- `kb%spk_provider(body, center, frame, tbeg, tend, provider)`. It
  generalizes the virtual SPK files of `pepper_programmatic` to segments
  owned by a kernel: in the keeper's table, removed by UNLOAD. SPKCOV and
  SPKOBJ learn virtual handles.
- For formats whose states come from a model (OMM with SGP4). Not for
  interpolating a table another way: new interpolation methods are added
  to PEPPER itself (D8).

### Phase 5: attitude and the pool (R15, R17)

- `kb%ck_quaternions` (CK type 3 in memory), and `kb%sclk`: a simple
  linear SCLK owned by the kernel, for formats whose epochs are not
  spacecraft clock times (or an existing SCLK kernel).
- One file making an SPK and a CK.
- Pool assignments from loaders (frame definitions, other variables),
  replayed when the pool is rebuilt.

### Later

- PepperyPy and the C interface: **done for programmatic kernels**
  (2026-11-15): `pepper_builder_*` and `pepper_spice_load_kernel` in C
  (`src/capi/pepper_capi_core.f90`), `KernelBuilder` and
  `Spice.load_kernel` in PepperyPy, from NumPy arrays; `write_kernel` and
  `kernel_format` are generated. Kernel loaders are not registered from C
  or Python (they are Fortran types; `remove_loader` is not wrapped
  either), so FURNSH's `loader` argument is not in the C interface.
- **Done (2026-11-15): kernel loaders in the C interface and PepperyPy.**
  C: `pepper_spice_add_loader` (load, detect and unload functions with
  user data; `capi_loader`, which gives the C load function a context of the
  instance made for the call and copies its errors), `pepper_spice_remove_loader`,
  `pepper_loader_names`, `pepper_spice_furnsh_loader`; the builder's
  `pepper_builder_add_ephemeris`, `_add_frame`, `_frame_rotations`.
  PepperyPy: `KernelLoader`, `Spice.add_loader`, `remove_loader`, `loaders`,
  `furnsh(loader=)`; `KernelBuilder.add_ephemeris`, `add_frame`,
  `frame_rotations`. A Fortran loader library is registered from C by a
  function it exports (taking the instance's handle, `spice_of`), if it is
  linked into the same program as PEPPER; not on a PepperyPy instance,
  whose PEPPER is inside its extension module (that needs PEPPER as a shared
  library, or the loader built into PepperyPy). The original to-do:
  - FURNSH's `loader` argument in `pepper_spice_furnsh` and PepperyPy's
    `furnsh(path, loader=...)` (and `KERNEL_LOADERS` already works through
    them);
  - registering loaders: a loader written in C (a function that fills a
    builder through `pepper_builder_*`, with optional `detect` and
    `unload` functions and user data, as the programmatic providers take
    function pointers) and in Python (a class with `load(ctx, kb, file)`,
    `detect`, `unload`, called through those C callbacks, under the GIL);
  - Fortran loader libraries (FreeFlyer, OEM) usable from C and Python: an
    entry point each library exports to register its loader on an
    instance handle (e.g. `pepper_freeflyer_register(spice)`), so a Python
    program can use them without writing a loader;
  - `remove_loader`, a list of the registered loaders, and their kept data
    (through the loader's own API).
- More builder segment types: 2/3 (Chebyshev, for compact fits), 8/12
  (equally spaced), 10 (two-line elements), 18/19, 21.

## 7. Size

PEPPER, phases 1–3: the synthetic DAF and its audit are most of the work.
With the builder, the registry, FURNSH, the test loader and the tests,
probably 2,500–3,500 lines, smaller than the in-memory DAF work.

## 8. How the planned loaders use it

| Format | Library | Uses |
|---|---|---|
| **FreeFlyer ephemeris** (v1–v3) | its own (`freeflyer_loader_plan.md`) | extension `.FFephem`/`.ephem` and detection; versions (R11); discontinuities as segments sharing an epoch (R3, R4); `adapt` (R7); Lagrange and Hermite (R5); StartTime/StopTime as bounds (R6); TAI seconds + nanoseconds since an origin (R10); `define_body` for `Spacecraft` (R14); mass and comments kept (R12); attitude columns with phase 5 (R15, R17) |
| **CCSDS OEM** (KVN, XML) | PEPPER + ndm-fortran | detection (`CCSDS_OEM_VERS`, `<oem`/`<ndm`); segments as type 13 or 9 (R3, R5); useable times as bounds (R6); `TIME_SYSTEM` (R10); `REF_FRAME`, `CENTER_NAME` (R9); `OBJECT_NAME` (R14); covariances kept (R12); versions and KVN/XML (R11) |
| **CCSDS OMM** | same | provider segments with SGP4 (R16), or SPK type 10 |
| **CCSDS AEM** | same | CK and SCLK (R15) |
| **STK ephemeris** (`.e`) | its own | `EphemerisTimePosVel`, Lagrange/Hermite (R5), `ScenarioEpoch` + offsets (R10), several objects (R8) |
| **SP3** (GNSS) | its own | many satellites per file (R8), GPS time (R10); positions only, so Chebyshev fits (types 2/3) or computed velocities |
| **Binary formats** (Code-500, ...) | their own | the loader's own I/O (R2) |
