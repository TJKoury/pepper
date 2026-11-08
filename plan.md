

PEPPER: Planetary Ephemerides, Positions, Pointing, Events & Rotations. A modern Fortran reimagining of SPICELIB.

This is the plan to convert SPICELIB into PEPPER:

1. **(done)** First, we need to combine the spicelib code that NAIF releases in code archives for different platforms into one single codebase. Note: all we care about is src/spicelib, not the other tools in the archives. This will organize all the code in a unified structure, making it easier to manage and maintain. Platform-specific files will be placed in separate directories to keep the code modular and maintainable.
   *Result:* `tools/import_naif.py`; platform variants merged into `.F` files selected by `SPICE_OS_*` / `SPICE_COMPILER_*` macros set in `fpm.toml`. The tspice test suite was imported the same way and runs in parallel (`pixi run tspice`).
2. **(done)** convert all the code from fixed-form to free-form Fortran.
   *Result:* `tools/fixed2free.py` (verified by identical object files). Follow-up cleanups: NAIF disclaimer moved to `LICENSE`; `src/` code lowercased (`tools/lowercase_code.py`); the only Hollerith removed.
3. Refactor the code to adopt modern Fortran practices, such as using modules, explicit interfaces, and allocatable arrays. This will improve code readability, maintainability, and performance. **The code must also become thread-safe** so that ephemeris, frame and time computations can be called in parallel. See the detailed plan below.

At each stage, we must test the code to make sure we haven't broken anything. To do this we will use the NAIF test suite (tspice).

We will test on gfortran on mac, using FPM and a pixi environment. But we want the code to be portable and work across different platforms and compilers as well. The only platforms to consider are Windows (x64), Linux (x64), and Mac (arm64). The only compilers to consider are gfortran and intel (ifx).

we want to start with the latest SPICELIB relase (N0067)

### links:

spicelib archives for each platform/compiler: https://naif.jpl.nasa.gov/naif/toolkit_FORTRAN.html
tspice: https://naif.jpl.nasa.gov/pub/naif/misc/tspice/N0067/

---

## Step 3 in detail: modernization and thread safety

### Goals

- Modern Fortran (2018): every procedure in a module (submodules for large implementations), explicit interfaces, `intent`, `implicit none`, no `entry`, no `save`d mutable state, no implicit `save` (initialized declarations, `data`).
- **Thread safety:** many threads can compute states (SPK), orientations (PCK/CK/frames) and time conversions (UTC/ET/SCLK) at the same time.
- **Multiple instances:** independent sets of loaded kernels can exist in one program (e.g. different ephemeris versions side by side).
- tspice keeps passing at every step (`pixi run tspice`), and every step is committable on its own.

### Where the state is today (N0067 survey)

| | count |
|---|---|
| source files / procedures + entry points | 1415 / 1733 |
| files with `save` (79 of them a bare `save` = everything) | 327 |
| files with `data` statements (implicit `save`) | 309 |
| umbrella routines / `entry` points | 76 / 318 |
| `common` blocks | 0 |
| files calling `chkin` (error traceback) / `sigerr` (signal error) | 1017 / 796 |
| files with SPICE cells (`(lbcell:*)` arrays) / assumed-size dummies | 186 / 597 |
| procedures called directly by tspice (of which internal `zz*`) | 1160 (291) |

The `save`d state falls into a few kinds:

1. **Constants computed on first call** (`first` flag, e.g. `pi()`, `dpr()`, conversion tables): about 120 files. Trivial to fix: they become module `parameter`s.
2. **Memo caches** that avoid repeated lookups. Examples: name-to-ID translation guarded by pool-change counters (`svctr*`, `svtarg`, `svobs`), and the last parsed aberration-correction string (`prvcor`, `uselt`, ...). These are harmless serially, but they are races in parallel.
3. **Real shared data** owned by a subsystem through an umbrella routine:

   | subsystem | umbrella / key routines |
   |---|---|
   | error handling | `trcpkg` (traceback), `seterr`/`failed`, `putmsg`/`getmsg`, `erract`/`errdev` |
   | kernel pool | `pool` (21 entries) |
   | kernel management | `keeper` (`furnsh`/`unload`) |
   | file layer | `zzddhman` (handle manager), `dafah`, `dasfm`, record buffers (`dafrwd`, `dasrwr`) |
   | segment selection buffers | `spkbsr`, `ckbsr`, `pckbsr`, `zzdskbsr` |
   | frames and bodies | `framex`, `zzbodtrn`, dynamic frames |
   | time and SCLK | `zztime`, `sc01` |
   | search and query subsystems | GF search state passed to callbacks (`zzgf*`), EK query manager (`ekqmgr`, `zzekpage`) |
   | Fortran unit bookkeeping | `fndlun`, `reslun` |

### Concurrency model (proposal)

Split the state in two:

- **`type(pepper_kernels)` (shared, read-mostly):** everything that loading kernels creates.
  - The kernel registry (`keeper`), the kernel pool, open-file table and DAF/DAS file summaries, segment directories, and body/frame/SCLK definitions from kernels.
  - It changes only through load/unload/pool-write calls, and every change bumps a **generation counter**.
- **`type(pepper_context)` (one per thread, cheap):** a pointer to a `pepper_kernels` plus all per-call mutable state.
  - Error status, message and traceback; all memo caches, which are valid while their stored generation matches the kernel set's; segment-selection buffers; DAF/DAS record caches; scratch space; GF search state.

How it's used: load kernels into a `pepper_kernels`, then create one `pepper_context` per thread (e.g. inside an OpenMP parallel region) attached to it. A context is never shared between threads. Several `pepper_kernels` objects can coexist.

**Contract (phase 1 of this design):** queries from any number of contexts may run concurrently, but loading, unloading or pool writes on a kernel set must not run while that set is being queried. This needs no locks, so the library itself needs neither OpenMP nor pthreads. A locked or copy-on-write kernel set can be added later if concurrent loading is ever needed.

The pool's watcher mechanism (`swpool`/`cvpool`) and the `zzctr` counters are replaced by the generation counter. Each cache stores the generation it was built at.

**File I/O:** SPK/CK/PCK/DSK data is read from DAF files at query time, so concurrent queries read the same files. All file access goes through one internal module (`pepper_daf_io`), so the strategy can change without touching callers:

- (a) **Shared units:** direct-access `read(unit, rec=)` on shared units, relying on the runtime's per-unit locking (gfortran does this; ifx needs `-reentrancy threaded`). Simplest, but the per-unit lock serializes all reads of a file.
- (b) **Positional reads:** `pread` through `iso_c_binding`. Truly concurrent, no Fortran I/O.
- (c) **In memory:** whole segments or files read into memory at load time (memory cost; DE440 is about 115 MB).

Start with (a); measure; move to (b) or (c) if contention shows. Units come from `newunit=` (`fndlun`/`reslun` go away).

### Error handling

SPICELIB's error system is global and touched everywhere: `sigerr`/`failed`/`return`/`chkin`/`chkout`, actions `ABORT`/`RETURN`/`REPORT`, and an output device. It moves into the context:

- Each routine that can signal an error takes the context as its first argument. The rewrite is mechanical: `if (return()) return` → `if (ctx%failed()) return`, `call chkin('X')` → `call ctx%chkin('X')`, and so on.
- The traceback stays per context, so error messages keep the call chain. It could later be made optional for speed.
- The default action for the new API should be `RETURN` (the caller checks `ctx%failed()` and gets the message), not `ABORT`. A library must not stop a multi-threaded program. The legacy layer keeps SPICELIB's defaults.
- Purely numerical routines that never signal errors (most vector/matrix math) do not get a context and become `pure`/`elemental` where possible.

### Module structure

One module per subsystem, plus a `pepper` module that re-exports the public API. Large implementations go in submodules (interface in the module, code in submodules) to keep recompilation cascades small. Proposed modules:

- **Basics:** `pepper_kinds`, `pepper_constants`, `pepper_math` (vectors/matrices/quaternions/conics; pure), `pepper_strings`, `pepper_search_sort`, `pepper_cells` (cells/sets/windows).
- **Infrastructure:** `pepper_errors`, `pepper_context` (`pepper_kernels`, `pepper_context`), `pepper_pool`, `pepper_kernel_mgr`, `pepper_daf_io`, `pepper_daf`, `pepper_das`, `pepper_dla`.
- **Ephemeris, orientation and time:** `pepper_spk`, `pepper_pck`, `pepper_ck`, `pepper_frames`, `pepper_bodies`, `pepper_time`, `pepper_sclk`.
- **Geometry and search:** `pepper_geometry` (illumination, occultation, limbs, ...), `pepper_dsk`, `pepper_gf`, `pepper_ek`.

Each umbrella routine becomes a module (or part of one):

- its `save`d variables become components of a state type held in `pepper_kernels` or `pepper_context`;
- each `entry` becomes a module procedure;
- the `data` initializations become component default values or `parameter`s.

**API shape (proposal):** plain module procedures with the context as the first argument, `call spkezr(ctx, targ, et, ref, abcorr, obs, state, lt)`, keeping SPICELIB's names and argument order. Making all ~600 public routines type-bound would bloat the type. A few top-level convenience type-bound procedures (`ctx%furnsh`, `ctx%spkezr`, `ctx%str2et`, `ctx%pxform`, ...) can be added on top. Descriptive aliases can come later.

**Callbacks (GF, user-defined functions):** today their state passes through `save`d umbrellas, because the callback signatures are fixed. In the new design, abstract interfaces take the context plus a user-data argument (`class(*)`, or a type the user extends), so state flows through arguments.

### Legacy compatibility layer

tspice calls 1160 library procedures directly, 291 of them internal (`zz*`) routines and entry points. Rewriting all those tests up front is not practical. So:

- `src/legacy/` holds external procedures with SPICELIB's original names and F77 signatures. They forward to the module procedures using a global **default context and kernel set**.
- They are generated by a script from the module interfaces wherever the mapping is mechanical.
- The layer is explicitly **not** thread-safe, like the original SPICELIB.
- This lets the migration go subsystem by subsystem: once a subsystem moves into modules, its legacy wrappers replace the old externals and tspice keeps testing it unchanged.
- The same layer serves existing SPICELIB users.
- New tests are written against the module API. tspice families can be ported to it over time, after which their wrappers for internal routines can be dropped.

### Compiler and build requirements for reentrancy

- All procedures must be reentrant. gfortran places local arrays larger than `-fmax-stack-var-size` in static memory unless `-frecursive` (or `-fopenmp`) is given; ifx needs `-recursive` / `-reentrancy threaded`. Add these to the `platform` feature in `fpm.toml` now, and mark procedures `recursive` as they are modularized. (gfortran does not yet make procedures recursive by default as Fortran 2018 specifies.)
- Thread stacks are small (`OMP_STACKSIZE`), and SPICELIB has large local work arrays (e.g. buffers sized for 5000 segments). Make large locals `allocatable` or move them into the context.
- Remove all implicit `save`: initialized local declarations and `data`. A script check in CI should reject new ones.

### Arguments that will break with explicit interfaces

With modules, `-fallow-argument-mismatch` stops helping: mismatches become compile errors. Known patterns:

- **SPICE cells:** arrays declared `(lbcell:*)` with 6 control elements (186 files).
- **Packed DAF summaries:** integer and double data packed into one double array, including `equivalence` in 5 files.
- **Workspace arrays** passed with a different rank or type than declared.
- **Scalars passed where arrays are expected.**

Plan: keep assumed-size dummies where needed at first (`(lbcell:*)` works with explicit interfaces when callers pass arrays), catalogue the remaining mismatches (compile without `-fallow-argument-mismatch`), and fix call sites per subsystem. Typed cell/window derived types can come later as a new API. Removing `-fallow-argument-mismatch` is a milestone.

### Testing

- **tspice** (through the legacy layer) after every change: `pixi run tspice`.
- **Unit tests** for the module API (fpm tests) as each subsystem is converted.
- **Concurrency tests** (test-only OpenMP):
  - N threads, each with its own context on a shared kernel set, compute `spkezr`, `pxform`/`sxform`, `str2et`/`et2utc` and `sce2c`/`scs2e` over many epochs. Results must be **bitwise identical** to a serial run.
  - Also: several kernel sets used concurrently.
  - Kernels come from tspice's generators (`tstspk`, `tstpck`, `tstlsk`, ...), so no downloads are needed; a real DE kernel is optional, for benchmarks.
- **ThreadSanitizer** (`-fsanitize=thread`, gfortran on Linux) over the concurrency tests to catch remaining shared state.
- **State inventory check:** a script lists every `save`/`data`/initialized-declaration/`entry` in `src/`. The count must reach zero outside `parameter`s, and CI keeps it there.
- **CI:** gfortran and ifx on Linux x64, macOS arm64 and Windows x64.

### Phases

Each phase ends with tspice green, the concurrency tests that apply passing, and a commit.

0. **Preparation:**
   - the state-inventory script (per routine: `save`/`data`/`entry`, classification, call graph) to drive and track the work;
   - reentrancy flags in `fpm.toml`;
   - the CI matrix;
   - answers to the open questions below.
1. **Stateless core into modules:** constants (first-call `save`s become `parameter`s), vector/matrix math, strings, search/sort, cells/sets/windows logic. Mark `pure`/`elemental`; generate the first legacy wrappers. Low risk, and it builds the module, wrapper and test machinery.
2. **Context and errors:** `pepper_context`, the error/traceback system in the context, the default context for the legacy layer. Establish the mechanical rewrite pattern (`ctx` argument, `ctx%failed()`, ...) used in all later phases.
3. **Kernel set:** `pepper_kernels`, kernel pool, kernel management (`furnsh`/`unload`/`kclear`), body and frame name tables, the generation counter in place of watchers and `zzctr`.
4. **File layer:** the DAF/DAS/DLA handle manager and record caches (per context), `pepper_daf_io` with strategy (a), `newunit`.
5. **Ephemeris, orientation, time (the main use case):** SPK, PCK, CK, frames (including dynamic frames), time (`str2et`/`et2utc`/`deltet`/`unitim`/`timout`), SCLK.
   - **Milestone:** parallel ephemeris and time tests pass, including under ThreadSanitizer.
6. **Geometry, DSK, GF, EK:** derived geometry, DSK, GF (callback redesign), EK. EK is large (189 files) and probably the least used, so it could come last or be deprioritized.
7. **Cleanup:**
   - remove `-fallow-argument-mismatch`;
   - drop internals only the legacy layer needs;
   - reduce the platform-specific code (`zzplatfm`, I/O quirks) now that `newunit` and standard I/O are used;
   - convert NAIF header comments to FORD documentation;
   - benchmarks: serial vs parallel, and against the original SPICELIB.

### Risks

- **Size:** about 1400 routines, so the transformations must be scripted and verified (object-file comparison where the change is purely mechanical; tspice otherwise), and reviewable in subsystem-sized commits.
- **Numerical fidelity:** moving code into modules can change inlining and therefore round-off. The concurrency tests compare bitwise against serial runs of the *same* build; tspice uses tolerances.
- **Performance:** per-context caches start cold in every thread; I/O contention on shared files (see the File I/O options above).
- **Compiler differences:** Fortran I/O thread safety and reentrancy flags differ between gfortran and ifx, so both must be tested.

### Open questions for review

1. **API style:** `call spkezr(ctx, ...)` with module procedures (recommended), or type-bound `ctx%spkezr(...)` throughout?
2. **Names:** keep SPICELIB names in the new API (recommended for now), or introduce descriptive names?
3. **Errors in the new API:** context status checked with `ctx%failed()` and default action `RETURN` (recommended)? Also optional `stat`/`errmsg` arguments on public routines?
4. **Concurrency contract:** is "load first, then query concurrently" enough, or must loading/unloading run concurrently with queries?
5. **Dependencies:** library free of OpenMP, using it only in tests (recommended)?
6. **Scope and priority:** are EK and GF needed early, or can they come after the ephemeris/time milestone?
7. **Minimum compiler versions:** e.g. gfortran ≥ 13, ifx 2024+.
8. **Kinds:** keep default `integer` (32-bit) and `double precision` as `real64`, or move to explicit kinds everywhere (`int32`/`real64`)?
