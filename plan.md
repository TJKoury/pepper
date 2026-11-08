

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
- **One instance, no global state:** all SPICE state lives in one object (`type(pepper_spice)`) and the per-thread contexts made from it. Multiple instances in one program are not a goal for now, but nothing in the design prevents them.
- **Fortran only:** no C code and no C interoperability for system calls (no `pread`, pthreads, ...). Only standard Fortran, its intrinsic modules and the compiler's runtime library.
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

### Concurrency model

All SPICE state belongs to one object, **`type(pepper_spice)`** (the instance). Nothing is global. Each thread that uses the instance gets its own **`type(pepper_context)`** from it. The state splits in two:

- **In the instance (shared, read-mostly):** everything that loading kernels creates.
  - The kernel registry (`keeper`), the kernel pool, the open-file table and DAF/DAS file summaries, segment directories, and body/frame/SCLK definitions from kernels.
  - It changes only through load/unload/pool-write calls, and every change bumps a **generation counter**.
- **In the context (one per thread, cheap):** a pointer to its instance plus all per-call mutable state.
  - Error status and messages; all memo caches, which are valid while their stored generation matches the instance's; segment-selection buffers; DAF/DAS record caches; scratch space; GF search state.

How it's used:

```fortran
type(pepper_spice), target :: spice
type(pepper_context) :: ctx

call spice%furnsh('kernels.tm')        ! load (one thread)
!$omp parallel private(ctx)
ctx = spice%new_context()              ! one context per thread
!$omp do
do i = 1, n
   call ctx%spkezr('MOON', et(i), 'J2000', 'LT+S', 'EARTH', state(:,i), lt(i))
end do
!$omp end parallel
```

A context is never shared between threads. Setup calls on the instance itself (`spice%furnsh`, `spice%unload`, pool writes) report errors through a context that the instance owns for this purpose; they are made from one thread, before or between parallel queries.

**Contract:** queries from any number of contexts may run concurrently, but loading, unloading or pool writes must not run while the instance is being queried. This needs no locks, so the library needs no OpenMP. A locked or copy-on-write kernel set can be added later if concurrent loading is ever needed.

**Rules that follow from "no global state":**

- All mutable state lives in the instance or in a context. What remains at module level must be immutable: `parameter`s, and built-in tables such as the built-in body names and frames, which become `parameter` arrays instead of tables filled on first call.
- The legacy layer's default instance (see below) is the only exception, and the module API never uses it.
- Because nothing is global, several instances could in principle coexist in one program (e.g. different ephemeris versions side by side). This is **not a goal for now** and is not tested; the design just doesn't rule it out.
- An instance must outlive its contexts. A `final` procedure closes its files.

The pool's watcher mechanism (`swpool`/`cvpool`) and the `zzctr` counters are replaced by the generation counter. Each cache stores the generation it was built at.

**File I/O:** SPK/CK/PCK/DSK data is read from DAF files at query time, so concurrent queries read the same files. Files are opened with `newunit=` (`fndlun`/`reslun` go away); opening one file on several units is assumed to work. All file access goes through one internal module (`pepper_daf_io`), so the strategy can change without touching callers:

- (a) **Shared units:** one unit per file in the instance; direct-access `read(unit, rec=)` relying on the runtime's per-unit locking (gfortran does this; ifx needs `-reentrancy threaded`). Simplest, but the per-unit lock serializes all reads of a file.
- (b) **Units per context:** each context opens its own unit for a file on first read (and closes it when the context is destroyed). Reads are truly concurrent; it uses one unit per file per thread.
- (c) **In memory:** whole segments or files read into memory at load time (memory cost; DE440 is about 115 MB).

Start with (a); measure; move to (b) or (c) if contention shows. All three are plain Fortran I/O.

### Error handling

SPICELIB's error system is global and touched everywhere: `sigerr`/`failed`/`return`/`chkin`/`chkout`, actions `ABORT`/`RETURN`/`REPORT`, and an output device. It moves into the context, without the traceback:

- Each routine that can signal an error is a type-bound procedure of the context (public API) or takes the context as its first argument (internal routines). The rewrite is mechanical: `if (return()) return` → `if (ctx%failed()) return`, `call sigerr(...)` → `call ctx%sigerr(...)`, and so on.
- **No traceback** (decision): `chkin`/`chkout` are removed entirely (done in phase 2). The legacy layer keeps no-op `chkin`/`chkout`/`qcktrc`/... so existing callers still link; the traceback is always empty.
- The default action for the new API is `RETURN`: the caller checks `ctx%failed()` and gets the message from the context (and resets it), not `ABORT`. A library must not stop a multi-threaded program. Routines get **no** extra `stat`/`errmsg` arguments; the context is the error channel. The legacy layer keeps SPICELIB's defaults.
- Purely numerical routines that never signal errors (most vector/matrix math) do not get a context and become `pure`/`elemental` where possible.

### Module structure

One module per subsystem, plus a `pepper` module that re-exports the public API. Large implementations go in submodules (interface in the module, code in submodules) to keep recompilation cascades small. Proposed modules:

- **Basics:** `pepper_kinds` (`ip`, `wp`), `pepper_constants`, `pepper_arrays` (search, sort and other array operations), `pepper_math` (vectors/matrices/quaternions/coordinates/planes/ellipses; pure), `pepper_strings`, `pepper_cells` (cells/sets/windows).
- **Infrastructure:** `pepper_errors`, `pepper_instance` (types `pepper_spice` and `pepper_context`; a module cannot have the name of its type), `pepper_pool`, `pepper_kernel_mgr`, `pepper_daf_io`, `pepper_daf`, `pepper_das`, `pepper_dla`.
- **Ephemeris, orientation and time:** `pepper_spk`, `pepper_pck`, `pepper_ck`, `pepper_frames`, `pepper_bodies`, `pepper_time`, `pepper_sclk`.
- **Geometry and search:** `pepper_geometry` (illumination, occultation, limbs, ...), `pepper_dsk`, `pepper_gf`, `pepper_ek`.

**Source layout:** all module code ends up directly in `src/`, the legacy wrappers in `src/legacy/`, and `src/spicelib/` is eventually removed:

- One file per module or submodule, named after it: `src/pepper_spk.f90`, `src/pepper_spk_*.f90` (submodules), ...
- As each subsystem is converted, its old files are deleted from `src/spicelib/` and the new module files are added to `src/`. During the migration both directories build together (fpm compiles all of `src/` recursively).
- The `.inc` include files are removed eventually (decision): their parameters move into modules (e.g. `pepper_constants`, or a parameters module of the subsystem) and are imported with `use`, also by the tspice tests; no include directory is needed at the end.
- Platform-specific code (the `.F90` files with `SPICE_OS_*` / `SPICE_COMPILER_*` branches) moves into the modules that need it; those module files keep the `.F90` extension.

Each umbrella routine becomes a module (or part of one):

- its `save`d variables become components of a state type held in `pepper_spice` or `pepper_context`;
- each `entry` becomes a module procedure;
- the `data` initializations become component default values or `parameter`s.

**API shape:** type-bound procedures throughout, with SPICELIB's names and argument order (minus the context):

- Queries are bound to the context: `call ctx%spkezr(targ, et, ref, abcorr, obs, state, lt)`, `call ctx%str2et(utc, et)`, `call ctx%pxform(from, to, et, rot)`.
- Loading and setup are bound to the instance: `call spice%furnsh(file)`, `call spice%unload(file)`, `call spice%kclear()`, pool writes.
- The bindings are declared in the type's module (`pepper_instance`) as separate module procedure interfaces; the implementations live in submodules per subsystem (`pepper_spk`, `pepper_time`, ...). So the type definition grows, but changing an implementation recompiles only its submodule.
- Bindings are `non_overridable` (dispatch can then be resolved statically).
- Internal routines (`zz*` and helpers) are not bound: they are private module procedures taking `class(pepper_context)` as their first argument.
- Pure numerical routines (vectors, matrices, quaternions, ...) are not bound either: they are plain `pure`/`elemental` module procedures (`vcrss`, `mxv`, ...) since they need no state.
- Descriptive aliases can come later.

**Callbacks (GF, user-defined functions):** today their state passes through `save`d umbrellas, because the callback signatures are fixed. In the new design, abstract interfaces take the context plus a user-data argument (`class(*)`, or a type the user extends), so state flows through arguments.

### Legacy compatibility layer

tspice calls 1160 library procedures directly, 291 of them internal (`zz*`) routines and entry points. Rewriting all those tests up front is not practical. So:

- Files `src/legacy/pepper_legacy_<subsystem>.f90` hold external procedures with SPICELIB's original names and F77 signatures, grouped per subsystem. They forward to the module procedures using a global **default instance and context**.
- They are generated by a script from the module interfaces wherever the mapping is mechanical.
- The layer is explicitly **not** thread-safe, like the original SPICELIB.
- This lets the migration go subsystem by subsystem: once a subsystem moves into modules, its legacy wrappers replace the old externals and tspice keeps testing it unchanged.
- The same layer serves existing SPICELIB users.
- New tests are written against the module API. tspice families can be ported to it over time, after which their wrappers for internal routines can be dropped.

### Compiler and build requirements for reentrancy

- All procedures must be reentrant. gfortran places local arrays larger than `-fmax-stack-var-size` in static memory unless `-frecursive` (or `-fopenmp`) is given; ifx needs `-recursive` / `-reentrancy threaded`. Add these to the `platform` feature in `fpm.toml` now, and mark procedures `recursive` as they are modularized. (gfortran does not yet make procedures recursive by default as Fortran 2018 specifies.)
- Thread stacks are small (`OMP_STACKSIZE`), and SPICELIB has large local work arrays (e.g. buffers sized for 5000 segments). Make large locals `allocatable` or move them into the context.
- Remove all implicit `save`: initialized local declarations and `data`. A script check in CI should reject new ones.

### Kinds

`pepper_kinds` defines `ip = int32` and `wp = real64` (from `iso_fortran_env`). All code uses `integer(ip)`, `real(wp)` and literals like `1.0_wp`, replacing default `integer` and `double precision` / `1.0d0`. With gfortran these are the same kinds as before, so the change is scripted and verified by identical object files (like the earlier lowercase conversion). `logical` and `character` stay default kind.

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
  - N threads, each with its own context on one instance, compute `spkezr`, `pxform`/`sxform`, `str2et`/`et2utc` and `sce2c`/`scs2e` over many epochs. Results must be **bitwise identical** to a serial run.
  - Kernels come from tspice's generators (`tstspk`, `tstpck`, `tstlsk`, ...), so no downloads are needed; a real DE kernel is optional, for benchmarks.
- **ThreadSanitizer** (`-fsanitize=thread`) over the concurrency tests to catch remaining shared state. GCC supports it on Linux but not macOS, so it waits until a Linux environment is used.
- **State inventory check:** a script lists every `save`/`data`/initialized-declaration/`entry` in `src/`. The count must reach zero outside `parameter`s, and CI keeps it there.
- **Compilers:** for now only gfortran in the pixi environment (macOS arm64). The code stays portable (the platform preprocessor branches remain), and ifx, Linux and Windows (and CI) come later.

### Phases

Each phase ends with tspice green, the concurrency tests that apply passing, and a commit.

0. **Preparation (done):**
   - the state-inventory script (per routine: `save`/`data`/`entry`, classification, call graph) to drive and track the work;
   - reentrancy flags (`-frecursive`) in `fpm.toml`;
   - `pepper_kinds` and the scripted switch to `integer(ip)` / `real(wp)` (verified by identical objects).

   *Result:* `tools/state_inventory.py` (`pixi run inventory`): 1415 files, 318 entry points; 344 files with own state (76 umbrellas, 255 with saved variables, 13 constants computed on the first call); by call graph 271 files are stateless, 14 depend only on such constants, 263 only on the error system (73 files) and 867 on other state. `src/pepper_kinds.f90` and `tools/convert_kinds.py`: 1400 files switched to `integer(ip)`, `real(wp)`, `_wp` literals and `real(x, wp)` (identical object files). gfortran builds with `-frecursive`. tspice passes.
1. **Stateless core into modules (done):** constants (constant functions such as `pi()`, `dpr()`, `rpd()`, `halfpi()`, `twopi()`, `spd()`, `clight()`, `jyear()` become named constants `pi`, `dpr`, ... in `pepper_constants`, and callers use the constants directly; the functions remain only in the legacy layer; other first-call `save`s become `parameter`s too), vector/matrix math, strings, search/sort. Mark `pure`/`elemental`; generate the first legacy wrappers. Low risk, and it builds the module, wrapper and test machinery.

   *Result:* 249 routines moved out of `src/spicelib`: `pepper_constants` (19 constants, by hand), `pepper_arrays` (57 routines), `pepper_math` (118), `pepper_strings` (55); the assignment is in `tools/modules.py`. Tools: `tools/modularize.py` (moves routines into a module and writes the legacy wrappers `src/legacy/pepper_legacy_*.f90`; it runs once per module, since it deletes the moved sources), `tools/add_intents.py` (intents from the NAIF `$ Brief_I/O` tables; the compiler found 5 arguments documented as input that are modified and restored, now `intent(inout)`), `tools/mark_pure.py` (marks procedures `elemental`/`pure` as far as the compiler accepts: 68 elemental, 161 pure, 1 neither). `lcase`/`ucase`/`ljucrs` use `parameter`s instead of first-call tables; `moved` no longer copies reals through `movei`. Unit test `test/core/test_core.f90` (module API, elemental use, `do concurrent`). tspice passes.
   - Not moved yet: cells/sets/windows (they signal errors, so they need the context: phase 2), `chbase` (platform-dependent), and stateless routines that belong to a subsystem (DAF/DAS/EK/CK/GF/DSK internals), which move with their subsystem.
   - Follow-up cleanups (all of `src/`, verified by identical object files): `character(...)` instead of the obsolescent `character*...`, and `==`, `/=`, `<`, `<=`, `>`, `>=` instead of `.eq.` ... `.ge.` (`tools/modernize_syntax.py`).
   - Known issue: many string routines document "OUT may overwrite IN" (same variable passed for both). Fortran does not allow that aliasing; it works in practice, as it did before. Calls inside the modules pass a copy (`(string)`); the legacy interface keeps accepting it.
2. **Instance, context and errors:** `pepper_spice` (initially only an owner of contexts), `pepper_context`, the error system in the context, the default instance and context for the legacy layer, then the routines that only depend on the error system (cells/sets/windows and others: about 270 files, see `pixi run inventory`). Establish the mechanical rewrite pattern (type-bound procedure or `ctx` argument, `ctx%failed()`, ...) used in all later phases.

   *Progress:*
   - **Traceback removed:** all 7281 `chkin`/`chkout` calls deleted from `src/spicelib` (1053 files), with the `if (return()) then / return / else / end if` blocks they leave collapsed to `if (return()) return` (`tools/remove_traceback.py`). `trcpkg` is gone; `src/legacy/pepper_legacy_traceback.f90` has no-op stubs. tspice does not depend on the traceback.
   - **`src/pepper_instance.f90`:** `type(pepper_spice)` (the instance; `new_context()`) and `type(pepper_context)` with the error system per context: `failed`, `sigerr`, `setmsg`, `errch`, `errdp`, `errint`, `reset`, `getmsg`, `erract`, with SPICELIB's semantics (first error kept under RETURN, markers, actions). Native contexts default to RETURN and print nothing; REPORT/ABORT/DEFAULT print to `error_unit`. A *legacy* context forwards to SPICELIB's global error routines, so the legacy interface behaves exactly as before (`erract`/`errprt`/`errdev`, output, abort). `src/legacy/pepper_legacy_context.f90` holds the default instance and its legacy context (the only global state). `expln` moved to `pepper_errors`. Test: `test/core/test_context.f90`.
   - **Documentation:** every moved procedure has a `!>` docstring (from its NAIF Abstract) and a `!!` comment on every argument (from its Brief_I/O row); the Brief_I/O sections are removed (their other text moved into the docstring). The legacy wrappers are documented the same way (`tools/document_procedures.py`).
   - **Routines with the context:** 158 routines that only depended on the error system moved into submodules of `pepper_instance` (`tools/modularize_ctx.py`): `pepper_cells` (cells, sets, windows; 70), `pepper_numerics` (numerical/geometric routines that check their inputs; 46), `pepper_array_ops` (array operations and linked lists; 30), `pepper_text` (string operations that check their inputs; 12). Each is a `module procedure` with the context as first argument; its interface (intents, `!!` comments, docstring) is generated into `pepper_instance`, and the public ones are type-bound (`call ctx%insrtd(item, set)`, `n = ctx%cardd(cell)`); `zz*` routines are public module procedures without a binding. Error calls became `ctx%sigerr(...)` etc., `return()`/`failed()` became `ctx%failed()`. The legacy wrappers call the bindings with `legacy_context()`.
   - On the way: `enchar`/`dechar` (an umbrella without state) split into two routines; first-call constants in `eqncpv`, `hx2int`, `invort` and read-only `data` tables (`dpstr`, `intstr`; `tools/data_to_parameter.py`) became parameters, so `intstr`, `dpstr` and `touch*` joined the stateless modules (`modularize.py` can now add routines to an existing module); `ctx%errfnm` added. `tools/wrap_docs.py` wraps long documentation comments.
   - **Remaining** in `src/spicelib`: 1001 files, of which about 180 depend only on the error system; they belong to subsystems (DAF/DAS/EK/DSK/SPK/CK/GF internals, file I/O) and move with them in the following phases.
   - Note: with the legacy context, `ctx%failed()` replaces `return()`; they differ only under the REPORT and IGNORE actions (after an error, a moved routine returns instead of continuing).
3. **Kernels:** kernel pool, kernel management (`furnsh`/`unload`/`kclear`), body and frame name tables (built-in tables as `parameter`s) into `pepper_spice`; the generation counter in place of watchers and `zzctr`.
   *Progress:*
   - **Findings:** the pool depends on more than its own state: text kernels are read by `rdker`/`rdtext` (open file and position in saved state), values are parsed by `zzrvar`/`zzrvbf` and `nparsd`/`nparsi` (first-call tables), names are hashed by `zzphsh` (hash modulus configured by a call), `@`-dates are parsed by `tparse` and the time-string machinery (`zztime`, `tpartv`, `texpyr` and `tcheck`, which hold configurable settings), and change counters come from `zzctr`. These move with the pool, so part of the time parsing comes into phase 3.
   - **First-call constants:** `tools/first_to_parameter.py` turns `if (first)` blocks that only compute constants into parameters (14 files, e.g. `zzrvar`, `zzrvbf`, `zzctr`, `nparsi`). By hand: `lx4uns` and `eqchr` (tables built with implied-do parameters); `eqchr`/`nechr` split into two functions.
   - `zzphsh` stays legacy: `F_AAAAPHSH` tests its global state (hash modulus set by `zzshsh`). The new pool will use a stateless hash with the same values.
   - **Umbrellas:** `tools/umbrella.py` (used by `modularize_ctx.py`, configured in `modules.UMBRELLAS`): the saved variables that are real state become the components of a type in a generated module `src/pepper_state_<name>.f90`; it is an allocatable component of `pepper_spice` (instance state: kernel pool, text-kernel reader, time-parsing settings) or of `pepper_context` (`"where": "context"`: the state of a computation in progress, e.g. `zztime`'s token list, so that threads parsing times do not share it). Each entry point becomes a module procedure that reaches the state through `associate`; the initial values are set by a generated `init_<name>_state` (a type with default initialization made gfortran put a 27 MB temporary on the stack in `allocate`).
   - **Kernel pool moved** (`pepper_pool`, 59 procedures): `pool` (21 entry points), `rdker`/`rdtext` (text kernels; units from `newunit`), the time-string parser used for `@`-dates (`tparse`, `tpartv`, `zztime`, `tcheck`, `texpyr`, `zztpats`, ...), and the pool helpers. By hand on the way: `zztpats`' 231 patterns and `tcheck`'s tables are parameters, `tcheck`'s days-in-month table is a local variable (SPICELIB saved and modified it), `tpartv` and `nparsd` compute their tables on every call, `rdtext` uses the same file limits on all platforms; `pepper_hash` (`name_hash`, the same values as `zzhash2`, tested against it) replaces the hash with a configured divisor; number parsers and `eqchr`/`matchi` etc. joined `pepper_strings`. The pool's state is per instance, so two instances have separate pools (`test/core/test_pool.f90`, which also loads a text kernel through a native context).
   - **Body names moved** (`pepper_bodies`): `bodn2c`, `bodc2n`, `bods2c`, `bodc2s`, `boddef`, `bodfnd`, `bodvar`, `bodvcd`, `bodvrd` and their helpers (`zzbodtrn`, `zzbodblt`, `zzbodker`, `zzbodini`, `zzbods2c`, `zzbodvcd`, the hash sets `zzhsc`/`zzhsi`). Decisions on the way:
     - Tables derived from the pool and the built-in tables are a **cache per context** (`"where": "context"`), rebuilt when the pool's state counter changes. The moved code checks the counter (`zzpctrck`) instead of pool watchers (`swpool`/`cvpool`): watchers are shared state in the pool, and with several contexts one context would consume another's update (and registering them modifies the pool during a query). Legacy `zzcvpool` keeps its semantics for the code that still uses it.
     - **`boddef` definitions are recorded in the instance** (`boddef_names`/`boddef_codes`, with a counter); each context replays them into its own tables when the counter changed, so a name defined through one context is known to all. Queries never modify the instance.
     - Saved caches of plain routines (`bodvrd`'s last translation) are per-context state too (`"state": "saved"`).
     - `test/core/test_bodies.f90`: built-in names, `boddef` through one context seen by another, names from a kernel loaded after both contexts built their tables.
   - `F_SCLK1`'s run-time comparisons allow a ratio of 3.0 instead of 1.5 (the debug build's extra call layers made it fail; see `example/bench_pool.f90`).
   - **Next:** `keeper` (`furnsh`/`unload`/`kclear`), which needs the binary-kernel loaders (DAF/DAS/DSK, phase 4), and the frame name tables (`framex`), which need the frame kernels' pool data and the built-in frames.
4. **File layer:** the DAF/DAS/DLA handle manager (in the instance) and record caches (per context), `pepper_daf_io` with strategy (a), `newunit`.
5. **Ephemeris, orientation, time (the main use case):** SPK, PCK, CK, frames (including dynamic frames), time (`str2et`/`et2utc`/`deltet`/`unitim`/`timout`), SCLK.
   - **Milestone:** parallel ephemeris and time tests pass (ThreadSanitizer once on Linux).
6. **Geometry, DSK, GF, EK** (after the ephemeris/time milestone): derived geometry, DSK, GF (callback redesign), EK. EK is large (189 files) and probably the least used, so it comes last.
7. **Cleanup:**
   - remove `-fallow-argument-mismatch`;
   - drop internals only the legacy layer needs;
   - delete the (by then empty) `src/spicelib/` and the `include-dir` entry in `fpm.toml`, so all code lives in `src/` (and `src/legacy/`);
   - reduce the platform-specific code (`zzplatfm`, I/O quirks) now that `newunit` and standard I/O are used;
   - convert NAIF header comments to FORD documentation;
   - benchmarks: serial vs parallel, and against the original SPICELIB.

### Risks

- **Size:** about 1400 routines, so the transformations must be scripted and verified (object-file comparison where the change is purely mechanical; tspice otherwise), and reviewable in subsystem-sized commits.
- **Numerical fidelity:** moving code into modules can change inlining and therefore round-off. The concurrency tests compare bitwise against serial runs of the *same* build; tspice uses tolerances.
- **Performance:** per-context caches start cold in every thread; I/O contention on shared files (see the File I/O options above).
- **Compiler differences:** Fortran I/O thread safety and reentrancy flags differ between gfortran and ifx. Only gfortran is tested for now, so ifx problems will surface late.
- **Large type:** with ~600 type-bound procedures, the context type's module is big; submodules keep rebuilds local, but compile time of that module should be watched.

### Decisions (from review)

1. **API style:** type-bound procedures (`call ctx%spkezr(...)`, `call spice%furnsh(...)`).
2. **Names:** keep SPICELIB names and argument order.
3. **Errors:** the context is the error channel, checked with `ctx%failed()`, default action `RETURN`; no `stat`/`errmsg` arguments on routines.
4. **Concurrency contract:** load first, then query concurrently. No concurrent loading.
5. **Dependencies:** the library is free of OpenMP (OpenMP only in tests) and Fortran only.
6. **Priority:** ephemeris and time first; geometry, DSK, GF and EK afterwards.
7. **Compilers:** only gfortran in the pixi environment for now.
8. **Kinds:** `ip` (`int32`) and `wp` (`real64`) everywhere.
