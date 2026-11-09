# Removing SPICELIB's fixed limits

SPICELIB sizes nearly everything at compile time, because Fortran 77 had no
dynamic memory. Examples: 5000 loaded files, 26003 pool variables, 255-character
file names, 161 body definitions, 100 surfaces in a method string. These limits
come from the language SPICELIB was written in, not from SPICE itself. This plan
removes the arbitrary ones. Its goals, in priority order:

1. **Speed**, and memory: no slower at the hot paths (`spkezr`, `pxform`, pool
   reads), and much smaller contexts.
2. **Clarity**: idiomatic modern standard Fortran.
3. **Maintainability and readability**: one definition per limit; records in
   place of parallel arrays.
4. **Robustness**: every removed limit has a test that goes past it.

The data come from a survey of the code (November 2026). Line numbers are
omitted; the names are searchable in `src/pepper_states.f90`, `src/inc/` and the
named routines.

## What is a limit, and which ones go

Each fixed size falls into one of five groups:

| Group | Meaning | Action |
|---|---|---|
| **A. Capacity** | An arbitrary maximum that surfaces as an error (or silent truncation) when exceeded | **Remove**: the structure grows |
| **B. Cache** | A buffer whose size only affects speed; when full it evicts or falls back | **Keep** (sized for speed), fix those that fail instead of evicting |
| **C. Format / OS / semantic** | Fixed by a file format, the OS, or a guard against nonsense input | **Keep**, documented as such |
| **D. Name lengths** | Lengths of names that are part of SPICE conventions and the public API (36, 32, 80) | **Decision needed** (see the end) |
| **E. Caller buffers** | The caller supplies the array (cells, windows, `room` arguments) | **Separate API track**: add variants that return allocatable results |

## Inventory

### A. Capacity limits to remove

**Loading kernels.** One `furnsh` passes through several tables, and each one
is a limit. They must grow together, or the smallest becomes the limit.

| Structure | Limit | When full | Notes |
|---|---|---|---|
| KEEPER (`keeper_state`, instance) | 5000 files + 300 text kernels | `NOMOREROOM` | 4 parallel arrays. `srces` indexes `files`, so unloading compacts and renumbers |
| ZZDDHMAN file table (instance) | `ftsize` 5000 (DAF + DAS together) | `FTFULL` | 8 parallel arrays |
| DAFAH file table (instance) | 5000 | `DAFFTFULL` | Parallel arrays plus a cell `fhlist` |
| DAFFA search table (context, 10.3 MB) | = DAFAH size | Garbage-collects closed files | Must follow DAFAH. 10 MB per context |
| DASFM file table (instance) | 5000 | `DASFTFULL` | `lnk` pool plus parallel arrays plus a cell |
| SPKBSR / CKBSR / PCKBSR / ZZDSKBSR file tables (instance) | 5000 each | `SPKFILETABLEFULL`, `CKTOOMANYFILES`, `PCKFILETABLEFULL`, `DSKTOOMANYFILES` | Unreachable today, because the open fails first |
| EKQMGR (instance) | 20 files, 200 segments, 100 tables, 500 columns, 10000 column descriptors | `EKFILETABLEFULL`, `EKSEGTABLEFULL`, `EKTABLELISTFULL`, `EKCOLATTRTABLEFULL`, `COLDESCTABLEFULL` | 5 interlinked `lnk` pools whose node indices are stored in other tables |
| DAFEC / DASEC file tables (context) | 5000 | `FILETABLEFULL` | Only for reading comments |
| DAFANA (context) | 20 DAFs being written | `STFULL` | Only for writing files |

**Kernel pool** (`pool_state`, instance, 9 MB fixed):

| Limit | When full | Notes |
|---|---|---|
| `maxvar` 26003 variables (also the number of hash buckets) | `KERNELPOOLFULL` | Growing means rehashing every name |
| `maxval` 400000 numbers, `maxlin` 15000 strings | `KERNELPOOLFULL` (text kernels); `NOMOREROOM` (`pcpool`/`pdpool`/`pipool`) | `lnk` pools. These can grow without moving nodes |
| `maxchr` 80 characters per string value | **Silent truncation** | See D |

**Names and definitions** (per context unless noted):

| Structure | Limit | When full | Notes |
|---|---|---|---|
| ZZBODTRN kernel names | `nroom` 14983 names in `NAIF_BODY_NAME` | `KERVARTOOBIG` | Rebuilt on every pool change, so it can be sized to the actual count. Its callers have a 540 KB stack array (`kernor`) |
| ZZBODTRN BODDEF table | `maxe − nperm` = 161 definitions | `TOOMANYPAIRS` | The instance record is already growable; only the per-context table is fixed |
| ZZSRFTRN surfaces | `mxnsrf` 2000 | `TOOMANYSURFACES` | Hash table with parallel arrays |
| ZZPRSMET method strings | `maxsrf` 100 surfaces per `SURFACES =` list | `TOOMANYSURFACES` | Sizes `srflst(100)` in 13 state types and in GF locals |
| ZZSWFXFM switch frames | `maxbas` 15000 base frames for one frame | `TOOMANYBASEFRAMES` | The frame table itself is a cache (B) |
| SC01 (SCLK) | `mxpart` 9999 partitions, `3*mxcoef` 300000 coefficients per clock | `TOOMANYPARTITIONS`, `TOOMANYCOEFFS` | `mxpart` also sizes `scencd`/`scdecd` state and a 240 KB stack array in `scpars` |
| TISBOD / BODEUL | `maxang` 200 nutation/precession angles | Error from `bodvcd` (`ARRAYTOOSMALL`, not confirmed) | |
| ZZDSKSBF / ZZDSKSNS | 10000 segments for one body | `SEGMENTTABLEFULL`, `BUFFEROVERFLOW` | Unlike SPK, CK and PCK, which fall back to a slower search |

**Strings:**

| Limit | Where | When exceeded |
|---|---|---|
| File and path names 255 (`filen`, `filsiz`, `rdtext` `lstfil`, `closed_files`) | KEEPER, ZZDDHMAN, DAS, RDTEXT, about 200 locals | `FILENAMETOOLONG`/`PATHTOOLONG` in `furnsh`. **Silent truncation** in `zzddhopn` and `dasopr`, which then open the wrong path (SPICELIB bug) |
| Error messages: short 25, long 1840, device 255 | context | **Silent truncation**. `et2lst`'s 26-character short message is cut today (SPICELIB bug) |
| EK query 2000 characters, string compares 1024 | `zzekscan`, `ekfind`, `ekpsel` | Silent truncation |
| DSK method string 1000 | `zzprsmet` | Silent truncation |

**Work buffers that limit problem size** (algorithm locals):

| Limit | Where | When exceeded |
|---|---|---|
| `maxhit` 1000 segments containing a point | `zzdskbun`, `zzdskbux` (88 KB stack) | `TOOMANYHITS`, `BUFFERTOOSMALL` |
| `nvxlst` 50000 voxels on a ray; `maxpl` 256000 plates | `dskx02` (1.6 MB stack frame) | `ARRAYTOOSMALL` |
| `maxwin` 2000 tangent points per half-plane | `limbpt`, `termpt`, `zztanslv` | `ARRAYTOOSMALL`, `WINDOWEXCESS` |
| `maxvrt` 10000 FOV vertices | GF FOV search (`zzgffvin`), `zzelvupy` | `BOUNDARYTOOBIG`, `INVALIDCOUNT` |
| `nmax` 200 leap seconds | `deltet` | `BUFFERTOOSMALL` |
| EK query limits: 10 tables, 1000 constraints, 500 tokens, 50 selected columns, ... | `zzekpars`, `zzeknrml` | Parse error in the query |

### B. Caches (keep; size for speed)

- **Unit table** (`utsize` 23, per context): LRU over open Fortran units. It also
  bounds file descriptors (C), so it stays an LRU.
- **BSR body and segment tables** (SPK/CK 100000 segments, 10 MB per context
  each; PCK 5000; DSK 10000). When full they evict, or search without
  buffering. Keep them as caches, but start them small and grow them up to a
  cap: most contexts use a tiny fraction.
- Other caches that are fine:
  - DAFRWD and DASRWR record buffers;
  - DASA2L;
  - TKFRAM (200 frames);
  - TISBOD (157 bodies);
  - SC01 (100 clocks);
  - ZZSCLK;
  - DSKX02 vertex buffer;
  - ZZDSKSBF body table (10);
  - EK scratch area;
  - ZZSWFXFM frame table.
- **FRAMEX kernel-frame cache** (5209): a cache that **fails instead of
  evicting**. The 5210th distinct kernel frame looked up between pool changes
  gives `HASHISFULL` (SPICELIB bug). Fix: reset when full, or grow.
- **The pool change log** (10000) already degrades gracefully. The watches and
  the BODDEF record are already growable.

### C. Keep (format, OS, semantic)

- **DAF and DAS formats:**
  - segment ID length 40 (`NC = 8*(ND+(NI+1)/2)`);
  - record sizes (1000/1024 comment records);
  - `ND`/`NI`;
  - DSK type 2 `maxcgr` 100000 coarse voxels;
  - `maxplt`/`maxvrt`/`maxvox`;
  - the EK B-tree node sizes;
  - `mxclsg` 100 columns per EK segment (probably format; to be confirmed).
- **SCLK kernels:** `mxnfld` 10 fields.
- **Text kernel lines:** 132 characters. This is NAIF's documented text-kernel
  rule, to be confirmed in `kernel.req`. Longer lines are silently truncated
  today. They should be an error instead.
- **Built-in data:** `nperm`, `ninert`, `nninrt`, `maxbfr` (fixed tables).
- **Guards:**
  - `RECURSIONTOODEEP`: a dynamic frame may not be used at recursion level 1.
    `plan.md` records the decision to keep it.
  - Iteration caps (`nearpt`, `zzcxbrut`).
  - GF progress line lengths (an 80-column display).
- **Frame chains:** no fixed length (the buffer of 10 nodes is folded).

### D. Name lengths (decision needed)

| Length | What | Where it appears |
|---|---|---|
| 36 | Body names (`maxl`), surface names (`sfnmln`) | `bodc2n` output, kernels (longer names are truncated by `gcpool`) |
| 32 | Frame names (`frnmln`, declared 109 times), pool variable names (`maxlen`; `BADVARNAME` if longer; also `kvnmln`, watch names) | Kernels, `VARNAMETOOLONG` in the dynamic-frame routines when `FRAME_<name>_<item>` exceeds 32 |
| 80 | Pool string values (`maxchr`) | Silently truncated |

### E. Caller buffers (API track)

- **185 interfaces take a cell, set or window** (150 of them public):
  - the cell, set and window routines;
  - the 16 GF searches;
  - `spkcov`/`ckcov`/`pckcov`;
  - `spkobj`/`ckobj`/`dskobj`;
  - the 51 symbol-table routines.
- **About 55 take an output array with its size:**
  - names like `room`, `maxn` or `nroom` (e.g. `gdpool`, `bodvrd`, `getfov`, `dskp02`);
  - the routines signal `ARRAYTOOSMALL`, `CELLTOOSMALL` or `WINDOWEXCESS`;
  - character outputs truncate silently.

## Design

### Building blocks (one new module, `pepper_containers`)

1. **Growable arrays of records.**
   - Replace each set of parallel arrays with one array of a derived type plus a
     count, for example
     `type(loaded_file), allocatable :: files(:); integer :: nfiles`.
   - One generic `grow(array, needed)` doubles the capacity with
     `move_alloc`.
   - Records are clearer and grow in one step.
   - Where a hot loop scans one field of many entries (BSR file and segment
     searches), measure records against separate arrays before choosing.
2. **Hash maps.**
   - One string→index map and one integer→index map, growing at a load factor
     of about 0.7.
   - They replace the `zzhsc*`/`zzhsi*` users:
     - FRAMEX;
     - ZZBODTRN;
     - ZZSRFTRN;
     - SC01;
     - ZZSWFXFM;
     - TISBOD.
   - They also replace the pool's name table.
   - Today every table has as many buckets as its capacity, so growing any of
     them means rehashing. The new maps rehash internally.
   - `name_hash` and the legacy `ZZHASH`/`ZZSHSH` stay for tspice. `f_aaaaphsh`
     expects hash values computed with the divisor 26003.
3. **Linked-list pools that grow.** Where a `lnk` pool remains (pool values,
   BSR segment lists), add `lnkgrow`. It appends nodes to the free list, keeps
   existing node indices and updates the header in rows −1 and 0. This lets
   tables grow without rewriting their algorithms. Replacing a pool by plain
   arrays (one per body or variable) is a later, per-table decision, made with
   measurements.
4. **Variable-length strings.**
   - `character(:), allocatable` for scalars.
   - A small `type :: string; character(:), allocatable :: s; end type` for
     arrays (file names, pool string values).
   - Interfaces keep `character(*)` arguments. Where a routine returns text of
     unknown length, add a variant with an `allocatable, intent(out)` result.

Each block gets its own unit test, with growth, rehash, and many entries, before
any subsystem uses it.

### Rules

- **One definition per limit.**
  - SPICELIB repeats each limit as a local `parameter` in every routine that
    uses it:
    - `ftsize` 39 times;
    - `filsiz` 70;
    - `frnmln` 109;
    - pool limits about 110;
    - `maxwin` 30.
  - Before anything grows, the locals are removed and each limit is one named
    constant (or, once dynamic, the table's own `size()`/count).
- **Growth and `associate`.**
  - The entry points reach their state through `associate`.
  - If an array is reallocated while an `associate` name refers to it, that
    name refers to freed memory.
  - Growth happens only in a small `ensure_room` routine called *before* the
    `associate` block, or in code outside one. The tests run with
    `-fcheck=all` and AddressSanitizer to catch violations.
- **Concurrency.**
  - Instance tables grow only while kernels are loaded or the pool is changed.
    The contract already forbids that while other threads query.
  - Context tables belong to one thread.
  - No locks are needed. `plan.md` (decision 4) is stated again in the API
    docs.
- **No silent truncation.**
  - Where a fixed length must stay (C), exceeding it is an error, never a
    truncation.
  - This holds for text kernel lines, format constants and caller-supplied
    `character(*)` outputs. Outputs that may be cut get a `found`/length
    variant, or are documented.
- **Start small, grow on demand.**
  - Initial capacities are sized for typical use (tens of files, a few
    thousand pool variables).
  - Contexts shrink from tens of MB to KB. With the per-context states this
    makes `new_context` and the first call in each thread cheaper, which
    helps the parallel use case.
- **Measure.**
  - A benchmark set is recorded before the first change.
  - After each phase the hot paths must not be slower beyond noise. Memory per
    context and per instance is reported.

## Phases

Each phase is one or more commits, and each ends with `pixi run tspice` at
399/399 (with the noted test changes) and `pixi run test` passing.

0. **Foundations** (no change in behavior):
   - **Benchmarks:**
     - loading N small kernels;
     - `spkezr`/`pxform` on many bodies and frames;
     - `gdpool` with many variables;
     - `bodn2c` with many kernel names;
     - `new_context` plus the first call;
     - memory per context.
   - **One definition per limit:** remove the local parameter copies (a
     scripted change, verified by object-file comparison).
   - **`pepper_containers`:** growable arrays, hash maps, `lnkgrow`, the
     string type, with unit tests.
   - **Test generators:** a helper that writes N small SPK, PCK and text
     kernels and N frame or body definitions to a temporary directory, so
     tests can exceed the old limits quickly.
1. **Strings that truncate silently** (fixes two SPICELIB bugs):
   - **File and path names:** allocatable strings in KEEPER, ZZDDHMAN, DAS,
     RDTEXT and `closed_files`.
     - `FILENAMETOOLONG` and `PATHTOOLONG` go away. Path names are limited
       only by the OS.
     - Test: load a kernel through a path longer than 255 characters.
   - **Error messages:** deferred-length `short`, `long` and `device`.
     `getmsg` still truncates to the caller's buffer; an allocatable getter is
     added. This fixes `et2lst`'s code.
   - **Text kernel lines over 132 characters:** an error instead of
     truncation, unless the decision is to allow longer lines.
2. **Number of loaded kernels:**
   - **Tables, in order:**
     - ZZDDHMAN file table;
     - DAFAH, DAFFA, DASFM;
     - the four BSR file tables;
     - KEEPER;
     - DAFEC and DASEC;
     - EKQMGR's five pools, last and possibly as a separate step;
     - DAFANA.
   - **Each table** becomes an array of records that grows.
   - **Error codes:** these go away:
     - `FTFULL`, `DAFFTFULL`, `DASFTFULL`;
     - `NOMOREROOM` from `furnsh`;
     - the BSR "file table full" errors;
     - the EK table errors.
   - **Tests:**
     - load 10000 small kernels (twice the old limit);
     - unload half and reload;
     - `kclear`;
     - parallel queries afterwards;
     - an `-fcheck=all` build.
   - **tspice:**
     - `F_DDHOPN` (FTFULL case), `F_DDHFTSIZE`, `F_PCKBSR` (FTFULL) and
       `F_ZZDSKBSR` (FTFULL) change to expect success;
     - `UTFULL` (unit table, B) stays.
3. **Kernel pool:**
   - **Name table:** the hash map.
   - **Values:** grow with `lnkgrow`, or move to per-variable allocatable arrays
     (measure `gdpool` and `ldpool`).
   - **String values:** `maxchr`, depending on decision D.
   - **Error codes:** `KERNELPOOLFULL` and `NOMOREROOM` go away.
   - **`szpool`:** reports the current capacity, or `huge(0)`. `MAXAGT` and
     `MXNOTE` are not enforced already.
   - **Tests:** 100000 variables, a 10⁶-value variable, and a text kernel with
     many variables. None exist today.
4. **Names and definitions:**
   - **Bodies:**
     - ZZBODTRN's kernel-name table is sized to the pool's contents;
     - the BODDEF table grows (`TOOMANYPAIRS` goes);
     - the 540 KB `kernor` locals go;
     - `f_bodcod` and `f_zzbdkr` change.
   - **Frames:** the FRAMEX kernel-frame cache uses the hash map and grows. This
     fixes `HASHISFULL`. Test: 20000 TK frames.
   - **Surfaces:**
     - ZZSRFTRN grows;
     - method strings take any number of surfaces (`maxsrf`);
     - the 13 `srflst(100)` state arrays become allocatable.
   - **Switch frames:** `maxbas` per frame.
   - **SCLK:**
     - partitions and coefficients per clock grow;
     - the `scencd`/`scdecd`/`scpars` work arrays follow;
     - `TOOMANYPARTITIONS` and `TOOMANYCOEFFS` go;
     - `f_zzsc01` changes.
   - **TISBOD and BODEUL:** `maxang`.
5. **Segment caches and memory:**
   - The BSR segment and body tables start small and grow up to a cap. Do they
     still need a cap? Measure.
   - The DSK segment table falls back or grows instead of failing:
     `BUFFEROVERFLOW` and `SEGMENTTABLEFULL` go. `f_zzdskbsr` and `f_sbf`
     change.
   - DAFFA follows DAFAH.
   - Target: a fresh context under 1 MB, from about 40 MB.
6. **Algorithm work buffers:**
   - **DSK:** `maxhit` and the `dskx02` voxel and plate lists. `xdda` returns an
     allocatable result, which removes the 1.6 MB stack frame.
   - **Tangent points:** the `limbpt`/`termpt` `maxwin`.
   - **FOV vertices:** GF `maxvrt`.
   - **Leap seconds:** `deltet`.
   - **EK queries:** last. The encoded query is a flat integer cell laid out
     from the maxima, so this is a rewrite of the parser's output, and the
     least-used subsystem.
7. **API for caller buffers** (E, its own design document first):
   - **Allocatable outputs:** variants like `gdpool(name, values)` with
     `values` allocatable, `bodvrd`, `getfov`, `spkcov` into a window type,
     and GF searches with allocated work windows.
   - **The legacy wrappers** in tspice keep the cell versions, possibly built
     on the new ones.
   - **What stays:** the fixed-size interfaces, for compatibility.

Phases 1 to 3 can be done in any order after 0. Phase 2 has the most
user-visible value (the number of kernels). Phase 5 has the largest memory and
parallel-startup gain, and it depends on the containers from 0.

## Testing

- **Past every limit:** a test in `test/core` for each removed limit, at least
  twice the old value, through the public API. The kernel generator keeps
  these fast. Large cases (10000 kernels) can be a separate `slow` test if
  needed.
- **tspice:** cases that assert an old limit (`F_DDHOPN`, `F_DDHFTSIZE`,
  `F_PCKBSR`, `F_ZZDSKBSR`, `F_SBF`, `F_BODCOD`, `F_ZZBDKR`, `F_ZZSC01`,
  `F_SRFTRN`, `F_ZZSWFXFM`) change to expect success, each with a PEPPER note.
  Tests of tspice's own copies of SPICELIB code (`t_spkbsr`, `t_ckbsr`,
  `f_zzhsc`, `f_zzhsi`) are not affected.
- **Untested today, tested after:** `KERNELPOOLFULL`, `NOMOREROOM`, `DASFTFULL`
  (headers only in `f_das`), the FRAMEX cache, and the 100-surface method
  string.
- **Growth under `associate`:** a debug build with `-fcheck=all` and
  `-fsanitize=address` runs the growth tests.
- **Parallel:** `test_parallel` loads more than the old limits, then queries
  from all threads, and compares bitwise with a serial run.

## Decisions for the user

1. **Name lengths (D).**
   - Recommendation: keep 36/32 as the length of *outputs* (the API's
     `character(*)` arguments are already flexible). Store names internally
     at their full length, and signal an error instead of truncating a longer
     name from a kernel.
   - Pool string values (80): lift, since they are stored, not part of a
     format.
2. **Text kernel lines.** Make longer lines an error (strict, compatible with
   SPICELIB), or accept them (more permissive, kernels then not portable to
   SPICELIB).
3. **Caps on caches.** Fixed caps, or settings on the instance (e.g.
   `spice%set_cache_size(...)`).
4. **`szpool`.** The current capacity, or `huge(0)` for "unlimited".
