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
  buffering. Keep them as caches, but start them small and grow them with no
  cap (decision 3): most contexts use a tiny fraction.
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
- **Text kernel lines:** 132 characters (kept, decision 2). This is NAIF's
  documented text-kernel rule. Longer lines are silently truncated today.
- **Built-in data:** `nperm`, `ninert`, `nninrt`, `maxbfr` (fixed tables).
- **Guards:**
  - `RECURSIONTOODEEP`: a dynamic frame may not be used at recursion level 1.
    `plan.md` records the decision to keep it.
  - Iteration caps (`nearpt`, `zzcxbrut`).
  - GF progress line lengths (an 80-column display).
- **Frame chains:** no fixed length (the buffer of 10 nodes is folded).

### D. Name lengths (decided: no limit)

| Length | What | Where it appears |
|---|---|---|
| 36 | Body names (`maxl`), surface names (`sfnmln`) | `bodc2n` output, kernels (longer names are truncated by `gcpool`) |
| 32 | Frame names (`frnmln`, declared 109 times), pool variable names (`maxlen`; `BADVARNAME` if longer; also `kvnmln`, watch names) | Kernels, `VARNAMETOOLONG` in the dynamic-frame routines when `FRAME_<name>_<item>` exceeds 32. Frame names: done (phase 4) |
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

0. **Foundations** (no change in behavior). Done: the benchmarks (with the
   baseline below), `pepper_containers` and the test generators. One
   definition per limit is done in each phase for the limits it touches:
   the census found hundreds of local copies (`lbcell` 454, `frnmln` 69,
   `ftsize` 41 with two meanings: 5000 for the file tables, 20 in EK), and
   most of them sit in code that the phases rewrite.
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
1. **Strings that truncate silently.** Done:
   - **Error messages:** the short and long messages and the device name
     are `character(:), allocatable` in the context, with no length limit.
     `getmsg` still cuts to the caller's variable; `ctx%message(option)`
     returns the whole text. `DEVICENAMETOOLONG` is gone. This fixes
     `et2lst`'s code (test: `test_context`).
   - **Text kernel lines:** the 132-character limit stays (decision 2),
     documented in the `furnsh` and `ldpool` headers and interfaces; a test
     (`test_pool`) pins the behavior (a value past column 132 is lost).
   - **File and path names** moved to phase 2: they are stored in the same
     tables (KEEPER, ZZDDHMAN, DAF/DAS) that phase 2 turns into growable
     records, so each table is converted once.
2. **Number of loaded kernels, and file names.** Done (each table an array
   of records or growable arrays, file names allocatable):
   - ZZDDHMAN (handle manager): records with names of any length, binary
     search by handle; `FTFULL` is gone. Fixes ZZDDHOPN's truncation.
   - DAFAH and DASFM (DAF and DAS file tables): records, binary search by
     handle (SPICELIB: a linear search in DAFHSF and a list walk in DASFM
     on every lookup); the duplicate handle sets (FHLIST) and DASCLS's
     copy of them are gone; `DAFFTFULL`, `DASFTFULL` are gone.
   - DAFFA (DAF searches, per context): starts with 16 entries and grows
     (SPICELIB: 5000, about 10 MB per context).
   - The four BSR file tables (SPK, CK, PCK, DSK): growable; their "file
     table full" errors are gone.
   - KEEPER: records with names of any length; `NOMOREROOM`,
     `FILENAMETOOLONG`, `PATHTOOLONG` are gone; meta-kernel file names and
     path values are read at their full length.
   - Unloading reloads files only when the unloaded entry is a second load
     of a file. One difference from SPICELIB: its reload also moved every
     file loaded through FURNSH above the files loaded directly (SPKLEF,
     CKLPF, PCKLOF, ...); PEPPER keeps the existing order.
   - RDKER and RDTEXT keep the name of the text kernel being read whole.
   - `closed_files` is a string map (it was a list scanned on each open).
   - Tests: `test_limits` (paths over 255 characters through FURNSH, UNLOAD
     and meta-kernels; 6000 SPKs; priorities after unloading).
   - DAFEC, DASEC (comments being read from several files): grow
     (SPICELIB: 5000 entries per context).
   - DAFANA (DAFs being written at once): starts with 4 entries and grows
     (SPICELIB: 20, `STFULL`).
   - File names in error messages: `errhan` takes the name whole from the
     handle manager. SPICELIB's DAF and DAS routines had 44 local
     255-character name variables, most of them unused; those that fetched
     a name for a message use `errhan` now. The record readers (ZZDAFGDR, ZZDAFGSR, ZZDASGRD, ...) no
     longer copy a file name they do not use on every call.
   - Unused `FTSIZE`/`TBSIZE` locals removed (26).
   - Tests: `test_limits` also writes 40 DAFs at once and reads the
     comments of 20 DAFs in turn; tspice `F_DAFANA` expects success past
     20 DAFs.
   - EKQMGR (20 EK files, 100 tables, ...) moved to phase 6, with the rest
     of the EK query limits.

   The plan for the step:
   - **File and path names** (from phase 1): allocatable strings in the
     records of KEEPER, ZZDDHMAN, DAS, RDTEXT and `closed_files`.
     - `FILENAMETOOLONG` and `PATHTOOLONG` go away. Path names are limited
       only by the OS. This fixes ZZDDHOPN's truncation.
     - The 27 DAF and 17 DAS `character(filen)` locals that fetch a file
       name for an error message use `errhan` (or an allocatable name)
       instead.
     - Test: load a kernel through a path longer than 255 characters.
   - **Tables, in order:**
     - ZZDDHMAN file table;
     - DAFAH, DAFFA, DASFM;
     - the four BSR file tables;
     - KEEPER;
     - DAFEC and DASEC;
     - EKQMGR's five pools, last and possibly as a separate step;
     - DAFANA.
   - **Each table** becomes an array of records that grows.
   - **Unloading without reloading:** remove the file from the BSR file
     tables directly and keep the order of the others, instead of reloading
     every remaining file of the type. Reloading is needed only when the
     unloaded file is also loaded under another entry (`single` false). This
     takes the unload of N files from O(N²) file opens to O(N).
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
3. **Kernel pool.** Done:
   - **Storage:** each variable is a record (name, type, values in an
     allocatable array of numbers or of `string`s) in slots that grow, found
     by name through a `string_map`. SPICELIB's hash table of 26003 names and
     linked lists of 400000 numbers and 15000 strings (about 9 MB per
     instance) are gone, with `KERNELPOOLFULL`, `NOMOREROOM` and
     `BADVARNAME`.
   - **Names and string values:** any length (decision 1). Text kernel
     strings are no longer cut at 80 characters; `LMPOOL`'s buffer parser
     no longer writes past the end of a value (SPICELIB bug).
   - **Order:** `GNPOOL` and `WRPOOL` list the variables in the order they
     were created (SPICELIB: its hash table's order); `WRPOOL` writes a long
     name whole (a line over 132 characters cannot be read back whole).
   - **`szpool`:** reports `huge(1)` for every limit (decision 4).
   - **Watches:** agents and watched names have any length; DWPOOL moves
     the last watch into the deleted one's place instead of shifting all
     of them.
   - **Parsers** (ZZRVAR, ZZRVBF): SPICELIB's tokenizing and checks, with
     the storage replaced; ZZGPNM and ZZCLN are gone.
   - **Tests:** `test_limits` (100000 variables, 10**6 values, 200-character
     names and values, a watch on a long name, `szpool`, re-creating a
     deleted variable); tspice `F_POOL` (whole 81-character strings, long
     names accepted), `F_TKFRAM` and `F_AAAAPHSH` (SPICELIB's MAXVAR as a
     constant: they sized loops and a hash divisor with `szpool`).
   - **Measured** (both builds in temporary worktrees): `gdpool` 0.45 to
     0.35 µs, `pxform` on TK frames 14 to 6.8 µs, body names loaded 1.43 to
     1.21 µs per name, text kernels unchanged (1.2 µs per variable).
4. **Names and definitions.** Done so far:
   - **FRAMEX** (per context): the caches of kernel pool frames are a
     `string_map` and an `int_map` over growable arrays (SPICELIB: hash
     tables of 5209 entries, about 550 KB per context). Fixes `HASHISFULL`
     and NAMFRM's cache, which never hit. Test: 20000 TK frames in
     `test_limits`.
   - **Watches:** an agent is found through a map (it was a linear search
     over the context's watches; TKFRAM keeps up to 200).
   - **Measured:** `pxform` on a random TK frame 6.8 to 5.3 µs (14 µs before
     phase 3).
   - **Body names** (ZZBODTRN, per context): the kernel pool names and the
     built-in and BODDEF definitions are `body_names` records (names and
     codes in arrays that grow, a name map and a code map, SPICELIB's
     precedence). `KERVARTOOBIG` (14983 kernel names) and `TOOMANYPAIRS`
     (853 definitions) are gone; names have any length (the kernel names
     are read whole from the pool; SPICELIB's MAXL was 36). The 540 KB
     `kernor` locals of four routines are gone. tspice's ZZBODKER adapter
     keeps SPICELIB's interface for `F_ZZBDKR`. Tests: `test_limits`
     (20000 kernel names, 500 BODDEFs, 60-character names); tspice
     `F_BODCOD` (past NROOM and MAXE).
   - **Surface names** (ZZSRFTRN, per context): a `surface_names` record
     (names, IDs and bodies in arrays that grow; maps keyed by (name,
     body) and (ID, body), SPICELIB's precedence). `TOOMANYSURFACES`
     (2000) is gone; names have any length (SFNMLN was 36). ZZSRFINI is
     gone; tspice's ZZSRFKER adapter keeps SPICELIB's interface.
   - **Surface lists in method strings:** no limit (MAXSRF was 100). The
     12 saved lists are allocatable and grow before their ASSOCIATE
     blocks (to `len(method)/2+1`: a surface takes at least two
     characters); GF's lists are automatic arrays of that size; the
     MAXSRF checks of ZZDSKSIT and ZZSUDSKI are gone. ZZPRSMET's work
     string and token arrays are sized from the method (SPICELIB: 1000
     characters, cut without an error, and 100 tokens).
   - **Tests:** `test_limits` (3000 surface names, a 60-character name, a
     150-surface method string of about 1100 characters).
   - **SCLK type 1** (SC01, per context): the database's buffers and
     clock table grow (an `int_map` of the clocks; SPICELIB: 100 clocks in
     a hash table, emptied when full, and DBUFSZ numbers, about 2.6 MB
     per context). No limit on coefficients (MXCOEF 100000 records,
     `TOOMANYCOEFFS`) or partitions (MXPART 9999, `TOOMANYPARTITIONS`,
     also SCLD01's `SIZEOUTOFRANGE`). ZZSCIN01, ZZSCAD01 and ZZSCUP01
     work on the state and run before the type 1 routines' ASSOCIATE
     blocks; tspice keeps copies of SPICELIB's array versions for
     `F_ZZSC01`. SCENCD, SCDECD and SCPARS hold the partitions in arrays
     of the clock's size (their saved MXPART work arrays, 240 KB per
     routine and context, are gone; SPICELIB copied the partitions in
     before checking their number). SCDECD formats any partition number
     (PARTLN was 5 characters: 4 digits and the slash). Tests:
     `test_limits` (100500 records, 12000 partitions, a string in
     partition 10001, 150 clocks); tspice `F_ZZSC01`, `F_SCLK0`.
   - **Switch frames** (ZZSWFXFM): the base-frame arrays of the frame
     table start at 64 entries and grow to the size of the frame's
     `FRAME_<name>_ALIGNED_WITH` variable before it is fetched (MAXBAS
     15000, `TOOMANYBASEFRAMES`, is gone from the library). ZZSWFFET takes
     an optional room argument so tspice `F_ZZSWFXFM` still tests the
     legacy error; its case 16 (15001 base frames) now succeeds.
   - **TISBOD and BODEUL:** no limit on nutation/precession angles (MAXANG
     200). The coefficient arrays are sized to the body's kernel variables,
     and TISBOD's cache keeps each body's angle data in its own allocatable
     record (SPICELIB held 8 arrays of MAXANG for each of 157 bodies, about
     1.5 MB per context, plus BODEUL's saved work arrays). Test:
     `test_limits` (300 angles agree with 1 angle when the other
     coefficients are zero).
   - **Frame names** (decision 1): no length limit (SPICELIB: 32
     characters, FRNMLN; longer names were truncated, so two frames whose
     names differ only after the 32nd character were the same frame).
     - FRAMEX keeps kernel frame names as strings, normalizes the name
       in a copy of its own length, and reads `FRAME_<ID>_NAME` and
       `OBJECT_<ID or name>_FRAME` at full length (`pool_string`, a new
       accessor). FRMNAM, CIDFRM and CCIFRM are wrappers of
       `frame_name_of`, `frame_of_center` and `frame_of_class`, which
       return allocatable names; the library uses those.
       `body_name_of` does the same for ZZBODC2N.
     - The ZZDYNxxx routines share one lookup of
       `FRAME_<ID or name>_<item>`, `dynamic_frame_variable`, with no
       length limit (`VARNAMETOOLONG` is gone); ZZDYNVAC's body is
       `dynamic_frame_strings`, which returns the values at full length
       (product frames).
     - TKFRAM, the four dynamic-frame evaluators, ZZSWFFET, KPLFRM,
       ZZNOFCON, TISBOD, ILLUM, SUBPT, SUBSOL, SRFXPT, ET2LST,
       ZZDSKSPH, ZZDSKSBA, OCCULT and the GF searches use allocatable
       names; the frame names saved by the GF searches, DSKXSI, DSKXV
       and ZZCVSTAT are allocatable components.
     - ZZNAMFRM (the "last name looked up" caches of about 40 routines)
       does not save a name longer than the caller's 32-character
       buffer: a truncated name could match a different, shorter name.
       Long names are looked up on every call (a hash lookup).
     - CCIFRM and KPLFRM fetch the matching variable names once
       (`pool_names`); CCIFRM examined each name up to 100 times (see
       `spicelib_bugs.md`).
     - Test: `test_limits` (TK, dynamic, product and switch frames whose
       names differ only after the 32nd character; CIDFRM, CNMFRM,
       CCIFRM, KPLFRM; states in those frames).
   - **GF method strings:** no length limit. OCCULT and GFOCCE copied the
     shape/method strings into 500 characters (MTHLEN), ZZGFOCIN saved
     them in 500 (`svfmth`, `svbmth`), ZZGFCOIN (GFSUBC, GFSNTC) and
     ZZGFILIN (GFILUM) in 200: a longer DSK method (a long
     surface list) was cut, a syntax error or the wrong surfaces. The
     copies and the saved strings are allocatable (the saved ones are
     referenced directly, not through ASSOCIATE). Test: `test_limits`
     `test_occult_methods` (OCCULT with a DSK method of 61 surfaces,
     1487 characters; SPICELIB: SPICE(SYNTAXERROR) at index 486).
   - **Still fixed, speed only:** the geometry routines save the previous
     method in a fixed buffer (`prvmth`, 500); a longer method is parsed
     again on every call.

   The plan for the rest:
   - **Bodies:**
     - ZZBODTRN's kernel-name table is sized to the pool's contents;
     - the BODDEF table grows (`TOOMANYPAIRS` goes);
     - the 540 KB `kernor` locals go;
     - `f_bodcod` and `f_zzbdkr` change.
   - **Frames:** the FRAMEX kernel-frame cache uses the hash map and grows. This
     fixes `HASHISFULL`. Test: 20000 TK frames (done). Frame names of any
     length (done).
   - **Surfaces:**
     - ZZSRFTRN grows;
     - method strings take any number of surfaces (`maxsrf`);
     - the 13 `srflst(100)` state arrays become allocatable.
   - **Switch frames:** `maxbas` per frame (done).
   - **SCLK:**
     - partitions and coefficients per clock grow;
     - the `scencd`/`scdecd`/`scpars` work arrays follow;
     - `TOOMANYPARTITIONS` and `TOOMANYCOEFFS` go;
     - `f_zzsc01` changes.
   - **TISBOD and BODEUL:** `maxang` (done).
5. **Segment caches and memory** (done):
   - **Segment index** (`pepper_segment_index`): the segments of the
     loaded SPK, PCK, CK and DSK files are indexed when a file is loaded,
     by body (instrument for CK, central body for DSK), in SPICELIB's
     search order: the file loaded last first, and within a file the last
     segment first. The index is in the instance and only read by queries
     (kernels are loaded before queries run).
     - SPICELIB found a body's segments at its first query in each
       context, reading the summaries of every loaded file (opening each),
       and kept them in per-context tables (SPK and CK: 100000 segments,
       about 10 MB each; PCK: 5000; DSK: 10000 and 100 bodies). Those
       tables, their eviction and their "search without buffering"
       fallback are gone: SPKSFS, PCKSFS, CKBSS/CKSNS, ZZDSKBSS/ZZDSKSNS
       walk the body's list. pepper_segments.f90 went from 12929 to
       about 6100 lines (NAIF's documentation kept).
     - A context keeps only the segment returned last for each body and
       its re-use interval (SPK, PCK, CK), as SPICELIB did, in a map that
       grows; and the CK and DSK search cursors.
     - Loading reads all the summaries of the file (SPICELIB read them
       lazily). A file whose summaries cannot be read is not loaded.
     - The DSK segment table cannot overflow: `BUFFEROVERFLOW` is gone
       (`F_ZZDSKBSR` cases 30, 32 and 36 changed).
     - Test: `test_limits` `test_search_order` (overlapping segments in
       and across files, re-use intervals, a file loaded twice, unloads;
       CK segments without data for a covered time) passes with the old
       code too; `test_many_spks` now queries all 6000 bodies.
   - **ZZDSKSBF** (per context): its segment table grows to hold the
     segments of the bodies it buffers (SPICELIB: 10000 segments, 2.8 MB
     per context, `SEGMENTTABLEFULL`; ZZDSKSBA keeps the check for callers
     that pass their own arrays, as `F_SBF` does). The body table (10)
     stays.
   - **Unit table**: the files a context keeps open are an instance
     setting, `spice%set_max_open_files(n)` (default 23, SPICELIB's
     UTSIZE; at least 4), read when a context first opens a file.
     ZZDDHGTU and ZZDDHF2H take the table's size as an optional argument
     (default UTSIZE, for `F_DDHGTU` and the legacy interface). Test:
     `test_limits` `test_open_files`.
   - DAFFA follows DAFAH (done in phase 2).
   - **Measured** (`bench-limits` full, release, both builds in temporary
     worktrees, same session; µs per operation):

     | Operation | Before (`7ac7cc5`) | After |
     |---|---:|---:|
     | `furnsh` (SPK) | 76.7 | 68.9 |
     | `spkgeo`, one body | 0.58 | 0.38 |
     | `spkgeo`, a random body among 2000 files | 205 | 13.8 |
     | the same, `set_max_open_files(2016)` | | 0.80 |
     | `unload` (SPK), 2000 loaded | 8.1 | 7.3 |
     | first `spkgeo` in a new context | 1560 | 23 |

     Whole run (100 contexts): 157 s → 129 s (system time 40 s → 9.7 s),
     peak memory 1.11 GB → 0.60 GB. With 23 open files, the random query
     is now the cost of reopening a file (the setting removes it).
6. **Algorithm work buffers.** Done:
   - **DSK ray intercepts** (DSKX02, per context): SPICELIB's work
     arrays (50000 voxels on a ray, 256000 plates in a voxel group, the
     100000-entry coarse grid: 3.4 MB per context, and 1.6 MB more on the
     stack) are allocatable. The coarse grid and the voxel lists are sized
     from the segment's grid when a segment is first used (XDDA steps one
     voxel along one axis at a time, so a ray crosses at most `nx+ny+nz`
     voxels; that memory is proportional to the segment's own voxel data);
     the plate lists grow with the plates read. `ARRAYTOOSMALL` is gone.
     XDDA keeps its interface (the caller's room).
   - **DSK segments containing a point** (ZZDSKBUN, ZZDSKBUX): the hit
     buffers are sized by the number of segments searched (SPICELIB: 1000,
     `TOOMANYHITS`, `BUFFERTOOSMALL`; 88 KB on the stack).
   - **Tangent points** (LIMBPT, TERMPT through ZZTANGNT and ZZTANSLV):
     the window of transitions and the points in a cutting half-plane
     grow (SPICELIB: 2000, `ARRAYTOOSMALL`/`WINDOWEXCESS`). ZZTANSLV
     and ZZTANGNT take allocatable arrays; ZZTANSLV's optional `room`
     keeps SPICELIB's errors for the legacy interface (`F_ZZTANSLV`
     cases 16 and 17). New `grow_cell` (pepper_containers) grows a
     double precision cell or window.
   - **FOV vertices** (GF FOV searches, FOVRAY, FOVTRG through
     ZZGFFVIN; ZZELVUPY): the boundary vectors are held for as many as
     the instrument's kernel variable has (SPICELIB: `MAXVRT` = 10000,
     `BOUNDARYTOOBIG`; 400 KB per context), and ZZELVUPY's work array
     grows to the number of vectors (`MAXFOV` = 10000, `INVALIDCOUNT`;
     240 KB). GETFVN keeps its caller's room (phase 7).
   - **Leapseconds:** DELTET's buffer grows (SPICELIB: 200,
     `BUFFERTOOSMALL`), and so does TTRANS's table (SPICELIB: 140, the
     later ones silently dropped, so UTC strings disagreed with DELTET;
     `spicelib_bugs.md`).
   - Tests (`test_limits`): `test_leapseconds` (300), `test_fov_vertices`
     (12000 boundary vectors: FOVRAY and ZZELVUPY), `test_dsk_hits` (1100
     segments containing a point: SRFNRM and DSKXV). Not tested past the
     old limit: the DSKX02 lists (a ray through more than 50000 voxels, or
     256000 plates in a voxel group, needs a very large shape model) and
     the 2000 tangent points in one half-plane.

   - **Thread safety** (found by `pixi run test-parallel`, which failed
     in about half of its runs since the phase 4 string helpers): gfortran
     keeps the length of a deferred-length character function result
     (`character(:), allocatable` result) in a static variable at each
     call site, shared by the threads. FRAMEX's kernel variable names
     (`int_text`, `marker_text`, `frame_variable`, phase 4) came out with
     other threads' lengths, so lookups failed and the heap was corrupted.
     The 13 such functions of the library (also the error messages,
     `ctx%message`, file names, surface keys, WRPOOL's quotes) return
     strings of a length computed from their arguments, or are
     subroutines. `tools/check_threads.py` (`pixi run check-threads`, run
     by `pixi run test`) rejects such functions in the sources; `pixi run
     check-threads-ir` lists any uninitialized static in the generated
     code (none). `test-parallel`: 0 failures in 20 runs.

   - **EK work areas** (step A of the EK work):
     - The EK scratch area (ZZEKSCA, per context) keeps SPICELIB's
       layout: up to 2.5 million integers in memory, the rest in a
       scratch DAS. The in-memory part grows as the stack does (SPICELIB:
       10 MB allocated by the first EK query of a context), the scratch
       DAS is opened when the stack first goes past it (SPICELIB: on the
       first push, a file per context even for small queries), and a
       clean-up clears only the part in use (SPICELIB: all 10 MB on every
       clean-up). Test: `test_limits` `test_ek_scratch` (3 million
       integers: reads, updates and pops across the boundary).
     - ZZEKJSRT (sorting query results): the work arrays are allocated
       for the rows being sorted and freed after (SPICELIB: saved arrays
       for 250000 rows, 12 MB per context). Beyond 250000 rows it still
       sorts through the scratch area, as SPICELIB does.

   - **Loaded EKs** (EKQMGR, instance; step B of the EK work): the
     five linked list pools (files, segments, column descriptors, column
     attributes, tables) and their parallel arrays are allocatable and
     grow: EKLEF makes room for the whole file (its segments and their
     columns) before loading it (SPICELIB: 20 files, 200 segments, 100
     tables, 500 column attribute entries, 10000 column descriptors, 0.6
     MB in the instance; EKFILETABLEFULL, EKSEGTABLEFULL,
     EKTABLELISTFULL, EKCOLATTRTABLEFULL, COLDESCTABLEFULL). The
     per-table list of files (TBFILS) is gone: SPICELIB filled and read
     it wrongly, and unloading worked only through the segment handles
     (`spicelib_bugs.md`). The nine copies of the pool initialization
     are one helper. Test: `test_limits` `test_ek_tables` (25 files, 250
     segments, 126 tables, 6300 column attribute entries, 12500 column
     descriptors; queries, unloading 10 files, loading one again).

   - **EK query text and parsing** (step C1 of the EK work): the
     encoding of a query no longer has room only for the largest query.
     The query is scanned whole (SPICELIB: copied into 2000 characters,
     the rest cut off silently); the scanner's buffers and the encoded
     query's string and number parts are sized from the query's length
     (a token per character at most; SPICELIB: 500 tokens, 100 numbers,
     2000 characters); the integer part of the encoded query grows as
     ZZEKPARS and ZZEKNRML write it (SPICELIB: EQIMIN elements). The
     encoded query's layout was already count-based (each part found
     from the counts of the parts before it), so its readers are
     unchanged. ZZEKNRML's work arrays are local, sized from the token
     count, and its pools grow (SPICELIB: 0.4 MB per context, "Relation
     table is full." and the like); the normalized WHERE clause is
     checked against MAXCON (1000 constraints), which SPICELIB wrote past
     (`spicelib_bugs.md`), until step C2 sized EKSRCH's constraint
     arrays (the check is gone). ZZEKQINI has no minimum sizes; ZZEKPARS, ZZEKNRML and
     ZZEKENCD take the integer part as an allocatable (the legacy
     interface copies SPICELIB's cell in and out). Test: `test_limits`
     `test_ek_queries` (a 2700-character, 600-token WHERE clause; one
     with 64 conjunctions of 7 relations; the pools were also checked
     starting from 2 nodes).

   - **EK query counts** (step C2 of the EK work): no maximum number of
     FROM tables (MAXTAB, MXJOIN: 10), SELECT columns (MAXSEL: 50), ORDER
     BY columns (MAXORD: 10), constraints (MAXCON: 1000), join
     constraints in a conjunction (MXJCON: 100) or join row sets
     (conjunctions, MXJRS: 200). The arrays sized by them are sized for
     the query: EKQMGR's per-query arrays in the context state
     (ekqmgr_query_room, before EKSRCH's associate) and EKSRCH's locals,
     ZZEKVSET's join row set arrays and ZZEKJPRP's join constraint arrays
     (state, grown), and the row and segment vectors of EKNELT, EKGC,
     EKGD, EKGI, ZZEKJSQZ, ZZEKJSRT, ZZEKWEED, ZZEKJOIN, and the table
     lists of ZZEKPCOL, ZZEKNRES, ZZEKKEY's constraint arrays (locals,
     allocated with the count). The parse errors for too many tables,
     SELECT or ORDER BY columns are gone, and C1's check of the
     constraints against MAXCON. ZZEKJOIN's check of the first join row
     set's table count tested the second's (`spicelib_bugs.md`). Test:
     `test_limits` `test_ek_queries` (250 conjunctions, 1024 constraints,
     60 SELECT and 12 ORDER BY columns, a join of 11 tables with 120 join
     constraints in one conjunction).

   - **EK string comparisons** (step C3 of the EK work): strings are
     compared whole (SPICELIB: their first MAXSTR = 1024 characters, in
     ZZEKECMP, ZZEKSCMP, ZZEKVMCH and ZZEKFRX, so that strings agreeing
     that far were equal for =, <, LIKE, ORDER BY and joins, and the value
     in a query was cut too). The four routines read column entries with
     `zzekrsc_whole`: a buffer of the column's declared length, or for
     variable-length strings 1024 characters to start, read again at the
     string's length if longer. ZZEKJSRT failed (SPICE(STRINGTRUNCATED))
     to order by a fixed-count string column longer than its 32-character
     sort keys; it reads such strings whole. ZZEKRSC now sets the length
     of class 6 strings (`spicelib_bugs.md`). Compatibility: an index
     that SPICELIB built row by row (EKACEC) orders strings agreeing in
     their first 1024 characters by record, not by the rest of the string
     (the fast loader sorted them whole); lookups by such strings in such
     a file may miss. Test: `test_limits` `test_ek_strings` (1201-character
     strings agreeing in their first 1200: =, LIKE, ORDER BY, joins, on
     indexed, plain and 1500-character fixed-length columns, a deleted
     indexed row; ORDER BY a 40-character fixed-count column), all
     failing on the previous code. tspice: F_EK04 and F_NNEK04 cases 5
     to 7 (ZZEKSCMP on strings that agree in their first 1024
     characters) expect the whole-string results.

   Phase 6 is done.
7. **API for caller buffers** (E; design: [`phase7_design.md`](phase7_design.md)):
   - **Allocatable outputs:** variants like `gdpool(name, values)` with
     `values` allocatable, `bodvrd`, `getfov`, `spkcov` into a window type,
     and GF searches with allocated work windows.
   - **The legacy wrappers** in tspice keep the cell versions, possibly built
     on the new ones.
   - **What stays:** the fixed-size interfaces, for compatibility.

Phases 1 to 3 can be done in any order after 0. Phase 2 has the most
user-visible value (the number of kernels). Phase 5 has the largest memory and
parallel-startup gain, and it depends on the containers from 0.

## Baseline

`pixi run bench-limits` (test/core/bench_limits.f90), before any change
(at `fb40b19`), release build, macOS arm64, gfortran 16, under
`/usr/bin/time -l`. Microseconds per operation:

Note: compare only builds made in the same place. A release build in the
repository's `build/` ran these operations up to twice as slowly as the same
code built in a temporary worktree under `/private/tmp` (both old and new
code; the cause is not known). The table below is from the repository; the
comparison after phase 2 (below) has both builds in temporary worktrees.

| Operation | n | µs |
|---|---:|---:|
| `furnsh` (SPK, one segment each) | 2000 | 119 |
| `spkgeo`, one body | 200000 | 1.0 |
| `spkgeo`, a random body among all files | 200000 | 192 |
| `unload` (SPK) | 2000 | 16693 |
| `furnsh` (text kernel), per variable | 20000 | 2.1 |
| `gdpool`, a random variable | 200000 | 0.45 |
| `furnsh` (body names), per name | 10000 | 1.4 |
| `bodn2c`, a random kernel name | 200000 | 0.55 |
| `bodc2n`, a random kernel code | 200000 | 0.34 |
| `pxform`, each TK frame once | 2000 | 23 |
| `pxform`, a random TK frame | 200000 | 13.5 |
| `new_context` | 100 | 0.67 |
| first `spkgeo` in a new context | 100 | 2501 |

Whole run: 77 s (19.6 s user, 55.5 s system), peak memory 2.19 GB.

What it shows:

- **Unloading is quadratic:** 16.7 ms per file with 2000 loaded (33 s in
  total). This is SPICELIB's design, not a table size: after unloading an
  SPK, KEEPER reloads every remaining SPK (`spklef`) to restore the
  priority order, and each reload opens the file to identify it
  (`zzddhopn`/`zzddhf2h`). CK, DSK and PCK do the same.
- **Queries spread over many files are 190 times slower** than repeated
  queries of one body. The likely cause (to confirm with a profile): the
  unit table keeps 23 files open, so most queries close and reopen a file,
  which would also explain the system time.
- **A context costs about 20 MB and 2.5 ms** once it makes its first SPK
  query (the fixed-size BSR tables are allocated and zeroed): 100 contexts
  take 2 GB.
- **TK frames use the pool:** each takes 10 variables, so 26003 pool
  variables limit a kernel set to about 2600 TK frames, well before the
  frame cache's 5209.

### After phase 2 (DDH, DAF, DAS, DAFFA, BSR file tables, KEEPER)

Both builds in temporary worktrees, same session; µs per operation (two
runs each):

| Operation | Before (`333d736`) | After |
|---|---:|---:|
| `furnsh` (SPK) | 74–77 | 68 |
| `spkgeo`, one body | 0.58 | 0.55 |
| `spkgeo`, a random body among 2000 files | 216–218 | 201–202 |
| `unload` (SPK), 2000 loaded | 21036–21302 | 8.1 |
| `gdpool` | 0.45–0.46 | 0.44–0.47 |
| first `spkgeo` in a new context | 2489–2692 | 1319–1436 |

Peak memory of the full run (100 contexts): 2.19 GB before, 1.15 GB after.

## Inefficiencies

Things SPICELIB does slowly or with more memory than needed, found along the
way (not limits as such). Each gets fixed in the phase that touches the code,
or listed for later.

| Where | Cost | Fix | Phase |
|---|---|---|---|
| KEEPER `unload` | Reloads every remaining SPK/CK/PCK/DSK after an unload (one file open each): O(N²) opens to unload N files, 16.7 ms per unload at 2000 files | Done: reload only when the unloaded entry is a second load of a file | 2 |
| DAFAH `dafhsf`, DASFM | A linear search (DAF) or list walk (DAS) of the open files on every handle lookup; sets of the same handles kept twice, DASCLS copying the set on each close | Done: records sorted by handle, binary search | 2 |
| `closed_files` (PEPPER) | A list of every file ever closed, scanned on each open | Done: a string map | 2 |
| ZZDAFGDR, ZZDAFGSR, ZZDAFGFR, ZZDASGRD, ZZDASGRI | Copy the file's name (255 characters) from the handle manager on every record read, without using it | Done: not copied | 2 |
| DASCLS | Copies the set of all open DAS handles on every close, to test one | Done: one lookup | 2 |
| SPKBSR, CKBSR, PCKBSR, ZZDSKBSR | The first query of a body in a context reads the summary records of every file loaded after the one holding it (opening each: the unit table keeps 23 open): O(N) file opens. Measured: a random body among 2000 SPKs, 200 µs against 0.55 µs for a known one; the first queries in `test_limits` (6000 SPKs) took minutes when every 7th body was queried | Done: the segment index (205 → 13.8 µs; the rest is reopening files) | 5 |
| DAFFA `dafbfs`, `dafbbs` | Walk the per-context list of searched DAFs (most recent first) to find the handle: O(DAFs searched) per search | Moot for the segment searches: they no longer search DAFs; loading searches the file just opened, the head of the list | 5 |
| ZZDDHOPN (`zzddhf2h`) | Opens a file to identify it (inode/name) on every load, including reloads | Follows from the unload fix; consider identifying by name first | 2 |
| SPKBSR + unit table | A random query among 2000 SPKs takes 192 µs against 1 µs for one body; probably the 23-unit LRU closing and reopening files (to confirm with a profile) | Done: `set_max_open_files` (13.8 → 0.80 µs with every file open) | 5 |
| `wrline` (error reports, `pepper_output`) | Looks up the file by name (`inquire`) for every line written | Keep the unit with the device in the context | later (only when reports go to a file) |
| POOL (GDPOOL, GCPOOL, GIPOOL, DTPOOL) | Values in linked lists: a lookup walks the list from its head to START, and DTPOOL counts the values by walking them | Done: arrays (slices) | 3 |
| POOL (GNPOOL, WRPOOL) | Scan all 26003 hash buckets on every call; CCIFRM and KPLFRM page through the matches with START, one scan per page | Done: the slots in use | 3 |
| Watches (`watch_index`, PEPPER) | Find an agent by a linear search over all watches of the context; TKFRAM keeps one watch per buffered frame (up to 200) and calls CVPOOL on every lookup | Done: a map from agent to watch | 4 |
| FRAMEX (NAMFRM) | Adds kernel frame names to its cache as `FRAME_<NAME>` but looks up `<NAME>`: every lookup of a kernel frame by name reads the pool | Done: cached under the name | 4 |
| ZZBODTRN (BODDEF, `sync_boddefs`) | Each definition rebuilds the maps of all the definitions (SPICELIB's ZZBODINI), and a context replays all of the instance's BODDEFs when they change: O(n**2) for n definitions | Done: a new name is added to the maps (they are rebuilt only when a redefinition removes an assignment), and a context applies only the definitions made since it last looked (unless there was a reset). `bench-limits` (10000 names): BODDEF 835 → 0.64 µs per name; the first query in another context 815 → 0.31 µs per name; one more definition then seen by another context 8 s → under 1 µs | 6 |
| ZZBODTRN, FRAMEX (per context) | Every context builds its own tables of body names and kernel frames from the pool; ZZBODTRN rebuilt its table after any change of the pool (phase 3 replaced SPICELIB's watch of NAIF_BODY_NAME and NAIF_BODY_CODE by the pool's state counter, when watches were shared between contexts) | Done: ZZBODTRN watches the two variables again (watches are per context since phase 4). `bench-limits` (10000 kernel names): a lookup after a change of another variable 2760 → 1.3 µs. Not done: building the tables once per instance. The first lookup in a new context costs 2.8 ms with 10000 kernel names (it is proportional to the number of names), once per context and change of the names; FRAMEX builds its caches frame by frame (the first PXFORM in a new context: 51 µs with 2000 kernel frames). A table shared by the contexts would need a lock on its first build (the library has no OpenMP), or a build when kernels are loaded, which moves ZZBODKER's errors from the query to the load | 6 |
| BSR tables | 10 MB each, allocated and zeroed in full by the first query of a context: 2.5 ms and about 20 MB per context | Done: replaced by the index; a context keeps a growing re-use map (first query 1560 → 23 µs) | 5 |
| CCIFRM | Scans the pool with GNPOOL in batches of 100 and, because GIPOOL overwrites the batch count, restarts each batch one name later: each `FRAME_*_CLASS` name is examined up to 100 times, and each batch is a full pool scan | Done: names fetched once | 4 |
| DELTET | Reads its five kernel variables (and copies all the leapseconds) on every call: every UTC/ET conversion; for ET epochs it also computes the ET of every leap epoch (two sines each) on every call | Done: read when the pool changes (pool counter), the ET of each leap epoch computed then; results bitwise identical. `bench-limits` (28 leapseconds): UTC 0.234 → 0.041 µs, ET 0.360 → 0.043 µs | 6 |
| STR2ET (TPARTV, ZZTPATS) | 21.6 µs per call, 45 times ET2UTC: 90 percent of it was ZZTPATS building and sorting TPARTV's table of 231 time-string patterns (ORDERC, REORDC) on every call. SPICELIB did it once and saved the table; PEPPER had dropped the saved copy | Done: the table is a constant in the sorted order (bitwise the same table; `test_time` checks ORDERC leaves it unchanged). 21.6 → 1.8 µs. The rest is SPICELIB's tokenizer (TPARTV ~75%, mostly POS from ZZSUBT): an algorithm rewrite, not worth it at this cost | 6 |
| FURNSH of a binary kernel; of a text kernel (ZZDDHF2H, GETFAT; the pool) | A binary kernel was opened four times per load: ZZDDHF2H (through GETFAT, then ZZDDHOPN) opens it to compare it with the loaded files, and GETFAT opens it to read its ID word. The pool hashed each new name twice (find, then set) with a `mod` per character, re-trimmed the 132-character key at each probe, and CLPOOL (every unload of a text kernel) freed the slots and the index | Done: during one ZZLDKER, ZZDDHF2H keeps its result for a file not in the table, with the start of its first record, until the table changes (two opens per load); `string_map_insert` (one lookup), a masked hash, keys trimmed once, CLPOOL keeps the capacity. `pixi run compare-spicelib` (fastest of 7 runs): an SPK 45.2 µs per file (SPICELIB -O3: 67), TK frames 6.07 µs per frame (9.48; 6.5 in an earlier run) | 6 |

## Testing

- **Past every limit:** a test in `test/core` for each removed limit, at least
  twice the old value, through the public API. The kernel generator keeps
  these fast. Large cases (10000 kernels) can be a separate `slow` test if
  needed.
- **tspice:** cases that assert an old limit (`F_DDHOPN`, `F_DDHFTSIZE`,
  `F_PCKBSR`, `F_ZZDSKBSR`, `F_SBF`, `F_BODCOD`, `F_ZZBDKR`, `F_ZZSC01`,
  `F_SRFTRN`, `F_ZZSWFXFM`, `F_EK04`, `F_NNEK04`) change to expect success, each with a PEPPER note.
  Tests of tspice's own copies of SPICELIB code (`t_spkbsr`, `t_ckbsr`,
  `f_zzhsc`, `f_zzhsi`) are not affected.
- **Untested today, tested after:** `KERNELPOOLFULL`, `NOMOREROOM`, `DASFTFULL`
  (headers only in `f_das`), the FRAMEX cache, and the 100-surface method
  string.
- **Growth under `associate`:** a debug build with `-fcheck=all` and
  `-fsanitize=address` runs the growth tests.
- **Parallel:** `test_parallel` loads more than the old limits, then queries
  from all threads, and compares bitwise with a serial run.
  `pixi run test` builds it without OpenMP (one thread): run `pixi run
  test-parallel`, several times (a race shows in some runs only), and
  `pixi run check-threads-ir` after changes to string-returning code.

## Decisions

Made (2026-11-09):

1. **Name lengths (D): no limit.** Body, surface, frame and pool variable names,
   and pool string values, are stored as allocatable strings with no maximum.
   The 36/32/80 constants remain only where a fixed-length output is part of an
   existing interface; a caller's shorter variable truncates as any Fortran
   assignment does.
2. **Text kernel lines: keep 132 for now.** Document it (the `furnsh`/`ldpool`
   docstrings and the user-facing docs), including what happens to a longer
   line today.
3. **Caches: no large static arrays.** The principle: nothing is allocated at
   a fixed size that may not be used; every table starts small and grows on
   demand.
   - **No cap** for caches bounded by the loaded data: the SPK/CK/PCK/DSK
     segment and body tables, TKFRAM, SC01 and the FRAMEX kernel-frame cache
     (which also ends its `HASHISFULL` failure).
   - **An instance setting** only for the unit table (open files per context),
     whose bound is the OS limit on open files; default 23.
   - **Unchanged:** the small fixed buffers tuned for speed (DAFRWD and DASRWR
     record buffers, DASA2L).
4. **`szpool`: reports unlimited** (`huge(0)` for the sizes that no longer have
   a limit).
