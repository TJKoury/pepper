# Tables shared by the contexts of an instance

Each context builds its own copies of tables that are the same in every
context: the body names, the kernel frames, the leapseconds, the TK frame
rotations, the PCK body constants, the SCLK database, and several tables of
built-in constants. Most are derived from the kernel pool, which already lives
in the instance. This plan moves them into the instance, built once and read
by every context. The goals:

1. **Cheap contexts.** A new context's first query no longer rebuilds
   anything. Today, with 10000 kernel body names, the first `bodn2c` in a new
   context costs 2.8 ms; with 2000 kernel frames, the first `pxform` costs
   51 µs (limits.md, `bench-limits`). Thread pools and Python threads, which
   make many contexts, pay this cost per thread and per change of the
   kernels.
2. **Smaller contexts.** One copy per instance instead of one per thread.
   Fixed-size buffers such as TISBOD's 157 bodies, TKFRAM's 200 frames and
   FRAMEX's hash tables go away from the context.
3. **The same results and errors.** The results are bitwise the same, every
   error is signaled by the same query as before, and tspice is unchanged.

limits.md lists the per-context name tables as "not done" (Inefficiencies,
ZZBODTRN/FRAMEX row). The obstacle it names is that a table shared by the
contexts would need a lock on its first build, and the library has no locks.
This plan avoids the lock (see Design).

## Inventory

**Derived from the kernel pool or the instance, rebuilt per context:**

| State | What each context builds | Invalidated by |
|---|---|---|
| `zzbodtrn` | name↔code maps of the built-in names plus BODDEF (`def`), and of NAIF_BODY_NAME/CODE (`ker`). BODDEFs are replayed from the instance's list | BODDEF / `define_body` (boddef_epoch), the two pool variables |
| `framex_state` | caches of kernel frames: name→ID and ID→(name, center, class, class ID), filled one lookup at a time, emptied on any pool change | `FRAME_*` variables |
| `tkfram_state` | buffer of 200 TK frame rotations, one pool watch per buffered frame | `TKFRAME_<id>_*`, `FRAME_<id>_*` |
| `tisbod_state` | buffer of 157 bodies' PCK orientation constants (RA, DEC, W polynomials, nutation-precession terms) | `BODY<id>_*` (and `BODY<bc>_NUT_PREC_ANGLES`) |
| `sc01_state`, `zzsclk_state` | the SCLK type 1 database (coefficients, partitions, fields) per clock, and the sets of clocks checked | `SCLK*_<id>` |
| `deltet_state` | the five DELTET values, the leapseconds and their ET epochs | `DELTET/*` |
| `ttrans_state` | TAI at the leapsecond days (`taitab`, `daytab`) | `DELTET/DELTA_AT` |
| `zzswfxfm_state` | switch frames' base frame lists (a cache of 1013 frames) | `FRAME_<id>_*` |

**Built-in constants, set up per context on its first call:**

| State | What |
|---|---|
| `zzbodblt` | the built-in body names (`nperm`), normalized and sorted |
| `framex_state` (the `b*` arrays) | the built-in frames, with their name and ID hash tables (`maxbfr`) |
| `chgirf_context_state` | the rotations from J2000 to each built-in inertial frame |
| `ttrans_state` (`unifrm`, `recog`, ...) | the time system tables |
| `timout_state` (`marks`, ...) | the format mark tables |
| `zzdynfrm_state`, `zzwahr_state`, `zzmobliq_state`, `tisbod_state` (`d`, `t`, `j2code`) | names of kernel variables, and unit constants |

**Already shared (the precedent):** the segment index (phase 5 of limits.md;
contexts keep only a reuse map), the pool, the programmatic ephemerides and
frames, and the files held in memory.

**Stays per context.** This is state that is per thread by nature:
- record buffers (DAFRWD, DASRWR), unit tables, the segment reuse maps;
- the "previous call" memos (SC01's `prvsc`, TKFRAM's `at`, ZZSWFXFM's
  `prvfrm`, BODVRD's last name, IRFNUM's last frame);
- work variables, prepared queries, GF and EK state;
- user pool watches (SWPOOL/CVPOOL).

## Design

### Who writes the tables, and when

The library rule: kernel changes are `spice%` methods, and no query runs while
one is in progress. Contexts only read. The shared tables follow the pool:

- **Dirty flags.** The pool's change recorder (the code that logs
  `changed_names` and bumps `pool_changes`) marks the affected tables dirty, by
  the variable's name: `NAIF_BODY_*`, `FRAME_*`, `TKFRAME_*`, `BODY*`,
  `SCLK*`, `DELTET/*`. It also records which entries changed (frame ID, body
  ID, clock ID), so that the update is incremental. BODDEF and `define_body`
  mark the body names dirty. A CLPOOL, or a full change log, marks
  everything dirty.
- **Refresh while dirty.** A table is refreshed when it is read while dirty,
  or at the end of the public `spice%` method that dirtied it, whichever comes
  first.
  - At the end of a method, a `commit` refreshes every dirty table, so
    between `spice%` calls no table is ever dirty.
  - A table can be dirty only while a kernel change is in progress, and then,
    by the rule, nothing else runs. A read that finds it dirty is therefore
    always inside that change (FURNSH's own work, a loader's callback, a text
    kernel's `@` time needing leapseconds loaded earlier in the same
    meta-kernel), on the one thread that is changing the kernels. So it can
    refresh the table itself, without a lock.
  - This keeps today's order of effects within a meta-kernel: a later file
    sees the names, frames and leapseconds of an earlier one.
- **Incremental updates.** A change of `FRAME_1400001_CENTER` reparses frame
  1400001 only, and a change of `BODY399_PM` reparses body 399 only. A loop of
  10000 `pdpool` calls therefore costs O(10000), not O(10000²). Changes that
  can't be applied this way rebuild the whole table: NAIF_BODY_NAME/CODE
  (order matters, since the latest assignment wins), CLPOOL, and an
  overflowed change log.
- **Debug check.** A debug build checks, on entry to a query from a context
  other than the instance's own, that no table is dirty, and signals
  SPICE(BUG) if one is. Such a context exists only while no change is in
  progress, so a dirty table means a public `spice%` method forgot its
  `commit`. A unit test also walks the public mutating methods.

### Errors at the same query as before

SPICE reports a bad definition when it is used, not when it is loaded. For
example, ZZBODKER rejects mismatched NAIF_BODY_NAME/CODE at the next name
lookup, and TKFRAM rejects a bad `TKFRAME_<id>_SPEC` when the frame is
evaluated. tspice tests this. The shared tables keep that behavior:

- **A deferred error.** A table, or a table entry (one TK frame, one body,
  one clock), stores the short message, the long message and the
  traceback-relevant names of the error its build met. The query that reads
  the entry signals that error, exactly where SPICE would have.
- **Loads never fail** because of a derived table. FURNSH's own errors are
  unchanged.

### Reading without side effects

Contexts read the tables concurrently, so every read must be pure:
- no "last found" memo inside a shared map;
- no lazy allocation;
- no growth on lookup.

`string_map` and `int_map` lookups have to be checked for this. A memo, such
as "the previous clock", stays in the context.

### Built-in constants

These don't depend on the pool. Two options:
- **(a) Parameters** where Fortran can express them. A sorted name table is a
  constant that a tool can generate, as `data_to_parameter.py` does. Watch
  compile RAM: large array constructors cost gfortran memory (see the memory
  note on hoisting `pepper_instance_module` uses).
- **(b) Built once** when the instance is created, into a `builtin_tables`
  component. The hash tables and the normalized names suit this.

Recommended: (a) for small tables, (b) for the hashed ones. Either way the
per-context copies and their `first` flags go.

## Phases

0. **Measure.**
   - Add to `bench-limits`: a context's `storage_size` per state, after typical
     queries; the cost of the first query of each kind in a new context (name,
     frame, TK frame, PCK body, SCLK, UTC); and FURNSH of a large meta-kernel
     (where the cost moves to).
   - Baselines go into limits.md, Baseline.
1. **Infrastructure.**
   - Dirty flags and the changed IDs, from the pool's change recorder.
   - `commit` in every public mutating `spice%` method (FURNSH, UNLOAD,
     KCLEAR, LDPOOL, LMPOOL, P?POOL, DVPOOL, CLPOOL, BODDEF, `load_kernel`,
     `add_frame`/`remove_frame`, `add_ephemeris`, ...).
   - The deferred error type, the debug check, and the test of the method
     list. No table moves yet.
2. **Built-in constants** (ZZBODBLT, FRAMEX's built-ins, CHGIRF, TTRANS,
   TIMOUT, ZZDYNFRM, the unit constants). Low risk. This shrinks every
   context and removes the first-call setup.
3. **Body names.**
   - The instance holds the built-in plus BODDEF map and the kernel map,
     updated at BODDEF and `define_body` time, with kernel-owned names removed
     at unload.
   - ZZBODTRN reads them, and the per-context BODDEF replay and `zzbodtrn`
     copies go.
   - Precedence as now: kernel pool, then BODDEF, then built-in; the latest
     wins.
4. **Kernel frames (FRAMEX).**
   - A full index, built from the `FRAME_*` variables: name→ID, and
     ID→(name, center, class, class ID).
   - NAMFRM, FRMNAM, FRINFO, CIDFRM, CNMFRM and CCIFRM read it.
   - The per-context `knm`/`kid` caches go, and a pool change no longer
     empties every context's cache.
5. **Leapseconds** (DELTET, TTRANS's `taitab`/`daytab`). Small tables, but
   every UTC conversion in a new context reads the pool for them today.
6. **TK frames.**
   - The rotation and relative frame of each TK frame, computed by the
     existing TKFRAM code at refresh, with a deferred error per frame.
   - The 200-frame buffer and its per-frame pool watches go.
7. **PCK text constants (TISBOD).** Each body's parsed constants, by body ID,
   with a deferred error per body. The 157-body buffer goes.
8. **SCLK type 1 (SC01, ZZSCLK).** The database per clock, with a deferred
   error per clock.
9. **Then measure again.**
   - Candidates if they show up: switch frames (ZZSWFXFM), and the parsed
     definitions of dynamic frames (ZZDYNFRM reads them from the pool on
     evaluation).
   - Also a shared FRAMEX-style cache for `bodvrd`-style constant reads, if
     the pool reads show in profiles.

Each phase: `pixi run test`, `test_parallel` repeated (many contexts reading
the shared tables), tspice 399/399, the Python suites, and the bench numbers
before and after.

## Testing

- **Equivalence.** For each table, the same queries in a fresh context and in
  an old context, before and after a change, compared bitwise. This covers a
  meta-kernel whose later files use an earlier file's names, frames and
  leapseconds (refresh while dirty).
- **Errors.** Each deferred error is signaled by the same query, with the
  same messages, as before. tspice covers most of them: the F_ZZBODTRN,
  F_TKFRAM, F_FRAMEX, F_SCLK, F_DELTET families.
- **Parallel.** `test_parallel` creates contexts on all threads after each
  load and queries names, frames, TK frames, PCK bodies, SCLK and UTC
  conversions, compared with a serial run.
- **Incremental updates.** A change of one frame's or one body's variables
  updates only it. A test changes one, then checks the others are still
  answered from the table (a counter of rebuilds, in a debug build).

## Risks and decisions

- **Cost moves to loading.**
  - A program that loads many definitions and queries few now pays for
    building the whole table at the load.
  - The incremental updates keep this in proportion to what changed.
  - Phase 0's FURNSH benchmark and `compare-spicelib` show whether it
    matters. For the TK, PCK and SCLK tables, if eager parsing costs too much,
    a table can store just "changed IDs" and parse an entry on its first
    read while dirty. That variant has to be single-threaded, so it works
    only inside a change; between changes every entry must be parsed already.
- **The `commit` discipline.** A public mutating method without `commit` would
  leave stale tables for other threads. The debug check and the method-list
  test guard it.
- **Programs that break the rule** (querying from one thread while loading on
  another) are already undefined. Shared tables don't make that safe, and
  this plan doesn't try to.
- **To decide:**
  - parameters or instance-built tables for the built-in constants;
  - whether phases 6–8 parse eagerly or keep "parse on first read inside a
    change";
  - whether to also share BODVRD-style constant lookups (phase 9).
