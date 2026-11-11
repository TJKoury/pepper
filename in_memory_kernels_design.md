# Binary kernels in memory: design

**Status:** proposal; nothing implemented yet. The decisions D1–D5 at the end
are open.

## The problem

PEPPER reads binary kernels the way SPICELIB does: one 1 KB record at a time,
when a query needs it. Each context keeps the last 100 records in a buffer
(DAFRDR's `ctx%dafrwd`). A query whose record is not in that buffer reads it
from the file with a Fortran direct-access READ, a system call.

The data of a DE kernel or an Earth orientation kernel covering years spans
thousands of records. Queries at scattered epochs therefore miss the buffer
nearly every time. The comparison with ANISE
(`pepper_vs_spicelib.md`, "Compared with ANISE") measures the cost:

| Query (DE421, earth_latest_high_prec.bpc) | Random epochs over 20 years | Sequential epochs |
|---|--:|--:|
| SPKGEO Moon from Earth, PEPPER | 9.56 µs | 0.29 µs |
| PXFORM J2000 to ITRF93, PEPPER | 3.69 µs | 0.27 µs |
| The same queries, ANISE (whole files in memory) | 0.37 / 0.46 µs | 0.36 / 0.42 µs |

The arithmetic is as fast as ANISE's. The difference is the file reads. The
item is open in `limits.md` (Inefficiencies, "SPK and binary PCK record
reads").

## The proposal

When a binary kernel is loaded, optionally read the whole file into memory
once. All later reads of that file, in every context, are then array copies
from memory:

```fortran
call spice%furnsh('de440.bsp', in_memory=.true.)    ! the file: read whole, once
call spice%furnsh('big_ck.bc')                      ! read record by record, as now
call spice%furnsh('mission.tm', in_memory=.true.)   ! every binary kernel the meta-kernel lists
```

### Why an option at FURNSH, and why the whole file

- **The trade is per file.** Memory equals the file size: DE421 is 17 MB,
  DE440 114 MB, a mission CK can be gigabytes. A program typically wants the
  planetary ephemeris and Earth orientation in memory, but not a large CK
  queried over a short span. FURNSH is where a program says which files it
  uses, so that is where it says how.
- **The whole file at load fits the instance/context design.** Loading
  happens on the instance while no queries run, and contexts only read. A copy
  made by FURNSH, and never changed until the file is unloaded, can be read by
  any number of contexts at once with no lock. This keeps the rule that the
  library has no locks.
  - The alternatives are worse. A copy made lazily by the first query (for
    example, a segment's data on first use) would have contexts writing
    shared memory, which needs a lock.
  - Per-context lazy copies would multiply the memory by the number of
    contexts.
- **No memory mapping.** `mmap` would give the same effect without the
  copy, but it needs C interop, which the project excludes. Fortran's stream
  access reads the file in one READ, at disk speed.

### What changes

**1. The instance's file table holds the copy.** Each entry of
`spice%zzddhman%files(:)` (`ddh_file`, `pepper_states.f90`) gets:

```fortran
real(wp), allocatable :: image(:)   !! the whole file, when loaded in memory
                                    !! (DAF word address a is image(a))
```

The copy is the file's records in order, so DAF word address `a` (record
`(a-1)/128 + 1`, word `mod(a-1,128) + 1`) is simply `image(a)`.

**2. FURNSH makes the copy.** `spice_furnsh(this, file, in_memory)` gets the
optional argument:
- It sets an instance flag for the duration of the load.
- After ZZLDKER has loaded a DAF (SPKLEF, CKLPF or PCKLOF return its handle),
  a new routine `load_image(spice, handle)` fills the entry's image. It opens
  the file with `access='stream'`, `form='unformatted'`, and reads it with one
  READ into `image(1:size/8)`.
- For a meta-kernel, the flag applies to every binary kernel it lists (see D2).
- A file loaded twice shares one handle. If either load asks for memory, the
  file is in memory. The image stays until the last load of the file is
  unloaded, which is when the handle manager closes it.

**3. Reads come from the copy.** All reads of DAF data go through DAFGDA
(268 calls in the library). It calls DAFGDR, which calls DAFRDR (the
per-context buffer), which calls ZZDAFGDR (the READ).
- **DAFGDA:** if the handle's file has an image, copy
  `data(1:n) = image(baddr:eaddr)` and return. This skips the buffer, its
  linear search, the per-record copies and the handle checks. It is the fast
  path, used by all segment readers (SPKR*, PCKR*, CKR*, DSK type 2 index
  reads).
- **ZZDAFGDR:** if the file has an image, copy the record
  `image((recno-1)*128+1 : recno*128)` instead of the READ. This covers the
  other record readers (DAFGSR for summaries, DAFRDR's other callers) with
  one change.
- **Finding the image fast:** DAFGDA would look up the handle in the file
  table (a binary search) on every call.
  - A context keeps the last handle and the index of its entry.
  - The cache is valid while `files_seen == spice%file_generation`, so a
    load or unload resets it, through the existing `files_check`.

**4. Unload frees the copy.** The image is part of the file table entry.
When the handle manager closes the file (UNLOAD, KCLEAR), `ddh_remove` drops
the entry and its image. As now, no query may run during the unload.

**5. Files without a copy work as now.** Record-by-record reads, the
per-context buffer and the unit table are unchanged. A program that never
passes `in_memory` sees no difference.

### Side effects

- **Fewer open files.** Contexts never open a unit for a file in memory, so
  the unit table (23 units by default, `set_max_open_files`) is not used for
  it, and N contexts × M files no longer means N×M file descriptors.
- **Slower loading of those files.** Reading the whole file takes longer
  than FURNSH reading the summaries only. An estimate is 2–10 ms for DE421 at
  SSD speeds; it will be measured. ANISE pays the same: its load of the
  kernel set is 4.0 ms, against PEPPER's 1.8 ms.
- **The same results.** The bytes are the same, so every query gives the
  same result to the last bit. This will be checked with bench_compare's
  checksums.
- **Memory.** The size of the file, once per instance, no matter how many
  contexts read it. `spice%kinfo` (or a new query) can report it.

### Not in scope (first step)

- **DAS files (DSK, EK).** The same idea applies to ZZDASGRD, ZZDASGRI and
  ZZDASGRC, but DAS mixes double, integer and character records. DSK type 2
  queries would benefit; that is a second step (D4).
- **Files open for writing** (SPKOPN, CKOPN, ...). Only kernels loaded for
  reading can be in memory.
- **The per-context record buffer.** Its 100-entry linear search stays for
  files not in memory. Making it larger, or hashed, is a separate,
  independent improvement.

## Expected speed

The sequential-epoch numbers are the target, since they are the cost with
the data at hand:

| Query | Now, random epochs | In memory (expected) | ANISE |
|---|--:|--:|--:|
| SPKGEO Moon from Earth | 9.56 µs | ~0.3 µs | 0.37 µs |
| SPKEZR Mars barycenter, LT+S | 9.4 µs | ~1.3 µs | 0.77 µs |
| SPKPOS Sun, CN+S | 9.41 µs | ~0.8 µs | 1.14 µs |
| PXFORM J2000 to ITRF93 | 3.69 µs | ~0.27 µs | 0.46 µs |
| PXFORM MOON_ME to J2000 | 1.71 µs | ~0.6 µs | 0.60 µs |

The DAFGDA fast path also skips the buffer search and the per-record copies,
so in-memory queries may be slightly faster than today's sequential ones.

## Tests

- **Unit tests** (`test/core`):
  - In memory and from the file give bitwise-identical states and rotations,
    at random and sequential epochs.
  - The same file loaded twice, once in memory and once not: one handle, and
    the image is created by the load that asks for it and kept until the
    last unload.
  - UNLOAD and KCLEAR free the image.
  - A meta-kernel with `in_memory`.
  - A file in memory queried from several contexts.
- **Parallel:** `test_parallel` with the kernels in memory, run several
  times.
- **tspice:** the whole suite with every binary kernel in memory. An
  environment variable (or the legacy default, D5) turns it on, so NAIF's 399
  families exercise the in-memory path. This includes the non-native
  (big-endian) files, which take the fallback (D3).
- **Speed:** `compare-anise` and `compare-spicelib` get "in memory" rows.

## Decisions

**D1. The API.** Proposed: an optional argument
`spice%furnsh(file, in_memory=.true.)`, plus an instance default
`spice%set_in_memory(.true.)` for programs that want every binary kernel in
memory. The argument overrides the default.

**D2. Meta-kernels.** Proposed: `in_memory` on a meta-kernel applies to all
the binary kernels it lists. Optionally, a meta-kernel variable could choose
per file, but a new variable would make the meta-kernel unreadable to
SPICELIB, which would reject or ignore it. The proposal is the argument only.

**D3. Files that cannot be copied.** Two cases:
- **Non-native files** (big-endian on this machine): their records are
  translated on every read today. Proposed: translate the whole file once at
  load into a native image (ZZXLATED, record by record). Alternatively, read
  them from the file as now, without an error.
- **Allocation failure** for a huge file. Proposed: signal an error
  (SPICE(MALLOCFAILED) or a new SPICE(NOTENOUGHMEMORY)) and do not load the
  file. Alternatively, fall back silently to file reads.

**D4. DAS (DSK) files.** In scope now, or a second step after DAF? Proposed:
the second step.

**D5. The legacy interface.** SPICELIB's FURNSH has no option. Proposed:
legacy programs get the instance default (off), plus a PEPPER-only legacy
routine `pepper_set_in_memory(flag)` to change it. tspice can use that routine
to run the suite in memory.
