# EK (Events Kernel): notes and options

Notes for a later decision on what to do with the EK subsystem. No code has
been changed.

## What EK is

The Events Kernel is a small relational database that NAIF wrote in
Fortran 77, with its own binary file format and a query language that is a
subset of SQL.

- **Purpose.** In the 1990s NAIF wanted mission "events" in SPICE next to
  the geometry: command sequences, science plans, the operations notebook.
  The design had three parts: ESP (science plan), ESQ (sequence of
  commands) and ENB (notebook). Only ESQ, the binary database, saw lasting
  use.
- **Storage.** A binary DAS file holding segments. Each segment is a table
  with named, typed columns: character (fixed or variable length), integer,
  double precision and time. A column can be scalar or an array per row,
  indexed, and nullable. These choices make the nine column classes in the
  code.
- **Queries.** A subset of SQL, run against all loaded EK files together:

  ```
  SELECT TIME, CMD FROM SEQUENCE WHERE TIME BETWEEN '2004 JAN 1' AND '2004 FEB 1' ORDER BY TIME
  ```

  It has `WHERE` with `AND`/`OR`/`NOT`, `LIKE`, `BETWEEN` and `IS NULL`,
  `ORDER BY`, and joins of up to 10 tables. NAIF wrote the whole engine:
  tokenizer, parser, normalization of the WHERE clause, join planning,
  sorting, and indexes.
- **API.**
  - Load: `furnsh` or `eklef`.
  - Query: `ekfind`, then `ekgc`, `ekgd` and `ekgi` to fetch values.
  - Write: `ekopn`, `ekbseg`, `ekappr`, `ekace*`, and the fast loaders
    `ekifld`/`ekffld`.
  - Read records directly: `ekrce*` (PEPPER: `ek_values`).
- **Another use.** The star catalog routines (`stcl01`, `stcf01`,
  `stcg01`, `stcc01`) read SPICE star catalogs, such as subsets of Tycho-2,
  which are stored as EK tables.
- **Size in PEPPER.** About 70,000 lines: `src/pepper_ek.f90` (42,000
  lines, 120 procedures) and `src/pepper_ek_storage.f90` (28,000 lines, 111
  procedures). It is the largest subsystem, and probably the least used.
  The limits work (`limits.md`, phases 2 and 6) spent much of its effort
  here: the query sizes, the join limits, and the SPICELIB bugs in ZZEKNRML
  and ZZEKJSRT (`spicelib_bugs.md`).
- **Users.** Few today. A handful of missions published sequence and
  catalog EKs (Cassini is the best known), and the star catalogs use the
  format. Most SPICE users never use EKs, and NAIF has not developed the
  subsystem for a long time.

## Could SQL replace it?

As a design, yes. A relational table and a query language are what SQLite
(or CSV files and pandas) do, much better and with full SQL. Nobody would
design EK today.

As a replacement inside PEPPER, not really:

1. **The files are external data.** Existing EK files (mission sequence
   archives, star catalogs) are read-only data that others produce. Dropping
   the reader breaks them, and nobody will republish them as SQLite.
2. **SQLite is a C library.** `plan.md` decides that the library is Fortran
   only, with no C or C interop. Real SQL means binding SQLite through
   `iso_c_binding` and depending on a native library: a reversal of that
   decision, and harder builds (Windows, ifx, CI).
3. **Compatibility.** The EK API and its query language are part of
   SPICELIB, and tspice tests them extensively.

## Options

- **A. Keep it as it is.** It works, it is tested, and its limits are gone.
  The cost is maintaining a large body of code.
- **B. Make EK optional.** Move EK (and the star catalog routines) into a
  separate fpm package or feature, so that the core library (ephemerides,
  frames, time, DSK, GF) does not carry 70,000 lines most users never need.
  When EK is not built in, `furnsh` recognizes an EK and signals a clear
  error.
- **C. Read-only EK.** Keep loading and querying, and drop or freeze the
  writer API. It saves less than it seems: most of the complexity is in the
  query engine.
- **D. Converters outside the library.** A small tool (Python with spiceypy,
  or a Fortran program on PEPPER) that exports EK tables to SQLite, CSV or
  Parquet. Users who want real SQL use the export; the library is
  unchanged.
- **E. SQLite behind the EK API.** Load EK tables into an in-memory SQLite
  database and translate the queries. It is possible, but it contradicts the
  Fortran-only decision and gains little for a feature that few use.

## Recommendation

B, possibly with D: separate EK from the core so that the library can be
built without it, and offer an export for SQL users, while existing EK files
and code keep working. Leave the EK internals as they are; do not move them
to SQL.

## First step for B

Map what the rest of the library calls in EK. It is probably little beyond:

- loading and unloading (`furnsh`/`unload` through `eklef`/`ekuef`, and
  `kclear`);
- the star catalog routines;
- the EK state in `pepper_states` and `pepper_instance_module`: the EK file and
  segment tables, and the query state per context.

That map gives the boundary of a separate package or feature, and shows
whether `pepper_instance_module`'s type-bound procedures for EK can move with it.
