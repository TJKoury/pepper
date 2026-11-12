# Modernization options

What PEPPER could add or change if a SPICELIB-like system were designed
today. These are options to choose from, not a plan of record; each says
what it would give and roughly what it would take. Status of each option is
at the end.

## Where the time goes today

With the binary kernels in memory, a state or frame query takes 0.2 to 0.8
µs, as fast as or faster than ANISE (`pepper_vs_spicelib.md`). What remains
of that time is mostly SPICELIB's design, not arithmetic: every call
translates names, parses the aberration correction, finds the frame chain,
and finds the chain of segments from the target to the observer's common
center, before it evaluates a Chebyshev polynomial. The largest gains come
from changing the shape of the API, not from tuning the algorithms.

## 1. Speed: resolve once, evaluate many times

- **Prepared queries.** Resolve a query once: names to IDs, the aberration
  correction to flags, the frame to an ID. Then evaluate it at many epochs
  without the string work:

  ```fortran
  type(state_query) :: q
  call ctx%prepare_state(q, 'MOON', 'EARTH', 'J2000', 'LT+S')
  call ctx%query_state(q, et, state, lt)
  ```

  The same for frame transformations (`prepare_rotation`). Estimated 2 to 5
  times faster per query (to be measured).
- **Batch evaluation.** `ctx%query_state(q, ets, states, lts)` over an array
  of epochs: the segment and Chebyshev record found for one epoch usually
  serve the next; the loop over epochs suits vectorization, OpenMP or
  `do concurrent`. Most programs that need many states (trajectory sampling,
  plots, Monte Carlo, scheduling) want them in batches.
- **Merged ("flattened") ephemerides.** At load time or on request, build
  for each body the sorted list of the segments that win the priority order,
  so a query does one binary search instead of a priority walk; or merge
  chosen bodies over a time range into one in-memory table (a compiled
  ephemeris).
- **Light time.** SPICELIB iterates the light-time equation a fixed number
  of times; Newton's method with the known velocity converges in 2 to 3
  iterations and can stop early.
- **Adaptive GF.** The GF solver steps through the window at a fixed step
  and misses events shorter than it. With Chebyshev representations the
  rate of change of a quantity can be bounded and the step chosen
  adaptively, with refinement by Brent's method or Newton's: faster, and
  short events are not missed silently.
- **Faster in-memory loading.** The one place PEPPER is slower than ANISE
  (4.4 ms against 3.8 ms for the benchmark's kernel set): read files
  directly into the arrays that the queries use.

## 2. API and data model

- **Precise time.** Seconds past J2000 in one double give about 0.1 µs
  resolution today, and less further from J2000. A two-part epoch (integer
  days and a fraction, or integer nanoseconds; ANISE uses hifitime) gives
  exact conversions between TDB, TT, TAI, UTC and GPS, leap-second-safe
  arithmetic, and nanoseconds over centuries. A new `type(epoch)` beside
  the SPICELIB API.
- **Typed identifiers.** `type(body)`, `type(frame)` and an aberration
  correction enumeration, resolved once, with names only at the program's
  edges: errors caught at compile time, and no string work per call.
  Prepared queries are a first step.
- **Errors per call.** Optional `stat`/`errmsg` arguments on new routines,
  beside the context's error state (`plan.md` kept the SPICELIB style for
  the existing API).
- **Immutable kernel sets.** Build a kernel set, freeze it, share it between
  threads read-only; build a new set in the background and swap it in
  atomically. Loading while queries run becomes safe without locks (today
  the rule "load first, then query" is not checked).
- **Arrays instead of cells.** Window and set operations (union,
  intersection, complement) on `intervals(2, n)` and ID arrays, so that new
  code never needs cells (phase 7 started this).

## 3. New capabilities

- **Frames, bodies and ephemerides defined in code**, without text kernels:
  simulators and estimators need this. Done: `spice%add_ephemeris` and
  `spice%add_frame` with provider objects (`programmatic_plan.md`).
- **Ephemerides from data:** fit Chebyshev or Hermite polynomials to a
  trajectory with error control, and write SPK or keep it in memory
  (bspline-fortran is an option).
- **Derivatives:** derivatives of frame transformations, or forward-mode
  automatic differentiation through queries, for navigation and estimation.
- **Modern formats:** JSON or TOML in place of text kernels, converted
  losslessly both ways; perhaps ANISE's formats for interoperability.
- **Bindings:** a Python package on PEPPER, thread-safe, with batch queries
  on NumPy arrays (a thin C interface, outside the Fortran-only library).

## 4. To drop or make optional

- EK: see `ek_notes.md`.
- The SPICELIB utility routines kept only for compatibility, and the cell
  API: a compatibility module, so that the core API is modern.
- SPICELIB conventions such as 132-character text kernel lines, for new
  formats only (`limits.md` keeps them for text kernels).

## Suggested order

1. Prepared queries and batch evaluation, for states and frames: the
   largest measurable speedup, no change to the file formats, added beside
   the existing API.
2. A precise time type, as the base of new APIs.
3. Merged ephemerides (builds on 1).
4. Adaptive GF.

## Status

### Prepared queries and batch evaluation: done

`ctx%prepare_state` / `ctx%query_state` and `ctx%prepare_rotation` /
`ctx%query_rotation` / `ctx%query_transform` (`src/pepper_queries.f90`,
`src/pepper_query_types.f90`), one epoch or a batch. The results are
SPKEZR's, PXFORM's and SXFORM's to the last bit (`test/core/test_queries.f90`:
30,240 states over every combination of target, observer, frame and
correction; rotations and transformations; batches; errors).

What makes state queries faster: names, correction and frame resolved once;
the target's segment chain found from the descriptors before evaluation, so
that only the links the result needs are evaluated (SPKGEO evaluates the
whole chain to the barycenter); the current Chebyshev record kept for SPK
types 2 and 3. The light time and aberration code is SPICELIB's, statement
for statement, on these states.

Measured (`pixi run bench-queries`; tables in `pepper_vs_spicelib.md`,
"Prepared queries"):

- states 1.5 to 2.5 times faster with the kernels in memory (the Moon from
  the Earth: 0.27 to 0.12 µs; ANISE: 0.35 µs), 2 to 4 times at sequential
  epochs read from files;
- frames about 1.1 times (only the names are saved), except rotations that
  do not depend on time (14 times: computed once);
- batches: the speed of the single queries.

The estimate above (2 to 5 times) holds for geometric states and sequential
epochs; corrected states gain less, because the light time and aberration
corrections evaluate several states per epoch (SPKACS evaluates the
observer at three epochs for its acceleration).

Next steps for this option:

- **Frames:** the time is in the orientation itself. For binary PCK frames
  (ITRF93, MOON_PA), keep the current record as for SPK, and compute only
  the rotation for PXFORM (TISBOD computes the state transformation and
  PXFORM keeps its rotation). For text PCK frames, the body's polynomials
  could be prepared once.
- **Batches:** evaluate the Chebyshev series for all the epochs of a record
  at once (vectorizable), and reuse the observer's states between epochs.
- **Corrected states:** the observer's acceleration (SPKACS's two extra
  states) could come from the Chebyshev series' second derivative instead of
  a finite difference; this changes the results in the last bits, so it
  would be an option, not the default.

### Programmatic ephemerides and frames: done

`programmatic_plan.md`.

### The others

Not started.
