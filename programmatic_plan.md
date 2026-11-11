# Programmatic ephemerides and frames

Bodies whose states and frames whose orientations come from the program
(Fortran procedures), not from kernel files, usable everywhere a body or a
frame from a kernel is: SPKEZR and the other state routines, light time and
aberration corrections, frame transformations, GF searches, prepared
queries.

## Origin: the SPICELIB experiment

[spicelib-modifications](https://github.com/jacobwilliams/spicelib-modifications)
added this to unmodified SPICELIB N0067 with three patches and a module:

- **SPKBSR (SPKSFS):** before searching the loaded files, ZZSPKFC looks the
  body up in a registry; if it is registered and the epoch is in its
  [begin, end], it returns a *virtual segment*: a descriptor of type 100,
  handle -100, with the registered center and frame.
- **SPKPVN:** type 100: SPKR100 calls the user's callback
  `user_f(body, et, state, found)`; SPKE100 copies the state.
- **ZZDYNFRM:** a registered frame ID calls the user's callback
  `user_frame_f(frcode, et, xform, relative, found)` before any dynamic-frame
  kernel data is read. A minimal frame kernel declares the frame (name, ID,
  class 5, center).
- **Registry:** one global callback for all bodies and one for all frames,
  with growable arrays of registrations.

The virtual segment of a new type is the right idea: everything above
SPKSFS works unchanged. Its limits, which PEPPER's design removes:

1. Only ZZDYNFRM (the SXFORM path) is hooked; PXFORM goes through
   ROTGET/ZZDYNROT, which looks for the dynamic frame's definition in the
   kernel pool and fails.
2. The frames still need a frame kernel (name, ID, class, center).
3. One global callback for all bodies (and one for all frames), without user
   data, in module variables: not thread-safe, and a program's data must be
   global too.
4. Registered bodies are checked before any file: they always override
   loaded SPKs; outside [begin, end] the query falls back to the files.
5. The descriptor is rebuilt (DAFPS) on every call; all registrations share
   handle -100; nothing else (coverage, listings) knows the bodies.

## Design in PEPPER

### Providers

A provider is an object, not a global callback: it carries its own data,
one per body or frame.

```fortran
type, abstract :: ephemeris_provider
contains
   procedure(ephemeris_state_interface), deferred :: state
      ! (self, ctx, body, et, state, found); self is intent(in)
end type

type, abstract :: frame_provider
contains
   procedure(frame_transform_interface), deferred :: transform
      ! (self, ctx, frame, et, xform, relative, found)
   procedure :: rotation   ! (self, ctx, frame, et, rotate, relative, found);
                           ! default: the transform's rotation block
end type
```

`ephemeris_procedure` and `frame_procedure` wrap plain procedures (the
experiment's callbacks, with the context added) for programs that do not
need user data.

The state is that of the body relative to its registered center in its
registered frame. The transformation is from this frame to the RELATIVE
frame: it maps states relative to this frame to states relative to
RELATIVE, as ROTGET's and FRMGET's results (a TK frame's rotation, a dynamic
frame's transformation) do, with its derivative (`[R 0; dR/dt R]`). (The
experiment documented its callback the other way round, while ZZDYNFRM,
which it returned to, uses this direction: its demo frame turns the other
way from its description.)

Providers are called from any context, so from several threads at once:
`self` is `intent(in)`, and a provider must not change shared data. The
context is passed so that a provider can signal errors (`ctx%sigerr`) and
call PEPPER (another body's state, a frame).

### Ephemerides: virtual SPK files

```fortran
call spice%add_ephemeris(body, center, frame, tbeg, tend, provider)
call spice%remove_ephemeris(body)
```

- **A virtual file:** registration adds a one-segment "file" to the
  instance's SPK segment index (`pepper_segment_index`, phase 5 of
  `limits.md`): a unique negative handle, the body as its key, the coverage
  [tbeg, tend], and a descriptor of type 100 (center, frame) packed once.
  The segment epoch is advanced, so every context forgets its segment
  choices, as when a file is loaded.
- **Priority:** SPICELIB's rule, the last loaded first: a provider added
  after a file overrides it for its coverage, and a file loaded after the
  provider overrides the provider. Adding a body again replaces its
  registration (and makes it the most recent). Outside its coverage the
  body's other segments (files) apply, as with any segment.
- **Evaluation:** SPKPVN's type 100 finds the registration by handle and
  calls `provider%state(ctx, body, et, state, found)`; `found = .false.`
  signals SPICE(NOEPHEMERISDATA) with the body and the epoch.
- Everything that finds segments through SPKSFS uses the provider:
  SPKGEO, SPKEZR and the other state routines, corrections, GF, prepared
  queries (whose fast path evaluates types other than 2 and 3 through
  SPKPVN).

### Frames: frame kernel data written by the registration

```fortran
call spice%add_frame(name, code, center, provider)
call spice%remove_frame(name)
```

- **No frame kernel:** the registration writes the frame's definition into
  the kernel pool itself: `FRAME_<name> = code`, `FRAME_<code>_NAME`,
  `FRAME_<code>_CLASS = 7`, `FRAME_<code>_CLASS_ID = code`,
  `FRAME_<code>_CENTER`. NAMFRM, FRMNAM, FRINFO, CIDFRM and CCIFRM need no
  change, and the contexts' frame tables follow the pool's change counter,
  as for a frame kernel.
- **A new frame class, 7 (programmatic)** (`pepper_inc_frmtyp`: `prgmtc`):
  ROTGET (PXFORM's path) calls `provider%rotation`, FRMGET (SXFORM's path)
  `provider%transform`; both return the RELATIVE frame, so programmatic
  frames can be relative to any frame, programmatic ones included, and
  REFCHG/FRMCHG walk the chain as for any frame. This covers both paths
  (the experiment's limit 1) and does not touch the dynamic frames.
- Adding a frame again (the same code) replaces it; a name or code of a
  built-in frame is refused (SPICE(FRAMEEXISTS)).

### Providers that make queries

A provider gets the context of the query, so it may make queries of its
own: a frame defined by bodies' states (a two-body frame, X toward a body,
Z along the orbital angular momentum), a body defined from other bodies (a
midpoint, a Lagrange point). These queries may call providers in turn.

- **Reentrancy:** the paths that a provider re-enters are recursive: the
  frame routines already were (dynamic frames make ephemeris queries), and
  SPKPVN, the provider calls, the entry points a provider calls (SPKEZR,
  SPKPOS, SPKCPO, SPKCPT, SPKCVO, SPKCVT, PXFORM, SXFORM) and the prepared
  queries' routines are now too. gfortran's `-frecursive` (and Intel
  Fortran's `-recursive`, in `fpm.toml`) keep all locals on the stack. The
  saved "last input" caches were already safe across nesting (dynamic
  frames, phase 14).
- **Cycles:** a provider whose result depends on itself (its body's state
  at the same epoch, its frame's orientation) would recurse until the stack
  is exhausted. The context counts the provider calls in progress; past 50,
  SPICE(RECURSIONTOODEEP). The depth is limited, not the bodies: a provider
  may ask for its own body at another epoch, where a file covers it.
- **Contract:** a provider must not change the kernels (`spice%furnsh`,
  `add_*`) or run a GF search (whose state is in the context). A provider
  procedure that can be nested (its queries reach bodies or frames of the
  same provider type) must be `recursive`.

### Clean-up with the rest of the system

- **KCLEAR** removes every programmatic ephemeris (their virtual files
  leave the segment index) and every programmatic frame (it forgets them
  before it empties the pool).
- **CLPOOL** keeps the programmatic frames: it writes their definitions into
  the emptied pool. They are registrations, like loaded files (which CLPOOL
  does not unload either), and UNLOAD of a text kernel empties the pool and
  reloads the other text kernels: a program's frames must not disappear
  then. A text kernel loaded later that defines the same frame overrides the
  definition, as a later kernel does.
- `remove_ephemeris` and `remove_frame` remove one.

### Not in scope

- The legacy interface (tspice): no wrappers for these routines.
- Coverage and object routines on files (SPKCOV, SPKOBJ, `spk_coverage`):
  they read files; a program knows its providers.

## Tests (`test/core/test_programmatic.f90`)

- **States:** a provider against an SPK written with the same motion
  (linear motion, which SPK type 9 of degree 1 represents exactly): the same
  states from SPKGEO, SPKEZR with every correction, SPKPOS, in an inertial
  and a non-inertial frame, and from prepared queries; a body whose center
  is itself programmatic.
- **Priority:** a provider added after a file overrides it; a file loaded
  after it overrides the provider; outside the provider's coverage the file
  applies; `remove_ephemeris` restores the file; an epoch covered by
  neither: SPICE(SPKINSUFFDATA).
- **Errors:** a provider that returns `found = .false.`
  (SPICE(NOEPHEMERISDATA)); an error signaled by the provider; an unknown
  registration frame.
- **Frames:** a rotating frame (the experiment's frame: 1 degree per day
  about Z) through PXFORM and SXFORM against the analytic rotation and its
  derivative; a programmatic frame relative to another programmatic frame
  and to IAU_EARTH; a constant programmatic frame against the equivalent TK
  frame; names and classes (NAMFRM, FRMNAM, FRINFO); states in a
  programmatic frame; a prepared rotation.
- **Contexts:** a context created before a registration sees it.
- **Clean-up:** after KCLEAR neither the bodies nor the frames exist; after
  CLPOOL and after UNLOAD of a text kernel the frames still do;
  `remove_ephemeris`, `remove_frame`.
- **GF:** a distance search on a programmatic body (a known crossing time).
- **Providers that make queries:** a two-body frame (PXFORM, SPKEZR), a
  midpoint body, both together with corrections and through a prepared
  query, a provider asking for its own body at an epoch a file covers, a
  cycle (SPICE(RECURSIONTOODEEP), then the context works again);
  `test_parallel`: a provider that queries another programmatic body, from
  4 threads.

## Status

Done.

- `src/pepper_programmatic.f90`: the registries, the virtual SPK files,
  the frames' definitions, the providers' calls; the provider types and
  the instance's methods in `pepper_instance`; SPKPVN's type 100, ROTGET's
  and FRMGET's class 7, KCLEAR, CLPOOL.
- A frame of class 7 without a provider (one defined by a frame kernel,
  say) signals SPICE(UNKNOWNFRAMETYPE), as any class PEPPER cannot
  evaluate: NAIF's F_ROTGET and F_FRMGET, which use class 7 as their
  unknown class, pass unchanged.
- `test/core/test_programmatic.f90` (the tests above), and
  `test/core/test_parallel.f90`: a programmatic body and frame queried from
  4 threads give the serial results to the last bit.
