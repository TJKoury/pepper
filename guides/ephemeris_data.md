# Getting ephemeris data into PEPPER

PEPPER answers the same questions as SPICE (where is this body, at this time,
seen from there, in this frame), and it can take the trajectories it answers
from in four ways:

| | You have | PEPPER does | Use |
|---|---|---|---|
| [1. SPICE kernels](#1-spice-kernels) | `.bsp` files (and the other kernels) | reads and interpolates them | `furnsh` |
| [2. Providers](#2-providers-your-code-computes-the-states) | code that computes a state at any time | calls your code on every query | `add_ephemeris` |
| [3. Programmatic kernels](#3-programmatic-kernels-your-states-pepper-interpolates) | states at epochs, in arrays | interpolates them, like an SPK file | `kernel_builder`, `load_kernel` |
| [4. Kernel loaders](#4-kernel-loaders-files-in-other-formats) | files in a format SPICE does not know | `furnsh` hands them to your reader | `kernel_loader`, `add_loader` |

Whatever the source, a body is then used the same way: `spkezr`, `spkpos`,
`spkgeo` and the other state routines, light time and aberration
corrections, frame transformations, GF searches, prepared queries, from any
number of threads. Sources can be mixed freely; [Priority](#priority-when-sources-overlap)
explains which one answers when several cover the same body and time.

Contents:

- [Choosing a way](#choosing-a-way)
- [The basics: instances, contexts, queries, errors](#the-basics-instances-contexts-queries-errors)
- [1. SPICE kernels](#1-spice-kernels)
- [2. Providers: your code computes the states](#2-providers-your-code-computes-the-states)
- [3. Programmatic kernels: your states, PEPPER interpolates](#3-programmatic-kernels-your-states-pepper-interpolates)
- [4. Kernel loaders: files in other formats](#4-kernel-loaders-files-in-other-formats)
- [Reference frames](#reference-frames)
- [Body names](#body-names)
- [Priority when sources overlap](#priority-when-sources-overlap)
- [Threads](#threads)
- [From C and Python](#from-c-and-python)
- [Reference](#reference)
- [Troubleshooting](#troubleshooting)

## Choosing a way

- **You have SPK files**: [1](#1-spice-kernels). Nothing else is needed.
- **You have a formula or a model** (two-body motion, an analytic theory, a
  propagator you call, live data): [2](#2-providers-your-code-computes-the-states).
  PEPPER asks your code for the state at each query's time; nothing is
  interpolated.
- **You have computed states** (a simulation's output, another tool's
  results, a table you read yourself) and want PEPPER to interpolate them as
  it does SPK data: [3](#3-programmatic-kernels-your-states-pepper-interpolates).
  You can also save the result as a real `.bsp` file.
- **Your users have files in some format** and you want `furnsh` to load
  them like SPK files (by extension, in meta-kernels):
  [4](#4-kernel-loaders-files-in-other-formats). A loader is a reader you
  write once; it uses 3 (for files of states) or 2 (for files that hold a
  model).

| | Providers (2) | Programmatic kernels (3) | Loaders (4) |
|---|---|---|---|
| Who computes a state at time t | your code | PEPPER (SPK types 9, 13) | PEPPER, or your model |
| Your code runs during queries | yes, from every thread | no | only for models |
| A kernel (KDATA, SPKCOV, UNLOAD) | no | yes | yes |
| Can be saved as a `.bsp` | no | yes (`write_kernel`) | yes, unless it holds a model |
| Loaded by `furnsh` and meta-kernels | no | no | yes |

## The basics: instances, contexts, queries, errors

```fortran
use pepper_module

type(pepper_instance), target :: spice      ! the loaded data (target: contexts point to it)
type(pepper_context) :: ctx                 ! queries, one per thread
real(wp) :: et, state(6), lt

call spice%furnsh('kernels.tm')             ! changes of the data: methods of the instance
ctx = spice%new_context()
call ctx%str2et('2026 NOV 15 12:00 TDB', et)
call ctx%spkezr('MOON', et, 'J2000', 'LT+S', 'EARTH', state, lt)
```

- **The instance** holds everything loaded: kernels, providers, kernel
  loaders, body names. Changes are its methods (`furnsh`, `unload`,
  `add_ephemeris`, `load_kernel`, `add_loader`, ...). They must not run
  while other threads query.
- **A context** makes queries. Each thread uses its own (`spice%new_context()`),
  all of them reading the same instance at once.
- **Errors** follow SPICE: a routine signals an error and returns. Check the
  instance after a change and the context after queries:

  ```fortran
  type(pepper_context), pointer :: own
  character(1840) :: msg
  call spice%furnsh('missing.bsp')
  if (spice%failed()) then
     own => spice%context()            ! the instance's own context holds its errors
     call own%getmsg('LONG', msg)
     print '(a)', trim(msg)
     call own%reset()
  end if
  ```

  Each section below lists the short messages (`SPICE(...)`) its routines
  signal.

Epochs are TDB seconds past J2000 (ET) everywhere below; states are km and
km/s.

## 1. SPICE kernels

SPICE's files load exactly as in SPICE:

```fortran
call spice%furnsh('naif0012.tls')
call spice%furnsh('de440.bsp')
call spice%furnsh('mission.tm')             ! a meta-kernel: every file it lists
```

- **Any SPK** (all segment types), and every other kernel type: CK, PCK,
  text kernels, frame kernels, DSK, EK.
- **Meta-kernels** with `KERNELS_TO_LOAD`, `PATH_SYMBOLS` and
  `PATH_VALUES`.
- **Text kernels in JSON** (a file named `*.json`) for variables, body names
  and frames (README, "JSON kernels"); SPK data itself is always binary.
- **Read whole into memory**, for speed with scattered epochs:
  `spice%furnsh('de440.bsp', in_memory=.true.)` (or
  `spice%set_in_memory(.true.)` for every load from then on).
  `spice%kernel_memory()` tells the bytes used.
- **Unload**: `spice%unload('de440.bsp')` (the name exactly as loaded),
  `spice%kclear()` for everything. A file loaded twice needs two unloads.
- **What is loaded**: `ctx%ktotal('SPK', n)`, `ctx%kdata(i, 'SPK', file,
  type, source, handle, found)`, or `ctx%kernel_info(file, type, source,
  handle, found)` with names of any length.
- **Coverage**: `ctx%spk_coverage(file, body, intervals)` (an `(2, n)` array
  of intervals) and `ctx%spk_objects(file, ids)`.

## 2. Providers: your code computes the states

A provider is an object with a `state` procedure: PEPPER calls it whenever a
query needs the body, at the query's epoch.

### With a procedure

The quickest way: wrap a subroutine in `ephemeris_procedure`.

```fortran
use pepper_module

type(ephemeris_procedure) :: orbit

orbit%f => circular_orbit
call spice%add_ephemeris(-999, 399, 'J2000', 0.0_wp, 1.0e7_wp, orbit)
call ctx%spkezr('-999', 1000.0_wp, 'J2000', 'LT+S', 'MOON', state, lt)

contains

   !> The state of body -999 relative to the Earth (399) in J2000.
   subroutine circular_orbit(ctx, body, et, state, found)
      class(pepper_context), intent(inout) :: ctx
      integer(ip), intent(in) :: body
      real(wp), intent(in) :: et
      real(wp), intent(out) :: state(6)
      logical, intent(out) :: found
      real(wp), parameter :: r = 7000.0_wp, w = 1.0e-3_wp
      state = [r*cos(w*et), r*sin(w*et), 0.0_wp, -r*w*sin(w*et), r*w*cos(w*et), 0.0_wp]
      found = .true.
   end subroutine circular_orbit
```

### With your own type

When the model has data (elements, coefficients, a handle to a propagator),
extend `ephemeris_provider`. The instance keeps a copy of the object, data
included.

```fortran
type, extends(ephemeris_provider) :: kepler_orbit
   real(wp) :: elts(8)      ! conic elements (OSCELT's), with their epoch and GM
contains
   procedure :: state => kepler_state
end type

subroutine kepler_state(self, ctx, body, et, state, found)
   class(kepler_orbit), intent(in) :: self      ! intent(in): it must not change
   class(pepper_context), intent(inout) :: ctx
   integer(ip), intent(in) :: body
   real(wp), intent(in) :: et
   real(wp), intent(out) :: state(6)
   logical, intent(out) :: found
   call ctx%conics(self%elts, et, state)
   found = .not. ctx%failed()
end subroutine kepler_state

! ...
call spice%add_ephemeris(-1001, 10, 'ECLIPJ2000', t0, t1, kepler_orbit(elts))
```

### The rules

- `state` gives the state of `body` relative to the registered center, in
  the registered frame. Return `found = .false.` when there is none at `et`
  (the query then signals SPICE(NOEPHEMERISDATA)).
- **The registration**: `add_ephemeris(body, center, frame, tbeg, tend,
  provider)`. The body takes part in the segment search as a file loaded at
  that moment, within [tbeg, tend]. Adding the same body again replaces the
  registration.
- **Threads**: providers are called from every context, so from several
  threads at once. They must not change shared data (`self` is
  `intent(in)`).
- **Queries inside a provider** are allowed, through the `ctx` it is given:
  another body's state, a frame, a kernel variable (a body defined relative
  to others, a frame from two bodies). A provider that ends up needing its
  own result (more than 50 nested calls) gives SPICE(RECURSIONTOODEEP). A
  provider whose queries can reach providers of the same type must be
  `recursive`.
- **Removing**: `spice%remove_ephemeris(body)`; `kclear` removes all.
- **Frames work the same way**: `spice%add_frame(name, code, center,
  provider)` with a `frame_provider` (README, "Programmatic ephemerides and
  frames").
- **Errors**: SPICE(UNKNOWNFRAME), SPICE(BADDESCRTIMES) (tbeg after tend),
  SPICE(BODYANDCENTERSAME).

A provider is not a kernel: KDATA, SPKCOV and `write_kernel` do not know it.
For that, put it in a programmatic kernel ([below](#models-in-a-kernel)).

## 3. Programmatic kernels: your states, PEPPER interpolates

A `kernel_builder` collects SPK segments made from your arrays; loaded, it
is a kernel with no file: an SPK held in memory, used exactly like one read
from disk.

```fortran
use pepper_module

type(kernel_builder) :: kb
real(wp) :: epochs(n), states(6, n)       ! your trajectory: states(:, k) at epochs(k)

! ... fill epochs (increasing) and states ...

call kb%spk_hermite(ctx, -10001, 399, 'J2000', epochs, states, 7)
call spice%load_kernel('my_trajectory', kb)
if (spice%failed()) ...

call ctx%spkezr('-10001', et, 'J2000', 'NONE', 'EARTH', state, lt)
```

### Segments and interpolation

Each call adds a segment; the interpolation is SPICE's own:

| Call | SPK type | Interpolation | Degree |
|---|---|---|---|
| `kb%spk_lagrange(ctx, body, center, frame, epochs, states, degree, ...)` | 9 | Lagrange, on positions and velocities separately | 1 to 27; needs at least degree + 1 states |
| `kb%spk_hermite(ctx, body, center, frame, epochs, states, degree, ...)` | 13 | Hermite, positions and velocities together | odd, 1 to 27; needs (degree + 1) / 2 states |

Hermite uses the velocities, so it is usually the better choice for
trajectories; degree 7 to 11 is typical. Epochs must increase; their spacing
may vary.

Optional arguments:

- **`tbeg`, `tend`**: the segment's coverage, inside its epochs. The extra
  points at the ends still serve the interpolation, so the states near the
  bounds are as good as in the middle. Default: the first and last epoch.
- **`ident`**: the segment identifier (up to 40 printable characters).
- **`adapt = .true.`**: a segment with too few states for its degree is not
  an error. The degree is lowered to what the states allow. Two states make
  a Hermite cubic (from the velocities, better than a line), and a single
  state is dropped, since it covers no interval. Useful for short arcs
  between maneuvers.

The builder's checks are those of SPKW09 and SPKW13, with the same short
messages: SPICE(TOOFEWSTATES), SPICE(INVALIDDEGREE),
SPICE(TIMESOUTOFORDER), SPICE(BADDESCRTIMES), SPICE(SEGIDTOOLONG),
SPICE(NONPRINTABLECHARS), plus SPICE(INVALIDSIZE) (states not 6 by n) and
SPICE(BARYCENTEREPHEM) (a body that is its own center). The frame is checked
when the builder is loaded (SPICE(INVALIDREFFRAME)).

### Several segments, discontinuities, gaps

Segments are kept in the order added, and the later one wins where they
overlap. That gives maneuvers and gaps directly:

```fortran
! a maneuver at epochs(50): the segment after it starts there, with the new velocity
call kb%spk_hermite(ctx, -10001, 399, 'J2000', epochs(1:50),   before(:, 1:50),  7)
call kb%spk_hermite(ctx, -10001, 399, 'J2000', epochs(50:100), after(:, 50:100), 7)
```

- **At the shared epoch** the later segment answers, so the state there is
  the one after the maneuver. Just before it, the first segment answers.
- **Interpolation never crosses the maneuver**: each segment has only its
  own side's states.
- **Coverage stays one interval** (SPKCOV merges touching intervals).
- **Segments with time between them** leave a gap: two coverage intervals,
  and a query in the gap fails as with any SPK (SPICE(SPKINSUFFDATA)).
- **Many bodies, centers and frames** can be in one builder.

### Loading, replacing, unloading

```fortran
call spice%load_kernel('my_trajectory', kb)             ! the builder is emptied
call spice%load_kernel('my_trajectory', kb2)            ! replaces it (highest priority)
call spice%load_kernel('other', kb3, keep=.true.)       ! kb3 keeps its segments
call spice%unload('my_trajectory')
```

- **The name** is any non-blank string, never looked up on disk. It is
  what KDATA reports and what UNLOAD, SPKCOV and SPKOBJ take.
- **Loading under a name in use** (another kernel, or a loaded file)
  replaces it: the old one is unloaded first.
- **Kernels are not changed in place.** A growing trajectory (a simulation,
  a filter) keeps its builder (`keep=.true.`), adds segments, and loads it
  again under the same name.
- **An error loads nothing**, and the builder is left as it was.
- **`kb%clear()`** empties a builder; `kb%nsegments()` counts its segments.
- **`ctx%kernel_format(name, loader, format, found)`** gives the
  description set with `kb%set_format(text)`, or `(programmatic)`.

### Saving as a real SPK file

```fortran
call spice%write_kernel('my_trajectory', 'my_trajectory.bsp')
```

The file is an ordinary SPK (types 9 and 13): SPICE, CSPICE, SpiceyPy and
other SPICE tools read it, and PEPPER reads it back with the same states, to
the last bit. `write_kernel` does not overwrite a file
(SPICE(FILEEXISTS)). A builder is therefore also a converter: fill it from
any source, write a `.bsp`.

### Body names

`kb%define_body(ctx, 'MY_SPACECRAFT', -10001)` defines a name while the
kernel is loaded; see [Body names](#body-names).

### Models in a kernel

A builder can also hold providers: `kb%add_ephemeris(ctx, body, center,
frame, tbeg, tend, provider)`. Unlike `spice%add_ephemeris`, the provider
then belongs to the kernel: SPKCOV and SPKOBJ report it under the kernel's
name, UNLOAD and KCLEAR remove it with the kernel, and kernels of the same
body coexist (the last loaded first; unloading it restores the earlier). This
is mainly for loaders ([below](#files-that-hold-a-model)). A kernel with a
provider cannot be written to a file (SPICE(NOTWRITABLE)): its states are not
stored anywhere.

### Building in threads

A builder belongs to no instance: each thread can fill its own builder while
other threads query (the expensive part, packing the data, happens as
segments are added). Only `load_kernel` touches the instance, and it must
run while no queries run, like `furnsh`. The `ctx` the builder's methods take
is only for reporting errors: any context, such as the thread's own.

## 4. Kernel loaders: files in other formats

A kernel loader is a reader of a file format SPICE does not know (an
in-house ephemeris format, a CSV table, a FreeFlyer or CCSDS OEM file,
...). Registered on an instance, it is used by `furnsh`: by the file's
extension, by looking at the file's content, or by name. It reads the file
into a builder, and the file is then a kernel like any SPK: queries,
SPKCOV, KDATA, UNLOAD, meta-kernels.

PEPPER has no loader for any real format; loaders are written by you, or
come from separate libraries.

### Writing a loader

Extend `kernel_loader` and implement `load`; `detect` and `unload` are
optional.

```fortran
module csv_loader_module
   use pepper_module
   implicit none
   private
   public :: csv_loader

   !> Files of lines "et, x, y, z, vx, vy, vz", after a first line
   !> "CSVEPH body center frame".
   type, extends(kernel_loader) :: csv_loader
      integer :: degree = 9          ! a setting of the loader
   contains
      procedure :: load => csv_load
      procedure :: detect => csv_detect
   end type

contains

   subroutine csv_load(self, ctx, kb, file)
      class(csv_loader), intent(inout) :: self
      class(pepper_context), intent(inout) :: ctx     ! the instance's: lookups and errors
      type(kernel_builder), intent(inout) :: kb        ! empty: fill it
      character(*), intent(in) :: file
      real(wp), allocatable :: e(:), s(:, :)
      real(wp) :: row(7)
      character(16) :: tag, frame
      integer(ip) :: body, center
      integer :: lun, ios, n, line

      open (newunit=lun, file=file, status='old', action='read', iostat=ios)
      if (ios /= 0) then
         call ctx%setmsg('The file ''#'' could not be opened.')
         call ctx%errch('#', file)
         call ctx%sigerr('SPICE(FILEOPENFAILED)')
         return
      end if
      read (lun, *, iostat=ios) tag, body, center, frame
      line = 1
      allocate (e(1000), s(6, 1000))
      n = 0
      do
         read (lun, *, iostat=ios) row
         if (ios < 0) exit                      ! end of file
         line = line + 1
         if (ios > 0) then
            call ctx%setmsg('File ''#'', line #: not 7 numbers.')
            call ctx%errch('#', file)
            call ctx%errint('#', line)
            call ctx%sigerr('SPICE(BADCSVFILE)')
            close (lun)
            return
         end if
         n = n + 1                              ! (grow e and s as needed)
         e(n) = row(1)
         s(:, n) = row(2:7)
      end do
      close (lun)
      call kb%set_format('CSV ephemeris')
      call kb%spk_lagrange(ctx, body, center, trim(frame), e(1:n), s(:, 1:n), &
                           int(self%degree, ip), adapt=.true.)
   end subroutine csv_load

   !> Accept a file whose first line starts with CSVEPH.
   logical function csv_detect(self, file, head) result(accept)
      class(csv_loader), intent(in) :: self
      character(*), intent(in) :: file
      character(*), intent(in) :: head            ! the file's first 4096 bytes
      accept = head(1:6) == 'CSVEPH'
   end function csv_detect

end module csv_loader_module
```

What `load` does:

- **Read the file** with its own I/O (text or binary).
- **Map the file's names** to NAIF codes and frames. It can look them up
  through `ctx` (`ctx%bodn2c`, `ctx%namfrm`), use settings of its own (a
  table the program fills, say), or define them
  ([below](#names-in-files)).
- **Convert the file's times** to ET: `ctx%str2et` for UTC strings
  (the program must have loaded a leap-seconds kernel), `ctx%unitim` for
  TAI, TT, GPS seconds. For epochs given as seconds after an origin,
  subtract in integers (whole seconds and nanoseconds apart) before
  converting to `real(wp)`, so the precision is kept.
- **Fill the builder**: segments, body names, providers.
- **Report errors** in `ctx` with a message naming the file and line. If
  `load` signals an error, or the builder's checks fail, nothing of the file
  is loaded.
- **Use only SPICE's interpolations**: map the file's method to type 9
  (Lagrange) or 13 (Hermite), or signal an error (e.g. SPICE(UNSUPPORTEDINTERP)
  for a spline). Do not substitute one method for another.

The optional procedures:

- **`detect(self, file, head)`**: return `.true.` to read a file found by
  its content (`head` is its first 4096 bytes). It must be quick and change
  nothing. The default accepts no file.
- **`unload(self, file)`**: called when the file's kernel is unloaded (also
  by KCLEAR), to free what `load` kept. The default does nothing.

`self` is `intent(inout)` in `load`: the loader may keep data an SPK has no
place for (covariances, comments, a mass column) and offer it through its own
procedures.

### Registering and loading

```fortran
use csv_loader_module

type(csv_loader) :: csv

call spice%add_loader('CSV', csv, extensions=['.csveph'])
call spice%furnsh('track.csveph')                    ! by extension
call spice%furnsh('track_2026.txt')                  ! by detect (content)
call spice%furnsh('odd_name.dat', loader='CSV')      ! by name
```

- **`add_loader(name, loader, extensions, replace)`**: the instance keeps a
  copy of `loader`, with its settings at that moment. Names and extensions
  are case-insensitive. Registering a name or extension again is an error
  (SPICE(LOADERCONFLICT)) unless `replace = .true.`, which replaces the
  loader, or moves the extension to the new one.
- **`spice%remove_loader(name)`** removes a loader. Kernels it loaded stay
  loaded, but their `unload` callbacks are no longer called.
- **`spice%loader(name, ptr)`** gives a pointer to the instance's copy of a
  loader (`class(kernel_loader), pointer`), to read what it kept:

  ```fortran
  class(kernel_loader), pointer :: p
  call spice%loader('CSV', p)
  select type (p)
  type is (csv_loader)
     print *, p%degree
  end select
  ```

### How `furnsh` chooses a loader

With `loader=` (not blank), the registered loader with that name reads the
file, overriding everything else: `furnsh` does not check that the file
exists, what it is (even a `.bsp` would be given to the loader), its
extension, or the other loaders' `detect`. The loader reports its own errors.

Without `loader=`:

1. The file must exist (SPICE(NOSUCHFILE)).
2. A SPICE binary kernel (SPK, CK, binary PCK, DSK, EK, known by the ID word
   in its first record) is loaded by PEPPER. Loaders are never given one by
   extension or detection.
3. Else the registered loader whose extension ends the file name reads it
   (the longest match, so `.oem.xml` beats `.xml`).
4. Else the loaders' `detect`, the most recently registered first.
5. Else PEPPER's text readers: `*.json` as a JSON kernel, anything else as a
   text kernel or meta-kernel.

Other names given as `loader=`:

- **`SPK`, `CK`, `PCK`, `DSK`, `EK`, `TEXT`, `JSON`, `META`**, when no loader
  is registered under the name, mean "load as without loaders" (steps 2 and
  5). They do not choose the type: a `.json` file is still read as JSON.
  Their use is to keep registered loaders from claiming a file. A loader
  registered under one of these names takes its place.
- **Any other name** no loader has: SPICE(UNKNOWNLOADER), and nothing is
  loaded.

### Meta-kernels

Files in `KERNELS_TO_LOAD` go through the same steps 1 to 5, so loaders with
extensions or `detect` work in meta-kernels unchanged. To name a loader for a
listed file, add `KERNEL_LOADERS`: pairs of a file (as written in
`KERNELS_TO_LOAD`, or with its path symbols replaced) and a loader's name.

```
KPL/MK
\begindata
PATH_SYMBOLS    = ( 'K' )
PATH_VALUES     = ( '/data/kernels' )
KERNELS_TO_LOAD = ( '$K/naif0012.tls',
                    '$K/de440.bsp',
                    '$K/track.csveph',
                    '$K/track_2026.txt' )
KERNEL_LOADERS  = ( '$K/track_2026.txt', 'CSV' )
\begintext
```

An odd number of values in `KERNEL_LOADERS` is SPICE(BADKERNELLOADERS).
`furnsh`'s `loader=` applies to the file passed, so for a meta-kernel it is
the loader of the meta-kernel itself, not of the files it lists.

### Files that hold a model

Some formats store a model rather than states: orbital elements and a
propagator (two-line elements, a state with a GM for two-body motion, an
analytic theory). The loader then reads the model into a provider of its own
and adds it to the builder:

```fortran
type, extends(ephemeris_provider) :: elements_model
   real(wp) :: elts(8)
contains
   procedure :: state => elements_state      ! computes the state from self%elts at et
end type

subroutine model_load(self, ctx, kb, file)
   ...
   ! read the file -> elts, body, center, frame, the valid span t0, t1
   call kb%add_ephemeris(ctx, body, center, frame, t0, t1, elements_model(elts))
end subroutine
```

The reader (`load`) runs once, at `furnsh`; the evaluator (`state`) runs at
every query, from any thread, so it must not change itself. PEPPER keeps a
copy of the evaluator, with the data in it, and the registration (body,
center, frame, span, the kernel it belongs to). UNLOAD of the file removes
both. One file may have rows and models.

### Names in files

Files usually name their object (`Spacecraft = "SC1"`). The loader can
define the name, so that `spkezr('SC1', ...)` works while the file is loaded:

```fortran
call kb%define_body(ctx, 'SC1', -10001)
```

The code still has to come from somewhere: the file, a table the program
gives the loader, or a convention of the loader. See [Body names](#body-names).

### What is loaded, and unloading

```fortran
character(:), allocatable :: loader, format
call ctx%kernel_format('track.csveph', loader, format, found)   ! 'CSV', 'CSV ephemeris'
call spice%unload('track.csveph')
```

- KDATA and KINFO list such a kernel as an `SPK`, under the file's name (as
  passed to `furnsh`; for a meta-kernel's file, with its path symbols
  replaced).
- `unload(name)` removes it (and calls the loader's `unload`); `kclear`
  removes everything.
- A file loaded twice is read twice (two kernels); each `unload` removes the
  most recent.
- `spice%write_kernel(file, 'track.bsp')` converts it to a real SPK (unless
  it holds a model).

## Reference frames

Frames (how a frame is oriented, relative to another, at each time) come
in the same four ways:

| | You have | Use | |
|---|---|---|---|
| 1 | frame kernels, CK, PCK files; or a frame's definition (fixed, two-vector, of date, Euler, product, body-fixed rotation models) | `furnsh`, or the definition's keywords set from code (`lmpool`, `pcpool`, ...) or in a JSON kernel | SPICE's own |
| 2 | code that computes the orientation at any time | `spice%add_frame(name, code, center, provider)` | a `frame_provider` |
| 3 | rotations at epochs (matrices or quaternions) | `kb%frame_rotations(...)`, `load_kernel` | PEPPER interpolates |
| 4 | files in other formats | a loader that calls `kb%frame_rotations` (tables) or `kb%add_frame` (a model) | `furnsh` |

### Frames from parameters (1)

A frame defined by a few keywords needs no file and no code. For example,
an Earth–Moon rotating frame, its X axis toward the Moon and its Y axis
along the Moon's velocity, as a JSON kernel:

```json
{
  "FRAME_EARTH_MOON_ROT":          1400001,
  "FRAME_1400001_NAME":            "EARTH_MOON_ROT",
  "FRAME_1400001_CLASS":           5,
  "FRAME_1400001_CLASS_ID":        1400001,
  "FRAME_1400001_CENTER":          399,
  "FRAME_1400001_RELATIVE":        "J2000",
  "FRAME_1400001_DEF_STYLE":       "PARAMETERIZED",
  "FRAME_1400001_FAMILY":          "TWO-VECTOR",
  "FRAME_1400001_PRI_AXIS":        "X",
  "FRAME_1400001_PRI_VECTOR_DEF":  "OBSERVER_TARGET_POSITION",
  "FRAME_1400001_PRI_OBSERVER":    "EARTH",
  "FRAME_1400001_PRI_TARGET":      "MOON",
  "FRAME_1400001_PRI_ABCORR":      "NONE",
  "FRAME_1400001_SEC_AXIS":        "Y",
  "FRAME_1400001_SEC_VECTOR_DEF":  "OBSERVER_TARGET_VELOCITY",
  "FRAME_1400001_SEC_OBSERVER":    "EARTH",
  "FRAME_1400001_SEC_TARGET":      "MOON",
  "FRAME_1400001_SEC_ABCORR":      "NONE",
  "FRAME_1400001_SEC_FRAME":       "J2000"
}
```

The same keywords can be set at run time with `spice%lmpool(lines)`. They
stay in the kernel pool until `clpool`, `kclear` or `dvpool`. NAIF's Frames
Required Reading describes every definition style.

### Frames computed by code (2)

```fortran
type(frame_procedure) :: spin

spin%f => my_spin                ! subroutine my_spin(ctx, frame, et, xform, relative, found)
call spice%add_frame('MY_SPIN', 1400002, 399, spin)
call ctx%pxform('J2000', 'MY_SPIN', et, rot)
```

The provider's `transform` gives the 6x6 state transformation from the frame
to the frame it is relative to (`relative`, an ID code), as a TK frame's
rotation does. Optionally, `rotation` gives a faster 3x3 for PXFORM. The
rules are those of ephemeris providers (thread safety, queries inside,
`found`). `remove_frame(name)` removes it.

### Rotations at epochs (3)

```fortran
real(wp) :: epochs(n), quats(4, n)     ! or rots(3, 3, n)

call kb%frame_rotations(ctx, 'MY_FRAME', 1400100, 399, 'J2000', epochs, quats, 7)
call spice%load_kernel('my_frame_data', kb)
call ctx%pxform('J2000', 'MY_FRAME', et, rot)     ! interpolated
call spice%unload('my_frame_data')                ! the frame is gone
```

- **The rotations** map vectors from the base frame (here J2000) to the new
  frame: `v_frame = R v_base`, so `pxform('J2000', 'MY_FRAME', et)` is R.
  Give them as 3x3 matrices (`rots(3, 3, n)`) or as SPICE quaternions
  (scalar first, `quats(4, n)`). Quaternion signs need not be continuous.
  Pass `inverse = .true.` if yours map from the frame to the base.
- **No clock:** the epochs are ET. PEPPER stores the table as a CK type 5
  segment with ET as its time tags, held in memory, and evaluates it with
  SPICE's CK type 5 routines, so the interpolation is SPICE's.
- **Interpolation:** `method = 'LAGRANGE'` (default) or `'HERMITE'`. The
  window must have an even size: an odd degree for Lagrange (1, 3, ...,
  23); 3, 7, 11, 15, 19 or 23 for Hermite.
- **Angular velocities**, optional: `av(3, n)`, the frame's angular velocity
  relative to the base, in the base's coordinates (rad/s). Hermite needs
  them, and is then much more accurate between the rows. Without them,
  SXFORM's derivative comes from the interpolation.
- **Coverage:** `tbeg`, `tend` (inside the epochs; default their span).
  Outside it the frame has no orientation and queries fail. With several
  tables of one frame, in one kernel or several, the last loaded that covers
  the epoch answers. A later kernel can therefore override part of the
  time, and the earlier one shows through elsewhere.
- **The frame** has the name, ID code and center you give. Use codes from
  1400000 on for your own frames, as NAIF suggests for programmatic ones.
  It exists while the kernel is loaded. The name and code must not be those
  of another frame (SPICE(FRAMEEXISTS)), except the same frame from another
  kernel or from `add_frame`.
- **Not writable yet:** `write_kernel` refuses a kernel with frames
  (SPICE(NOTWRITABLE)). Writing a `.bc` with its clock and frame kernels is
  a planned addition.

Errors: SPICE(INVALIDDEGREE), SPICE(UNSUPPORTEDINTERP) (another method, or
Hermite without `av`), SPICE(ZEROQUATERNION), SPICE(NOTAROTATION) (a matrix
that is not a rotation), SPICE(INVALIDSIZE), SPICE(TIMESOUTOFORDER),
SPICE(BADDESCRTIMES); at the load, SPICE(UNKNOWNFRAME) (the base) and
SPICE(FRAMEEXISTS).

### Frames from files (4)

A loader adds frames to its builder as it adds segments: `kb%frame_rotations`
for a file of rotations, and `kb%add_frame(ctx, name, code, center, provider)`
for a file that holds an orientation model (the loader's own
`frame_provider`, holding what it read). The frames go away when the file is
unloaded.

```fortran
! in a loader's load(): a table, then a model
call kb%frame_rotations(ctx, 'SC_ATTITUDE', -10001000, -10001, 'J2000', epochs, quats, 7)
call kb%add_frame(ctx, 'SPIN_MODEL', 1400200, 399, my_spin_model(rate))
```

`spice%add_frame` for a frame that a kernel defines is allowed: the
program's registration is the latest, and `remove_frame` removes only the
program's.

## Body names

Bodies are found by name or by NAIF ID code. Names come from:

| Source | How | Removed by |
|---|---|---|
| Built in | the NAIF IDs (`EARTH`, `MOON`, ...) | never |
| The kernel pool | a text or JSON kernel with `NAIF_BODY_NAME` / `NAIF_BODY_CODE` | unloading the kernel; `kclear` |
| The program | `spice%boddef(name, code)` | nothing (as in SPICE; `kclear` keeps them) |
| A kernel built in memory | `kb%define_body(ctx, name, code)` (programmatic kernels, loaders) | unloading that kernel; `kclear` |

Precedence: names from the kernel pool first, then `boddef` and kernels'
names (the later definition first), then the built-in names. When a kernel's
name is removed, an earlier definition of the same name is in effect again:

```fortran
call spice%boddef('SC1', -5)
call spice%furnsh('a.csveph')       ! defines SC1 = -10001
call ctx%bodn2c('SC1', code, found) ! -10001
call spice%unload('a.csveph')
call ctx%bodn2c('SC1', code, found) ! -5 again
```

Every context sees name changes on its next lookup, including contexts made
before the change.

## Priority when sources overlap

When several sources cover the same body at the same time, the segment
search of SPICE decides, with every source taking part as a loaded file:

- **The most recently loaded wins**, whether it is an SPK file, a
  programmatic kernel, a loader's kernel, or an `add_ephemeris` provider
  (which counts as a file loaded when it was added).
- **Within one file or kernel**, the later segment wins (and a kernel's
  providers come after its table segments).
- **Unloading** brings back what the unloaded source was hiding.
- `add_ephemeris` for a body that already has one *from the program*
  replaces it (it becomes the most recent). Kernels' providers are not
  replaced: two loaded files with the same body coexist.
- `remove_ephemeris(body)` removes the program's provider only, never one a
  kernel owns.

## Threads

- **Queries** run from any number of threads at once, each with its own
  context, whatever the sources.
- **Changes** (`furnsh`, `unload`, `kclear`, `add_ephemeris`,
  `load_kernel`, `add_loader`, `boddef`) are methods of the instance and
  must not run while other threads query.
- **Providers** (and a loader's models) are called from the querying
  threads: they must not change shared data.
- **Builders** can be filled by any thread, each its own, while others query.
- **Loaders** run only inside `furnsh`, `unload` and `kclear`, on the
  instance.

## From C and Python

| | C (`include/pepper.h`) | Python (PepperyPy) |
|---|---|---|
| SPICE kernels | `pepper_spice_furnsh(spice, file, in_memory)` | `spice.furnsh(path, in_memory=None)` |
| Providers | `pepper_spice_add_ephemeris(spice, body, center, frame, tbeg, tend, fn, user)` | `spice.add_ephemeris(body, center, frame, tbeg, tend, provider)` |
| Programmatic kernels | `pepper_builder_new`, `pepper_builder_spk` (type 9 or 13), `pepper_builder_define_body`, `pepper_spice_load_kernel` | `KernelBuilder` (`spk_hermite`, `spk_lagrange`, `define_body`), `spice.load_kernel(name, kb)` |
| Save as SPK | `pepper_spice_write_kernel` | `spice.write_kernel(name, file)` |
| Kernel loaders | not yet | not yet |
| Models in a kernel | not yet | not yet |
| Frames: providers | `pepper_spice_add_frame` | `spice.add_frame(...)` |
| Frames: tables and models in a kernel | not yet | not yet |

Python, a programmatic kernel from NumPy arrays:

```python
import numpy as np
from pepperypy import Spice, KernelBuilder

spice = Spice()
ets = 60.0 * np.arange(200)
states = np.column_stack([...])            # (n, 6): km, km/s

kb = KernelBuilder()
kb.spk_hermite(-10001, 399, 'J2000', ets, states, 7)
kb.define_body('MY_SC', -10001)
spice.load_kernel('my_trajectory', kb)
state, lt = spice.context().spkezr('MY_SC', ets[10], 'J2000', 'NONE', 'EARTH')
spice.write_kernel('my_trajectory', 'my_trajectory.bsp')
```

Kernel loaders are Fortran types, so they cannot be registered from C or
Python yet; the plan (`plans/external_loaders_plan.md`) has this as a to-do.

The legacy interface (SPICELIB's routines, `use pepper_legacy`) works on a
default instance, `legacy_spice()` (module `pepper_legacy_context`). A
program can register loaders on it, and the legacy `FURNSH` then uses them by
extension, `detect` and `KERNEL_LOADERS` (it has no `loader` argument):

```fortran
use pepper_legacy_context, only: legacy_spice
type(pepper_instance), pointer :: s
s => legacy_spice()
call s%add_loader('CSV', csv, extensions=['.csveph'])
call furnsh('track.csveph')                ! the legacy FURNSH
```

## Reference

### The routines

| Routine | What it does |
|---|---|
| `spice%furnsh(file, in_memory, loader)` | load a kernel, a meta-kernel, or a file a loader reads |
| `spice%unload(name)`, `spice%kclear()` | unload one kernel, or everything |
| `spice%add_ephemeris(body, center, frame, tbeg, tend, provider)` | register a provider |
| `spice%remove_ephemeris(body)` | remove the program's provider of a body |
| `kb%spk_hermite(ctx, body, center, frame, epochs, states, degree, tbeg, tend, ident, adapt)` | add a Hermite (type 13) segment |
| `kb%spk_lagrange(...)` | add a Lagrange (type 9) segment |
| `kb%add_ephemeris(ctx, body, center, frame, tbeg, tend, provider, ident)` | add a provider owned by the kernel |
| `kb%define_body(ctx, name, code)` | define a body name owned by the kernel |
| `kb%frame_rotations(ctx, name, code, center, base, epochs, quats_or_rots, degree, av, method, inverse, tbeg, tend, ident)` | add a table of rotations: a frame defined by the kernel |
| `kb%add_frame(ctx, name, code, center, provider)` | add a frame provider owned by the kernel |
| `spice%add_frame(name, code, center, provider)`, `spice%remove_frame(name)` | the program's frame providers |
| `kb%set_format(text)`, `kb%clear()`, `kb%nsegments()` | describe, empty, count |
| `spice%load_kernel(name, kb, keep)` | load a builder as a kernel |
| `spice%write_kernel(name, file)` | write a kernel built in memory to an SPK file |
| `spice%add_loader(name, loader, extensions, replace)` | register a kernel loader |
| `spice%remove_loader(name)`, `spice%loader(name, ptr)` | remove one, access one |
| `ctx%kernel_format(file, loader, format, found)` | the loader and description of a kernel |
| `ctx%spk_coverage(file, body, intervals)`, `ctx%spk_objects(file, ids)` | coverage and bodies of an SPK or kernel |
| `ctx%kdata`, `ctx%kinfo`, `ctx%kernel_info`, `ctx%ktotal` | what is loaded |

The full documentation of each is in the API docs (fdoc): `spice_furnsh` has
the complete rules of how a loader is chosen.

### The errors

| Short message | When |
|---|---|
| SPICE(NOSUCHFILE) | `furnsh` of a file that does not exist (without `loader=`) |
| SPICE(UNKNOWNLOADER) | `loader=` or `KERNEL_LOADERS` names no registered loader |
| SPICE(LOADERCONFLICT) | a loader name or extension registered twice without `replace` |
| SPICE(BADKERNELLOADERS) | `KERNEL_LOADERS` with an odd number of values |
| SPICE(TOOFEWSTATES), SPICE(INVALIDDEGREE), SPICE(TIMESOUTOFORDER), SPICE(BADDESCRTIMES), SPICE(SEGIDTOOLONG), SPICE(NONPRINTABLECHARS) | a builder segment fails SPKW09's or SPKW13's checks |
| SPICE(INVALIDSIZE) | states not a 6 by n array |
| SPICE(BARYCENTEREPHEM), SPICE(BODYANDCENTERSAME) | a body that is its own center |
| SPICE(INVALIDREFFRAME), SPICE(UNKNOWNFRAME) | an unknown frame, found at `load_kernel` (or `furnsh` of a loader's file) |
| SPICE(BLANKFILENAME) | `load_kernel` with a blank name |
| SPICE(BLANKNAMEASSIGNED) | `define_body` with a blank name |
| SPICE(NOSUCHKERNEL) | `write_kernel` of a name no kernel built in memory has |
| SPICE(FILEEXISTS) | `write_kernel` to a file that exists |
| SPICE(NOTWRITABLE) | `write_kernel` of a kernel that holds a provider or frames |
| SPICE(FRAMEEXISTS) | a frame's name or code is another frame's |
| SPICE(ZEROQUATERNION), SPICE(NOTAROTATION) | a rotation table's bad quaternion or matrix |
| SPICE(NOFILEFORKERNEL) | an operation that needs a file (such as reading its comments through a unit) on a kernel built in memory |
| SPICE(NOEPHEMERISDATA) | a provider returned `found = .false.` |
| SPICE(RECURSIONTOODEEP) | providers nested more than 50 deep |

A loader's own errors are its library's (e.g. SPICE(BADCSVFILE) above).

## Troubleshooting

- **SPICE(SPKINSUFFDATA) or "no data" for a body you loaded.** Check the
  coverage (`spk_coverage`), the center chain (the body's center must
  itself be reachable from the observer), and, for a builder, `tbeg`/`tend`
  and gaps between segments.
- **`unload` does nothing.** The name must match exactly as loaded: `'./a.bsp'`
  and `'a.bsp'` are different names. A meta-kernel's files are named with
  their path symbols replaced. `kdata` lists the names.
- **A loader is not used for a file.** A SPICE binary kernel is never given
  to a loader except by `loader=`. Check the extension (registered, and the
  longest match among loaders), or that `detect` accepts the file's first
  bytes. `kernel_format` tells which loader read a kernel.
- **A text kernel is taken by a loader.** A loader's extension or `detect`
  claimed it: give `loader='TEXT'` (PEPPER's own loading) for that file.
- **States differ slightly from another tool's at times between the
  rows.** Interpolation differs between tools (window placement, the method
  at the ends). At the rows the states match within rounding; check the
  degree and the method the file names.
- **Times are off by about a minute.** UTC versus TDB: a loader reading UTC
  must convert with the leap seconds (`str2et` with a leap-seconds kernel
  loaded first).
- **A name works in one run but not another.** Kernel pool names
  (`NAIF_BODY_NAME`) take precedence over `boddef` and kernels' names;
  a name kernel loaded later can redefine it.
