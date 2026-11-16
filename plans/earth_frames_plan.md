# Earth frames: modern precession, nutation and Earth orientation

A stand-alone modern Fortran library that computes the Earth's orientation
with the current IAU models, from the IAU SOFA algorithms and the EOP files
CelesTrak publishes. It gives PEPPER the frames it lacks (GCRF, CIRS, TIRS,
ITRF by IAU 2006/2000A, TEME, MOD/TOD of either model family) as programmatic
frames, configured by a JSON file.

The name used below, `earth-frames`, is a placeholder.

## Why

What SPICE offers today:
- **ITRF93:** from a binary PCK that NAIF builds from EOP data
  (`earth_latest_high_prec.bpc`). Its accuracy is fine, but its models are
  NAIF's choice, it covers only the dates NAIF published, and it gives no
  control over the EOP.
- **Frames of date** (`EARTH_TRUE_EQUATOR_OF_DATE` and the others): only
  IAU 1976 precession and IAU 1980 nutation, and without the observed
  corrections dψ and dε.
- **No frame bias.** SPICE's J2000 is used as if it were ICRF; the bias
  between them is about 23 mas.
- **Missing frames:** TEME (the frame of TLEs), GCRF, CIRS and TIRS are not
  built in.
- **No UT1** as a time scale, and no way to load EOP.

Orekit, GMAT, STK and Vallado's codes all use the IERS 2010 conventions: IAU
2006 precession, IAU 2000A nutation, CIO-based Earth rotation, and EOP from
IERS. This library gives PEPPER the same frames, so PEPPER can be compared
with them directly.

## Scope

**In:**
- The frames listed below, as 3x3 rotations and 6x6 state transformations
  to J2000.
- EOP from CelesTrak (CSV and legacy TXT).
- Time handling: ET to TT, UTC and UT1.
- A JSON configuration.
- A PEPPER adapter: a `frame_provider`, a registration call, and a kernel
  loader for the configuration file.

**Out (later, or other libraries):**
- Tidal and libration corrections to the EOP (IERS Conventions ch. 5 and 8;
  not in SOFA).
- IERS EOP formats other than CelesTrak's: finals2000A, C04 directly.
- Python wrappers.
- Ecliptic frames of date, unless phase 5 adds them (SOFA's ECM06 makes it
  easy).

**The core has no PEPPER dependency.** The models, the EOP reader and the
time conversions work without PEPPER, so the library is also useful on its
own. Only the adapter uses PEPPER.

## Frames

Every frame is given **relative to J2000** (EME2000). PEPPER chains from
there, and one query evaluates the models once. Default names and IDs are
listed; the configuration can rename them, and the IDs start at 1400000, as
NAIF suggests for programmatic frames.

| Frame | Meaning | Models (option `model`) | EOP used |
|---|---|---|---|
| `GCRF` | ICRS axes, geocentric | frame bias only (BI00 / BP06), constant | none |
| `MOD` | mean equator and equinox of date | `IAU1976` (PMAT76), `IAU2006` (PMAT06, bias included) | none |
| `TOD` | true equator and equinox of date | `IAU1980` (PNM80 + dψ, dε), `IAU2000A`/`IAU2000B`/`IAU2006` (equinox-based, + dX, dY) | dψ, dε or dX, dY |
| `TEME` | true equator, mean equinox (SGP4) | `IAU1980` as Vallado et al. (2006) define it: PEF rotated by GMST 1982 | (UT1 for PEF), dψ, dε optional |
| `PEF` | pseudo-Earth-fixed (IAU 1980 family) | TOD rotated by GAST (GST94) | UT1, dψ, dε |
| `CIRS` | Celestial Intermediate Reference System | `IAU2006` (XYS06A), `IAU2000A`, `IAU2000B` | dX, dY |
| `TIRS` | Terrestrial Intermediate Reference System | CIRS rotated by the Earth rotation angle (ERA00) | UT1, dX, dY |
| `ITRF` | the terrestrial frame | `IAU2006` (CIO-based, default), `IAU2000A`/`B`, `IAU1980` (equinox-based, = PEF + polar motion); `method`: `CIO` or `EQUINOX` | UT1, x, y, dX, dY (or dψ, dε) |

Options per frame:
- `polar_motion`: x, y and s′ (SP00, POM00); default true; ITRF only.
- `pole_offsets`: dX, dY or dψ, dε from the EOP; default true.
- `eop_required`: whether a missing EOP value is an error, or the frame falls
  back to zero.

**Accuracy to document:**
- IAU 2006/2000A with pole offsets matches the observed pole to the EOP's
  accuracy (sub-mas).
- IAU 2000B: about 1 mas.
- IAU 1980 without dψ, dε: tens of mas.

**Names.** The defaults must not be SPICE's names (`ITRF93`, `EARTH_FIXED`),
so the two can be compared side by side. A user who wants `EARTH_FIXED` to
mean this ITRF defines it as a TK alias, as SPICE's frames kernels already
do.

**Matching EOP to models.** The pole offsets in the EOP refer to a specific
model. In IERS C04 (CelesTrak's source), dX and dY are relative to IAU
2006/2000A, and dψ and dε to IAU 1976/1980. Using them with another model is
an error. This must be confirmed against the C04 documentation before
implementation.

## Time

PEPPER gives the provider ET (TDB seconds past J2000). The models need:

- **TT.** `ctx%unitim(et, 'TDB', 'TT')`, so that ET means exactly what it
  means everywhere else in PEPPER. As an option (`time.tdb_tt: "sofa"`),
  SOFA's DTDB.
- **UTC**, for the EOP's daily index and for UT1. Either PEPPER's leapseconds
  (the loaded LSK, through `ctx`; the default, consistent with STR2ET), or
  the EOP's DAT column (`time.leap_seconds: "eop"`).
  - SOFA's DAT has a built-in table that ages, so it is not used.
- **UT1** = UTC + (UT1−UTC), interpolated.
- **Two-part Julian dates** for SOFA: `2451545.0` and `et/86400`, so no
  precision is lost to one large double.

## EOP

**Formats** (as CelesTrak documents them; checked 2026-11-16):
- **CSV**, `EOP-All.csv` (from 1962) and `EOP-Last5Years.csv`. Header
  `DATE,MJD,X,Y,UT1-UTC,LOD,DPSI,DEPS,DX,DY,DAT,DATA_TYPE`.
  - Units: x, y, dψ, dε, dX, dY in arcseconds; UT1−UTC and LOD in seconds;
    DAT in seconds.
  - `DATA_TYPE` is `O` (observed) or `P` (predicted).
- **Legacy TXT**, `EOP-All.txt` and `EOP-Last5Years.txt`. Fixed columns
  `FORMAT(I4,I3,I3,I6,2F10.6,2F11.7,4F10.6,I4)`, with keywords (`VERSION`,
  `UPDATED`, `NUM_OBSERVED_POINTS`, `BEGIN OBSERVED`/`END OBSERVED`,
  `BEGIN PREDICTED`/`END PREDICTED`, and an obsolete `NGA_COEFFICIENTS`
  section to skip).
- **Source:** IERS EOP C04 for the observed data, and IERS predictions for
  180 days; updated daily.

**Reading:**
- Daily rows, sorted by MJD and checked: daily spacing, no gaps unless
  allowed, a known header.
- Each row is marked observed or predicted.
- Errors name the file and the line.
- The table is immutable once read.

**Interpolation** (`eop.interpolation`):
- `linear`, or `lagrange` with `lagrange_points` (default 4, IERS practice).
- **UT1 is interpolated as UT1−TAI**, which is continuous, then converted
  back. UT1−UTC jumps by one second at each leap second, so interpolating it
  directly is wrong across a leap second.

**Outside the table** (`eop.outside`):
- `error` (default): SPICE(EOPNOTAVAILABLE) through the provider (`found`
  false, with the error signaled).
- `zero`: the values are zero, as SOFA would assume.
- `hold`: the nearest row.

**Predicted rows** (`eop.predicted`): `use` (default), `error`, or `zero`.
The query can't tell whether a prediction was used, so this choice belongs in
the configuration.

**LOD** gives the Earth's rotation rate for the 6x6 derivative:
ω = 7.292115146706979e-5 · (1 − LOD/86400) rad/s.

## Derivatives (the 6x6)

PEPPER's `sxform` needs dR/dt.

- `derivative: "rotation"` (default): an analytic derivative of the
  Earth-rotation factor (ERA or GAST, with the ω above), with precession,
  nutation and polar motion taken as constant over the derivative. This is
  what SPICE's PCK frames effectively do. The error is about 1e-12 rad/s,
  small against ω's 7e-5.
- `derivative: "finite_difference"`: a central difference of the full
  rotation (step configurable), for frames without Earth rotation (MOD, TOD,
  CIRS) when their slow rate matters.
- The frames without Earth rotation report their derivative from the
  configured method. The constant GCRF bias has a zero derivative.

The adapter also implements `rotation` (the 3x3 only), so `pxform` doesn't
pay for the derivative.

## Performance

- **IAU 2000A nutation** is about 1365 luni-solar and planetary terms per
  evaluation: tens of microseconds, against well under a microsecond for a
  binary PCK.
- **`cip.method: "table"`** precomputes the CIP's X, Y and s (or the
  equinox-based angles) on a grid over the EOP span (default step 0.5 day),
  and interpolates them (Lagrange). Done at load. Whether 0.5 day is fine
  enough for microarcsecond accuracy is checked in testing.
- **`cip.method: "series"`** evaluates the series per query.
- **IAU 2000B** (77 terms) is a cheaper series for users who accept about
  1 mas.
- **Thread safety.** The provider is `intent(in)` and immutable after setup,
  with no "last epoch" cache, because PEPPER calls it from many contexts at
  once. Any cache would have to live outside the provider. None is planned:
  the table method makes one more unnecessary.

## Configuration (JSON, json-fortran)

```json
{
  "eop": {
    "file": "EOP-All.csv",
    "format": "celestrak-csv",
    "interpolation": "lagrange",
    "lagrange_points": 4,
    "outside": "error",
    "predicted": "use"
  },
  "time": {
    "leap_seconds": "pepper",
    "tdb_tt": "pepper"
  },
  "cip": { "method": "table", "step_days": 0.5 },
  "derivative": "rotation",
  "frames": [
    { "name": "GCRF",          "id": 1400500, "type": "GCRF" },
    { "name": "ITRF_IAU2006",  "id": 1400501, "type": "ITRF", "model": "IAU2006", "method": "CIO" },
    { "name": "TIRS_IAU2006",  "id": 1400502, "type": "TIRS", "model": "IAU2006" },
    { "name": "CIRS_IAU2006",  "id": 1400503, "type": "CIRS", "model": "IAU2006" },
    { "name": "TOD_IAU1980",   "id": 1400504, "type": "TOD",  "model": "IAU1980" },
    { "name": "TEME",          "id": 1400505, "type": "TEME" },
    { "name": "ITRF_IAU1980",  "id": 1400506, "type": "ITRF", "model": "IAU1980", "polar_motion": true }
  ]
}
```

- **Paths** (`eop.file`) are relative to the configuration file.
- **Every key is checked.** An unknown key, a wrong type, or an inconsistent
  pair (dX/dY with IAU1980) is an error naming its JSON path, e.g.
  `frames[3].model`.
- **Defaults** as listed above. A configuration of only `{"eop": {"file":
  ...}}` gives the default frame set.
- A frame may override `derivative` and `cip`.

## Architecture

```
earth-frames/
  fpm.toml                 # dependencies: json-fortran; pepper (adapter only)
  src/
    ef_kinds.f90           # ip, wp
    ef_sofa_*.f90          # the SOFA routines used, in modern Fortran (see SOFA)
    ef_eop.f90             # eop_table: read CSV/TXT, interpolate
    ef_time.f90            # TT, UTC, UT1 from ET (given TDB-TT and leapseconds)
    ef_models.f90          # rotation(model, frame type, tt, ut1, eop) -> R, dR/dt
    ef_config.f90          # JSON -> earth_frames_config (json-fortran)
    ef_frames.f90          # earth_frame_set: the configured frames, CIP tables
  pepper/
    ef_pepper.f90          # earth_frame_provider (extends frame_provider),
                           # register_earth_frames, earth_frames_loader
  test/ ...
  example/ ...
```

- **`ef_models`** is the core API: pure procedures that take TT and UT1 (as
  two-part dates) and the EOP values, and return the rotation from J2000 to
  the frame and its derivative. It needs no PEPPER, no files and no JSON.
- **`earth_frame_set`** holds the EOP table, the CIP tables and the frame
  definitions. It is built from a configuration, then read-only.
- **Packaging.** One fpm package is simplest. Making the adapter a second
  package (in `pepper/`, depending on the core by path) keeps the core usable
  without PEPPER. To decide; see the end.

## SOFA

SOFA's license (checked 2026-11-16, Issue 2023-10-11, the nineteenth Fortran
77 release) allows two ways to use it:

- **(a) Unmodified copies of the Fortran 77 `.for` files, wrapped.** No
  derived-work duties beyond keeping the license. But this is fixed-form
  F77 code with `iau_` names and implicit interfaces, and fpm's handling of
  `.for` files must be checked.
- **(b) A modern Fortran translation of the routines used (recommended).**
  This is a derived work, which must:
  - say it uses routines derived from SOFA under license and is not
    provided or endorsed by SOFA;
  - describe, in its source, how each routine differs from the original;
  - use no routine name containing `iau` or `sofa`, in any case. For example,
    `iau_PNM06A` becomes `pnm06a` in module `ef_sofa_pn`;
  - not misrepresent the origin, and reproduce the license conditions intact
    in the source distribution.

  Each translated routine gets a header: "Derived from SOFA iau_XXX (Issue
  2023-10-11): translated to free-form Fortran 2008, explicit kinds and
  intents, pure; the algorithm and constants unchanged."

  The license makes an exception for DAT (leap seconds), which may be
  replaced under its own name. This library doesn't need it.

**Routines needed** (to be confirmed against SOFA's index):
- **Bias and precession:** BI00, BP00, BP06, PB06, PMAT76, PMAT06, PREC76,
  OBL80, OBL06, PFW06, FW2M, NUMAT.
- **Nutation:** NUT80, NUT00A, NUT00B, NUT06A, PN00, PN06, PNM80, PNM00A,
  PNM00B, PNM06A, with the fundamental arguments FAL03, FALP03, FAF03, FAD03,
  FAOM03, FAME03, FAVE03, FAE03, FAMA03, FAJU03, FASA03, FAUR03, FANE03 and
  FAPA03.
- **CIO:** XY06, S06, S00, XYS00A, XYS00B, XYS06A, C2IXYS, C2IXY.
- **Earth rotation:** ERA00, GMST82, GMST06, GST94, GST06, GST06A, EE00,
  EE06A, EQEQ94.
- **Polar motion and assembly:** SP00, POM00, C2TCIO, C2TEQX.
- **Time (optional):** DTDB.
- **Matrix and angle helpers** (RX, RY, RZ, RXR, TR, ANP, ANPM): replaced by
  the library's own small helpers. They aren't worth translating.

**Tests.** SOFA's `t_sofa_f.for` gives test values for each routine. They
become the library's unit tests: the same inputs and expected values, with
SOFA's tolerances.

## PEPPER integration

**1. Register in code:**

```fortran
type(earth_frame_set) :: earth

call earth%load(ctx, 'earth_frames.json')       ! reads the EOP, builds the CIP tables
call register_earth_frames(spice, earth)        ! spice%add_frame for each configured frame
call ctx%pxform('J2000', 'ITRF_IAU2006', et, r)
```

The provider holds a pointer to the immutable set, so the frames share one
EOP table.

**2. Load as a kernel (recommended for users).** A kernel loader registered
for `.frames.json`, built on the loader support already in PEPPER:

```fortran
call spice%add_loader('EARTH_FRAMES', earth_frames_loader(), extensions=['.frames.json'])
call spice%furnsh('earth.frames.json')          ! frames defined, owned by this kernel
call spice%unload('earth.frames.json')          ! frames gone
```

- The loader reads the configuration and the EOP, and adds each frame with
  `kb%add_frame`, so the frames belong to the kernel and go at UNLOAD.
- `.frames.json` is matched as a loader extension (FURNSH's step 3) before
  PEPPER's own `.json` kernels (step 5). The longest extension wins.
- A meta-kernel can list it next to the LSK it depends on. With
  `time.leap_seconds: "pepper"`, the LSK must be loaded before the first
  query, not before the load.
- A new EOP file means unloading and loading the configuration again.

**Rules from PEPPER's providers:**
- Queries inside the provider are allowed (`ctx%unitim`, the leapseconds).
- `found` is false with an error signaled when EOP is missing.
- No state is written.

**C and Python:** later, through the C interface's frame callbacks or a
small C shim (`earth_frames_register(spice, path)`), plus a Python wrapper of
that one call.

## Validation

1. **SOFA's test values** for every translated routine (`t_sofa_f`).
2. **SOFA's Earth-attitude cookbook example** (the worked example in "SOFA
   Tools for Earth Attitude": given UTC, EOP x, y, UT1−UTC, dX and dY, it
   gives every intermediate matrix for each method). Every frame type and
   model is checked against it to about 1e-12. It also confirms the order of
   the corrections.
3. **TEME:** Vallado et al. (2006), "Revisiting Spacetrack Report #3" (AIAA
   2006-6753): the example satellite's TEME→ITRF/PEF results.
4. **Orekit:** fixtures from a small Orekit program (GCRF, CIRS, TIRS,
   ITRF/IERS 2010 without tides, TOD/MOD, TEME), using the same CelesTrak
   file, at a few hundred epochs from 1980 to now. The two must agree to
   better than about 1e-10 rad, once the EOP interpolation is the same.
   Differences are traced, not just bounded.
5. **SPICE ITRF93:** compared with `earth_latest_high_prec.bpc` over its span.
   The ITRF frames should agree with it to the mas level (centimeters at the
   surface). A larger difference points to a units or sign error.
6. **Internal consistency:**
   - `rotation` against the upper-left block of `transform`;
   - the analytic derivative against a central difference;
   - R·Rᵀ = I;
   - CIO-based ITRF against equinox-based ITRF (they agree to µas);
   - TOD from IAU 1980 with dψ, dε against TOD from IAU 2006 with dX, dY
     (they agree to the EOP's level).
7. **EOP edge cases:** interpolation across a leap second (UT1 continuous);
   the first and last rows; predicted rows under each policy; gaps; a
   malformed line (the error names the line).
8. **PEPPER:**
   - `pxform`/`sxform` through the registered frames;
   - load and unload through the loader;
   - many contexts querying at once (bitwise the same as serial);
   - a TK alias (`EARTH_FIXED` → `ITRF_IAU2006`).

## Phases

1. **SOFA translation and its tests.** The routines above, with
   `t_sofa_f`'s values; the license notices.
2. **Models** (`ef_models`): every frame type and model from TT, UT1 and EOP
   values; the cookbook example; the derivatives.
3. **EOP and time:** the CSV and TXT readers, the interpolation
   (UT1−TAI), the policies; ET to TT, UTC and UT1.
4. **Configuration and frame set:** json-fortran parsing and checking,
   defaults, the CIP tables.
5. **PEPPER adapter:** the provider, `register_earth_frames`, the kernel
   loader; PEPPER tests; the SPICE ITRF93 comparison.
6. **Orekit and Vallado comparisons**, the performance measurements (series
   against table, against a binary PCK), and docs: a user guide like
   `guides/ephemeris_data.md`, with the accuracy table.
7. **Later:**
   - tidal and libration EOP corrections;
   - IERS finals2000A and C04 readers;
   - ecliptic frames of date;
   - C and Python;
   - a writer that turns a configured ITRF into a binary PCK, for SPICE
     users. PEPPER's `write_kernel` writes CKs, not binary PCKs, so this
     needs a PCK type 2 fit.

## Decisions to make

- **The library's name and repository.**
- **SOFA:** translate (b, recommended) or vendor unmodified (a).
- **Packaging:** one fpm package with PEPPER as a dependency, or a core
  package plus a PEPPER adapter package.
- **Default leap seconds:** PEPPER's LSK (recommended: consistent with the
  rest of PEPPER) or the EOP's DAT.
- **Default CIP method:** `table` (fast; the table's accuracy to be confirmed)
  or `series` (simplest).
- **Default frame names and IDs**, and whether the frames are relative to
  J2000 only (recommended: one model evaluation per query) or chained
  (ITRF→TIRS→CIRS→GCRF→J2000, so each frame's definition shows its
  structure, at several evaluations per query).
- **Tides in v1?** Recommended: no; add them in phase 7 with IERS
  Conventions code (not SOFA).
