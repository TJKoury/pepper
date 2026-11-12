# JSON text kernels: plan

**Status:** implemented, with the decisions D1–D7 as proposed.

What was built:
- `src/pepper_json.f90`: the parser (pure, no state), `json_kernel_parse`;
- `ldjson` and `is_json_kernel` in `src/pepper_pool.f90`, and the dispatch
  in LDPOOL;
- `tools/tk2json.py`: text kernel → JSON (comments kept as `//` lines), and
  `--reverse`;
- the tests:
  - `test/core/test_json_kernels.f90`: the parser and its errors, the same
    pool from `.tpc` and `.json`, the extension, atomic loads, watches,
    bodies and frames, KEEPER, meta-kernels;
  - `test_parallel` now loads its frame kernel as JSON;
  - `python/tests/test_json_kernels.py`: jsonspice's tests, and the
    converter both ways on SpiceyPy's test kernels;
  - a JSON kernel in `capi/test_capi.c`;
- the README's "JSON kernels" section, and `pepper_vs_spicelib.md`.

Found along the way:
- **STPOOL and SEPOOL (FURNSH's `PATH_VALUES` and `KERNELS_TO_LOAD`)** read
  each string through an 80-character buffer, SPICELIB's limit of pool
  strings. PEPPER's pool strings have any length, so a longer path value or
  file name was cut silently. They now read the whole string; this applied
  to text kernels too.
- **SPICELIB's NPARSD**, the number reader of text kernels, is not correctly
  rounded (up to a few ulps; `spicelib_bugs.md`). JSON numbers, read with
  Fortran's READ, are, so the same literal can differ in the last bits.
  The tests compare to within a few ulps.

## Origin: jsonspice

[jsonspice](https://github.com/jacobwilliams/jsonspice) defines a JSON form of
SPICE text kernels (see also [JSON + SPICE](https://degenerateconic.com/json-spice.html)).
It monkey-patches SpiceyPy's `furnsh`. When a file name ends in `.json`, or
the argument is a dict, it parses the JSON in Python and feeds each variable
to the kernel pool through `pdpool`/`pcpool`. `+=` is emulated with
`dtpool`/`gdpool`/`gcpool` followed by `dvpool` and a rewrite. Meta-kernel
variables are handled by recursive `furnsh` calls.

```json5
// An example kernel
{
    "TIME3": "@1972-JAN-1",          // a time: UTC seconds past J2000
    "AAA" : 1,
    "+AAA" : 2,                      // "+=": append to AAA
    "VARIABLE": [1.0, 2.0, 3.0],
    "VARIABLE2": 42.0
}
```

That approach has the following limits:
- **No KEEPER entry.** The kernels are not entered in KEEPER. `unload` of a
  JSON kernel does nothing, and `kdata`/`ktotal` do not list it.
- **Lost on unload.** Unloading any other text kernel clears the pool and
  reloads only the files KEEPER knows, so the JSON variables are lost.
- **Different meta-kernel semantics.** jsonspice loads the listed kernels
  *before* the file's other variables, and never puts `KERNELS_TO_LOAD`,
  `PATH_SYMBOLS` or `PATH_VALUES` in the pool. That differs from FURNSH.
- **Not atomic.** An error part-way through leaves some variables loaded.
- **Slow `+=`.** Each append reads the whole variable back, deletes it and
  writes it again.

## Design in PEPPER

A JSON kernel is a text kernel with a different syntax. It goes through
FURNSH, KEEPER, the pool and its watches exactly as a `.tpc` does. Only the
reader is new.

### Where it plugs in

All text kernel loads already pass through `ldpool`:
- FURNSH: `furnsh` → `zzldker` → `ldpool`, for a file GETFAT does not
  recognize as DAF/DAS/XFR/TE1.
- UNLOAD: `unload` reloads the remaining TEXT and META kernels with
  `ldpool` (pepper_keeper.f90).
- The user calls `spice%ldpool(file)`.

So `ldpool` dispatches on the file name:
- If it ends in `.json` (in any case), the file is JSON, read by the new
  `ldjson`.
- Otherwise the existing text kernel reader (`rdknew`/`zzrvar`) reads it.

The file's content is not inspected (D2). A JSON kernel without the
extension is read as a text kernel and loads nothing, since it has no
`\begindata`. A text kernel named `.json` gets a JSON syntax error.

KEEPER records the file as type `TEXT`, or as `META` when it sets
`KERNELS_TO_LOAD`. `kdata`, `kinfo`, `ktotal('TEXT')` and `unload` work
unchanged, and so does `zzbodkik` after the load. GETFAT is unchanged
(SPICELIB's routine, public API; D3).

### The parser: `pepper_json` (new module, pure)

The module has no context, no state and no I/O. Its input is the file's
text; its output is the list of assignments in file order:

```fortran
type :: json_assignment
   character(:), allocatable :: name      ! without the leading '+'
   logical :: append = .false.            ! "+NAME"
   integer :: kind                        ! numeric, string or time
   real(wp), allocatable :: numbers(:)
   type(string), allocatable :: strings(:)
   type(string), allocatable :: times(:)  ! '@' values, without the '@'
   integer :: line                        ! of the key, for the messages
end type
pure subroutine json_kernel_parse(text, assignments, ok, message, line)
```

It accepts:
- standard JSON (RFC 8259), with string escapes including `\uXXXX`, which
  becomes UTF-8;
- two extensions: `//` line comments, which jsonspice files use (through
  json5), and trailing commas (D4).

Mapping from JSON to the pool:

| JSON | Pool |
|---|---|
| number | numeric value |
| `true` / `false` | 1.0 / 0.0 (as jsonspice) |
| string | character value |
| string `"@..."` | numeric: the time, UTC seconds past J2000 (TPARSE, as `@` in text kernels) |
| array of the above | the values in order; all numeric/time, or all strings |
| `"+NAME": ...` | append to NAME (the text kernel `+=`); creates NAME if absent |
| `null`, nested object, empty array, mixed array | error |

Duplicate keys apply in order, as repeated `NAME = ...` lines in a text
kernel do: the later one replaces the earlier. Names follow the text kernel
rules (non-blank, printable, no embedded blanks) and have any length, as
everywhere in PEPPER.

A hand-written parser is a few hundred lines. It is recommended over a
dependency such as json-fortran (D1): PEPPER has no dependencies, and the
fpm, meson (PepperyPy) and C builds would all need one. A pure procedure is
also thread-safe by construction, and the subset needed is small: one
object of scalars and flat arrays.

### The loader: `ldjson` (pepper_pool)

```text
read the file whole (stream access, one allocation)
json_kernel_parse                    -> syntax errors: nothing loaded
validate every assignment            -> TPARSE of '@' values,
                                        append type vs pool type
apply: ps%create / clear_values (unless append) / add_number / add_string,
       pool_changed(name) per variable  (watches, caches)
```

Parsing and validating before applying anything makes a JSON kernel
**atomic**: on an error, nothing in it is loaded. A text kernel's error
leaves the variables before it loaded; that difference goes in
`pepper_vs_spicelib.md` (D5). Appending is a direct `add_*` on the existing
record, with no read-back.

Error messages follow `zzrvar`'s style: they give the file and line, use
SPICELIB's short messages where one fits, and add one new one:

| Error | Short message |
|---|---|
| syntax | SPICE(JSONSYNTAXERROR) (new) |
| `null` / nested / empty / mixed | SPICE(BADVARASSIGN) |
| append of strings to numbers, or the reverse | SPICE(TYPEMISMATCH) |
| bad `@` time | SPICE(BADTIMESPEC) (D6) |

### Meta-kernels

No new code is needed. FURNSH watches `KERNELS_TO_LOAD`, `PATH_SYMBOLS` and
`PATH_VALUES` in the pool, and a JSON kernel that sets them is a meta-kernel
with SPICELIB's semantics:
- the file's variables load first, then the listed kernels;
- `$SYMBOL` substitution and `+` continuation of long names are SPICELIB's;
- the listed kernels may themselves be JSON;
- the three variables are removed afterwards.

```json5
{
    "PATH_VALUES": ["/data/kernels"],
    "PATH_SYMBOLS": ["K"],
    "KERNELS_TO_LOAD": ["$K/naif0012.tls", "$K/de440.bsp", "$K/masses.json"]
}
```

### The interfaces

No new API: JSON kernels are files, loaded by the existing routines.
- **Fortran:** `spice%furnsh('x.json')` and `spice%ldpool('x.json')`. The
  legacy module's `FURNSH` works too.
- **C:** `pepper_furnsh`.
- **PepperyPy:** `furnsh`. jsonspice's file-based tests are ported to
  `python/tests/test_json_kernels.py`.

jsonspice's `furnsh(dict)` (loading a Python dict) is not supported.

## Tests (`test/core/test_json_kernels.f90`)

- **Parser** (pure; no instance):
  - scalars, arrays, escapes, `\u`, `//` comments, trailing commas, a BOM;
  - `/* */` is a syntax error;
  - every error case, with its line number.
- **Equivalence:** the same data as `.tpc` and as `.json`, in two instances,
  gives the same pool: `gnpool`, `dtpool`, `gdpool`/`gcpool`. This includes
  `@` times, `+=`, and NAIF's generic `pck00011.tpc` converted to JSON
  (`gdpool` of every BODY variable).
- **Behaviour:**
  - `.json` and `.JSON` are read as JSON; the same JSON under another name
    is read as a text kernel and loads nothing;
  - errors load nothing (atomic);
  - watches fire (`swpool`/`cvpool`);
  - a frame or body defined in JSON is used by `pxform`/`bodn2c`;
  - `kdata`/`ktotal('TEXT')`;
  - `unload` of a JSON kernel;
  - `unload` of a `.tpc` reloads the JSON kernels;
  - `kclear`.
- **Meta-kernels:**
  - a JSON meta-kernel loading `.tpc`, `.bsp` and `.json` with
    `PATH_SYMBOLS`;
  - a `.tm` meta-kernel listing a `.json`.
- **Threads:** `test_parallel` loads part of its kernel set as JSON.
- **Interfaces:** the jsonspice test suite under PepperyPy, and the C test.
- **tspice:** unchanged and must still pass (the dispatch must not touch
  `.tpc` loads).

## Work

1. `pepper_json`: the parser and its unit tests.
2. `ldjson` and the dispatch in `ldpool`; the error messages; the Fortran
   tests (equivalence, behaviour, meta-kernels).
3. Interfaces: the ported jsonspice tests under PepperyPy, and a JSON kernel
   in the C test.
4. Documentation:
   - the README: a "JSON kernels" section with the format table and the meta
     example;
   - `pepper_vs_spicelib.md`: JSON kernels and atomic loads as PEPPER
     additions;
   - fdoc on every new procedure.
5. Optional: a converter, `tools/tk2json.py` (text kernel → JSON, keeping
   the comment block as leading `//` lines), and its reverse for SPICELIB
   users.

## Decisions

**D1. Parser.** Proposed: a hand-written pure parser (`pepper_json`), with no
dependency. Alternative: [json-fortran](https://github.com/jacobwilliams/json-fortran)
as an fpm dependency, which would also need to be added to the meson build
for PepperyPy.

**D2. Detection.** By the `.json` extension (case-insensitive), as jsonspice
does. It is simple and predictable, and it costs nothing for `.tpc` loads.
Detection by content (the first significant character is `{`) was
considered and dropped.

**D3. Kernel type.** Proposed: KEEPER and `kdata` report `TEXT`/`META`, and
GETFAT is unchanged, so that programs written for SPICELIB see a text kernel.
Alternative: a new type `JSON`, which `kdata` would report and
`ktotal('JSON')` would count. That breaks programs that count TEXT kernels.

**D4. Syntax accepted.** Proposed: JSON plus `//` line comments and trailing
commas. `/* */` block comments and the rest of JSON5 (unquoted keys, single
quotes, hex numbers, `Infinity`) are not supported.

**D5. Atomic loads.** Proposed: a JSON kernel with an error loads nothing.
Alternative: SPICELIB's text-kernel behaviour, where the variables before
the error stay loaded.

**D6. A bad `@` time.** jsonspice keeps the string as a character value when
TPARSE fails. Proposed: SPICE(BADTIMESPEC), as for a text kernel, because
silently storing a string hides typos in dates. A literal string starting
with `@` is then not expressible. If that is needed, `"@@..."` could mean a
literal `@`.

**D7. Nested objects.** Proposed: an error, as jsonspice does, for now. A
possible extension is to flatten them with `_`, so that
`{"BODY399": {"RADII": [...], "PM": [...]}}` gives `BODY399_RADII` and
`BODY399_PM`. Such a file would not be readable by jsonspice.
