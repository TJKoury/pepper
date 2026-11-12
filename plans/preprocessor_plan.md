# Removing the preprocessor

SPICELIB's platform-specific source files were merged into single `.F90`
files whose code is selected by macros (`SPICE_OS_*`, `SPICE_COMPILER_*`,
set by the `platform` feature in `fpm.toml`). This plan removes the
directives: standard Fortran replaces what is still needed, and what is not
needed goes. Without directives, every source file is `.f90`, and the
`-D` flags, the `/fpp` flag of Intel Fortran on Windows and FORD's
preprocessing settings are no longer needed.

## Inventory (before)

### Library (`src/`): 19 directives in 5 files

| Where | What the directives select | Needed? | Change |
|---|---|---|---|
| `chbase` (`pepper_cells.F90`) | the size of the character set: 256 on Windows/Intel, 128 elsewhere | No. It is used only by ENCHAR/DECHAR, which store the size and cardinality of character cells in memory (never in a file); any value up to 256 works with both compilers. PRTENC/PRTDEC, which write to EK files, use 128 everywhere. | One value, 128 (what gfortran builds use now). CHBASE stays, for compatibility. |
| `writln` (`pepper_das.F90`) | Windows/Intel writes to standard output list-directed, to avoid Fortran carriage control taking the first character | No. Carriage control is a leftover of old DEC/Intel defaults; current Intel Fortran applies it only when asked (`/ccdefault`, `/vms`), and `'(A)'` behaves the same everywhere. | `write (unit, '(A)')` for every unit. |
| `prompt` (`pepper_geometry.F90`) | Windows/Intel writes a leading blank (carriage control); both branches use the non-standard `$` edit descriptor | No. | `write (output_unit, '(A)', advance='no')` and `flush`. |
| `zzgfdsps` (`pepper_gf.F90`) | the GF progress report moves the cursor with ANSI escape sequences (ESC[A, ESC[B), except on Windows/Intel | No (a terminal matter, not a compiler one; the classic Windows console shows the escapes as text). | No escape sequences: the report writes plain lines. |
| `zzplatfm` (`pepper_files.F90`) | the platform names: SYSTEM, O/S, COMPILER, TEXT_FORMAT | The values are descriptive (FILE_FORMAT, which decides how binary files are read, is LTL-IEEE everywhere and has no directive). TEXT_FORMAT is the line terminator of text files written by the Fortran run-time library, used by ZZASCII and NAIF's F_ZZASC tests. | Found at run time with standard Fortran: COMPILER from `compiler_version()` (ISO_FORTRAN_ENV); the operating system from the environment variable `OS` (`Windows_NT` on Windows) and from files that exist only on one system (`/usr/bin/cygwin1.dll` under Cygwin, `/System/Library/CoreServices/SystemVersion.plist` on macOS, `/proc/version` on Linux); TEXT_FORMAT CR-LF on Windows (not Cygwin), LF elsewhere. |
| `pckeul` (`pepper_pck_api.f90`) | not a directive: `filen = 128` | No: never used (the Windows/Intel `filen = 128` / `255` directive of `todo.md` was removed with the SPICELIB copies, commit 6a52a5e). | Deleted. |

`pepper_daf.F90` and `pepper_keeper.F90` have no directives left (their last
ones went with the platform cleanups): renamed only.

### tspice: all `SPICE_COMPILER_INTEL`

| File | Intel vs others | Change |
|---|---|---|
| `support/zzgetenv.F90` | GETENVQQ vs GETENV | `get_environment_variable` |
| `support/exesys.F90` | SYSTEMQQ vs SYSTEM | `execute_command_line` |
| `support/getcml.F90` | NARGS/GETARG with status vs IARGC/GETARG | `command_argument_count`, `get_command_argument` |
| `support/edtcmd.F90` | the editor command line (NUL termination for GFORTRAN) | `execute_command_line` (through EXESYS) |
| `support/edtcom.F90` | the default editor, `edit` vs `emacs` (a system matter, not a compiler one) | `emacs`, or the editor named by the environment variable `EDITOR` |
| `support/getfnm.F90`, `getfnm_1.F90`, `expfnm_1.F90` | file name expansion only for non-Intel | Deleted: nothing calls them (EXPFNM_2, which is used, has no directives) |
| `f_tangpt2.F90` | looser tolerances for Intel | A run-time test of `compiler_version()` chooses the tolerances |
| `f_dnearp.F90` | one test case run only for Intel | Run for every compiler if it passes with gfortran; otherwise a run-time test of `compiler_version()` |
| `f_zzasc2.F90` | no directives left | renamed only |

## After

- No preprocessor directives in `src/` or `tspice/`; every source file
  `.f90`.
- `fpm.toml` (both packages): the `platform` feature keeps only the compiler
  options that are not about the platform (gfortran's `-frecursive`; tspice's
  `-fallow-argument-mismatch`). No `-D` flags, no `/fpp`.
- FORD/fdoc: no preprocessing settings.
- Renames on a case-insensitive file system go through a temporary name
  (`git mv x.F90 x.tmp && git mv x.tmp x.f90`), so that git records them.

## Status

Done. Results:

- No directives in `src/` or `tspice/`; the 15 `.F90` files are `.f90`, and
  NAIF's unused GETFNM, GETFNM_1 and EXPFNM_1 are deleted.
- `zzplatfm` on this Mac (gfortran 16): SYSTEM `MAC`, O/S `MAC OS-X`,
  COMPILER `GCC version 16.2.0`, TEXT_FORMAT `LF`. (The SYSTEM value was
  `INTEL MAC` before, wrong on Apple silicon.)
- F_DNEARP's case that N0067 ran only with Intel Fortran passes with
  gfortran: it runs for every compiler.
- To check on ifx/Windows (no such build here): the `writln`, `prompt` and
  `zzplatfm` changes, and tspice's system calls through
  EXECUTE_COMMAND_LINE, GET_ENVIRONMENT_VARIABLE and GET_COMMAND_ARGUMENT.
