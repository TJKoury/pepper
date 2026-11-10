# Bugs and inconsistencies found in SPICELIB (N0067)

Found while moving SPICELIB into modules (explicit interfaces, intents, per-context
state). Each entry says what is wrong in N0067, how it shows, and what PEPPER does.
Unless noted, the behavior of a correct program is unchanged, and the tspice tests
pass as with SPICELIB.

Kinds:
- **code**: the code does something wrong or relies on undefined behavior;
- **interface**: an argument is passed against its declaration (works with implicit
  interfaces and sequence association);
- **docs**: the header (Brief_I/O table, argument names) disagrees with the code.

## Code

| Routine | Problem | PEPPER |
|---|---|---|
| GETFVN | `INSTID` is declared DOUBLE PRECISION, but passed to integer arguments (ZZBODS2C, GETFOV). It works only because a little-endian machine keeps the integer in the double's first bytes. | Declared INTEGER. |
| ZZNRDDP (ZZDPPER) | `PE0`, `PINC0`, `PL0`, `PGH0`, `PH0` are never assigned (since the 2011 removal of DOPERT) and are subtracted, relying on saved storage being zero. `PREEP` is compared before it is first set. | All saved state is set to zero (blank, false) when it is created (`tools/zero_state.py`), as static storage was in practice. |
| ZZDSKSEL | A LOGICAL FUNCTION whose ENTRY points (e.g. ZZDSKSBD) are invoked with CALL (in ZZDSKSPH). Invalid Fortran that F77 compilers tolerated. | The value is referenced in an IF (`if ( zzdsksbd(...) ) continue`). |
| ZZGFCOU (ZZGFCOCD, ZZGFCOSD) | The error message on an unreachable path uses `CRDSYS`, an argument of another entry point (ZZGFCOIN), not of these. | Uses the saved `SVCSYS`. |
| ZZGFOCU (ZZGFOCIN) | The back body's shape/method string is parsed (ZZPRSMET) with the front body's ID `IDFRNT`, not `IDBACK`. Affects only the parsing and checking of surface names in the back body's DSK method string. | Unchanged (faithful). |
| ZZGFRPWK | `LS` (length of the report's begin message) is computed only by ZZGFTSWK; ZZGFWKAD replaces the begin message without updating it, so ZZGFWKIN's messages can be cut or padded. | Unchanged (faithful). |
| ZZGFREL, ZZGFRELX | `CSTEP` is saved but never assigned (it keeps its DATA value .FALSE.). Harmless. | Unchanged. |
| GFEVNT | `DREF` is set only when a DREF parameter is supplied; otherwise it keeps the value from an earlier call. | Unchanged. |
| NAMFRM (FRAMEX) | Kernel frame names are added to the name cache as `FRAME_<NAME>` (after PREFIX) but looked up as `<NAME>`, so the cache never hits and every lookup reads the pool. Results are correct. | Fixed: cached under the name (`limits.md`, phase 4). |
| UDF | Assigns `X = X + 0` and `VALUE = VALUE + 0` to its inputs (to use them). | Removed. |
| GFSTEP | Assigns `TIME = TIME` to its input. | Removed. |
| ZZCKCV05 | Passes the DP word `BUFFER(1)` to ERRINT. | Passes the integer subtype. |
| SGBWFS | Passes the array `PKTSIZ` to ERRINT. | Passes `PKTSIZ(1)`. |
| STMP03 | Truncates its series at 10 terms with Intel Fortran on Windows (`#if`), 11 elsewhere. | 11 everywhere. |
| POOL | Message text "an entry which performs performs no run-time function". | Unchanged (faithful). |
| ET2LST | Signals `SPICE(CANTGETROTATIONTYPE)`, 26 characters; short messages hold 25, so it is stored as `SPICE(CANTGETROTATIONTYPE` (without the closing parenthesis). | Fixed: messages have no length limit (`limits.md`, phase 1). |
| ZZDDHOPN (ZZDDHMAN) | Copies the file name into a `CHARACTER*(FILEN)` (255) local without a length check: a longer path is silently truncated, and the wrong file is opened or inquired. FURNSH checks the length, but DAFOPR, DASOPR, etc. called directly do not. | Fixed: file names have no length limit (`limits.md`, phase 2). |
| RDKER, RDTEXT | RDKER keeps the name of the text kernel being read in `CHARACTER*(FILSIZ)` (255) and reads the following lines through RDTEXT with that name; RDTEXT recognizes the file by comparing it with its own 255-character copy. A longer path works only because both copies are cut the same way; two paths that agree in their first 255 characters would be confused. | Fixed: both names have no length limit. |
| FRAMEX | The cache of kernel-defined frames (`MAXKFR` = 5209 entries) is reset only when the pool changes, and entries are added with ZZHSCADD/ZZHSIADD without checking for room (ZZHSCAVL), so looking up the 5210th distinct kernel frame between pool changes signals `SPICE(HASHISFULL)` instead of evicting. | Fixed: the caches grow (`limits.md`, phase 4). |
| ZZRVBF (LMPOOL) | Copies the characters of a string value into the pool's 80-character slot without the bounds check ZZRVAR has: a string value longer than 80 characters in an LMPOOL buffer is written past the end of its slot, into the next value. | Fixed: string values have no length limit (`limits.md`, phase 3). |
| ZZBODDEF (BODDEF) | The documented limit is `MAXP` = 150 new names, but the check is against `MAXE` − `NPERM` = 161; `MAXP` is not used in code. | Unchanged. |

## EK

Found while documenting the EK state. "Checked" entries were confirmed against the
N0067 source; the others are from reading the code and are likely but not tested.
PEPPER keeps SPICELIB's behavior in all of them.

| Routine | Problem | Status |
|---|---|---|
| EKQMGR (EKLEF) | When a segment of an already loaded table is found, the file is stored in `TBFILS(1, TBCURR)` (overwriting the first file) instead of `TBFILS(TBFLSZ+1, TBCURR)`, and the count is incremented; the handle is also added again for each segment of that table in the file. | Checked |
| EKQMGR (EKUEF) | `FND` is still .TRUE. from finding the file and is not reset for each table, so the per-table search of `TBFILS` never runs: every table is treated as containing the file, and `TBFLSZ` is decremented for all of them (it can become negative). Segments are still removed correctly (that test uses `STHAN`), and `TBFILS` is read only here, which mostly hides the EKLEF bug. | Checked |
| EKQMGR (EKLEF, abort cleanup) | The cleanup loop tests `.NOT. FND` without EKLEF setting `FND` (the value is left by the previous entry point); same missing per-table reset as EKUEF. | Reported |
| EKQMGR (EKNELT, EKGC, EKGD, EKGI) | The SELECT index is checked against `NSEL`, but the "valid range 1:#" message is filled with `NTAB`. | Reported |
| ZZEKJTST (ZZEKJNXT) | `LELTS` and `RELTS` are never assigned (ZZEKJPRP saves only the key constraint's elements); they are passed to ZZEKVMCH, so the other join constraints are compared with element index 0 (zero storage). | Checked |
| ZZEKJTST (ZZEKJNXT) | The test `SVCP1(CNSTR) <= SVNT1` that places the two halves of a row vector uses the width of the join row set holding the key's left table, which is NT2 when that table is in the second set. | Reported |
| ZZEKVADR (ZZEKVSET, ZZEKVCAL) | `MAXRWV = BEGIDX(NJRS) + J` is one more than the last row vector index, so ZZEKVCAL accepts one index past the end; some messages give ranges that differ from the checks, and one passes an extra ERRINT. | Likely (checked the code) |
| ZZEKAD06 | The "Column index = #; valid range is 1:#" error passes `NREC` instead of `NCOLS`; the comment "Make sure the record exists" has no check. | Reported |
| STCC01 | Every early return with an error message skips EKCLS, so the catalog file stays open; NCOLS and the column names come from the last segment only. | Reported |
| ZZEKTRLK | After a failed lookup the page buffer `PAGE` is overwritten but the cached `OLD*` values and `LEAF` are not, so a following lookup in the same tree could use the wrong page (an error was signaled first). | Reported |
| ZZEKPAGE | The fill buffers `CFILL`, `DFILL`, `IFILL` are set only by ZZEKPGIN but used by ZZEKPGAN and ZZEKPGAL for any file (zero storage; `CFILL` would not be blanks). | Reported (harmless in PEPPER: state starts blank/zero) |

Note: the per-context query results (EKSRCH) point into the instance's tables of
loaded EK files; a load or unload after a query invalidates them, in SPICELIB too. In
PEPPER this is covered by the rule that kernels are not loaded while other contexts
query.

## Interface (passed against the declaration)

| Routine | Problem | PEPPER |
|---|---|---|
| ZZEKUE04, ZZEKUE05, ZZEKUE06 | `IVALS`, `DVALS`, `CVALS` are declared as scalars but are arrays: they are passed on to ZZEKAD04/05/06, which use `NVALS` of them. | Declared as arrays `( * )`. |
| Callers of TTRANS (STR2ET, SGP4 routines) | Pass 8-element time vectors; TTRANS declares 10. | 10 elements. |
| ZZSCUP01 | The pool counter `POLCTR` is declared scalar; it is a 2-element counter. | Declared as the counter array. |
| DNEARP | Passes an array (`DALT`) for NEARPT's scalar altitude. | Passes `DALT(1)`. |
| ZZDAFGFR | Passes scalars to ZZXLATEI's array argument. | One-element arrays. |
| Many (EK, DAS, DSK, GF, ...) | Scalars passed to array arguments and arrays to scalar arguments (sequence association), e.g. a DAS address range of one word read into a scalar. | One-element arrays, first elements (`tools/fix_scalar_args.py`, `tools/fix_rank_args.py`). |

## Arguments changed although documented as inputs

These routines modify arguments their Brief_I/O table calls inputs (the code is
what SPICELIB does; PEPPER declares them `intent(inout)`):

- ZZCK4D2I: `DPCOEF` (overwritten while unpacking).
- SPKE18, SPKE19: `RECORD` (packets transposed in place).
- CKW04A: `PKTSIZ`, `PKTDAT` (packed in place).
- ZZGFSPIN: `SHAPE` (normalized in place).
- ZZDSIN (SGP4): `EMSQ`.
- ZZDPSEC, ZZDPPER (SDP4): the perturbed elements (`XLL`, `OMGASM`, `XNODES`, `EM`, `XINC`).
- EK: the segment descriptor `SEGDSC` in the column writers and updaters (ZZEKAD01-06, ZZEKDE01-06, ZZEKUE01-06), `COLDSC` in ZZEKCIX1, `CDSCRS` in ZZEKBS01, `RCPTRS` in EKFFLD; `SEGNO` in EKACEI, EKRCEC, EKRCED, EKRCEI, EKUCEC, EKUCED, EKUCEI (reassigned from the descriptor); `RECNO` in EKDELR.
- ZZGFREL, ZZGFRELX: `WORK` (workspace).
- DAFRA: the order vector.
- Cells documented as outputs are inputs too (their size); declared `intent(inout)`.

## Documentation

| Routine | Problem |
|---|---|
| SCTE01 | Brief_I/O says ET is an input; it is the output. |
| ZZDDHNFC | `NATBFF` is an output. |
| ZZASCII | TERMIN marked `0` (zero) instead of `O`; LINE (a work buffer) marked I. |
| ZZDPPER | XLL marked `0` (zero) instead of `O`. |
| ZZDPSEC | OMGDOT missing from the table. |
| ZZDYNOAC | MAXN missing from the table. |
| ZZDSKBSR (ZZDSKSNS) | `DLSDSC` for `DLADSC`. |
| ZZSEGBOX | The table and Detailed_Output name the arguments CENTER and RADIUS; they are BOXCTR and MAXR. |
| ZZDSKBUX | DC and IC missing from the table. |
| ZZGFRPWK (ZZGFWKAD) | The table lists TOTAL (an argument of ZZGFWKIN) instead of TCHECK. |
| ZZEKQORD, ZZEKQSEL | TABIDX listed twice; the second is COLIDX. |
| ZZEKRP2N | `RECTPR` for `RECPTR`. |
| ZZEKRBCK | `RECPTR` for `RECNO`. |
| ZZEKVMCH | `RHAN` for `RHANS`. |
| ZZEKAD05, ZZEKAD06 | The table describes an older argument list (SEGNO, RECNO, COLUMN for SEGDSC, COLDSC, RECPTR). |
| ZZEKJTST (ZZEKJPRP) | ELTS1 and ELTS2 missing from the table. |
| ZZEKCCHK, ZZEKTRES, ZZEKNRES | QUERY missing from the table. |
| ZZDSCM, ZZDSIN, ZZDSPC, ZZINIL (SGP4) | No Brief_I/O table; some arguments documented in neither Detailed_Input nor Detailed_Output. |

The fixed headers keep NAIF's text otherwise.
