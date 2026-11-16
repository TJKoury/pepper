!> Spacecraft clocks of type 1: the SCLK kernel data, conversion between ticks, encoded clock, strings and ephemeris time.
!>
!> The procedures keep SPICELIB's names and documentation; the context is
!> their first argument (interfaces and type-bound procedures in
!> [[pepper_instance_module]]). Moved here from src/spicelib by
!> tools/modularize_ctx.py; the external procedures with the original
!> interface are in tspice/src/legacy/pepper_legacy_sclk.f90.
!>
!>## SCLU01 (SCLK look up, type 1)
!>
!> PEPPER: SPICELIB's umbrella routine SCLU01 is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_sclu01. Its documentation follows.
!>
!>### Abstract
!>
!>     Look up type 1 SCLK kernel data.
!>
!>### Detailed Input
!>
!>     See entry points SCLI01, SCLD01.
!>
!>### Detailed Output
!>
!>     See entry points SCLI01, SCLD01.
!>
!>### Parameters
!>
!>     See the INCLUDE file sclk.inc for descriptions and values
!>     of the global parameters used by this routine and
!>     its entry points.
!>
!>### Exceptions
!>
!>     1)  If SCLU01 is called directly, the error SPICE(BOGUSENTRY) is
!>         signaled.
!>
!>     See entry points SCLI01, SCLD01 for descriptions of exceptions
!>     specific to those routines.
!>
!>### Files
!>
!>     None.
!>
!>### Particulars
!>
!>     This routine is a utility whose purpose is to localize error
!>     checking for type 1 SCLK kernel pool lookups in a single place.
!>
!>     SLCU01 exists solely as an umbrella routine in which the
!>     variables for its entry points are declared.  SCLU01 should never
!>     be called directly.
!>
!>### Examples
!>
!>     See entry points SCLI01, SCLD01.
!>
!>### Restrictions
!>
!>     1)  SCLU01 handles lookups of type 1 SCLK data only.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>     W.L. Taber         (JPL)
!>     E.D. Wright        (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR)
!>
!>        New checks on item sizes have been added: sizes are now
!>        compared against upper bounds as well as lower bounds.
!>        Previously only lower bounds were used.
!>
!>        Bug fix: corrected index error in SCLD01 modulus range check.
!>        Corrected comments about count checks in SCLI01. Made
!>        cosmetic changes to code and comments in both SCLD01 and
!>        SCLI01, and in this routine. Deleted unused parameter MXNCFF.
!>
!>        Edited the umbrella routine and all its entry points headers
!>        to comply with NAIF standard.
!>
!>-    SPICELIB Version 2.3.0, 05-FEB-2008 (NJB)
!>
!>        Values of parameters
!>
!>           MXCOEF, MXPART, MXNFLD, NDELIM, MXTSYS
!>
!>        are now provided by the INCLUDE file sclk.inc.
!>
!>-    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW)
!>
!>        Entry points SCLI01 and SCLD01 were update to use kernel pool
!>        fetch routines GIPOOL and GDPOOL respectively. Formerly these
!>        entry points called the deprecated routine RTPOOL.
!>
!>        All headers have been updated to remove warnings about memory
!>        corruption that could occur due to use of RTPOOL.
!>
!>        Header references to LDPOOL were replaced with references to
!>        FURNSH.
!>
!>-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
!>
!>        Entry points SCLI01 and SCLD01 were updated to fix a bug:
!>        if a kernel pool lookup fails, the number of elements returned
!>        N is now set to zero.
!>
!>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
!>
!>        Entry point SCLI01 was updated to handle a time
!>        system specification for the `parallel' time system
!>        in the SCLK kernel. Comment section for permuted index
!>        source lines was added following the header.
!>
!>-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB)
!>
!>### Revisions
!>
!>-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
!>
!>        Entry points SCLI01 and SCLD01 were updated to fix a bug:
!>        if a kernel pool lookup fails, the number of elements returned
!>        N is now set to zero. Formerly, these routines returned
!>        whatever value was returned by RTPOOL.  RTPOOL, however,
!>        does not set N to zero when the data item requested from it
!>        is not found.
!>
!>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
!>
!>        Entry point SCLI01 was updated to handle a time
!>        system specification for the `parallel' time system
!>        in the SCLK kernel. The update consists of these
!>        changes:
!>
!>           -- The parameter MXTSYS is now defined.
!>
!>           -- The local saved variable NAMLST has been expanded
!>              to include the name SCLK01_TIME_SYSTEM
!>
!>           -- The local saved variable LB has been expanded to
!>              include the lower bound for the number of returned
!>              values when SCLK01_TIME_SYSTEM_nn is looked up in
!>              the kernel pool.
!>
!>           -- SCLI01 checks the value returned by RTPOOL when
!>              SCLK01_TIME_SYSTEM_nn is looked up to verify that
!>              it is within the range [1, MXTSYS].
!>
!>        Also, a comment section for permuted index source lines was
!>        added following the header.
!>
!>### Implementation notes
!>
!>     MXCOFA is the maximum size of the coefficient array.
!>
!>     COFIDX is the index of the coefficient name in NAMLST.
!>
!>     NFLIDX is the index of the SCLK field count in NAMLST.
!>
!>     OFFIDX is the index of the SCLK offsets in NAMLST.
!>
!>     MODIDX is the index of the SCLK moduli in NAMLST.
!>
!>     DELIDX is the index of the delimiter code name in NAMLST.  If
!>     the declaration of NAMLST or assignment of values to NAMLST
!>     changes, this parameter value may have to change.
!>
!>     SYSIDX is the index of the time system in NAMLST.
!>
!>     Names of type 1 SCLK items and lower bounds on the number of
!>     associated values.
!>
!>## SC01 (Spacecraft clock, type 1)
!>
!> PEPPER: SPICELIB's umbrella routine SC01 is not a procedure any more: its entry points are the module procedures below, and its saved variables are in pepper_state_sc01. Its documentation follows.
!>
!>### Abstract
!>
!>     Perform time conversions between different representations of
!>     type 1 spacecraft clock.
!>
!>### Detailed Input
!>
!>     See the entry points SCTK01, SCFM01, SCET01, SCTE01, SCEC01.
!>
!>### Detailed Output
!>
!>     See the entry points SCTK01, SCFM01, SCET01, SCTE01, SCEC01.
!>
!>### Parameters
!>
!>     MXCOEF   is the maximum number of coefficient sets in the
!>              array COEFFS that defines the mapping between
!>              encoded type 1 SCLK and a parallel time system,
!>              such as TDB or TDT. This array has dimension
!>              3 x MXCOEF. The value of MXCOEF may be increased
!>              as required.
!>
!>     MXPART   is the maximum number of partitions for any type 1
!>              spacecraft clock. Type 1 SCLK kernels contain
!>              start and stop times for each partition. The value
!>              of MXPART may be increased as required.
!>
!>     MXNFLD   is an upper bound on the number of components in
!>              the clock string.
!>
!>     DPLEN    is an upper bound on the width of the individual
!>              components of the clock string.
!>
!>     DELIMS   are the characters that are accepted delimiters of
!>              the clock components in the input SCLK string.
!>
!>### Exceptions
!>
!>     1)  If SC01 is called directly, the error SPICE(BOGUSENTRY)
!>         is signaled.
!>
!>     2)  See the entry points SCTK01, SCFM01, SCET01, SCTE01 for a
!>         description of the exceptions specific to those routines.
!>
!>### Files
!>
!>     An SCLK kernel appropriate to the spacecraft clock identified
!>     by SC must be loaded at the time any entry point of this
!>     routine is called.
!>
!>     If the SCLK kernel used with this routine does not map SCLK
!>     directly to barycentric dynamical time, a leapseconds kernel
!>     must be loaded at the time any entry point of this routine is
!>     called.
!>
!>     Normally kernels are loaded by user applications once at the
!>     start of execution. It is not necessary to load them repeatedly.
!>
!>### Particulars
!>
!>     SC01 serves as an umbrella routine under which the shared
!>     variables of its entry points are declared.  SC01 should
!>     never be called directly.
!>
!>     The entry points of SC01 are
!>
!>        SCTK01 ( SCLK to ticks,          type 1 )
!>        SCFM01 ( Format,                 type 1 )
!>        SCET01 ( ET to ticks,            type 1 )
!>        SCEC01 ( ET to continuous ticks, type 1 )
!>        SCTE01 ( Ticks to ET,            type 1 )
!>        SCTY01 ( Return type of SCLK )
!>        SCPR01 ( Return partition start and stop times, type 1 )
!>
!>### Examples
!>
!>     See the entry points.
!>
!>### Restrictions
!>
!>     1)  Efficiency of the conversion routines in this package
!>         depends on the assumption that the kernel pool is updated
!>         relatively infrequently. Any modification of kernel pool data,
!>         including setting watches, will cause entry points of this
!>         package to re-buffer saved SCLK data.
!>
!>### Literature References
!>
!>     None.
!>
!>### Author and Institution
!>
!>     N.J. Bachman       (JPL)
!>     J. Diaz del Rio    (ODC Space)
!>     J.M. Lynch         (JPL)
!>     B.V. Semenov       (JPL)
!>     E.D. Wright        (JPL)
!>
!>### Version
!>
!>-    SPICELIB Version 4.0.0, 01-DEC-2021 (NJB) (JDR)
!>
!>        Added entry points SCPR01 (return partition data) and
!>        SCTY01 (indicate whether clock is type 1).
!>
!>        Updated all entry points to support buffering of data for
!>        multiple clocks.
!>
!>        Added error checks for invalid time system codes.
!>
!>        Edited headers of umbrella routine and all entry points to
!>        comply with NAIF standard.
!>
!>-    SPICELIB Version 3.4.0, 09-SEP-2013 (BVS)
!>
!>        Updated to keep track of the POOL counter and call ZZCVPOOL.
!>
!>-    SPICELIB Version 3.3.0, 05-MAR-2009 (NJB)
!>
!>        Bug fix: the entry points of this routine now keep track of
!>        whether their kernel pool look-ups succeeded. If not, a kernel
!>        pool lookup is attempted on the next call to any entry point
!>        of this routine.
!>
!>-    SPICELIB Version 3.2.0, 17-FEB-2008 (NJB)
!>
!>        Bug fix: changed maximum value arguments to 1 in
!>        calls to SCLI01 to fetch NFIELD and DELCDE values.
!>
!>        Bug fix: spaces between fields are now inserted
!>        correctly when the output field delimiter is blank.
!>
!>-    SPICELIB Version 3.1.1, 22-AUG-2006 (EDW)
!>
!>        Replaced references to LDPOOL with references
!>        to FURNSH.
!>
!>-    SPICELIB Version 3.1.0, 24-JAN-2003 (BVS)
!>
!>        Increased MXCOEF to 10000.
!>
!>-    SPICELIB Version 3.0.0, 09-MAR-1999 (NJB)
!>
!>        Added new entry point SCEC01. Removed some extraneous
!>        C's from column 1; these had been added by a wayward
!>        preprocessor.
!>
!>        Removed local variable RNDCLK; entry point SCTE01 no longer
!>        creates a rounded version of its input argument.
!>
!>        Updated/fixed various comments here and in entry SCET01.
!>
!>-    SPICELIB Version 2.1.0, 07-JUL-1996 (NJB)
!>
!>        Removed declaration, DATA and SAVE statements for unused
!>        variables NFDMSG and OLDID.
!>
!>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
!>
!>        All entry points were updated to handle SCLK kernels that
!>        map between SCLK and a variety of time systems; formerly
!>        only TDB was supported. All entry points have had corrections
!>        and additions made to their headers. Comment section for
!>        permuted index source lines was added following the header.
!>
!>-    SPICELIB Version 1.0.0, 03-SEP-1990 (NJB) (JML)
!>
!>### Revisions
!>
!>-    SPICELIB Version 3.3.0, 05-MAR-2009 (NJB)
!>
!>        Bug fix: the entry points of this routine now keep track of
!>        whether their kernel pool look-ups succeeded. If not, a kernel
!>        pool lookup is attempted on the next call to any entry point
!>        of this routine.
!>
!>        All entry points of this routine look up the same kernel
!>        variables, and use the saved variable UPDATE to indicate that
!>        a kernel pool look-up is needed. A look-up failure occurring
!>        in any entry point will now prevent all entry points from
!>        relying on stored kernel data.
!>
!>-    SPICELIB Version 3.2.0, 17-FEB-2008 (NJB)
!>
!>        Bug fix: changed maximum value arguments to 1 in
!>        calls to SCLI01 to fetch NFIELD and DELCDE values.
!>
!>        Bug fix: spaces between fields are now inserted
!>        correctly when the output field delimiter is blank.
!>
!>        Unused parameter INITID was removed.
!>
!>-    SPICELIB Version 3.1.0, 24-JAN-2003 (BVS)
!>
!>        Increased MXCOEF to 10000.
!>
!>-    SPICELIB Version 3.0.0, 09-MAR-1999 (NJB)
!>
!>        Added new entry point SCEC01. Removed some extraneous
!>        C's from column 1; these had been added by a wayward
!>        preprocessor.
!>
!>        Removed local variable RNDCLK; entry point SCTE01 no longer
!>        creates a rounded version of its input argument.
!>
!>        Updated/fixed various comments here and in entry SCET01.
!>
!>-    SPICELIB Version 2.1.0, 07-JUL-1996 (NJB)
!>
!>        Removed declaration, DATA and SAVE statements for unused
!>        variables NFDMSG and OLDID.
!>
!>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
!>
!>        Entry points SCET01 and SCTE01 were updated to handle a time
!>        system specification for the `parallel' time system
!>        in the SCLK kernel. Formerly, the only time system that
!>        an SCLK kernel could map SCLK to was TDB. Now TDT is
!>        supported, and the mechanism for allowing other parallel
!>        time systems is in place.
!>
!>        To support a new parallel time system, it is necessary
!>        to
!>
!>           -- Update SCTE01 so that after the routine converts an input
!>              tick value to a value in the parallel system, the
!>              resulting value is converted to TDB. See the current
!>              treatment of TDT in that routine for an example of how
!>              this is done.
!>
!>           -- Update SCET01 so that the input TDB value can be
!>              converted to a value in the new parallel system when
!>              required. This converted value is then used as an input
!>              to the interpolation algorithm performed in SCET01. See
!>              the current treatment of TDT in that routine for an
!>              example of how this is done.
!>
!>           -- Update the parameter MXTSYS in SCLU01 to indicate the
!>              new number of supported parallel time systems.
!>
!>           -- Update the SCLK Required Reading to document the
!>              description of the currently supported parallel time
!>              systems.
!>
!>        See the named entry points for further details.
!>
!>        The kernel pool routines SWPOOL and CVPOOL are now used
!>        to determine when it is necessary to look up kernel pool
!>        constants. The variable UPDATE is now used to indicate
!>        when it is necessary to look up the kernel variables used by
!>        this suite of routines. All of the entry points SCFM01,
!>        SCTK01, SCET01, and SCTE01 were affected by this update.
!>
!>        All of the entry points have had their headers updated to
!>        discuss the fact that a leapseconds kernel will now need to be
!>        loaded in order to use SCLK kernels that map between SCLK and
!>        a parallel time system other than TDB.
!>
!>        In this routine, a comment section for permuted index
!>        source lines was added following the header.
!>
!>### Implementation notes
!>
!>     SCLK pool:
!>
!>     Declarations of statement functions
!>
!>     There are at least a half dozen distinct items to save. We're
!>     safer just saving everything.
!>
!>     Maintenance programming note: the coefficient buffer
!>     should be saved in any event to prevent memory problems
!>     on some platforms.
!>
!>     NEW: Statement functions
!>
!>     PEPPER: the statement functions below are written out where they
!>     were used (they read the SCLK data buffer, which is per-context
!>     state now; tools/inline_statement_functions.py). Their descriptions
!>     are kept.
!>
!>     Function COEFFS returns the Ith element of the Jth coefficient
!>     record for the record set starting at index COFBAS+1 in the
!>     double precision SCLK data buffer.
!>
!>     Function PREND returns the Ith partition end time for the
!>     partition end time set starting at index ENDBAS+1 in the double
!>     precision SCLK data buffer.
!>
!>     Function PRSTRT returns the Ith partition start time for the
!>     partition start time set starting at index STRBAS+1 in the double
!>     precision SCLK data buffer.
!>
!>     Function MODULI returns the Ith field modulus for the modulus
!>     array starting at index MODBAS+1 in the double
!>     precision SCLK data buffer.
!>
!>     Function OFFSET returns the Ith field offset for the offset
!>     array starting at index OFFBAS+1 in the double
!>     precision SCLK data buffer.
!>
!>     For later implementation:
!>
!>     Function CMPWID returns the width in characters required to
!>     represent as a string the maximum count of the Ith field.
!>
!>      CMPWID( I ) = INTBUF( OFFCMP + I )
submodule (pepper_instance_module) pepper_sclk

   implicit none


   ! Parameters shared by this file's procedures (PEPPER: SPICELIB
   ! declared them in each routine that used them).
   integer(ip), parameter :: msglen = 320  !! length of an error message template
   character(msglen), parameter :: bvlmsg = 'Invalid value of #. Value was #.'  !! message: a kernel variable's value is invalid
   integer(ip), parameter :: cofidx = 1  !! index of the coefficients variable among the SCLK items
   integer(ip), parameter :: kvnmln = 32  !! length of a kernel variable name
   integer(ip), parameter :: namlen = 80  !! length of an SCLK kernel variable name
   character(msglen), parameter :: nfdmsg = '# not found. Did you load the SCLK kernel?'
      !! message: a kernel variable was not found
   integer(ip), parameter :: nflidx = 4  !! index of the number of fields variable among the SCLK items
   integer(ip), parameter :: nkitem = 9  !! number of SCLK kernel items
   character(msglen), parameter :: nummsg = '# values found for #: valid range is #:#.'
      !! message: a kernel variable has a wrong number of values

contains

      !>## Detailed Input
      !>
      !>     NAME,
      !>     SC       are, respectively, a name and a NAIF integer code
      !>              of a spacecraft that together define the name of a
      !>              requested kernel data item. NAME is the full name
      !>              as it appears in the SCLK kernel, except that it
      !>              lacks the final underscore and spacecraft integer
      !>              code (actually, the negative of the spacecraft
      !>              code).  This routine combines NAME and SC to
      !>              make up the appropriate kernel variable name.
      !>
      !>              For example, to look up data associated with the
      !>              name
      !>
      !>                 SCLK01_N_FIELDS_77
      !>
      !>              you would supply NAME as
      !>
      !>                 SCLK01_N_FIELDS
      !>
      !>              and SC as -77.
      !>
      !>     MAXNV    is the maximum number of values to return.  MAXNV
      !>              is used to prevent SCLI01 from writing past the end
      !>              of the supplied array IVAL.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of values actually returned.
      !>
      !>     IVAL     is an array containing the requested integer
      !>              kernel data item.
      !>
      !>## Parameters
      !>
      !>     MXNFLD   is an upper bound on the number of fields in a
      !>              SCLK string.
      !>
      !>     NDELIM   is the number of delimiter codes.
      !>
      !>     MXTSYS   is the maximum number of supported parallel time
      !>              systems that SCLK values may be mapped to or from.
      !>
      !>## Exceptions
      !>
      !>     1)  If item specified by NAME and SC is not found in the kernel
      !>         pool, and if the presence of the item is required, the error
      !>         SPICE(KERNELVARNOTFOUND) is signaled. The output arguments
      !>         are not modified.
      !>
      !>         If the specified item is not required, the output argument N
      !>         will take the value 0, and the output argument IVAL is not
      !>         modified.
      !>
      !>     2)  If the item specified by NAME and SC is found but does not
      !>         have numeric type, the error SPICE(BADKERNELVARTYPE) is
      !>         signaled.
      !>
      !>     3)  This routine can check certain data for validity. If any of
      !>         these items have invalid values, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled. The output arguments are
      !>         not modified. The values in question are:
      !>
      !>            -  The number of fields of a SCLK string
      !>            -  The number of delimiter codes
      !>            -  The output delimiter code
      !>            -  The time system code
      !>
      !>     4)  If the dimension of the requested item exceeds MAXNV, the
      !>         error SPICE(ARRAYTOOSMALL) is signaled.
      !>
      !>     5)  If the dimension of the requested item is outside of the
      !>         limits for that item, the error SPICE(INVALIDSIZE) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The purpose of this routine is to localize error checking for
      !>     lookups of type 1 SCLK kernel pool data. This routine handles
      !>     lookups of integer data.
      !>
      !>## Examples
      !>
      !>     1)  To get the number of SCLK fields for the Galileo spacecraft
      !>         clock, you can use the code fragment below:
      !>
      !>            C
      !>            C     Load the SCLK kernel in question. We use a
      !>            C     made-up name for the kernel file; you would use
      !>            C     the actual name of your kernel file instead if you
      !>            C     were to carry out this procedure.
      !>            C
      !>                  CALL FURNSH ( 'SAMPLE_GLL_SCLK.KER' )
      !>
      !>                  SC   = -77
      !>                  NAME = 'SCLK01_N_FIELDS'
      !>
      !>                  CALL SCLI01 ( NAME, SC, MXNFLD, N, NFIELD )
      !>
      !>         After this subroutine call, NFIELD has the value 4.
      !>
      !>## Restrictions
      !>
      !>     1)  SCLI01 assumes that a SCLK kernel appropriate to the
      !>         spacecraft identified by SC has been loaded.
      !>
      !>     2)  SCLI01 handles lookups of type 1 SCLK data only.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR)
      !>
      !>        New checks on item sizes have been added: sizes are now
      !>        compared against upper bounds as well as lower bounds.
      !>        Previously only lower bounds were used.
      !>
      !>        Corrected comments about count checks. Made cosmetic changes
      !>        to code and comments.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW)
      !>
      !>        Routine was updated to use GIPOOL instead of RTPOOL. Header
      !>        has been updated to remove warnings about memory corruption and
      !>        to document exception handling for output buffer overflow
      !>        errors.
      !>
      !>        Header references to LDPOOL were replaced with references to
      !>        FURNSH.
      !>
      !>-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
      !>
      !>        This entry point was updated to fix a bug: if a kernel pool
      !>        lookup fails, the number of elements returned N is now set to
      !>        zero.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
      !>
      !>        SCLI01 was updated to handle a time system specification for
      !>        the `parallel' time system in the SCLK kernel. Some
      !>        corrections and other minor enhancements were made to the
      !>        header. Comment section for permuted index source lines was
      !>        added following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
      !>
      !>        This entry point was updated to fix a bug: if a kernel pool
      !>        lookup fails, the number of elements returned N is now set to
      !>        zero. Formerly, this routine returned whatever value was
      !>        returned by RTPOOL.  RTPOOL, however, does not set N to zero
      !>        when the data item requested from it is not found.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
      !>
      !>        Entry point SCLI01 was updated to handle a time
      !>        system specification for the `parallel' time system
      !>        in the SCLK kernel. The update consists of these
      !>        changes:
      !>
      !>           -- The parameter MXTSYS is now defined.
      !>
      !>           -- The local saved variable NAMLST has been expanded
      !>              to include the name SCLK01_TIME_SYSTEM
      !>
      !>           -- The local saved variable LB has been expanded to
      !>              include the lower bound for the number of returned
      !>              values when SCLK01_TIME_SYSTEM_nn is looked up in
      !>              the kernel pool.
      !>
      !>           -- SCLI01 checks the value returned by RTPOOL when
      !>              SCLK01_TIME_SYSTEM_nn is looked up to verify that
      !>              it is within the range [1, MXTSYS].
      !>
      !>        Also, a comment section for permuted index source lines was
      !>        added following the header.
      !>
      !>        The $Exceptions header section was updated accordingly.
      !>
      !>        Some corrections and other minor enhancements were made to the
      !>        header.
      !>
      !>     Standard SPICE error handling.
      module subroutine scli01 ( ctx, name, sc, maxnv, n, ival )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: repmc, suffix, repmc_inplace, repmi_inplace
      use pepper_inc_sclk
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of kernel data item, NAIF spacecraft ID code.
      integer(ip), intent(in) :: sc  !! Name of kernel data item, NAIF spacecraft ID code.
      integer(ip), intent(in) :: maxnv  !! Maximum number of integer values to return.
      integer(ip), intent(out) :: n  !! Number of values actually returned.
      integer(ip), intent(out) :: ival ( * )  !! Returned integer values.
      integer(ip), parameter :: mxcofa = 3 * mxcoef
      integer(ip), parameter :: delidx = 7
      integer(ip), parameter :: sysidx = 9
      character(msglen), parameter :: bvlmsg = 'Invalid value found for #:  #.'
      character(msglen) :: errmsg
      character(kvnmln) :: kvname
      character(namlen), parameter :: namlst ( nkitem ) = [ character(namlen) :: &
         'SCLK01_COEFFICIENTS', 'SCLK_PARTITION_START', 'SCLK_PARTITION_END', 'SCLK01_N_FIELDS',&
         'SCLK01_OFFSETS', 'SCLK01_MODULI', 'SCLK01_OUTPUT_DELIM', 'SCLK01_KERNEL_ID',&
         'SCLK01_TIME_SYSTEM' ]
      character(namlen) :: tmpnam
      character(1) :: type
      integer(ip) :: i
      integer(ip), parameter :: lb ( nkitem ) = [ &
         3, 1, 1, 1,&
         1, 1, 1, 1,&
         0 ]
      integer(ip) :: nf
      integer(ip) :: nfield
      integer(ip), parameter :: ub ( nkitem ) = [ &
         mxcofa, mxpart, mxpart, 1,&
         mxnfld, mxnfld, 1, 1,&
         1 ]
      logical :: found
      if ( ctx%failed() ) return

!
!     Form the name of the kernel pool data item, and do the lookup.
!     Note that eventually we should use a kernel pool lookup entry
!     that allows us to specify the maximum number of entries that
!     can be returned.
!
      tmpnam = name

      call suffix ( '_#',    0,       tmpnam )
      call repmi_inplace ( tmpnam, '#', -sc )

!
!     Make sure we have enough room for the item in our output
!     array. Look up the dimension of the item.
!
      call dtpool ( ctx, tmpnam, found, n, type )

      if ( n > maxnv ) then

         call ctx%setmsg ( 'Item # for SCLK # has size # but ' &
         //            'output array has size #.'         )
         call ctx%errch  ( '#',  tmpnam                       )
         call ctx%errint ( '#',  sc                           )
         call ctx%errint ( '#',  n                            )
         call ctx%errint ( '#',  maxnv                        )
         call ctx%sigerr ( 'SPICE(ARRAYTOOSMALL)'             )
         return

      end if

!
!     Check the data type of the variable.
!
      if (  found  .and.  ( type /= 'N' )  ) then

         call ctx%setmsg ( 'Kernel variable # for spacecraft ' &
         //            'clock # does not have numeric type.' )
         call ctx%errch  ( '#', tmpnam                           )
         call ctx%errint ( '#', sc                               )
         call ctx%sigerr ( 'SPICE(BADKERNELVARTYPE)'             )
         return

      end if

      call gipool ( ctx, tmpnam, 1, maxnv, n, ival, found )

      if ( ctx%failed() ) return

!
!     Make sure we found what we were looking for, if the item
!     is required.
!
      if ( .not. found ) then
!
!        Currently, the only item that is NOT required is the time
!        system specification.  In any case, no values will be returned.
!
         n = 0

         if (  name == namlst(sysidx)  ) then

            return

         else

            call ctx%setmsg (  nfdmsg                    )
            call ctx%errch  ( '#', tmpnam                )
            call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)' )
            return

         end if

      end if

!
!     Now we must check that the number of returned values is in the
!     appropriate range. We test for the following conditions:
!
!        - The SCLK field count kernel variable is a scalar.
!
!        - The delimiter code kernel variable is a scalar.
!
!        - The time system code kernel variable, if present, is a
!          scalar. At this point in the code, the variable is known
!          to be present.
!
!     Note that the kernel pool doesn't allow a kernel variable to
!     not have associated values. Checks against the lower bound 1 are
!     done just to simplify the code.
!
!     See if the input name is in the list of items we know about.
!     If it is, perform the bound checks that apply.
!
      i = findloc(namlst, name, dim=1)

      if ( i /= 0 ) then

         if (  (  n < lb(i) ) .or. ( n > ub(i) )  ) then

            call repmi  ( ctx,  nummsg, '#', n,      errmsg )
            call repmc_inplace ( errmsg, '#', tmpnam )
            call repmi_inplace ( errmsg, '#', lb(i) )
            call repmi_inplace ( errmsg, '#', ub(i) )
            call ctx%setmsg (  errmsg                      )
            call ctx%sigerr ( 'SPICE(SIZEOUTOFRANGE)'      )
            return

         end if

      end if

!
!     Check values of kernel variables:
!
!        - The output delimiter code is at least 1 and is not
!          greater than the number of delimiters.
!
!        - The field count is at least 1 and is not greater than
!          MXNFLD.
!
!        - The time system code is at least 1 and is not greater
!          than MXTSYS.
!
      if ( name == namlst(delidx) ) then

         if (  ( ival(1) < 1 ) .or. ( ival(1) > ndelim )  ) then

            call repmc  (  bvlmsg, '#', tmpnam,  errmsg )
            call repmi_inplace ( errmsg, '#', ival(1) )
            call ctx%setmsg (  errmsg                       )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'      )
            return

         end if

      end if

      if ( name == namlst(nflidx) ) then

         if (  ( ival(1) < 1 ) .or. ( ival(1) > mxnfld )  ) then

            call repmc  (  bvlmsg, '#', tmpnam,  errmsg )
            call repmi_inplace ( errmsg, '#', ival(1) )
            call ctx%setmsg (  errmsg                       )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'      )
            return

         end if

      end if

      if ( name == namlst(sysidx) ) then

         if (  ( ival(1) < 1 ) .or. ( ival(1) > mxtsys )  ) then

            call repmc  (  bvlmsg, '#', tmpnam,  errmsg )
            call repmi_inplace ( errmsg, '#', ival(1) )
            call ctx%setmsg (  errmsg                       )
            call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'      )
            return

         end if

      end if

      return
      end subroutine scli01

      !>## Detailed Input
      !>
      !>     NAME,
      !>     SC       are, respectively, a name and a NAIF integer code
      !>              of a spacecraft that together define the name of a
      !>              requested kernel data item. NAME is the full name
      !>              as it appears in the SCLK kernel, except that it
      !>              lacks the final underscore and spacecraft integer
      !>              code (actually, the negative of the spacecraft
      !>              code).  This routine combines NAME and SC to
      !>              make up the appropriate kernel variable name.
      !>
      !>              For example, to look up data associated with the
      !>              name
      !>
      !>                 SCLK01_COEFFICIENTS_77
      !>
      !>              you would supply NAME as
      !>
      !>                 SCLK01_COEFFICIENTS
      !>
      !>              and SC as -77.
      !>
      !>     MAXNV    is the maximum number of values to return.  MAXNV
      !>              is used to prevent SCLD01 from writing past the end
      !>              of the supplied array DVAL.
      !>
      !>## Detailed Output
      !>
      !>     N        is the number of values actually returned.
      !>
      !>     DVAL     is an array containing the requested double
      !>              precision kernel data item.
      !>
      !>## Parameters
      !>
      !>     MXCOEF   is the maximum number of coefficient sets in the
      !>              array COEFFS that defines the mapping between
      !>              encoded type 1 SCLK and a parallel time system.
      !>              This array has dimension 3 x MXCOEF. The value of
      !>              MXCOEF may be increased as required.
      !>
      !>## Exceptions
      !>
      !>     1)  If item specified by NAME and SC is not found in the kernel
      !>         pool, the error SPICE(KERNELVARNOTFOUND) is signaled. The
      !>         output arguments are not modified.
      !>
      !>     2)  If the item specified by NAME and SC is found but does not
      !>         have numeric type, the error SPICE(BADKERNELVARTYPE) is
      !>         signaled.
      !>
      !>     3)  This routine can check certain data for validity. If any of
      !>         these items have invalid values, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled. The output arguments are
      !>         not modified. The values in question are:
      !>
      !>            - The number of coefficients.
      !>            - The number of partition start values.
      !>            - The number of partition end values.
      !>            - The number of moduli.
      !>            - The values of the moduli (lower bounds)
      !>            - The number of offsets.
      !>            - The number of kernel identifiers.
      !>
      !>     4)  If the partition times or SCLK coefficients themselves
      !>         are invalid, this routine does nothing about it. It is
      !>         simply not possible to detect all of the possible errors
      !>         that these data may be subject to.
      !>
      !>     5)  If the dimension of the requested item exceeds MAXNV, the
      !>         error SPICE(ARRAYTOOSMALL) is signaled.
      !>
      !>     6)  If the dimension of the requested item is outside of the
      !>         limits for that item, the error SPICE(INVALIDSIZE) is
      !>         signaled.
      !>
      !>     7)  If the dimension of the coefficient kernel variable is
      !>         not a multiple of 3, the error SPICE(INVALIDSIZE) is
      !>         signaled.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The purpose of this routine is to localize error checking for
      !>     lookups of type 1 SCLK kernel pool data. This routine handles
      !>     lookups of double precision data.
      !>
      !>## Examples
      !>
      !>     1)  Check a NAIF SCLK kernel for accuracy by converting the
      !>         encoded SCLK coefficients to strings with partition numbers
      !>         and converting the parallel times to UTC strings. Print out
      !>         the results in tabular form. In this example, the spacecraft
      !>         is Mars Observer, which has NAIF ID code -94. We could
      !>         make the program work for Galileo by using the NAIF ID code
      !>         -77 instead of -94.
      !>
      !>            C
      !>            C     Load the SCLK kernel in question, and also load
      !>            C     a leapseconds kernel. We use made-up names for the
      !>            C     kernel file; you would use the actual names of your
      !>            C     kernel files instead if you were to carry out this
      !>            C     procedure.
      !>            C
      !>                  CALL FURNSH ( 'SAMPLE_MO_SCLK.KER' )
      !>                  CALL FURNSH ( 'LEAPSECONDS.KER'    )
      !>
      !>                  CONAME =  SCLK01_COEFFICIENTS
      !>                  SC     =  -94
      !>
      !>            C
      !>            C     Grab the coefficients.
      !>            C
      !>                  CALL SCLD01 ( CONAME, SC, 3*MXCOEF, NCOEFF, COEFFS )
      !>
      !>            C
      !>            C     The SCLK coefficients are in the first row of the
      !>            C     coefficients array; the parallel times are in the
      !>            C     second. Since the parallel time system used for MO
      !>            C     is terrestrial dynamical time (TDT), we will convert
      !>            C     the parallel time values to ET (TDB) first and then
      !>            C     convert the resulting times to UTC.
      !>            C
      !>            C     In a more robust algorithm, we'd look up the parallel
      !>            C     time system code used in the SCLK kernel rather than
      !>            C     assume that it is a particular system. We omit this
      !>            C     check for simplicity.
      !>            C
      !>            C     We decode the SCLK coefficients using SCDECD. Write
      !>            C     out the results to a file we'll call COMPARE.DAT.
      !>            C
      !>                  OUTFIL = 'COMPARE.DAT'
      !>
      !>                  CALL WRLINE ( OUTFIL, '    SCLK               UTC' )
      !>                  CALL WRLINE ( OUTFIL, ' '                          )
      !>
      !>                  DO I = 1, NCOEFF / 3
      !>
      !>                     CALL SCDECD ( -94,  COEFF(1,I),  CLKSTR )
      !>            C
      !>            C        Convert the parallel time coefficients, which are
      !>            C        given in TDT, to ET. UNITIM returns this value.
      !>            C
      !>                     CALL ET2UTC ( UNITIM ( COEFF(2,I), 'TDT', 'TDB' ),
      !>                 .                 'D',
      !>                 .                  3,
      !>                 .                  UTC    )
      !>
      !>                     LINE = ' SCLK        UTC '
      !>
      !>                     CALL REPMC  ( LINE, 'SCLK', CLKSTR, LINE )
      !>                     CALL REPMC  ( LINE, 'UTC',  UTC,    LINE )
      !>
      !>                     CALL WRLINE ( OUTFIL, LINE )
      !>
      !>                  END DO
      !>
      !>## Restrictions
      !>
      !>     1)  SCLD01 assumes that a SCLK kernel appropriate to the
      !>         spacecraft identified by SC has been loaded.
      !>
      !>     2)  SCLD01 handles lookups of type 1 SCLK data only.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     W.L. Taber         (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR)
      !>
      !>        New checks on item sizes have been added: sizes are now
      !>        compared against upper bounds as well as lower bounds.
      !>        Previously only lower bounds were used. A check has been
      !>        added to verify that the coefficient count is a multiple
      !>        of 3.
      !>
      !>        Bug fix: corrected index error in modulus range check. Made
      !>        cosmetic changes to code and comments.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW)
      !>
      !>        Routine was updated to use GDPOOL instead of RTPOOL. Header
      !>        has been updated to remove warnings about memory corruption and
      !>        to document exception handling for output buffer overflow
      !>        errors.
      !>
      !>        Header references to LDPOOL were replaced with references to
      !>        FURNSH.
      !>
      !>-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
      !>
      !>        This entry point was updated to fix a bug: if a kernel pool
      !>        lookup fails, the number of elements returned N is now set to
      !>        zero.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
      !>
      !>        One constant was changed in the code for clarity; no functional
      !>        change results from this. Some corrections and other minor
      !>        enhancements were made to the header. Comment section for
      !>        permuted index source lines was added following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
      !>
      !>        This entry point was updated to fix a bug: if a kernel pool
      !>        lookup fails, the number of elements returned N is now set to
      !>        zero. Formerly, this routine returned whatever value was
      !>        returned by RTPOOL.  RTPOOL, however, does not set N to zero
      !>        when the data item requested from it is not found.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
      !>
      !>        The constant 1 was changed to 1.D0 in the test for the
      !>        validity of the moduli for a spacecraft clock. The change
      !>        was made simply for clarity.
      !>
      !>        Some corrections and other minor enhancements were made to the
      !>        header. Comment section for permuted index source lines was
      !>        added following the header.
      !>
      !>     Standard SPICE error handling.
      module subroutine scld01 ( ctx, name, sc, maxnv, n, dval )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: repmc, suffix, repmc_inplace, repmd_inplace, repmi_inplace
      use pepper_inc_sclk
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      character(*), intent(in) :: name  !! Name of kernel data item, NAIF spacecraft ID code.
      integer(ip), intent(in) :: sc  !! Name of kernel data item, NAIF spacecraft ID code.
      integer(ip), intent(in) :: maxnv  !! Maximum number of d.p. values to return.
      integer(ip), intent(out) :: n  !! Number of values actually returned.
      real(wp), intent(out) :: dval ( * )  !! Requested kernel data item.
      integer(ip) :: nfield_1(1)  !! one-element copy of nfield (array argument)
      integer(ip), parameter :: offidx = 5
      integer(ip), parameter :: modidx = 6
      character(msglen), parameter :: bvlmsg = 'Invalid value found for #:  #.'
      character(msglen) :: errmsg
      character(kvnmln) :: kvname
      character(namlen), parameter :: namlst ( nkitem ) = [ character(namlen) :: &
         'SCLK01_COEFFICIENTS', 'SCLK_PARTITION_START', 'SCLK_PARTITION_END', 'SCLK01_N_FIELDS',&
         'SCLK01_OFFSETS', 'SCLK01_MODULI', 'SCLK01_OUTPUT_DELIM', 'SCLK01_KERNEL_ID',&
         'SCLK01_TIME_SYSTEM' ]
      character(namlen) :: tmpnam
      character(1) :: type
      integer(ip) :: i
      integer(ip), parameter :: lb ( nkitem ) = [ &
         3, 1, 1, 1,&
         1, 1, 1, 1,&
         0 ]
      integer(ip) :: nf
      integer(ip) :: nfield
!
!     PEPPER: no maximum number of coefficients or partitions (SPICELIB:
!     3*MXCOEF and MXPART); the field counts keep MXNFLD (the format).
!
      integer(ip), parameter :: ub ( nkitem ) = [ &
         huge(1_ip), huge(1_ip), huge(1_ip), 1,&
         mxnfld, mxnfld, 1, 1,&
         1 ]
      logical :: found
      if ( ctx%failed() ) return

!
!     Form the name of the kernel pool datum, and do the lookup.
!
      tmpnam = name

      call suffix ( '_#',    0,        tmpnam )
      call repmi_inplace ( tmpnam, '#', -sc )

!
!     Make sure we have enough room for the item in our output
!     array. Look up the dimension of the item.
!
      call dtpool ( ctx, tmpnam,  found,  n,  type )

      if ( n > maxnv ) then

         call ctx%setmsg ( 'Item # has size # but output array ' &
         //            'has size #.'                         )
         call ctx%errch  ( '#',  tmpnam                          )
         call ctx%errint ( '#',  n                               )
         call ctx%errint ( '#',  maxnv                           )
         call ctx%sigerr ( 'SPICE(ARRAYTOOSMALL)'                )
         return

      end if

!
!     Check the data type of the variable.
!
      if (  found  .and.  ( type /= 'N' )  ) then

         call ctx%setmsg ( 'Kernel variable # for spacecraft ' &
         //            'clock # does not have numeric type.' )
         call ctx%errch  ( '#', tmpnam                           )
         call ctx%errint ( '#', sc                               )
         call ctx%sigerr ( 'SPICE(BADKERNELVARTYPE)'             )
         return

      end if

      call gdpool ( ctx, tmpnam, 1, maxnv, n, dval, found )

      if ( ctx%failed() ) then
!
!        This code should be unreachable but is provided for safety.
!
         return
      end if
!
!     Make sure we found what we were looking for.
!
      if ( .not. found ) then
!
!        No values are returned in this case.
!
         n = 0

         call ctx%setmsg (  nfdmsg                    )
         call ctx%errch  ( '#', tmpnam                )
         call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)' )
         return

      end if

!
!     Now we must check that the number of returned values is in the
!     appropriate range. We test for the following conditions:
!
!        - The number of coefficients is at least 3 and no more than
!          3 * MXCOEF
!
!        - The number of partition start values is at least 1 and no
!          more than MXPART
!
!        - The number of partition end values is at least 1 and no more
!          than MXPART
!
!        - The number of moduli is at least 1, no more than MXNFLD,
!          and matches the field count.
!
!        - The number of offsets is at least 1, no more than MXNFLD,
!          and matches the field count.
!
!     Note that the kernel pool doesn't allow a kernel variable to
!     not have associated values. Checks against the lower bound 1 are
!     done just to simplify the code.
!
!     See if the input name is in the list of items we know about.
!     If it is, perform the bounds checks that apply.
!
      i = findloc(namlst, name, dim=1)

      if ( i /= 0 ) then

         if (  (  n < lb(i) ) .or. ( n > ub(i) )  ) then

            call repmi  ( ctx,  nummsg, '#', n,      errmsg )
            call repmc_inplace ( errmsg, '#', tmpnam )
            call repmi_inplace ( errmsg, '#', lb(i) )
            call repmi_inplace ( errmsg, '#', ub(i) )
            call ctx%setmsg (  errmsg                      )
            call ctx%sigerr ( 'SPICE(SIZEOUTOFRANGE)'      )
            return

         end if

      end if

      if ( name == namlst(cofidx) ) then

         if (  ( (n/3)*3 )  /=  n  ) then

            call ctx%setmsg ( 'Coefficient count for # must ' &
            //            'be multiple of 3 but was #.'  )
            call ctx%errch  ( '#', tmpnam                    )
            call ctx%errint ( '#', n                         )
            call ctx%sigerr ( 'SPICE(INVALIDSIZE)'           )
            return

         end if

      end if

!
!     Check the values of the moduli themselves. Also check the
!     moduli count against the field count.
!
      if ( name == namlst(modidx) ) then

         do i = 1, n

            if ( dval(i) < 1.0_wp ) then
               call repmc  (  bvlmsg, '#', tmpnam,      errmsg )
               call repmd_inplace ( errmsg, '#', dval(i), 14 )
               call ctx%setmsg (  errmsg                           )
               call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'          )
               return
            end if

         end do

      end if

      if (       ( name == namlst(modidx) ) &
           .or.  ( name == namlst(offidx) )  ) then
!
!        Get the field count for this clock.
!
         kvname = namlst( nflidx )(:kvnmln)

         call suffix ( '_#',    0,        kvname )
         call repmi_inplace ( kvname, '#', -sc )

         call gipool ( ctx, kvname, 1, 1, nf, nfield_1, found )
         nfield = nfield_1(1)

         if ( ctx%failed() ) return

         if ( .not. found ) then

            call ctx%setmsg ( 'Field count was not found for SCLK #.' )
            call ctx%errint ( '#', sc                    )
            call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)' )
            return

         end if

         if ( n /= nfield ) then

            if ( name == namlst(modidx) ) then

               call ctx%setmsg ( 'Modulus count # does not match ' &
               //            'field count # for SCLK #.'      )

            else

               call ctx%setmsg ( 'Offset count # does not match ' &
               //            'field count # for SCLK #.'      )

            end if

            call ctx%errint ( '#', n               )
            call ctx%errint ( '#', nfield          )
            call ctx%errint ( '#', sc              )
            call ctx%sigerr ( 'SPICE(INVALIDSIZE)' )
            return

         end if

      end if

      return
      end subroutine scld01

      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines. Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     Initialize the SCLK type 1 database.
      module subroutine zzscin01 ( ctx )
      use pepper_kinds, only: ip
      use pepper_inc_zzsc01
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context (PEPPER: its type 1 SCLK database is emptied)
!
!     PEPPER: the database is the context's sc01_state (growable arrays
!     and a map of the clocks); emptying it keeps the arrays allocated.
!
      if ( .not. allocated ( ctx%sc01_state ) ) then
         allocate ( ctx%sc01_state )
         call init_sc01_state ( ctx%sc01_state )
      end if
      call ctx%sc01_state%clkmap%clear ( )
      ctx%sc01_state%nclk   = 0
      ctx%sc01_state%dpfree = 1
      ctx%sc01_state%ifree  = 1
      ctx%sc01_state%prvsc  = 0
      return
      end subroutine zzscin01

      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines. Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     Add to the SCLK type 01 database data for a specified SCLK.
      !>
      !>## Detailed Input
      !>
      !>     SC          is the SCLK ID of a type 1 clock. Data for this clock
      !>                 are to be added to the type 1 database.
      !>
      !>     HDSCLK      is an array containing the head node indices of the
      !>                 SCLK ID collision lists stored in SCPOOL.
      !>
      !>     SCPOOL      is a singly linked list pool containing collision
      !>                 lists for sets of SCLK IDs that have the same hash
      !>                 values.
      !>
      !>     CLKLST      is an array of SCLK IDs, which is parallel to the
      !>                 data portion of SCPOOL. Each allocated node of
      !>                 SCPOOL is the index in CLKLST of the associated
      !>                 SCLK ID.
      !>
      !>     DPFREE      is index in the database's double precision buffer of
      !>                 the first free element.
      !>
      !>     DPBUFF      is the double precision buffer of the type 1
      !>                 database.
      !>
      !>     IFREE       is index in the database's integer buffer of the
      !>                 first free element.
      !>
      !>     INTBUF      is the integer buffer of the type 1 database.
      !>
      !>     SCBASE      is an array of base addresses in the array INTBUF of
      !>                 the integer data sets associated with the clocks
      !>                 in the type 1 database. The Ith element of SCBASE is
      !>                 the base address of the clock at index I in the
      !>                 array CLKLST.
      !>
      !>                 The "base" address of an integer data set immediately
      !>                 precedes the first index of that data set: the index
      !>                 in INTBUF of the first element of the integer data of
      !>                 CLKLST(I) is SCBASE(I)+1.
      !>
      !>## Detailed Output
      !>
      !>     HDSCLK      is the input list head array, updated as needed to
      !>                 reflect the addition of the new clock. Note that the
      !>                 array won't be updated if the clock is added to a
      !>                 pre-existing collision list.
      !>
      !>     SCPOOL      is the input singly linked list pool, updated to
      !>                 reflect the addition of the new clock.
      !>
      !>     CLKLST      is an array of SCLK IDs, updated to reflect the
      !>                 addition of the new clock.
      !>
      !>     DPFREE      is index in the database's double precision buffer of
      !>                 the first free element after addition of data for the
      !>                 new clock.
      !>
      !>     DPBUFF      is the double precision buffer of the type 1
      !>                 database, updated to reflect the addition of the new
      !>                 clock.
      !>
      !>     IFREE       is index in the database's integer buffer of the
      !>                 first free element after addition of data for the new
      !>                 clock.
      !>
      !>     INTBUF      is the integer buffer of the type 1 database, updated
      !>                 to reflect the addition of the new clock.
      !>
      !>     SCBASE      is an array of base addresses in the array INTBUF,
      !>                 updated to reflect the addition of the new clock.
      !>
      !>     SCLKAT      is the index in CLKLST at which the input clock ID
      !>                 SC is stored.
      !>
      !>## Parameters
      !>
      !>     See the include files zzsc01.inc and zzctr.inc.
      !>
      !>## Exceptions
      !>
      !>     1)  If any of the kernel variables containing SCLK coefficients,
      !>         partition start or stop times, moduli, or offsets are not
      !>         found, the error SPICE(KERNELVARNOTFOUND) will be signaled.
      !>
      !>     2)  If any integer kernel variables are not found, the error will
      !>         be signaled by a routine in the call tree of this routine.
      !>
      !>     3)  If any kernel variable used by this routine does not have
      !>         numeric type, the error will be signaled by a routine in the
      !>         call tree of this routine.
      !>
      !>     4)  If the counts of partition start or stop times do not match,
      !>         the error SPICE(NUMPARTSUNEQUAL) will be signaled.
      !>
      !>     5)  If the count of partitions exceeds the limit MXPART,
      !>         the error SPICE(TOOMANYPARTITIONS) will be signaled.
      !>
      !>     6)  If the count of coefficients exceeds the limit 3*MXCOEF,
      !>         the error SPICE(TOOMANYCOEFFS) will be signaled.
      !>
      !>     7)  If any kernel variable other than the coefficient set or
      !>         the partition bounds has size that exceeds the applicable
      !>         limit, the error SPICE(KERNELVARTOOLARGE) will be signaled.
      !>
      !>     8)  If an error occurs while this routine looks up data from the
      !>         kernel pool, the error will be signaled by a routine in the
      !>         call tree of this routine.
      !>
      !>     9)  If an error occurs while this routine adds data to a hash,
      !>         the error will be signaled by a routine in the call tree of
      !>         this routine.
      !>
      !>     10) If the database contains entries for the maximum number
      !>         of clocks and an addition is requested, the database will
      !>         be re-initialized, and an entry for the new clock will be
      !>         created. This case is not an error.
      !>
      !>## Files
      !>
      !>     Appropriate kernels must be loaded by the calling program before
      !>     this routine is called.
      !>
      !>     The following data are required:
      !>
      !>        - An SCLK kernel providing data for the SCLK designated by
      !>          the input ID code SC.
      !>
      !>     In all cases, kernel data are normally loaded once per program
      !>     run, NOT every time this routine is called.
      !>
      !>## Particulars
      !>
      !>     None.
      !>
      !>## Examples
      !>
      !>     None. See usage in ZZSCUP01.
      !>
      !>## Restrictions
      !>
      !>     This is a SPICE-private routine. It should not be called directly
      !>     by user application code.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman   (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB)
      !>
      !>## Revisions
      !>
      !>     None.
      !>
      !>     SPICELIB functions
      !>
      !>     Local parameters
      !>
      !>     Following are parameters for the indices within the
      !>     array NAMLST of the kernel variable names used by the
      !>     SC01 entry points.
      !>
      !>     NOTE: indices for the d.p. variables must be listed first.
      !>
      !>     Maximum number of elements in coefficient array:
      !>
      !>     Local variables
      !>
      !>     Saved values
      !>
      !>     Initial values
      module subroutine zzscad01 ( ctx, sc, sclkat )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: suffix, repmi_inplace
      use pepper_containers, only: grow
      use pepper_inc_errhnd
      use pepper_inc_zzsc01
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context (PEPPER: its type 1 SCLK database grows)
      integer(ip), intent(in) :: sc  !! SCLK ID code.
      integer(ip), intent(out) :: sclkat  !! Index of the clock in the database.
      integer(ip), parameter :: namlen = 60
      integer(ip), parameter :: prsidx = cofidx + 1
      integer(ip), parameter :: preidx = prsidx + 1
      integer(ip), parameter :: offidx = preidx + 1
      integer(ip), parameter :: modidx = offidx + 1
      integer(ip), parameter :: nflidx = modidx + 1
      integer(ip), parameter :: delidx = nflidx + 1
      integer(ip), parameter :: sysidx = delidx + 1
      integer(ip), parameter :: nkitem = sysidx
      integer(ip), parameter :: ndp = modidx
      character(1) :: kvtype ( nkitem )
      character(namlen) :: kvname ( nkitem )
      character(namlen), parameter :: namlst ( nkitem ) = [ character(namlen) :: &
         'SCLK01_COEFFICIENTS', 'SCLK_PARTITION_START', 'SCLK_PARTITION_END', 'SCLK01_OFFSETS',&
         'SCLK01_MODULI', 'SCLK01_N_FIELDS', 'SCLK01_OUTPUT_DELIM', 'SCLK01_TIME_SYSTEM' ]
      integer(ip) :: dpbase
      integer(ip) :: i
      integer(ip) :: ibase
      integer(ip), parameter :: ibix ( nkitem ) = [ &
         ixbcof, ixbstr, ixbend, ixboff,&
         ixbmod, 0, 0, 0 ]
      integer(ip) :: kvsize ( nkitem )
      integer(ip) :: n
      integer(ip) :: ncoeff
      integer(ip) :: ndpval
      integer(ip) :: nmod
      integer(ip) :: noff
      integer(ip) :: npart
      integer(ip) :: npartb
      integer(ip) :: nparte
      integer(ip) :: nsys
      logical :: found
!
!     PEPPER: works on the context's sc01_state, whose arrays grow: there
!     is no limit on the number of clocks (SPICELIB: MXNCLK, 100, then the
!     database was emptied) or on the coefficients and partitions of a
!     clock (SPICELIB: MXCOEF, 100000, SPICE(TOOMANYCOEFFS); MXPART, 9999,
!     SPICE(TOOMANYPARTITIONS)). The offsets and moduli still have at
!     most MXNFLD values (the SCLK format's limit on fields).
!
      sclkat = 0
      if ( ctx%failed() ) return
      kvname = namlst
      do i = 1, nkitem
         call suffix ( '_#',        0,         kvname(i) )
         call repmi_inplace ( kvname(i),  '#',  -sc )
      end do
      do i = 1, ndp
         call dtpool ( ctx, kvname(i), found, kvsize(i), kvtype(i) )
         if ( ctx%failed() ) then
            call zzscin01 ( ctx )
            return
         end if
         if ( .not. found ) then
            call zzscin01 ( ctx )
            call ctx%setmsg ( 'Kernel variable # for spacecraft clock ' &
            //            '# was not found. An SCLK kernel for ' &
            //            'this clock may not have been loaded.'   )
            call ctx%errch  ( '#', kvname(i)                           )
            call ctx%errint ( '#', sc                                  )
            call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)'               )
            return
         end if
         if (       ( i == offidx .or. i == modidx ) &
              .and. ( kvsize(i) > mxnfld ) ) then
            call zzscin01 ( ctx )
            call ctx%setmsg ( 'The number of values associated with ' &
            //            'the kernel variable # for clock # ' &
            //            'is #, which exceeds the limit #.'     )
            call ctx%errch  ( '#', kvname(i)                         )
            call ctx%errint ( '#', sc                                )
            call ctx%errint ( '#', kvsize(i)                         )
            call ctx%errint ( '#', mxnfld                            )
            call ctx%sigerr ( 'SPICE(KERNELVARTOOLARGE)'             )
            return
         end if
      end do
      ncoeff = kvsize(cofidx)
      npartb = kvsize(prsidx)
      nparte = kvsize(preidx)
      nmod   = kvsize(modidx)
      noff   = kvsize(offidx)
      ndpval = ncoeff + npartb + nparte + nmod + noff
      if ( npartb /= nparte  ) then
         call zzscin01 ( ctx )
         call ctx%setmsg ( 'The numbers of partition start times # and ' &
         //            'stop times # are unequal for spacecraft ' &
         //            'clock #.'                                  )
         call ctx%errint ( '#', npartb                                 )
         call ctx%errint ( '#', nparte                                 )
         call ctx%errint ( '#', sc                                     )
         call ctx%sigerr ( 'SPICE(NUMPARTSUNEQUAL)'                    )
         return
      else
         npart = npartb
      end if
!
!     Make room for the clock (the arrays grow; nothing is emptied).
!
      associate ( s => ctx%sc01_state )
      call grow ( s%scbase, s%nclk + 1 )
      call grow ( s%intbuf, s%ifree + nivals - 1 )
      call grow ( s%dpbuff, s%dpfree + ndpval - 1 )
      ibase = s%ifree - 1
      call scli01 ( ctx, namlst(nflidx), sc, 1, n,    s%intbuf(ibase+ixnfld:) )
      call scli01 ( ctx, namlst(delidx), sc, 1, n,    s%intbuf(ibase+ixdlim:) )
      call scli01 ( ctx, namlst(sysidx), sc, 1, nsys, s%intbuf(ibase+ixtsys:) )
      if ( ctx%failed() ) then
         call zzscin01 ( ctx )
         return
      end if
      if ( nsys == 0 ) then
         s%intbuf(ibase+ixtsys) = tdb
      end if
      s%intbuf( ibase+ixncof ) = ncoeff
      s%intbuf( ibase+ixnprt ) = npart
      dpbase = s%dpfree - 1
      do i = 1, ndp
         s%intbuf( ibase + ibix(i) ) = dpbase
         call scld01 ( ctx, namlst(i), sc, max ( 1_ip, kvsize(i) ), n, s%dpbuff(dpbase + 1:) )
         if ( ctx%failed() ) then
            call zzscin01 ( ctx )
            return
         end if
         dpbase = dpbase + n
      end do
      s%nclk           = s%nclk + 1
      sclkat           = s%nclk
      s%scbase(sclkat) = ibase
      call s%clkmap%set ( sc, sclkat )
      s%dpfree = s%dpfree + ndpval
      s%ifree  = s%ifree  + nivals
      end associate
      return
      end subroutine zzscad01

      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines. Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     If necessary, update the SCLK type 01 database and associated
      !>     SCLK parameters.
      module subroutine zzscup01 ( ctx, sc )
      use pepper_kinds, only: ip
      use pepper_inc_zzsc01
      use pepper_inc_zzctr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context (PEPPER: the clock's data are made current in its database)
      integer(ip), intent(in) :: sc  !! SCLK ID code.
      integer(ip) :: ibase
      integer(ip) :: sclkat
      logical :: found
      logical :: samclk
      logical :: update
!
!     PEPPER: works on the context's sc01_state (it may grow the
!     database: callers do not hold ASSOCIATE names of its arrays).
!
      if ( ctx%failed() ) return
      associate ( s => ctx%sc01_state )
      call zzpctrck ( ctx, s%polctr, update )
      if ( update ) then
         call zzscin01 ( ctx )
         samclk = .false.
      else
         samclk = ( sc /= 0 ) .and. ( sc == s%prvsc )
      end if
      if ( .not. samclk )  then
         call s%clkmap%get ( sc, sclkat, found )
         if ( .not. found ) then
            call zzscad01 ( ctx, sc, sclkat )
            if ( ctx%failed() ) then
               s%nfield = 0
               s%delcde = 0
               s%timsys = 0
               s%ncoeff = 0
               s%npart  = 0
               s%prvsc  = 0
               return
            end if
         end if
         ibase    = s%scbase( sclkat )
         s%nfield = s%intbuf( ibase+ixnfld )
         s%delcde = s%intbuf( ibase+ixdlim )
         s%timsys = s%intbuf( ibase+ixtsys )
         s%ncoeff = s%intbuf( ibase+ixncof )
         s%npart  = s%intbuf( ibase+ixnprt )
         s%cofbas = s%intbuf( ibase+ixbcof )
         s%strbas = s%intbuf( ibase+ixbstr )
         s%endbas = s%intbuf( ibase+ixbend )
         s%modbas = s%intbuf( ibase+ixbmod )
         s%offbas = s%intbuf( ibase+ixboff )
      end if
      end associate
      return
      end subroutine zzscup01

      !>## Abstract
      !>
      !>     SPICE Private routine intended solely for the support of SPICE
      !>     routines.  Users should not call this routine directly due
      !>     to the volatile nature of this routine.
      !>
      !>     Determine whether or not an SCLK kernel is available for mapping
      !>     ET to Ticks and back again for a particular C-kernel ID.
      !>
      !>## Detailed Input
      !>
      !>     CKID       is the C-kernel ID-code for some object for which
      !>                and SCLK is required.
      !>
      !>     SCLKID     is the ID-code to pass to SCE2C to convert ET times
      !>                to ticks.
      !>
      !>## Detailed Output
      !>
      !>     The function returns TRUE if an SCLK specification is present
      !>     in the kernel pool that is suitable for mapping ticks to ET and
      !>     back for the C-kernel object specified by CKID.  If such
      !>     information is not available, or is deemed to be corrupt or
      !>     incomplete, the function returns FALSE.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This is a utility for checking that sufficient SCLK information
      !>     is available for mapping between ET and SCLK for the object
      !>     specified by CKID
      !>
      !>## Examples
      !>
      !>     None.
      !>
      !>## Restrictions
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     W.L. Taber      (JPL)
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.1.0, 15-AUG-2000 (WLT)
      !>
      !>        Removed the check fo the SCLK Time system as it is not
      !>        formally required for an SCLK specification to be complete.
      !>
      !>-    SPICELIB Version 1.0.0, 17-FEB-2000 (WLT)
      module logical function zzsclk ( ctx, ckid, sclkid )
      use pepper_kinds, only: ip
      use pepper_strings, only: intstr
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: ckid  !! CK ID-code for the object of interest.
      integer(ip), intent(in) :: sclkid  !! Idcode to pass to the SCLOCK routines for CKID
      integer(ip), parameter :: wdsize = 32
      integer(ip), parameter :: maxids = 10
      character(wdsize) :: sclk
      character(wdsize) :: sclkvr ( 7 )
      character(wdsize) :: type
      character(wdsize) :: agent
      integer(ip) :: i
      integer(ip) :: n
      logical :: keepid
      logical :: update
      logical :: watch
      logical :: found
      if ( .not. allocated ( ctx%zzsclk_state ) ) allocate ( ctx%zzsclk_state )
      associate ( known => ctx%zzsclk_state%known, &
                  passed => ctx%zzsclk_state%passed, &
                  first => ctx%zzsclk_state%first, &
                  dtsize => ctx%zzsclk_state%dtsize )
      zzsclk = .false.
!
!     Standard SPICE error handling.
!
      if ( ctx%failed() ) return

      if ( first ) then
         first = .false.

         dtsize(1) = 1
         dtsize(2) = 1
         dtsize(3) = 1
         dtsize(4) = 1
         dtsize(5) = 3
         dtsize(6) = 1
         dtsize(7) = 1

         call ssizei ( ctx, maxids, known )
         call ssizei ( ctx, maxids, passed )
      end if
!
!     We've got a text kernel (or meta kernel). See if there is an
!     SCLK kernel loaded for the CKID provided in the calling inputs.
!     If not, we'll use the default -CKID/1000 for the SCLK ID.
!
      call intstr ( -sclkid, sclk )
      agent = 'ZZSCLK' // sclk

!
!     See if this is an ID-code we've encountered before.  If it
!     is we can make use of stored knowledge about this ID-code.
!
      if ( elemi( ctx, sclkid, known ) ) then

         watch  = .false.
         keepid = .true.
         call cvpool ( ctx, agent, update )

      else if ( cardi( ctx,known) < sizei( ctx,known) ) then
!
!        The SCLKID specified is not in the list of SCLKIDs for
!        this routine and there is room left in the pool of
!        SCLKIDs to keep track of one more.  Put this ID into
!        the list of known IDS
!
         call insrti( ctx, sclkid, known )
         update = .true.
         watch  = .true.
         keepid = .true.

      else

         update = .true.
         keepid = .false.
         watch  = .false.

      end if

      if ( .not. update ) then
!
!        Nothing has changed in the kernel pool w.r.t this agent.
!        The test for an SCLK will not have changed either.
!
         zzsclk = elemi( ctx, sclkid, passed )
         return

      end if
!
!     If we are still here, we need to look in the kernel pool
!     to see if we have an SCLK for this object.
!
!     Construct all of the expected SCLK variables are
!     available for this SCLK.
!
      sclkvr(1) = 'SCLK_DATA_TYPE_'       // sclk
      sclkvr(2) = 'SCLK01_N_FIELDS_'      // sclk
      sclkvr(3) = 'SCLK01_MODULI_'        // sclk
      sclkvr(4) = 'SCLK01_OFFSETS_'       // sclk
      sclkvr(5) = 'SCLK01_COEFFICIENTS_'  // sclk
      sclkvr(6) = 'SCLK_PARTITION_START_' // sclk
      sclkvr(7) = 'SCLK_PARTITION_END_'   // sclk
!
!     If we are supposed to watch for this agent, we add him to
!     the list of kernel pool agents.
!
      if ( watch ) then
         call swpool ( ctx, agent, 7, sclkvr )
         call cvpool ( ctx, agent,    update )
      end if
!
!     Check for all of the required variables and structure in
!     the kernel pool.
!
      do i = 1, 7

         call dtpool ( ctx, sclkvr(i), found, n, type )

         if (       .not. found &
              .or.  type /= 'N' &
              .or. (n/dtsize(i))*dtsize(i) /= n   )then
!
!           We don't have adequate SCLK data for the specified
!           object.  Remove this AGENT from the list of agents
!           that have passed the test.
!
            call removi ( ctx, sclkid, passed )
            return
         end if

      end do
!
!     Once we get to this point, we know we have SCLK data.  If
!     there is room to WATCH for this agent,
!
      if ( keepid ) then
         call insrti ( ctx, sclkid, passed )
      end if

!
!     As far as we can tell, everything looks ok.
!
      zzsclk = .true.
      return
      end associate
      end function zzsclk

      !>## Detailed Input
      !>
      !>     SC       is a NAIF spacecraft identification code. See the
      !>              $Examples section below, and also the NAIF_IDS
      !>              required reading file for a complete list of body ID
      !>              codes.
      !>
      !>     CLKSTR   on input is the character representation of a
      !>              spacecraft clock count (SCLK), without a partition
      !>              number.
      !>
      !>              Using Galileo as an example, a SCLK string without
      !>              a partition number has the form
      !>
      !>                             wwwwwwww:xx:y:z
      !>
      !>              where z is a mod-8 counter (values 0-7) which
      !>              increments approximately once every 8 1/3 ms., y is a
      !>              mod-10 counter (values 0-9) which increments once
      !>              every time z turns over, i.e., approximately once every
      !>              66 2/3 ms., xx is a mod-91 (values 0-90) counter
      !>              which increments once every time y turns over, i.e.,
      !>              once every 2/3 seconds. wwwwwwww is the Real-Time
      !>              Image Count (RIM), which increments once every time
      !>              xx turns over, i.e., once every 60 2/3 seconds. The
      !>              roll-over expression for the RIM is 16777215, which
      !>              corresponds to approximately 32 years.
      !>
      !>              wwwwwwww, xx, y, and z are referred to interchangeably
      !>              as the fields or components of the spacecraft count.
      !>              SCLK components may be separated by any of the
      !>              single character delimiters in the string DELIMS, with
      !>              any number of spaces separating the components and
      !>              the delimiters. The presence of the RIM component
      !>              is required. Successive components may be omitted, and
      !>              in such cases are assumed to represent zero values.
      !>
      !>              Values for the individual components may exceed the
      !>              maximum expected values. For instance, '0:0:0:9' is
      !>              an acceptable Galileo clock string, and indicates the
      !>              same time interval as '0:0:1:1'.
      !>
      !>              Consecutive delimiters containing no intervening digits
      !>              are treated as if they delimit zero components, except
      !>              in the case of blanks.  Consecutive blanks are treated
      !>              as a single blank.
      !>
      !>              Trailing zeros should always be included to match the
      !>              length of the counter.  For example, a Galileo clock
      !>              count of '25684.90' should not be represented as
      !>              '25684.9'.
      !>
      !>              Some spacecraft clock components have offset, or
      !>              starting, values different from zero. For example,
      !>              with an offset value of 1, a mod 20 counter would
      !>              cycle from 1 to 20 instead of from 0 to 19.
      !>
      !>              See the SCLK required reading for a detailed
      !>              description of the Galileo, Mars Observer, and Voyager
      !>              clock formats.
      !>
      !>              See the $Examples section in SCTK01, below.
      !>
      !>## Detailed Output
      !>
      !>     TICKS    is the number of "ticks" corresponding to the input
      !>              spacecraft clock string CLKSTR.  "Ticks" are the units
      !>              in which encoded SCLK strings are represented.
      !>
      !>              A typical Galileo SCLK string looks like
      !>
      !>                           'wwwwwwww xx y z',
      !>
      !>              as described above. Since z is the mod-8 (one tick)
      !>              counter, the number of ticks represented by y is 8*y.
      !>              And since y is the mod-10 counter, the number of ticks
      !>              represented by xx is 10*8*xx. The total number of
      !>              ticks represented by the above string is
      !>
      !>                            wwwwwwww( 7280 ) +
      !>                                  xx(   80 ) +
      !>                                   y(    8 ) +
      !>                                   z
      !>
      !>              Clock strings for other spacecraft are converted in
      !>              a similar manner.
      !>
      !>              See $Examples below.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  This routine assumes that that an SCLK kernel appropriate to
      !>         the spacecraft clock identified by the input argument SC has
      !>         been loaded. If an SCLK kernel has not been loaded, does not
      !>         contain all of the required data, or contains invalid data, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine. The output argument TICKS will not be modified.
      !>
      !>         The variables that must be set by the SCLK kernel are:
      !>
      !>            -  The number of fields in an (unabridged) SCLK string
      !>            -  The output delimiter code
      !>            -  The parallel time system code
      !>            -  The moduli of the fields of an SCLK string
      !>            -  The offsets for each clock field.
      !>            -  The SCLK coefficients array
      !>            -  The partition start times
      !>            -  The partition end times
      !>
      !>     2)  When using SCLK kernels that map SCLK to a time system other
      !>         than ET (also called barycentric dynamical time---`TDB'), it
      !>         is necessary to have a leapseconds kernel loaded at the time
      !>         this routine is called. If a leapseconds kernel is required
      !>         for conversion between SCLK and ET but is not loaded, an error
      !>         is signaled by a routine in the call tree of this routine. The
      !>         output argument TICKS will not be modified.
      !>
      !>         The time system that an SCLK kernel maps SCLK to is indicated
      !>         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
      !>         is the negative of the NAIF integer code for the spacecraft.
      !>         The time system used in a kernel is TDB if and only if the
      !>         variable is assigned the value 1.
      !>
      !>     3)  If any of the following kernel variables have invalid values,
      !>         the error will be diagnosed by routines called by this
      !>         routine:
      !>
      !>            -  The time system code
      !>            -  The number of SCLK coefficients
      !>            -  The number of partition start times
      !>            -  The number of partition end times
      !>            -  The number of fields of a SCLK string
      !>            -  The number of moduli for a SCLK string
      !>
      !>         If the number of values for any item read from the kernel
      !>         pool exceeds the maximum allowed value, it is may not be
      !>         possible to diagnose the error correctly, since overwriting
      !>         of memory may occur. This particular type of error is not
      !>         diagnosed by this routine.
      !>
      !>     4)  The input argument CLKSTR may be invalid for a variety of
      !>         reasons:
      !>
      !>            -- One of the extracted clock components cannot be parsed
      !>               as an integer
      !>
      !>            -- CLKSTR contains too many components
      !>
      !>            -- the value  of one of the components is less than the
      !>               offset value
      !>
      !>         If any of these conditions is detected, the error
      !>         SPICE(INVALIDSCLKSTRING) is signaled. The output argument
      !>         TICKS will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     This routine converts a character string representation of a
      !>     spacecraft clock count into the number of ticks represented
      !>     by the clock count. An important distinction between this type
      !>     of conversion and that carried out by SCENCD is that this routine
      !>     treats spacecraft clock times as representations of time
      !>     intervals, not absolute times.
      !>
      !>     This routine does not make use of any partition information.
      !>     See SCENCD for details on how to make use of partition numbers.
      !>
      !>## Examples
      !>
      !>     1)  Below are some examples illustrating various inputs and the
      !>         resulting outputs for the Galileo spacecraft.
      !>
      !>         CLKSTR                TICKS
      !>         ----------------      --------------------
      !>         '0:0:0:1'             1
      !>         '0:0:1'               8
      !>         '0:1'                 80
      !>         '1'                   7280
      !>         '1 0 0 0'             7280
      !>         '1,0,0,0'             7280
      !>         '1:90'                14480
      !>         '1:9'                 8000
      !>         '1:09'                8000
      !>         '0-0-10'              80   |--  Third component is supposed
      !>         '0-1-0'               80   |    to be a mod-10 count.
      !>         '0/1/0'               Error: '/' is not an accepted delimiter.
      !>         '1: 00 : 0 : 1'       7281
      !>         '1:::1'               7281
      !>         '1.1.1.1.1'           Error: Too many components
      !>         '1.1.1.1.'            Error: The last delimiter signals that
      !>                                      a fifth component will follow.
      !>
      !>         The following examples are for the Voyager 2 spacecraft. Note
      !>         that the last component of the Voyager clock has an offset
      !>         value of 1.
      !>
      !>         CLKSTR                TICKS
      !>         ----------------      --------------------
      !>         '0.0.001'             0
      !>         '0:0:002'             1
      !>         '0:01'                800
      !>         '1'                   48000
      !>         '1.0'                 48000
      !>         '1.0.0'               Error: The 3rd component is never 0.
      !>         '0.0:100'             99
      !>         '0-60-1'              48000
      !>         '1-1-1'               48800
      !>         '1-1-2'               48801
      !>
      !>## Restrictions
      !>
      !>     1)  An SCLK kernel appropriate to the spacecraft clock identified
      !>         by SC must be loaded at the time this routine is called.
      !>
      !>     2)  If the SCLK kernel used with this routine does not map SCLK
      !>         directly to barycentric dynamical time, a leapseconds kernel
      !>         must be loaded at the time this routine is called.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     B.V. Semenov       (JPL)
      !>     R.E. Thurman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR)
      !>
      !>        Updated to support buffering of data for multiple clocks.
      !>        This entry point tracks kernel pool changes but no longer
      !>        sets or uses watches.
      !>
      !>        Updated long error message.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS)
      !>
      !>        Updated to keep track of the POOL counter and call ZZCVPOOL.
      !>
      !>-    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to this routine.
      !>
      !>-    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB)
      !>
      !>        Bug fix: changed maximum value arguments to 1 in
      !>        calls to SCLI01 to fetch NFIELD and DELCDE values.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
      !>
      !>        Header was updated, particularly $Exceptions and $Restrictions
      !>        sections. Kernel pool watch is now set on required kernel
      !>        variables. Comment section for permuted index source lines
      !>        was added following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 04-SEP-1990 (NJB) (JML) (RET)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB)
      !>
      !>        Bug fix: changed maximum value arguments to 1 in
      !>        calls to SCLI01 to fetch NFIELD and DELCDE values.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
      !>
      !>        This routine now uses the new kernel pool watch capability
      !>        to determine when it is necessary to look up SCLK variables.
      !>        This method of checking for kernel pool updates replaces the
      !>        previously used once-per-call lookup of the SCLK_KERNEL_ID
      !>        kernel variable.
      !>
      !>        The header was updated to discuss the fact that a leapseconds
      !>        kernel will now need to be loaded in order to use SCLK kernels
      !>        that map between SCLK and a parallel time system other than
      !>        TDB. The $Exceptions and $Restrictions sections were affected.
      !>
      !>        A comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>     Standard SPICE error handling.
      module subroutine sctk01 ( ctx, sc, clkstr, ticks )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: lparsm, nparsd
      use pepper_inc_zzctr
      use pepper_inc_zzsc01
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft ID code.
      character(*), intent(in) :: clkstr  !! Character representation of a clock count.
      real(wp), intent(out) :: ticks  !! Number of ticks represented by the clock count.
      character(1), parameter :: del    ( ndelim ) = &
         [ character(1) :: '.', ':', '-', ',', ' ' ]
      integer(ip) :: n
      real(wp) :: savcmp
      integer(ip) :: pntr
      integer(ip) :: i
      if ( .not. allocated ( ctx%sc01_state ) ) then
         allocate ( ctx%sc01_state )
         call init_sc01_state ( ctx%sc01_state )
      end if
!
!     PEPPER: make the clock's data current before the ASSOCIATE block
!     (the database may grow).
!
      if ( ctx%failed() ) return
      if ( ctx%sc01_state%pass1 ) then
         call zzscin01 ( ctx )
         call zzctruin ( ctx, ctx%sc01_state%polctr )
         ctx%sc01_state%pass1 = .false.
      end if
      call zzscup01 ( ctx, sc )
      if ( ctx%failed() ) then
         ctx%sc01_state%pass1 = .true.
         return
      end if
      associate ( scbase => ctx%sc01_state%scbase, &
                  cmp => ctx%sc01_state%cmp, &
                  error => ctx%sc01_state%error, &
                  cmptks => ctx%sc01_state%cmptks, &
                  cmpval => ctx%sc01_state%cmpval, &
                  dpbuff => ctx%sc01_state%dpbuff, &
                  cofbas => ctx%sc01_state%cofbas, &
                  delcde => ctx%sc01_state%delcde, &
                  dpfree => ctx%sc01_state%dpfree, &
                  end => ctx%sc01_state%end, &
                  endbas => ctx%sc01_state%endbas, &
                  ifree => ctx%sc01_state%ifree, &
                  intbuf => ctx%sc01_state%intbuf, &
                  modbas => ctx%sc01_state%modbas, &
                  ncoeff => ctx%sc01_state%ncoeff, &
                  nfield => ctx%sc01_state%nfield, &
                  npart => ctx%sc01_state%npart, &
                  offbas => ctx%sc01_state%offbas, &
                  polctr => ctx%sc01_state%polctr, &
                  prvsc => ctx%sc01_state%prvsc, &
                  strbas => ctx%sc01_state%strbas, &
                  timsys => ctx%sc01_state%timsys, &
                  pass1 => ctx%sc01_state%pass1 )
      if ( ctx%failed() ) return

!
!     Get parameters and data for this clock from the type 1 database.
!     Update the database if necessary.
!

!
!     If our clock string is blank, we can stop now.
!
      if ( clkstr == ' ' ) then

         pass1 = .true.

         call ctx%setmsg ( 'CLKSTR is blank.'         )
         call ctx%sigerr ( 'SPICE(INVALIDSCLKSTRING)' )
         return

      end if

!
!     Determine how many ticks is each field is worth.
!
      cmptks( nfield ) = 1.0_wp

      do  i = nfield-1, 1, -1
         cmptks(i) = cmptks(i+1) * ( dpbuff( modbas + (i+1) ) )
      end do

!
!     Parse the clock components from the input string. There should
!     be at most NFIELD of them, but, in order to check for too long
!     a clock string, we'll let LPARSM take up to MXNFLD components and
!     then test for an error.
!
!     NOTE: LPARSM is error-free.
!
      call lparsm ( clkstr, delims, mxnfld, n, cmp )

!
!     If the string has too many fields for the specified spacecraft
!     then signal an error.
!
      if  ( n > nfield )  then

         pass1 = .true.

         call ctx%setmsg ( 'CLKSTR has # fields, which is too many.' )
         call ctx%errint ( '#' , n                                   )
         call ctx%sigerr ( 'SPICE(INVALIDSCLKSTRING)'                )
         return

      end if

!
!     Convert each of the components into numbers.  Error if any
!     of the conversions screw up.  NPARSD doesn't assign a value
!     to ' ', so assign the numeric value of the blank components
!     to be equal to the offset value.
!
      do i = 1, n

         if ( cmp(i) == ' ' )  then
            cmpval(i) = ( dpbuff( offbas + i ) )
         else
            call nparsd ( cmp(i), cmpval(i), error, pntr )
         end if

         if ( error /= ' ' ) then

            pass1 = .true.

            call ctx%setmsg ( 'Could not parse SCLK component # from ' &
            //           '# as a number.'                          )
            call ctx%errch  ( '#', cmp(i)                              )
            call ctx%errch  ( '#', clkstr                              )
            call ctx%sigerr ( 'SPICE(INVALIDSCLKSTRING)'               )
            return

         end if

!
!        Subtract off the offset value so that we can do base ten
!        arithmetic.  Also, if any of the components become negative
!        as a result of the subtraction, then that component must
!        have been invalid.
!
         savcmp    = cmpval(i)
         cmpval(i) = cmpval(i) - ( dpbuff( offbas + i ) )

         if  ( anint( cmpval(i) ) < 0.0_wp  )   then

            pass1 = .true.

            call ctx%setmsg ( 'Component number # in the SCLK string, ' &
            //            'counting left to right, is invalid: ' &
            //            'component # is less than field offset #.' )
            call ctx%errint ( '#', i                                     )
            call ctx%errdp  ( '#', savcmp                                )
            call ctx%errdp  ( '#', ( dpbuff( offbas + i ) )                             )
            call ctx%sigerr ( 'SPICE(INVALIDSCLKSTRING)'                 )
            return

          end if

      end do

!
!     Convert to ticks by multiplying the value of each component by
!     the number of ticks each component count represents, and then
!     add up the results.
!
      ticks = 0.0_wp

      do i = 1, n
         ticks = ticks + cmpval(i)*cmptks(i)
      end do

!
!     Keep track of the last spacecraft clock ID encountered.
!
      prvsc = sc

      return
      end associate
      end subroutine sctk01

      !>## Detailed Input
      !>
      !>     SC       is a NAIF spacecraft identification code. See the
      !>              $Examples section below, and also the KERNEL required
      !>              reading file for a complete list of body ID codes.
      !>
      !>     TICKS    is the number of ticks to be converted to a spacecraft
      !>              clock string, where a tick is defined to be
      !>              the smallest time increment expressible by the
      !>              spacecraft clock.
      !>
      !>              If TICKS contains a fractional part, the string that
      !>              results is the same as if TICKS had been rounded to
      !>              the nearest whole number.
      !>
      !>              See $Examples below.
      !>
      !>## Detailed Output
      !>
      !>     CLKSTR   on output is the character string representation of
      !>              the spacecraft clock count. The returned string has
      !>              the form
      !>
      !>                               'wwwwwwww:xx:y:z',
      !>
      !>              where the number of components and the width of each
      !>              one are different for each spacecraft. The delimiter
      !>              used is determined by a kernel pool variable and is
      !>              one of the five specified by the parameter DELIMS.
      !>              See $Examples below.
      !>
      !>              If CLKSTR is not long enough to accommodate the
      !>              formatted tick value, the result will be truncated on
      !>              the right.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  This routine assumes that that an SCLK kernel appropriate to
      !>         the spacecraft clock identified by the input argument SC has
      !>         been loaded. If an SCLK kernel has not been loaded, does not
      !>         contain all of the required data, or contains invalid data, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine. The output argument CLKSTR will not be modified.
      !>
      !>         The variables that must be set by the SCLK kernel are:
      !>
      !>            -  The number of fields in an (unabridged) SCLK string
      !>            -  The output delimiter code
      !>            -  The parallel time system code
      !>            -  The moduli of the fields of an SCLK string
      !>            -  The offsets for each clock field.
      !>            -  The SCLK coefficients array
      !>            -  The partition start times
      !>            -  The partition end times
      !>
      !>     2)  When using SCLK kernels that map SCLK to a time system other
      !>         than ET (also called barycentric dynamical time---`TDB'), it
      !>         is necessary to have a leapseconds kernel loaded at the time
      !>         this routine is called. If a leapseconds kernel is required
      !>         for conversion between SCLK and ET but is not loaded, an error
      !>         is signaled by a routine in the call tree of this routine. The
      !>         output argument CLKSTR will not be modified.
      !>
      !>         The time system that an SCLK kernel maps SCLK to is indicated
      !>         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
      !>         is the negative of the NAIF integer code for the spacecraft.
      !>         The time system used in a kernel is TDB if and only if the
      !>         variable is assigned the value 1.
      !>
      !>     3)  If any of the following kernel variables have invalid values,
      !>         the error will be diagnosed by routines called by this
      !>         routine:
      !>
      !>            -  The time system code
      !>            -  The number of SCLK coefficients
      !>            -  The number of partition start times
      !>            -  The number of partition end times
      !>            -  The number of fields of a SCLK string
      !>            -  The number of moduli for a SCLK string
      !>
      !>         If the number of values for any item read from the kernel
      !>         pool exceeds the maximum allowed value, it is may not be
      !>         possible to diagnose the error correctly, since overwriting
      !>         of memory may occur. This particular type of error is not
      !>         diagnosed by this routine.
      !>
      !>     4)  If the input value for TICKS is negative, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled. The output argument
      !>         CLKSTR will not be modified.
      !>
      !>     5)  If the output argument CLKSTR is too short to accommodate
      !>         the output string produced by this routine, the error
      !>         SPICE(SCLKTRUNCATED) is signaled. The output string
      !>         CLKSTR will not be modified.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     The routine determines the values of the components of the
      !>     spacecraft clock count that is equivalent to the number TICKS.
      !>     The information needed to perform this operation, such as the
      !>     number of clock components and their moduli, is provided by
      !>     an SCLK kernel file. Normally, your program should load this
      !>     file during initialization.
      !>
      !>     This routine does not make use of any partition information.
      !>     See SCDECD for details on how to make use of partition numbers.
      !>
      !>## Examples
      !>
      !>     Below are some examples illustrating various inputs and the
      !>      resulting outputs for the Galileo spacecraft.
      !>
      !>         TICKS                 CLKSTR
      !>         ----------------      --------------------
      !>         -1                    Error: Ticks must be a positive number
      !>         0                     '0:00:0:0'
      !>         1                     '0:00:0:1'
      !>         1.3                   '0:00:0:1'
      !>         1.5                   '0:00:0:2'
      !>         2                     '0:00:0:2'
      !>         7                     '0:00:0:7'
      !>         8                     '0:00:1:0'
      !>         80                    '0:01:0:0'
      !>         88                    '0:01:1:0'
      !>         7279                  '0:90:9:7'
      !>         7280                  '1:00:0:0'
      !>         1234567890            '169583:45:6:2'
      !>
      !>     The following examples are for the Voyager 2 spacecraft.
      !>     Note that the third component of the Voyager clock has an
      !>     offset value of one.
      !>
      !>         TICKS                 CLKSTR
      !>         ----------------      --------------------
      !>         -1                    Error: Ticks must be a positive number
      !>         0                     '00000 00 001'
      !>         1                     '00000 00 002'
      !>         1.3                   '00000:00:002'
      !>         1.5                   '00000.00.003'
      !>         2                     '00000-00-003'
      !>         799                   '00000,00,800'
      !>         800                   '00000 01 001'
      !>         47999                 '00000 59 800'
      !>         48000                 '00001 00 001'
      !>         3145727999            '65535 59 800'
      !>
      !>## Restrictions
      !>
      !>     1)  An SCLK kernel appropriate to the spacecraft clock identified
      !>         by SC must be loaded at the time this routine is called.
      !>
      !>     2)  If the SCLK kernel used with this routine does not map SCLK
      !>         directly to barycentric dynamical time, a leapseconds kernel
      !>         must be loaded at the time this routine is called.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     J.M. Lynch         (JPL)
      !>     B.V. Semenov       (JPL)
      !>     R.E. Thurman       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR)
      !>
      !>        Updated to support buffering of data for multiple clocks.
      !>        This entry point tracks kernel pool changes but no longer
      !>        sets or uses watches.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS)
      !>
      !>        Updated to keep track of the POOL counter and call ZZCVPOOL.
      !>
      !>-    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to this routine.
      !>
      !>-    SPICELIB Version 2.1.0, 17-FEB-2008 (NJB)
      !>
      !>        Bug fix: changed maximum value arguments to 1 in
      !>        calls to SCLI01 to fetch NFIELD and DELCDE values.
      !>
      !>        Bug fix: spaces between fields are now inserted
      !>        correctly when the output field delimiter is blank.
      !>
      !>-    SPICELIB Version 2.0.1, 18-JUL-1996 (NJB)
      !>
      !>        Misspelling in header fixed.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
      !>
      !>        Error is now signaled if truncation of output string occurs.
      !>        Header was updated, particularly $Exceptions and $Restrictions
      !>        sections. Kernel pool watch is now set on required kernel
      !>        variables. Comment section for permuted index source lines
      !>        was added following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB) (JML) (RET)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.1.0, 17-FEB-2008 (NJB)
      !>
      !>        Bug fix: changed maximum value arguments to 1 in
      !>        calls to SCLI01 to fetch NFIELD and DELCDE values.
      !>
      !>        Bug fix: spaces between fields are now inserted
      !>        correctly when the output field delimiter is blank.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
      !>
      !>        An error is now signaled if truncation of output string
      !>        occurs.
      !>
      !>        The header was updated to discuss exception handling when
      !>        the output string is truncated. The header was also expanded
      !>        to discuss the fact that a leapseconds kernel will now need to
      !>        be loaded in order to use SCLK kernels that map between SCLK
      !>        and a parallel time system other than TDB. The $Exceptions
      !>        and $Restrictions sections were affected.
      !>
      !>        This routine now uses the new kernel pool watch capability
      !>        to determine when it is necessary to look up SCLK variables.
      !>        This method of checking for kernel pool updates replaces the
      !>        previously used once-per-call lookup of the SCLK_KERNEL_ID
      !>        kernel variable.
      !>
      !>        A comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>     Standard SPICE error handling.
      module subroutine scfm01 ( ctx, sc, ticks, clkstr )
      use pepper_kinds, only: ip, wp
      use pepper_strings, only: prefix, suffix
      use pepper_inc_zzctr
      use pepper_inc_zzsc01
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft identification code.
      character(*), intent(out) :: clkstr  !! Character string representation of the clock count.
      real(wp), intent(in) :: ticks  !! Number of ticks represented by a clock count.
      character(1), parameter :: del    ( ndelim ) = &
         [ character(1) :: '.', ':', '-', ',', ' ' ]
      integer(ip) :: j
      character(dplen) :: dpchar
      integer(ip) :: needed
      integer(ip) :: pad
      integer(ip) :: i
      real(wp) :: maxwid
      real(wp) :: rem
      if ( .not. allocated ( ctx%sc01_state ) ) then
         allocate ( ctx%sc01_state )
         call init_sc01_state ( ctx%sc01_state )
      end if
!
!     PEPPER: make the clock's data current before the ASSOCIATE block
!     (the database may grow).
!
      if ( ctx%failed() ) return
      if ( ctx%sc01_state%pass1 ) then
         call zzscin01 ( ctx )
         call zzctruin ( ctx, ctx%sc01_state%polctr )
         ctx%sc01_state%pass1 = .false.
      end if
      call zzscup01 ( ctx, sc )
      if ( ctx%failed() ) then
         ctx%sc01_state%pass1 = .true.
         return
      end if
      associate ( scbase => ctx%sc01_state%scbase, &
                  cmp => ctx%sc01_state%cmp, &
                  cmptks => ctx%sc01_state%cmptks, &
                  cmpval => ctx%sc01_state%cmpval, &
                  dpbuff => ctx%sc01_state%dpbuff, &
                  cmpwid => ctx%sc01_state%cmpwid, &
                  cofbas => ctx%sc01_state%cofbas, &
                  delcde => ctx%sc01_state%delcde, &
                  dpfree => ctx%sc01_state%dpfree, &
                  end => ctx%sc01_state%end, &
                  endbas => ctx%sc01_state%endbas, &
                  ifree => ctx%sc01_state%ifree, &
                  intbuf => ctx%sc01_state%intbuf, &
                  length => ctx%sc01_state%length, &
                  modbas => ctx%sc01_state%modbas, &
                  ncoeff => ctx%sc01_state%ncoeff, &
                  nfield => ctx%sc01_state%nfield, &
                  npart => ctx%sc01_state%npart, &
                  offbas => ctx%sc01_state%offbas, &
                  polctr => ctx%sc01_state%polctr, &
                  prvsc => ctx%sc01_state%prvsc, &
                  strbas => ctx%sc01_state%strbas, &
                  timsys => ctx%sc01_state%timsys, &
                  pass1 => ctx%sc01_state%pass1 )
      if ( ctx%failed() ) return

!
!     On the first pass through the subroutine, or if the spacecraft
!     clock ID changes, we will set watches on the SCLK kernel
!     variables for the current clock.
!

!
!     Get parameters and data for this clock from the type 1 database.
!     Update the database if necessary.
!
!
!     Determine how many ticks each field is worth.
!
      cmptks( nfield ) = 1.0_wp

      do  i = nfield-1, 1, -1
         cmptks(i) = cmptks(i+1) * ( dpbuff( modbas + (i+1) ) )
      end do

!
!     Determine the width of each field.
!
      do i  = 1, nfield

         maxwid     =  ( dpbuff( modbas + i ) ) + ( dpbuff( offbas + i ) ) - 1.0_wp

         cmpwid(i)  =  1 + int ( log10( maxwid + 0.5_wp ) )

      end do

!
!     Check whether the output string is long enough to contain the
!     string we're about to assemble.  We need room for (NFIELD - 1)
!     delimiters as well as for the numeric fields.
!
      needed  =  nfield  -  1  +  sum(cmpwid(1:nfield))

      if (  len( clkstr )  <  needed  ) then

         pass1 = .true.

         call ctx%setmsg ( 'Output argument has declared length #; '      // &
                       'required length is #. Input tick value was #.' )
         call ctx%errint ( '#',  len(clkstr)                               )
         call ctx%errint ( '#',  needed                                    )
         call ctx%errdp  ( '#',  ticks                                     )
         call ctx%sigerr ( 'SPICE(SCLKTRUNCATED)'                          )
         return

      end if

!
!     Need to check that TICKS is a positive number.
!
      if (  anint( ticks ) < 0  ) then

         pass1 = .true.

         call ctx%setmsg ( 'Negative value for SCLK ticks: #' )
         call ctx%errdp  ( '#', ticks                         )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'           )
         return

      end if

!
!     Determine the value of each of the components. This is done by
!     successively dividing by the number of ticks each component value
!     is worth.
!
      rem = anint ( ticks )

      do i = 1, nfield - 1

         cmpval(i)   =  aint ( rem / cmptks(i) ) + ( dpbuff( offbas + i ) )
         rem         =  mod  ( rem,  cmptks(i) )

      end do

      cmpval(nfield) =  rem + ( dpbuff( offbas + nfield ) )

!
!     Convert the values of each component from double precision
!     numbers to character strings.
!
      do  i = 1, nfield
!
!        NOTE: DPSTRF is error-free.
!
         call dpstrf ( ctx, cmpval(i), dplen, 'F', dpchar )

         end        =  index ( dpchar, '.' ) - 1
         length(i)  =  end - 1
         cmp(i)     =  dpchar(2:end)

      end do

!
!     Pad on the left with zeros if necessary.
!
      do  i = 1, nfield

         pad = cmpwid(i) - length(i)

         if ( pad > 0 )  then

            do  j = 1, pad
               call prefix ( '0', 0, cmp(i) )
            end do

         end if

      end do

!
!     Construct the clock string with a delimiter separating
!     each field.
!
      clkstr = cmp(1)

      do  i = 2, nfield

         if ( del(delcde) /= ' ' ) then

            call prefix ( del(delcde), 0, cmp(i) )
            call suffix ( cmp(i),      0, clkstr )
         else
            call suffix ( cmp(i),      1, clkstr )
         end if

      end do

!
!     Keep track of the last spacecraft clock ID encountered.
!
      prvsc = sc

      return
      end associate
      end subroutine scfm01

      !>## Detailed Input
      !>
      !>     SC       is a NAIF ID code for a spacecraft, one of whose
      !>              clock values is represented by SCLKDP.
      !>
      !>     SCLKDP   is an encoded type 1 spacecraft clock value
      !>              produced by the routine SCENCD. SCLKDP is a
      !>              count of ticks since spacecraft clock start:
      !>              partition information IS included in the encoded
      !>              value.
      !>
      !>## Detailed Output
      !>
      !>     ET       is the ephemeris time, seconds past J2000, that
      !>              corresponds to SCLKDP.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  This routine assumes that that an SCLK kernel appropriate to
      !>         the spacecraft clock identified by the input argument SC has
      !>         been loaded. If an SCLK kernel has not been loaded, does not
      !>         contain all of the required data, or contains invalid data, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine. The output argument ET will not be modified.
      !>
      !>         The variables that must be set by the SCLK kernel are:
      !>
      !>            -  The number of fields in an (unabridged) SCLK string
      !>            -  The output delimiter code
      !>            -  The parallel time system code
      !>            -  The moduli of the fields of an SCLK string
      !>            -  The offsets for each clock field.
      !>            -  The SCLK coefficients array
      !>            -  The partition start times
      !>            -  The partition end times
      !>
      !>     2)  When using SCLK kernels that map SCLK to a time system other
      !>         than ET (also called barycentric dynamical time---`TDB'), it
      !>         is necessary to have a leapseconds kernel loaded at the time
      !>         this routine is called. If a leapseconds kernel is required
      !>         for conversion between SCLK and ET but is not loaded, an error
      !>         is signaled by a routine in the call tree of this routine. The
      !>         output argument ET will not be modified.
      !>
      !>         The time system that an SCLK kernel maps SCLK to is indicated
      !>         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
      !>         is the negative of the NAIF integer code for the spacecraft.
      !>         The time system used in a kernel is TDB if and only if the
      !>         variable is assigned the value 1.
      !>
      !>     3)  If any of the following kernel variables have invalid values,
      !>         the error will be diagnosed by routines called by this
      !>         routine:
      !>
      !>            -  The number of SCLK coefficients
      !>            -  The number of partition start times
      !>            -  The number of partition end times
      !>            -  The number of fields of a SCLK string
      !>            -  The number of moduli for a SCLK string
      !>
      !>         If the number of values for any item read from the kernel
      !>         pool exceeds the maximum allowed value, it is may not be
      !>         possible to diagnose the error correctly, since overwriting
      !>         of memory may occur. This particular type of error is not
      !>         diagnosed by this routine.
      !>
      !>     4)  If the time system code is not recognized, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     5)  If the input SCLK value SCLKDP is out of range, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled. The output argument ET
      !>         will not be modified.
      !>
      !>     6)  If the SCLK rate used to interpolate SCLK values is
      !>         nonpositive, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>         The output argument SCLKDP will not be modified.
      !>
      !>     7)  If the partition times or SCLK coefficients themselves
      !>         are invalid, this routine will almost certainly give
      !>         incorrect results. This routine cannot diagnose errors
      !>         in the partition times or SCLK coefficients, except possibly
      !>         by crashing.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     SCTE01 is not usually called by routines external to SPICELIB.
      !>     The conversion routine SCT2E converts any type of encoded
      !>     spacecraft clock value produced by SCENCD to ephemeris seconds
      !>     past J2000.  SCT2E is the preferred user interface routine
      !>     because its interface specification does not refer to spacecraft
      !>     clock types. However, direct use of SCTE01 by user routines is
      !>     not prohibited.
      !>
      !>## Examples
      !>
      !>     1)  Convert an encoded type 1 SCLK value to ET:
      !>
      !>         During program initialization, load the leapseconds and SCLK
      !>         kernels. We will assume that these files are named
      !>         "LEAPSECONDS.KER" and "SCLK.KER".  You must substitute the
      !>         actual names of these files in your code.
      !>
      !>            CALL CLPOOL
      !>            CALL FURNSH ( 'LEAPSECONDS.KER' )
      !>            CALL FURNSH ( 'SCLK.KER'        )
      !>
      !>         If SCLKDP is an encoded spacecraft clock value, if SC
      !>         is the NAIF integer code for the spacecraft whose
      !>         SCLK <--> ET mapping is defined by the data in SCLK.KER,
      !>         then the call
      !>
      !>            CALL SCTE01 ( SC, SCLKDP, ET )
      !>
      !>         will return the ET value corresponding to SCLKDP.
      !>
      !>         For example, if SC is -77, indicating the Galileo spacecraft,
      !>         and if a Galileo SCLK kernel is loaded, then if SCLKDP
      !>         is set to
      !>
      !>            7.2800000000000E+05
      !>
      !>         the call
      !>
      !>            CALL SCTE01 ( SC, SCLKDP, ET )
      !>
      !>         returns ET as
      !>
      !>            -3.2286984854565E+08
      !>
      !>         on a VAX 11/780 running VMS 5.3, Fortran 5.5.
      !>
      !>## Restrictions
      !>
      !>     1)  An SCLK kernel appropriate to the spacecraft clock identified
      !>         by SC must be loaded at the time this routine is called.
      !>
      !>     2)  If the SCLK kernel used with this routine does not map SCLK
      !>         directly to barycentric dynamical time, a leapseconds kernel
      !>         must be loaded at the time this routine is called.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 4.0.0, 01-DEC-2021 (NJB) (JDR)
      !>
      !>        Updated to support buffering of data for multiple clocks.
      !>        This entry point tracks kernel pool changes but no longer
      !>        sets or uses watches.
      !>
      !>        A check for invalid time system code was added.
      !>
      !>        A check for nonpositive clock rate was added.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>-    SPICELIB Version 3.3.0, 09-SEP-2013 (BVS)
      !>
      !>        Updated to keep track of the POOL counter and call ZZCVPOOL.
      !>
      !>-    SPICELIB Version 3.2.0, 05-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to this routine.
      !>
      !>-    SPICELIB Version 3.1.0, 09-NOV-2007 (NJB)
      !>
      !>        Bug fix: changed maximum value arguments to 1 in
      !>        calls to SCLI01 to fetch NFIELD and DELCDE values.
      !>
      !>-    SPICELIB Version 3.0.0, 06-JAN-1998 (NJB)
      !>
      !>        Removed local variable RNDCLK; this entry point no longer
      !>        creates a rounded version of its input argument. Use of
      !>        ANINT to round coefficients has been discontinued.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
      !>
      !>        This routine was updated to handle SCLK kernels that use
      !>        TDT as their `parallel' time system. Header was updated,
      !>        particularly $Exceptions and $Restrictions. Watch is now
      !>        set on required kernel variables. Comment section for
      !>        permuted index source lines was added following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 21-AUG-1990 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 3.1.0, 09-NOV-2007 (NJB)
      !>
      !>        Bug fix: changed maximum value arguments to 1 in
      !>        calls to SCLI01 to fetch NFIELD and DELCDE values.
      !>
      !>-    SPICELIB Version 3.0.0, 06-JAN-1998 (NJB)
      !>
      !>        Removed local variable RNDCLK; this entry point no longer
      !>        creates a rounded version of its input argument. Use of
      !>        ANINT to round coefficients has been discontinued.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
      !>
      !>        This routine was updated to handle a time system specification
      !>        for the `parallel' time system used in the SCLK kernel.
      !>
      !>        Specific changes include:
      !>
      !>           -- The time system code is looked up along with the
      !>              other SCLK specification parameters.
      !>
      !>           -- The time value arrived at by interpolation of the
      !>              SCLK-to-parallel time mapping is converted to TDB
      !>              if the parallel time system is TDT.
      !>
      !>        The header was expanded to discuss the fact that a leapseconds
      !>        kernel will now need to be loaded in order to use SCLK kernels
      !>        that map between SCLK and a parallel time system other than
      !>        TDB. The $Exceptions and $Restrictions sections were affected.
      !>
      !>        This routine now uses the new kernel pool watch capability
      !>        to determine when it is necessary to look up SCLK variables.
      !>        This method of checking for kernel pool updates replaces the
      !>        previously used once-per-call lookup of the SCLK_KERNEL_ID
      !>        kernel variable.
      !>
      !>        A comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>     Standard SPICE error handling.
      module subroutine scte01 ( ctx, sc, sclkdp, et )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      use pepper_inc_zzsc01
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft ID code.
      real(wp), intent(in) :: sclkdp  !! Type 1 SCLK, encoded as ticks since clock start.
      real(wp), intent(out) :: et  !! Ephemeris time, seconds past J2000. (PEPPER: an output; SPICELIB's table said I)
      character(1), parameter :: del    ( ndelim ) = &
         [ character(1) :: '.', ':', '-', ',', ' ' ]
      real(wp) :: const
      real(wp) :: tikmsc
      real(wp) :: rate
      real(wp) :: tikdif
      integer(ip) :: i
      integer(ip) :: upper
      real(wp) :: mxtick
      integer(ip) :: middle
      if ( .not. allocated ( ctx%sc01_state ) ) then
         allocate ( ctx%sc01_state )
         call init_sc01_state ( ctx%sc01_state )
      end if
!
!     PEPPER: make the clock's data current before the ASSOCIATE block
!     (the database may grow).
!
      if ( ctx%failed() ) return
      if ( ctx%sc01_state%pass1 ) then
         call zzscin01 ( ctx )
         call zzctruin ( ctx, ctx%sc01_state%polctr )
         ctx%sc01_state%pass1 = .false.
      end if
      call zzscup01 ( ctx, sc )
      if ( ctx%failed() ) then
         ctx%sc01_state%pass1 = .true.
         return
      end if
      associate ( scbase => ctx%sc01_state%scbase, &
                  dpbuff => ctx%sc01_state%dpbuff, &
                  partim => ctx%sc01_state%partim, &
                  cofbas => ctx%sc01_state%cofbas, &
                  delcde => ctx%sc01_state%delcde, &
                  dpfree => ctx%sc01_state%dpfree, &
                  end => ctx%sc01_state%end, &
                  endbas => ctx%sc01_state%endbas, &
                  ifree => ctx%sc01_state%ifree, &
                  intbuf => ctx%sc01_state%intbuf, &
                  lower => ctx%sc01_state%lower, &
                  modbas => ctx%sc01_state%modbas, &
                  ncoeff => ctx%sc01_state%ncoeff, &
                  nfield => ctx%sc01_state%nfield, &
                  npart => ctx%sc01_state%npart, &
                  offbas => ctx%sc01_state%offbas, &
                  polctr => ctx%sc01_state%polctr, &
                  prvsc => ctx%sc01_state%prvsc, &
                  strbas => ctx%sc01_state%strbas, &
                  timsys => ctx%sc01_state%timsys, &
                  pass1 => ctx%sc01_state%pass1 )
      if ( ctx%failed() ) return

!
!     Get parameters and data for this clock from the type 1 database.
!     Update the database if necessary.
!

!
!     To check whether SCLKDP is in range, we must find the end time
!     of the last partition, in total ticks since spacecraft clock
!     start.
!
      mxtick = 0.0_wp

      do i = 1, npart
         mxtick  =  anint (  ( dpbuff( endbas + i ) ) - ( dpbuff( strbas + i ) ) + mxtick  )
      end do

!
!     We now check that SCLKDP is in range.  COEFFS(1,1) and
!     MXTICK are, respectively, the first and last absolute
!     tick values of the clock.
!
      if (      ( sclkdp < ( dpbuff( cofbas + 1 + (1-1)*3 ) )    ) &
           .or. ( sclkdp > mxtick         )   ) then

         pass1 = .true.

         call ctx%setmsg (  bvlmsg                  )
         call ctx%errch  ( '#', 'SCLKDP'            )
         call ctx%errdp  ( '#',  sclkdp             )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)' )
         return

      end if

!
!     Ok, if we made it this far, we can actually interpret the tick
!     value.  But by this time, we're not in very good mood.
!

!
!     Find the tick value in COEFFS closest to the rounded input tick
!     value.  The tick values in COEFFS are monotone increasing, so we
!     can do a binary search to find index of the greatest tick value
!     in the coefficient array that is less than or equal to SCLKDP.
!
!     There are two cases:
!
!        1) SCLKDP is bounded by the least and greatest SCLK
!           coefficients in the array.  In this case, we must search
!           the array for a consecutive pair of records whose SCLK
!           values bound SCLKDP.
!
!        2) SCLKDP is greater than or equal to all of the SCLK
!           coefficients.  In that case, we don't need to search:  the
!           last SCLK value in the array is the one we want.
!

      if (  sclkdp <  ( dpbuff( cofbas + 1 + ((ncoeff/3)-1)*3 ) ) ) then

         lower  =  1
         upper  =  ncoeff / 3

!
!        In the following loop, we maintain an invariant:
!
!           COEFFS( 1, LOWER )  <  SCLKDP  <  COEFFS( 1, UPPER )
!                               -
!
!        At each step, we decrease the distance between LOWER and
!        UPPER, while keeping the above statement true.  The loop
!        terminates when LOWER = UPPER - 1.
!
!        Note that we start out with if LOWER < UPPER, since we've
!        already made sure that the invariant expression above is true.
!
         do while ( lower < upper - 1 )

            middle  =  ( lower + upper ) / 2

            if (  sclkdp  < ( dpbuff( cofbas + 1 + (middle-1)*3 ) ) ) then
               upper = middle
            else
               lower = middle
            end if

         end do

!
!        We've got SCLKDP trapped between two tick values that are
!        `adjacent' in the list:
!
!           COEFFS ( 1, LOWER )  and
!           COEFFS ( 1, UPPER )
!
!        since the second value must be greater than the first.  So
!
!           COEFFS( 1, LOWER )
!
!        is the last tick value in the coefficients array less than or
!        equal to SCLKDP.
!

      else
!
!        SCLKDP is greater than or equal to all of the SCLK
!        coefficients in the coefficients array.
!
         lower = ncoeff / 3

      end if

!
!     Now we evaluate a linear polynomial to find the time value that
!     corresponds to SCLKDP.  The coefficients of the polynomial are
!     the time and rate (in units of seconds per tick) that correspond
!     to the tick value
!
!        COEFFS( 1, LOWER )
!
!     We call these coefficients CONST and RATE.  The rates in the
!     coefficients array are in units of seconds per most significant
!     SCLK count, so we use the conversion factor TIKMSC to change the
!     rate to seconds per tick.
!
      tikmsc = 1.0_wp

      do i = nfield, 2, -1
         tikmsc  =  tikmsc * ( dpbuff( modbas + i ) )
      end do

      if (  ( dpbuff( cofbas + 3 + (lower-1)*3 ) ) <= 0.0_wp  ) then

         pass1 = .true.

         call ctx%setmsg ( 'Invalid SCLK rate.'     )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)' )
         return

      end if

      tikdif =  sclkdp  -  ( dpbuff( cofbas + 1 + (lower-1)*3 ) )
      const  =             ( dpbuff( cofbas + 2 + (lower-1)*3 ) )
      rate   =             ( dpbuff( cofbas + 3 + (lower-1)*3 ) )  /  tikmsc

      partim =  const  +  ( rate * tikdif )

!
!     Convert the parallel time to TDB, if the system is not TDB.
!     We don't need to check the validity of TIMSYS, because SCLI01
!     already made this check.
!
      if ( timsys == tdb ) then

         et = partim

      else if ( timsys == tdt ) then

         et = unitim ( ctx, partim, 'TDT', 'TDB' )

         if ( ctx%failed() ) return

      else
!
!        This code should be unreachable. It's present for safety.
!
         pass1 = .true.

         call ctx%setmsg ( 'Invalid time system code # was found for ' &
         //            'SCLK #.'                                   )
         call ctx%errint ( '#', timsys                                 )
         call ctx%errint ( '#', sc                                     )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                    )
         return

      end if

!
!     Keep track of the last spacecraft clock ID encountered.
!
      prvsc = sc

      return
      end associate
      end subroutine scte01

      !>## Detailed Input
      !>
      !>     SC       is a NAIF ID code for a spacecraft, one of whose
      !>              clock values is represented by SCLKDP.
      !>
      !>     ET       is an ephemeris time, specified in seconds past
      !>              J2000, whose equivalent encoded SCLK value is
      !>              desired.
      !>
      !>## Detailed Output
      !>
      !>     SCLKDP   is the encoded type 1 spacecraft clock value
      !>              that corresponds to ET. The value is obtained
      !>              by mapping ET, using the piecewise linear mapping
      !>              defined by the SCLK kernel, to a value that may
      !>              have a non-zero fractional part, and then
      !>              rounding this value to the nearest double precision
      !>              whole number.
      !>
      !>              SCLKDP represents total time since spacecraft
      !>              clock start and hence does reflect partition
      !>              information.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  This routine assumes that that an SCLK kernel appropriate to
      !>         the spacecraft clock identified by the input argument SC has
      !>         been loaded. If an SCLK kernel has not been loaded, does not
      !>         contain all of the required data, or contains invalid data, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine. The output argument SCLKDP will not be modified.
      !>
      !>         The variables that must be set by the SCLK kernel are:
      !>
      !>            -  The number of fields in an (unabridged) SCLK string
      !>            -  The output delimiter code
      !>            -  The parallel time system code
      !>            -  The moduli of the fields of an SCLK string
      !>            -  The offsets for each clock field.
      !>            -  The SCLK coefficients array
      !>            -  The partition start times
      !>            -  The partition end times
      !>
      !>     2)  When using SCLK kernels that map SCLK to a time system other
      !>         than ET (also called barycentric dynamical time---`TDB'), it
      !>         is necessary to have a leapseconds kernel loaded at the time
      !>         this routine is called. If a leapseconds kernel is required
      !>         for conversion between SCLK and ET but is not loaded, an error
      !>         is signaled by a routine in the call tree of this routine. The
      !>         output argument SCLKDP will not be modified.
      !>
      !>         The time system that an SCLK kernel maps SCLK to is indicated
      !>         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
      !>         is the negative of the NAIF integer code for the spacecraft.
      !>         The time system used in a kernel is TDB if and only if the
      !>         variable is assigned the value 1.
      !>
      !>     3)  If any of the following kernel variables have invalid values,
      !>         the error will be diagnosed by routines called by this
      !>         routine:
      !>
      !>            -  The number of SCLK coefficients
      !>            -  The number of partition start times
      !>            -  The number of partition end times
      !>            -  The number of fields of a SCLK string
      !>            -  The number of moduli for a SCLK string
      !>
      !>         If the number of values for any item read from the kernel
      !>         pool exceeds the maximum allowed value, it is may not be
      !>         possible to diagnose the error correctly, since overwriting
      !>         of memory may occur. This particular type of error is not
      !>         diagnosed by this routine.
      !>
      !>     4)  If the time system code is not recognized, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     5)  If the input ephemeris time value ET is out of range, the
      !>         error SPICE(VALUEOUTOFRANGE) is signaled. The output argument
      !>         SCLKDP will not be modified.
      !>
      !>     6)  If the SCLK rate used to interpolate SCLK values is
      !>         nonpositive, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>         The output argument SCLKDP will not be modified.
      !>
      !>     7)  If the partition times or SCLK coefficients themselves
      !>         are invalid, this routine will almost certainly give
      !>         incorrect results. This routine cannot diagnose errors
      !>         in the partition times or SCLK coefficients, except possibly
      !>         by crashing.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     Normally, the newer entry point SCEC01 (ET to continuous ticks,
      !>     type 1) should be used in place of this routine.
      !>
      !>     SCET01 is not usually called by routines external to SPICELIB.
      !>     The conversion routine SCE2T converts ephemeris seconds past J2000
      !>     to any type of discrete, encoded type 1 spacecraft clock value.
      !>     For conversion to continuous, encoded SCLK, SCE2C is the preferred
      !>     user interface routine because its interface specification does
      !>     not refer to spacecraft clock types. For conversion to discrete,
      !>     encoded SCLK, SCE2T is the preferred interface routine.
      !>
      !>     However, direct use of SCET01 by user routines is not prohibited.
      !>
      !>## Examples
      !>
      !>     1)  Converting ET to encoded type 1 SCLK:
      !>
      !>         During program initialization, load the leapseconds and SCLK
      !>         kernels. We will assume that these files are named
      !>         "LEAPSECONDS.KER" and "SCLK.KER".  You must substitute the
      !>         actual names of these files in your code.
      !>
      !>            CALL CLPOOL
      !>            CALL FURNSH ( 'LEAPSECONDS.KER' )
      !>            CALL FURNSH ( 'SCLK.KER'        )
      !>
      !>         If SC is -77, indicating the Galileo spacecraft, and
      !>         ET is set to
      !>
      !>            -3.2286984854565E+08
      !>
      !>         then the call
      !>
      !>            CALL SCET01 ( SC, ET, SCLKDP )
      !>
      !>         returns SCLKDP as
      !>
      !>            7.2800000000000E+05
      !>
      !>         on a VAX 11/780 running VMS 5.3, Fortran 5.5. Note that
      !>         the result should be the same (except for the output format)
      !>         on most computers, since the result is a double precision
      !>         whole number.
      !>
      !>## Restrictions
      !>
      !>     1)  An SCLK kernel appropriate to the spacecraft clock identified
      !>         by SC must be loaded at the time this routine is called.
      !>
      !>     2)  If the SCLK kernel used with this routine does not map SCLK
      !>         directly to barycentric dynamical time, a leapseconds kernel
      !>         must be loaded at the time this routine is called.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>     E.D. Wright        (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR)
      !>
      !>        Updated to support buffering of data for multiple clocks.
      !>        This entry point tracks kernel pool changes but no longer
      !>        sets or uses watches.
      !>
      !>        A check for invalid time system code was added.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Corrected description of invalid clock rate exception.
      !>
      !>-    SPICELIB Version 2.3.0, 09-SEP-2013 (BVS)
      !>
      !>        Updated to keep track of the POOL counter and call ZZCVPOOL.
      !>
      !>-    SPICELIB Version 2.2.0, 05-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to this routine.
      !>
      !>-    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB)
      !>
      !>        Bug fix: changed maximum value arguments to 1 in
      !>        calls to SCLI01 to fetch NFIELD and DELCDE values.
      !>
      !>-    SPICELIB Version 2.0.3, 22-AUG-2006 (EDW)
      !>
      !>        Replaced references to LDPOOL with references
      !>        to FURNSH.
      !>
      !>-    SPICELIB Version 2.0.2, 09-MAR-1999 (NJB)
      !>
      !>        Comments were updated; references to SCE2C and SCEC01 were
      !>        added.
      !>
      !>-    SPICELIB Version 2.0.1, 18-JUL-1996 (NJB)
      !>
      !>        Typo in comment fixed.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
      !>
      !>        This routine was updated to handle SCLK kernels that use
      !>        TDT as their `parallel' time system. Header was updated,
      !>        particularly $Exceptions and $Restrictions. Watch is now
      !>        set on required kernel variables. Comment section for
      !>        permuted index source lines was added following the header.
      !>
      !>-    SPICELIB Version 1.0.0, 04-SEP-1990 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 2.1.0, 09-NOV-2007 (NJB)
      !>
      !>        Bug fix: changed maximum value arguments to 1 in
      !>        calls to SCLI01 to fetch NFIELD and DELCDE values.
      !>
      !>-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB)
      !>
      !>        This routine was updated to handle a time system specification
      !>        for the `parallel' time system used in the SCLK kernel.
      !>
      !>        Specific changes include:
      !>
      !>           -- The time system code is looked up along with the
      !>              other SCLK specification parameters.
      !>
      !>           -- The input TDB value is converted, if necessary, to the
      !>              time system used in the parallel-time-to-SCLK mapping
      !>              defined by the current SCLK coefficients for the
      !>              specified spacecraft clock. This conversion is performed
      !>              prior to determination by interpolation of the
      !>              corresponding encoded SCLK value.
      !>
      !>        The header was expanded to discuss the fact that a leapseconds
      !>        kernel will now need to be loaded in order to use SCLK kernels
      !>        that map between SCLK and a parallel time system other than
      !>        TDB. The $Exceptions and $Restrictions sections were affected.
      !>
      !>        This routine now uses the new kernel pool watch capability
      !>        to determine when it is necessary to look up SCLK variables.
      !>        This method of checking for kernel pool updates replaces the
      !>        previously used once-per-call lookup of the SCLK_KERNEL_ID
      !>        kernel variable.
      !>
      !>        A comment section for permuted index source lines was added
      !>        following the header.
      !>
      !>     Standard SPICE error handling.
      module subroutine scet01 ( ctx, sc, et, sclkdp )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      use pepper_inc_zzsc01
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft ID code.
      real(wp), intent(out) :: sclkdp  !! Type 1 SCLK, encoded as ticks since clock start.
      real(wp), intent(in) :: et  !! Ephemeris time, seconds past J2000.
      character(1), parameter :: del    ( ndelim ) = &
         [ character(1) :: '.', ':', '-', ',', ' ' ]
      real(wp) :: timdif
      real(wp) :: const
      real(wp) :: tikmsc
      real(wp) :: rate
      real(wp) :: dval
      integer(ip) :: i
      integer(ip) :: upper
      real(wp) :: mxtick
      integer(ip) :: middle
      if ( .not. allocated ( ctx%sc01_state ) ) then
         allocate ( ctx%sc01_state )
         call init_sc01_state ( ctx%sc01_state )
      end if
!
!     PEPPER: make the clock's data current before the ASSOCIATE block
!     (the database may grow).
!
      if ( ctx%failed() ) return
      if ( ctx%sc01_state%pass1 ) then
         call zzscin01 ( ctx )
         call zzctruin ( ctx, ctx%sc01_state%polctr )
         ctx%sc01_state%pass1 = .false.
      end if
      call zzscup01 ( ctx, sc )
      if ( ctx%failed() ) then
         ctx%sc01_state%pass1 = .true.
         return
      end if
      associate ( scbase => ctx%sc01_state%scbase, &
                  dpbuff => ctx%sc01_state%dpbuff, &
                  partim => ctx%sc01_state%partim, &
                  cofbas => ctx%sc01_state%cofbas, &
                  delcde => ctx%sc01_state%delcde, &
                  dpfree => ctx%sc01_state%dpfree, &
                  end => ctx%sc01_state%end, &
                  endbas => ctx%sc01_state%endbas, &
                  ifree => ctx%sc01_state%ifree, &
                  intbuf => ctx%sc01_state%intbuf, &
                  lower => ctx%sc01_state%lower, &
                  modbas => ctx%sc01_state%modbas, &
                  ncoeff => ctx%sc01_state%ncoeff, &
                  nfield => ctx%sc01_state%nfield, &
                  npart => ctx%sc01_state%npart, &
                  offbas => ctx%sc01_state%offbas, &
                  polctr => ctx%sc01_state%polctr, &
                  prvsc => ctx%sc01_state%prvsc, &
                  strbas => ctx%sc01_state%strbas, &
                  timsys => ctx%sc01_state%timsys, &
                  pass1 => ctx%sc01_state%pass1 )
      if ( ctx%failed() ) return

!
!     Get parameters and data for this clock from the type 1 database.
!     Update the database if necessary.
!

!
!     Convert the input TDB time to the parallel time system, if the
!     parallel system is not TDB.
!
!     We don't need to check the validity of TIMSYS, because SCLI01
!     already made this check.
!
      if ( timsys == tdb ) then

         partim = et

      else if ( timsys == tdt ) then

         partim = unitim ( ctx, et, 'TDB', 'TDT' )

         if ( ctx%failed() ) return

      else
!
!        This code should be unreachable. It's present for safety.
!
         pass1 = .true.

         call ctx%setmsg ( 'Invalid time system code # was found for ' &
         //            'SCLK #.'                                   )
         call ctx%errint ( '#', timsys                                 )
         call ctx%errint ( '#', sc                                     )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                    )
         return

      end if
!
!     We'd like to ascertain whether PARTIM is between the minimum
!     time value in the coefficients array and the end time
!     corresponding to the number of ticks since spacecraft clock
!     start at the end of the last partition.
!
!     Checking the time value is a special case; we'll convert the time
!     value to ticks, and then check whether the resulting value is
!     less than the total number of ticks since spacecraft clock start
!     at the end of the last partition.  So, this check is performed
!     at the end of the routine.
!
!     Find the time value in COEFFS closest to the input time value.
!     The time values are ordered, so we can do a binary search for the
!     closest one.  When the search is done, we will have found the
!     index of the greatest time value in the coefficient array that
!     is less than or equal to PARTIM.
!
!     There are three cases:
!
!        1) PARTIM is less than the least time coefficient in the array.
!           In this case, we'll use the first coefficient set in the
!           kernel to extrapolate from.  We don't automatically treat
!           this case as an error because PARTIM could round up to the
!           minimum tick value when converted to ticks.
!
!        2) PARTIM is bounded by the least and greatest time
!           coefficients in the array.  In this case, we must search
!           the array for a consecutive pair of records whose time
!           values bound PARTIM.
!
!        3) PARTIM is greater than or equal to all of the time
!           coefficients.  In that case, we don't need to search:  the
!           last time value in the array is the one we want.
!
      if (  partim  <  ( dpbuff( cofbas + 2 + (1-1)*3 ) )  ) then
!
!        The coefficient set to use for extrapolation is the first.
!
         lower = 1

      else if (  partim < ( dpbuff( cofbas + 2 + ((ncoeff/3)-1)*3 ) )  ) then
!
!        In the following loop, we maintain an invariant:
!
!           COEFFS( 2, LOWER )  <   PARTIM   <   COEFFS( 2, UPPER )
!                               -
!
!        At each step, we decrease the distance between LOWER and
!        UPPER, while keeping the above statement true.  The loop
!        terminates when LOWER = UPPER - 1.
!
!        Note that we start out with if LOWER < UPPER, since we've
!        already made sure that the invariant expression above is true.
!

         lower  =  1
         upper  =  ncoeff / 3

         do while ( lower < upper - 1 )

            middle  =  ( lower + upper ) / 2

            if (  partim  <  ( dpbuff( cofbas + 2 + (middle-1)*3 ) )  ) then
               upper = middle
            else
               lower = middle
            end if

         end do

!
!        We've got PARTIM trapped between two time values that are
!        `adjacent' in the list:
!
!           COEFFS ( 2, LOWER )  and
!           COEFFS ( 2, UPPER )
!
!        since the second value must be greater than the first.  So
!
!           COEFFS( 2, LOWER )
!
!        is the last time value in the coefficients array less than or
!        equal to PARTIM.
!

      else
!
!        PARTIM is greater than or equal to all of the time values in
!        the coefficients array.
!
         lower = ncoeff / 3

      end if

!
!     Now we evaluate a linear polynomial to find the tick value that
!     corresponds to PARTIM.  The coefficients of the polynomial are
!     the tick value and rate (in units of ticks per second) that
!     correspond to the time value
!
!        COEFFS( 2, LOWER )
!
!     We call these coefficients CONST and RATE.  The rates in the
!     coefficients array are in units of seconds per most significant
!     clock count, so we use the conversion factor TIKMSC (`ticks per
!     most significant count') to change the rate to seconds per tick.
!
!     One other thing:  SCLKDP should be an integral number of ticks.
!     We use the generic `nearest whole number' function ANINT to
!     ensure this.
!
      timdif =  partim  -  ( dpbuff( cofbas + 2 + (lower-1)*3 ) )
      const  =             ( dpbuff( cofbas + 1 + (lower-1)*3 ) )

      if (  ( dpbuff( cofbas + 3 + (lower-1)*3 ) ) <= 0.0_wp  ) then

         pass1 = .true.

         call ctx%setmsg ( 'Invalid SCLK rate.'     )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)' )
         return

      end if

      tikmsc = 1.0_wp

      do i = nfield, 2, -1
         tikmsc  =  tikmsc * ( dpbuff( modbas + i ) )
      end do

      rate   =  1.0_wp   /   (  ( dpbuff( cofbas + 3 + (lower-1)*3 ) ) / tikmsc  )

      sclkdp =  anint (  const   +   ( rate * timdif )  )

!
!     Now, we'll see whether the SCLK value we've found is meaningful.
!     If it's too large, that's because the input PARTIM was beyond the
!     maximum value we can handle.  To check whether PARTIM is in
!     range, we must find the end time of the last partition, in total
!     ticks since spacecraft clock start.
!
      mxtick  =  anint ( ( dpbuff( endbas + 1 ) ) - ( dpbuff( strbas + 1 ) ) )

      do i = 2, npart
         mxtick  =  anint ( ( dpbuff( endbas + i ) ) -  ( dpbuff( strbas + i ) )  +  mxtick )
      end do

!
!     Make sure that ET does not precede the ET corresponding to
!     the clock's minimum tick value or exceed the ET corresponding to
!     the clock's maximum tick value.  We'll do the comparison
!     using the tick value that ET mapped to and the minimum and
!     maximum tick values of the spacecraft clock.
!
!     Convert SCLKDP and COEFFS(1,1) to whole numbers, so that
!     direct comparisons without tolerances are possible.
!
      sclkdp = anint ( sclkdp      )
      dval   = anint ( ( dpbuff( cofbas + 1 + (1-1)*3 ) ) )

      if (      ( sclkdp < dval  ) &
           .or. ( sclkdp > mxtick       )   ) then

         pass1 = .true.

         call ctx%setmsg (  bvlmsg                  )
         call ctx%errch  ( '#', 'ET'                )
         call ctx%errdp  ( '#',  et                 )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)' )
         return

      end if

!
!     Keep track of the last spacecraft clock ID encountered.
!
      prvsc = sc

      return
      end associate
      end subroutine scet01

      !>## Detailed Input
      !>
      !>     SC       is a NAIF ID code for a spacecraft, one of whose
      !>              clock values is represented by SCLKDP.
      !>
      !>     ET       is an ephemeris time, specified in seconds past
      !>              J2000, whose equivalent encoded SCLK value is
      !>              desired.
      !>
      !>## Detailed Output
      !>
      !>     SCLKDP   is the continuous encoded type 1 spacecraft clock
      !>              value corresponding to ET. The value is obtained
      !>              by mapping ET, using the piecewise linear mapping
      !>              defined by the SCLK kernel, to a value that may
      !>              have a non-zero fractional part. Unlike the output
      !>              of SCET01, SCLKDP is not rounded by this routine.
      !>
      !>              SCLKDP represents total time since spacecraft
      !>              clock start and hence does reflect partition
      !>              information.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  This routine assumes that that an SCLK kernel appropriate to
      !>         the spacecraft clock identified by the input argument SC has
      !>         been loaded. If an SCLK kernel has not been loaded, does not
      !>         contain all of the required data, or contains invalid data, an
      !>         error is signaled by a routine in the call tree of this
      !>         routine. The output argument SCLKDP will not be modified.
      !>
      !>         The variables that must be set by the SCLK kernel are:
      !>
      !>            -  The number of fields in an (unabridged) SCLK string
      !>            -  The output delimiter code
      !>            -  The parallel time system code
      !>            -  The moduli of the fields of an SCLK string
      !>            -  The offsets for each clock field.
      !>            -  The SCLK coefficients array
      !>            -  The partition start times
      !>            -  The partition end times
      !>
      !>     2)  When using SCLK kernels that map SCLK to a time system other
      !>         than ET (also called barycentric dynamical time---`TDB'), it
      !>         is necessary to have a leapseconds kernel loaded at the time
      !>         this routine is called. If a leapseconds kernel is required
      !>         for conversion between SCLK and ET but is not loaded, an error
      !>         is signaled by a routine in the call tree of this routine. The
      !>         output argument SCLKDP will not be modified.
      !>
      !>         The time system that an SCLK kernel maps SCLK to is indicated
      !>         by the variable SCLK_TIME_SYSTEM_nn in the kernel, where nn
      !>         is the negative of the NAIF integer code for the spacecraft.
      !>         The time system used in a kernel is TDB if and only if the
      !>         variable is assigned the value 1.
      !>
      !>     3)  If any of the following kernel variables have invalid values,
      !>         the error will be diagnosed by routines called by this
      !>         routine:
      !>
      !>            -  The number of SCLK coefficients
      !>            -  The number of partition start times
      !>            -  The number of partition end times
      !>            -  The number of fields of a SCLK string
      !>            -  The number of moduli for a SCLK string
      !>
      !>         If the number of values for any item read from the kernel
      !>         pool exceeds the maximum allowed value, it is may not be
      !>         possible to diagnose the error correctly, since overwriting
      !>         of memory may occur. This particular type of error is not
      !>         diagnosed by this routine.
      !>
      !>     4)  If the time system code is not recognized, the error
      !>         SPICE(VALUEOUTOFRANGE) is signaled.
      !>
      !>     5)  If the input ephemeris time value ET is out of range, the
      !>         error SPICE(VALUEOUTOFRANGE) is signaled. The output argument
      !>         SCLKDP will not be modified.
      !>
      !>     6)  If the SCLK rate used to interpolate SCLK values is
      !>         nonpositive, the error SPICE(VALUEOUTOFRANGE) is signaled.
      !>         The output argument SCLKDP will not be modified.
      !>
      !>     7)  If the partition times or SCLK coefficients themselves
      !>         are invalid, this routine will almost certainly give
      !>         incorrect results. This routine cannot diagnose errors
      !>         in the partition times or SCLK coefficients, except possibly
      !>         by crashing.
      !>
      !>## Files
      !>
      !>     None.
      !>
      !>## Particulars
      !>
      !>     SCEC01 is not usually called by routines external to SPICELIB.
      !>     The conversion routine SCE2C converts ephemeris seconds
      !>     past J2000 to any type of encoded spacecraft clock value.
      !>     SCE2C is the preferred user interface routine because its
      !>     interface specification does not refer to spacecraft clock types.
      !>     However, direct use of SCEC01 by user routines is not prohibited.
      !>
      !>## Examples
      !>
      !>     1)  Converting ET to encoded type 1 SCLK:
      !>
      !>         During program initialization, load the leapseconds and SCLK
      !>         kernels. We will assume that these files are named
      !>         "LEAPSECONDS.KER" and "SCLK.KER".  You must substitute the
      !>         actual names of these files in your code.
      !>
      !>            CALL CLPOOL
      !>            CALL FURNSH ( 'LEAPSECONDS.KER' )
      !>            CALL FURNSH ( 'SCLK.KER'        )
      !>
      !>         If SC is -77, indicating the Galileo spacecraft, and
      !>         ET is set to
      !>
      !>            -27848635.8149248
      !>
      !>         then the call
      !>
      !>            CALL SCEC01 ( SC, ET, SCLKDP )
      !>
      !>         returns SCLKDP as
      !>
      !>            35425287435.8554
      !>
      !>         on a NeXT workstation running NEXTSTEP 3.3.
      !>
      !>## Restrictions
      !>
      !>     1)  An SCLK kernel appropriate to the spacecraft clock identified
      !>         by SC must be loaded at the time this routine is called.
      !>
      !>     2)  If the SCLK kernel used with this routine does not map SCLK
      !>         directly to barycentric dynamical time, a leapseconds kernel
      !>         must be loaded at the time this routine is called.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>     B.V. Semenov       (JPL)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 2.0.0, 01-DEC-2021 (NJB) (JDR)
      !>
      !>        Updated to support buffering of data for multiple clocks.
      !>        This entry point tracks kernel pool changes but no longer
      !>        sets or uses watches.
      !>
      !>        A check for invalid time system code was added.
      !>
      !>        Edited the header to comply with NAIF standard.
      !>
      !>        Corrected description of invalid clock rate exception.
      !>
      !>-    SPICELIB Version 1.4.0, 09-SEP-2013 (BVS)
      !>
      !>        Updated to keep track of the POOL counter and call ZZCVPOOL.
      !>
      !>-    SPICELIB Version 1.3.0, 05-MAR-2009 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to this routine.
      !>
      !>-    SPICELIB Version 1.2.0, 09-NOV-2007 (NJB)
      !>
      !>        Bug fix: this routine now keeps track of whether its
      !>        kernel pool look-up succeeded. If not, a kernel pool
      !>        lookup is attempted on the next call to this routine.
      !>
      !>-    SPICELIB Version 1.1.0, 09-NOV-2007 (NJB)
      !>
      !>        Bug fix: changed maximum value arguments to 1 in
      !>        calls to SCLI01 to fetch NFIELD and DELCDE values.
      !>
      !>-    SPICELIB Version 1.0.0, 13-FEB-1999 (NJB)
      !>
      !>## Revisions
      !>
      !>-    SPICELIB Version 1.1.0, 09-NOV-2007 (NJB)
      !>
      !>        Bug fix: changed maximum value arguments to 1 in
      !>        calls to SCLI01 to fetch NFIELD and DELCDE values.
      !>
      !>     Standard SPICE error handling.
      module subroutine scec01 ( ctx, sc, et, sclkdp )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      use pepper_inc_zzsc01
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft ID code.
      real(wp), intent(out) :: sclkdp  !! Type 1 SCLK, encoded as continuous ticks since clock start.
      real(wp), intent(in) :: et  !! Ephemeris time, seconds past J2000.
      character(1), parameter :: del    ( ndelim ) = &
         [ character(1) :: '.', ':', '-', ',', ' ' ]
      real(wp) :: timdif
      real(wp) :: const
      real(wp) :: tikmsc
      real(wp) :: rate
      integer(ip) :: i
      integer(ip) :: upper
      real(wp) :: mxtick
      integer(ip) :: middle
      if ( .not. allocated ( ctx%sc01_state ) ) then
         allocate ( ctx%sc01_state )
         call init_sc01_state ( ctx%sc01_state )
      end if
!
!     PEPPER: make the clock's data current before the ASSOCIATE block
!     (the database may grow).
!
      if ( ctx%failed() ) return
      if ( ctx%sc01_state%pass1 ) then
         call zzscin01 ( ctx )
         call zzctruin ( ctx, ctx%sc01_state%polctr )
         ctx%sc01_state%pass1 = .false.
      end if
      call zzscup01 ( ctx, sc )
      if ( ctx%failed() ) then
         ctx%sc01_state%pass1 = .true.
         return
      end if
      associate ( scbase => ctx%sc01_state%scbase, &
                  dpbuff => ctx%sc01_state%dpbuff, &
                  partim => ctx%sc01_state%partim, &
                  cofbas => ctx%sc01_state%cofbas, &
                  delcde => ctx%sc01_state%delcde, &
                  dpfree => ctx%sc01_state%dpfree, &
                  end => ctx%sc01_state%end, &
                  endbas => ctx%sc01_state%endbas, &
                  ifree => ctx%sc01_state%ifree, &
                  intbuf => ctx%sc01_state%intbuf, &
                  lower => ctx%sc01_state%lower, &
                  modbas => ctx%sc01_state%modbas, &
                  ncoeff => ctx%sc01_state%ncoeff, &
                  nfield => ctx%sc01_state%nfield, &
                  npart => ctx%sc01_state%npart, &
                  offbas => ctx%sc01_state%offbas, &
                  polctr => ctx%sc01_state%polctr, &
                  prvsc => ctx%sc01_state%prvsc, &
                  strbas => ctx%sc01_state%strbas, &
                  timsys => ctx%sc01_state%timsys, &
                  pass1 => ctx%sc01_state%pass1 )
      if ( ctx%failed() ) return

!
!     Get parameters and data for this clock from the type 1 database.
!     Update the database if necessary.
!

!
!     Convert the input TDB time to the parallel time system, if the
!     parallel system is not TDB.
!
!     We don't need to check the validity of TIMSYS, because SCLI01
!     already made this check.
!
      if ( timsys == tdb ) then

         partim = et

      else if ( timsys == tdt ) then

         partim = unitim ( ctx, et, 'TDB', 'TDT' )

         if ( ctx%failed() ) return

      else
!
!        This code should be unreachable. It's present for safety.
!
         pass1 = .true.

         call ctx%setmsg ( 'Invalid time system code # was found for ' &
         //            'SCLK #.'                                   )
         call ctx%errint ( '#', timsys                                 )
         call ctx%errint ( '#', sc                                     )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)'                    )
         return

      end if

!
!     We'd like to ascertain whether PARTIM is between the minimum
!     time value in the coefficients array and the end time
!     corresponding to the number of ticks since spacecraft clock
!     start at the end of the last partition.
!
!     Checking the time value is a special case; we'll convert the time
!     value to ticks, and then check whether the resulting value is
!     less than the total number of ticks since spacecraft clock start
!     at the end of the last partition.  So, this check is performed
!     at the end of the routine.
!
!     Find the time value in COEFFS closest to the input time value.
!     The time values are ordered, so we can do a binary search for the
!     closest one.  When the search is done, we will have found the
!     index of the greatest time value in the coefficient array that
!     is less than or equal to PARTIM.
!
!     There are two cases:
!
!        1) PARTIM is bounded by the least and greatest time
!           coefficients in the array.  In this case, we must search
!           the array for a consecutive pair of records whose time
!           values bound PARTIM.
!
!        2) PARTIM is greater than or equal to all of the time
!           coefficients.  In that case, we don't need to search:  the
!           last time value in the array is the one we want.
!
      if (  partim  <  ( dpbuff( cofbas + 2 + (1-1)*3 ) )  ) then
!
!        PARTIM precedes the coverage of the kernel.
!
         pass1 = .true.

         call ctx%setmsg (  bvlmsg                  )
         call ctx%errch  ( '#', 'ET'                )
         call ctx%errdp  ( '#',  et                 )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)' )
         return

      else if (  partim < ( dpbuff( cofbas + 2 + ((ncoeff/3)-1)*3 ) )  ) then
!
!        In the following loop, we maintain an invariant:
!
!           COEFFS( 2, LOWER )  <   PARTIM   <   COEFFS( 2, UPPER )
!                               -
!
!        At each step, we decrease the distance between LOWER and
!        UPPER, while keeping the above statement true.  The loop
!        terminates when LOWER = UPPER - 1.
!
!        Note that we start out with if LOWER < UPPER, since we've
!        already made sure that the invariant expression above is true.
!

         lower  =  1
         upper  =  ncoeff / 3

         do while ( lower < upper - 1 )

            middle  =  ( lower + upper ) / 2

            if (  partim  <  ( dpbuff( cofbas + 2 + (middle-1)*3 ) )  ) then
               upper = middle
            else
               lower = middle
            end if

         end do

!
!        We've got PARTIM trapped between two time values that are
!        `adjacent' in the list:
!
!           COEFFS ( 2, LOWER )  and
!           COEFFS ( 2, UPPER )
!
!        since the second value must be greater than the first.  So
!
!           COEFFS( 2, LOWER )
!
!        is the last time value in the coefficients array less than or
!        equal to PARTIM.
!

      else
!
!        PARTIM is greater than or equal to all of the time values in
!        the coefficients array.
!
         lower = ncoeff / 3

      end if

!
!     Now we evaluate a linear polynomial to find the tick value that
!     corresponds to PARTIM.  The coefficients of the polynomial are
!     the tick value and rate (in units of ticks per second) that
!     correspond to the time value
!
!        COEFFS( 2, LOWER )
!
!     We call these coefficients CONST and RATE.  The rates in the
!     coefficients array are in units of seconds per most significant
!     clock count, so we use the conversion factor TIKMSC (`ticks per
!     most significant count') to change the rate to seconds per tick.
!
      timdif =  partim  -  ( dpbuff( cofbas + 2 + (lower-1)*3 ) )
      const  =             ( dpbuff( cofbas + 1 + (lower-1)*3 ) )

      if (  ( dpbuff( cofbas + 3 + (lower-1)*3 ) ) <= 0.0_wp  ) then

         pass1 = .true.

         call ctx%setmsg ( 'Invalid SCLK rate #.'   )
         call ctx%errdp  ( '#', ( dpbuff( cofbas + 3 + (lower-1)*3 ) )  )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)' )
         return

      end if

      tikmsc = 1.0_wp

      do i = nfield, 2, -1
         tikmsc  =  tikmsc * ( dpbuff( modbas + i ) )
      end do

      rate   =  1.0_wp   /  (  ( dpbuff( cofbas + 3 + (lower-1)*3 ) ) / tikmsc  )

      sclkdp =  const  +  ( rate * timdif )

!
!     Now, we'll see whether the SCLK value we've found is meaningful.
!     If it's too large, that's because the input PARTIM was beyond the
!     maximum value we can handle.  To check whether PARTIM is in
!     range, we must find the end time of the last partition, in total
!     ticks since spacecraft clock start.
!
      mxtick  =  anint ( ( dpbuff( endbas + 1 ) ) - ( dpbuff( strbas + 1 ) ) )

      do i = 2, npart
         mxtick  =  anint ( ( dpbuff( endbas + i ) ) -  ( dpbuff( strbas + i ) )  +  mxtick )
      end do

!
!     Make sure that ET does not exceed the ET corresponding to
!     the clock's maximum tick value.  We'll do the comparison
!     using the tick value that ET mapped to and the maximum tick
!     value of the spacecraft clock.
!
      if ( sclkdp > mxtick ) then

         pass1 = .true.

         call ctx%setmsg (  bvlmsg                  )
         call ctx%errch  ( '#', 'ET'                )
         call ctx%errdp  ( '#',  et                 )
         call ctx%sigerr ( 'SPICE(VALUEOUTOFRANGE)' )
         return

      end if

!
!     Keep track of the last spacecraft clock ID encountered.
!
      prvsc = sc

      return
      end associate
      end subroutine scec01

      !>## Detailed Input
      !>
      !>     SC       is a NAIF ID code for a spacecraft clock.
      !>
      !>## Detailed Output
      !>
      !>     CLKTYP   is the integer spacecraft clock type. The type
      !>              need not be 1.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If a kernel variable identifying requested SCLK type is not
      !>         found, the error SPICE(KERNELVARNOTFOUND) is signaled.
      !>
      !>     2)  If an error occurs in the course of updating the type 1 SCLK
      !>         database, the error is signaled by a routine in the call tree
      !>         of this routine.
      !>
      !>     3)  It is not an error to request the type of a clock that is not
      !>         type 1.
      !>
      !>## Files
      !>
      !>     Appropriate kernels must be loaded by the calling program before
      !>     this routine is called.
      !>
      !>     The following data are required:
      !>
      !>     -  An SCLK kernel providing data for the SCLK designated by
      !>        the input ID code SC.
      !>
      !>     In all cases, kernel data are normally loaded once per program
      !>     run, NOT every time this routine is called.
      !>
      !>## Particulars
      !>
      !>     SCLK "type" is a SPICE-specific concept. It indicates which
      !>     mechanism is used by SPICE for representing time as measured
      !>     by a specified clock, and for converting those time measurements
      !>     to standard time systems such as TDB.
      !>
      !>     This routine speeds up determination of a spacecraft clock's type
      !>     by querying the type 1 SCLK database for information about that
      !>     clock. If the clock is not found, the kernel pool is searched,
      !>     and if the clock is determined to be type 1, information about it
      !>     is stored in the type 1 database. If the clock is not type 1,
      !>     the type is returned, but the type 1 database is not modified.
      !>
      !>     This is the only entry point of SC01 for which it is valid to
      !>     supply the ID of a clock not of type 1.
      !>
      !>## Examples
      !>
      !>     See usage in the SPICELIB function SCTYPE.
      !>
      !>## Restrictions
      !>
      !>     1)  This is essentially a SPICE-private routine. It should not
      !>         be called directly by user application code.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB) (JDR)
      !>
      !>        New entry point.
      module subroutine scty01 ( ctx, sc, clktyp )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      use pepper_inc_zzsc01
      use pepper_strings, only: repmi_inplace
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft clock ID code.
      integer(ip), intent(out) :: clktyp  !! NAIF spacecraft clock type.
      integer(ip) :: clktyp_1(1)  !! one-element copy of clktyp (array argument)
      character(1), parameter :: del    ( ndelim ) = &
         [ character(1) :: '.', ':', '-', ',', ' ' ]
      integer(ip) :: n
      logical :: samclk
      logical :: update
      logical :: found
      character(kvnmln) :: kvname
      if ( .not. allocated ( ctx%sc01_state ) ) then
         allocate ( ctx%sc01_state )
         call init_sc01_state ( ctx%sc01_state )
      end if
      associate ( cofbas => ctx%sc01_state%cofbas, &
                  delcde => ctx%sc01_state%delcde, &
                  dpfree => ctx%sc01_state%dpfree, &
                  end => ctx%sc01_state%end, &
                  endbas => ctx%sc01_state%endbas, &
                  ifree => ctx%sc01_state%ifree, &
                  modbas => ctx%sc01_state%modbas, &
                  ncoeff => ctx%sc01_state%ncoeff, &
                  nfield => ctx%sc01_state%nfield, &
                  npart => ctx%sc01_state%npart, &
                  offbas => ctx%sc01_state%offbas, &
                  polctr => ctx%sc01_state%polctr, &
                  prvsc => ctx%sc01_state%prvsc, &
                  strbas => ctx%sc01_state%strbas, &
                  timsys => ctx%sc01_state%timsys, &
                  pass1 => ctx%sc01_state%pass1 )
      if ( ctx%failed() ) return

!
!     At this point, PRVSC is either 0 or the ID of the last type 1
!     clock for which a successful call to an entry point of SC01
!     was made. PRVSC is never assigned the code of a clock that isn't
!     type 1.
!
      if ( pass1 ) then
!
!        Initialize local data structures.
!
         call zzscin01 ( ctx )
         call zzctruin ( ctx, polctr )

         pass1 = .false.

      end if

!
!     The clock type is unknown at this point.
!
      clktyp = 0

!
!     Reinitialize the SC01 database if the kernel pool has been
!     updated. The call below syncs POLCTR.
!
      call zzpctrck ( ctx, polctr, update )

      if ( update ) then
!
!        Initialize local data structures.
!
         call zzscin01 ( ctx )

         samclk = .false.

      else
!
!        We never indicate the clock is the "same" as the previous
!        one unless the clock is type 1.
!
         samclk = ( sc /= 0 ) .and. ( sc == prvsc )

      end if

      if ( .not. samclk )  then
!
!        Get the SCLK type of the input SCLK.
!
         kvname = 'SCLK_DATA_TYPE_#'
         call repmi_inplace ( kvname,  '#',  -sc )

         call gipool ( ctx, kvname, 1, 1, n, clktyp_1, found )
         clktyp = clktyp_1(1)

         if ( ctx%failed() .or. .not. found ) then

            if ( ctx%failed() ) then

               pass1 = .true.

               return

            end if

            if ( .not. found ) then
!
!              The GIPOOL call succeeded but the kernel variable was
!              not found.
!
               pass1 = .true.

               call ctx%setmsg ( 'Kernel variable # was not ' &
               //            'found in the kernel pool.' )
               call ctx%errch  ( '#', kvname                 )
               call ctx%sigerr ( 'SPICE(KERNELVARNOTFOUND)'  )

               return

            end if

         end if

!
!        At this point, the clock type is set.
!
         if ( clktyp /= 1 ) then
!
!           The clock is not type 1, so we don't add it to the
!           SC01 database.
!
            prvsc = 0

            return

         end if

!
!        The SCLK is type 1 but may not be in the SC01 database, and
!        in any case is not the last type 1 clock seen on a successful
!        call to any SC01 entry point. Update the SCLK parameters and,
!        if necessary, the database.
!
         call zzscup01 ( ctx, sc )

         if ( ctx%failed() ) then

            pass1 = .true.

            return

         end if

      end if

!
!     We can arrive here only if the clock type is 1.
!
      clktyp = 1
      prvsc  = sc

      return
      end associate
      end subroutine scty01

      !>## Detailed Input
      !>
      !>     SC       is a NAIF ID code for a spacecraft clock.
      !>
      !>## Detailed Output
      !>
      !>     NPARTN   is the number of partitions for the clock
      !>              designated by SC.
      !>
      !>     PARBEG,
      !>     PAREND   are, respectively, arrays of partition begin and
      !>              end times for the clock designated by SC. The
      !>              times are expressed as encoded SCLK.
      !>
      !>## Parameters
      !>
      !>     None.
      !>
      !>## Exceptions
      !>
      !>     1)  If an error occurs in the course of updating the type 1 SCLK
      !>         database, the error is signaled by a
      !>         routine in the call tree of this routine.
      !>
      !>## Files
      !>
      !>     Appropriate kernels must be loaded by the calling program before
      !>     this routine is called.
      !>
      !>     The following data are required:
      !>
      !>     -  An SCLK kernel providing data for the SCLK designated by
      !>        the input ID code SC.
      !>
      !>     In all cases, kernel data are normally loaded once per program
      !>     run, NOT every time this routine is called.
      !>
      !>## Particulars
      !>
      !>     None.
      !>
      !>## Examples
      !>
      !>     See usage in the SPICELIB function SCPART.
      !>
      !>## Restrictions
      !>
      !>     1)  This is essentially a SPICE-private routine. It should not
      !>         be called directly by user application code.
      !>
      !>## Literature References
      !>
      !>     None.
      !>
      !>## Author and Institution
      !>
      !>     N.J. Bachman       (JPL)
      !>     J. Diaz del Rio    (ODC Space)
      !>
      !>## Version
      !>
      !>-    SPICELIB Version 1.0.0, 01-DEC-2021 (NJB) (JDR)
      !>
      !>        New entry point.
      module subroutine scpr01 ( ctx, sc, npartn, parbeg, parend )
      use pepper_kinds, only: ip, wp
      use pepper_inc_zzctr
      use pepper_inc_zzsc01
      implicit none
      class(pepper_context), intent(inout) :: ctx  !! the context
      integer(ip), intent(in) :: sc  !! NAIF spacecraft clock ID code.
      integer(ip), intent(out) :: npartn  !! Number of partitions.
      real(wp), intent(out) :: parbeg ( * )  !! Partition begin times.
      real(wp), intent(out) :: parend ( * )  !! Partition end times.
      character(1), parameter :: del    ( ndelim ) = &
         [ character(1) :: '.', ':', '-', ',', ' ' ]
      if ( .not. allocated ( ctx%sc01_state ) ) then
         allocate ( ctx%sc01_state )
         call init_sc01_state ( ctx%sc01_state )
      end if
!
!     PEPPER: make the clock's data current before the ASSOCIATE block
!     (the database may grow).
!
      if ( ctx%failed() ) return
      if ( ctx%sc01_state%pass1 ) then
         call zzscin01 ( ctx )
         call zzctruin ( ctx, ctx%sc01_state%polctr )
         ctx%sc01_state%pass1 = .false.
      end if
      call zzscup01 ( ctx, sc )
      if ( ctx%failed() ) then
         ctx%sc01_state%pass1 = .true.
         return
      end if
      associate ( scbase => ctx%sc01_state%scbase, &
                  dpbuff => ctx%sc01_state%dpbuff, &
                  cofbas => ctx%sc01_state%cofbas, &
                  delcde => ctx%sc01_state%delcde, &
                  dpfree => ctx%sc01_state%dpfree, &
                  end => ctx%sc01_state%end, &
                  endbas => ctx%sc01_state%endbas, &
                  ifree => ctx%sc01_state%ifree, &
                  intbuf => ctx%sc01_state%intbuf, &
                  modbas => ctx%sc01_state%modbas, &
                  ncoeff => ctx%sc01_state%ncoeff, &
                  nfield => ctx%sc01_state%nfield, &
                  npart => ctx%sc01_state%npart, &
                  offbas => ctx%sc01_state%offbas, &
                  polctr => ctx%sc01_state%polctr, &
                  prvsc => ctx%sc01_state%prvsc, &
                  strbas => ctx%sc01_state%strbas, &
                  timsys => ctx%sc01_state%timsys, &
                  pass1 => ctx%sc01_state%pass1 )
      if ( ctx%failed() ) return

!
!     Get partition data for this clock. Update the type 1 database
!     if necessary.
!

!
!     Set the output partition arguments.
!
      parbeg(1:npart) = dpbuff(strbas+1:strbas+1+npart-1)
      parend(1:npart) = dpbuff(endbas+1:endbas+1+npart-1)

      npartn = npart

      prvsc  = sc

      return
      end associate
      end subroutine scpr01

end submodule pepper_sclk
