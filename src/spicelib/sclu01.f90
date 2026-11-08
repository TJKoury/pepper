!$Procedure SCLU01 ( SCLK look up, type 1 )

      SUBROUTINE SCLU01 (  NAME,  SC,  MAXNV,  N,  IVAL,  DVAL )

! $ Abstract
!
!     Look up type 1 SCLK kernel data.
!
! $ Required_Reading
!
!     KERNEL
!     SCLK
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'sclk.inc'

      CHARACTER*(*)         NAME
      INTEGER               SC
      INTEGER               MAXNV
      INTEGER               N
      INTEGER               IVAL ( * )
      DOUBLE PRECISION      DVAL ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  ENTRY POINTS
!     --------  ---  --------------------------------------------------
!     NAME       I   SCLD01, SCLI01
!     SC         I   SCLD01, SCLI01
!     MAXNV      I   SCLD01, SCLI01
!     N          O   SCLD01, SCLI01
!     IVAL       O   SCLI01
!     DVAL       O   SCLD01
!     MXCOEF     P   SCLD01, SCLI01
!     MXPART     P   SCLD01, SCLI01
!     MXNFLD     P   SCLD01, SCLI01
!     NDELIM     P   SCLI01
!     MXTSYS     P   SCLI01
!
! $ Detailed_Input
!
!     See entry points SCLI01, SCLD01.
!
! $ Detailed_Output
!
!     See entry points SCLI01, SCLD01.
!
! $ Parameters
!
!     See the INCLUDE file sclk.inc for descriptions and values
!     of the global parameters used by this routine and
!     its entry points.
!
! $ Exceptions
!
!     1)  If SCLU01 is called directly, the error SPICE(BOGUSENTRY) is
!         signaled.
!
!     See entry points SCLI01, SCLD01 for descriptions of exceptions
!     specific to those routines.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is a utility whose purpose is to localize error
!     checking for type 1 SCLK kernel pool lookups in a single place.
!
!     SLCU01 exists solely as an umbrella routine in which the
!     variables for its entry points are declared.  SCLU01 should never
!     be called directly.
!
! $ Examples
!
!     See entry points SCLI01, SCLD01.
!
! $ Restrictions
!
!     1)  SCLU01 handles lookups of type 1 SCLK data only.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR)
!
!        New checks on item sizes have been added: sizes are now
!        compared against upper bounds as well as lower bounds.
!        Previously only lower bounds were used.
!
!        Bug fix: corrected index error in SCLD01 modulus range check.
!        Corrected comments about count checks in SCLI01. Made
!        cosmetic changes to code and comments in both SCLD01 and
!        SCLI01, and in this routine. Deleted unused parameter MXNCFF.
!
!        Edited the umbrella routine and all its entry points headers
!        to comply with NAIF standard.
!
!-    SPICELIB Version 2.3.0, 05-FEB-2008 (NJB)
!
!        Values of parameters
!
!           MXCOEF, MXPART, MXNFLD, NDELIM, MXTSYS
!
!        are now provided by the INCLUDE file sclk.inc.
!
!-    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW)
!
!        Entry points SCLI01 and SCLD01 were update to use kernel pool
!        fetch routines GIPOOL and GDPOOL respectively. Formerly these
!        entry points called the deprecated routine RTPOOL.
!
!        All headers have been updated to remove warnings about memory
!        corruption that could occur due to use of RTPOOL.
!
!        Header references to LDPOOL were replaced with references to
!        FURNSH.
!
!-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
!
!        Entry points SCLI01 and SCLD01 were updated to fix a bug:
!        if a kernel pool lookup fails, the number of elements returned
!        N is now set to zero.
!
!-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
!
!        Entry point SCLI01 was updated to handle a time
!        system specification for the `parallel' time system
!        in the SCLK kernel. Comment section for permuted index
!        source lines was added following the header.
!
!-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     lookup type_1 spacecraft_clock
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
!
!        Entry points SCLI01 and SCLD01 were updated to fix a bug:
!        if a kernel pool lookup fails, the number of elements returned
!        N is now set to zero. Formerly, these routines returned
!        whatever value was returned by RTPOOL.  RTPOOL, however,
!        does not set N to zero when the data item requested from it
!        is not found.
!
!-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
!
!        Entry point SCLI01 was updated to handle a time
!        system specification for the `parallel' time system
!        in the SCLK kernel. The update consists of these
!        changes:
!
!           -- The parameter MXTSYS is now defined.
!
!           -- The local saved variable NAMLST has been expanded
!              to include the name SCLK01_TIME_SYSTEM
!
!           -- The local saved variable LB has been expanded to
!              include the lower bound for the number of returned
!              values when SCLK01_TIME_SYSTEM_nn is looked up in
!              the kernel pool.
!
!           -- SCLI01 checks the value returned by RTPOOL when
!              SCLK01_TIME_SYSTEM_nn is looked up to verify that
!              it is within the range [1, MXTSYS].
!
!        Also, a comment section for permuted index source lines was
!        added following the header.
!
!-&


!
!     SPICELIB functions
!
      INTEGER               ISRCHC

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN =        320 )

      INTEGER               KVNMLN
      PARAMETER           ( KVNMLN =         32 )

!
!     MXCOFA is the maximum size of the coefficient array.
!
      INTEGER               MXCOFA
      PARAMETER           ( MXCOFA = 3 * MXCOEF )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN =         80 )

      INTEGER               NKITEM
      PARAMETER           ( NKITEM =          9 )

!
!     COFIDX is the index of the coefficient name in NAMLST.
!
      INTEGER               COFIDX
      PARAMETER           ( COFIDX =  1 )

!
!     NFLIDX is the index of the SCLK field count in NAMLST.
!
      INTEGER               NFLIDX
      PARAMETER           ( NFLIDX =  4 )

!
!     OFFIDX is the index of the SCLK offsets in NAMLST.
!
      INTEGER               OFFIDX
      PARAMETER           ( OFFIDX =  5 )

!
!     MODIDX is the index of the SCLK moduli in NAMLST.
!
      INTEGER               MODIDX
      PARAMETER           ( MODIDX =  6 )

!
!     DELIDX is the index of the delimiter code name in NAMLST.  If
!     the declaration of NAMLST or assignment of values to NAMLST
!     changes, this parameter value may have to change.
!
      INTEGER               DELIDX
      PARAMETER           ( DELIDX =  7 )

!
!     SYSIDX is the index of the time system in NAMLST.
!
      INTEGER               SYSIDX
      PARAMETER           ( SYSIDX =  9 )


!
!     Local variables
!
      CHARACTER*(MSGLEN)    BVLMSG
      CHARACTER*(MSGLEN)    ERRMSG
      CHARACTER*(KVNMLN)    KVNAME
      CHARACTER*(MSGLEN)    NFDMSG
      CHARACTER*(MSGLEN)    NUMMSG
      CHARACTER*(NAMLEN)    NAMLST ( NKITEM )
      CHARACTER*(NAMLEN)    TMPNAM
      CHARACTER*(1)         TYPE

      INTEGER               I
      INTEGER               LB     ( NKITEM )
      INTEGER               NF
      INTEGER               NFIELD
      INTEGER               UB     ( NKITEM )

      LOGICAL               FOUND


!
!     Saved variables
!
      SAVE                  BVLMSG
      SAVE                  LB
      SAVE                  NAMLST
      SAVE                  NFDMSG
      SAVE                  NUMMSG
      SAVE                  UB

!
!     Initial values
!

!
!     Names of type 1 SCLK items and lower bounds on the number of
!     associated values.
!
      DATA (   NAMLST(I),            LB(I),  UB(I),   I = 1, NKITEM ) / &

            'SCLK01_COEFFICIENTS',      3,   MXCOFA, &
            'SCLK_PARTITION_START',     1,   MXPART, &
            'SCLK_PARTITION_END',       1,   MXPART, &
            'SCLK01_N_FIELDS',          1,   1, &
            'SCLK01_OFFSETS',           1,   MXNFLD, &
            'SCLK01_MODULI',            1,   MXNFLD, &
            'SCLK01_OUTPUT_DELIM',      1,   1, &
            'SCLK01_KERNEL_ID',         1,   1, &
            'SCLK01_TIME_SYSTEM',       0,   1                        /


      DATA NFDMSG / '# not found. Did you load the SCLK kernel?'      /

      DATA NUMMSG / '# values found for #: valid range is #:#.'       /

      DATA BVLMSG / 'Invalid value found for #:  #.'                  /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SCLU01' )

      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )

      CALL CHKOUT ( 'SCLU01' )
      RETURN


!$Procedure SCLI01 ( SCLK lookup of integer data, type 1 )

      ENTRY SCLI01 ( NAME, SC, MAXNV, N, IVAL )

! $ Abstract
!
!     Look up integer type 1 SCLK data from the kernel pool.
!
! $ Required_Reading
!
!     KERNEL
!     SCLK
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               SC
!     INTEGER               MAXNV
!     INTEGER               N
!     INTEGER               IVAL   ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME,
!     SC         I   Name of kernel data item, NAIF spacecraft ID code.
!     MAXNV      I   Maximum number of integer values to return.
!     N          O   Number of values actually returned.
!     IVAL       O   Returned integer values.
!     MXNFLD     P   Maximum number of fields in an SCLK string.
!     NDELIM     P   Maximum number of delimiter codes.
!     MXTSYS     P   Maximum number of supported parallel time systems.
!
! $ Detailed_Input
!
!     NAME,
!     SC       are, respectively, a name and a NAIF integer code
!              of a spacecraft that together define the name of a
!              requested kernel data item. NAME is the full name
!              as it appears in the SCLK kernel, except that it
!              lacks the final underscore and spacecraft integer
!              code (actually, the negative of the spacecraft
!              code).  This routine combines NAME and SC to
!              make up the appropriate kernel variable name.
!
!              For example, to look up data associated with the
!              name
!
!                 SCLK01_N_FIELDS_77
!
!              you would supply NAME as
!
!                 SCLK01_N_FIELDS
!
!              and SC as -77.
!
!
!     MAXNV    is the maximum number of values to return.  MAXNV
!              is used to prevent SCLI01 from writing past the end
!              of the supplied array IVAL.
!
! $ Detailed_Output
!
!     N        is the number of values actually returned.
!
!     IVAL     is an array containing the requested integer
!              kernel data item.
!
! $ Parameters
!
!     MXNFLD   is an upper bound on the number of fields in a
!              SCLK string.
!
!     NDELIM   is the number of delimiter codes.
!
!     MXTSYS   is the maximum number of supported parallel time
!              systems that SCLK values may be mapped to or from.
!
! $ Exceptions
!
!     1)  If item specified by NAME and SC is not found in the kernel
!         pool, and if the presence of the item is required, the error
!         SPICE(KERNELVARNOTFOUND) is signaled. The output arguments
!         are not modified.
!
!         If the specified item is not required, the output argument N
!         will take the value 0, and the output argument IVAL is not
!         modified.
!
!     2)  If the item specified by NAME and SC is found but does not
!         have numeric type, the error SPICE(BADKERNELVARTYPE) is
!         signaled.
!
!     3)  This routine can check certain data for validity. If any of
!         these items have invalid values, the error
!         SPICE(VALUEOUTOFRANGE) is signaled. The output arguments are
!         not modified. The values in question are:
!
!            -  The number of fields of a SCLK string
!            -  The number of delimiter codes
!            -  The output delimiter code
!            -  The time system code
!
!     4)  If the dimension of the requested item exceeds MAXNV, the
!         error SPICE(ARRAYTOOSMALL) is signaled.
!
!     5)  If the dimension of the requested item is outside of the
!         limits for that item, the error SPICE(INVALIDSIZE) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The purpose of this routine is to localize error checking for
!     lookups of type 1 SCLK kernel pool data. This routine handles
!     lookups of integer data.
!
! $ Examples
!
!     1)  To get the number of SCLK fields for the Galileo spacecraft
!         clock, you can use the code fragment below:
!
!            C
!            C     Load the SCLK kernel in question. We use a
!            C     made-up name for the kernel file; you would use
!            C     the actual name of your kernel file instead if you
!            C     were to carry out this procedure.
!            C
!                  CALL FURNSH ( 'SAMPLE_GLL_SCLK.KER' )
!
!                  SC   = -77
!                  NAME = 'SCLK01_N_FIELDS'
!
!                  CALL SCLI01 ( NAME, SC, MXNFLD, N, NFIELD )
!
!
!         After this subroutine call, NFIELD has the value 4.
!
! $ Restrictions
!
!     1)  SCLI01 assumes that a SCLK kernel appropriate to the
!         spacecraft identified by SC has been loaded.
!
!     2)  SCLI01 handles lookups of type 1 SCLK data only.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR)
!
!        New checks on item sizes have been added: sizes are now
!        compared against upper bounds as well as lower bounds.
!        Previously only lower bounds were used.
!
!        Corrected comments about count checks. Made cosmetic changes
!        to code and comments.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW)
!
!        Routine was updated to use GIPOOL instead of RTPOOL. Header
!        has been updated to remove warnings about memory corruption and
!        to document exception handling for output buffer overflow
!        errors.
!
!        Header references to LDPOOL were replaced with references to
!        FURNSH.
!
!-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
!
!        This entry point was updated to fix a bug: if a kernel pool
!        lookup fails, the number of elements returned N is now set to
!        zero.
!
!-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
!
!        SCLI01 was updated to handle a time system specification for
!        the `parallel' time system in the SCLK kernel. Some
!        corrections and other minor enhancements were made to the
!        header. Comment section for permuted index source lines was
!        added following the header.
!
!-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     lookup of type_1 spacecraft_clock integer data
!     lookup type_1 spacecraft_clock integer data
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
!
!        This entry point was updated to fix a bug: if a kernel pool
!        lookup fails, the number of elements returned N is now set to
!        zero. Formerly, this routine returned whatever value was
!        returned by RTPOOL.  RTPOOL, however, does not set N to zero
!        when the data item requested from it is not found.
!
!-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
!
!        Entry point SCLI01 was updated to handle a time
!        system specification for the `parallel' time system
!        in the SCLK kernel. The update consists of these
!        changes:
!
!           -- The parameter MXTSYS is now defined.
!
!           -- The local saved variable NAMLST has been expanded
!              to include the name SCLK01_TIME_SYSTEM
!
!           -- The local saved variable LB has been expanded to
!              include the lower bound for the number of returned
!              values when SCLK01_TIME_SYSTEM_nn is looked up in
!              the kernel pool.
!
!           -- SCLI01 checks the value returned by RTPOOL when
!              SCLK01_TIME_SYSTEM_nn is looked up to verify that
!              it is within the range [1, MXTSYS].
!
!        Also, a comment section for permuted index source lines was
!        added following the header.
!
!        The $Exceptions header section was updated accordingly.
!
!        Some corrections and other minor enhancements were made to the
!        header.
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SCLI01' )

!
!     Form the name of the kernel pool data item, and do the lookup.
!     Note that eventually we should use a kernel pool lookup entry
!     that allows us to specify the maximum number of entries that
!     can be returned.
!
      TMPNAM = NAME

      CALL SUFFIX ( '_#',    0,       TMPNAM )
      CALL REPMI  ( TMPNAM, '#', -SC, TMPNAM )

!
!     Make sure we have enough room for the item in our output
!     array. Look up the dimension of the item.
!
      CALL DTPOOL ( TMPNAM, FOUND, N, TYPE )

      IF ( N .GT. MAXNV ) THEN

         CALL SETMSG ( 'Item # for SCLK # has size # but ' &
         //            'output array has size #.'         )
         CALL ERRCH  ( '#',  TMPNAM                       )
         CALL ERRINT ( '#',  SC                           )
         CALL ERRINT ( '#',  N                            )
         CALL ERRINT ( '#',  MAXNV                        )
         CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'             )
         CALL CHKOUT ( 'SCLI01'                           )
         RETURN

      END IF

!
!     Check the data type of the variable.
!
      IF (  FOUND  .AND.  ( TYPE .NE. 'N' )  ) THEN

         CALL SETMSG ( 'Kernel variable # for spacecraft ' &
         //            'clock # does not have numeric type.' )
         CALL ERRCH  ( '#', TMPNAM                           )
         CALL ERRINT ( '#', SC                               )
         CALL SIGERR ( 'SPICE(BADKERNELVARTYPE)'             )
         CALL CHKOUT ( 'SCLI01'                              )
         RETURN

      END IF

      CALL GIPOOL ( TMPNAM, 1, MAXNV, N, IVAL, FOUND )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'SCLI01' )
         RETURN
      END IF

!
!     Make sure we found what we were looking for, if the item
!     is required.
!
      IF ( .NOT. FOUND ) THEN
!
!        Currently, the only item that is NOT required is the time
!        system specification.  In any case, no values will be returned.
!
         N = 0

         IF (  NAME .EQ. NAMLST(SYSIDX)  ) THEN

            CALL CHKOUT ( 'SCLI01' )
            RETURN

         ELSE

            CALL SETMSG (  NFDMSG                    )
            CALL ERRCH  ( '#', TMPNAM                )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)' )
            CALL CHKOUT ( 'SCLI01'                   )
            RETURN

         END IF

      END IF

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
      I = ISRCHC ( NAME, NKITEM, NAMLST )

      IF ( I .NE. 0 ) THEN

         IF (  (  N .LT. LB(I) ) .OR. ( N .GT. UB(I) )  ) THEN

            CALL REPMI  (  NUMMSG, '#', N,      ERRMSG )
            CALL REPMC  (  ERRMSG, '#', TMPNAM, ERRMSG )
            CALL REPMI  (  ERRMSG, '#', LB(I),  ERRMSG )
            CALL REPMI  (  ERRMSG, '#', UB(I),  ERRMSG )
            CALL SETMSG (  ERRMSG                      )
            CALL SIGERR ( 'SPICE(SIZEOUTOFRANGE)'      )
            CALL CHKOUT ( 'SCLI01'                     )
            RETURN

         END IF

      END IF

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
      IF ( NAME .EQ. NAMLST(DELIDX) ) THEN

         IF (  ( IVAL(1) .LT. 1 ) .OR. ( IVAL(1) .GT. NDELIM )  ) THEN

            CALL REPMC  (  BVLMSG, '#', TMPNAM,  ERRMSG )
            CALL REPMI  (  ERRMSG, '#', IVAL(1), ERRMSG )
            CALL SETMSG (  ERRMSG                       )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'      )
            CALL CHKOUT ( 'SCLI01'                      )
            RETURN

         END IF

      END IF


      IF ( NAME .EQ. NAMLST(NFLIDX) ) THEN

         IF (  ( IVAL(1) .LT. 1 ) .OR. ( IVAL(1) .GT. MXNFLD )  ) THEN

            CALL REPMC  (  BVLMSG, '#', TMPNAM,  ERRMSG )
            CALL REPMI  (  ERRMSG, '#', IVAL(1), ERRMSG )
            CALL SETMSG (  ERRMSG                       )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'      )
            CALL CHKOUT ( 'SCLI01'                      )
            RETURN

         END IF

      END IF


      IF ( NAME .EQ. NAMLST(SYSIDX) ) THEN

         IF (  ( IVAL(1) .LT. 1 ) .OR. ( IVAL(1) .GT. MXTSYS )  ) THEN

            CALL REPMC  (  BVLMSG, '#', TMPNAM,  ERRMSG )
            CALL REPMI  (  ERRMSG, '#', IVAL(1), ERRMSG )
            CALL SETMSG (  ERRMSG                       )
            CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'      )
            CALL CHKOUT ( 'SCLI01'                      )
            RETURN

         END IF

      END IF

      CALL CHKOUT ( 'SCLI01' )
      RETURN


!$Procedure SCLD01 ( SCLK lookup of double precision data, type 1 )

      ENTRY SCLD01 ( NAME, SC, MAXNV, N, DVAL )

! $ Abstract
!
!     Look up double precision type 1 SCLK data from the kernel pool.
!
! $ Required_Reading
!
!     KERNEL
!     SCLK
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations
!
!     CHARACTER*(*)         NAME
!     INTEGER               SC
!     INTEGER               MAXNV
!     INTEGER               N
!     DOUBLE PRECISION      DVAL   ( * )
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME,
!     SC         I   Name of kernel data item, NAIF spacecraft ID code.
!     MAXNV      I   Maximum number of d.p. values to return.
!     N          O   Number of values actually returned.
!     DVAL       O   Requested kernel data item.
!     MXCOEF     P   Maximum number of coefficient sets in SCLK kernel.
!
! $ Detailed_Input
!
!     NAME,
!     SC       are, respectively, a name and a NAIF integer code
!              of a spacecraft that together define the name of a
!              requested kernel data item. NAME is the full name
!              as it appears in the SCLK kernel, except that it
!              lacks the final underscore and spacecraft integer
!              code (actually, the negative of the spacecraft
!              code).  This routine combines NAME and SC to
!              make up the appropriate kernel variable name.
!
!              For example, to look up data associated with the
!              name
!
!                 SCLK01_COEFFICIENTS_77
!
!              you would supply NAME as
!
!                 SCLK01_COEFFICIENTS
!
!              and SC as -77.
!
!
!     MAXNV    is the maximum number of values to return.  MAXNV
!              is used to prevent SCLD01 from writing past the end
!              of the supplied array DVAL.
!
! $ Detailed_Output
!
!     N        is the number of values actually returned.
!
!     DVAL     is an array containing the requested double
!              precision kernel data item.
!
! $ Parameters
!
!     MXCOEF   is the maximum number of coefficient sets in the
!              array COEFFS that defines the mapping between
!              encoded type 1 SCLK and a parallel time system.
!              This array has dimension 3 x MXCOEF. The value of
!              MXCOEF may be increased as required.
!
! $ Exceptions
!
!     1)  If item specified by NAME and SC is not found in the kernel
!         pool, the error SPICE(KERNELVARNOTFOUND) is signaled. The
!         output arguments are not modified.
!
!     2)  If the item specified by NAME and SC is found but does not
!         have numeric type, the error SPICE(BADKERNELVARTYPE) is
!         signaled.
!
!     3)  This routine can check certain data for validity. If any of
!         these items have invalid values, the error
!         SPICE(VALUEOUTOFRANGE) is signaled. The output arguments are
!         not modified. The values in question are:
!
!            - The number of coefficients.
!            - The number of partition start values.
!            - The number of partition end values.
!            - The number of moduli.
!            - The values of the moduli (lower bounds)
!            - The number of offsets.
!            - The number of kernel identifiers.
!
!     4)  If the partition times or SCLK coefficients themselves
!         are invalid, this routine does nothing about it. It is
!         simply not possible to detect all of the possible errors
!         that these data may be subject to.
!
!     5)  If the dimension of the requested item exceeds MAXNV, the
!         error SPICE(ARRAYTOOSMALL) is signaled.
!
!     6)  If the dimension of the requested item is outside of the
!         limits for that item, the error SPICE(INVALIDSIZE) is
!         signaled.
!
!     7)  If the dimension of the coefficient kernel variable is
!         not a multiple of 3, the error SPICE(INVALIDSIZE) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The purpose of this routine is to localize error checking for
!     lookups of type 1 SCLK kernel pool data. This routine handles
!     lookups of double precision data.
!
! $ Examples
!
!     1)  Check a NAIF SCLK kernel for accuracy by converting the
!         encoded SCLK coefficients to strings with partition numbers
!         and converting the parallel times to UTC strings. Print out
!         the results in tabular form. In this example, the spacecraft
!         is Mars Observer, which has NAIF ID code -94. We could
!         make the program work for Galileo by using the NAIF ID code
!         -77 instead of -94.
!
!            C
!            C     Load the SCLK kernel in question, and also load
!            C     a leapseconds kernel. We use made-up names for the
!            C     kernel file; you would use the actual names of your
!            C     kernel files instead if you were to carry out this
!            C     procedure.
!            C
!                  CALL FURNSH ( 'SAMPLE_MO_SCLK.KER' )
!                  CALL FURNSH ( 'LEAPSECONDS.KER'    )
!
!                  CONAME =  SCLK01_COEFFICIENTS
!                  SC     =  -94
!
!            C
!            C     Grab the coefficients.
!            C
!                  CALL SCLD01 ( CONAME, SC, 3*MXCOEF, NCOEFF, COEFFS )
!
!            C
!            C     The SCLK coefficients are in the first row of the
!            C     coefficients array; the parallel times are in the
!            C     second. Since the parallel time system used for MO
!            C     is terrestrial dynamical time (TDT), we will convert
!            C     the parallel time values to ET (TDB) first and then
!            C     convert the resulting times to UTC.
!            C
!            C     In a more robust algorithm, we'd look up the parallel
!            C     time system code used in the SCLK kernel rather than
!            C     assume that it is a particular system. We omit this
!            C     check for simplicity.
!            C
!            C     We decode the SCLK coefficients using SCDECD. Write
!            C     out the results to a file we'll call COMPARE.DAT.
!            C
!                  OUTFIL = 'COMPARE.DAT'
!
!                  CALL WRLINE ( OUTFIL, '    SCLK               UTC' )
!                  CALL WRLINE ( OUTFIL, ' '                          )
!
!                  DO I = 1, NCOEFF / 3
!
!                     CALL SCDECD ( -94,  COEFF(1,I),  CLKSTR )
!            C
!            C        Convert the parallel time coefficients, which are
!            C        given in TDT, to ET. UNITIM returns this value.
!            C
!                     CALL ET2UTC ( UNITIM ( COEFF(2,I), 'TDT', 'TDB' ),
!                 .                 'D',
!                 .                  3,
!                 .                  UTC    )
!
!                     LINE = ' SCLK        UTC '
!
!                     CALL REPMC  ( LINE, 'SCLK', CLKSTR, LINE )
!                     CALL REPMC  ( LINE, 'UTC',  UTC,    LINE )
!
!                     CALL WRLINE ( OUTFIL, LINE )
!
!                  END DO
!
! $ Restrictions
!
!     1)  SCLD01 assumes that a SCLK kernel appropriate to the
!         spacecraft identified by SC has been loaded.
!
!     2)  SCLD01 handles lookups of type 1 SCLK data only.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.0, 01-DEC-2021 (NJB) (JDR)
!
!        New checks on item sizes have been added: sizes are now
!        compared against upper bounds as well as lower bounds.
!        Previously only lower bounds were used. A check has been
!        added to verify that the coefficient count is a multiple
!        of 3.
!
!        Bug fix: corrected index error in modulus range check. Made
!        cosmetic changes to code and comments.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.2.0, 20-NOV-2006 (NJB) (EDW)
!
!        Routine was updated to use GDPOOL instead of RTPOOL. Header
!        has been updated to remove warnings about memory corruption and
!        to document exception handling for output buffer overflow
!        errors.
!
!        Header references to LDPOOL were replaced with references to
!        FURNSH.
!
!-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
!
!        This entry point was updated to fix a bug: if a kernel pool
!        lookup fails, the number of elements returned N is now set to
!        zero.
!
!-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
!
!        One constant was changed in the code for clarity; no functional
!        change results from this. Some corrections and other minor
!        enhancements were made to the header. Comment section for
!        permuted index source lines was added following the header.
!
!-    SPICELIB Version 1.0.0, 06-SEP-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     lookup of type_1 spacecraft_clock d.p. data
!     lookup type_1 spacecraft_clock d.p. data
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.1.0, 19-OCT-1992 (NJB)
!
!        This entry point was updated to fix a bug: if a kernel pool
!        lookup fails, the number of elements returned N is now set to
!        zero. Formerly, this routine returned whatever value was
!        returned by RTPOOL.  RTPOOL, however, does not set N to zero
!        when the data item requested from it is not found.
!
!-    SPICELIB Version 2.0.0, 17-APR-1992 (NJB) (WLT)
!
!        The constant 1 was changed to 1.D0 in the test for the
!        validity of the moduli for a spacecraft clock. The change
!        was made simply for clarity.
!
!        Some corrections and other minor enhancements were made to the
!        header. Comment section for permuted index source lines was
!        added following the header.
!
!-&


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'SCLD01' )

!
!     Form the name of the kernel pool datum, and do the lookup.
!
      TMPNAM = NAME

      CALL SUFFIX ( '_#',    0,        TMPNAM )
      CALL REPMI  ( TMPNAM, '#', -SC,  TMPNAM )

!
!     Make sure we have enough room for the item in our output
!     array. Look up the dimension of the item.
!
      CALL DTPOOL ( TMPNAM,  FOUND,  N,  TYPE )

      IF ( N .GT. MAXNV ) THEN

         CALL SETMSG ( 'Item # has size # but output array ' &
         //            'has size #.'                         )
         CALL ERRCH  ( '#',  TMPNAM                          )
         CALL ERRINT ( '#',  N                               )
         CALL ERRINT ( '#',  MAXNV                           )
         CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                )
         CALL CHKOUT ( 'SCLD01'                              )
         RETURN

      END IF

!
!     Check the data type of the variable.
!
      IF (  FOUND  .AND.  ( TYPE .NE. 'N' )  ) THEN

         CALL SETMSG ( 'Kernel variable # for spacecraft ' &
         //            'clock # does not have numeric type.' )
         CALL ERRCH  ( '#', TMPNAM                           )
         CALL ERRINT ( '#', SC                               )
         CALL SIGERR ( 'SPICE(BADKERNELVARTYPE)'             )
         CALL CHKOUT ( 'SCLD01'                              )
         RETURN

      END IF

      CALL GDPOOL ( TMPNAM, 1, MAXNV, N, DVAL, FOUND )

      IF ( FAILED() ) THEN
!
!        This code should be unreachable but is provided for safety.
!
         CALL CHKOUT ( 'SCLD01' )
         RETURN
      END IF
!
!     Make sure we found what we were looking for.
!
      IF ( .NOT. FOUND ) THEN
!
!        No values are returned in this case.
!
         N = 0

         CALL SETMSG (  NFDMSG                    )
         CALL ERRCH  ( '#', TMPNAM                )
         CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)' )
         CALL CHKOUT ( 'SCLD01'                   )
         RETURN

      END IF

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
      I = ISRCHC ( NAME, NKITEM, NAMLST )

      IF ( I .NE. 0 ) THEN

         IF (  (  N .LT. LB(I) ) .OR. ( N .GT. UB(I) )  ) THEN

            CALL REPMI  (  NUMMSG, '#', N,      ERRMSG )
            CALL REPMC  (  ERRMSG, '#', TMPNAM, ERRMSG )
            CALL REPMI  (  ERRMSG, '#', LB(I),  ERRMSG )
            CALL REPMI  (  ERRMSG, '#', UB(I),  ERRMSG )
            CALL SETMSG (  ERRMSG                      )
            CALL SIGERR ( 'SPICE(SIZEOUTOFRANGE)'      )
            CALL CHKOUT ( 'SCLD01'                     )
            RETURN

         END IF

      END IF


      IF ( NAME .EQ. NAMLST(COFIDX) ) THEN

         IF (  ( (N/3)*3 )  .NE.  N  ) THEN

            CALL SETMSG ( 'Coefficient count for # must ' &
            //            'be multiple of 3 but was #.'  )
            CALL ERRCH  ( '#', TMPNAM                    )
            CALL ERRINT ( '#', N                         )
            CALL SIGERR ( 'SPICE(INVALIDSIZE)'           )
            CALL CHKOUT ( 'SCLD01'                       )
            RETURN

         END IF

      END IF

!
!     Check the values of the moduli themselves. Also check the
!     moduli count against the field count.
!
      IF ( NAME .EQ. NAMLST(MODIDX) ) THEN

         DO I = 1, N

            IF ( DVAL(I) .LT. 1.D0 ) THEN
               CALL REPMC  (  BVLMSG, '#', TMPNAM,      ERRMSG )
               CALL REPMD  (  ERRMSG, '#', DVAL(I), 14, ERRMSG )
               CALL SETMSG (  ERRMSG                           )
               CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'          )
               CALL CHKOUT ( 'SCLD01'                          )
               RETURN
            END IF

         END DO

      END IF


      IF (       ( NAME .EQ. NAMLST(MODIDX) ) &
           .OR.  ( NAME .EQ. NAMLST(OFFIDX) )  ) THEN
!
!        Get the field count for this clock.
!
         KVNAME = NAMLST( NFLIDX )(:KVNMLN)

         CALL SUFFIX ( '_#',    0,        KVNAME )
         CALL REPMI  ( KVNAME, '#', -SC,  KVNAME )

         CALL GIPOOL ( KVNAME, 1, 1, NF, NFIELD, FOUND )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'SCLD01')
            RETURN
         END IF

         IF ( .NOT. FOUND ) THEN

            CALL SETMSG ( 'Field count was not found for SCLK #.' )
            CALL ERRINT ( '#', SC                    )
            CALL SIGERR ( 'SPICE(KERNELVARNOTFOUND)' )
            CALL CHKOUT ( 'SCLD01'                   )
            RETURN

         END IF

         IF ( N .NE. NFIELD ) THEN

            IF ( NAME .EQ. NAMLST(MODIDX) ) THEN

               CALL SETMSG ( 'Modulus count # does not match ' &
               //            'field count # for SCLK #.'      )

            ELSE

               CALL SETMSG ( 'Offset count # does not match ' &
               //            'field count # for SCLK #.'      )

            END IF

            CALL ERRINT ( '#', N               )
            CALL ERRINT ( '#', NFIELD          )
            CALL ERRINT ( '#', SC              )
            CALL SIGERR ( 'SPICE(INVALIDSIZE)' )
            CALL CHKOUT ( 'SCLD01'             )
            RETURN

         END IF

      END IF

      CALL CHKOUT ( 'SCLD01' )
      RETURN
      END
