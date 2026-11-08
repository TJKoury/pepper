!$Procedure SCLU01 ( SCLK look up, type 1 )

      subroutine sclu01 (  name,  sc,  maxnv,  n,  ival,  dval )

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

      implicit none

      include               'sclk.inc'

      character*(*)         name
      integer               sc
      integer               maxnv
      integer               n
      integer               ival ( * )
      double precision      dval ( * )

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
      integer               isrchc

      logical               failed
      logical               return

!
!     Local parameters
!
      integer               msglen
      parameter           ( msglen =        320 )

      integer               kvnmln
      parameter           ( kvnmln =         32 )

!
!     MXCOFA is the maximum size of the coefficient array.
!
      integer               mxcofa
      parameter           ( mxcofa = 3 * mxcoef )

      integer               namlen
      parameter           ( namlen =         80 )

      integer               nkitem
      parameter           ( nkitem =          9 )

!
!     COFIDX is the index of the coefficient name in NAMLST.
!
      integer               cofidx
      parameter           ( cofidx =  1 )

!
!     NFLIDX is the index of the SCLK field count in NAMLST.
!
      integer               nflidx
      parameter           ( nflidx =  4 )

!
!     OFFIDX is the index of the SCLK offsets in NAMLST.
!
      integer               offidx
      parameter           ( offidx =  5 )

!
!     MODIDX is the index of the SCLK moduli in NAMLST.
!
      integer               modidx
      parameter           ( modidx =  6 )

!
!     DELIDX is the index of the delimiter code name in NAMLST.  If
!     the declaration of NAMLST or assignment of values to NAMLST
!     changes, this parameter value may have to change.
!
      integer               delidx
      parameter           ( delidx =  7 )

!
!     SYSIDX is the index of the time system in NAMLST.
!
      integer               sysidx
      parameter           ( sysidx =  9 )


!
!     Local variables
!
      character*(msglen)    bvlmsg
      character*(msglen)    errmsg
      character*(kvnmln)    kvname
      character*(msglen)    nfdmsg
      character*(msglen)    nummsg
      character*(namlen)    namlst ( nkitem )
      character*(namlen)    tmpnam
      character*(1)         type

      integer               i
      integer               lb     ( nkitem )
      integer               nf
      integer               nfield
      integer               ub     ( nkitem )

      logical               found


!
!     Saved variables
!
      save                  bvlmsg
      save                  lb
      save                  namlst
      save                  nfdmsg
      save                  nummsg
      save                  ub

!
!     Initial values
!

!
!     Names of type 1 SCLK items and lower bounds on the number of
!     associated values.
!
      data (   namlst(i),            lb(i),  ub(i),   i = 1, nkitem ) / &

            'SCLK01_COEFFICIENTS',      3,   mxcofa, &
            'SCLK_PARTITION_START',     1,   mxpart, &
            'SCLK_PARTITION_END',       1,   mxpart, &
            'SCLK01_N_FIELDS',          1,   1, &
            'SCLK01_OFFSETS',           1,   mxnfld, &
            'SCLK01_MODULI',            1,   mxnfld, &
            'SCLK01_OUTPUT_DELIM',      1,   1, &
            'SCLK01_KERNEL_ID',         1,   1, &
            'SCLK01_TIME_SYSTEM',       0,   1                        /


      data nfdmsg / '# not found. Did you load the SCLK kernel?'      /

      data nummsg / '# values found for #: valid range is #:#.'       /

      data bvlmsg / 'Invalid value found for #:  #.'                  /

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'SCLU01' )

      call sigerr ( 'SPICE(BOGUSENTRY)' )

      call chkout ( 'SCLU01' )
      return


!$Procedure SCLI01 ( SCLK lookup of integer data, type 1 )

      entry scli01 ( name, sc, maxnv, n, ival )

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
      if ( return () ) then
         return
      end if

      call chkin ( 'SCLI01' )

!
!     Form the name of the kernel pool data item, and do the lookup.
!     Note that eventually we should use a kernel pool lookup entry
!     that allows us to specify the maximum number of entries that
!     can be returned.
!
      tmpnam = name

      call suffix ( '_#',    0,       tmpnam )
      call repmi  ( tmpnam, '#', -sc, tmpnam )

!
!     Make sure we have enough room for the item in our output
!     array. Look up the dimension of the item.
!
      call dtpool ( tmpnam, found, n, type )

      if ( n .gt. maxnv ) then

         call setmsg ( 'Item # for SCLK # has size # but ' &
         //            'output array has size #.'         )
         call errch  ( '#',  tmpnam                       )
         call errint ( '#',  sc                           )
         call errint ( '#',  n                            )
         call errint ( '#',  maxnv                        )
         call sigerr ( 'SPICE(ARRAYTOOSMALL)'             )
         call chkout ( 'SCLI01'                           )
         return

      end if

!
!     Check the data type of the variable.
!
      if (  found  .and.  ( type .ne. 'N' )  ) then

         call setmsg ( 'Kernel variable # for spacecraft ' &
         //            'clock # does not have numeric type.' )
         call errch  ( '#', tmpnam                           )
         call errint ( '#', sc                               )
         call sigerr ( 'SPICE(BADKERNELVARTYPE)'             )
         call chkout ( 'SCLI01'                              )
         return

      end if

      call gipool ( tmpnam, 1, maxnv, n, ival, found )

      if ( failed() ) then
         call chkout ( 'SCLI01' )
         return
      end if

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

         if (  name .eq. namlst(sysidx)  ) then

            call chkout ( 'SCLI01' )
            return

         else

            call setmsg (  nfdmsg                    )
            call errch  ( '#', tmpnam                )
            call sigerr ( 'SPICE(KERNELVARNOTFOUND)' )
            call chkout ( 'SCLI01'                   )
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
      i = isrchc ( name, nkitem, namlst )

      if ( i .ne. 0 ) then

         if (  (  n .lt. lb(i) ) .or. ( n .gt. ub(i) )  ) then

            call repmi  (  nummsg, '#', n,      errmsg )
            call repmc  (  errmsg, '#', tmpnam, errmsg )
            call repmi  (  errmsg, '#', lb(i),  errmsg )
            call repmi  (  errmsg, '#', ub(i),  errmsg )
            call setmsg (  errmsg                      )
            call sigerr ( 'SPICE(SIZEOUTOFRANGE)'      )
            call chkout ( 'SCLI01'                     )
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
      if ( name .eq. namlst(delidx) ) then

         if (  ( ival(1) .lt. 1 ) .or. ( ival(1) .gt. ndelim )  ) then

            call repmc  (  bvlmsg, '#', tmpnam,  errmsg )
            call repmi  (  errmsg, '#', ival(1), errmsg )
            call setmsg (  errmsg                       )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'      )
            call chkout ( 'SCLI01'                      )
            return

         end if

      end if


      if ( name .eq. namlst(nflidx) ) then

         if (  ( ival(1) .lt. 1 ) .or. ( ival(1) .gt. mxnfld )  ) then

            call repmc  (  bvlmsg, '#', tmpnam,  errmsg )
            call repmi  (  errmsg, '#', ival(1), errmsg )
            call setmsg (  errmsg                       )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'      )
            call chkout ( 'SCLI01'                      )
            return

         end if

      end if


      if ( name .eq. namlst(sysidx) ) then

         if (  ( ival(1) .lt. 1 ) .or. ( ival(1) .gt. mxtsys )  ) then

            call repmc  (  bvlmsg, '#', tmpnam,  errmsg )
            call repmi  (  errmsg, '#', ival(1), errmsg )
            call setmsg (  errmsg                       )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'      )
            call chkout ( 'SCLI01'                      )
            return

         end if

      end if

      call chkout ( 'SCLI01' )
      return


!$Procedure SCLD01 ( SCLK lookup of double precision data, type 1 )

      entry scld01 ( name, sc, maxnv, n, dval )

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
      if ( return () ) then
         return
      end if

      call chkin ( 'SCLD01' )

!
!     Form the name of the kernel pool datum, and do the lookup.
!
      tmpnam = name

      call suffix ( '_#',    0,        tmpnam )
      call repmi  ( tmpnam, '#', -sc,  tmpnam )

!
!     Make sure we have enough room for the item in our output
!     array. Look up the dimension of the item.
!
      call dtpool ( tmpnam,  found,  n,  type )

      if ( n .gt. maxnv ) then

         call setmsg ( 'Item # has size # but output array ' &
         //            'has size #.'                         )
         call errch  ( '#',  tmpnam                          )
         call errint ( '#',  n                               )
         call errint ( '#',  maxnv                           )
         call sigerr ( 'SPICE(ARRAYTOOSMALL)'                )
         call chkout ( 'SCLD01'                              )
         return

      end if

!
!     Check the data type of the variable.
!
      if (  found  .and.  ( type .ne. 'N' )  ) then

         call setmsg ( 'Kernel variable # for spacecraft ' &
         //            'clock # does not have numeric type.' )
         call errch  ( '#', tmpnam                           )
         call errint ( '#', sc                               )
         call sigerr ( 'SPICE(BADKERNELVARTYPE)'             )
         call chkout ( 'SCLD01'                              )
         return

      end if

      call gdpool ( tmpnam, 1, maxnv, n, dval, found )

      if ( failed() ) then
!
!        This code should be unreachable but is provided for safety.
!
         call chkout ( 'SCLD01' )
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

         call setmsg (  nfdmsg                    )
         call errch  ( '#', tmpnam                )
         call sigerr ( 'SPICE(KERNELVARNOTFOUND)' )
         call chkout ( 'SCLD01'                   )
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
      i = isrchc ( name, nkitem, namlst )

      if ( i .ne. 0 ) then

         if (  (  n .lt. lb(i) ) .or. ( n .gt. ub(i) )  ) then

            call repmi  (  nummsg, '#', n,      errmsg )
            call repmc  (  errmsg, '#', tmpnam, errmsg )
            call repmi  (  errmsg, '#', lb(i),  errmsg )
            call repmi  (  errmsg, '#', ub(i),  errmsg )
            call setmsg (  errmsg                      )
            call sigerr ( 'SPICE(SIZEOUTOFRANGE)'      )
            call chkout ( 'SCLD01'                     )
            return

         end if

      end if


      if ( name .eq. namlst(cofidx) ) then

         if (  ( (n/3)*3 )  .ne.  n  ) then

            call setmsg ( 'Coefficient count for # must ' &
            //            'be multiple of 3 but was #.'  )
            call errch  ( '#', tmpnam                    )
            call errint ( '#', n                         )
            call sigerr ( 'SPICE(INVALIDSIZE)'           )
            call chkout ( 'SCLD01'                       )
            return

         end if

      end if

!
!     Check the values of the moduli themselves. Also check the
!     moduli count against the field count.
!
      if ( name .eq. namlst(modidx) ) then

         do i = 1, n

            if ( dval(i) .lt. 1.d0 ) then
               call repmc  (  bvlmsg, '#', tmpnam,      errmsg )
               call repmd  (  errmsg, '#', dval(i), 14, errmsg )
               call setmsg (  errmsg                           )
               call sigerr ( 'SPICE(VALUEOUTOFRANGE)'          )
               call chkout ( 'SCLD01'                          )
               return
            end if

         end do

      end if


      if (       ( name .eq. namlst(modidx) ) &
           .or.  ( name .eq. namlst(offidx) )  ) then
!
!        Get the field count for this clock.
!
         kvname = namlst( nflidx )(:kvnmln)

         call suffix ( '_#',    0,        kvname )
         call repmi  ( kvname, '#', -sc,  kvname )

         call gipool ( kvname, 1, 1, nf, nfield, found )

         if ( failed() ) then
            call chkout ( 'SCLD01')
            return
         end if

         if ( .not. found ) then

            call setmsg ( 'Field count was not found for SCLK #.' )
            call errint ( '#', sc                    )
            call sigerr ( 'SPICE(KERNELVARNOTFOUND)' )
            call chkout ( 'SCLD01'                   )
            return

         end if

         if ( n .ne. nfield ) then

            if ( name .eq. namlst(modidx) ) then

               call setmsg ( 'Modulus count # does not match ' &
               //            'field count # for SCLK #.'      )

            else

               call setmsg ( 'Offset count # does not match ' &
               //            'field count # for SCLK #.'      )

            end if

            call errint ( '#', n               )
            call errint ( '#', nfield          )
            call errint ( '#', sc              )
            call sigerr ( 'SPICE(INVALIDSIZE)' )
            call chkout ( 'SCLD01'             )
            return

         end if

      end if

      call chkout ( 'SCLD01' )
      return
      end
