!$Procedure SYGETD ( Return all components for a symbol )

      subroutine sygetd ( name, &
                          tabsym, tabptr, tabval, &
                          n,      values, &
                          found                     )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the dimension and associated values for a particular
!     symbol.
!
! $ Required_Reading
!
!     SYMBOLS
!
! $ Keywords
!
!     SYMBOLS
!
! $ Declarations

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      character(*)          name
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      real(wp)              tabval     ( lbcell:* )
      integer(ip)           n
      real(wp)              values     (        * )
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose components are to be
!                    returned.
!     TABSYM,
!     TABPTR,
!     TABVAL     I   Components of the symbol table.
!     N          O   Dimension of the symbol.
!     VALUES     O   Values associated with the symbol.
!     FOUND      O   .TRUE. if the symbol NAME is in the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose components are to be
!              returned. If NAME is not in the symbol table, FOUND is
!              .FALSE.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a double precision symbol table.
!              The symbol NAME may or may not be in the symbol
!              table. The symbol table is not modified by this
!              subroutine.
!
! $ Detailed_Output
!
!     N        is the dimension of the symbol NAME. The dimension is
!              the number of values associated with the given symbol.
!              N is defined only if the output argument FOUND is
!              .TRUE.
!
!     VALUES   is an array containing the values associated with the
!              symbol. If the array is not large enough to hold all
!              of the values associated with NAME, as many as will
!              fit are returned. VALUES is defined only if the
!              output argument FOUND is .TRUE.
!
!     FOUND    is .TRUE. if NAME is in the symbol table.
!              If NAME is not in the table, FOUND is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If there is an issue while reading the components of a double
!         precision symbol table, an error is signaled by a routine in
!         the call tree of this routine. This normally indicates that
!         the double precision symbol table is corrupted.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!         BODY4_POLE_RA -->    3.17681D2
!                              1.08D-1
!                              0.0D0
!         DELTA_T_A     -->    3.2184D1
!         K             -->    1.657D-3
!         MEAN_ANOM     -->    6.239996D0
!                              1.99096871D-7
!         ORBIT_ECC     -->    1.671D-2
!
!      Let the dimension of VALUES be 3.
!
!      The calls,
!
!      CALL SYGETD ( 'K',            TABSYM, TABPTR, TABVAL,
!     .               N,             VALUES, FOUND           )
!
!      CALL SYGETD ( 'BODY4_POLE_RA', TABSYM, TABPTR, TABVAL,
!     .               N,              VALUES, FOUND            )
!
!      CALL SYGETD ( 'BODY4_PRIME',   TABSYM, TABPTR, TABVAL,
!     .               N,              VALUES, FOUND           )
!
!
!      return the values for N, VALUES, and FOUND associated with NAME:
!
!
!         NAME            N        VALUES                      FOUND
!         ----------     ---      -----------------------     -------
!         K               1          1.657D-3                  .TRUE.
!         BODY4_POLE_RA   3          3.17681D2                 .TRUE.
!                                    1.08D-1
!                                    0.0D0
!         BODY4_PRIME                                         .FALSE.
!
! $ Restrictions
!
!     1)  This subroutine does not check to see if the output array
!         VALUES is large enough to hold all of the values associated
!         with the symbol NAME. The caller must provide the required
!         space.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 16-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Moved entry
!        from $Exceptions to $Restrictions and added entry #1 in
!        $Exceptions.
!
!-    SPICELIB Version 1.0.2, 03-NOV-2005 (NJB)
!
!        Various header corrections were made. In particular,
!        the header no longer asserts that this routine will
!        "return as many values as will fit" in the output array
!        VALUES.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     fetch all components for a symbol
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      integer(ip)           cardc
      logical               return
      integer(ip)           sumai

!
!     Local variables
!
      integer(ip)           nsym

      integer(ip)           locsym
      integer(ip)           locval



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SYGETD' )
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!     We trust that the user has supplied enough room.
!
      else

         found = .true.

         locval = sumai ( tabptr(1), locsym-1 ) + 1
         n      = tabptr(locsym)

         call moved ( tabval(locval), n, values )

      end if


      call chkout ( 'SYGETD' )
      return
      end
