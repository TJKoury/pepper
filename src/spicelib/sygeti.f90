!$Procedure SYGETI ( Return all components for a symbol )

      subroutine sygeti ( name, &
                          tabsym, tabptr, tabval, &
                          n,      values, &
                          found                     )
      use pepper_kinds, only: ip

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
      integer(ip)           tabval     ( lbcell:* )
      integer(ip)           n
      integer(ip)           values     (        * )
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
!
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
!     TABVAL   are the components of an integer symbol table.
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
!     1)  If there is an issue while reading the components of a
!         integer symbol table, an error is signaled by a routine in
!         the call tree of this routine. This normally indicates that
!         the integer symbol table is corrupted.
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
!         books   -->   5
!         erasers -->   6
!         pencils -->  12
!                      24
!         pens    -->  10
!                      12
!                      24
!
!      Let the dimension of VALUES be 3.
!
!      The calls,
!
!      CALL SYGETI ( 'pencils', TABSYM, TABPTR, TABVAL,
!     .               N,         VALUES, FOUND           )
!
!      CALL SYGETI ( 'pens',     TABSYM, TABPTR, TABVAL,
!     .               N,         VALUES, FOUND           )
!
!      CALL SYGETI ( 'desks',    TABSYM, TABPTR, TABVAL,
!     .               N,         VALUES, FOUND           )
!
!
!      return the values for N, VALUES, and FOUND associated with NAME:
!
!         NAME         N      VALUES    FOUND
!         ----------  ---    --------  -------
!         pencils      2        12      .TRUE.
!                               24
!         pens         3        10      .TRUE.
!                               12
!                               24
!         desks                        .FALSE.
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
         call chkin ( 'SYGETI' )
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

         call movei ( tabval(locval), n, values )

      end if


      call chkout ( 'SYGETI' )
      return
      end
