!$Procedure SYNTHC ( Return Nth value associated with the symbol )

      subroutine synthc ( name, &
                          nth, &
                          tabsym, tabptr, tabval, &
                          value, &
                          found                               )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the Nth value associated with a particular symbol in a
!     character symbol table.
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

      integer(ip), parameter :: lbcell = -5

      character(*)          name
      integer(ip)           nth
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      character(*)          tabval     ( lbcell:* )
      character(*)          value
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose Nth associated value is
!                    to be returned.
!     NTH        I   Index of the value to be returned.
!     TABSYM,
!     TABPTR,
!     TABVAL     I   Components of the symbol table.
!     VALUE      O   Nth value associated with the symbol.
!     FOUND      O   .TRUE. if the Nth value of the symbol exists.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose Nth associated value
!              is to be returned. If NAME is not in the symbol table,
!              FOUND is .FALSE.
!
!     NTH      is the index of the value to be returned. If the
!              value of NTH is out of range ( NTH < 1 or NTH is
!              greater than the dimension of the symbol ) FOUND is
!              .FALSE.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a character symbol table.
!              The symbol table is not modified by this subroutine.
!
! $ Detailed_Output
!
!     VALUES   is the NTH value associated with the symbol NAME.
!
!     FOUND    is .TRUE. if NAME is in the symbol table and the NTH
!              value associated with NAME exists. Otherwise FOUND
!              is .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If there is an issue while reading the components of a
!         character symbol table, an error is signaled by a routine in
!         the call tree of this routine. This normally indicates that
!         the character symbol table is corrupted.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Two conditions will cause the value of FOUND to be .FALSE.:
!
!     1)  The symbol NAME is not in the symbol table.
!
!     2)  NTH is out of range ( NTH < 1 or NTH is greater than the
!         dimension of the symbol ).
!
! $ Examples
!
!     The contents of the symbol table are:
!
!         BOHR      -->   HYDROGEN ATOM
!         EINSTEIN  -->   SPECIAL RELATIVITY
!                         PHOTOELECTRIC EFFECT
!                         BROWNIAN MOTION
!         FERMI     -->   NUCLEAR FISSION
!
!      The calls,
!
!      CALL SYNTHC ( 'EINSTEIN', 2, TABSYM, TABPTR, TABVAL, VALUE,
!     .               FOUND                                        )
!
!      CALL SYNTHC ( 'BORN',     2, TABSYM, TABPTR, TABVAL, VALUE,
!     .               FOUND                                        )
!
!      CALL SYNTHC ( 'MAXWELL',  5, TABSYM, TABPTR, TABVAL, VALUE,
!     .               FOUND                                        )
!
!      return the values of VALUE and FOUND corresponding to NAME and
!      NTH:
!
!         NAME          NTH           VALUE                 FOUND
!         ----------   -----     ----------------------    -------
!         EINSTEIN       2       PHOTOELECTRIC EFFECT       .TRUE.
!         BORN           2                                 .FALSE.
!         MAXWELL        5                                 .FALSE.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
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
!        Edited the header to comply with NAIF standard. Added entry #1
!        in $Exceptions section.
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
!     fetch nth value associated with a symbol
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
      if ( return () ) return

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
!     If the value of NTH is out of range, that's a problem too.
!
      else if ( nth < 1  .or.  nth > tabptr(locsym) ) then

         found = .false.

!
!     Otherwise, we can proceed without fear of error. Merely locate
!     and return the appropriate component from the values table.
!
      else

         found  = .true.

         locval = sumai ( tabptr(1), locsym-1 ) + nth
         value  = tabval(locval)

      end if

      return
      end
