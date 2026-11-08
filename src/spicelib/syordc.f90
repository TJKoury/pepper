!$Procedure SYORDC ( Order the components of a single symbol )

      subroutine syordc ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Order the components of a single symbol in a character symbol
!     table. The components are ordered according to the ASCII collating
!     sequence.
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
      character(*)          tabval     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose components are to be
!                    ordered.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose components are to be
!              ordered.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a character symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a character symbol table.
!
!              On output, the components of the symbol are sorted
!              according to ASCII collating sequence. If NAME is not in
!              the symbol table, the symbol table is not modified.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If the symbol NAME is not in the symbol table, the symbol table
!     is not modified.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   SPECIAL RELATIVITY
!                        PHOTOELECTRIC EFFECT
!                        BROWNIAN MOTION
!        FERMI     -->   NUCLEAR FISSION
!
!     The call,
!
!        CALL SYORDC ( 'EINSTEIN', TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        BOHR      -->   HYDROGEN ATOM
!        EINSTEIN  -->   BROWNIAN MOTION
!                        PHOTOELECTRIC EFFECT
!                        SPECIAL RELATIVITY
!        FERMI     -->   NUCLEAR FISSIONC
!
!
!     Note that the call,
!
!        CALL SYORDC ( 'MAXWELL', TABSYM, TABPTR, TABVAL )
!
!     will not modify the symbol table because the symbol "MAXWELL" is
!     not in the symbol table.
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
!-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
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
!     order the components of a single symbol
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
      integer(ip)           n



!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     How many symbols?
!
      nsym = cardc ( tabsym )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If so, sort the components in place.
!
      if ( locsym > 0 ) then

         locval = sumai ( tabptr(1), locsym-1 ) + 1
         n      = tabptr(locsym)

         call shellc ( tabptr(locsym), tabval(locval) )

      end if


      return
      end
