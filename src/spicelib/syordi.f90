!$Procedure SYORDI ( Order the components of a single symbol )

      subroutine syordi ( name, tabsym, tabptr, tabval )

! $ Abstract
!
!     Order the components of a single symbol in an integer symbol
!     table. The components are sorted in increasing order.
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

      integer               lbcell
      parameter           ( lbcell = -5 )

      character*(*)         name
      character*(*)         tabsym     ( lbcell:* )
      integer               tabptr     ( lbcell:* )
      integer               tabval     ( lbcell:* )

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
!     TABVAL   are the components of an integer symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of an integer symbol table.
!
!              On output, the components of the symbol are sorted in
!              increasing order. If NAME is not in the symbol table, the
!              symbol table is not modified.
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
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!                     24
!        pens    -->  10
!                     24
!                     12
!                     36
!                      4
!
!     The call,
!
!        CALL SYORDI ( 'pens', TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!                     24
!        pens    -->   4
!                     10
!                     12
!                     24
!                     36
!
!     Note that the call,
!
!        CALL SYORDI ( 'desks', TABSYM, TABPTR, TABVAL )
!
!     will not modify the symbol table because the symbol "desks" is
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
      integer               bsrchc
      integer               cardc
      logical               return
      integer               sumai

!
!     Local variables
!
      integer               nsym

      integer               locsym
      integer               locval
      integer               n



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SYORDI' )
      end if


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
      if ( locsym .gt. 0 ) then

         locval = sumai ( tabptr(1), locsym-1 ) + 1
         n      = tabptr(locsym)

         call shelli ( tabptr(locsym), tabval(locval) )

      end if


      call chkout ( 'SYORDI' )
      return
      end
