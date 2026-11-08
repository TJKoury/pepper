!$Procedure SYDELI ( Delete a symbol from a symbol table )

      subroutine sydeli ( name, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Delete a symbol from an integer symbol table. The symbol
!     and its associated values are deleted.
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
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      integer(ip)           tabval     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol to be deleted.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol to be deleted from the symbol
!              table.
!
!              If the symbol does not exist, the symbol table remains
!              unchanged. This subroutine is case sensitive, NAME must
!              match the symbol exactly.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of an integer symbol table.
!
!              On input, the table may or may not contain the symbol
!              NAME.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of an integer symbol table.
!
!              On output, the symbol table no longer contains the symbol
!              NAME or its associated values. If NAME is not a symbol,
!              the components of the symbol table remain unchanged.
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
!     None.
!
! $ Examples
!
!     In the following example the subroutine SYDELI is used to delete
!     the symbol "pens" and its values from the symbol table.
!
!     The contents of the symbol table are:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!        pens    -->  10
!                     12
!                     24
!
!     The call
!
!        CALL SYDELI ( 'pens', TABSYM, TABPTR, TABVAL )
!
!     deletes the symbol "pens" from the symbol table. The
!     components of the symbol table on output are:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
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
!     delete a symbol from a symbol table
!
!-&

!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      integer(ip)           cardc
      integer(ip)           cardi
      logical               return
      integer(ip)           sumai

!
!     Local variables
!
      integer(ip)           nsym
      integer(ip)           nptr
      integer(ip)           nval

      integer(ip)           locsym
      integer(ip)           locval
      integer(ip)           dimval

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )
      nptr = cardi ( tabptr )
      nval = cardi ( tabval )

!
!     Is this symbol even in the table?
!
      locsym = bsrchc ( name, nsym, tabsym(1) )

!
!     If it's not in the table, we're done. If it is, we can proceed
!     without fear of overflow.
!
      if ( locsym > 0 ) then

         locval = sumai ( tabptr(1), locsym-1 ) + 1
         dimval = tabptr(locsym)

         call remlac ( 1, locsym, tabsym(1), nsym )
         call scardc ( nsym, tabsym )

         call remlai ( 1, locsym, tabptr(1), nptr )
         call scardi ( nptr, tabptr )

         call remlai ( dimval, locval, tabval(1), nval )
         call scardi ( nval, tabval )

      end if

      return
      end
