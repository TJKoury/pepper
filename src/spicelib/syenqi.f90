!$Procedure SYENQI ( Enqueue a value onto a symbol )

      subroutine syenqi ( name, value, tabsym, tabptr, tabval )

! $ Abstract
!
!     Enqueue a value onto a particular symbol in an integer
!     symbol table. If the symbol is not in the table, a new one
!     is created.
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
      integer               value
      character*(*)         tabsym     ( lbcell:* )
      integer               tabptr     ( lbcell:* )
      integer               tabval     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol onto which the value is
!                    enqueued.
!     VALUE      I   Value to be enqueued.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol onto which the value is to
!              be enqueued. If NAME is not in the symbol table, a new
!              symbol having the value VALUE is created.
!
!     VALUE    is the value to be enqueued onto the symbol, NAME.
!              The value is inserted in the value table after the
!              last value associated with the symbol.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of an integer symbol table.
!              The symbol NAME may or may not be in the symbol
!              table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of an integer symbol table.
!
!              On output, the value table contains the new value in
!              addition to the old values associated with the symbol
!              NAME. The pointer table is updated to reflect the change
!              in the dimension of the symbol.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the addition of the new value to the symbol table causes an
!         overflow in the value table, the error SPICE(VALUETABLEFULL)
!         is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If the symbol NAME is not in the symbol table, a new symbol is
!     created which has the value VALUE.
!
! $ Examples
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
!     The call,
!
!        CALL SYENQI ( 'books', 12, TABSYM, TABPTR, TABVAL )
!
!     produces the symbol table:
!
!        books   -->   5
!                     12
!        erasers -->   6
!        pencils -->  12
!        pens    -->  10
!                     12
!                     24
!
!     The next call,
!
!        CALL SYENQI ( 'desks', 23, TABSYM, TABPTR, TABVAL )
!
!     then produces the symbol table:
!
!        books   -->   5
!                     12
!        desks   -->  23
!        erasers -->   6
!        pencils -->  12
!        pens    -->  10
!                     12
!                     24
!
!     Notice that the symbol "desks" was created by the last call.
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
!     enqueue a value onto a symbol
!
!-&


!
!     SPICELIB functions
!
      integer               cardc
      integer               cardi
      integer               lstlec
      logical               return
      integer               sizei
      integer               sumai

!
!     Local variables
!
      integer               nsym
      integer               nval

      integer               locsym
      integer               locval
      logical               oldsym



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SYENQI' )
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )
      nval = cardi ( tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym .ne. 0  .and.  tabsym(locsym) .eq. name )

!
!     If it's not already in the table, use SET to create a brand new
!     symbol.
!
      if ( .not. oldsym ) then

         call syseti ( name, value, tabsym, tabptr, tabval )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      else if (  nval .ge. sizei ( tabval ) ) then

         call setmsg ( 'SYENQI: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         call errint ( '$', value )
         call errch  ( '#', name  )
         call sigerr ( 'SPICE(VALUETABLEFULL)' )


!
!     If there's room, add the new value to the value table. Add one
!     to the dimension, and put the value in the right place.
!
      else

         locval = sumai ( tabptr(1), locsym ) + 1

         call inslai ( value, 1, locval, tabval(1), nval )
         call scardi ( nval, tabval )

         tabptr(locsym) = tabptr(locsym) + 1

      end if


      call chkout ( 'SYENQI' )
      return
      end
