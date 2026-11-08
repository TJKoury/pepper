!$Procedure SYSETD ( Set the value associated with a symbol )

      subroutine sysetd ( name, value, tabsym, tabptr, tabval )

! $ Abstract
!
!     Set the value of a particular symbol in a double precision symbol
!     table. If the symbol already exists, the previous values
!     associated with it are removed, otherwise a new symbol is created.
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
      double precision      value
      character*(*)         tabsym     ( lbcell:* )
      integer               tabptr     ( lbcell:* )
      double precision      tabval     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose associated value is to be
!                    set.
!     VALUE      I   Associated value of the symbol NAME.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose associated value is to
!              be set.
!
!              If NAME has values associated with it, they are removed,
!              and VALUE becomes the only value associated with NAME. If
!              NAME is not in the symbol table, a new symbol is created,
!              provided there is room in the symbol table.
!
!     VALUE    is the new value associated with the symbol NAME.
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a double precision symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a double precision symbol table.
!
!              If NAME has values associated with it, they are removed,
!              and VALUE becomes the only value associated with NAME. If
!              NAME is not in the symbol table, a new symbol is created,
!              provided there is room in the symbol table.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the addition of a new symbol causes an overflow in the
!         name table, the error SPICE(NAMETABLEFULL) is signaled.
!
!     2)  If the addition of a new symbol causes an overflow in the
!         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
!
!     3)  If the addition of a new symbol causes an overflow in the
!         value table, the error SPICE(VALUETABLEFULL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If NAME has values associated with it, they are removed, and VALUE
!     becomes the only value associated with NAME. If NAME is not in the
!     symbol table, a new symbol is created, provided there is room in
!     the symbol table.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        DELTA_T_A -->    3.2184D1
!        K         -->    1.657D-3
!        MEAN_ANOM -->    6.239996D0
!                         1.99096871D-7
!        ORBIT_ECC -->    1.671D-2
!
!     The call,
!
!     CALL SYSETD ( 'ORBIT_ECC', 1.67125D-2, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        DELTA_T_A -->    3.2184D1
!        K         -->    1.657D-3
!        MEAN_ANOM -->    6.239996D0
!                         1.99096871D-7
!        ORBIT_ECC -->    1.67125D-2
!
!     Note that the previous value associated with the symbol
!     "ORBIT_ECC" has been deleted, and now the value 1.67125D-2
!     is associated with the symbol.
!
!
!     The next call,
!
!     CALL SYSETD ( 'EB', 1.671D-2, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        DELTA_T_A -->    3.2184D1
!        EB        -->    1.671D-2
!        K         -->    1.657D-3
!        MEAN_ANOM -->    6.239996D0
!                         1.99096871D-7
!        ORBIT_ECC -->    1.67125D-2
!
!     Note that the new symbol "EB" was created by the last call.
!     A new symbol is created only if there is room in the symbol
!     table.
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
!-    SPICELIB Version 1.1.0, 17-JUN-2021 (JDR)
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
!     set the value associated with a symbol
!
!-&


!
!     SPICELIB functions
!

      integer               cardc
      integer               cardd
      integer               cardi
      integer               lstlec
      logical               return
      integer               sizec
      integer               sized
      integer               sizei
      integer               sumai

!
!     Local variables
!
      integer               nsym
      integer               nptr
      integer               nval

      integer               locsym
      integer               locval
      integer               dimval
      logical               oldsym



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SYSETD' )
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )
      nptr = cardi ( tabptr )
      nval = cardd ( tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym .ne. 0  .and.  tabsym(locsym) .eq. name )

!
!     If it's already in the table, there's no chance of overflow.
!     Leave the name where it is. Remove all but one of the existing
!     values, replacing that with the new value. And set the dimension
!     to one.
!
      if ( oldsym ) then

         locval = sumai  ( tabptr(1), locsym-1 ) + 1
         dimval = tabptr(locsym)

         if ( dimval .gt. 1 ) then
            call remlad ( dimval-1, locval, tabval(1), nval )
            call scardd ( nval, tabval )
         end if

         tabptr(locsym) = 1
         tabval(locval) = value

!
!     Otherwise, we can't proceed unless we know that we have enough
!     room for one extra addition in all three tables.
!
      else if ( nsym .ge. sizec ( tabsym ) ) then

            call setmsg ( 'SYSETD: Addition of the new symbol # '     // &
                          'causes an overflow in the name table.' )
            call errch  ( '#', name )
            call sigerr ( 'SPICE(NAMETABLEFULL)' )


      else if ( nptr .ge. sizei ( tabptr ) ) then

            call setmsg ( 'SYSETD: Addition of the new symbol # '     // &
                          'causes an overflow in the pointer table.' )
            call errch  ( '#', name )
            call sigerr ( 'SPICE(POINTERTABLEFULL)' )


      else if ( nval .ge. sized ( tabval ) ) then

            call setmsg ( 'SYSETD: Addition of the new symbol # '     // &
                          'causes an overflow in the value table.' )
            call errch  ( '#', name )
            call sigerr ( 'SPICE(VALUETABLEFULL)' )


!
!     If there's room, add the new name to the name table. Give the
!     symbol dimension one, and put the value in the right place.
!
      else

         call inslac ( name, 1, locsym+1, tabsym(1), nsym )
         call scardc ( nsym, tabsym )

         call inslai ( 1, 1, locsym+1, tabptr(1), nptr )
         call scardi ( nptr, tabptr )

         locval = sumai ( tabptr(1), locsym ) + 1

         call inslad ( value, 1, locval, tabval(1), nval )
         call scardd ( nval, tabval )

      end if


      call chkout ( 'SYSETD' )
      return
      end
