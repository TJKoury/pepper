!$Procedure SYPSHD ( Push a value onto a particular symbol )

      subroutine sypshd ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Push a value onto a particular symbol in a double precision
!     symbol table. The previous value(s) associated with the symbol
!     is extended at the front. A new symbol is created if necessary.
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
      real(wp)              value
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      real(wp)              tabval     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol onto which the value is to be
!                    pushed.
!     VALUE      I   Value that is to be pushed onto the symbol NAME.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol onto which the value is to
!              be pushed.
!
!              If NAME is not in the symbol table, a new symbol is
!              created.
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
!              The value is added to the symbol table at the front of
!              the previous value(s) associated with the symbol NAME. If
!              NAME is not originally in the symbol table, a new symbol
!              is created.
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
!     If the symbol NAME is not in the symbol table, a new symbol
!     is created.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        BODY4_POLE_RA -->    1.08D-1
!                             0.0D0
!        DELTA_T_A     -->    3.2184D1
!        K             -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!
!     The call,
!
!        CALL SYPSHD ( 'BODY4_POLE_RA',  3.17681D2,
!       .               TABSYM,          TABPTR,    TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        BODY4_POLE_RA -->    3.17681D2
!                             1.08D-1
!                             0.0D0
!        DELTA_T_A     -->    3.2184D1
!        K             -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!
!     The next call,
!
!        CALL SYPSHC ( 'BODY4_GM', 4.2826286548993737D4,
!       .               TABSYM,     TABPTR,              TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        BODY4_GM      -->    4.2826286548993737D4
!        BODY4_POLE_RA -->    3.17681D2
!                             1.08D-1
!                             0.0D0
!        DELTA_T_A     -->    3.2184D1
!        K             -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!
!     Note that a new symbol "BODY4_GM" was created by the last call.
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
!     push a value onto a particular symbol
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           cardc
      integer(ip)           cardd
      integer(ip)           lstlec
      logical               return
      integer(ip)           sized
      integer(ip)           sumai

!
!     Local variables
!
      integer(ip)           nsym
      integer(ip)           nval

      integer(ip)           locsym
      integer(ip)           locval
      logical               oldsym


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )
      nval = cardd ( tabval )

!
!     Where does this symbol belong? Is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If it's not already in the table, use SET to create a brand new
!     symbol.
!
      if ( .not. oldsym ) then

         call sysetd ( name, value, tabsym, tabptr, tabval )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      else if (  nval >= sized ( tabval ) ) then

         call setmsg ( 'SYPSHD: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         call errdp  ( '$', value )
         call errch  ( '#', name  )
         call sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!     If there's room, add the new value to the value table. Add one
!     to the dimension, and put the value in the right place.
!
      else

         locval = sumai ( tabptr(1), locsym-1 ) + 1

         call inslad ( value, 1, locval, tabval(1), nval )
         call scardd ( nval, tabval )

         tabptr(locsym) = tabptr(locsym) + 1

      end if


      return
      end
