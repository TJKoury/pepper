!$Procedure SYENQC ( Enqueue a value onto a symbol )

      subroutine syenqc ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Enqueue a value onto a particular symbol in a character
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

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      character(*)          name
      character(*)          value
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      character(*)          tabval     ( lbcell:* )

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
!     TABVAL   are the components of a character symbol table.
!              The symbol NAME may or may not be in the symbol
!              table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a character symbol table.
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
!         BOHR      -->   HYDROGEN ATOM
!         EINSTEIN  -->   SPECIAL RELATIVITY
!                         PHOTOELECTRIC EFFECT
!                         BROWNIAN MOTION
!         FERMI     -->   NUCLEAR FISSION
!
!
!      The call,
!
!         CALL SYENQC ( 'EINSTEIN', 'GENERAL RELATIVITY',
!        .               TABSYM, TABPTR, TABVAL           )
!
!      produces the symbol table:
!
!         BOHR      -->   HYDROGEN ATOM
!         EINSTEIN  -->   SPECIAL RELATIVITY
!                         PHOTOELECTRIC EFFECT
!                         BROWNIAN MOTION
!                         GENERAL RELATIVITY
!         FERMI     -->   NUCLEAR FISSION
!
!      The next call,
!
!         CALL SYENQC ( 'HAHN', 'NUCLEAR FISSION',
!        .              TABSYM, TABPTR, TABVAL     )
!
!      then produces the symbol table:
!
!         BOHR      -->   HYDROGEN ATOM
!         EINSTEIN  -->   SPECIAL RELATIVITY
!                         PHOTOELECTRIC EFFECT
!                         BROWNIAN MOTION
!                         GENERAL RELATIVITY
!         FERMI     -->   NUCLEAR FISSION
!         HAHN      -->   NUCLEAR FISSION
!
!      Note that a new symbol "HAHN" was created by the last call.
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

      integer(ip)           cardc
      integer(ip)           lstlec
      logical               return
      integer(ip)           sizec
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
      nval = cardc ( tabval )

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

         call sysetc ( name, value, tabsym, tabptr, tabval )

!
!     If it is in the table, we can't proceed unless we know that we
!     have enough room for one extra addition in the value table.
!
      else if (  nval >= sizec ( tabval ) ) then

         call setmsg ( 'SYENQC: The addition of the value $ to the '  // &
                       'symbol # causes an overflow in the value '    // &
                       'table.' )
         call errch  ( '$', value )
         call errch  ( '#', name  )
         call sigerr ( 'SPICE(VALUETABLEFULL)' )


!
!     If there's room, add the new value to the value table at the
!     correct location, and add one to the dimension.
!
      else

         locval = sumai ( tabptr(1), locsym ) + 1

         call inslac ( value, 1, locval, tabval(1), nval )
         call scardc ( nval, tabval )

         tabptr(locsym) = tabptr(locsym) + 1

      end if


      return
      end
