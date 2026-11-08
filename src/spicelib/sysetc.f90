!$Procedure SYSETC ( Set the value associated with a symbol )

      subroutine sysetc ( name, value, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Set the value of a particular symbol in a character symbol table.
!     If the symbol already exists, the previous values associated with
!     it are removed, otherwise a new symbol is created.
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

      character*(*)         name
      character*(*)         value
      character*(*)         tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      character*(*)         tabval     ( lbcell:* )

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
!     TABVAL   are the components of a character symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a character symbol table.
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
!         BOHR      -->   HYDROGEN ATOM
!         EINSTEIN  -->   SPECIAL RELATIVITY
!                         PHOTOELECTRIC EFFECT
!                         BROWNIAN MOTION
!         FERMI     -->   NUCLEAR FISSION
!         PAULI     -->   EXCLUSION PRINCIPLE
!                         NEUTRINO
!
!      The call,
!
!      CALL SYSETC ( 'EINSTEIN', 'GENERAL RELATIVITY',
!     .               TABSYM,     TABPTR,      TABVAL  )
!
!      modifies the contents of the symbol table to be:
!
!         BOHR      -->   HYDROGEN ATOM
!         EINSTEIN  -->   GENERAL RELATIVITY
!         FERMI     -->   NUCLEAR FISSION
!         PAULI     -->   EXCLUSION PRINCIPLE
!                         NEUTRINO
!
!      Note that the previous values associated with the symbol
!      "EINSTEIN" have been deleted, and now only the new value is
!      associated with the symbol.
!
!
!      The next call,
!
!      CALL SYSETC ( 'MILLIKAN', 'PHOTOELECTRIC EFFECT'
!     .               TABSYM,     TABPTR,       TABVAL   )
!
!      modifies the contents of the symbol table to be:
!
!         BOHR      -->   HYDROGEN ATOM
!         EINSTEIN  -->   GENERAL RELATIVITY
!         FERMI     -->   NUCLEAR FISSION
!         MILLIKAN  -->   PHOTOELECTRIC EFFECT
!         PAULI     -->   EXCLUSION PRINCIPLE
!                         NEUTRINOC
!
!      Note that the new symbol "MILLIKAN" was created by the last call.
!      A new symbol is created only if there is room in the symbol
!      table.
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

      integer(ip)           cardc
      integer(ip)           cardi
      integer(ip)           lstlec
      logical               return
      integer(ip)           sizec
      integer(ip)           sizei
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
      logical               oldsym



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SYSETC' )
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )
      nptr = cardi ( tabptr )
      nval = cardc ( tabval )

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
            call remlac ( dimval-1, locval, tabval(1), nval )
            call scardc ( nval, tabval )
         end if

         tabptr(locsym) = 1
         tabval(locval) = value

!
!     Otherwise, we can't proceed unless we know that we have enough
!     room for one extra addition in all three tables.
!
      else if ( nsym .ge. sizec ( tabsym ) ) then

            call setmsg ( 'SYSETC: Addition of the new symbol # '   // &
                          'causes an overflow in the name table.' )
            call errch  ( '#', name )
            call sigerr ( 'SPICE(NAMETABLEFULL)' )


      else if ( nptr .ge. sizei ( tabptr ) ) then

            call setmsg ( 'SYSETC: Addition of the new symbol # '     // &
                          'causes an overflow in the pointer table.' )
            call errch  ( '#', name )
            call sigerr ( 'SPICE(POINTERTABLEFULL)' )


      else if ( nval .ge. sizec ( tabval ) ) then

            call setmsg ( 'SYSETC: Addition of the new symbol #  '    // &
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

         call inslac ( value, 1, locval, tabval(1), nval )
         call scardc ( nval, tabval )

      end if


      call chkout ( 'SYSETC' )
      return
      end
