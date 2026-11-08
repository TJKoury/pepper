!$Procedure SYDUPD ( Create a duplicate of a symbol )

      subroutine sydupd ( name, copy, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Create a duplicate of a symbol within a double precision symbol
!     table. If a symbol with the new name already exists, its
!     components are replaced.
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
      character(*)          copy
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      real(wp)              tabval     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol to be duplicated.
!     COPY       I   Name of the new symbol.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol to be duplicated.
!
!              The components associated with NAME will be given to the
!              new symbol COPY. If NAME is not in the symbol table, no
!              duplicate symbol can be made.
!
!     COPY     is the name of the new symbol. If a symbol with the
!              name COPY already exists in the symbol table, its
!              components are replaced by the components of NAME.
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
!              On output, the symbol table contains a new symbol COPY
!              whose components are the same as the components of
!              NAME.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the symbol NAME is not in the symbol table, the error
!         SPICE(NOSUCHSYMBOL) is signaled.
!
!     2)  If duplication of the symbol causes an overflow in the
!         name table, the error SPICE(NAMETABLEFULL) is signaled.
!
!     3)  If duplication of the symbol causes an overflow in the
!         pointer table, the error SPICE(POINTERTABLEFULL) is signaled.
!
!     4)  If duplication of the symbol causes an overflow in the
!         value table, the error SPICE(VALUETABLEFULL) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     If the symbol NAME is not in the symbol table, no duplicate symbol
!     can be made.
!
!     If the symbol COPY is already in the symbol table, its components
!     are replaced by the components of NAME.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        DELTA_T_A -->   32.184
!        K         -->    1.657D-3
!        MEAN_ANOM -->    6.239996D0
!                         1.99096871D-7
!        ORBIT_ECC -->    1.671D-2
!
!     The code,
!
!        CALL SYDUPD ( 'K', 'EB', TABSYM, TABPTR, TABVAL )
!
!     produces the symbol table:
!
!        DELTA_T_A -->   32.184
!        EB        -->    1.657D-3
!        K         -->    1.657D-3
!        MEAN_ANOM -->    6.239996D0
!                         1.99096871D-7
!        ORBIT_ECC -->    1.671D-2
!
!     The code,
!
!        CALL SYDUPD ( 'M0', 'M1', TABSYM, TABPTR, TABVAL )
!
!     produces the error SPICE(NOSUCHSYMBOL) because the symbol "M0" is
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
!     create a duplicate of a symbol
!
!-&

!
!     SPICELIB functions
!

      integer(ip)           cardc
      integer(ip)           cardd
      integer(ip)           cardi
      integer(ip)           lstlec
      logical               return
      integer(ip)           sizec
      integer(ip)           sized
      integer(ip)           sizei
      integer(ip)           sumai

!
!     Local variables
!
      integer(ip)           nsym
      integer(ip)           nptr
      integer(ip)           nval

      integer(ip)           locsym  ( 2 )
      integer(ip)           locval  ( 2 )
      integer(ip)           dimval  ( 2 )
      logical               oldsym  ( 2 )
      integer(ip)           newsym
      integer(ip)           newval
      integer(ip)           i

!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )
      nptr = cardi ( tabptr )
      nval = cardd ( tabval )

!
!     Where do these symbols belong? Are they already in the table?
!
      locsym(1) = lstlec ( name, nsym, tabsym(1) )
      locsym(2) = lstlec ( copy, nsym, tabsym(1) )

      oldsym(1) = (       locsym(1)         /= 0 &
                    .and. tabsym(locsym(1)) == name )

      oldsym(2) = (       locsym(2)         /= 0 &
                    .and. tabsym(locsym(2)) == copy )

!
!     If the original symbol is not in the table, we can't make a copy.
!
      if ( .not. oldsym(1) ) then

         call setmsg ( 'SYDUPD: The symbol to be duplicated, #, is ' // &
                       'not in the symbol table.' )
         call errch  ( '#', name )
         call sigerr ( 'SPICE(NOSUCHSYMBOL)' )

!
!     Otherwise, we need to know the dimension, to check for overflow.
!
      else

         locval(1) = sumai  ( tabptr(1), locsym(1)-1 ) + 1
         dimval(1) = tabptr(locsym(1))

!
!        If the new symbol already exists, we need to know its dimension
!        too, for the same reason.
!
         if ( oldsym(2) ) then
            locval(2) = sumai ( tabptr(1), locsym(2)-1 ) + 1
            dimval(2) = tabptr(locsym(2))
            newsym    = 0
         else
            locval(2) = sumai ( tabptr(1), locsym(2) ) + 1
            dimval(2) = 0
            newsym    = 1
         end if

         newval = dimval(1) - dimval(2)

!
!        Can we make a copy without overflow?
!
         if ( nsym+newsym > sizec ( tabsym ) ) then

            call setmsg ( 'SYDUPD: Duplication of the symbol # '    // &
                          'causes an overflow in the name table.' )
            call errch  ( '#', name )
            call sigerr ( 'SPICE(NAMETABLEFULL)' )

         else if ( nptr+newsym > sizei ( tabptr ) ) then

            call setmsg ( 'SYDUPD: Duplication of the symbol # '    // &
                          'causes an overflow in the pointer table.' )
            call errch  ( '#', name )
            call sigerr ( 'SPICE(POINTERTABLEFULL)' )

         else if ( nval+newval > sized ( tabval ) ) then

            call setmsg ( 'SYDUPD: Duplication of the symbol # '    // &
                          'causes an overflow in the value table.' )
            call errch  ( '#', name )
            call sigerr ( 'SPICE(VALUETABLEFULL)' )

!
!        Looks like we can.
!
         else

!
!           If the copy exists, remove the current contents and
!           change the dimension. Otherwise add the new name and
!           dimension to the name and pointer tables.
!
            if ( dimval(2) > 0 ) then
              call remlad ( dimval(2), locval(2), tabval(1), nval )
              call scardd ( nval, tabval )

              tabptr(locsym(2)) = dimval(1)

              if ( locval(1) > locval(2) ) then
                 locval(1) = locval(1) - dimval(2)
              end if

            else
              call inslac ( copy, 1, locsym(2)+1, tabsym(1), nsym )
              call scardc ( nsym, tabsym )

              call inslai ( dimval(1), 1, locsym(2)+1, tabptr(1), nptr )
              call scardi ( nptr, tabptr )
            end if

!
!           In either case, allocate space for the new symbol values,
!           and copy them in one by one. (INSLAx won't work if the
!           copy is earlier in the table than the original.)
!
            do i = nval, locval(2), -1
              tabval(i+dimval(1)) = tabval(i)
            end do

            if ( locval(1) > locval(2) ) then
               locval(1) = locval(1) + dimval(1)
            end if

            do i = 0, dimval(1) - 1
              tabval(locval(2)+i) = tabval(locval(1)+i)
            end do

            call scardd ( nval + dimval(1), tabval )

         end if

      end if

      return
      end
