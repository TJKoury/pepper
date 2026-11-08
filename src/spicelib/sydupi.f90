!$Procedure SYDUPI ( Create a duplicate of a symbol )

      subroutine sydupi ( name, copy, tabsym, tabptr, tabval )

! $ Abstract
!
!     Create a duplicate of a symbol within an integer symbol table.
!     If a symbol with the new name already exists, its components
!     are replaced.
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
      character*(*)         copy
      character*(*)         tabsym     ( lbcell:* )
      integer               tabptr     ( lbcell:* )
      integer               tabval     ( lbcell:* )

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
!              new symbol COPY. If NAME is not in the symbol table,
!              no duplicate symbol can be made.
!
!     COPY     is the name of the new symbol. If a symbol with the name
!              COPY already exists in the symbol table, its components
!              are replaced by the components of NAME.
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
!              On output, the symbol table contains a new symbol COPY
!              whose components are the same as the components of NAME.
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
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!        pens    -->  10
!                     12
!                     24
!
!     The code,
!
!        CALL SYDUPI ( 'books', 'tablets', TABSYM, TABPTR, TABVAL )
!
!     produces the symbol table:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!        pens    -->  10
!                     12
!                     24
!        tablets -->   5
!
!     The code,
!
!        CALL SYDUPC ( 'desks', 'chairs', TABSYM, TABPTR, TABVAL )
!
!     produces the error SPICE(NOSUCHSYMBOL) because the symbol
!     "desks" is not in the symbol table.
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
      integer               cardc
      integer               cardi
      integer               lstlec
      logical               return
      integer               sizec
      integer               sizei
      integer               sumai

!
!     Local variables
!
      integer               nsym
      integer               nptr
      integer               nval

      integer               locsym  ( 2 )
      integer               locval  ( 2 )
      integer               dimval  ( 2 )
      logical               oldsym  ( 2 )
      integer               newsym
      integer               newval
      integer               i


!
!     Standard SPICE error handling
!
      if ( return () ) then
         return
      else
         call chkin ( 'SYDUPI' )
      end if

!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )
      nptr = cardi ( tabptr )
      nval = cardi ( tabval )

!
!     Where do these symbols belong? Are they already in the table?
!
      locsym(1) = lstlec ( name, nsym, tabsym(1) )
      locsym(2) = lstlec ( copy, nsym, tabsym(1) )

      oldsym(1) = (       locsym(1)         .ne. 0 &
                    .and. tabsym(locsym(1)) .eq. name )

      oldsym(2) = (       locsym(2)         .ne. 0 &
                    .and. tabsym(locsym(2)) .eq. copy )

!
!     If the original symbol is not in the table, we can't make a copy.
!
      if ( .not. oldsym(1) ) then

         call setmsg ( 'SYDUPI: The symbol to be duplicated, #, is ' // &
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
         if ( nsym+newsym .gt. sizec ( tabsym ) ) then

            call setmsg ( 'SYDUPI: Duplication of the symbol # '      // &
                          'causes an overflow in the name table.' )
            call errch  ( '#', name )
            call sigerr ( 'SPICE(NAMETABLEFULL)' )


         else if ( nptr+newsym .gt. sizei ( tabptr ) ) then

            call setmsg ( 'SYDUPI: Duplication of the symbol # '      // &
                          'causes an overflow in the pointer table.' )
            call errch  ( '#', name )
            call sigerr ( 'SPICE(POINTERTABLEFULL)' )


         else if ( nval+newval .gt. sizei ( tabval ) ) then

            call setmsg ( 'SYDUPI: Duplication of the symbol # '      // &
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
            if ( dimval(2) .gt. 0 ) then
              call remlai ( dimval(2), locval(2), tabval(1), nval )
              call scardi ( nval, tabval )

              tabptr(locsym(2)) = dimval(1)

              if ( locval(1) .gt. locval(2) ) then
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

            if ( locval(1) .gt. locval(2) ) then
               locval(1) = locval(1) + dimval(1)
            end if

            do i = 0, dimval(1) - 1
              tabval(locval(2)+i) = tabval(locval(1)+i)
            end do

            call scardi ( nval + dimval(1), tabval )

         end if

      end if


      call chkout ( 'SYDUPI' )
      return
      end
