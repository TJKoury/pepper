!$Procedure SYPUTI ( Set the values associated with a symbol )

      subroutine syputi ( name, values, n, tabsym, tabptr, tabval )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Set the values of a particular symbol in an integer symbol table.
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

      character(*)          name
      integer(ip)           values     (        * )
      integer(ip)           n
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      integer(ip)           tabval     ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose associated values are to
!                    be put into the symbol table.
!     VALUES     I   Values to be associated with the symbol NAME.
!     N          I   Number of values in VALUES.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose associated values are to
!              be set.
!
!              If NAME has values associated with it, they are removed,
!              and the elements of VALUES become the values associated
!              with NAME. If NAME is not in the symbol table, a new
!              symbol is created, provided there is room in the symbol
!              table.
!
!     VALUES   are the new values associated with the symbol NAME.
!
!     N        is the number of elements in the VALUES array.
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
!              If NAME has values associated with it, they are removed,
!              and the elements of VALUES become the values associated
!              with NAME. If NAME is not in the symbol table, a new
!              symbol is created, provided there is room in the symbol
!              table.
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
!     3)  If the addition of new values causes an overflow in the
!         value table, the error SPICE(VALUETABLEFULL) is signaled.
!
!     4)  If N < 1, the error SPICE(INVALIDARGUMENT) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This subroutine is like SYSETC, but SYPUTC allows **several**
!     values to be associated with a symbol.
!
!     If NAME has values associated with it, they are removed, and
!     the elements of VALUES become the values associated with NAME.
!     If NAME is not in the symbol table, a new symbol is created,
!     provided there is room in the symbol table.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  12
!                     23
!        pens    -->  10
!                     12
!                     24
!
!     If VALUES contains the elements,
!
!          12
!          24
!          36
!
!     the call
!
!        CALL SYPUTI ( 'desks', VALUES, 3, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        books   -->   5
!        desks   -->  12
!                     24
!                     36
!        erasers -->   6
!        pencils -->  12
!                     23
!        pens    -->  10
!                     12
!                     24
!     The call,
!
!        CALL SYPUTI ( 'pens', VALUES, 3, TABSYM, TABPTR, TABVAL )
!
!     modifies the contents of the symbol table to be:
!
!        books   -->   5
!        desks   -->  12
!                     24
!                     36
!        erasers -->   6
!        pencils -->  12
!                     23
!        pens    -->  12
!                     24
!                     36
!
!     Note that the previous values associated with "pens" have been
!     replaced by the values in VALUES.
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
!     N.J. Bachman       (JPL)
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
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (HAN) (NJB)
!
!-&


! $ Index_Entries
!
!     set the values associated with a symbol
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
      integer(ip)           newsym
      integer(ip)           newval



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'SYPUTI' )
      end if

!
!     Check to see if the number of values is a valid quantity.
!
      if ( n < 1 ) then
         call setmsg ( 'SYPUTI: The dimension of the values array is' // &
                       'less than one.' )
         call sigerr ( 'SPICE(INVALIDARGUMENT)' )
         call chkout ( 'SYPUTI' )
         return
      end if


!
!     How many symbols to start with?
!
      nsym = cardc ( tabsym )
      nptr = cardi ( tabptr )
      nval = cardi ( tabval )

!
!     Where does this symbol belong? is it already in the table?
!
      locsym = lstlec ( name, nsym, tabsym(1) )
      oldsym = ( locsym /= 0  .and.  tabsym(locsym) == name )

!
!     If the new symbol already exists, we need to know its dimension
!     to check for overflow.
!
      if ( oldsym ) then
         locval = sumai ( tabptr(1), locsym-1 ) + 1
         dimval = tabptr(locsym)
         newsym = 0
      else
         locval = sumai ( tabptr(1), locsym ) + 1
         dimval = 0
         newsym = 1
      end if

      newval = n - dimval

!
!     Can we do this without overflow?
!

      if ( nsym+newsym > sizec ( tabsym ) ) then

         call setmsg ( 'SYPUTI: Addition of the new symbol # causes ' // &
                       'an overflow in the name table.' )
         call errch  ( '#', name )
         call sigerr ( 'SPICE(NAMETABLEFULL)' )


      else if ( nptr+newsym > sizei ( tabptr ) ) then

         call setmsg ( 'SYPUTI: Addition of the new symbol # causes ' // &
                       'an overflow in the pointer table.' )
         call errch  ( '#', name )
         call sigerr ( 'SPICE(POINTERTABLEFULL)' )


      else if ( nval+newval > sizei ( tabval ) ) then

         call setmsg ( 'SYPUTC: Addition of the new symbol # causes ' // &
                       'an overflow in the value table.' )
         call errch  ( '#', name )
         call sigerr ( 'SPICE(VALUETABLEFULL)' )


!
!     Looks like we can.
!
      else

!
!        If the symbol exists, remove the current contents and
!        change the dimension. Otherwise add the new name and
!        dimension to the name and pointer tables.
!
         if ( dimval > 0 ) then
            call remlai ( dimval, locval, tabval(1), nval )
            call scardi ( nval, tabval )

            tabptr(locsym) = n

         else
            call inslac ( name, 1, locsym+1, tabsym(1), nsym )
            call scardc ( nsym, tabsym )

            call inslai ( n,    1, locsym+1, tabptr(1), nptr )
            call scardi ( nptr, tabptr )
         end if

!
!        In either case, insert the values from the input array into
!        the value table.
!
         call inslai ( values, n, locval, tabval(1), nval )
         call scardi ( nval, tabval )

      end if


      call chkout ( 'SYPUTI' )
      return
      end
