!$Procedure SYPOPI ( Pop a value from a particular symbol )

      subroutine sypopi ( name, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Pop a value associated with a particular symbol in an integer
!     symbol table. The first value associated with the symbol is
!     removed, and subsequent values are moved forward.
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
      integer(ip)           value
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   Name of the symbol whose associated value is to be
!                    popped.
!     TABSYM,
!     TABPTR,
!     TABVAL    I-O  Components of the symbol table.
!     VALUE      O   Value that was popped.
!     FOUND      O   .TRUE. if the symbol exists, .FALSE. otherwise.
!
! $ Detailed_Input
!
!     NAME     is the name of the symbol whose associated value is to
!              be popped.
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
!              On output, the value is removed from the symbol table,
!              and the remaining values associated with the symbol are
!              moved forward in the value table. If no other values are
!              associated with the symbol, the symbol is removed from
!              the symbol table.
!
!     VALUE    is the value that was popped. This value was the first
!              value in the symbol table that was associated with the
!              symbol NAME.
!
!     FOUND    is .TRUE. if NAME is in the symbol table, otherwise it is
!              .FALSE.
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
!     If there are no remaining values associated with the symbol
!     after VALUE has been popped, the symbol is removed from the
!     symbol table.
!
! $ Examples
!
!     The contents of the symbol table are:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  18
!                     12
!        pens    -->  10
!                     12
!                     24
!
!     The call,
!
!        CALL SYPOPI ( 'pens', TABSYM, TABPTR, TABVAL, VALUE, FOUND )
!
!     modifies the contents of the symbol table to be:
!
!        books   -->   5
!        erasers -->   6
!        pencils -->  18
!                     12
!        pens    -->  12
!                     24
!
!     FOUND is .TRUE., and VALUE is 10.
!
!     The next call,
!
!        CALL SYPOPI ( 'erasers', TABSYM, TABPTR, TABVAL, VALUE, FOUND )
!
!     modifies the contents of the symbol table to be:
!
!        books   -->   5
!        pencils -->  18
!                     12
!        pens    -->  12
!                     24
!
!      FOUND is .TRUE., and VALUE is 6. Note that because
!      "erasers" had only one value associated with it, it was removed
!      from the symbol table.
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
!     L.S. Elson         (JPL)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
!
!        PLACEHOLDER: Please provide version description.
!
!-    SPICELIB Version 1.0.2, 10-DEC-2002 (LSE)
!
!        Fixed typo (FISSION') in header
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
!     pop a value from a particular symbol
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

!
!     Standard SPICE error handling.
!
      if ( return () ) return

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
!     If it's not in the table, it's definitely a problem.
!
      if ( locsym == 0 ) then

         found = .false.

!
!     If it is in the table, we can proceed without fear of overflow.
!
      else

         found = .true.

!
!        Begin by saving and removing the initial value for this
!        symbol from the value table.
!
         locval = sumai ( tabptr(1), locsym-1 ) + 1
         value  = tabval(locval)

         call remlai ( 1, locval, tabval(1), nval )
         call scardi ( nval, tabval )

!
!        If this was the sole value for the symbol, remove the
!        symbol from the name and pointer tables. Otherwise just
!        decrement the dimension.
!
         if ( tabptr(locsym) == 1 ) then

            call remlac ( 1, locsym, tabsym(1), nsym )
            call scardc ( nsym, tabsym )

            call remlai ( 1, locsym, tabptr(1), nptr )
            call scardi ( nptr, tabptr )

         else
            tabptr(locsym) = tabptr(locsym) - 1
         end if

      end if

      return
      end
