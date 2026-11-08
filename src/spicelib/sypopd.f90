!$Procedure SYPOPD ( Pop a value from a particular symbol )

      subroutine sypopd ( name, tabsym, tabptr, tabval, value, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Pop a value associated with a particular symbol in a double
!     precision symbol table. The first value associated with the
!     symbol is removed, and subsequent values are moved forward.
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
      character(*)          tabsym     ( lbcell:* )
      integer(ip)           tabptr     ( lbcell:* )
      real(wp)              tabval     ( lbcell:* )
      real(wp)              value
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
!     TABVAL   are the components of a double precision symbol table.
!
! $ Detailed_Output
!
!     TABSYM,
!     TABPTR,
!     TABVAL   are the components of a double precision symbol table.
!
!              The value is removed from the symbol table, and the
!              remaining values associated with the symbol are moved
!              forward in the value table. If no other values are
!              associated with the symbol, the symbol is removed from
!              the symbol table.
!
!     VALUE    is the value that was popped. This value was the first
!              value in the symbol table that was associated with the
!              symbol NAME.
!
!     FOUND    is .TRUE. if NAME is in the symbol table, otherwise
!              it is .FALSE.
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
!        BODY4_POLE_RA -->    3.17681D2
!                             1.08D-1
!                             0.0D0
!        DELTA_T_A     -->    3.2184D1
!        K             -->    1.657D-3
!        MEAN_ANOM     -->    6.239996D0
!                             1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!
!     The call,
!
!        CALL SYPOPD ( 'MEAN_ANOM', TABSYM, TABPTR, TABVAL,
!       .                                   VALUE,  FOUND   )
!
!     modifies the contents of the symbol table to be:
!
!        BODY4_POLE_RA -->    3.17681D2
!                             1.08D-1
!                             0.0D0C
!        DELTA_T_A     -->    3.2184D1
!        K             -->    1.657D-3
!        MEAN_ANOM     -->    1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!
!     FOUND is .TRUE., and VALUE is 6.239996D0.
!
!
!     The next call,
!
!        CALL SYPOPD ( 'K', TABSYM, TABPTR, TABVAL, VALUE, FOUND )
!
!     modifies the contents of the symbol table to be:
!
!        BODY4_POLE_RA -->    3.17681D2
!                             1.08D-1
!                             0.0D0C
!        DELTA_T_A     -->    3.2184D1
!        MEAN_ANOM     -->    1.99096871D-7
!        ORBIT_ECC     -->    1.671D-2
!
!      FOUND is .TRUE., and VALUE is  1.657D-3. Note that because
!      "K" had only one value associated with it, it was removed
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
!     pop a value from a particular symbol
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           bsrchc
      integer(ip)           cardc
      integer(ip)           cardd
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
      nval = cardd ( tabval )

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

         call remlad ( 1, locval, tabval(1), nval )
         call scardd ( nval, tabval )

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
