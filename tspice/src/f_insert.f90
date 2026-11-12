!$Procedure F_INSERT ( SPICE set insertion tests )

      SUBROUTINE F_INSERT ( OK )
      use pepper_legacy

! $ Abstract
!
!     Exercise SPICELIB set insertion routines.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Particulars
!
!     This routine tests the SPICELIB set insertion routines:
!
!        INSRTC
!        INSRTD
!        INSRTI
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 09-NOV-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )

      INTEGER               ITMWID
      PARAMETER           ( ITMWID = 30 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               CSIZE
      PARAMETER           ( CSIZE  = 300 )

      INTEGER               DSIZE
      PARAMETER           ( DSIZE  = 200 )

      INTEGER               ISIZE
      PARAMETER           ( ISIZE  = 100 )


!
!     Local Variables
!
      CHARACTER*(ITMWID)    CSET   ( LBCELL : CSIZE )
      CHARACTER*(ITMWID)    CVALS  ( CSIZE+1 )
      CHARACTER*(LNSIZE)    LNVALS ( CSIZE )
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(ITMWID)    XITEMC ( CSIZE )

      DOUBLE PRECISION      DSET   ( LBCELL : DSIZE )
      DOUBLE PRECISION      XITEMD ( DSIZE )

      INTEGER               CARD
      INTEGER               I
      INTEGER               ISET   ( LBCELL : ISIZE )
      INTEGER               N
      INTEGER               XITEMI ( ISIZE )

!
!     Saved values
!

!
!     Initial values
!



!
!     Open the test family.
!
      CALL TOPEN ( 'F_INSERT' )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTI test:  insert distinct elements in order.'

      CALL TCASE ( TITLE )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = 1, CARD

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMI( I ) = I

      END DO

      N = CARDI ( ISET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAI ( 'ITEMS', ISET(1), '=', XITEMI, N, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTI test:  insert non-distinct elements in order.'

      CALL TCASE ( TITLE )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = 1, CARD

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMI( I ) = I

      END DO

      N = CARDI ( ISET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAI ( 'ITEMS', ISET(1), '=', XITEMI, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTI test:  insert distinct elements in reverse ' // &
              'order.'

      CALL TCASE ( TITLE )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = CARD, 1, -1

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMI( I ) = I

      END DO

      N = CARDI ( ISET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAI ( 'ITEMS', ISET(1), '=', XITEMI, N, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTI test:  insert non-distinct elements in reverse '// &
              'order.'

      CALL TCASE ( TITLE )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = CARD, 1, -1

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMI( I ) = I

      END DO

      N = CARDI ( ISET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAI ( 'ITEMS', ISET(1), '=', XITEMI, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTI test:  insert non-distinct elements in non-'// &
              'sequential order.'

      CALL TCASE ( TITLE )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = 1, CARD

         CALL INSRTI ( CARD+1-I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMI( I ) = I

      END DO

      N = CARDI ( ISET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAI ( 'ITEMS', ISET(1), '=', XITEMI, N, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INSRTI error case:  set overflow.' )

      CALL SSIZEI ( ISIZE, ISET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = ISIZE

      DO I = 1, CARD

         CALL INSRTI ( I, ISET )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL INSRTI ( CARD+1, ISET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETEXCESS)', OK )



!*************************************************************
!
!
!     INSRTD tests follow.
!
!
!*************************************************************


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTD test:  insert distinct elements in order.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = 1, CARD

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMD( I ) = I

      END DO

      N = CARDD ( DSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAD ( 'ITEMS', DSET(1), '=', XITEMD, N, 0.D0, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTD test:  insert non-distinct elements in order.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = 1, CARD

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMD( I ) = I

      END DO

      N = CARDD ( DSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAD ( 'ITEMS', DSET(1), '=', XITEMD, N, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTD test:  insert distinct elements in reverse ' // &
              'order.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = CARD, 1, -1

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMD( I ) = I

      END DO

      N = CARDD ( DSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAD ( 'ITEMS', DSET(1), '=', XITEMD, N, 0.D0, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTD test:  insert non-distinct elements in reverse '// &
              'order.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = CARD, 1, -1

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMD( I ) = I

      END DO

      N = CARDD ( DSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAD ( 'ITEMS', DSET(1), '=', XITEMD, N, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTD test:  insert non-distinct elements in non-'// &
              'sequential order.'

      CALL TCASE ( TITLE )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = 1, CARD

         CALL INSRTD ( DBLE(CARD+1-I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMD( I ) = I

      END DO

      N = CARDD ( DSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAD ( 'ITEMS', DSET(1), '=', XITEMD, N, 0.D0, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INSRTD error case:  set overflow.' )

      CALL SSIZED ( DSIZE, DSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = DSIZE

      DO I = 1, CARD

         CALL INSRTD ( DBLE(I), DSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL INSRTD ( DBLE(CARD+1), DSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETEXCESS)', OK )



!*************************************************************
!
!
!     INSRTC tests follow.
!
!
!*************************************************************



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTC test:  insert distinct elements in order.'

      CALL TCASE ( TITLE )

!
!     Create an array of character set values to be used in place
!     of the numeric values used in the preceding tests.
!
      DO I = 1, CSIZE
!
!        See the description of T_ITHSYM at the bottom of this file.
!
         CALL T_ITHSYM ( I, CSIZE+1, CVALS(I) )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        For each element of CVALS, create a longer string that agrees
!        with the first ITMWID characters of the element.
!
         LNVALS(I) = CVALS(I)

         CALL SUFFIX ( 'YYY', 0, LNVALS(I) )

      END DO

!
!     Create one extra element of CVALS, which will be used later.
!
      CALL T_ITHSYM ( CSIZE+1, CSIZE+1, CVALS(CSIZE+1) )


      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTC test:  insert non-distinct elements in order.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTC test:  insert non-distinct elements in order. '// &
              'This time use duplicate elements that disagree with ' // &
              'the set elements only at positions past the string '  // &
              'length of the set.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTC ( LNVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTC test:  insert distinct elements in reverse ' // &
              'order.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = CARD, 1, -1

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )



!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTC test:  insert non-distinct elements in reverse '// &
              'order. '                                               // &
              'This time use duplicate elements that disagree with '  // &
              'the set elements only at positions past the string '   // &
              'length of the set.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = CARD, 1, -1

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTC ( LNVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTC test:  insert non-distinct elements in non-'// &
              'sequential order.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(CARD+1-I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'INSRTC test:  insert non-distinct elements in non-'    // &
              'sequential order. '                                    // &
              'This time use duplicate elements that disagree with '  // &
              'the set elements only at positions past the string '   // &
              'length of the set.'

      CALL TCASE ( TITLE )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(CARD+1-I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL INSRTC ( LNVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

         XITEMC( I ) = CVALS(I)

      END DO

      N = CARDC ( CSET )

      CALL CHCKSI ( 'N', N, '=', CARD, 0, OK )

      CALL CHCKAC ( 'ITEMS', CSET(1), '=', XITEMC, N, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'INSRTC error case:  set overflow.' )

      CALL SSIZEC ( CSIZE, CSET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CARD = CSIZE

      DO I = 1, CARD

         CALL INSRTC ( CVALS(I), CSET )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END DO

      CALL INSRTC ( CVALS(CARD+1), CSET )
      CALL CHCKXC ( .TRUE., 'SPICE(SETEXCESS)', OK )



!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END



!
!     The utility routine T_ITHSYM creates the name of the ith
!     symbol.  The names created by this routine have the form
!
!        XXX...XXXnnn
!
!     where nnn is the input number N, printed with a sufficient
!     number of leading zeros so that all numbers in the range
!     1:NMAX have the same width.  The number is padded with
!     leading 'X' characters up to the length of the string NAME.
!
!     The calling sequence of the utility is:
!
!        SUBROUTINE T_ITHSYM ( N, NMAX, NAME )
!
!     This utility is defined in the file
!
!        f_symtbi.for
!
