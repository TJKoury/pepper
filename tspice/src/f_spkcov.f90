
!$Procedure      F_SPKCOV ( SPKCOV tests )

      SUBROUTINE F_SPKCOV ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the SPICELIB routines
!
!        SPKCOV
!        SPKOBJ
!
! $ Version
!
!-    TSPICE Version 1.0.0, 30-DEC-2004 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local parameters
!
      CHARACTER*(*)         CK
      PARAMETER           ( CK     = 'spkcov.bc' )

      CHARACTER*(*)         EK
      PARAMETER           ( EK     = 'spkcov.bes' )

      CHARACTER*(*)         SCLK
      PARAMETER           ( SCLK   = 'spkcov.tsc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'spkcov.bsp' )

      CHARACTER*(*)         XFRSPK
      PARAMETER           ( XFRSPK = 'spkcov.xsp' )


      DOUBLE PRECISION      DELTA
      PARAMETER           ( DELTA  = 1.D-6 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NBOD
      PARAMETER           ( NBOD   = 3 )

      INTEGER               MAXCOV
      PARAMETER           ( MAXCOV = 1000 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      COVER  ( LBCELL : MAXCOV )
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      STATES ( 6, 2 )
      DOUBLE PRECISION      XCOVER ( LBCELL : MAXCOV, NBOD )

      INTEGER               BODY   ( NBOD )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IDS    ( LBCELL : NBOD+1 )
      INTEGER               J
      INTEGER               NSEG   ( NBOD )
      INTEGER               XIDS   ( LBCELL : NBOD+1 )
      INTEGER               XUNIT

!
!     Saved variables
!
      SAVE                  BODY
      SAVE                  NSEG

!
!     Initial values
!
      DATA                  BODY  / 4,  5,  6  /
      DATA                  NSEG  / 10, 20, 30 /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SPKCOV' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create SPK file.' )


!
!     Create an SPK file with data for three bodies.
!
      CALL SPKOPN ( SPK, SPK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Initialize the expected coverage windows.
!
      DO I = 1, NBOD
         CALL SSIZED ( MAXCOV, XCOVER(LBCELL,I) )
      END DO

      CALL CLEARD ( 12, STATES )

      DO I = 1, NBOD

         DO J = 1, NSEG(I)
!
!           Create segments for body I.
!
            IF ( I .EQ. 1 ) THEN
!
!              Create NSEG(1) segments, each one separated
!              by a 1 second gap.
!
               FIRST = (J-1) * 11.D0
               LAST  = FIRST + 10.D0

            ELSE IF ( I .EQ. 2 ) THEN
!
!              Create NSEG(2) segments, each one separated
!              by a 1 second gap.  This time, create the
!              segments in decreasing time order.
!
               FIRST = ( NSEG(2) - J ) * 101.D0
               LAST  = FIRST + 100.D0

            ELSE
!
!              I equals 3.
!
!              Create NSEG(3) segments with no gaps.
!
               FIRST = (J-1) * 1000.D0
               LAST  = FIRST + 1000.D0

            END IF

!
!           Add to the expected coverage window for this body.
!
            CALL WNINSD ( FIRST, LAST, XCOVER(LBCELL,I) )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL SPKW08 (  HANDLE,  BODY(I), 399,    'J2000', &
                           FIRST,   LAST,    'TEST', 1, &
                           2,       STATES,  FIRST,  LAST-FIRST+DELTA )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Loop through the canned cases.
!
      DO I = 1, NBOD
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Check coverage for body #.'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

         CALL SSIZED ( MAXCOV, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKCOV ( SPK, BODY(I), COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check cardinality of coverage window.
!
         CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER ), &
                       '=',            CARDD( XCOVER(LBCELL,I) ), &
                       0,              OK                         )

!
!        Check coverage window.
!
         CALL CHCKAD ( 'COVER', COVER(1), &
                       '=',     XCOVER(1,I),  CARDD(COVER), &
                       0.D0,    OK                          )

      END DO


!
!     Loop through the canned cases.  This time, use a coverage
!     window that already contains data.
!
      CALL SCARDD ( 0, COVER )


      DO I = 1, NBOD
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Check coverage for body #; COVER starts out ' // &
                 'non-empty.'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

         CALL SSIZED ( MAXCOV, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL WNINSD ( 1.D6, 1.D7, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKCOV ( SPK, BODY(I), COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Check cardinality of coverage window.
!
         CALL WNINSD ( 1.D6, 1.D7, XCOVER(LBCELL,I) )

         CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER ), &
                       '=',            CARDD( XCOVER(LBCELL,I) ), &
                       0,              OK                        )

!
!        Check coverage window.
!
         CALL CHCKAD ( 'COVER', COVER(1), &
                       '=',     XCOVER(1,I),  CARDD(COVER), &
                       0.D0,    OK                          )

      END DO


!
!     Error cases:
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage for a transfer SPK.' )

      CALL TXTOPN ( XFRSPK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBT ( SPK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLOSE ( XUNIT )


      CALL SPKCOV ( XFRSPK, BODY(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage for a CK.' )


      CALL TSTCK3  ( CK, SCLK, .FALSE., .FALSE., .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCOV ( CK, BODY(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage for an EK.' )

      CALL TSTEK ( EK, 1, 20, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCOV ( EK, BODY(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )



! ******************************************************
! ******************************************************
! ******************************************************
!     SPKOBJ tests
! ******************************************************
! ******************************************************
! ******************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find objects in our test SPK.' )

      CALL SSIZEI ( NBOD, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( NBOD, XIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NBOD
         CALL INSRTI ( BODY(I), XIDS )
      END DO

      CALL SPKOBJ ( SPK, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check cardinality of object set.
!
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ), &
                       '=',       CARDI( XIDS ), &
                       0,         OK             )

!
!     Check object set.
!
      CALL CHCKAI ( 'IDS',      IDS(1), &
                    '=',        XIDS(1),  CARDI(XIDS),   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find objects in our test SPK.  Start with ' // &
                   'non-empty ID set.'                          )

      CALL SSIZEI ( NBOD+1, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( NBOD+1, XIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INSRTI ( -1000000, XIDS )

      DO I = 1, NBOD
         CALL INSRTI ( BODY(I), XIDS )
      END DO

      CALL INSRTI ( -1000000, IDS )

      CALL SPKOBJ ( SPK, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check cardinality of object set.
!
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ), &
                       '=',       CARDI( XIDS ), &
                       0,         OK             )

!
!     Check object set.
!
      CALL CHCKAI ( 'IDS',      IDS(1), &
                    '=',        XIDS(1),  CARDI(XIDS),   OK )
!
!     Error cases:
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find objects for a transfer SPK.' )


!
!     Initialize the IDS set.
!
      CALL SSIZEI ( NBOD, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKOBJ ( XFRSPK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find objects for a CK.' )

      CALL SPKOBJ ( CK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find objects for an EK.' )


      CALL SPKOBJ ( EK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )




!
!     Clean up.
!
      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( XFRSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( EK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
