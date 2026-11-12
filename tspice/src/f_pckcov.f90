
!$Procedure      F_PCKCOV ( PCKCOV tests )

      SUBROUTINE F_PCKCOV ( OK )
      use pepper_legacy, pepper_legacy_frame => frame
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the SPICELIB routines
!
!        PCKCOV
!        PCKFRM
!
! $ Version
!
!-    TSPICE Version 1.0.0, 26-NOV-2007 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local parameters
!
      CHARACTER*(*)         CK
      PARAMETER           ( CK     = 'pckcov.bc' )

      CHARACTER*(*)         EK
      PARAMETER           ( EK     = 'pckcov.bes' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'pckcov.bpc' )

      CHARACTER*(*)         SCLK
      PARAMETER           ( SCLK   = 'pckcov.tsc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'pckcov.bsp' )

      CHARACTER*(*)         XFRPCK
      PARAMETER           ( XFRPCK = 'pckcov.xsp' )


      DOUBLE PRECISION      DELTA
      PARAMETER           ( DELTA  = 1.D-6 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NFRM
      PARAMETER           ( NFRM   = 3 )

      INTEGER               MAXCOV
      PARAMETER           ( MAXCOV = 1000 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      COVER  ( LBCELL : MAXCOV )
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      INTLEN
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      COEFFS ( 2, 3 )
      DOUBLE PRECISION      XCOVER ( LBCELL : MAXCOV, NFRM )

      INTEGER               FRAME  ( NFRM )
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               IDS    ( LBCELL : NFRM+1 )
      INTEGER               J
      INTEGER               NSEG   ( NFRM )
      INTEGER               XIDS   ( LBCELL : NFRM+1 )
      INTEGER               XUNIT

!
!     Saved variables
!
      SAVE                  FRAME
      SAVE                  NSEG

!
!     Initial values
!
      DATA                  FRAME  / 4,  5,  6  /
      DATA                  NSEG   / 10, 20, 30 /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_PCKCOV' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create PCK file.' )


!
!     Create an PCK file with data for three frames.
!
      CALL PCKOPN ( PCK, PCK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Initialize the expected coverage windows.
!
      DO I = 1, NFRM
         CALL SSIZED ( MAXCOV, XCOVER(LBCELL,I) )
      END DO

      CALL CLEARD ( 6, COEFFS )

      DO I = 1, NFRM

         DO J = 1, NSEG(I)
!
!           Create segments for frame I.
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
!           Add to the expected coverage window for this frame.
!
            CALL WNINSD ( FIRST, LAST, XCOVER(LBCELL,I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           There's no need for the data to be complicated. Create
!           just one record per segment.
!
            COEFFS(1,1) =  11.D0
            COEFFS(2,1) =  21.D0

            COEFFS(1,2) =  12.D0
            COEFFS(2,2) =  22.D0

            COEFFS(1,3) =  13.D0
            COEFFS(2,3) =  23.D0


            INTLEN = LAST - FIRST

            CALL PCKW02 (  HANDLE,  FRAME(I), 'J2000', &
                           FIRST,   LAST,     'TEST',   INTLEN, &
                           1,       1,        COEFFS,   FIRST  )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


      CALL PCKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Loop through the canned cases.
!
      DO I = 1, NFRM
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Check coverage for frame #.'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

         CALL SSIZED ( MAXCOV, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PCKCOV ( PCK, FRAME(I), COVER )
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


      DO I = 1, NFRM
!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Check coverage for frame #; COVER starts out ' // &
                 'non-empty.'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

         CALL SSIZED ( MAXCOV, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL WNINSD ( 1.D6, 1.D7, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PCKCOV ( PCK, FRAME(I), COVER )
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
      CALL TCASE ( 'Try to find coverage for a transfer PCK.' )

      CALL TXTOPN ( XFRPCK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBT ( PCK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CLOSE ( XUNIT )


      CALL PCKCOV ( XFRPCK, FRAME(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage for a CK.' )


      CALL TSTCK3  ( CK, SCLK, .FALSE., .FALSE., .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCKCOV ( CK, FRAME(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage for an SPK.' )


      CALL TSTSPK ( SPK, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCKCOV ( SPK, FRAME(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find coverage for an EK.' )

      CALL TSTEK ( EK, 1, 20, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCKCOV ( EK, FRAME(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )



! ******************************************************
! ******************************************************
! ******************************************************
!     PCKFRM tests
! ******************************************************
! ******************************************************
! ******************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find frames in our test PCK.' )

      CALL SSIZEI ( NFRM, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( NFRM, XIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NFRM
         CALL INSRTI ( FRAME(I), XIDS )
      END DO

      CALL PCKFRM ( PCK, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check cardinality of frame set.
!
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ), &
                       '=',       CARDI( XIDS ), &
                       0,         OK             )

!
!     Check frame set.
!
      CALL CHCKAI ( 'IDS',      IDS(1), &
                    '=',        XIDS(1),  CARDI(XIDS),   OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Find frames in our test PCK.  Start with ' // &
                   'non-empty ID set.'                          )

      CALL SSIZEI ( NFRM+1, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZEI ( NFRM+1, XIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INSRTI ( -1000000, XIDS )

      DO I = 1, NFRM
         CALL INSRTI ( FRAME(I), XIDS )
      END DO

      CALL INSRTI ( -1000000, IDS )

      CALL PCKFRM ( PCK, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check cardinality of frame set.
!
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ), &
                       '=',       CARDI( XIDS ), &
                       0,         OK             )

!
!     Check frame set.
!
      CALL CHCKAI ( 'IDS',      IDS(1), &
                    '=',        XIDS(1),  CARDI(XIDS),   OK )
!
!     Error cases:
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find frames for a transfer PCK.' )


!
!     Initialize the IDS set.
!
      CALL SSIZEI ( NFRM, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCKFRM ( XFRPCK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find frames for a CK.' )

      CALL PCKFRM ( CK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find frames for an EK.' )


      CALL PCKFRM ( EK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Try to find frames for an SPK.' )


      CALL PCKFRM ( SPK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )




!
!     Clean up.
!
      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( XFRPCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( CK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( EK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )
      RETURN
      END
