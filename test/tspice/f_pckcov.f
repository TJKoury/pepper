
C$Procedure      F_PCKCOV ( PCKCOV tests )

      SUBROUTINE F_PCKCOV ( OK )
      IMPLICIT NONE

      LOGICAL               OK

C$ Abstract
C
C     This routine tests the SPICELIB routines
C     
C        PCKCOV
C        PCKFRM
C
C$ Version
C
C-    TSPICE Version 1.0.0, 26-NOV-2007 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      INTEGER               CARDD
      INTEGER               CARDI

C
C     Local parameters
C
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

C
C     Local variables
C
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

C
C     Saved variables
C
      SAVE                  FRAME
      SAVE                  NSEG

C
C     Initial values
C
      DATA                  FRAME  / 4,  5,  6  /
      DATA                  NSEG   / 10, 20, 30 /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_PCKCOV' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create PCK file.' )


C
C     Create an PCK file with data for three frames.
C
      CALL PCKOPN ( PCK, PCK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Initialize the expected coverage windows.
C
      DO I = 1, NFRM
         CALL SSIZED ( MAXCOV, XCOVER(LBCELL,I) )
      END DO

      CALL CLEARD ( 6, COEFFS )

      DO I = 1, NFRM

         DO J = 1, NSEG(I)
C
C           Create segments for frame I.
C
            IF ( I .EQ. 1 ) THEN
C
C              Create NSEG(1) segments, each one separated
C              by a 1 second gap.
C
               FIRST = (J-1) * 11.D0
               LAST  = FIRST + 10.D0

            ELSE IF ( I .EQ. 2 ) THEN
C
C              Create NSEG(2) segments, each one separated
C              by a 1 second gap.  This time, create the 
C              segments in decreasing time order.
C
               FIRST = ( NSEG(2) - J ) * 101.D0
               LAST  = FIRST + 100.D0

            ELSE
C
C              I equals 3.
C
C              Create NSEG(3) segments with no gaps.
C
               FIRST = (J-1) * 1000.D0
               LAST  = FIRST + 1000.D0

            END IF

C
C           Add to the expected coverage window for this frame.
C
            CALL WNINSD ( FIRST, LAST, XCOVER(LBCELL,I) )
            CALL CHCKXC ( .FALSE., ' ', OK )

C
C           There's no need for the data to be complicated. Create
C           just one record per segment.
C
            COEFFS(1,1) =  11.D0
            COEFFS(2,1) =  21.D0

            COEFFS(1,2) =  12.D0
            COEFFS(2,2) =  22.D0

            COEFFS(1,3) =  13.D0
            COEFFS(2,3) =  23.D0


            INTLEN = LAST - FIRST

            CALL PCKW02 (  HANDLE,  FRAME(I), 'J2000',
     .                     FIRST,   LAST,     'TEST',   INTLEN,
     .                     1,       1,        COEFFS,   FIRST  )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


      CALL PCKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Loop through the canned cases.
C
      DO I = 1, NFRM
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Check coverage for frame #.'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

         CALL SSIZED ( MAXCOV, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PCKCOV ( PCK, FRAME(I), COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check cardinality of coverage window.
C
         CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER ), 
     .                 '=',            CARDD( XCOVER(LBCELL,I) ),
     .                 0,              OK                         )

C
C        Check coverage window.
C
         CALL CHCKAD ( 'COVER', COVER(1), 
     .                 '=',     XCOVER(1,I),  CARDD(COVER),
     .                 0.D0,    OK                          )
         
      END DO


C
C     Loop through the canned cases.  This time, use a coverage 
C     window that already contains data.
C      
      CALL SCARDD ( 0, COVER )


      DO I = 1, NFRM
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Check coverage for frame #; COVER starts out ' //
     .           'non-empty.'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

         CALL SSIZED ( MAXCOV, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL WNINSD ( 1.D6, 1.D7, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL PCKCOV ( PCK, FRAME(I), COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Check cardinality of coverage window.
C
         CALL WNINSD ( 1.D6, 1.D7, XCOVER(LBCELL,I) )

         CALL CHCKSI ( 'CARDD(COVER)', CARDD( COVER ), 
     .                 '=',            CARDD( XCOVER(LBCELL,I) ),
     .                 0,              OK                        )

C
C        Check coverage window.  
C         
         CALL CHCKAD ( 'COVER', COVER(1), 
     .                 '=',     XCOVER(1,I),  CARDD(COVER),
     .                 0.D0,    OK                          )
         
      END DO


C
C     Error cases:
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage for a transfer PCK.' )

      CALL TXTOPN ( XFRPCK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBT ( PCK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CLOSE ( XUNIT )


      CALL PCKCOV ( XFRPCK, FRAME(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage for a CK.' )

      
      CALL TSTCK3  ( CK, SCLK, .FALSE., .FALSE., .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCKCOV ( CK, FRAME(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage for an SPK.' )

      
      CALL TSTSPK ( SPK, .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCKCOV ( SPK, FRAME(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage for an EK.' )

      CALL TSTEK ( EK, 1, 20, .FALSE., HANDLE )      
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCKCOV ( EK, FRAME(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )



C ******************************************************
C ******************************************************
C ******************************************************
C     PCKFRM tests
C ******************************************************
C ******************************************************
C ******************************************************

C
C --- Case: ------------------------------------------------------
C
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

C
C     Check cardinality of frame set.
C
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ), 
     .                 '=',       CARDI( XIDS ),
     .                 0,         OK             )

C
C     Check frame set.
C
      CALL CHCKAI ( 'IDS',      IDS(1), 
     .              '=',        XIDS(1),  CARDI(XIDS),   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Find frames in our test PCK.  Start with ' //
     .             'non-empty ID set.'                          )

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

C
C     Check cardinality of frame set.
C
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ), 
     .                 '=',       CARDI( XIDS ),
     .                 0,         OK             )

C
C     Check frame set.
C
      CALL CHCKAI ( 'IDS',      IDS(1), 
     .              '=',        XIDS(1),  CARDI(XIDS),   OK )
C
C     Error cases:
C

      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find frames for a transfer PCK.' )


C
C     Initialize the IDS set.
C
      CALL SSIZEI ( NFRM, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCKFRM ( XFRPCK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find frames for a CK.' )

      CALL PCKFRM ( CK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find frames for an EK.' )


      CALL PCKFRM ( EK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find frames for an SPK.' )


      CALL PCKFRM ( SPK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )




C
C     Clean up.
C
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
