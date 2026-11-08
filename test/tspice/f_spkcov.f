
C$Procedure      F_SPKCOV ( SPKCOV tests )

      SUBROUTINE F_SPKCOV ( OK )
      IMPLICIT NONE

      LOGICAL               OK

C$ Abstract
C
C     This routine tests the SPICELIB routines
C     
C        SPKCOV
C        SPKOBJ
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Version
C
C-    TSPICE Version 1.0.0, 30-DEC-2004 (NJB)
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

C
C     Local variables
C
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

C
C     Saved variables
C
      SAVE                  BODY
      SAVE                  NSEG

C
C     Initial values
C
      DATA                  BODY  / 4,  5,  6  /
      DATA                  NSEG  / 10, 20, 30 /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_SPKCOV' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  create SPK file.' )


C
C     Create an SPK file with data for three bodies.
C
      CALL SPKOPN ( SPK, SPK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Initialize the expected coverage windows.
C
      DO I = 1, NBOD
         CALL SSIZED ( MAXCOV, XCOVER(LBCELL,I) )
      END DO

      CALL CLEARD ( 12, STATES )

      DO I = 1, NBOD

         DO J = 1, NSEG(I)
C
C           Create segments for body I.
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
C           Add to the expected coverage window for this body.
C
            CALL WNINSD ( FIRST, LAST, XCOVER(LBCELL,I) )
            CALL CHCKXC ( .FALSE., ' ', OK )


            CALL SPKW08 (  HANDLE,  BODY(I), 399,    'J2000',
     .                     FIRST,   LAST,    'TEST', 1,
     .                     2,       STATES,  FIRST,  LAST-FIRST+DELTA )
            CALL CHCKXC ( .FALSE., ' ', OK )

         END DO

      END DO


      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Loop through the canned cases.
C
      DO I = 1, NBOD
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Check coverage for body #.'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

         CALL SSIZED ( MAXCOV, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKCOV ( SPK, BODY(I), COVER )
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


      DO I = 1, NBOD
C
C --- Case: ------------------------------------------------------
C
         TITLE = 'Check coverage for body #; COVER starts out ' //
     .           'non-empty.'
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

         CALL SSIZED ( MAXCOV, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL WNINSD ( 1.D6, 1.D7, COVER )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL SPKCOV ( SPK, BODY(I), COVER )
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
      CALL TCASE ( 'Try to find coverage for a transfer SPK.' )

      CALL TXTOPN ( XFRSPK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DAFBT ( SPK, XUNIT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CLOSE ( XUNIT )


      CALL SPKCOV ( XFRSPK, BODY(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage for a CK.' )

      
      CALL TSTCK3  ( CK, SCLK, .FALSE., .FALSE., .FALSE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCOV ( CK, BODY(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find coverage for an EK.' )

      CALL TSTEK ( EK, 1, 20, .FALSE., HANDLE )      
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCOV ( EK, BODY(1), COVER )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )



C ******************************************************
C ******************************************************
C ******************************************************
C     SPKOBJ tests
C ******************************************************
C ******************************************************
C ******************************************************

C
C --- Case: ------------------------------------------------------
C
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

C
C     Check cardinality of object set.
C
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ), 
     .                 '=',       CARDI( XIDS ),
     .                 0,         OK             )

C
C     Check object set.
C
      CALL CHCKAI ( 'IDS',      IDS(1), 
     .              '=',        XIDS(1),  CARDI(XIDS),   OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Find objects in our test SPK.  Start with ' //
     .             'non-empty ID set.'                          )

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

C
C     Check cardinality of object set.
C
      CALL CHCKSI ( 'CARDI(IDS)', CARDI( IDS  ), 
     .                 '=',       CARDI( XIDS ),
     .                 0,         OK             )

C
C     Check object set.
C
      CALL CHCKAI ( 'IDS',      IDS(1), 
     .              '=',        XIDS(1),  CARDI(XIDS),   OK )
C
C     Error cases:
C

      
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find objects for a transfer SPK.' )


C
C     Initialize the IDS set.
C
      CALL SSIZEI ( NBOD, IDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKOBJ ( XFRSPK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFORMAT)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find objects for a CK.' )

      CALL SPKOBJ ( CK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFILETYPE)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Try to find objects for an EK.' )


      CALL SPKOBJ ( EK, IDS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDARCHTYPE)', OK )




C
C     Clean up.
C
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
