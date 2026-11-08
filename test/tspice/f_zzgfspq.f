C$Procedure F_ZZGFSPQ ( ZZGFSPQ family tests )

      SUBROUTINE F_ZZGFSPQ ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        ZZGFSPQ
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
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the low-level SPICELIB
C     geometry routine ZZGFSPQ.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     E.D. Wright     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.1, 13-DEC-2011 (EDW)
C
C        Deleted extraneous trailing whitespace.
C
C-    TSPICE Version 1.0.0, 08-MAR-2009 (EDW)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      VSEP
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      VNORM



C
C     Local parameters
C
      CHARACTER*(*)         LSK
      PARAMETER           ( LSK    = 'gfsep.tls' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck' )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT = 1.D-12 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

C
C     ATOL is a tolerance value for computing arc sine.
C

      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL  = 1.D-12 )


C
C     Local variables
C
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    REF
      CHARACTER*(LNSIZE)    CORR   ( NCORR )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    TIME0

      DOUBLE PRECISION      XR   ( 2, 4)
      DOUBLE PRECISION      PV1  ( 3 )
      DOUBLE PRECISION      PV2  ( 3 )
      DOUBLE PRECISION      RADA ( 3 )
      DOUBLE PRECISION      RADB ( 3 )
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      ANG1
      DOUBLE PRECISION      ANG2
      DOUBLE PRECISION      RANGE1
      DOUBLE PRECISION      RANGE2

      INTEGER               HAN1
      INTEGER               DIM
      INTEGER               TARG1
      INTEGER               TARG2
      INTEGER               OBS
      INTEGER               I
      INTEGER               J
      INTEGER               K


C
C     Save all.
C
      SAVE

C
C     Initial values
C
      DATA                  CORR / 'NONE',
     .                             'lt',  ' lt+s',  ' cn',  ' cn + s',
     .                             'XLT', 'XLT + S', 'XCN', 'XCN+S'   /



C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFSPQ' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

C
C     Create an LSK, load using FURNSH.
C
      CALL ZZTSTLSK( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( LSK )
      CALL CHCKXC( .FALSE., ' ', OK )


C
C     Create the PCK for Nat's Solar System.
C
      CALL NATPCK ( PCK1,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK1 )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create the SPK for Nat's Solar System.
C
      CALL NATSPK ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C     Create a confinement window from ET0 and ET1.
C
      TIME0 = '2000 JAN 1  12:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Case 1
C
      CALL TCASE ( 'Ephemeris data unavailable' )

      TARG1 = -203
      TARG2 = 499
      OBS   = 10
      R1    = 0.D0
      R2    = 0.D0
      REF   = 'J2000'
      ABCORR= 'NONE'

      CALL ZZGFSPQ ( ET0, TARG1, TARG2, R1, R2,
     .               OBS, ABCORR, REF, VALUE )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )


C
C     Case 2
C
      CALL TCASE ( 'Negative body radius R1 and R2' )

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      TARG1 = 1000
      TARG2 = 2000
      OBS   = 10
      R1    = -RADA(1)
      R2    = 0.D0
      REF   = 'J2000'
      ABCORR= 'NONE'

      CALL ZZGFSPQ ( ET0, TARG1, TARG2, R1, R2,
     .               OBS, ABCORR, REF, VALUE )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUS)',   OK )

      R1    = 0.D0
      R2    = -RADB(1)

      CALL ZZGFSPQ ( ET0, TARG1, TARG2, R1, R2,
     .               OBS, ABCORR, REF, VALUE )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUS)',   OK )



C
C     Case 2
C

C
C     Check a simple calculation for all CORR.
C
      TARG1 = 1000
      TARG2 = 2000
      OBS   = 10
      R1    = 0.D0
      R2    = 0.D0
      REF   = 'J2000'
      ABCORR= 'NONE'

      DO I = 1, NCORR

         CALL TCASE ( CORR(I) )

         CALL ZZGFSPQ ( ET0, TARG1, TARG2, R1, R2,
     .                  OBS, CORR(I), REF, VALUE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


C
C     Check a simple calculation for invalid CORR.
C
      DO I = 1, NCORR

         ABCORR = 'Z' // CORR(I)(:LNSIZE-1)

         CALL TCASE ( ABCORR )

         CALL ZZGFSPQ ( ET0, TARG1, TARG2, R1, R2,
     .                  OBS, ABCORR, REF, VALUE )

         CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)', OK )

      END DO


C
C     Case 3
C

C
C     Run a loop over one orbit of ALPHA at 50 TDB second
C     intervals with each of the aborration corrections.
C     Any occultation event exists for longer than 50 seconds.
C     Compare the value returned from ZZGFSPQ with a direct
C     calculation.
C
      TARG1 = 1000
      TARG2 = 2000
      OBS   = 10
      REF   = 'J2000'

      XR(1,1) = 0.D0
      XR(1,2) = RADA(1)
      XR(1,3) = 0.D0
      XR(1,4) = RADA(1)
      XR(2,1) = 0.D0
      XR(2,2) = 0.D0
      XR(2,3) = RADB(1)
      XR(2,4) = RADB(1)


      DO I=0, 86400, 50

         DO J = 1, NCORR

            ABCORR = CORR(J)

            DO K = 1, 4

               R1 = XR(1,K)
               R2 = XR(2,K)

               TITLE = 'ET = #1, R1 = #2, R2 = #3, CORR = #4'
               CALL REPMD ( TITLE, '#1', ET, 6, TITLE )
               CALL REPMD ( TITLE, '#2', R1, 6, TITLE )
               CALL REPMD ( TITLE, '#3', R2, 6, TITLE )
               CALL REPMI ( TITLE, '#4', J,     TITLE )
               CALL TCASE( TITLE )

               ET = ET0 + DBLE(I)

               CALL ZZGFSPQ ( ET, TARG1, TARG2, R1, R2,
     .                        OBS, ABCORR, REF, VALUE )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL SPKEZP ( TARG1, ET, REF, ABCORR, OBS, PV1, LT )
               CALL SPKEZP ( TARG2, ET, REF, ABCORR, OBS, PV2, LT )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               RANGE1 = VNORM( PV1 )
               RANGE2 = VNORM( PV2 )
               ANG1   = DASINE( R1/RANGE1, ATOL )
               ANG2   = DASINE( R2/RANGE2, ATOL )

               THETA = VSEP ( PV1, PV2 ) - ANG1 - ANG2
C
C              The results should match to roundoff.
C
               CALL CHCKSD ( TITLE, VALUE, '=', THETA, TIGHT, OK )

            END DO

         END DO

      END DO



C
C Case n
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END

C
C23456789012345678901234567890123456789012345678901234567890123456789012
C
