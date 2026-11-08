C$Procedure F_GFSEP ( GFSEP family tests )

      SUBROUTINE F_GFSEP ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFSEP
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the higher-level SPICELIB
C     geometry routine GFSEP.
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
C     J. Diaz del Rio    (ODC Space)
C     E.D. Wright        (JPL)
C
C$ Version
C
C-    TSPICE Version 1.4.0, 29-MAY-2021 (EDW) (JDR)
C
C        Replaced calls to routine ZZGFSPQ with function ZZSEPQ.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 1.3.0, 28-JAN-2017 (EDW)
C
C        Modified GFSTOL test case to correspond to change
C        in ZZGFSOLVX.
C
C-    TSPICE Version 1.2.0, 20-NOV-2012 (EDW)
C
C        Added test on GFSTOL use.
C
C-    TSPICE Version 1.1.0, 07-JUN-2010 (EDW)
C
C        Minor edit to code comments eliminating typo.
C
C        Edits to code to improve structure and readability.
C
C-    TSPICE Version 1.0.0, 17-DEC-2008 (EDW)
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      ZZSEPQ

      INTEGER               WNCARD


C
C     Local parameters
C
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'gfsep.pck' )

      CHARACTER*(*)         LSK
      PARAMETER           ( LSK    = 'gfsep.tls' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck' )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-8 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 10000 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )
      CHARACTER*(LNSIZE)    SHAPES ( 2 )
      CHARACTER*(LNSIZE)    TARG1
      CHARACTER*(LNSIZE)    TARG2
      CHARACTER*(LNSIZE)    SHAPE1
      CHARACTER*(LNSIZE)    SHAPE2
      CHARACTER*(LNSIZE)    FRAME1
      CHARACTER*(LNSIZE)    FRAME2
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    TIME0
      CHARACTER*(TIMLEN)    TIME1

      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      POSA   (3)
      DOUBLE PRECISION      POSB   (3)
      DOUBLE PRECISION      RADA   (3)
      DOUBLE PRECISION      RADB   (3)
      DOUBLE PRECISION      HANGA
      DOUBLE PRECISION      HANGB
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      PI_
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      END
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWSEP )
      DOUBLE PRECISION      SEP

      INTEGER               HAN1
      INTEGER               I
      INTEGER               J
      INTEGER               COUNT
      INTEGER               DIM


C
C     Save everything
C
      SAVE


C
C     Initial values
C
      DATA                 CORR  / 'NONE',
     .                             'lt',  ' lt+s',  ' cn',  ' cn + s',
     .                             'XLT', 'XLT + S', 'XCN', 'XCN+S'   /

      DATA                 SHAPES / 'POINT', 'SPHERE'   /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFSEP' )

      PI_ = PI()

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

C
C     Create an LSK, load using FURNSH.
C
      CALL ZZTSTLSK( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( LSK )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create a PCK, load using FURNSH.
C
      CALL ZZTSTPCK( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
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
      TIME0 = '2000 JAN 1  00:00:00 TDB'
      TIME1 = '2000 APR 1  00:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C    Error cases
C

C
C   Case 1
C
      CALL TCASE ( 'Non-positive step size' )

      STEP   = 0.D0
      ADJUST = 0.D0
      TARG1  = 'MOON'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'IAU_MOON'
      TARG2  = 'EARTH'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'SUN',   '=',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )



C
C   Case 2
C
      CALL TCASE ( 'Non unique body IDs.' )

      STEP   = SPD()
      TARG1  = 'MOON'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'IAU_MOON'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'SUN',   '=',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'MOON',   '=',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'EARTH',   '=',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



C
C   Case 3
C
      CALL TCASE ( 'Invalid aberration correction specifier' )

      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              'X',     'SUN',   '=',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



C
C   Case 4
C
      CALL TCASE ( 'Invalid relations operator' )

      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'SUN',   '==',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )



C
C   Case 5
C
      CALL TCASE ( 'Invalid body names' )

      TARG1  = 'X'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'SUN', 'ABSMAX',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'X'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'MOON', 'ABSMAX',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'X', 'ABSMAX',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )



C
C     Case 6
C
      CALL TCASE ( 'Negative adjustment value' )

      ADJUST = -1.0D0
      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'SUN', 'ABSMAX',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )



C
C     Case 7
C
      CALL TCASE ( 'Ephemeris data unavailable' )

      ADJUST = 0.0D0

C
C     A vehicle/probe has a point shape so frame name not processed.
C
      TARG1  = 'DAWN'
      SHAPE1 = 'POINT'
      FRAME1 = 'NULL'
      TARG2  = 'MARS'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MARS'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'SUN',  'ABSMAX',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )



C
C   Case 8
C
      CALL TCASE ( 'Unknown shape name.' )

      STEP   = SPD()
      TARG1  = 'MOON'
      SHAPE1 = 'PANCAKE'
      FRAME1 = 'NULL'
      TARG2  = 'EARTH'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'SUN',   '=',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )

      TARG1  = 'MOON'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'EARTH'
      SHAPE2 = 'BLOB'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'SUN',   '=',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   MAXWIN, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )



C
C   Case 9
C
      CALL TCASE ( 'Invalid value for MW' )

      STEP   = SPD()
      TARG1  = 'MOON'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'EARTH'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'SUN',   '=',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   1, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1,
     .              TARG2,    SHAPE2, FRAME2,
     .              CORR(1), 'SUN',   '=',
     .              0.D0,     ADJUST, STEP,
     .              CNFINE,   3, NWSEP, WORK,
     .              RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )



C
C     Case 10
C
      CALL TCASE ( 'ABSMIN' )

C
C     Perform a simple search using ALPHA and BETA for times
C     of minimum angular separation as seen from the sun.
C     Recall ALPHA - BETA occultation occurs every day
C     at 12:00 PM TDB.
C

      STEP   = 60.D0
      ADJUST = 0.D0
      REFVAL = 0.0D0

C
C     Store the time bounds of our search interval in
C     the CNFINE confinement window.
C
      CALL STR2ET ( '2000 JAN 01 TDB', ET0 )
      CALL STR2ET ( '2000 JAN 02 TDB', ET1 )

      CALL SCARDD  ( 0, CNFINE)
      CALL WNINSD ( ET0, ET1, CNFINE )

      FRAME1 = 'ALPHAFIXED'
      FRAME2 = 'BETAFIXED'

      DO I = 1, 2

         DO J = 1, 2

            CALL GFSEP ( 'ALPHA',  SHAPES(I), FRAME1,
     .                   'BETA',   SHAPES(J), FRAME2,
     .                   'NONE',  'SUN',     'ABSMIN',
     .                    REFVAL,  ADJUST, STEP,
     .                    CNFINE,  MAXWIN, NWSEP, WORK,
     .                    RESULT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           Check the number of intervals in the result window.
C
            COUNT = 0
            COUNT = WNCARD(RESULT)
            CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

            IF ( COUNT .EQ. 1 ) THEN

               CALL WNFETD ( RESULT, 1, BEG, END  )

               CALL SPKPOS('ALPHA', BEG, 'J2000', 'NONE', 'SUN',
     .                                                 POSA, LT)
               CALL SPKPOS('BETA',  BEG, 'J2000', 'NONE', 'SUN',
     .                                                 POSB, LT)

C
C              The angular separation should be within the MEDIUM
C              tolerance of 0.D0.
C
               CALL CHCKSD ( 'LOCMIN', VSEP(POSA, POSB), '~', 0.D0,
     .                                                      MEDIUM, OK )

            END IF

         END DO

      END DO

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )



C
C     Perform the test blocks for each aberration correction value.
C

      TARG1  = 'ALPHA'
      FRAME1 = 'ALPHAFIXED'
      TARG2  = 'BETA'
      FRAME2 = 'BETAFIXED'

      DO J=1, NCORR

         ABCORR = CORR(J)

C
C        Case 11
C
         CALL REPMC ('Relative =, sphere/sphere #', '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'SPHERE'

C
C        Perform a simple search using ALPHA and BETA for times
C        angular separation equal zero for the allowed body shapes.
C
         REFVAL = 0.0D0

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1,
     .                 TARG2,   SHAPE2, FRAME2,
     .                 ABCORR,  'SUN',  '=',
     .                 REFVAL,  ADJUST, STEP,
     .                 CNFINE,  MAXWIN, NWSEP, WORK,
     .                 RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2, 0, OK )

C
C        Two events should exist during the defined time interval.
C        We test one of the two.
C
         IF ( COUNT .EQ. 2 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSA, LT)
            CALL SPKPOS( TARG2,  BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSB, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )
            HANGB = DASIN( RADB(1)/VNORM(POSB) )

C
C           The angular separation should be within the MEDIUM
C           tolerance of the sum of body's half angles.
C
            CALL CHCKSD ( TITLE,
     .                     VSEP(POSA, POSB),
     .                     '~',
     .                     HANGA + HANGB,
     .                     MEDIUM,
     .                     OK )
         END IF



C
C     Case 12
C
         CALL REPMC ('Relative =, sphere/point #', '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'POINT'

         REFVAL = 0.0D0

         CALL GFSEP ( TARG1,  SHAPE1, FRAME1,
     .                TARG2,   SHAPE2, FRAME2,
     .                ABCORR,  'SUN',  '=',
     .                REFVAL,  ADJUST, STEP,
     .                CNFINE,  MAXWIN, NWSEP, WORK,
     .                RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2, 0, OK )

C
C        Two events should exist during the defined time interval.
C        We test one of the two.
C
         IF ( COUNT .EQ. 2 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000',  ABCORR, 'SUN',
     .                                                 POSA, LT)
            CALL SPKPOS( TARG2,  BEG, 'J2000',  ABCORR, 'SUN',
     .                                                 POSB, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )

C
C           The angular separation should be within the MEDIUM
C           tolerance of body ALPHA's half angle.
C
            CALL CHCKSD ( TITLE,
     .                    VSEP(POSA, POSB),
     .                    '~',
     .                     HANGA,
     .                     MEDIUM,
     .                     OK )
         END IF



C
C        Case 13
C
         CALL REPMC ('Relative =, point/sphere #', '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'POINT'
         SHAPE2 = 'SPHERE'

         REFVAL = 0.0D0

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1,
     .                 TARG2,   SHAPE2, FRAME2,
     .                 ABCORR,  'SUN',  '=',
     .                 REFVAL,  ADJUST, STEP,
     .                 CNFINE,  MAXWIN, NWSEP, WORK,
     .                 RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2, 0, OK )

C
C        Two events should exist during the defined time interval.
C        We test one of the two.
C
         IF ( COUNT .EQ. 2 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSA, LT)
            CALL SPKPOS( TARG2, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSB, LT)

            HANGB = DASIN( RADB(1)/VNORM(POSB) )

C
C          The angular separation should be within the MEDIUM
C          tolerance of body BETA's half angle.
C
            CALL CHCKSD ( TITLE,
     .                     VSEP(POSA, POSB),
     .                     '~',
     .                     HANGB,
     .                     MEDIUM,
     .                     OK )
         END IF



C
C        Case 14
C
         CALL TCASE ( 'Relative =, point/point' )

         SHAPE1 = 'POINT'
         SHAPE2 = 'POINT'

         REFVAL = 0.0D0

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1,
     .                 TARG2,   SHAPE2, FRAME2,
     .                 ABCORR, 'SUN',  'ABSMIN',
     .                 REFVAL,  ADJUST, STEP,
     .                 CNFINE,  MAXWIN, NWSEP, WORK,
     .                 RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)

         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

C
C        One event should exist during the defined time interval
C        (as we search for a non-zero separation).
C
         IF ( COUNT .EQ. 1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSA, LT)
            CALL SPKPOS( TARG2, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSB, LT)
C
C           The angular separation should be within the MEDIUM
C           tolerance of zero.
C
            CALL CHCKSD ( 'Relative =, point/point',
     .                  VSEP(POSA, POSB),
     .                  '~',
     .                  0.D0,
     .                  MEDIUM,
     .                  OK )
         END IF



C
C        Case 15
C
         CALL REPMC ('ABSMAX #, sphere/sphere' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'SPHERE'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1,
     .                 TARG2,   SHAPE2, FRAME2,
     .                 ABCORR, 'SUN',  'ABSMAX',
     .                 REFVAL,  ADJUST, STEP,
     .                 CNFINE,  MAXWIN, NWSEP, WORK,
     .                 RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

C
C        One events should exist during the defined time interval.
C
         IF ( COUNT .EQ.1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSA, LT)
            CALL SPKPOS( TARG2, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSB, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )
            HANGB = DASIN( RADB(1)/VNORM(POSB) )

            SEP = ZZSEPQ ( BEG, 1000, 2000, RADA(1), RADB(1),
     .                 10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           The angular separation should be within the MEDIUM
C           tolerance of PI - HANGA - HANGB.
C
            CALL CHCKSD ( TITLE,
     .                     PI_ - (HANGA + HANGB),
     .                     '~',
     .                     SEP,
     .                     MEDIUM,
     .                     OK )

         END IF



C
C        Case 16
C
         CALL REPMC ( 'ABSMAX #, sphere/point' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'POINT'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1,
     .                 TARG2,  SHAPE2, FRAME2,
     .                 ABCORR, 'SUN',  'ABSMAX',
     .                 REFVAL,  ADJUST, STEP,
     .                 CNFINE,  MAXWIN, NWSEP, WORK,
     .                 RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

C
C        Once events should exist during the defined time interval.
C
         IF ( COUNT .EQ.1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSA, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )

C
C           The angular separation should be within the MEDIUM
C           tolerance of body BETA's half angle.
C

            SEP = ZZSEPQ ( BEG, 1000, 2000, RADA(1), 0.D0,
     .                     10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           The angular separation should be within the MEDIUM
C           tolerance  of PI - HANGA.
C
            CALL CHCKSD (  TITLE,
     .                     PI_ - HANGA,
     .                     '~',
     .                     SEP,
     .                     MEDIUM,
     .                     OK )

         END IF



C
C        Case 17
C
         CALL REPMC ('ABSMAX #, point/sphere' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'POINT'
         SHAPE2 = 'SPHERE'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1,
     .                 TARG2,  SHAPE2, FRAME2,
     .                 ABCORR, 'SUN',  'ABSMAX',
     .                 REFVAL,  ADJUST, STEP,
     .                 CNFINE,  MAXWIN, NWSEP, WORK,
     .                 RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

C
C        Once events should exist during the defined time interval.
C
         IF ( COUNT .EQ.1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG2,  BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSB, LT)

            HANGB = DASIN( RADB(1)/VNORM(POSB) )

C
C           The angular separation should be within the MEDIUM
C           tolerance of body BETA's half angle.
C

            SEP = ZZSEPQ ( BEG, 1000, 2000, 0.D0, RADB(1),
     .                     10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           The angular separation should be within the MEDIUM
C           tolerance of PI - HANGB.
C
            CALL CHCKSD ( TITLE,
     .                     PI_ - HANGB,
     .                     '~',
     .                     SEP,
     .                     MEDIUM,
     .                     OK )

         END IF



C
C        Case 18
C
         CALL REPMC ('ABSMAX #, point/point' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'POINT'
         SHAPE2 = 'POINT'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1,
     .                 TARG2,  SHAPE2, FRAME2,
     .                 ABCORR, 'SUN',  'ABSMAX',
     .                 REFVAL,  ADJUST, STEP,
     .                 CNFINE,  MAXWIN, NWSEP, WORK,
     .                 RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

C
C        Once events should exist during the defined time interval.
C
         IF ( COUNT .EQ. 1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

C
C           The angular separation should be within the MEDIUM
C           tolerance of body BETA's half angle.
C

            SEP = ZZSEPQ ( BEG, 1000, 2000, 0.D0, 0.D0,
     .                 10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           The angular separation should be within the MEDIUM
C           tolerance of PI.
C
            CALL CHCKSD ( TITLE,
     .                     PI_,
     .                     '~',
     .                     SEP,
     .                     MEDIUM,
     .                     OK )

         END IF



C
C        Case 19
C
         CALL REPMC ('ABSMIN #, sphere/sphere' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'SPHERE'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1,
     .                 TARG2,  SHAPE2, FRAME2,
     .                 ABCORR, 'SUN',  'ABSMIN',
     .                 REFVAL,  ADJUST, STEP,
     .                 CNFINE,  MAXWIN, NWSEP, WORK,
     .                 RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

C
C        One events should exist during the defined time interval.
C
         IF ( COUNT .EQ.1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSA, LT)
            CALL SPKPOS( TARG2, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSB, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )
            HANGB = DASIN( RADB(1)/VNORM(POSB) )

C
C           The angular separation should be within the MEDIUM
C           tolerance of body BETA's half angle.
C

            SEP = ZZSEPQ ( BEG, 1000, 2000, RADA(1), RADB(1),
     .                 10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           The angular separation should be within the MEDIUM
C           tolerance of - HANGA - HANGB.
C
            CALL CHCKSD ( TITLE,
     .                    -(HANGA + HANGB),
     .                     '~',
     .                     SEP,
     .                     MEDIUM,
     .                     OK )

         END IF



C
C        Case 20
C
         CALL REPMC ( 'ABSMIN #, sphere/point' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'POINT'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1,
     .                 TARG2,  SHAPE2, FRAME2,
     .                 ABCORR, 'SUN',  'ABSMIN',
     .                 REFVAL,  ADJUST, STEP,
     .                 CNFINE,  MAXWIN, NWSEP, WORK,
     .                 RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

C
C        One events should exist during the defined time interval.
C
         IF ( COUNT .EQ. 1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSA, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )

C
C           The angular separation should be within the MEDIUM
C           tolerance of body BETA's half angle.
C

            SEP = ZZSEPQ ( BEG, 1000, 2000, RADA(1), 0.D0,
     .                     10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           The angular separation should be within the MEDIUM
C           tolerance of -HANGA.
C
            CALL CHCKSD (  TITLE,
     .                     - HANGA,
     .                     '~',
     .                     SEP,
     .                     MEDIUM,
     .                     OK )

         END IF



C
C        Case 21
C
         CALL REPMC ( 'ABSMIN #, point/sphere' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'POINT'
         SHAPE2 = 'SPHERE'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1,
     .                 TARG2,  SHAPE2, FRAME2,
     .                 ABCORR, 'SUN',  'ABSMIN',
     .                 REFVAL,  ADJUST, STEP,
     .                 CNFINE,  MAXWIN, NWSEP, WORK,
     .                 RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

C
C        Check the number of intervals in the result window.
C
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

C
C        Once events should exist during the defined time interval.
C
         IF ( COUNT .EQ. 1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG2, BEG, 'J2000', ABCORR, 'SUN',
     .                                                 POSB, LT)

            HANGB = DASIN( RADB(1)/VNORM(POSB) )

C
C           The angular separation should be within the MEDIUM
C           tolerance of body BETA's half angle.
C

            SEP = ZZSEPQ ( BEG, 1000, 2000, 0.D0, RADB(1),
     .                     10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           The angular separation should be within the MEDIUM
C           tolerance of - HANGB.
C
            CALL CHCKSD (  TITLE,
     .                     -HANGB,
     .                     '~',
     .                     SEP,
     .                     MEDIUM,
     .                     OK )

         END IF

      END DO



C
C     Case 22
C
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


C
C     Re-run a valid search after using GFSTOL to set the convergence
C     tolerance to a value that should cause a numerical error signal.
C

      STEP   = 60.D0
      ADJUST = 0.D0
      REFVAL = 0.0D0
      FRAME1 = 'ALPHAFIXED'
      FRAME2 = 'BETAFIXED'

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFSEP ( 'ALPHA', SHAPES(1), FRAME1,
     .             'BETA',  SHAPES(1), FRAME2,
     .             'NONE',  'SUN',     'ABSMIN',
     .             REFVAL,  ADJUST, STEP,
     .             CNFINE,  MAXWIN, NWSEP, WORK,
     .             RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Reset tol.
C

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFSEP ( 'ALPHA', SHAPES(1), FRAME1,
     .             'BETA',  SHAPES(1), FRAME2,
     .             'NONE',  'SUN',     'ABSMIN',
     .             REFVAL,  ADJUST, STEP,
     .             CNFINE,  MAXWIN, NWSEP, WORK,
     .             RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     The values in the time window should not match
C     as the search used different tolerances. Check
C     the first value in the first interval.
C
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )



C
C     Reset the convergence tolerance.
C
      CALL GFSTOL ( CNVTOL )



C
C Case n
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
