!$Procedure F_GFSEP ( GFSEP family tests )

      SUBROUTINE F_GFSEP ( OK )
      use pepper_legacy
      use pepper_inc_gf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        GFSEP
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the higher-level SPICELIB
!     geometry routine GFSEP.
!
! $ Examples
!
!     None.
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
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    TSPICE Version 1.4.0, 29-MAY-2021 (EDW) (JDR)
!
!        Replaced calls to routine ZZGFSPQ with function ZZSEPQ.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.3.0, 28-JAN-2017 (EDW)
!
!        Modified GFSTOL test case to correspond to change
!        in ZZGFSOLVX.
!
!-    TSPICE Version 1.2.0, 20-NOV-2012 (EDW)
!
!        Added test on GFSTOL use.
!
!-    TSPICE Version 1.1.0, 07-JUN-2010 (EDW)
!
!        Minor edit to code comments eliminating typo.
!
!        Edits to code to improve structure and readability.
!
!-    TSPICE Version 1.0.0, 17-DEC-2008 (EDW)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     SPICELIB functions
!



!
!     Local parameters
!
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

!
!     Local variables
!
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


!
!     Save everything
!
      SAVE


!
!     Initial values
!
      DATA                 CORR  / 'NONE', &
                                   'lt',  ' lt+s',  ' cn',  ' cn + s', &
                                   'XLT', 'XLT + S', 'XCN', 'XCN+S'   /

      DATA                 SHAPES / 'POINT', 'SPHERE'   /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFSEP' )

      PI_ = PI()

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

!
!     Create an LSK, load using FURNSH.
!
      CALL ZZTSTLSK( LSK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( LSK )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create a PCK, load using FURNSH.
!
      CALL ZZTSTPCK( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create the PCK for Nat's Solar System.
!
      CALL NATPCK ( PCK1,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK1 )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create the SPK for Nat's Solar System.
!
      CALL NATSPK ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!     Create a confinement window from ET0 and ET1.
!
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


!
!    Error cases
!

!
!   Case 1
!
      CALL TCASE ( 'Non-positive step size' )

      STEP   = 0.D0
      ADJUST = 0.D0
      TARG1  = 'MOON'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'IAU_MOON'
      TARG2  = 'EARTH'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'SUN',   '=', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )



!
!   Case 2
!
      CALL TCASE ( 'Non unique body IDs.' )

      STEP   = SPD()
      TARG1  = 'MOON'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'IAU_MOON'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'SUN',   '=', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'MOON',   '=', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'EARTH',   '=', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



!
!   Case 3
!
      CALL TCASE ( 'Invalid aberration correction specifier' )

      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    'X',     'SUN',   '=', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



!
!   Case 4
!
      CALL TCASE ( 'Invalid relations operator' )

      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'SUN',   '==', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )



!
!   Case 5
!
      CALL TCASE ( 'Invalid body names' )

      TARG1  = 'X'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MOON'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'SUN', 'ABSMAX', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'X'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'MOON', 'ABSMAX', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'X', 'ABSMAX', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )



!
!     Case 6
!
      CALL TCASE ( 'Negative adjustment value' )

      ADJUST = -1.0D0
      TARG1  = 'EARTH'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'MOON'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'SUN', 'ABSMAX', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )



!
!     Case 7
!
      CALL TCASE ( 'Ephemeris data unavailable' )

      ADJUST = 0.0D0

!
!     A vehicle/probe has a point shape so frame name not processed.
!
      TARG1  = 'DAWN'
      SHAPE1 = 'POINT'
      FRAME1 = 'NULL'
      TARG2  = 'MARS'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'IAU_MARS'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'SUN',  'ABSMAX', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )



!
!   Case 8
!
      CALL TCASE ( 'Unknown shape name.' )

      STEP   = SPD()
      TARG1  = 'MOON'
      SHAPE1 = 'PANCAKE'
      FRAME1 = 'NULL'
      TARG2  = 'EARTH'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'SUN',   '=', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )

      TARG1  = 'MOON'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'EARTH'
      SHAPE2 = 'BLOB'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'SUN',   '=', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   MAXWIN, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )



!
!   Case 9
!
      CALL TCASE ( 'Invalid value for MW' )

      STEP   = SPD()
      TARG1  = 'MOON'
      SHAPE1 = 'SPHERE'
      FRAME1 = 'NULL'
      TARG2  = 'EARTH'
      SHAPE2 = 'SPHERE'
      FRAME2 = 'NULL'

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'SUN',   '=', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   1, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )

      CALL GFSEP (  TARG1,    SHAPE1, FRAME1, &
                    TARG2,    SHAPE2, FRAME2, &
                    CORR(1), 'SUN',   '=', &
                    0.D0,     ADJUST, STEP, &
                    CNFINE,   3, NWSEP, WORK, &
                    RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDDIMENSION)',   OK )



!
!     Case 10
!
      CALL TCASE ( 'ABSMIN' )

!
!     Perform a simple search using ALPHA and BETA for times
!     of minimum angular separation as seen from the sun.
!     Recall ALPHA - BETA occultation occurs every day
!     at 12:00 PM TDB.
!

      STEP   = 60.D0
      ADJUST = 0.D0
      REFVAL = 0.0D0

!
!     Store the time bounds of our search interval in
!     the CNFINE confinement window.
!
      CALL STR2ET ( '2000 JAN 01 TDB', ET0 )
      CALL STR2ET ( '2000 JAN 02 TDB', ET1 )

      CALL SCARDD  ( 0, CNFINE)
      CALL WNINSD ( ET0, ET1, CNFINE )

      FRAME1 = 'ALPHAFIXED'
      FRAME2 = 'BETAFIXED'

      DO I = 1, 2

         DO J = 1, 2

            CALL GFSEP ( 'ALPHA',  SHAPES(I), FRAME1, &
                         'BETA',   SHAPES(J), FRAME2, &
                         'NONE',  'SUN',     'ABSMIN', &
                          REFVAL,  ADJUST, STEP, &
                          CNFINE,  MAXWIN, NWSEP, WORK, &
                          RESULT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           Check the number of intervals in the result window.
!
            COUNT = 0
            COUNT = WNCARD(RESULT)
            CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

            IF ( COUNT .EQ. 1 ) THEN

               CALL WNFETD ( RESULT, 1, BEG, END  )

               CALL SPKPOS('ALPHA', BEG, 'J2000', 'NONE', 'SUN', &
                                                       POSA, LT)
               CALL SPKPOS('BETA',  BEG, 'J2000', 'NONE', 'SUN', &
                                                       POSB, LT)

!
!              The angular separation should be within the MEDIUM
!              tolerance of 0.D0.
!
               CALL CHCKSD ( 'LOCMIN', VSEP(POSA, POSB), '~', 0.D0, &
                                                            MEDIUM, OK )

            END IF

         END DO

      END DO

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )



!
!     Perform the test blocks for each aberration correction value.
!

      TARG1  = 'ALPHA'
      FRAME1 = 'ALPHAFIXED'
      TARG2  = 'BETA'
      FRAME2 = 'BETAFIXED'

      DO J=1, NCORR

         ABCORR = CORR(J)

!
!        Case 11
!
         CALL REPMC ('Relative =, sphere/sphere #', '#', ABCORR, TITLE)
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'SPHERE'

!
!        Perform a simple search using ALPHA and BETA for times
!        angular separation equal zero for the allowed body shapes.
!
         REFVAL = 0.0D0

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1, &
                       TARG2,   SHAPE2, FRAME2, &
                       ABCORR,  'SUN',  '=', &
                       REFVAL,  ADJUST, STEP, &
                       CNFINE,  MAXWIN, NWSEP, WORK, &
                       RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2, 0, OK )

!
!        Two events should exist during the defined time interval.
!        We test one of the two.
!
         IF ( COUNT .EQ. 2 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSA, LT)
            CALL SPKPOS( TARG2,  BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSB, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )
            HANGB = DASIN( RADB(1)/VNORM(POSB) )

!
!           The angular separation should be within the MEDIUM
!           tolerance of the sum of body's half angles.
!
            CALL CHCKSD ( TITLE, &
                           VSEP(POSA, POSB), &
                           '~', &
                           HANGA + HANGB, &
                           MEDIUM, &
                           OK )
         END IF



!
!     Case 12
!
         CALL REPMC ('Relative =, sphere/point #', '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'POINT'

         REFVAL = 0.0D0

         CALL GFSEP ( TARG1,  SHAPE1, FRAME1, &
                      TARG2,   SHAPE2, FRAME2, &
                      ABCORR,  'SUN',  '=', &
                      REFVAL,  ADJUST, STEP, &
                      CNFINE,  MAXWIN, NWSEP, WORK, &
                      RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2, 0, OK )

!
!        Two events should exist during the defined time interval.
!        We test one of the two.
!
         IF ( COUNT .EQ. 2 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000',  ABCORR, 'SUN', &
                                                       POSA, LT)
            CALL SPKPOS( TARG2,  BEG, 'J2000',  ABCORR, 'SUN', &
                                                       POSB, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )

!
!           The angular separation should be within the MEDIUM
!           tolerance of body ALPHA's half angle.
!
            CALL CHCKSD ( TITLE, &
                          VSEP(POSA, POSB), &
                          '~', &
                           HANGA, &
                           MEDIUM, &
                           OK )
         END IF



!
!        Case 13
!
         CALL REPMC ('Relative =, point/sphere #', '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'POINT'
         SHAPE2 = 'SPHERE'

         REFVAL = 0.0D0

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1, &
                       TARG2,   SHAPE2, FRAME2, &
                       ABCORR,  'SUN',  '=', &
                       REFVAL,  ADJUST, STEP, &
                       CNFINE,  MAXWIN, NWSEP, WORK, &
                       RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 2, 0, OK )

!
!        Two events should exist during the defined time interval.
!        We test one of the two.
!
         IF ( COUNT .EQ. 2 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSA, LT)
            CALL SPKPOS( TARG2, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSB, LT)

            HANGB = DASIN( RADB(1)/VNORM(POSB) )

!
!          The angular separation should be within the MEDIUM
!          tolerance of body BETA's half angle.
!
            CALL CHCKSD ( TITLE, &
                           VSEP(POSA, POSB), &
                           '~', &
                           HANGB, &
                           MEDIUM, &
                           OK )
         END IF



!
!        Case 14
!
         CALL TCASE ( 'Relative =, point/point' )

         SHAPE1 = 'POINT'
         SHAPE2 = 'POINT'

         REFVAL = 0.0D0

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1, &
                       TARG2,   SHAPE2, FRAME2, &
                       ABCORR, 'SUN',  'ABSMIN', &
                       REFVAL,  ADJUST, STEP, &
                       CNFINE,  MAXWIN, NWSEP, WORK, &
                       RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)

         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

!
!        One event should exist during the defined time interval
!        (as we search for a non-zero separation).
!
         IF ( COUNT .EQ. 1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSA, LT)
            CALL SPKPOS( TARG2, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSB, LT)
!
!           The angular separation should be within the MEDIUM
!           tolerance of zero.
!
            CALL CHCKSD ( 'Relative =, point/point', &
                        VSEP(POSA, POSB), &
                        '~', &
                        0.D0, &
                        MEDIUM, &
                        OK )
         END IF



!
!        Case 15
!
         CALL REPMC ('ABSMAX #, sphere/sphere' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'SPHERE'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1, &
                       TARG2,   SHAPE2, FRAME2, &
                       ABCORR, 'SUN',  'ABSMAX', &
                       REFVAL,  ADJUST, STEP, &
                       CNFINE,  MAXWIN, NWSEP, WORK, &
                       RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

!
!        One events should exist during the defined time interval.
!
         IF ( COUNT .EQ.1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSA, LT)
            CALL SPKPOS( TARG2, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSB, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )
            HANGB = DASIN( RADB(1)/VNORM(POSB) )

            SEP = ZZSEPQ ( BEG, 1000, 2000, RADA(1), RADB(1), &
                       10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           The angular separation should be within the MEDIUM
!           tolerance of PI - HANGA - HANGB.
!
            CALL CHCKSD ( TITLE, &
                           PI_ - (HANGA + HANGB), &
                           '~', &
                           SEP, &
                           MEDIUM, &
                           OK )

         END IF



!
!        Case 16
!
         CALL REPMC ( 'ABSMAX #, sphere/point' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'POINT'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1, &
                       TARG2,  SHAPE2, FRAME2, &
                       ABCORR, 'SUN',  'ABSMAX', &
                       REFVAL,  ADJUST, STEP, &
                       CNFINE,  MAXWIN, NWSEP, WORK, &
                       RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

!
!        Once events should exist during the defined time interval.
!
         IF ( COUNT .EQ.1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSA, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )

!
!           The angular separation should be within the MEDIUM
!           tolerance of body BETA's half angle.
!

            SEP = ZZSEPQ ( BEG, 1000, 2000, RADA(1), 0.D0, &
                           10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           The angular separation should be within the MEDIUM
!           tolerance  of PI - HANGA.
!
            CALL CHCKSD (  TITLE, &
                           PI_ - HANGA, &
                           '~', &
                           SEP, &
                           MEDIUM, &
                           OK )

         END IF



!
!        Case 17
!
         CALL REPMC ('ABSMAX #, point/sphere' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'POINT'
         SHAPE2 = 'SPHERE'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1, &
                       TARG2,  SHAPE2, FRAME2, &
                       ABCORR, 'SUN',  'ABSMAX', &
                       REFVAL,  ADJUST, STEP, &
                       CNFINE,  MAXWIN, NWSEP, WORK, &
                       RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

!
!        Once events should exist during the defined time interval.
!
         IF ( COUNT .EQ.1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG2,  BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSB, LT)

            HANGB = DASIN( RADB(1)/VNORM(POSB) )

!
!           The angular separation should be within the MEDIUM
!           tolerance of body BETA's half angle.
!

            SEP = ZZSEPQ ( BEG, 1000, 2000, 0.D0, RADB(1), &
                           10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           The angular separation should be within the MEDIUM
!           tolerance of PI - HANGB.
!
            CALL CHCKSD ( TITLE, &
                           PI_ - HANGB, &
                           '~', &
                           SEP, &
                           MEDIUM, &
                           OK )

         END IF



!
!        Case 18
!
         CALL REPMC ('ABSMAX #, point/point' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'POINT'
         SHAPE2 = 'POINT'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1, &
                       TARG2,  SHAPE2, FRAME2, &
                       ABCORR, 'SUN',  'ABSMAX', &
                       REFVAL,  ADJUST, STEP, &
                       CNFINE,  MAXWIN, NWSEP, WORK, &
                       RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

!
!        Once events should exist during the defined time interval.
!
         IF ( COUNT .EQ. 1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

!
!           The angular separation should be within the MEDIUM
!           tolerance of body BETA's half angle.
!

            SEP = ZZSEPQ ( BEG, 1000, 2000, 0.D0, 0.D0, &
                       10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           The angular separation should be within the MEDIUM
!           tolerance of PI.
!
            CALL CHCKSD ( TITLE, &
                           PI_, &
                           '~', &
                           SEP, &
                           MEDIUM, &
                           OK )

         END IF



!
!        Case 19
!
         CALL REPMC ('ABSMIN #, sphere/sphere' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'SPHERE'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1, &
                       TARG2,  SHAPE2, FRAME2, &
                       ABCORR, 'SUN',  'ABSMIN', &
                       REFVAL,  ADJUST, STEP, &
                       CNFINE,  MAXWIN, NWSEP, WORK, &
                       RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

!
!        One events should exist during the defined time interval.
!
         IF ( COUNT .EQ.1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSA, LT)
            CALL SPKPOS( TARG2, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSB, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )
            HANGB = DASIN( RADB(1)/VNORM(POSB) )

!
!           The angular separation should be within the MEDIUM
!           tolerance of body BETA's half angle.
!

            SEP = ZZSEPQ ( BEG, 1000, 2000, RADA(1), RADB(1), &
                       10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           The angular separation should be within the MEDIUM
!           tolerance of - HANGA - HANGB.
!
            CALL CHCKSD ( TITLE, &
                          -(HANGA + HANGB), &
                           '~', &
                           SEP, &
                           MEDIUM, &
                           OK )

         END IF



!
!        Case 20
!
         CALL REPMC ( 'ABSMIN #, sphere/point' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'SPHERE'
         SHAPE2 = 'POINT'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1, &
                       TARG2,  SHAPE2, FRAME2, &
                       ABCORR, 'SUN',  'ABSMIN', &
                       REFVAL,  ADJUST, STEP, &
                       CNFINE,  MAXWIN, NWSEP, WORK, &
                       RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

!
!        One events should exist during the defined time interval.
!
         IF ( COUNT .EQ. 1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG1, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSA, LT)

            HANGA = DASIN( RADA(1)/VNORM(POSA) )

!
!           The angular separation should be within the MEDIUM
!           tolerance of body BETA's half angle.
!

            SEP = ZZSEPQ ( BEG, 1000, 2000, RADA(1), 0.D0, &
                           10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           The angular separation should be within the MEDIUM
!           tolerance of -HANGA.
!
            CALL CHCKSD (  TITLE, &
                           - HANGA, &
                           '~', &
                           SEP, &
                           MEDIUM, &
                           OK )

         END IF



!
!        Case 21
!
         CALL REPMC ( 'ABSMIN #, point/sphere' , '#', ABCORR, TITLE )
         CALL TCASE ( TITLE )

         SHAPE1 = 'POINT'
         SHAPE2 = 'SPHERE'

         CALL GFSEP (  TARG1,  SHAPE1, FRAME1, &
                       TARG2,  SHAPE2, FRAME2, &
                       ABCORR, 'SUN',  'ABSMIN', &
                       REFVAL,  ADJUST, STEP, &
                       CNFINE,  MAXWIN, NWSEP, WORK, &
                       RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        Check the number of intervals in the result window.
!
         COUNT = 0
         COUNT = WNCARD(RESULT)
         CALL CHCKSI ( 'COUNT', COUNT, '=', 1, 0, OK )

!
!        Once events should exist during the defined time interval.
!
         IF ( COUNT .EQ. 1 ) THEN

            CALL WNFETD ( RESULT, 1, BEG, END  )

            CALL SPKPOS( TARG2, BEG, 'J2000', ABCORR, 'SUN', &
                                                       POSB, LT)

            HANGB = DASIN( RADB(1)/VNORM(POSB) )

!
!           The angular separation should be within the MEDIUM
!           tolerance of body BETA's half angle.
!

            SEP = ZZSEPQ ( BEG, 1000, 2000, 0.D0, RADB(1), &
                           10, ABCORR, 'J2000' )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           The angular separation should be within the MEDIUM
!           tolerance of - HANGB.
!
            CALL CHCKSD (  TITLE, &
                           -HANGB, &
                           '~', &
                           SEP, &
                           MEDIUM, &
                           OK )

         END IF

      END DO



!
!     Case 22
!
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


!
!     Re-run a valid search after using GFSTOL to set the convergence
!     tolerance to a value that should cause a numerical error signal.
!

      STEP   = 60.D0
      ADJUST = 0.D0
      REFVAL = 0.0D0
      FRAME1 = 'ALPHAFIXED'
      FRAME2 = 'BETAFIXED'

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFSEP ( 'ALPHA', SHAPES(1), FRAME1, &
                   'BETA',  SHAPES(1), FRAME2, &
                   'NONE',  'SUN',     'ABSMIN', &
                   REFVAL,  ADJUST, STEP, &
                   CNFINE,  MAXWIN, NWSEP, WORK, &
                   RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Reset tol.
!

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFSEP ( 'ALPHA', SHAPES(1), FRAME1, &
                   'BETA',  SHAPES(1), FRAME2, &
                   'NONE',  'SUN',     'ABSMIN', &
                   REFVAL,  ADJUST, STEP, &
                   CNFINE,  MAXWIN, NWSEP, WORK, &
                   RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     The values in the time window should not match
!     as the search used different tolerances. Check
!     the first value in the first interval.
!
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )



!
!     Reset the convergence tolerance.
!
      CALL GFSTOL ( CNVTOL )



!
! Case n
!
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
