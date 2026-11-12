!$Procedure F_ZZGFSPU ( ZZGFSPU family tests )

      SUBROUTINE F_ZZGFSPU ( OK )
      use pepper_legacy, pepper_legacy_frame => frame
      use pepper_inc_gf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        ZZGFSPU
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
!     This routine tests the low-level SPICELIB
!     geometry routine ZZGFSPU.
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
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    TSPICE Version 1.2.0, 30-NOV-2021 (EDW) (JDR)
!
!        Replaced calls to routine ZZGFSPQ with function ZZSEPQ.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.1.0, 18-JUN-2013 (EDW) (BVS)
!
!        Added test cases to confirm expected state of
!        initialized values and expected errors from ZZGFSPIN.
!
!-    TSPICE Version 1.0.0, 17-APR-2008 (EDW)
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
      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck' )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-12 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 25 )

!
!     Local variables
!

      CHARACTER*(ABCLEN)    TARGT1 ( 6 )
      CHARACTER*(ABCLEN)    OBSRVR ( 6 )
      CHARACTER*(ABCLEN)    TARGT2 ( 6 )
      CHARACTER*(ABCLEN)    XABCR
      CHARACTER*(ABCLEN)    XREF  (2)
      CHARACTER*(ABCLEN)    YREF

      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(ABCLEN)    CORR   ( NCORR )
      CHARACTER*(ABCLEN)    FRAME (2)
      CHARACTER*(LNSIZE)    FROM
      CHARACTER*(LNSIZE)    OF    (2)
      CHARACTER*(LNSIZE)    SHAPE (2)
      CHARACTER*(LNSIZE)    SHAPES(2)
      CHARACTER*(LNSIZE)    TXT

      DOUBLE PRECISION      AXESA  (3)
      DOUBLE PRECISION      AXESB  (3)
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      HANGA
      DOUBLE PRECISION      HANGB
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      POSA   (3)
      DOUBLE PRECISION      POSB   (3)
      DOUBLE PRECISION      RADA
      DOUBLE PRECISION      RADB
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      XRAD  (2)

      INTEGER               HAN1
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               XBOD  (2)
      INTEGER               XOBS
      INTEGER               XSHP  (2)

!
!     Indices 1:3 for Invalid body name test, 4:6 for not distinct
!     body names test.
!
      DATA     TARGT1 / 'ALPHA', 'X', 'ALPHA', 'ALPHA', 'ALPHA', 'SUN'/

      DATA     TARGT2 / 'X', 'BETA', 'BETA',  'ALPHA', 'BETA', 'BETA' /

      DATA     OBSRVR / 'SUN', 'SUN', 'X',    'SUN',   'BETA', 'SUN'  /

      DATA     CORR   / 'NONE', 'lt',  ' lt+s',  ' cn',  ' cn + s', &
                        'XLT', 'XLT + S', 'XCN', 'XCN+S'   /

      DATA     SHAPES / 'POINT', 'SPHERE' /


      SAVE                  CORR
      SAVE                  SHAPES
      SAVE                  TARGT1
      SAVE                  TARGT2
      SAVE                  OBSRVR


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFSPU' )

!
!     Case 1: Create kernels, load same.
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a PCK, load using FURNSH.
!

      CALL NATPCK ( PCK1,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK1 )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create an SPK, load using FURNSH.
!

      CALL NATSPK ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!     Assign the body radii.
!
      CALL ZZGFTREB( 1000, AXESA )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      RADA = MAX( AXESA(1), AXESA(2), AXESA(3) )

      CALL ZZGFTREB( 2000, AXESB )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      RADB = MAX( AXESB(1), AXESB(2), AXESB(3) )




!
!     Case 2: Invalid body names.
!
      ABCORR   =  CORR(1)
      SHAPE(1) = SHAPES(1)
      SHAPE(2) = SHAPES(1)
      FRAME(1) = 'ECLIPJ2000'
      FRAME(2) = 'ECLIPJ2000'

      DO I=1, 3

         OF(1)  =TARGT1(I)
         OF(2) = TARGT2(I)
         FROM  = OBSRVR(I)

         TXT = 'Invalid body name test. OF(1) = #, OF(2) = #, FROM = #'
         CALL REPMC ( TXT, '#', OF(1), TXT )
         CALL REPMC ( TXT, '#', OF(2), TXT )
         CALL REPMC ( TXT, '#', FROM,  TXT )

         CALL TCASE (TXT)

         CALL ZZGFSPIN ( OF, FROM, SHAPE, FRAME, ABCORR )
         CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

      END DO




!
!     Case 3: Not distinct body names.
!
      ABCORR   =  CORR(1)
      SHAPE(1) = SHAPES(1)
      SHAPE(2) = SHAPES(1)
      FRAME(1) = 'ECLIPJ2000'
      FRAME(2) = 'ECLIPJ2000'

      DO I=4, 6

         OF(1)  =TARGT1(I)
         OF(2) = TARGT2(I)
         FROM  = OBSRVR(I)

         TXT = 'Not distinct body name test. ' &
           //  'OF(1) = #, OF(2) = #, FROM = #'
         CALL REPMC ( TXT, '#', OF(1), TXT )
         CALL REPMC ( TXT, '#', OF(2), TXT )
         CALL REPMC ( TXT, '#', FROM,  TXT )

         CALL TCASE (TXT)

         CALL ZZGFSPIN ( OF, FROM, SHAPE, FRAME, ABCORR )
         CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      END DO




!
!     Case 4:
!
      ABCORR   =  CORR(1)
      SHAPE(1) = 'X'
      SHAPE(2) = SHAPES(2)
      FRAME(1) = 'ECLIPJ2000'
      FRAME(2) = 'ECLIPJ2000'
      OF(1)  =TARGT1(1)
      OF(2) = TARGT2(2)
      FROM  = OBSRVR(1)

      TXT = 'Invalid shape. ' &
           //  'SHAPE(1) = #, SHAPE(2) = #'
      CALL REPMC ( TXT, '#', SHAPE(1), TXT )
      CALL REPMC ( TXT, '#', SHAPE(2), TXT )

      CALL TCASE (TXT)

      CALL ZZGFSPIN ( OF, FROM, SHAPE, FRAME, ABCORR )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


      SHAPE(1) = SHAPES(1)
      SHAPE(2) = 'X'

      TXT = 'Invalid shape. ' &
           //  'SHAPE(1) = #, SHAPE(2) = #'
      CALL REPMC ( TXT, '#', SHAPE(1), TXT )
      CALL REPMC ( TXT, '#', SHAPE(2), TXT )

      CALL TCASE (TXT)

      CALL ZZGFSPIN ( OF, FROM, SHAPE, FRAME, ABCORR )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )



!
!     Case 5: Confirm initialized values are correctly saved.
!
      OF(1)    = '1000'
      OF(2)    = '2000'
      FROM     = 'SUN'
      FRAME(1) = 'ECLIPJ2000'
      FRAME(2) = 'ECLIPJ2000'


      DO I=1, 2

         DO J=1, 2

            DO K=1, NCORR

               ABCORR   = CORR(K)
               SHAPE(1) = SHAPES(I)
               SHAPE(2) = SHAPES(J)

               CALL REPMC ( 'ZZGFSPX #1 #2 #3', '#1', ABCORR,   TXT )
               CALL REPMC ( TXT,                '#2', SHAPE(1), TXT )
               CALL REPMC ( TXT,                '#3', SHAPE(2), TXT )

               CALL TCASE ( TXT )

               CALL ZZGFSPIN ( OF, FROM, SHAPE, FRAME, ABCORR )

               CALL ZZGFSPX ( XABCR, XBOD, YREF, XREF, &
                              XOBS,  XRAD, XSHP )

!
!              Squeaky cleanse the ABCORR string.
!
               CALL UCASE  ( ABCORR, ABCORR )
               CALL CMPRSS ( ' ', 0, ABCORR, ABCORR )

               CALL CHCKSC ( 'XABCR', XABCR,  '=', ABCORR, OK )


!
!              Check IDs for target bodies array.
!
               CALL CHCKSI ( 'XBOD 1', XBOD(1), '=', 1000, 0, OK )
               CALL CHCKSI ( 'XBOD 2', XBOD(2), '=', 2000, 0, OK )

!
!              All calculations use J2000 frame.
!
               CALL CHCKSC ( 'YREF', YREF,  '=', 'J2000', OK )

               CALL CHCKSC ( 'XREF(1)', XREF(1),  '=', FRAME(1), OK )
               CALL CHCKSC ( 'XREF(2)', XREF(2),  '=', FRAME(2), OK )

!
!              Check ID for observer body.
!
               CALL CHCKSI ( 'XOBS', XOBS, '=', 10, 0, OK )

!
!              Check Index for SHAPES array.
!
               CALL CHCKSI ( 'XSHP 1', XSHP(1), '=', I, 0, OK )
               CALL CHCKSI ( 'XSHP 2', XSHP(2), '=', J, 0, OK )

!
!              Check radii for spherical body, XSHP(I) = 2
!
               IF ( XSHP(1) .EQ. 2 ) THEN
                  CALL CHCKSD ( 'RAD 1', XRAD(1), '=', RADA, 0.D0, OK )
               END IF

               IF ( XSHP(2) .EQ. 2 ) THEN
                  CALL CHCKSD ( 'RAD 2', XRAD(2), '=', RADB, 0.D0, OK )
               END IF


            END DO

         END DO

      END DO




!
!     Test the ZZSEPQ result using ALPHA and BETA from Nat's Solar
!     System.
!
!     Step in increments of one minute from ET 0 to ET 86400 in steps
!     of 60 TDB seconds. Test the four combinations of Sphere and Point
!     object pairs. Perform each test in a block for clarity.
!

!
!     Perform the four test blocks for each aberration correction value.
!
      DO J=1, NCORR

         ABCORR = CORR(J)

!
!        Case 6: Sphere-Sphere
!
         ET = 0.D0

         DO I=1, 1440

            CALL REPMI ( 'ZZSEPQ Sphere/Sphere #1, ABCORR #2' , '#1', &
                                                            I, TXT )
            CALL REPMC ( TXT , '#2', ABCORR, TXT )
            CALL TCASE ( TXT )

            SEP = ZZSEPQ ( ET, &
                           1000, 2000, &
                           RADA, RADB, &
                           10, ABCORR, 'J2000' )

            CALL SPKPOS('ALPHA', ET, 'J2000', ABCORR, 'SUN', POSA, LT)
            CALL SPKPOS('BETA',  ET, 'J2000', ABCORR, 'SUN', POSB, LT)

            HANGA = DASIN( RADA/VNORM(POSA) )
            HANGB = DASIN( RADB/VNORM(POSB) )

            THETA = VSEP( POSA, POSB) - ( HANGA + HANGB )
            CALL CHCKSD ( 'ZZSEPQ', SEP, '~', THETA, VTIGHT, OK )

            ET  = ET + 60.D0

         END DO




!
!        Case 7: Point-Sphere
!
         RADA = 0.D0
         ET = 0.D0

         DO I=1, 1440

            CALL REPMI ( 'ZZSEPQ Point/Sphere #1, ABCORR #2' , '#1', &
                                                            I, TXT )
            CALL REPMC ( TXT , '#2', ABCORR, TXT )
            CALL TCASE ( TXT )

            SEP = ZZSEPQ ( ET, &
                           1000, 2000, &
                           RADA, RADB, &
                           10, ABCORR, 'J2000' )

            CALL SPKPOS('ALPHA', ET, 'J2000', ABCORR, 'SUN', POSA, LT)
            CALL SPKPOS('BETA',  ET, 'J2000', ABCORR, 'SUN', POSB, LT)

            HANGA = DASIN( RADA/VNORM(POSA) )
            HANGB = DASIN( RADB/VNORM(POSB) )

            THETA = VSEP( POSA, POSB) - ( HANGA + HANGB )
            CALL CHCKSD ( 'ZZSEPQ', SEP, '~', THETA, VTIGHT, OK )

            ET  = ET + 60.D0

         END DO




!
!        Case 8: Sphere-Point
!
         RADA = MAX( AXESA(1), AXESA(2), AXESA(3) )
         RADB = 0.D0
         ET   = 0.D0

         DO I=1, 1440

            CALL REPMI ( 'ZZSEPQ Sphere/Point #1, ABCORR #2' , '#1', &
                                                            I, TXT )
            CALL REPMC ( TXT , '#2', ABCORR, TXT )
            CALL TCASE ( TXT )

            SEP = ZZSEPQ ( ET, &
                           1000, 2000, &
                           RADA, RADB, &
                           10, ABCORR, 'J2000' )

            CALL SPKPOS('ALPHA', ET, 'J2000', ABCORR, 'SUN', POSA, LT)
            CALL SPKPOS('BETA',  ET, 'J2000', ABCORR, 'SUN', POSB, LT)

            HANGA = DASIN( RADA/VNORM(POSA) )
            HANGB = DASIN( RADB/VNORM(POSB) )

            THETA = VSEP( POSA, POSB) - ( HANGA + HANGB )
            CALL CHCKSD ( 'ZZSEPQ', SEP, '~', THETA, VTIGHT, OK )

            ET  = ET + 60.D0

         END DO




!
!        Case 9: Point-Point
!
         RADA = 0.D0
         RADB = 0.D0
         ET = 0.D0

         DO I=1, 1440

            CALL REPMI ( 'ZZSEPQ Point/Point #1, ABCORR #2' , '#1', &
                                                            I, TXT )
            CALL REPMC ( TXT , '#2', ABCORR, TXT )
            CALL TCASE ( TXT )

            SEP = ZZSEPQ ( ET, &
                           1000, 2000, &
                           RADA, RADB, &
                           10, ABCORR, 'J2000' )

            CALL SPKPOS('ALPHA', ET, 'J2000', ABCORR, 'SUN', POSA, LT)
            CALL SPKPOS('BETA',  ET, 'J2000', ABCORR, 'SUN', POSB, LT)

            HANGA = DASIN( RADA/VNORM(POSA) )
            HANGB = DASIN( RADB/VNORM(POSB) )

            THETA = VSEP( POSA, POSB) - ( HANGA + HANGB )
            CALL CHCKSD ( 'ZZSEPQ', SEP, '~', THETA, VTIGHT, OK )

            ET  = ET + 60.D0

         END DO

      END DO




!
!     Case N
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
