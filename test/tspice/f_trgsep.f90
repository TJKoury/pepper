!$Procedure F_TRGSEP ( TRGSEP tests )

      SUBROUTINE F_TRGSEP ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        TRGSEP
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

      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzbodtrn.inc'

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
!     geometry routine TRGSEP.
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
!     M. Costa Sitja     (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 30-NOV-2021 (MCS) (JDR)
!
!        Initial version based on procedure F_ZZSEPQ
!        (ZZSEPQ family tests).
!
!-&


! $ Index_Entries
!
!     Test family for TRGSEP
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      VSEP
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      TRGSEP
      DOUBLE PRECISION      VNORM

!
!     Local parameters
!
      CHARACTER*(*)         LSK
      PARAMETER           ( LSK    = 'trgsep.tls' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp'    )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck'    )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12       )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80           )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50           )

      INTEGER               NCORR
      PARAMETER           ( NCORR  = 9            )

!
!     ATOL is a tolerance value for computing arc sine.
!

      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL   = 1.D-12       )

!
!     Local variables
!
      CHARACTER*(16)        FRAME ( 4 )
      CHARACTER*(16)        SHAPE ( 3 )
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR + 1 )
      CHARACTER*(LNSIZE)    REF
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    TARG2
      CHARACTER*(MAXL)      OBSRVR
      CHARACTER*(MAXL)      TARG  (4)
      CHARACTER*(TIMLEN)    TIME0


      DOUBLE PRECISION      ANG1
      DOUBLE PRECISION      ANG2
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      PV1  ( 3 )
      DOUBLE PRECISION      PV2  ( 3 )
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      RADA ( 3 )
      DOUBLE PRECISION      RADB ( 3 )
      DOUBLE PRECISION      RAD1 ( 3 )

      DOUBLE PRECISION      RANGE1
      DOUBLE PRECISION      RANGE2
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      VALCR ( NCORR )
      DOUBLE PRECISION      VALUE

      DOUBLE PRECISION      XR   ( 2, 4 )

      INTEGER               DIM
      INTEGER               HAN1
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               M

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA             CORR   / 'NONE', &
                                'lt',  ' lt+s',  ' cn',  ' cn + s', &
                                'XLT', 'XLT + S', 'XCN', 'XCN+S', &
                                'BAD'                             /

      DATA             FRAME  / 'ALPHA_FIXED', 'BETA_FIXED', &
                                'NULL', 'BAD'                     /

      DATA             TARG   / 'ALPHA', 'BETA', &
                                'GAMMA', 'CALUFRAX'               /

      DATA             SHAPE  / 'POINT', 'SPHERE', 'BAD'          /

      DATA             RAD1   / 1.0D6, 1.0D6, 1.0D6               /

      DATA             VALCR  / 2.1816615649929D-02, &
                                2.1889337702096D-02, &
                                2.1889337702096D-02, &
                                2.1889337702096D-02, &
                                2.1889337702096D-02, &
                                2.1743893597763D-02, &
                                2.1743893597763D-02, &
                                2.1743893597763D-02, &
                                2.1743893597763D-02               /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_TRGSEP' )


!
!     *****************************************************************
!
!     Setup tests: TRGSEP
!
!     *****************************************************************
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

!
!     Create an LSK, load using FURNSH.
!
      CALL ZZTSTLSK( LSK )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL FURNSH  ( LSK )
      CALL CHCKXC  ( .FALSE., ' ', OK )

!
!     Create the PCK for Nat's Solar System.
!
      CALL NATPCK  ( PCK1,  .FALSE., .TRUE.  )
      CALL CHCKXC  ( .FALSE., ' ',   OK )

      CALL FURNSH  ( PCK1 )
      CALL CHCKXC  ( .FALSE., ' ', OK )

!
!     Create the SPK for Nat's Solar System.
!
      CALL NATSPK  ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC  ( .FALSE., ' ',   OK )

      CALL FURNSH  ( SPK1 )
      CALL CHCKXC  ( .FALSE., ' ',   OK )

      TIME0 = '2000 JAN 1  12:00:00 TDB'

      CALL STR2ET  ( TIME0, ET0 )
      CALL CHCKXC  ( .FALSE., ' ', OK )


!
!     *****************************************************************
!
!     Normal cases: TRGSEP
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Initial case to confirm functionality.
!
      CALL CHCKXC ( .FALSE., ' ',   OK )

      OBSRVR = 'SUN'

      DO I = 1, NCORR

         CALL TCASE ( 'Confirm basic functionality'// ' ' // CORR(I))

         VALUE = TRGSEP ( ET0, &
                          TARG(1),  SHAPE(1), FRAME(1), &
                          TARG(2),  SHAPE(1), FRAME(2), &
                          OBSRVR,   CORR(I) )

         CALL CHCKXC ( .FALSE., ' ',   OK )
         CALL CHCKSD ( 'VALUE', VALUE, '~', VALCR(I), TIGHT, OK )

      END DO

!
!     --- Case: -------------------------------------------------------
!
!     Run a loop over one orbit of ALPHA at 50 TDB second intervals
!     with each of the aberration corrections. Any occultation event
!     exists for longer than 50 seconds. Compare the value returned
!     from TRGSEP with a direct calculation.
!

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

!
!     1 - point, point
!     2 - sphere, point
!     3 - point, sphere,
!     4 - sphere, sphere.
!
      XR(1,1) = 0.D0
      XR(1,2) = RADA(1)
      XR(1,3) = 0.D0
      XR(1,4) = RADA(1)
      XR(2,1) = 0.D0
      XR(2,2) = 0.D0
      XR(2,3) = RADB(1)
      XR(2,4) = RADB(1)

      REF     = 'J2000'

      DO I = 0, 86400, 50

         DO J = 1, NCORR

            ABCORR = CORR(J)
            M = 1

            DO K = 1, 2

               DO L = 1 , 2

                  R1 = XR(1,M)
                  R2 = XR(2,M)
                  M  = M + 1

                  TITLE = 'SEP ET = #1, R1 = #2, R2 = #3, CORR = #4'
                  CALL REPMD ( TITLE, '#1', ET, 6, TITLE )
                  CALL REPMD ( TITLE, '#2', R1, 6, TITLE )
                  CALL REPMD ( TITLE, '#3', R2, 6, TITLE )
                  CALL REPMI ( TITLE, '#4', J,     TITLE )
                  CALL TCASE ( TITLE )

                  ET = ET0 + DBLE(I)

                  VALUE = TRGSEP( ET, &
                                  TARG(1), SHAPE(L), FRAME(1), &
                                  TARG(2), SHAPE(K), FRAME(2), &
                                  OBSRVR,  CORR(J) )

                  CALL SPKPOS ( TARG(1), ET, REF, CORR(J), OBSRVR, &
                                PV1, LT )

                  CALL SPKPOS ( TARG(2), ET, REF, CORR(J), OBSRVR, &
                                PV2, LT )

                  RANGE1 = VNORM( PV1 )
                  RANGE2 = VNORM( PV2 )

                  ANG1   = DASINE( R1/RANGE1, ATOL )
                  ANG2   = DASINE( R2/RANGE2, ATOL )

                  THETA = VSEP( PV1, PV2 ) - ANG1 - ANG2

!
!                 The results should match to roundoff, or
!                 something badly wrong.
!
                  CALL CHCKSD ( TITLE, VALUE, '=', THETA, TIGHT, OK )

               END DO

            END DO

         END DO

      END DO


!
!     *****************************************************************
!
!     Non-error exceptional cases: TRGSEP
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Null string as an argument for FRAME1 and FRAME2.
!
      CALL TCASE ( 'Empty string as an argument for frame' )

      ABCORR = 'NONE'

      VALUE = TRGSEP ( ET0, &
                       TARG(1), SHAPE(1), FRAME(4), &
                       TARG(2), SHAPE(2), FRAME(2), &
                       OBSRVR,  ABCORR )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      VALUE = TRGSEP ( ET0, &
                       TARG(1), SHAPE(1), FRAME(1), &
                       TARG(2), SHAPE(2), FRAME(4), &
                       OBSRVR,  ABCORR )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      VALUE = TRGSEP ( ET0, &
                       TARG(1), SHAPE(1), FRAME(4), &
                       TARG(2), SHAPE(2), FRAME(4), &
                       OBSRVR,  ABCORR )

      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!     *****************************************************************
!
!     Error cases: TRGSEP
!
!     *****************************************************************
!
!     --- Case: -------------------------------------------------------
!
!     Three objects TARG1, TARG2 and OBSRVR  are not distinct
!
      CALL TCASE ( 'Bodies not distinct' )

      VALUE = TRGSEP ( ET0, &
                       TARG(1),  SHAPE(1), FRAME(1), &
                       TARG(1),  SHAPE(1), FRAME(2), &
                       OBSRVR,   CORR(1)  )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      VALUE = TRGSEP ( ET0, &
                       TARG(1),  SHAPE(1), FRAME(1), &
                       TARG(2),  SHAPE(1), FRAME(2), &
                       TARG(2),  CORR(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      VALUE = TRGSEP ( ET0, &
                       TARG(1),  SHAPE(1), FRAME(1), &
                       TARG(2),  SHAPE(1), FRAME(2), &
                       TARG(1),  CORR(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


!
!     --- Case: -------------------------------------------------------
!
!     Incorrect object names for TARG1, TARG2 and OBSRVR.
!
      CALL TCASE ( 'Bad TARG1' )

      VALUE = TRGSEP ( ET0, &
                       TARG(4),  SHAPE(1), FRAME(1), &
                       TARG(2),  SHAPE(1), FRAME(2), &
                       OBSRVR,   CORR(1) )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      CALL TCASE ( 'Bad TARG2' )

      VALUE = TRGSEP ( ET0, &
                       TARG(1),  SHAPE(1), FRAME(1), &
                       TARG(4),  SHAPE(1), FRAME(2), &
                       OBSRVR,   CORR(1) )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      CALL TCASE ( 'Bad OBSRVR' )

      VALUE = TRGSEP ( ET0, &
                       TARG(1),  SHAPE(1), FRAME(1), &
                       TARG(2),  SHAPE(1), FRAME(2), &
                       TARG(4),  CORR(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
!     --- Case: -------------------------------------------------------
!
!     Incorrect centers for TARG1, FRAME1 and TARG2, FRAME2.
!
!
!     The current version of TRGSEP ignores the FRAME argument.
!     When 'ELLIPSOID' is implemented, the following Exception
!     test is to be implemented:
!
!     If the reference frame associated with TARG1, FRAME1, is not
!     centered on TARG1, or if the reference frame associated with
!     TARG2, FRAME2, is not centered on TARG2, an error is signaled
!     by a routine in the call tree of this routine. This
!     restriction does not apply to shapes 'SPHERE' and 'POINT', for
!     which the frame input is ignored.
!


!
!     --- Case: -------------------------------------------------------
!
!     Incorrect body shapes for targets.
!
      CALL TCASE ( 'Bad SHAPE1' )

      VALUE = TRGSEP ( ET0, &
                       TARG(1),  SHAPE(3), FRAME(1), &
                       TARG(2),  SHAPE(1), FRAME(2), &
                       OBSRVR,   CORR(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


      CALL TCASE ( 'Bad SHAPE2' )

      VALUE = TRGSEP ( ET0, &
                       TARG(1),  SHAPE(2), FRAME(1), &
                       TARG(2),  SHAPE(3), FRAME(2), &
                       OBSRVR,   CORR(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


!
!     --- Case: -------------------------------------------------------
!
!     Incorrect aberration correction.
!
      CALL TCASE ( 'Bad ABCORR' )

      VALUE = TRGSEP ( ET0, &
                       TARG(1),  SHAPE(2), FRAME(1), &
                       TARG(2),  SHAPE(2), FRAME(2), &
                       OBSRVR,   CORR(NCORR+1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
!     --- Case: -------------------------------------------------------
!
!     PCK data unavailable.
!
      CALL TCASE ( 'PCK data unavailable' )

      ABCORR = 'NONE'
      TARG2  = 'EARTH'


      VALUE = TRGSEP ( ET0, &
                       TARG(1), SHAPE(1), FRAME(1), &
                       TARG2,   SHAPE(2), FRAME(2), &
                       OBSRVR,  ABCORR )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)',   OK )


!
!     --- Case: -------------------------------------------------------
!
!     Ephemeris data unavailable.
!
      CALL TCASE ( 'Ephemeris data unavailable' )

      VALUE = TRGSEP ( ET0, &
                       TARG(1),  SHAPE(1), FRAME(1), &
                       TARG2,    SHAPE(1), FRAME(2), &
                       OBSRVR,   ABCORR )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )


!
!     --- Case: -------------------------------------------------------
!
!     OBSRVR is located within either one of the targets.
!
      CALL TCASE ( 'OBSRVR is located within one of the targets' )

      CALL PDPOOL ( 'BODY1000_RADII', 3, RAD1 )

      VALUE = TRGSEP ( ET0, &
                       TARG(1), SHAPE(2), FRAME(1), &
                       TARG(2), SHAPE(1), FRAME(2), &
                       TARG(3), ABCORR )

      CALL CHCKXC ( .TRUE., 'SPICE(INSIDEBODY)',   OK )


!
!     *****************************************************************
!
!     Clean up tests: TRGSEP
!
!     *****************************************************************
!
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
