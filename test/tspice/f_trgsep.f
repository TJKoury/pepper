C$Procedure F_TRGSEP ( TRGSEP tests )

      SUBROUTINE F_TRGSEP ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        TRGSEP
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

      INCLUDE 'zzabcorr.inc'
      INCLUDE 'zzbodtrn.inc'

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
C     geometry routine TRGSEP.
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
C     M. Costa Sitja     (JPL)
C     J. Diaz del Rio    (ODC Space)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 30-NOV-2021 (MCS) (JDR)
C
C        Initial version based on procedure F_ZZSEPQ
C        (ZZSEPQ family tests).
C
C-&


C$ Index_Entries
C
C     Test family for TRGSEP
C
C-&


C
C     SPICELIB functions
C
      DOUBLE PRECISION      VSEP
      DOUBLE PRECISION      DASINE
      DOUBLE PRECISION      TRGSEP
      DOUBLE PRECISION      VNORM

C
C     Local parameters
C
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

C
C     ATOL is a tolerance value for computing arc sine.
C

      DOUBLE PRECISION      ATOL
      PARAMETER           ( ATOL   = 1.D-12       )

C
C     Local variables
C
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

C
C     Saved variables
C
      SAVE

C
C     Initial values
C
      DATA             CORR   / 'NONE',
     .                          'lt',  ' lt+s',  ' cn',  ' cn + s',
     .                          'XLT', 'XLT + S', 'XCN', 'XCN+S',
     .                          'BAD'                             /

      DATA             FRAME  / 'ALPHA_FIXED', 'BETA_FIXED',
     .                          'NULL', 'BAD'                     /

      DATA             TARG   / 'ALPHA', 'BETA',
     .                          'GAMMA', 'CALUFRAX'               /

      DATA             SHAPE  / 'POINT', 'SPHERE', 'BAD'          /

      DATA             RAD1   / 1.0D6, 1.0D6, 1.0D6               /

      DATA             VALCR  / 2.1816615649929D-02,
     .                          2.1889337702096D-02,
     .                          2.1889337702096D-02,
     .                          2.1889337702096D-02,
     .                          2.1889337702096D-02,
     .                          2.1743893597763D-02,
     .                          2.1743893597763D-02,
     .                          2.1743893597763D-02,
     .                          2.1743893597763D-02               /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_TRGSEP' )


C
C     *****************************************************************
C
C     Setup tests: TRGSEP
C
C     *****************************************************************
C
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

C
C     Create an LSK, load using FURNSH.
C
      CALL ZZTSTLSK( LSK )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      CALL FURNSH  ( LSK )
      CALL CHCKXC  ( .FALSE., ' ', OK )

C
C     Create the PCK for Nat's Solar System.
C
      CALL NATPCK  ( PCK1,  .FALSE., .TRUE.  )
      CALL CHCKXC  ( .FALSE., ' ',   OK )

      CALL FURNSH  ( PCK1 )
      CALL CHCKXC  ( .FALSE., ' ', OK )

C
C     Create the SPK for Nat's Solar System.
C
      CALL NATSPK  ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC  ( .FALSE., ' ',   OK )

      CALL FURNSH  ( SPK1 )
      CALL CHCKXC  ( .FALSE., ' ',   OK )

      TIME0 = '2000 JAN 1  12:00:00 TDB'

      CALL STR2ET  ( TIME0, ET0 )
      CALL CHCKXC  ( .FALSE., ' ', OK )


C
C     *****************************************************************
C
C     Normal cases: TRGSEP
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Initial case to confirm functionality.
C
      CALL CHCKXC ( .FALSE., ' ',   OK )

      OBSRVR = 'SUN'

      DO I = 1, NCORR

         CALL TCASE ( 'Confirm basic functionality'// ' ' // CORR(I))

         VALUE = TRGSEP ( ET0,
     .                    TARG(1),  SHAPE(1), FRAME(1),
     .                    TARG(2),  SHAPE(1), FRAME(2),
     .                    OBSRVR,   CORR(I) )

         CALL CHCKXC ( .FALSE., ' ',   OK )
         CALL CHCKSD ( 'VALUE', VALUE, '~', VALCR(I), TIGHT, OK )

      END DO

C
C     --- Case: -------------------------------------------------------
C
C     Run a loop over one orbit of ALPHA at 50 TDB second intervals
C     with each of the aberration corrections. Any occultation event
C     exists for longer than 50 seconds. Compare the value returned
C     from TRGSEP with a direct calculation.
C

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

C
C     1 - point, point
C     2 - sphere, point
C     3 - point, sphere,
C     4 - sphere, sphere.
C
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

                  VALUE = TRGSEP( ET,
     .                            TARG(1), SHAPE(L), FRAME(1),
     .                            TARG(2), SHAPE(K), FRAME(2),
     .                            OBSRVR,  CORR(J) )

                  CALL SPKPOS ( TARG(1), ET, REF, CORR(J), OBSRVR,
     .                          PV1, LT )

                  CALL SPKPOS ( TARG(2), ET, REF, CORR(J), OBSRVR,
     .                          PV2, LT )

                  RANGE1 = VNORM( PV1 )
                  RANGE2 = VNORM( PV2 )

                  ANG1   = DASINE( R1/RANGE1, ATOL )
                  ANG2   = DASINE( R2/RANGE2, ATOL )

                  THETA = VSEP( PV1, PV2 ) - ANG1 - ANG2

C
C                 The results should match to roundoff, or
C                 something badly wrong.
C
                  CALL CHCKSD ( TITLE, VALUE, '=', THETA, TIGHT, OK )

               END DO

            END DO

         END DO

      END DO


C
C     *****************************************************************
C
C     Non-error exceptional cases: TRGSEP
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Null string as an argument for FRAME1 and FRAME2.
C
      CALL TCASE ( 'Empty string as an argument for frame' )

      ABCORR = 'NONE'

      VALUE = TRGSEP ( ET0,
     .                 TARG(1), SHAPE(1), FRAME(4),
     .                 TARG(2), SHAPE(2), FRAME(2),
     .                 OBSRVR,  ABCORR )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1), SHAPE(1), FRAME(1),
     .                 TARG(2), SHAPE(2), FRAME(4),
     .                 OBSRVR,  ABCORR )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1), SHAPE(1), FRAME(4),
     .                 TARG(2), SHAPE(2), FRAME(4),
     .                 OBSRVR,  ABCORR )

      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C     *****************************************************************
C
C     Error cases: TRGSEP
C
C     *****************************************************************
C
C     --- Case: -------------------------------------------------------
C
C     Three objects TARG1, TARG2 and OBSRVR  are not distinct
C
      CALL TCASE ( 'Bodies not distinct' )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1),  SHAPE(1), FRAME(1),
     .                 TARG(1),  SHAPE(1), FRAME(2),
     .                 OBSRVR,   CORR(1)  )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1),  SHAPE(1), FRAME(1),
     .                 TARG(2),  SHAPE(1), FRAME(2),
     .                 TARG(2),  CORR(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1),  SHAPE(1), FRAME(1),
     .                 TARG(2),  SHAPE(1), FRAME(2),
     .                 TARG(1),  CORR(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


C
C     --- Case: -------------------------------------------------------
C
C     Incorrect object names for TARG1, TARG2 and OBSRVR.
C
      CALL TCASE ( 'Bad TARG1' )

      VALUE = TRGSEP ( ET0,
     .                 TARG(4),  SHAPE(1), FRAME(1),
     .                 TARG(2),  SHAPE(1), FRAME(2),
     .                 OBSRVR,   CORR(1) )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      CALL TCASE ( 'Bad TARG2' )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1),  SHAPE(1), FRAME(1),
     .                 TARG(4),  SHAPE(1), FRAME(2),
     .                 OBSRVR,   CORR(1) )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      CALL TCASE ( 'Bad OBSRVR' )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1),  SHAPE(1), FRAME(1),
     .                 TARG(2),  SHAPE(1), FRAME(2),
     .                 TARG(4),  CORR(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C     --- Case: -------------------------------------------------------
C
C     Incorrect centers for TARG1, FRAME1 and TARG2, FRAME2.
C
C
C     The current version of TRGSEP ignores the FRAME argument.
C     When 'ELLIPSOID' is implemented, the following Exception
C     test is to be implemented:
C
C     If the reference frame associated with TARG1, FRAME1, is not
C     centered on TARG1, or if the reference frame associated with
C     TARG2, FRAME2, is not centered on TARG2, an error is signaled
C     by a routine in the call tree of this routine. This
C     restriction does not apply to shapes 'SPHERE' and 'POINT', for
C     which the frame input is ignored.
C


C
C     --- Case: -------------------------------------------------------
C
C     Incorrect body shapes for targets.
C
      CALL TCASE ( 'Bad SHAPE1' )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1),  SHAPE(3), FRAME(1),
     .                 TARG(2),  SHAPE(1), FRAME(2),
     .                 OBSRVR,   CORR(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


      CALL TCASE ( 'Bad SHAPE2' )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1),  SHAPE(2), FRAME(1),
     .                 TARG(2),  SHAPE(3), FRAME(2),
     .                 OBSRVR,   CORR(1) )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


C
C     --- Case: -------------------------------------------------------
C
C     Incorrect aberration correction.
C
      CALL TCASE ( 'Bad ABCORR' )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1),  SHAPE(2), FRAME(1),
     .                 TARG(2),  SHAPE(2), FRAME(2),
     .                 OBSRVR,   CORR(NCORR+1) )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C     --- Case: -------------------------------------------------------
C
C     PCK data unavailable.
C
      CALL TCASE ( 'PCK data unavailable' )

      ABCORR = 'NONE'
      TARG2  = 'EARTH'


      VALUE = TRGSEP ( ET0,
     .                 TARG(1), SHAPE(1), FRAME(1),
     .                 TARG2,   SHAPE(2), FRAME(2),
     .                 OBSRVR,  ABCORR )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)',   OK )


C
C     --- Case: -------------------------------------------------------
C
C     Ephemeris data unavailable.
C
      CALL TCASE ( 'Ephemeris data unavailable' )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1),  SHAPE(1), FRAME(1),
     .                 TARG2,    SHAPE(1), FRAME(2),
     .                 OBSRVR,   ABCORR )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )


C
C     --- Case: -------------------------------------------------------
C
C     OBSRVR is located within either one of the targets.
C
      CALL TCASE ( 'OBSRVR is located within one of the targets' )

      CALL PDPOOL ( 'BODY1000_RADII', 3, RAD1 )

      VALUE = TRGSEP ( ET0,
     .                 TARG(1), SHAPE(2), FRAME(1),
     .                 TARG(2), SHAPE(1), FRAME(2),
     .                 TARG(3), ABCORR )

      CALL CHCKXC ( .TRUE., 'SPICE(INSIDEBODY)',   OK )


C
C     *****************************************************************
C
C     Clean up tests: TRGSEP
C
C     *****************************************************************
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
