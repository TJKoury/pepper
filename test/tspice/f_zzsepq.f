C$Procedure F_ZZSEPQ ( ZZSEPQ family tests )

      SUBROUTINE F_ZZSEPQ ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routines
C
C        ZZSEPQ
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

      INCLUDE 'gf.inc'
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
C     geometry routine ZZSEPQ.
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
C-    TSPICE Version 1.3.0, 30-NOV-2021 (EDW)
C
C        Added TRGSEP tests.
C
C-    TSPICE Version 1.2.0, 21-JUN-2017 (EDW)
C
C        Replaced calls to routine ZZGFSPQ with function ZZSEPQ.
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
      DOUBLE PRECISION      TRGSEP
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      ZZSEPQ


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
      CHARACTER*(16)        FRAME (3)
      CHARACTER*(16)        SHAPE (3)
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR + 1 )
      CHARACTER*(LNSIZE)    REF
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(MAXL)      OBSRVR
      CHARACTER*(MAXL)      TARG  (3)
      CHARACTER*(TIMLEN)    TIME0

      DOUBLE PRECISION      ANG1
      DOUBLE PRECISION      ANG2
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MXRAD  (2)
      DOUBLE PRECISION      PV1  ( 3 )
      DOUBLE PRECISION      PV2  ( 3 )
      DOUBLE PRECISION      R1
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      RADA ( 3 )
      DOUBLE PRECISION      RADB ( 3 )
      DOUBLE PRECISION      RANGE1
      DOUBLE PRECISION      RANGE2
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      VALUE
      DOUBLE PRECISION      XR   ( 2, 4)

      INTEGER               BODS    (2)
      INTEGER               DIM
      INTEGER               FRAMES  (2)
      INTEGER               XFRAME (2)
      INTEGER               HAN1
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               M
      INTEGER               OBS
      INTEGER               TARG1
      INTEGER               TARG2

C
C     Save all.
C
      SAVE

C
C     Initial values
C
      DATA             CORR / 'NONE',
     .                        'lt',  ' lt+s',  ' cn',  ' cn + s',
     .                        'XLT', 'XLT + S', 'XCN', 'XCN+S', 'BAD' /

      DATA             FRAME  / 'ALPHA_FIXED', 'BETA_FIXED', 'BAD' /

      DATA             TARG   / 'ALPHA', 'BETA', 'CALUFRAX'    /

      DATA             SHAPE  / 'POINT', 'SPHERE', 'BAD'        /


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZSEPQ' )

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


      TIME0 = '2000 JAN 1  12:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Case 1: ZZSPIN. Initial check to confirm functionality.
C

      OBSRVR = 'SUN'
      CALL NAMFRM ( FRAME(1), XFRAME(1) )
      CALL NAMFRM ( FRAME(2), XFRAME(2) )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      WRITE(*,*)

      DO I = 1, NCORR

         CALL TCASE ( 'Confirm basic functionality'// ' ' // CORR(I))

C
C        SHAPE(2), "SPHERE", to check MXRAD.
C
         CALL ZZSPIN ( TARG(1),  SHAPE(2), FRAME(1),
     .                 TARG(2),  SHAPE(2), FRAME(2),
     .                 OBSRVR,   CORR(I),
     .                 BODS,     FRAMES,   MXRAD,
     .                 OBS )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSI ( 'TARG(1) ID', BODS(1), '=', 1000, 0, OK )
         CALL CHCKSI ( 'TARG(2) ID', BODS(2), '=', 2000, 0, OK )
         CALL CHCKSI ( 'OBSRVR ID',  OBS,     '=', 10, 0, OK )

         CALL CHCKSD ( 'MAXRAD(1)', MXRAD(1), '~',
     .                              0.73249397533875424D+05,
     .                              TIGHT, OK )

         CALL CHCKSD ( 'MAXRAD(1)', MXRAD(2), '~',
     .                             0.22891526271046937D+04,
     .                             TIGHT, OK )

C
C         The current version of ZZSPIN ignores the FRAME argument.
C
C         CALL CHCKSI ( 'FRAMES(1) ID', FRAMES(1), '=',
C     .                                 XFRAME(1), 0, OK )
C         CALL CHCKSI ( 'FRAMES(2) ID', FRAMES(2), '=',
C     .                                 XFRAME(2), 0, OK )
C

      END DO

      CALL TCASE ( 'BAD TARG1' )

      CALL ZZSPIN ( TARG(3),  SHAPE(1), FRAME(1),
     .              TARG(2),  SHAPE(1), FRAME(2),
     .              OBSRVR,   CORR(1),
     .              BODS,     FRAMES,   MXRAD,
     .              OBS )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      CALL TCASE ( 'BAD TARG2' )

      CALL ZZSPIN ( TARG(1),  SHAPE(1), FRAME(1),
     .              TARG(3),  SHAPE(1), FRAME(2),
     .              OBSRVR,   CORR(I),
     .              BODS,     FRAMES,   MXRAD,
     .              OBS )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      CALL TCASE ( 'BAD OBSRVR' )

      CALL ZZSPIN ( TARG(1),  SHAPE(1), FRAME(1),
     .              TARG(2),  SHAPE(1), FRAME(2),
     .              TARG(3),  CORR(1),
     .              BODS,     FRAMES,   MXRAD,
     .              OBS )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      CALL TCASE ( 'Bodies not distinct' )

      CALL ZZSPIN ( TARG(1),  SHAPE(1), FRAME(1),
     .              TARG(1),  SHAPE(1), FRAME(2),
     .              OBSRVR,  CORR(1),
     .              BODS,     FRAMES,   MXRAD,
     .              OBS )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      CALL ZZSPIN ( TARG(1),  SHAPE(1), FRAME(1),
     .              TARG(2),  SHAPE(1), FRAME(2),
     .              TARG(2),  CORR(1),
     .              BODS,     FRAMES,   MXRAD,
     .              OBS )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

      CALL ZZSPIN ( TARG(1),  SHAPE(1), FRAME(1),
     .              TARG(2),  SHAPE(1), FRAME(2),
     .              TARG(1),  CORR(1),
     .              BODS,     FRAMES,   MXRAD,
     .              OBS )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


      CALL TCASE ( 'BAD SHAPE1' )

      CALL ZZSPIN ( TARG(1),  SHAPE(3), FRAME(1),
     .              TARG(2),  SHAPE(1), FRAME(2),
     .              OBSRVR,   CORR(1),
     .              BODS,     FRAMES,   MXRAD,
     .              OBS )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


      CALL TCASE ( 'BAD SHAPE2)' )

      CALL ZZSPIN ( TARG(1),  SHAPE(2), FRAME(1),
     .              TARG(2),  SHAPE(3), FRAME(2),
     .              OBSRVR,   CORR(1),
     .              BODS,     FRAMES,   MXRAD,
     .              OBS )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)', OK )


      CALL TCASE ( 'BAD ABCORR)' )

      CALL ZZSPIN ( TARG(1),  SHAPE(2), FRAME(1),
     .              TARG(2),  SHAPE(2), FRAME(2),
     .              OBSRVR,   CORR(NCORR+1),
     .              BODS,     FRAMES,   MXRAD,
     .              OBS )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )



C
C     Case 2: ZZSEPQ
C
      CALL TCASE ( 'Ephemeris data unavailable' )

      TARG1 = -203
      TARG2 = 499
      OBS   = 10
      R1    = 0.D0
      R2    = 0.D0
      REF   = 'J2000'
      ABCORR= 'NONE'

      VALUE = ZZSEPQ( ET0, TARG1, TARG2, R1, R2,
     .                 OBS, ABCORR, REF )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RADA )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )

      CALL BODVRD ( 'BETA',  'RADII', 3, DIM, RADB )
      CALL CHCKSI ( 'BODVRD ALPHA', DIM, '=', 3, 0, OK )


C
C     Case 3
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

      VALUE = ZZSEPQ ( ET0, TARG1, TARG2, R1, R2,
     .                 OBS, ABCORR, REF )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUS)',   OK )

      R1    = 0.D0
      R2    = -RADB(1)

      VALUE = ZZSEPQ ( ET0, TARG1, TARG2, R1, R2,
     .               OBS, ABCORR, REF )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUS)',   OK )



C
C     Case 4
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

         VALUE = ZZSEPQ( ET0, TARG1, TARG2, R1, R2,
     .                  OBS, CORR(I), REF )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END DO


C
C     Check a simple calculation for invalid CORR.
C
      ABCORR = CORR(NCORR + 1)

      CALL TCASE ( ABCORR )

      VALUE = ZZSEPQ ( ET0, TARG1, TARG2, R1, R2,
     .               OBS, ABCORR, REF )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)', OK )


C
C     Case 5
C

C
C     Run a loop over one orbit of ALPHA at 50 TDB second
C     intervals with each of the aberration corrections.
C     Any occultation event exists for longer than 50 seconds.
C     Compare the value returned from ZZSEPQ with a direct
C     calculation.
C
      TARG1 = 1000
      TARG2 = 2000
      OBS   = 10
      REF   = 'J2000'

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

               VALUE = ZZSEPQ( ET, TARG1, TARG2, R1, R2,
     .                          OBS, ABCORR, REF )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL SPKEZP ( TARG1, ET, REF, ABCORR, OBS, PV1, LT )
               CALL SPKEZP ( TARG2, ET, REF, ABCORR, OBS, PV2, LT )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               RANGE1 = VNORM( PV1 )
               RANGE2 = VNORM( PV2 )
               ANG1   = DASINE( R1/RANGE1, ATOL )
               ANG2   = DASINE( R2/RANGE2, ATOL )

               THETA = VSEP( PV1, PV2 ) - ANG1 - ANG2
C
C              The results should match to roundoff.
C
               CALL CHCKSD ( TITLE, VALUE, '=', THETA, TIGHT, OK )

            END DO

         END DO

      END DO



C
C     Same loop using TRGSEP.
C

      REF   = 'J2000'

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

      DO I=0, 86400, 50

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
                  CALL TCASE( TITLE )

                  ET = ET0 + DBLE(I)

                  VALUE = TRGSEP( ET,
     .                         TARG(1),  SHAPE(L), FRAME(1),
     .                         TARG(2),  SHAPE(K), FRAME(2),
     .                         OBSRVR, ABCORR )
                  CALL CHCKXC ( .FALSE., ' ',   OK )


                  CALL SPKEZP ( TARG1, ET, REF, ABCORR, OBS, PV1, LT )
                  CALL CHCKXC ( .FALSE., ' ',   OK )

                  CALL SPKEZP ( TARG2, ET, REF, ABCORR, OBS, PV2, LT )
                  CALL CHCKXC ( .FALSE., ' ',   OK )

                  RANGE1 = VNORM( PV1 )
                  RANGE2 = VNORM( PV2 )

                  ANG1   = DASINE( R1/RANGE1, ATOL )
                  CALL CHCKXC ( .FALSE., ' ',   OK )

                  ANG2   = DASINE( R2/RANGE2, ATOL )
                  CALL CHCKXC ( .FALSE., ' ',   OK )

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
