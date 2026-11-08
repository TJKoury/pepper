!$Procedure F_ZZEDTMPT ( ZZEDTMPT tests )

      SUBROUTINE F_ZZEDTMPT ( OK )

! $ Abstract
!
!     Exercise the routine ZZEDTMPT.
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

      INCLUDE 'zzctr.inc'

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
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB routine ZZEDTMPT.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 30-JUN-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Other functions
!
!        T_RANDD ( LOWER, UPPER, SEED )
!
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      T_RANDD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT

!
!     Local Parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-13 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )
!
!     Local Variables
!
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(1)         LCHAR  ( 2 )

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALPHA
      DOUBLE PRECISION      AXIOFF ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BETA
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      DOT
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PLNVEC ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      PROJ   ( 3 )
      DOUBLE PRECISION      REFPLN ( UBPL )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SRCRAD
      DOUBLE PRECISION      TANPLN ( UBPL )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XPOINT ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NSHAPE
      INTEGER               SEED
      INTEGER               SHADIX

      LOGICAL               UMBRAL


!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!
      DATA                  LCHAR  / 'T', 'F' /
      DATA                  ORIGIN / 3 * 0.D0 /


!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZEDTMPT' )



!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
!     The following simple cases are meant to assist debugging.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Light source and target are spheres of ' &
      //           'the same size: A. Distance between centers ' &
      //           'is 3*A. Umbral case.'                       )

      A      =  1.D3
      B      =  A
      C      =  A
      SRCRAD =  A

      UMBRAL = .TRUE.

      CALL VPACK ( 3*A,  0.D0, 0.D0, AXIS   )

      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSCL ( 0.5D0*SQRT(2.D0)*A, PLNVEC, XPOINT )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Light source and target are spheres of ' &
      //           'the same size: A. Distance between centers ' &
      //           'is 3*A. Penumbral case.'                     )

      A      =  1.D3
      B      =  A
      C      =  A
      SRCRAD =  A

      UMBRAL = .FALSE.

      CALL VPACK ( 3*A,  0.D0, 0.D0, AXIS   )

      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     A tangent segment connecting the sphere and target,
!     in the plane defined by PLNVEC, crosses the axis with
!     angle ALPHA, where
!
!        sin( ALPHA ) = A / (1.5 * A)
!
!     The complementary angle is BETA.
!
      ALPHA = ASIN ( 1.D0/1.5D0 )

      BETA  = HALFPI() - ALPHA

      CALL VSCL ( SIN(BETA) * A * SQRT(2.D0)/2, PLNVEC, XPOINT )

      XPOINT(1) = A * COS(BETA)


      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Triaxial target. Source is larger than longest ' &
      //           'target axis. Umbral case.'                     )

      A      =  1.D3
      B      =  2*A
      C      =  A/2
      SRCRAD =  10*A


      UMBRAL = .TRUE.

      CALL VPACK ( 20*A,  0.D0, 0.D0, AXIS   )

      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure POINT is on the target. Find the scaled version
!     of POINT on the target surface and compare.
!
      CALL EDPNT ( POINT, A, B, C, XPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

!
!     Make sure POINT is in the reference plane. Project POINT
!     onto the plane and compare.
!
      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, REFPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( POINT, REFPLN, XPOINT )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

!
!     Make sure POINT is on the correct side of the axis with respect
!     to PLNVEC.
!
      DOT = VDOT ( POINT, PLNVEC )

      CALL CHCKSD ( 'point DOT', DOT, '>', 0.D0, TOL, OK )

!
!     Create a tangent plane at the terminator point. The distance
!     between this plane and the center of the source should be
!     the radius of the source.
!
      CALL SURFNM ( A, B, C, POINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NVP2PL ( NORMAL, POINT, TANPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( AXIS, TANPLN, PROJ   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DIST = VDIST( AXIS, PROJ )

      TOL  = TIGHT

      CALL CHCKSD ( 'DIST', DIST, '~/', SRCRAD, TOL, OK )

!
!     Make sure the source center is on the correct side of the
!     tangent plane.
!
      CALL VSUB ( AXIS, PROJ, AXIOFF )

      DOT = VDOT ( AXIOFF, NORMAL )

      CALL CHCKSD ( 'axis DOT', DOT, '<', 0.D0, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Triaxial target. Source is smaller than shortest ' &
      //           'target axis. Umbral case.'                       )

      A      =  1.D3
      B      =  2*A
      C      =  A/2
      SRCRAD =  A/10


      UMBRAL = .TRUE.

      CALL VPACK ( 40*A,  0.D0, 0.D0, AXIS   )

      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure POINT is on the target. Find the scaled version
!     of POINT on the target surface and compare.
!
      CALL EDPNT ( POINT, A, B, C, XPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

!
!     Make sure POINT is in the reference plane. Project POINT
!     onto the plane and compare.
!
      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, REFPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( POINT, REFPLN, XPOINT )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

!
!     Make sure POINT is on the correct side of the axis with respect
!     to PLNVEC.
!
      DOT = VDOT ( POINT, PLNVEC )

      CALL CHCKSD ( 'DOT', DOT, '>', 0.D0, TOL, OK )

!
!     Create a tangent plane at the terminator point. The distance
!     between this plane and the center of the source should be
!     the radius of the source.
!
      CALL SURFNM ( A, B, C, POINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NVP2PL ( NORMAL, POINT, TANPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( AXIS, TANPLN, PROJ   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DIST = VDIST( AXIS, PROJ )

      TOL  = TIGHT

      CALL CHCKSD ( 'DIST', DIST, '~/', SRCRAD, TOL, OK )


!
!     Make sure the source center is on the correct side of the
!     tangent plane.
!
      CALL VSUB ( AXIS, PROJ, AXIOFF )

      DOT = VDOT ( AXIOFF, NORMAL )

      CALL CHCKSD ( 'axis DOT', DOT, '<', 0.D0, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Triaxial target. Source is larger than longest ' &
      //           'target axis. Penumbral case.'                  )

      A      =  1.D3
      B      =  2*A
      C      =  A/2
      SRCRAD =  10*A

      UMBRAL = .FALSE.

      CALL VPACK ( 30*A,  0.D0, 0.D0, AXIS   )

      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure POINT is on the target. Find the scaled version
!     of POINT on the target surface and compare.
!
      CALL EDPNT ( POINT, A, B, C, XPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

!
!     Make sure POINT is in the reference plane. Project POINT
!     onto the plane and compare.
!
      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, REFPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( POINT, REFPLN, XPOINT )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

!
!     Make sure POINT is on the correct side of the axis with respect
!     to PLNVEC.
!
      DOT = VDOT ( POINT, PLNVEC )

      CALL CHCKSD ( 'DOT', DOT, '>', 0.D0, TOL, OK )

!
!     Create a tangent plane at the terminator point. The distance
!     between this plane and the center of the source should be
!     the radius of the source.
!
      CALL SURFNM ( A, B, C, POINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NVP2PL ( NORMAL, POINT, TANPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( AXIS, TANPLN, PROJ   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DIST = VDIST( AXIS, PROJ )

      TOL  = TIGHT

      CALL CHCKSD ( 'DIST', DIST, '~/', SRCRAD, TOL, OK )


!
!     Make sure the source center is on the correct side of the
!     tangent plane.
!
      CALL VSUB ( AXIS, PROJ, AXIOFF )

      DOT = VDOT ( AXIOFF, NORMAL )

      CALL CHCKSD ( 'axis DOT', DOT, '>', 0.D0, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Triaxial target. Source is smaller than shortest ' &
      //           'target axis. Penumbral case.'                    )

      A      =  1.D3
      B      =  2*A
      C      =  A/2
      SRCRAD =  A/10


      UMBRAL = .FALSE.

      CALL VPACK ( 30*A,  0.D0, 0.D0, AXIS   )

      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure POINT is on the target. Find the scaled version
!     of POINT on the target surface and compare.
!
      CALL EDPNT ( POINT, A, B, C, XPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

!
!     Make sure POINT is in the reference plane. Project POINT
!     onto the plane and compare.
!
      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, REFPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( POINT, REFPLN, XPOINT )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', XPOINT, 3, TOL, OK )

!
!     Make sure POINT is on the correct side of the axis with respect
!     to PLNVEC.
!
      DOT = VDOT ( POINT, PLNVEC )

      CALL CHCKSD ( 'DOT', DOT, '>', 0.D0, TOL, OK )

!
!     Create a tangent plane at the terminator point. The distance
!     between this plane and the center of the source should be
!     the radius of the source.
!
      CALL SURFNM ( A, B, C, POINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NVP2PL ( NORMAL, POINT, TANPLN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPRJP ( AXIS, TANPLN, PROJ   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DIST = VDIST( AXIS, PROJ )

      TOL  = TIGHT

      CALL CHCKSD ( 'DIST', DIST, '~/', SRCRAD, TOL, OK )


!
!     Make sure the source center is on the correct side of the
!     tangent plane.
!
      CALL VSUB ( AXIS, PROJ, AXIOFF )

      DOT = VDOT ( AXIOFF, NORMAL )

      CALL CHCKSD ( 'axis DOT', DOT, '>', 0.D0, TOL, OK )


!
!
!     The following cases use a variety of target shapes, scales, and
!     light source positions
!
!
      NLAT = 11
      NLON = 10

      DLAT =   PI() / (NLAT - 1)
      DLON = 2*PI() /  NLON

      NSHAPE = 10

      SEED   = -100


      DO SHADIX = 1, 2
!
!        Set the shadow type flag.
!
         UMBRAL =  ( SHADIX .EQ. 1 )

         DO I = 1, NLON

            LON = (I-1)*DLON

            DO J = 1, NLAT

               LAT = HALFPI() - (J-1)*DLAT

               DO K = 1, NSHAPE
!
!                 Pick scale factor.
!
                  SCALE = 10.D0 ** T_RANDD( -100.D0, 100.D0, SEED )

                  A      = SCALE *  T_RANDD( 1.D0, 10.D0, SEED )
                  B      = SCALE *  T_RANDD( 1.D0, 10.D0, SEED )
                  C      = SCALE *  T_RANDD( 1.D0, 10.D0, SEED )
                  SRCRAD = SCALE *  T_RANDD( 1.D0, 10.D0, SEED )
                  CALL CHCKXC ( .FALSE., ' ', OK )


!
!                 Pick a rotation angle for the cutting half plane.
!
                  THETA = T_RANDD ( 0.D0, 2*PI(), SEED )

                  CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
                  TITLE =  'Random case. SCALE = #; A = #; B = #; ' &
                  //       'C = #; SRCRAD = #, THETA (deg) = #; ' &
                  //       'UMBRAL = #.'

                  CALL REPMD ( TITLE, '#', SCALE,       14, TITLE )
                  CALL REPMD ( TITLE, '#', A,           14, TITLE )
                  CALL REPMD ( TITLE, '#', B,           14, TITLE )
                  CALL REPMD ( TITLE, '#', C,           14, TITLE )
                  CALL REPMD ( TITLE, '#', SRCRAD,      14, TITLE )
                  CALL REPMD ( TITLE, '#', THETA*DPR(), 14, TITLE )
                  CALL REPMC ( TITLE, '#', LCHAR(SHADIX),   TITLE )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL TCASE ( TITLE )

                  CALL LATREC ( 1000.D0*SCALE, LON, LAT, AXIS )

                  CALL VEQU  ( AXIS, X )
                  CALL FRAME ( X, Y, Z )

                  CALL VROTV ( Z, AXIS, THETA, PLNVEC )

                  CALL ZZEDTMPT ( UMBRAL, A,    B,      C, &
                                  SRCRAD, AXIS, PLNVEC, POINT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Make sure POINT is on the target. Find the scaled
!                 version of POINT on the target surface and compare.
!
                  CALL EDPNT ( POINT, A, B, C, XPOINT )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  TOL = VTIGHT

                  CALL CHCKAD ( 'POINT', POINT, '~~/', &
                                         XPOINT, 3, TOL, OK )

!
!                 Make sure POINT is in the reference plane. Project
!                 POINT onto the plane and compare.
!
                  CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, REFPLN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL VPRJP ( POINT, REFPLN, XPOINT )

                  TOL = VTIGHT

                  CALL CHCKAD ( 'POINT', POINT, '~~/', &
                                         XPOINT, 3, TOL, OK )
!
!                 Make sure POINT is on the correct side of the axis
!                 with respect to PLNVEC.
!
                  DOT = VDOT ( POINT, PLNVEC )

                  CALL CHCKSD ( 'DOT', DOT, '>', 0.D0, TOL, OK )

!
!                 Create a tangent plane at the terminator point. The
!                 distance between this plane and the center of the
!                 source should be the radius of the source.
!
                  CALL SURFNM ( A, B, C, POINT, NORMAL )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL NVP2PL ( NORMAL, POINT, TANPLN )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL VPRJP ( AXIS, TANPLN, PROJ   )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  DIST = VDIST( AXIS, PROJ )

                  TOL  = TIGHT

                  CALL CHCKSD ( 'DIST', DIST, '~/', SRCRAD, TOL, OK )

!
!                 Make sure the source center is on the correct side of
!                 the tangent plane.
!
                  CALL VSUB ( AXIS, PROJ, AXIOFF )

                  DOT = VDOT ( AXIOFF, NORMAL )

                  IF ( UMBRAL ) THEN

                     CALL CHCKSD ( 'axis DOT', DOT, '<', 0.D0, TOL, OK )
                  ELSE
                     CALL CHCKSD ( 'axis DOT', DOT, '>', 0.D0, TOL, OK )
                  END IF

               END DO

            END DO

         END DO

      END DO



!**********************************************************************
!
!     Error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad target radii.' )


      A      =  1.D3
      B      =  A
      C      =  A
      SRCRAD =  A

      UMBRAL = .FALSE.

      CALL VPACK ( 3*A,  0.D0, 0.D0, AXIS   )

      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL,  0.D0, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTMPT ( UMBRAL, -1.D0, B, C, SRCRAD, AXIS, PLNVEC, POINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTMPT ( UMBRAL, A,    0.D0, C, SRCRAD, AXIS, &
                      PLNVEC, POINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTMPT ( UMBRAL, A,   -1.D0, C, SRCRAD, AXIS, &
                      PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTMPT ( UMBRAL, A,    B,  0.D0, SRCRAD, AXIS, &
                      PLNVEC, POINT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

      CALL ZZEDTMPT ( UMBRAL, A,    B, -1.D0, SRCRAD, AXIS, &
                      PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad source radius' )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, 0.D0, AXIS, &
                      PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, -1.D0, AXIS, &
                      PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDRADIUS)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad axis vector' )

      CALL CLEARD ( 3, AXIS )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, &
                      PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

      CALL VPACK ( 3*A,  0.D0, 0.D0, AXIS   )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Source and target are too close.' )

      CALL VPACK ( A,  0.D0, 0.D0, AXIS   )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, &
                      PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(OBJECTSTOOCLOSE)', OK )

      CALL VPACK ( 3*A,  0.D0, 0.D0, AXIS   )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad plane reference vector' )

      CALL CLEARD ( 3, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, &
                      PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Axis and plane reference vector are ' &
      //           'linearly dependent.'                 )

      CALL VEQU ( AXIS, PLNVEC )

      CALL ZZEDTMPT ( UMBRAL, A, B, C, SRCRAD, AXIS, &
                      PLNVEC, POINT  )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


