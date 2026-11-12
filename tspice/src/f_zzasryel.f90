!$Procedure  F_ZZASRYEL ( Test ray-ellipse angular separation routine )

      SUBROUTINE F_ZZASRYEL ( OK )
      use pepper_legacy
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        ZZASRYEL
!
! $ Version
!
!-    TSPICE Version 1.0.0, 09-NOV-2005 (NJB)
!
!-&

!
!     SPICELIB functions
!

!
!     Local parameters
!
      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      DOUBLE PRECISION      MEDTOL
      PARAMETER           ( MEDTOL = 1.D-10 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-7 )

      DOUBLE PRECISION      SLOPPY
      PARAMETER           ( SLOPPY = 1.D-4 )


      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 255 )

      INTEGER               MAXN
      PARAMETER           ( MAXN   = 100 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      D(3)
      DOUBLE PRECISION      EXTPT  ( 3 )
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      PTPERP ( 3 )
      DOUBLE PRECISION      RAISE
      DOUBLE PRECISION      V(3)
      DOUBLE PRECISION      XANG
      DOUBLE PRECISION      XPT    ( 3 )

!
!     Saved variables
!

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZASRYEL' )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Simple inspection case using sphere. Unit sphere is '// &
              'centered at the origin.  View point is along +x '    // &
              'axis.  Ray lies in the x-z plane and slants up at '  // &
              'a 60 degree angle, passing over sphere. Find min.'

      CALL TCASE  ( TITLE )


!
!     Assign vertex and direction of ray.
!
      CALL VPACK (  2.D0, 0.D0, 0.D0,       V )
      CALL VPACK ( -1.D0, 0.D0, SQRT(3.D0), D )

!
!     Semi-axis lengths of "ellipsoid" whose limb is to be found.
!
      A    =  1.D0
      B    =  1.D0
      C    =  1.D0

      CALL EDLIMB   ( A, B, C, V, LIMB )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

      CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

!
!     The expected angular separation from the limb is 30 degrees.
!
      XANG = PI() / 6.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

!
!     The expected limb point at which the minimum angular separation
!     occurs is at ( 1/2, 0, sqrt(3)/2 ).  Since we find the point
!     by searching for a minimum, we expect only about single
!     precision agreement.
!
      CALL VPACK ( 5.D-1, 0.D0, SQRT(3.D0)/2.D0, XPT )

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, LOOSE, OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Simple inspection case using sphere. Unit sphere is '// &
              'centered at the origin.  View point is along +x '    // &
              'axis.  Ray lies in the x-z plane and slants up at '  // &
              'a 60 degree angle, passing over sphere.  Find max.'

      CALL TCASE  ( TITLE )


      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

!
!     The expected angular separation from the limb is 90 degrees.
!
      XANG = PI() / 2.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

!
!     The expected limb point at which the minimum angular separation
!     occurs is at ( 1/2, 0, -sqrt(3)/2 ).  Since we find the point
!     by searching for a minimum, we expect only about single
!     precision agreement.
!
      CALL VPACK ( 5.D-1, 0.D0, -SQRT(3.D0)/2.D0, XPT )

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, LOOSE, OK )




!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Simple inspection case using sphere. Unit sphere is '// &
              'centered at the origin.  View point is along +x '    // &
              'axis.  Ray lies along the -x axis.  Find min.'

      CALL TCASE  ( TITLE )
!
!     Now take the ray along the x-axis, pointing in the -x direction.
!
      CALL VPACK (  2.D0, 0.D0, 0.D0, V )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, D )

      CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )

!
!     The expected angular separation from the limb is 30 degrees.
!     Since the ray penetrates the interior of the plane region
!     bounded by the limb, the sign of the angle is negative.
!
      XANG =  - PI() / 6.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

!
!     The expected limb point at which the minimum angular separation
!     occurs could be anywhere on the limb.  All limb points have
!     x-component -1/2 and an orthogonal component with length
!     sqrt(3)/2.
!
      CALL CHCKSD   ( 'EXTPT(1)', EXTPT(1), '~', 0.5D0, LOOSE, OK )

      CALL VPACK ( 0.D0, XPT(2), XPT(3), PTPERP )

      CALL CHCKSD   ( '||perp||', VNORM(PTPERP), '~', SQRT(3.D0)/2.D0, &
                      LOOSE,      OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Simple inspection case using sphere. Unit sphere is '// &
              'centered at the origin.  View point is along +x '    // &
              'axis.  Ray lies along the -x axis.  Find max.'

      CALL TCASE  ( TITLE )

!
!     In this case, all limb points correspond to both minima and
!     maxima.
!
      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )

!
!     The expected angular separation from the limb is 30 degrees.
!     Since the ray penetrates the interior of the plane region
!     bounded by the limb, the sign of the angle is negative.
!
      XANG =  - PI() / 6.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

!
!     The expected limb point at which the minimum angular separation
!     occurs could be anywhere on the limb.  All limb points have
!     x-component -1/2 and an orthogonal component with length
!     sqrt(3)/2.
!
      CALL CHCKSD   ( 'EXTPT(1)', EXTPT(1), '~', 0.5D0, LOOSE, OK )

      CALL VPACK ( 0.D0, XPT(2), XPT(3), PTPERP )

      CALL CHCKSD   ( '||perp||', VNORM(PTPERP), '~', SQRT(3.D0)/2.D0, &
                      LOOSE,      OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Simple inspection case using sphere. Unit sphere is '// &
              'centered at the origin.  View point is on the +x '   // &
              'axis.  Ray points slightly above parallel to the -x '// &
              'axis.  Find min.'


      CALL TCASE  ( TITLE )
!
!     Raise the vertex a bit and repeat.
!
      RAISE = 1.D-6

      CALL VPACK (  2.D0, 0.D0, 0.D0,  V )
      CALL VPACK ( -1.D0, 0.D0, RAISE, D )

      CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )

!
!     The expected magnitude of the angular separation from the limb is
!     still about 30 degrees, since the limb shifts slightly when
!     the view point is raised.
!
!     Since the ray penetrates the interior of the plane region
!     bounded by the limb, the sign of the angle is negative.
!
      XANG =  - (  ( PI() / 6.D0 )  -  ATAN( RAISE / 1.D0 )  )

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

!
!     The expected limb point is again at the top of the limb.  Because
!     this is a near-degenerate case, the determination of the extreme
!     point will be quite inaccurate.
!
      CALL VPACK ( 5.D-1, 0.D0, SQRT(3.D0)/2.D0, XPT )

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, SLOPPY, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Simple inspection case using sphere. Unit sphere is '// &
              'centered at the origin.  View point is on the +x '   // &
              'axis.  Ray points slightly above parallel to the -x '// &
              'axis.  Find max.'

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )

!
!     The expected magnitude of the angular separation from the limb is
!     still about 30 degrees, since the limb shifts slightly when
!     the view point is raised.
!
!     Since the ray penetrates the interior of the plane region
!     bounded by the limb, the sign of the angle is negative.
!
!     This time the extreme point we seek is at the bottom of the limb.
!     Recall that "max" refers to maximum *magnitude* in this context.
!
      XANG =  - (  ( PI() / 6.D0 )  +  ATAN( RAISE / 1.D0 )  )

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

!
!     Because this is a near-degenerate case, the determination of the
!     extreme point will be quite inaccurate.
!
      CALL VPACK ( 5.D-1, 0.D0, -SQRT(3.D0)/2.D0, XPT )

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, SLOPPY, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Simple inspection case derived from first unit '     // &
              'sphere case.  This time ellipsoid is very wide '     // &
              'in the +/- y directions.  Find min.'

      CALL TCASE  ( TITLE )


!
!     Assign vertex and direction of ray.
!
      CALL VPACK (  2.D0, 0.D0, 0.D0,       V )
      CALL VPACK ( -1.D0, 0.D0, SQRT(3.D0), D )

!
!     Semi-axis lengths of ellipsoid whose limb is to be found.
!     Note length of y semi-axis.
!
      A    =  1.D0
      B    =  1.D3
      C    =  1.D0

      CALL EDLIMB   ( A, B, C, V, LIMB )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

      CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

!
!     The expected angular separation from the limb is 30 degrees.
!
      XANG = PI() / 6.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, MEDTOL, OK )

!
!     The expected limb point at which the minimum angular separation
!     occurs is at ( 1/2, 0, sqrt(3)/2 ).  Since we find the point
!     by searching for a minimum, and because of the rather extreme
!     geometry, we expect less than single precision agreement.
!
      CALL VPACK ( 5.D-1, 0.D0, SQRT(3.D0)/2.D0, XPT )

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, SLOPPY, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Extreme case:  this time ellipsoid is very wide '     // &
              'in the +/- y directions and flat in z-direction. '    // &
              'Find max.'

      CALL TCASE  ( TITLE )


!
!     Assign vertex and direction of ray.
!
      CALL VPACK (  2.D0, 0.D0, 0.D0,       V )
      CALL VPACK ( -1.D0, 0.D0, SQRT(3.D0), D )

!
!     Semi-axis lengths of ellipsoid whose limb is to be found.
!     Note length of y and z semi-axes.
!
      A    =  1.D0
      B    =  1.D3
      C    =  1.D-5

      CALL VPACK ( -1.D0, 0.D0, 1.D-4, D )

      CALL EDLIMB   ( A, B, C, V, LIMB )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

!
!     The expected angular separation from the limb is close to the
!     angular extent of the largest semi-axis of the limb.  This
!     is a rough estimate.
!
      XANG = ATAN (  ( B * SQRT(3.D0) / 2.D0 )  /  1.5D0 )

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, MEDTOL, OK )

!
!     The expected limb point at which the maximum angular separation
!     occurs is roughly at either of ( 1/2, +/- B*sqrt(3)/2, 0 ).
!     Since the actual point will have negative z-component, this is a
!     very rough estimate.
!
      IF ( EXTPT(2) .GT. 0 ) THEN

         CALL VPACK ( 5.D-1,  B*SQRT(3.D0)/2.D0, 0.D0, XPT )
      ELSE
         CALL VPACK ( 5.D-1, -B*SQRT(3.D0)/2.D0, 0.D0, XPT )
      END IF

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~~/', XPT, 3, SLOPPY, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Interior case with ray pointing in -x direction. ' // &
              'This time ellipsoid is very wide '                 // &
              'in the +/- y directions.  Find min.'

      CALL TCASE  ( TITLE )

      CALL VPACK (  2.D0, 0.D0, 0.D0, V )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, D )

      A    =  1.D0
      B    =  1.D3
      C    =  1.D0

      CALL EDLIMB   ( A, B, C, V, LIMB )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

      CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

!
!     The expected angular separation from the limb is 30 degrees.
!     Since the ray penetrates the interior of the plane region
!     bounded by the limb, the sign of the angle is negative.
!
      XANG = - PI() / 6.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, MEDTOL, OK )

!
!     The expected limb point at which the minimum angular separation
!     occurs is at either of ( 1/2, 0, +/- sqrt(3)/2 ).  Since we find
!     the point by searching for a minimum, and because of the rather
!     extreme geometry, we expect less than single precision agreement.
!
      IF ( EXTPT(3) .GT. 0 ) THEN

         CALL VPACK ( 5.D-1, 0.D0,  SQRT(3.D0)/2.D0, XPT )
      ELSE
         CALL VPACK ( 5.D-1, 0.D0, -SQRT(3.D0)/2.D0, XPT )
      END IF


      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, SLOPPY, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Interior case with ray pointing in -x direction. ' // &
              'This time ellipsoid is very wide '                 // &
              'in the +/- y directions.  Find max.'

      CALL TCASE  ( TITLE )

      CALL VPACK (  2.D0, 0.D0, 0.D0, V )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, D )

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

!
!     The expected angular separation from the limb the angular
!     extent of the largest-semi-axis, which lies on the y-axis.
!     Since the ray penetrates the interior of the plane region
!     bounded by the limb, the sign of the angle is negative.
!
      XANG = - ATAN (  ( B * SQRT(3.D0)/2.D0 )  /  1.5D0  )

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, MEDTOL, OK )

!
!     The expected limb point at which the minimum angular separation
!     occurs is at either of ( 1/2, +/- B*sqrt(3)/2, 0 ).  Since we
!     find the point by searching for a minimum, and because of the
!     rather extreme geometry, we expect less than single precision
!     agreement.
!
      IF ( EXTPT(2) .GT. 0 ) THEN

         CALL VPACK ( 5.D-1,  B*SQRT(3.D0)/2.D0, 0.D0, XPT )
      ELSE
         CALL VPACK ( 5.D-1, -B*SQRT(3.D0)/2.D0, 0.D0, XPT )
      END IF


      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, SLOPPY, OK )




!
!     Error cases:
!

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Direction vector is zero vector.'

      CALL TCASE  ( TITLE )

!
!     Assign vertex and direction of ray.
!
      CALL VPACK (  2.D0, 0.D0, 0.D0, V )

      CALL CLEARD ( 3, D )


!
!     Semi-axis lengths of "ellipsoid" whose limb is to be found.
!
      A    =  1.D0
      B    =  1.D0
      C    =  1.D0

      CALL EDLIMB   ( A, B, C, V, LIMB )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .TRUE.,  'SPICE(ZEROVECTOR)', OK    )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Limb has zero-length semi-minor axis'

      CALL TCASE  ( TITLE )

      LIMB(7) = 0.D0
      LIMB(8) = 0.D0
      LIMB(9) = 0.D0

      CALL VPACK ( -1.D0, 0.D0, SQRT(3.D0), D )

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .TRUE.,  'SPICE(INVALIDAXISLENGTH)', OK    )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Vertex lies in plane of ellipse.'

      LIMB(4) = 0.D0
      LIMB(5) = 2.D0
      LIMB(6) = 0.D0
      LIMB(7) = 1.D0
      LIMB(8) = 0.D0
      LIMB(9) = 0.D0

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .TRUE.,  'SPICE(DEGENERATECASE)', OK )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Unrecognized extremum specifier'

      A    =  1.D0
      B    =  1.D0
      C    =  1.D0

      CALL EDLIMB   ( A, B, C, V, LIMB )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

      CALL ZZASRYEL ( 'MX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .TRUE.,  'SPICE(NOTSUPPORTED)', OK )
      CALL T_SUCCESS ( OK )
      RETURN
      END
