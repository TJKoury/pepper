C$Procedure  F_ZZASRYEL ( Test ray-ellipse angular separation routine )

      SUBROUTINE F_ZZASRYEL ( OK )
      IMPLICIT NONE

      LOGICAL               OK

C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        ZZASRYEL
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
C-    TSPICE Version 1.0.0, 09-NOV-2005 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VNORM

C
C     Local parameters
C
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

C
C     Local variables
C
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

C
C     Saved variables
C

C
C     Initial values
C


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZASRYEL' )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Simple inspection case using sphere. Unit sphere is '//
     .        'centered at the origin.  View point is along +x '    //
     .        'axis.  Ray lies in the x-z plane and slants up at '  //
     .        'a 60 degree angle, passing over sphere. Find min.'

      CALL TCASE  ( TITLE )


C
C     Assign vertex and direction of ray.
C
      CALL VPACK (  2.D0, 0.D0, 0.D0,       V )
      CALL VPACK ( -1.D0, 0.D0, SQRT(3.D0), D )

C
C     Semi-axis lengths of "ellipsoid" whose limb is to be found.
C
      A    =  1.D0
      B    =  1.D0
      C    =  1.D0

      CALL EDLIMB   ( A, B, C, V, LIMB )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

      CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

C
C     The expected angular separation from the limb is 30 degrees.
C
      XANG = PI() / 6.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

C
C     The expected limb point at which the minimum angular separation
C     occurs is at ( 1/2, 0, sqrt(3)/2 ).  Since we find the point
C     by searching for a minimum, we expect only about single
C     precision agreement.
C
      CALL VPACK ( 5.D-1, 0.D0, SQRT(3.D0)/2.D0, XPT )

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, LOOSE, OK )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Simple inspection case using sphere. Unit sphere is '//
     .        'centered at the origin.  View point is along +x '    //
     .        'axis.  Ray lies in the x-z plane and slants up at '  //
     .        'a 60 degree angle, passing over sphere.  Find max.'

      CALL TCASE  ( TITLE )


      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

C
C     The expected angular separation from the limb is 90 degrees.
C
      XANG = PI() / 2.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

C
C     The expected limb point at which the minimum angular separation
C     occurs is at ( 1/2, 0, -sqrt(3)/2 ).  Since we find the point
C     by searching for a minimum, we expect only about single
C     precision agreement.
C
      CALL VPACK ( 5.D-1, 0.D0, -SQRT(3.D0)/2.D0, XPT )

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, LOOSE, OK )




C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Simple inspection case using sphere. Unit sphere is '//
     .        'centered at the origin.  View point is along +x '    //
     .        'axis.  Ray lies along the -x axis.  Find min.'

      CALL TCASE  ( TITLE )
C
C     Now take the ray along the x-axis, pointing in the -x direction.
C     
      CALL VPACK (  2.D0, 0.D0, 0.D0, V )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, D )

      CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )

C
C     The expected angular separation from the limb is 30 degrees.
C     Since the ray penetrates the interior of the plane region 
C     bounded by the limb, the sign of the angle is negative.
C
      XANG =  - PI() / 6.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

C
C     The expected limb point at which the minimum angular separation
C     occurs could be anywhere on the limb.  All limb points have
C     x-component -1/2 and an orthogonal component with length 
C     sqrt(3)/2.
C
      CALL CHCKSD   ( 'EXTPT(1)', EXTPT(1), '~', 0.5D0, LOOSE, OK )

      CALL VPACK ( 0.D0, XPT(2), XPT(3), PTPERP )

      CALL CHCKSD   ( '||perp||', VNORM(PTPERP), '~', SQRT(3.D0)/2.D0, 
     .                LOOSE,      OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Simple inspection case using sphere. Unit sphere is '//
     .        'centered at the origin.  View point is along +x '    //
     .        'axis.  Ray lies along the -x axis.  Find max.'

      CALL TCASE  ( TITLE )

C
C     In this case, all limb points correspond to both minima and
C     maxima.
C
      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )

C
C     The expected angular separation from the limb is 30 degrees.
C     Since the ray penetrates the interior of the plane region 
C     bounded by the limb, the sign of the angle is negative.
C
      XANG =  - PI() / 6.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

C
C     The expected limb point at which the minimum angular separation
C     occurs could be anywhere on the limb.  All limb points have
C     x-component -1/2 and an orthogonal component with length 
C     sqrt(3)/2.
C
      CALL CHCKSD   ( 'EXTPT(1)', EXTPT(1), '~', 0.5D0, LOOSE, OK )

      CALL VPACK ( 0.D0, XPT(2), XPT(3), PTPERP )

      CALL CHCKSD   ( '||perp||', VNORM(PTPERP), '~', SQRT(3.D0)/2.D0, 
     .                LOOSE,      OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Simple inspection case using sphere. Unit sphere is '//
     .        'centered at the origin.  View point is on the +x '   //
     .        'axis.  Ray points slightly above parallel to the -x '//
     .        'axis.  Find min.'


      CALL TCASE  ( TITLE )
C
C     Raise the vertex a bit and repeat.
C     
      RAISE = 1.D-6

      CALL VPACK (  2.D0, 0.D0, 0.D0,  V )
      CALL VPACK ( -1.D0, 0.D0, RAISE, D )

      CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )

C
C     The expected magnitude of the angular separation from the limb is
C     still about 30 degrees, since the limb shifts slightly when 
C     the view point is raised.
C
C     Since the ray penetrates the interior of the plane region 
C     bounded by the limb, the sign of the angle is negative.
C
      XANG =  - (  ( PI() / 6.D0 )  -  ATAN( RAISE / 1.D0 )  )

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

C
C     The expected limb point is again at the top of the limb.  Because
C     this is a near-degenerate case, the determination of the extreme
C     point will be quite inaccurate.
C
      CALL VPACK ( 5.D-1, 0.D0, SQRT(3.D0)/2.D0, XPT )

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, SLOPPY, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Simple inspection case using sphere. Unit sphere is '//
     .        'centered at the origin.  View point is on the +x '   //
     .        'axis.  Ray points slightly above parallel to the -x '//
     .        'axis.  Find max.'

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )

C
C     The expected magnitude of the angular separation from the limb is
C     still about 30 degrees, since the limb shifts slightly when 
C     the view point is raised.
C
C     Since the ray penetrates the interior of the plane region 
C     bounded by the limb, the sign of the angle is negative.
C
C     This time the extreme point we seek is at the bottom of the limb.
C     Recall that "max" refers to maximum *magnitude* in this context.
C     
      XANG =  - (  ( PI() / 6.D0 )  +  ATAN( RAISE / 1.D0 )  )

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, TIGHT, OK )

C
C     Because this is a near-degenerate case, the determination of the
C     extreme point will be quite inaccurate.
C
      CALL VPACK ( 5.D-1, 0.D0, -SQRT(3.D0)/2.D0, XPT )

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, SLOPPY, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Simple inspection case derived from first unit '     //
     .        'sphere case.  This time ellipsoid is very wide '     //
     .        'in the +/- y directions.  Find min.'

      CALL TCASE  ( TITLE )


C
C     Assign vertex and direction of ray.
C
      CALL VPACK (  2.D0, 0.D0, 0.D0,       V )
      CALL VPACK ( -1.D0, 0.D0, SQRT(3.D0), D )

C
C     Semi-axis lengths of ellipsoid whose limb is to be found.
C     Note length of y semi-axis.
C
      A    =  1.D0
      B    =  1.D3
      C    =  1.D0

      CALL EDLIMB   ( A, B, C, V, LIMB )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

      CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

C
C     The expected angular separation from the limb is 30 degrees.
C
      XANG = PI() / 6.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, MEDTOL, OK )

C
C     The expected limb point at which the minimum angular separation
C     occurs is at ( 1/2, 0, sqrt(3)/2 ).  Since we find the point
C     by searching for a minimum, and because of the rather extreme
C     geometry, we expect less than single precision agreement.
C
      CALL VPACK ( 5.D-1, 0.D0, SQRT(3.D0)/2.D0, XPT )

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, SLOPPY, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Extreme case:  this time ellipsoid is very wide '     //
     .        'in the +/- y directions and flat in z-direction. '    //
     .        'Find max.'

      CALL TCASE  ( TITLE )


C
C     Assign vertex and direction of ray.
C
      CALL VPACK (  2.D0, 0.D0, 0.D0,       V )
      CALL VPACK ( -1.D0, 0.D0, SQRT(3.D0), D )

C
C     Semi-axis lengths of ellipsoid whose limb is to be found.
C     Note length of y and z semi-axes.
C
      A    =  1.D0
      B    =  1.D3
      C    =  1.D-5

      CALL VPACK ( -1.D0, 0.D0, 1.D-4, D )

      CALL EDLIMB   ( A, B, C, V, LIMB )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

C
C     The expected angular separation from the limb is close to the
C     angular extent of the largest semi-axis of the limb.  This
C     is a rough estimate.
C
      XANG = ATAN (  ( B * SQRT(3.D0) / 2.D0 )  /  1.5D0 )

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, MEDTOL, OK )

C
C     The expected limb point at which the maximum angular separation
C     occurs is roughly at either of ( 1/2, +/- B*sqrt(3)/2, 0 ).
C     Since the actual point will have negative z-component, this is a
C     very rough estimate.
C
      IF ( EXTPT(2) .GT. 0 ) THEN

         CALL VPACK ( 5.D-1,  B*SQRT(3.D0)/2.D0, 0.D0, XPT )
      ELSE
         CALL VPACK ( 5.D-1, -B*SQRT(3.D0)/2.D0, 0.D0, XPT )
      END IF

      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~~/', XPT, 3, SLOPPY, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Interior case with ray pointing in -x direction. ' //
     .        'This time ellipsoid is very wide '                 //
     .        'in the +/- y directions.  Find min.'

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

C
C     The expected angular separation from the limb is 30 degrees.
C     Since the ray penetrates the interior of the plane region 
C     bounded by the limb, the sign of the angle is negative.
C
      XANG = - PI() / 6.D0

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, MEDTOL, OK )

C
C     The expected limb point at which the minimum angular separation
C     occurs is at either of ( 1/2, 0, +/- sqrt(3)/2 ).  Since we find
C     the point by searching for a minimum, and because of the rather
C     extreme geometry, we expect less than single precision agreement.
C
      IF ( EXTPT(3) .GT. 0 ) THEN

         CALL VPACK ( 5.D-1, 0.D0,  SQRT(3.D0)/2.D0, XPT )
      ELSE
         CALL VPACK ( 5.D-1, 0.D0, -SQRT(3.D0)/2.D0, XPT )
      END IF


      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, SLOPPY, OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Interior case with ray pointing in -x direction. ' //
     .        'This time ellipsoid is very wide '                 //
     .        'in the +/- y directions.  Find max.'

      CALL TCASE  ( TITLE )

      CALL VPACK (  2.D0, 0.D0, 0.D0, V )
      CALL VPACK ( -1.D0, 0.D0, 0.D0, D )

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

C
C     The expected angular separation from the limb the angular
C     extent of the largest-semi-axis, which lies on the y-axis.
C     Since the ray penetrates the interior of the plane region 
C     bounded by the limb, the sign of the angle is negative.
C
      XANG = - ATAN (  ( B * SQRT(3.D0)/2.D0 )  /  1.5D0  )

      CALL CHCKSD   ( 'ANGLE,', ANGLE, '~', XANG, MEDTOL, OK )

C
C     The expected limb point at which the minimum angular separation
C     occurs is at either of ( 1/2, +/- B*sqrt(3)/2, 0 ).  Since we
C     find the point by searching for a minimum, and because of the
C     rather extreme geometry, we expect less than single precision
C     agreement.
C
      IF ( EXTPT(2) .GT. 0 ) THEN

         CALL VPACK ( 5.D-1,  B*SQRT(3.D0)/2.D0, 0.D0, XPT )
      ELSE
         CALL VPACK ( 5.D-1, -B*SQRT(3.D0)/2.D0, 0.D0, XPT )
      END IF


      CALL CHCKAD   ( 'EXTPT,', EXTPT, '~', XPT, 3, SLOPPY, OK )




C 
C     Error cases:
C

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Direction vector is zero vector.'

      CALL TCASE  ( TITLE )

C
C     Assign vertex and direction of ray.
C
      CALL VPACK (  2.D0, 0.D0, 0.D0, V )

      CALL CLEARD ( 3, D )


C
C     Semi-axis lengths of "ellipsoid" whose limb is to be found.
C
      A    =  1.D0
      B    =  1.D0
      C    =  1.D0

      CALL EDLIMB   ( A, B, C, V, LIMB )
      CALL CHCKXC   ( .FALSE.,    ' ', OK    )

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .TRUE.,  'SPICE(ZEROVECTOR)', OK    )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Limb has zero-length semi-minor axis'

      CALL TCASE  ( TITLE )

      LIMB(7) = 0.D0
      LIMB(8) = 0.D0
      LIMB(9) = 0.D0

      CALL VPACK ( -1.D0, 0.D0, SQRT(3.D0), D )

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .TRUE.,  'SPICE(INVALIDAXISLENGTH)', OK    )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Vertex lies in plane of ellipse.'

      LIMB(4) = 0.D0
      LIMB(5) = 2.D0
      LIMB(6) = 0.D0
      LIMB(7) = 1.D0
      LIMB(8) = 0.D0
      LIMB(9) = 0.D0

      CALL ZZASRYEL ( 'MAX', LIMB, V, D, ANGLE, EXTPT )
      CALL CHCKXC   ( .TRUE.,  'SPICE(DEGENERATECASE)', OK )
      
C
C --- Case: ------------------------------------------------------
C
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
