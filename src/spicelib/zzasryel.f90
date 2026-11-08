!$Procedure      ZZASRYEL ( Angular separation of ray and ellipse )

      SUBROUTINE ZZASRYEL ( EXTREM, ELLIPS, VERTEX, DIR, ANGLE, EXTPT )
      IMPLICIT NONE

! $ Abstract
!
!     Find the minimum or maximum angular separation between a
!     specified ray and ellipse.
!
! $ Required_Reading
!
!     ELLIPSES
!
! $ Keywords
!
!     ELLIPSE
!     ELLIPSOID
!     GEOMETRY
!     MATH
!
! $ Declarations

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      CHARACTER*(*)         EXTREM
      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      EXTPT  ( 3 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     UBEL       P   Upper bound of SPICELIB ellipse.
!     UBPL       P   Upper bound of SPICELIB plane.
!     EXTREM     I   Type of extremum to find.
!     ELLIPS     I   SPICE ellipse.
!     VERTEX,
!     DIR        I   Vertex and direction vector of ray.
!     ANGLE      O   Angular separation of ray and ellipse (radians).
!     EXTPT      O   Point on ellipse where extremum is achieved.
!
! $ Detailed_Input
!
!     EXTREM         is a string indicating the type of extremum to
!                    find.  Values are 'MIN' and 'MAX'.  Blanks and
!                    case are not significant.  Only the first three
!                    non-blank characters of EXTREM are significant.
!
!
!     ELLIPS         is a SPICELIB ellipse data structure. ELLIPS must
!                    have non-zero semi-axis lengths.
!
!
!     VERTEX,
!     DIR            are the vertex and direction vector of a ray in
!                    three-dimensional space.
!
! $ Detailed_Output
!
!     ANGLE          is the specified extremum of angular separation of
!                    the input ray and the ellipse.  This is the
!                    minimum or maximum angular separation of the ray
!                    and any line segment extending from the ray's
!                    vertex to a point on the surface of the ellipse.
!                    Units are radians.
!
!                    If the input ray actually intersects the plane
!                    region bounded by the ellipse, ANGLE is set to a
!                    non-positive value whose magnitude is the minimum
!                    or maximum angular separation of the input ray and
!                    the ellipse.
!
!
!     EXTPT          is the point on the ellipse where the specified
!                    extreme value of the angular separation is
!                    achieved.  If there are multiple points where the
!                    extremum is achieved, any such point may be
!                    selected.
!
! $ Parameters
!
!     UBEL           is the upper bound of a SPICELIB ellipse data
!                    structure.
!
!     UBPL           is the upper bound of a SPICELIB plane data
!                    structure.
!
! $ Exceptions
!
!     1)  If the length of any semi-axis of the ellipse is
!         non-positive, the error SPICE(INVALIDAXISLENGTH) is
!         signaled.  ANGLE and EXTPT are not modified.
!
!     2)  If VERTEX lies in the plane of the ellipse, the error
!         SPICE(DEGENERATECASE) is signaled.  ANGLE and EXTPT are not
!         modified.
!
!     3)  If DIR is the zero vector, the error SPICE(ZEROVECTOR) is
!         signaled.  ANGLE and EXTPT are not modified.
!
!     4)  If EXTREM contains an unrecognized value, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Definition
!     ==========
!
!     The minimum or maximum angular separation of a ray and ellipse is
!     the minimum or maximum, taken over all points on the ellipse, of
!     the angular separation of the ray and the vector from the ray's
!     vertex to a point on the ellipse.
!
!     Uniqueness or multiplicity of minima
!     ====================================
!
!     Let's presume that the ray does not intersect the plane region
!     bounded by the ellipse.  If the ray's vertex does not lie in the
!     plane of the ellipse, the uniqueness of the minimizing point can
!     be verified by observing that the right circular cone of minimum
!     angular extent whose axis is the ray, and that is tangent to the
!     ellipse, will be tangent at the minimizing point and no other.
!     If the ray's vertex does lie in the plane of the ellipse, there
!     can be multiple tangency points.
!
!     If the ray intersects the plane region bounded by the ellipse,
!     there may be multiple absolute minima of the angular separation.
!     Consider the case where the ellipse is a circular cross section
!     of a right circular cone, and the ray is the cone's axis:  there
!     is an infinite set of solutions, since the minimum angular
!     separation is achieved at every point on the circle.
!
!
!     Uniqueness or multiplicity of maxima
!     ====================================
!
!     Let's presume that the ray does not intersect the plane region
!     bounded by the ellipse.  If the ray's vertex does not lie in the
!     plane of the ellipse, one observes that the right circular cone
!     of maximum angular extent whose axis is the ray, and that is
!     tangent to the ellipse, can still easily be tangent to the
!     ellipse at multiple points (consider an ellipse whose shape is
!     "almost" a line segment). The ray's vertex need not lie in the
!     plane of the ellipse for multiple tangency points to exist.
!
!     If the ray intersects the plane region bounded by the ellipse,
!     there may be multiple absolute maxima of the angular separation.
!
!
!     Extremum of angular separation versus distance
!     ==============================================
!
!     Note the point on the ellipse having minimum angular separation
!     from a ray is NOT necessarily the closest point on the ellipse to
!     the ray. You can verify this by considering the case of an
!     extremely eccentric ellipse and a ray that passes above it.   The
!     diagram below illustrates this situation.  The series of three
!     asterisks rising from left to right represents the ray; the other
!     asterisks represent the ellipse. The point `c' is the closest
!     point on the ellipse to the ray; the point `m' has the minimum
!     angular separation from the ray.
!
!     The analogous distinction applies to maximum angular separation
!     and maximum distance:  compare the points labeled 'M' and 'F'
!     in the diagram below.
!
!
!
!                                                            *
!                                                          (ray)
!                                  *
!    ray's vertex                (ray)
!        *
!
!
!  closest ellipse  ---->   c * * * * * * * * m   <-- point of minimum
!  point to the ray       *                     *        angular
!                           M * * * * * * * * F          separation
!
!                           ^                 ^
!     point of maximum angular                farthest ellipse
!     separation                              point from ray
!
!
!
!
!     Applications
!     ============
!
!     This subroutine can be used to:
!
!        -  measure the angular separation of
!           an instrument boresight from a body's limb
!
!        -  test for visibility of an ellipsoidal body within an
!           circular field of view (or, with more work, an elliptical
!           field of view)
!
!        -  test for occultation of one ellipsoidal body by another
!
!        -  support tests for intersection of an ellipsoidal body with
!           an umbral or penumbral shadow cast by another ellipsoidal
!           body blocking an ellipsoidal light source.
!
! $ Examples
!
!     1)  An example that can be readily checked by hand computation.
!
!            Let
!
!               A = 1
!               B = 1
!               C = 1
!
!               V = ( 2,   0,  0       )
!               D = ( -1,  0,  SQRT(3) )
!
!            The limb of the sphere as seen from the ray's vertex will
!            be the circle centered at ( .5, 0, 0 ), parallel to the
!            y-z plane,  with radius SQRT(3)/2.  The ray lies in the
!            x-z plane and passes over the ellipse, so the limb point
!            of minimum angular separation should be the highest point
!            on the limb.  This would be the point
!
!               ( .5, 0, SQRT(3)/2 ).
!
!            The tangent segment extending from the ray's vertex to the
!            point of mimimum angular separation makes an angle of
!            30 degrees with the x-axis, and the ray makes angle of 60
!            degrees with the x-axis, so the angular separation of the
!            ray and the limb should be 30 degrees.
!
!            For a ray have the same vertex but pointing in the -x
!            direction, the minimum point can be anywhere on the limb,
!            but the angle should be -30 degrees.
!
!            If the vertex is raised slightly (that is, the z-component
!            is increased slightly) and the ray points in the -x
!            direction, the mimimum point should be at the top of the
!            limb, and the angle should be a negative value with
!            magnitude slightly less than 30 degrees.
!
!            The program below should verify these results.
!
!
!               PROGRAM MINANG
!               IMPLICIT NONE
!
!               INTEGER               UBEL
!               PARAMETER           ( UBEL   = 9 )
!
!               DOUBLE PRECISION      DPR
!
!               DOUBLE PRECISION      V(3)
!               DOUBLE PRECISION      D(3)
!               DOUBLE PRECISION      A
!               DOUBLE PRECISION      B
!               DOUBLE PRECISION      C
!               DOUBLE PRECISION      ANGLE
!               DOUBLE PRECISION      LIMB  ( UBEL )
!               DOUBLE PRECISION      EXTPT ( 3 )
!
!               V(1) =  2.D0
!               V(2) =  0.D0
!               V(3) =  0.D0
!
!               D(1) = -1.D0
!               D(2) =  0.D0
!               D(3) =  SQRT( 3.D0 )
!
!               A    =  1.D0
!               B    =  1.D0
!               C    =  1.D0
!
!               CALL EDLIMB   ( A, B, C, V, LIMB )
!
!               CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
!
!               PRINT *, ' '
!               PRINT *, 'Angle is'
!               PRINT *, DPR() * ANGLE
!               PRINT *, 'Point of mimimum separation is'
!               PRINT *, EXTPT
!
!         C
!         C     Now take the ray along the x-axis,
!         C     pointing in the -x direction.
!         C
!               D(1) = -1.D0
!               D(2) =  0.D0
!               D(3) =  0.D0
!
!               CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
!
!               PRINT *, ' '
!               PRINT *, 'Angle is'
!               PRINT *, DPR() * ANGLE
!               PRINT *, 'Point of mimimum separation is'
!               PRINT *, EXTPT
!
!         C
!         C     Raise the vertex a bit and repeat.
!         C
!               V(1) =  2.D0
!               V(2) =  0.D0
!               V(3) =  1.D-6
!
!               CALL ZZASRYEL ( 'MIN', LIMB, V, D, ANGLE, EXTPT )
!
!               PRINT *, ' '
!               PRINT *, 'Angle is'
!               PRINT *, DPR() * ANGLE
!               PRINT *, 'Point of mimimum separation is'
!               PRINT *, EXTPT
!
!               END
!
!
! $ Restrictions
!
!     1) Under some unusual geometric conditions, the search used
!        in this algorithm may find a relative extremum which is not
!        an absolute extremum.  This can occur if there are two local
!        extrema of separation (both minima or both maxima)
!        located less than (2*pi/20) apart in the parameter domain for
!        the ellipse's limb, where the limb is parameterized as
!
!           CENTER + cos(theta)*SMAJOR  + sin(theta)*SMINOR,
!
!           0 <= theta <= 2*pi
!
!        and
!
!           CENTER is the center of the limb
!           SMAJOR is a semi-major axis vector of the limb
!           SMINOR is a semi-minor axis vector of the limb
!
!        The search can also fail to find an absolute extremum in cases
!        where there are two extrema (both minima or both maxima) that
!        are distant but very close to equal in terms of angular
!        separation from the input ray.
!
!
!     2) The point at which the minimum or maximum angular separation
!        occurs is determined to single precision.  Specifically, the
!        angular parameter THETA defining the location relative to the
!        semi-axes is determined at the single precision level.
!
!
! $ Literature_References
!
!     [1]  "Numerical Recipes -- The Art of Scientific Computing" by
!           William H. Press, Brian P. Flannery, Saul A. Teukolsky,
!           William T. Vetterling.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.2, 01-OCT-2021 (NJB)
!
!        Corrected typo in comments.
!
!-    SPICELIB Version 1.1.1, 28-FEB-2008 (BVS)
!
!        Corrected the contents of the Required_Reading section.
!
!-    SPICELIB Version 1.1.0, 14-NOV-2006 (NJB)
!
!        The parameter NPT has been replaced by two different
!        parameters:  one for the exterior minimum case and one for the
!        complementary cases. This change was made to improve accuracy.
!
!-    SPICELIB Version 1.0.0, 07-SEP-2005 (NJB)
!
!-&

! $ Index_Entries
!
!     angular separation of ray and ellipse
!
!-&



!
!     SPICELIB functions
!
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               FAILED
      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!

!
!     Tolerance used for loop convergence.  This tolerance applies
!     to the angular parameter used to specify points on the ellipse.
!
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL   = 1.D-9 )

!
!     Number of steps used to search the ellipse for region containing
!     the point of extreme angular separation.  We use two different
!     values:  one for the outer minimum case, which is mathematically
!     well behaved, and one for the other cases.
!
      INTEGER               SMLNPT
      PARAMETER           ( SMLNPT =  320 )

      INTEGER               BIGNPT
      PARAMETER           ( BIGNPT =  400 )

!
!     Maximum number of loop iterations allowed for extremum search.
!
      INTEGER               MAXITR
      PARAMETER           ( MAXITR = 100 )

!
!     Code returned in INRYPL indicating ray lies in plane.
!
      INTEGER               INF
      PARAMETER           ( INF    = -1 )

!
!     String length for extremum specifier.
!
      INTEGER               EXTLEN
      PARAMETER           ( EXTLEN = 3 )

!
!     Local variables
!
      CHARACTER*(EXTLEN)    EXTTYP

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ACOMP
      DOUBLE PRECISION      ASIGN
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BCOMP
      DOUBLE PRECISION      BTWEEN
      DOUBLE PRECISION      BTWPRX
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIFF   ( 3 )
      DOUBLE PRECISION      EPLANE ( UBPL )
      DOUBLE PRECISION      EXTPRX
      DOUBLE PRECISION      GR
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      LOWER
      DOUBLE PRECISION      LPT    ( 3 )
      DOUBLE PRECISION      NEWPT
      DOUBLE PRECISION      P2
      DOUBLE PRECISION      PROXY
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      UDIFF  ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      UPPER
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      VPRJ   ( 3 )
      DOUBLE PRECISION      XOFF   ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )

      INTEGER               I
      INTEGER               EXTIDX
      INTEGER               NITR
      INTEGER               NPT
      INTEGER               NXPTS

      LOGICAL               DOMIN

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ZZASRYEL' )
      END IF

!
!     Decide whether we're looking for a minimum or maximum.
!
      CALL CMPRSS ( ' ', 0, EXTREM, EXTTYP )
      CALL LJUST  (         EXTTYP, EXTTYP )

      IF ( EXTTYP .EQ. 'MIN' ) THEN

         DOMIN = .TRUE.

      ELSE IF ( EXTTYP .EQ. 'MAX' ) THEN

         DOMIN = .FALSE.

      ELSE

         CALL SETMSG ( 'Extremum specifier # was not recognized.' )
         CALL ERRCH  ( '#',  EXTREM                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'ZZASRYEL'                                 )
         RETURN

      END IF

!
!     Get the center and semi-axes of the ellipse.
!
      CALL EL2CGV ( ELLIPS, CENTER, SMAJOR, SMINOR )

!
!     The ellipse semi-axes must have positive length.
!
      A   =   VNORM(SMAJOR)
      B   =   VNORM(SMINOR)

      IF (  VZERO(SMAJOR) .OR. VZERO(SMINOR)  )   THEN

         CALL SETMSG ( 'Semi-axis lengths:  A = #, B = #.' )
         CALL ERRDP  ( '#', A                              )
         CALL ERRDP  ( '#', B                              )
         CALL SIGERR ( 'SPICE(INVALIDAXISLENGTH)'          )
         CALL CHKOUT ( 'ZZASRYEL'                          )
         RETURN

      END IF

!
!     Find the plane of the ellipse.
!
      CALL PSV2PL ( CENTER, SMAJOR, SMINOR, EPLANE )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZASRYEL' )
         RETURN
      END IF

!
!     The ray's direction vector must be non-zero.
!
      IF ( VZERO(DIR) ) THEN

         CALL SETMSG ( 'Ray''s direction vector must be non-zero.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                         )
         CALL CHKOUT ( 'ZZASRYEL'                                  )
         RETURN

      END IF

!
!     The ray's vertex must not lie in the plane of the ellipse.
!     The orthogonal projection of the point onto the plane should
!     yield a distinct vector.
!
      CALL VPRJP ( VERTEX, EPLANE, VPRJ )

      IF ( VDIST(VERTEX, VPRJ) .EQ. 0.D0  )  THEN

         CALL SETMSG ( 'Viewing point is in the plane of the ellipse.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                         )
         CALL CHKOUT ( 'ZZASRYEL'                                    )
         RETURN

      END IF

!
!     See whether the ray intersects the plane region bounded by the
!     ellipse.  If it does, set the limb angle sign to -1.  Otherwise
!     the sign is +1.
!
!     First, find the intersection of the ray and plane.
!
      CALL INRYPL ( VERTEX, DIR, EPLANE, NXPTS, XPT )

      IF ( NXPTS .EQ. INF )  THEN
!
!        We don't expect to hit this case since we've already tested
!        for the vertex lying in the ellipse plane.  However,
!        variations in round-off error make this case possible though
!        unlikely.
!
         CALL SETMSG ( 'Ray lies in the plane of the ellipse.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                 )
         CALL CHKOUT ( 'ZZASRYEL'                              )
         RETURN

      END IF

!
!     Give NPT an initial value.
!
      NPT = BIGNPT

      IF ( NXPTS .EQ. 0 ) THEN
!
!        The ray does not intersect the plane.
!
         ASIGN = 1.D0

      ELSE
!
!        The ray intersects the plane.  We must determine if the
!        ray intersects the region bounded by the ellipse.
!
!        Find the coordinates of the intersection point in a frame
!        aligned with the axes of the ellipse and centered at
!        the ellipse's center.
!
         CALL VSUB ( XPT, CENTER, XOFF )

         ACOMP = VDOT ( XOFF, SMAJOR ) / A
         BCOMP = VDOT ( XOFF, SMINOR ) / B

!
!        Now find the "level curve parameter" LEVEL for the offset of
!        the intersection point from the ellipse's center.
!
         LEVEL =  ACOMP**2 / A**2   +   BCOMP**2 / B**2

         IF ( LEVEL .LE. 1.D0 ) THEN
!
!           The ray-plane intersection is on the ellipse or inside the
!           plane region bounded by the ellipse.
!
            ASIGN = -1.D0

         ELSE

            ASIGN =  1.D0

            IF ( DOMIN ) THEN
!
!              We have the exterior minimum case:  the ray doesn't
!              penetrate the plane region bounded by the ellipse,
!              and we're looking for an absolute minimum of angular
!              separation.  We can use a fairly small number of test
!              points on the limb and still find the location of
!              minimum angular separation.
!
               NPT = SMLNPT

            END IF

         END IF

      END IF
!
!     ASIGN has been set.
!

!
!     The limb is the set of points
!
!        CENTER   +   cos(theta) SMAJOR   +   sin(theta) SMINOR
!
!     where theta is in the interval (-pi, pi].
!
!     We want to find the value of `theta' for which the angular
!     separation of ray and ellipse is minimized (or maximized).  To
!     improve efficiency, instead of working with angular separation,
!     we'll find the extremum of a proxy function:  the distance
!     between the unit ray direction vector and the unit vector in the
!     direction from the ray's vertex to a selected point on the
!     ellipse.  This function doesn't require an arcsine evaluation,
!     and its extrema occur at the same locations as the extrema of the
!     angular separation.
!
!     We'll compute the proxy value for the angular separation of the
!     ray and limb at NPT different points on the limb, where the
!     points are generated by taking equally spaced values of theta.
!     We'll find the extremum of the proxy function on this set of
!     points, and then search for the absolute extremum.
!
!     To make our computations more efficient, we'll subtract off
!     the ellipse's center from the vertex position to obtain a
!     translated ellipse centered at the origin.
!
      CALL VSUB ( VERTEX, CENTER, V2 )

      IF ( DOMIN ) THEN
         EXTPRX = 2.D0
      ELSE
         EXTPRX = 0.D0
      END IF

      EXTIDX = 0

      P2     = TWOPI()
      DELTA  = P2 / NPT

      CALL VHAT ( DIR, UDIR )


      DO I = 0, NPT-1

         THETA  =  I * DELTA

         CALL VLCOM3 (  -1.D0,        V2, &
                        COS(THETA),   SMAJOR, &
                        SIN(THETA),   SMINOR,   DIFF  )

         CALL VHAT ( DIFF, UDIFF )

         PROXY = VDIST ( UDIFF, UDIR )

         IF ( DOMIN ) THEN

            IF ( PROXY .LT. EXTPRX ) THEN

               EXTIDX  =  I
               EXTPRX  =  PROXY

            END IF

         ELSE

            IF ( PROXY .GT. EXTPRX ) THEN

               EXTIDX  =  I
               EXTPRX  =  PROXY

            END IF

         END IF

      END DO

!
!     The extreme value of the proxy function is EXTPRX, and was
!     obtained at the test point indexed by EXTIDX.  We find the values
!     of the proxy function at the neighboring points and perform a
!     `golden section' search.
!
!     In the following section of code,
!
!        LOWER          is the lower bound of the interval in which
!                       the extremum is bracketed.
!
!        UPPER          is the upper bound of the interval in which
!                       the extremum is bracketed.
!
!        BTWEEN         is a point between LOWER and UPPER.  The proxy
!                       function value corresponding to the angle
!                       BTWEEN is less than the proxy function value
!                       corresponding to LOWER and UPPER.
!
!        NEWPT          is a point between LOWER and UPPER such that
!                                                                  ___
!                          BTWEEN - LOWER                  3  -  \/ 5
!                          --------------    =   GR   =    ------------
!                          UPPER  - LOWER                        2
!
!
      GR     =  (  3.D0 -  SQRT(5.D0)  )  /  2.D0

      LOWER  =  ( P2 / NPT )  *  ( EXTIDX - 1 )
      UPPER  =  ( P2 / NPT )  *  ( EXTIDX + 1 )

!
!     We're going to move LOWER and UPPER closer together at each
!     iteration of the following loop, thus trapping the extremum. The
!     invariant condition that we will maintain is that the proxy value
!     corresponding to the angle BTWEEN is less (or more) than the proxy
!     value for the limb points corresponding to LOWER and UPPER.
!
!     The loop terminates when the offset by which we adjust LOWER or
!     UPPER is smaller than our tolerance value. This offset is no
!     larger than the difference between LOWER and BTWEEN.
!
      BTWEEN  =  ( P2 / NPT )  *  EXTIDX

!
!     We'll give the names LOWPRX and UPRPRX to the proxy function
!     values at the limb points corresponding to LOWER and UPPER,
!     respectively. We don't actually have to evaluate these values,
!     however. They are useful for understanding the minimization
!     algorithm we'll use, but are not actually used in the code.
!
!     We already know that the proxy function value corresponding to
!     BTWEEN is EXTPRX; this was computed above.
!
      BTWPRX  =  EXTPRX

!
!     Before starting our loop, we're going to shift all of our angles
!     by 2*pi, so that they're bounded away from zero.
!
      LOWER   =  LOWER  + P2
      UPPER   =  UPPER  + P2
      BTWEEN  =  BTWEEN + P2

      NITR    = 0
      PROXY   = 3.D0

      DO WHILE (        (  NITR                .LE. MAXITR ) &
                 .AND.  (  TOUCHD(UPPER-LOWER) .GT. TOL    )  )
!
!        At this point, the following order relations hold:
!
!           LOWER  <    BTWEEN    <   UPPER
!                  -              -
!
!           BTWPRX <  MIN ( LOWPRX, UPRPRX )
!                  -
!
!        Compute NEWPT.  This point is always located at the fraction
!        GR of the way into the larger of the intervals
!        [ LOWER, BTWEEN ] and [ BTWEEN, UPPER ].
!
!
         IF (  ( BTWEEN - LOWER )  .GT. ( UPPER - BTWEEN )  ) THEN

            NEWPT = LOWER   +  GR * ( BTWEEN - LOWER  )
         ELSE
            NEWPT = BTWEEN  +  GR * ( UPPER  - BTWEEN )
         END IF

!
!        We are going to shorten our interval by changing LOWER to
!        NEWPT or UPPER to BTWEEN, and if necessary, BTWEEN to NEWPT,
!        while maintaining the order relations of UPPER, LOWER, and
!        BTWEEN, and also the order relations of UPRPRX, LOWPRX, and
!        BTWPRX.  To do this, we need the proxy function value at
!        NEWPT.
!
         CALL VLCOM3 (  -1.D0,        V2, &
                        COS(NEWPT),   SMAJOR, &
                        SIN(NEWPT),   SMINOR,   DIFF  )

         CALL VHAT ( DIFF, UDIFF )

         PROXY = VDIST ( UDIFF, UDIR )

!
!        Swap NEWPT and BTWEEN if necessary, to ensure that
!
!           NEWPT  <  BTWEEN.
!                  _
!
         IF ( NEWPT .GT. BTWEEN ) THEN

            CALL SWAPD ( BTWEEN, NEWPT )
            CALL SWAPD ( BTWPRX, PROXY )

         END IF


         IF ( DOMIN ) THEN

            IF ( PROXY .GT. BTWPRX ) THEN

               LOWER  = NEWPT
            ELSE
               UPPER  = BTWEEN
               BTWEEN = NEWPT
               BTWPRX = PROXY
            END IF

         ELSE

            IF ( PROXY .LT. BTWPRX ) THEN

               LOWER  = NEWPT
            ELSE
               UPPER  = BTWEEN
               BTWEEN = NEWPT
               BTWPRX = PROXY
            END IF

         END IF

         NITR = NITR + 1

      END DO

!
!     At this point, LPT is a good estimate of the limb point at which
!     the extremum of the angular separation from the ray occurs.
!
      CALL VADD ( DIFF, V2, LPT )

!
!     Add the center back to LPT to find EXTPT on the original ellipse.
!
      CALL VADD ( CENTER, LPT, EXTPT )

!
!     Set the angular separation at EXTPT.
!
      ANGLE  =  VSEP( DIFF,  UDIR ) * ASIGN


      CALL CHKOUT ( 'ZZASRYEL' )
      RETURN
      END
