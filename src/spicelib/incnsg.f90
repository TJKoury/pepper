!$Procedure INCNSG ( Intersection of cone and line segment )

      SUBROUTINE INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1, &
                          ENDPT2, NXPTS, XPT1,  XPT2   )

! $ Abstract
!
!     Compute the points of intersection of a specified nappe of a cone
!     and a line segment.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONE
!     GEOMETRY
!     INTERSECTION
!     LINE
!     MATH
!     SEGMENT
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      ENDPT1 ( 3 )
      DOUBLE PRECISION      ENDPT2 ( 3 )
      INTEGER               NXPTS
      DOUBLE PRECISION      XPT1   ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     APEX       I   Apex of cone.
!     AXIS       I   Axis of cone.
!     ANGLE      I   Angle of cone.
!     ENDPT1,
!     ENDPT2     I   Endpoints of line segment.
!     NXPTS      O   Number of intersection points.
!     XPT1       O   First intersection point, if it exists.
!     XPT2       O   Second intersection point, if it exists.
!
! $ Detailed_Input
!
!     APEX     is the apex (tip) of the cone. In this routine's
!              documentation, we'll consider the cone to be a
!              semi-infinite pyramid with circular cross-section. In
!              some contexts, this object is called one "nappe" of
!              the complete cone.
!
!     AXIS     is an axis vector of the cone.
!
!     ANGLE    is the angular separation from AXIS of the rays
!              comprising the cone. Let the notation
!
!                 < A, B >
!
!              denote the dot product of vectors A and B, and let
!
!                 ||A||
!
!              denote the norm of vector A. Then the cone is the set
!              of points
!
!                           X-APEX       AXIS
!                 { X:  < ----------,  -------- >  =  cos(ANGLE) }
!                         ||X-APEX||   ||AXIS||
!
!
!     ENDPT1,
!     ENDPT2   are endpoints of a line segment. These points
!              must be distinct.
!
! $ Detailed_Output
!
!     NXPTS    is the number of points of intersection of the input
!              line segment and cone.
!
!     XPT1     is the point of intersection of the segment and cone
!              that is closest to ENDPT1, if an intersection exists.
!              If there are no intersections, XPT1 is undefined.
!
!     XPT2     is the point of intersection of the segment and cone
!              that is farthest from ENDPT1, if two points of
!              intersection exist. If there are not two
!              intersections, XPT2 is undefined.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If AXIS is the zero vector, the error SPICE(ZEROVECTOR)
!         is signaled.
!
!     2)  If ANGLE is less than zero, the error SPICE(INVALIDANGLE)
!         is signaled.
!
!     3)  If ENDPT1 and ENDPT2 coincide, the error
!         SPICE(ENDPOINTSMATCH) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is used by the SPICELIB DSK subsystem. In
!     particular, it is used to determine whether a ray contacts a
!     latitude boundary of a volume element in either planetocentric
!     latitudinal or planetodetic coordinates.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as input
!     (if any), the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Compute the intersection of a line segment and cone in
!        a simple case for which the results can easily be checked.
!
!        Let the apex of the cone be at the origin. Let the axis
!        of the cone lie on the +X axis. Let the angle of the cone
!        be 45 degrees. Let the line segment have endpoints
!
!           ENDPT1 = ( 1,   -2, sqrt(3)/2 )
!           ENDPT2 = ( 1,    2, sqrt(3)/2 )
!
!        We expect there to be two points of intersection:
!
!           XPT1   = ( 1, -1/2, sqrt(3)/2 )
!           XPT2   = ( 1,  1/2, sqrt(3)/2 )
!
!
!        Example code begins here.
!
!
!              PROGRAM INCNSG_EX1
!              IMPLICIT NONE
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      RPD
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         FMT1
!              PARAMETER           ( FMT1 = '(A,3F13.8)' )
!
!              CHARACTER*(*)         FMT2
!              PARAMETER           ( FMT2 = '(A,I2)' )
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      ANGLE
!              DOUBLE PRECISION      APEX   ( 3 )
!              DOUBLE PRECISION      AXIS   ( 3 )
!              DOUBLE PRECISION      ENDPT1 ( 3 )
!              DOUBLE PRECISION      ENDPT2 ( 3 )
!              DOUBLE PRECISION      SQ3
!              DOUBLE PRECISION      XPT1   ( 3 )
!              DOUBLE PRECISION      XPT2   ( 3 )
!
!              INTEGER               NXPTS
!
!        C
!        C     Set up the cone's geometric attributes.
!        C
!              CALL VPACK ( 0.D0, 0.D0, 0.D0, APEX )
!              CALL VPACK ( 1.D0, 0.D0, 0.D0, AXIS )
!
!              ANGLE = 45.D0 * RPD()
!        C
!        C     Initialize the line segment's endpoints.
!        C
!              SQ3 = SQRT( 3.D0  )
!
!              CALL VPACK ( 1.D0, -2.D0, SQ3/2, ENDPT1 )
!              CALL VPACK ( 1.D0,  2.D0, SQ3/2, ENDPT2 )
!        C
!        C     Find the points of intersection.
!        C
!              CALL INCNSG ( APEX,   AXIS,  ANGLE, ENDPT1,
!             .              ENDPT2, NXPTS, XPT1,  XPT2   )
!
!              WRITE (*,*) ' '
!              WRITE (*,FMT1) 'Apex:        ', APEX
!              WRITE (*,FMT1) 'Axis:        ', AXIS
!              WRITE (*,FMT1) 'Angle (deg): ', ANGLE/RPD()
!              WRITE (*,FMT1) 'Endpoint 1:  ', ENDPT1
!              WRITE (*,FMT1) 'Endpoint 2:  ', ENDPT2
!              WRITE (*,*) ' '
!              WRITE (*,FMT2) 'Number of intersection points: ',
!             .            NXPTS
!              WRITE (*,*) ' '
!              WRITE (*,FMT1) 'Point 1:    ', XPT1
!              WRITE (*,FMT1) 'Point 2:    ', XPT2
!              WRITE (*,*) ' '
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Apex:           0.00000000   0.00000000   0.00000000
!        Axis:           1.00000000   0.00000000   0.00000000
!        Angle (deg):   45.00000000
!        Endpoint 1:     1.00000000  -2.00000000   0.86602540
!        Endpoint 2:     1.00000000   2.00000000   0.86602540
!
!        Number of intersection points:  2
!
!        Point 1:       1.00000000  -0.50000000   0.86602540
!        Point 2:       1.00000000   0.50000000   0.86602540
!
!
! $ Restrictions
!
!     1)  This routine is designed to avoid arithmetic overflow in
!         normal cases, such as those in which the line segment is
!         nearly parallel to the cone. However, it is possible to cause
!         arithmetic overflow by using input vectors with extremely
!         large magnitudes.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 06-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 26-OCT-2016 (NJB)
!
!-&


! $ Index_Entries
!
!     intersection of line segment and cone
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      DOUBLE PRECISION      COSTOL
      PARAMETER           ( COSTOL = 1.D-10 )

      DOUBLE PRECISION      HSPTOL
      PARAMETER           ( HSPTOL = 1.D-14 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

!
!     Local variables
!
      DOUBLE PRECISION      A
      DOUBLE PRECISION      AXMAG
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CA2
      DOUBLE PRECISION      COLAT
      DOUBLE PRECISION      COSANG
      DOUBLE PRECISION      COSERR
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      DMAG
      DOUBLE PRECISION      DP1
      DOUBLE PRECISION      DP2
      DOUBLE PRECISION      LOCANG
      DOUBLE PRECISION      MAXLAT
      DOUBLE PRECISION      MAXP   ( 3 )
      DOUBLE PRECISION      MINLAT
      DOUBLE PRECISION      MINP   ( 3 )
      DOUBLE PRECISION      NRMPLN ( UBPL )
      DOUBLE PRECISION      OFF1   ( 3 )
      DOUBLE PRECISION      OFF2   ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PLNX   ( 3 )
      DOUBLE PRECISION      S1
      DOUBLE PRECISION      S2
      DOUBLE PRECISION      UAXIS  ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      UOFF1  ( 3 )
      DOUBLE PRECISION      UOFF2  ( 3 )
      DOUBLE PRECISION      UUAX
      DOUBLE PRECISION      UV1    ( 3 )
      DOUBLE PRECISION      UV2    ( 3 )
      DOUBLE PRECISION      V1     ( 3 )
      DOUBLE PRECISION      V1MAG
      DOUBLE PRECISION      V2     ( 3 )
      DOUBLE PRECISION      V2MAG
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      VTEMP2 ( 3 )
      DOUBLE PRECISION      W2
      DOUBLE PRECISION      WU
      DOUBLE PRECISION      WUAX
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XOFF1  ( 3 )
      DOUBLE PRECISION      XOFF2  ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )

      INTEGER               I
      INTEGER               N
      INTEGER               NPLNX

      LOGICAL               IN1
      LOGICAL               IN2
      LOGICAL               ISBRCK
      LOGICAL               NEG1
      LOGICAL               NEG2

!
!     Saved values
!
      SAVE                  ORIGIN
      SAVE                  Y
      SAVE                  Z

!
!     Initial values
!
      DATA                  ORIGIN / 3 * 0.D0         /
      DATA                  Y      / 0.D0, 1.D0, 0.D0 /
      DATA                  Z      / 0.D0, 0.D0, 1.D0 /

!
!     Use quasi-discovery check-in. We'll check in before
!     code sections that can generate SPICE errors, and check
!     out afterward. When those code sections are skipped,
!     we avoid traceback participation.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     No intersection was found so far.
!
      NXPTS = 0

!
!     The cone's axis vector must be non-zero.
!
      CALL UNORM ( AXIS, UAXIS, AXMAG )

      IF ( AXMAG .EQ. 0.D0 ) THEN

         CALL CHKIN  ( 'INCNSG'                                       )
         CALL SETMSG ( 'The cone''s axis vector must be non-zero ' &
         //            'but sadly, it failed to meet this criterion.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                            )
         CALL CHKOUT ( 'INCNSG'                                       )
         RETURN

      END IF

!
!     The cone's angular radius must be non-negative.
!
      IF ( ANGLE .LT. 0.D0 ) THEN

         CALL CHKIN  ( 'INCNSG'                             )
         CALL SETMSG ( 'The cone''s angular radius must be ' &
         //            ' non-negative but was # (radians).' )
         CALL ERRDP  ( '#', ANGLE                           )
         CALL SIGERR ( 'SPICE(INVALIDANGLE)'                )
         CALL CHKOUT ( 'INCNSG'                             )
         RETURN

      END IF

!
!     The endpoints of the segment must be distinct. Check this after
!     computing a unit direction vector for the line segment.
!
      CALL VSUB  ( ENDPT2, ENDPT1, DIR )

      CALL UNORM ( DIR, UDIR, DMAG )

      IF ( DMAG .EQ. 0.D0 ) THEN

         CALL CHKIN  ( 'INCNSG'                              )
         CALL SETMSG ( 'The distance between the segment''s ' &
         //            'endpoints was zero. First endpoint: ' &
         //            '(# # #).'                            )
         CALL ERRDP  ( '#', ENDPT1(1)                        )
         CALL ERRDP  ( '#', ENDPT1(2)                        )
         CALL ERRDP  ( '#', ENDPT1(3)                        )
         CALL SIGERR ( 'SPICE(ENDPOINTSMATCH)'               )
         CALL CHKOUT ( 'INCNSG'                              )
         RETURN

      END IF

!
!     Store the cosine of the cone's angular radius. We'll treat all
!     cases with COSANG equal to 0 as though the cone is actually a
!     plane normal to the axis and containing the apex.
!
      COSANG = COS( ANGLE )
      LOCANG = ANGLE
!
!     We'll work with a local axis that has angular separation of
!     no more than pi/2 from the nappe.
!
      IF ( COSANG .LT. 0.D0 ) THEN

         COSANG   = - COSANG
         LOCANG   =   PI() - ANGLE

         UAXIS(1) = - UAXIS(1)
         UAXIS(2) = - UAXIS(2)
         UAXIS(3) = - UAXIS(3)

      END IF

!
!     Compute the offsets of the endpoints of the segment from
!     the cone's apex.
!
      CALL VSUB ( ENDPT1, APEX, OFF1 )
      CALL VSUB ( ENDPT2, APEX, OFF2 )

!
!     Deal with some of the simple cases first.
!
      CALL VHAT ( OFF1, UOFF1 )
      CALL VHAT ( OFF2, UOFF2 )

      DP1  = VDOT( UOFF1, UAXIS )
      DP2  = VDOT( UOFF2, UAXIS )

!
!     The given axis is inside the nappe defined by the angular radius.
!
!     There's no intersection if both endpoints are in the interior of
!     the nappe of the cone (since the nappe is convex).
!
      IN1  = DP1 .GE. COSANG
      IN2  = DP2 .GE. COSANG
!
!     If the line segment lies on the far side of the plane that
!     contains the apex and is orthogonal to the axis, there's no
!     intersection.
!
      NEG1 = DP1 .LT. 0.D0
      NEG2 = DP2 .LT. 0.D0

      IF (      ( IN1  .AND. IN2  ) &
           .OR. ( NEG1 .AND. NEG2 ) ) THEN
!
!        The segment is in the interior of the cone or
!        on the far side of the plane.
!
         NXPTS = 0

         RETURN

      END IF

!
!     Here's where we handle the half-space case.
!
      IF ( ABS(COSANG) .LT. HSPTOL ) THEN
!
!        See whether the ray emanating from the first endpoint and
!        having direction UDIR hits the plane normal to the axis and
!        containing the apex. We'll call this plane NRMPLN.
!
!        NVP2PL can signal an error only if the input axis is the
!        zero vector. We've ensured that it isn't.
!
         CALL NVP2PL ( UAXIS,  APEX, NRMPLN )
         CALL INRYPL ( ENDPT1, UDIR, NRMPLN, NPLNX, PLNX )

!
!        If the ray doesn't hit the plane, we're done. Otherwise,
!        check the intercept.
!
         IF ( NPLNX .EQ. 1 ) THEN
!
!           The ray does hit the plane. If the intersection is on the
!           line segment, we have a solution.
!
            IF ( VDIST(PLNX, ENDPT1) .LE. DMAG ) THEN
!
!              The intercept is not further along the ray than the
!              second endpoint. It's a valid solution.
!
               NXPTS = 1
               CALL VEQU ( PLNX, XPT1 )

            END IF

         END IF
!
!        This is the end of the half-space case.
!
         RETURN

      END IF

!
!     At this point we've disposed of the trivial cases. We'll
!     set up a quadratic equation for the intersection of the
!     line segment with the surface of the cone's nappe.
!
!     Due to round-off errors, the solution of the quadratic may
!     either be inaccurate or may not be found at all. We'll
!     examine the solutions we find and solve the problem by
!     an alternate method if necessary. However, the quadratic
!     method is fast, so we give it priority.
!
!     The equation of a ray starting at ENDPT1 and having unit
!     direction vector UDIR is
!
!        RAY  = { ENDPT1 + s*UDIR, s >= 0 }                          (1)
!
!     The equation of the nappe of the cone is
!
!        CONE = { X: < X - APEX, UAXIS > = ||X-APEX|| * cos(ANGLE) } (2)
!
!     where ANGLE is the angular radius of the cone and UAXIS is the
!     unit axis vector. Substituting the right hand side expression of
!     (1) for X in equation (2) and squaring both sides yields a
!     quadratic equation for S. We'll derive the coefficients of the
!     equation below.
!
!     Let
!
!        Q  = X - APEX
!        W  = ENDPT1 - APEX
!        U  = UDIR
!        CA = cos(ANGLE)
!
!     We can translate the cone and ray by -APEX, and (1) and (2)
!     can be re-written as
!
!        RAY  = { W + s*U, s >= 0 }                                  (3)
!
!        CONE = { Q: < Q, UAXIS > = ||Q|| * cos(ANGLE) }             (4)
!
!
!        Substituting the ray expression for Q, we obtain
!
!           < W + s*U, UAXIS > = ||W+s*U|| * CA                      (5)
!
!        and squaring both sides yields
!
!                      2                                      2   2
!             <W,UAXIS>  + 2*<W,UAXIS>*<U,UAXIS>*s + <U,UAXIS> * s
!
!                    2                2       2
!           = ( ||W||  + 2*<W,U>*s + s  ) * CA                       (6)
!
!
!       Collecting coefficients of powers of s, we have
!
!                         2     2     2
!              ( <U,UAXIS>  - CA ) * s
!
!                                            2
!            + 2 * ( <W,UAXIS>*<U,UAXIS> - CA * <W,U> ) * s
!
!                       2        2    2
!            + <W,UAXIS>  - ||W|| * CA
!
!
!         =  0                                                       (7)
!
!
!      Before continuing, we observe that the only non-unit vector
!      in (7) is W. So the coefficients in (7) have no possibility
!      of overflowing unless the vertex of the ray is very far from
!      the apex of the cone.
!
!      W has been computed above as OFF1.
!
!
!         [ Consider adding check on OFF1 here. ]
!
!
!     Intermediate values:
!
      UUAX = VDOT( UDIR, UAXIS )
      WUAX = VDOT( OFF1, UAXIS )
      WU   = VDOT( OFF1, UDIR  )
      W2   = VDOT( OFF1, OFF1  )
      CA2  = COSANG * COSANG

!
!     Quadratic coefficients:
!
      A    = ( UUAX * UUAX ) - CA2

      B    = 2 * (  ( WUAX * UUAX ) - ( CA2 * WU )  )

      C    = ( WUAX * WUAX ) - ( W2 * CA2 )

!
!     We're not interested in solutions that lie outside
!     of the line segment. The length of the segment is
!     DMAG.
!
!     Solve the equation, using DMAG as an upper bound
!     on the magnitude of the roots.
!
      CALL ZZCNQUAD ( A, B, C, DMAG, N, S1, S2 )

!
!     Compute the possible intersection points and test them
!     to make sure they really are solutions.
!
      IF ( N .GT. 0 ) THEN
!
!        Start with the solution closest to the ray's vertex.
!        Compute XPT1 and make sure it's on the correct nappe
!        of the cone.
!
         IF ( S1 .GE. 0.D0 ) THEN

            XPT1(1) = ENDPT1(1) + S1*UDIR(1)
            XPT1(2) = ENDPT1(2) + S1*UDIR(2)
            XPT1(3) = ENDPT1(3) + S1*UDIR(3)

            CALL VSUB  ( XPT1, APEX, V1 )

!
!           See whether V1 is on the cone.
!
            CALL UNORM ( V1, UV1, V1MAG )

            IF ( V1MAG .GT. 0.D0 ) THEN

               COSERR = ABS( VDOT(UV1,UAXIS) - COSANG )
            ELSE
               COSERR = 0.D0
            END IF

            IF (      ( V1MAG  .EQ. 0.D0   ) &
                 .OR. ( COSERR .LT. COSTOL ) ) THEN
!
!              The root is on the cone (on the apex if V1MAG is zero).
!
!              We accept this root. Update NXPTS. Note that this is
!              not necessarily the final value of NXPTS; that
!              depends on the validity of the second root.

               NXPTS = 1

            END IF

         END IF

         IF ( N .EQ. 2 ) THEN
!
!           Check the second root.
!
            IF ( S2 .GE. 0.D0 ) THEN

               XPT2(1) = ENDPT1(1) + S2*UDIR(1)
               XPT2(2) = ENDPT1(2) + S2*UDIR(2)
               XPT2(3) = ENDPT1(3) + S2*UDIR(3)

               CALL VSUB  ( XPT2, APEX, V2 )

!
!              See whether V2 is on the cone.
!
               CALL UNORM ( V2, UV2, V2MAG )

               IF ( V2MAG .GT. 0.D0 ) THEN

                  COSERR = ABS( VDOT(UV2,UAXIS) - COSANG )
               ELSE
                  COSERR = 0.D0
               END IF

               IF (      ( V2MAG  .EQ. 0.D0   ) &
                    .OR. ( COSERR .LT. COSTOL ) ) THEN
!
!                 The root is on the cone (on the apex if V2MAG is
!                 zero).
!
!                 We accept this root.
!
                  NXPTS = NXPTS + 1

                  IF ( NXPTS .EQ. 1 ) THEN
!
!                    This is the only valid root; overwrite XPT1.
!
                     CALL VEQU ( XPT2, XPT1 )

                  END IF

               END IF

            END IF

         END IF

      END IF

!
!     We're not done yet. If we have fewer roots than we should, we'll
!     need to solve the problem by an alternate method.
!
!     If we have two roots, we're in good shape. Otherwise we must
!     determine how many roots should be found.
!
      IF ( NXPTS .LT. 2 ) THEN
!
!        We must determine the expected number of roots, and if
!        we didn't come up with them, we must find the roots
!        by an alternate method.
!
!        We'll examine the containment of the endpoints within the
!        cone.
!
!        The case where both endpoints are inside the cone was handled
!        earlier.
!
!        If one endpoint is inside the cone and one is outside,
!        we expect to have one root.
!
         IF (     ( IN1 .AND. ( .NOT. IN2 ) ) &
             .OR. ( IN2 .AND. ( .NOT. IN1 ) )  ) THEN
!
!           There's supposed to be one root. If we found none, find one
!           now.
!
            IF ( NXPTS .EQ. 0 ) THEN
!
!              ZZCXBRUT signals an error if the axis is the zero
!              vector, but not otherwise. We've already ruled out this
!              situation. Therefore, we don't check in before the
!              following call.
!
               CALL ZZCXBRUT ( APEX,   UAXIS,  LOCANG, &
                               ENDPT1, ENDPT2, XPT1,  ISBRCK )

               IF ( ISBRCK ) THEN
!
!                 As long as the root was bracketed, XPT1 is a
!                 solution.
!
                  NXPTS = 1

               END IF

            END IF


         ELSE

            CALL CHKIN ( 'INCNSG' )
!
!           Both endpoints are outside the cone. We could have zero to
!           two roots. If the minimum angular separation of the segment
!           from the axis is less than ANGLE, we expect to find two
!           roots; if it's equal to ANGLE, we expect to find one, and
!           if it's greater than ANGLE, none.
!
!           We'll transform OFF1 and OFF2 into a reference frame in
!           which angular separation from the axis is equivalent to
!           colatitude. Then we'll find the maximum latitude attained
!           on the segment.
!
!           We'll count the roots we find, so we'll start at zero.
!
            NXPTS = 0

            CALL FRAME ( UAXIS, X, Y )

            DO I = 1, 3

               XFORM(1,I) = X(I)
               XFORM(2,I) = Y(I)
               XFORM(3,I) = UAXIS(I)

            END DO

            CALL MXV ( XFORM, OFF1, XOFF1 )
            CALL MXV ( XFORM, OFF2, XOFF2 )

            CALL ZZSGLATX ( XOFF1, XOFF2, MINLAT, MINP, MAXLAT, MAXP )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'INCNSG' )
               RETURN
            END IF

!
!           COLAT is the colatitude of the point of maximum latitude.
!
            COLAT = HALFPI() - MAXLAT


            IF ( COLAT .LT. LOCANG ) THEN
!
!              MAXP is inside the cone. There should be an intersection
!              on the segment between XOFF1 and MAXP and another
!              between MAXP and XOFF2.
!
               CALL ZZCXBRUT ( ORIGIN, Z,    LOCANG, &
                               XOFF1,  MAXP, VTEMP,  ISBRCK )

               IF ( ISBRCK ) THEN
!
!                 Convert VTEMP to the original frame, then translate
!                 it so that it's represented as an offset from the
!                 origin.
!
                  CALL MTXV ( XFORM,  VTEMP, VTEMP2 )
                  CALL VADD ( VTEMP2, APEX,  XPT1   )

                  NXPTS = 1

               END IF

               CALL ZZCXBRUT ( ORIGIN, Z,     LOCANG, &
                               MAXP,   XOFF2, VTEMP,  ISBRCK )

               IF ( ISBRCK ) THEN
!
!                 Convert VTEMP to the original frame, then translate
!                 it so that it's represented as an offset from the
!                 origin.
!
                  CALL MTXV ( XFORM,  VTEMP, VTEMP2 )
                  CALL VADD ( VTEMP2, APEX,  XPT2   )

                  IF ( NXPTS .EQ. 1 ) THEN
!
!                    Both roots are valid.
!
                     NXPTS = 2

                  ELSE
!
!                    The second root is the only valid root. Move it
!                    into XPT1.
!
                     CALL VEQU ( XPT2, XPT1 )

                     NXPTS = 1

                  END IF

               END IF


            ELSE IF ( COLAT .EQ. LOCANG ) THEN
!
!              The root corresponds to a point of tangency of
!              the segment and cone. This occurs at the point
!              having maximum latitude: MAXP.
!
               CALL VEQU ( MAXP, XPT1 )

               NXPTS = 1

!
!           Note that if COLAT > LOCANG, there are no roots.
!
            END IF


            CALL CHKOUT ( 'INCNSG' )

         END IF
!
!        This is the end of portion of the "brute force" branch in
!        which both endpoints are outside the cone.
!
      END IF

!
!     NXPTS  has been set.
!
!     If NXPTS is 1, then XPT1 is set.
!
!     If NXPTS is 2, then both XPT1 and XPT2 are set.
!

      RETURN
      END
