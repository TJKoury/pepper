!$Procedure      ZZELVUPY ( Is ellipse in polygonal field of view? )

      SUBROUTINE ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, BOUNDS, FOUND )
      IMPLICIT NONE

! $ Abstract
!
!     Determine whether a specified ellipse intersects the pyramid
!     defined by a polygonal field of view.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     ELLIPSES
!     PLANES
!
! $ Keywords
!
!     ELLIPSE
!     GEOMETRY
!     MATH
!     PLANE
!
! $ Declarations

      INCLUDE 'errhnd.inc'

      INTEGER               UBEL
      PARAMETER           ( UBEL = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL = 4 )

      INTEGER               MAXFOV
      PARAMETER           ( MAXFOV = 10000 )

      DOUBLE PRECISION      ELLIPS ( UBEL )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      AXIS   ( 3 )
      INTEGER               N
      DOUBLE PRECISION      BOUNDS ( 3, N )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ELLIPS     I   A SPICELIB ellipse.
!     VERTEX     I   Vertex of a pyramid.
!     AXIS       I   Axis of a pyramid.
!     N          I   Number of boundary vectors of the pyramid.
!     BOUNDS     I   Boundary vectors of the pyramid.
!     FOUND      O   Flag indicating whether intersection was found.
!     UBEL       P   Upper bound of SPICELIB ellipse array.
!     UBPL       P   Upper bound of SPICELIB plane array.
!     MAXFOV     P   Maximum number of boundary vectors.
!
! $ Detailed_Input
!
!     ELLIPS         is a SPICELIB ellipse having non-zero semi-axes.
!
!     VERTEX         is the single point of intersection of the vectors
!                    defining the edges of a pyramid.  The vectors
!                    emanate from this point.  The pyramid represents
!                    the spatial region viewed by a polygonal field of
!                    view (FOV).
!
!     AXIS           is a vector emanating from VERTEX that lies inside
!                    the pyramid defined by VERTEX, N, and BOUNDS.
!                    AXIS represents the boresight direction of the FOV.
!
!     N,
!     BOUNDS         are, respectively, the number of boundary vectors
!                    defining the pyramid and the boundary vectors
!                    themselves.  Each pair of consecutive vectors in
!                    the array BOUNDS, together with VERTEX, defines a
!                    face of the pyramid.
!
!                    Each boundary vector must have angular separation
!                    of less than pi/2 radians from AXIS.
!
!                    For any plane that doesn't contain VERTEX and that
!                    intersects AXIS at right angles, the intersections
!                    of the boundary vectors with that plane are the
!                    vertices of a polygon.  The polygon need not be
!                    convex, but it must be non-self-intersecting.
!
!
! $ Detailed_Output
!
!     FOUND          is set to .TRUE. if the pyramid and ellipse
!                    intersect; otherwise FOUND is .FALSE.
!
! $ Parameters
!
!     UBEL           is the array upper bound for SPICELIB ellipses.
!
!     UBPL           is the array upper bound for SPICELIB planes.
!
!     MAXFOV         is the maximum number of boundary vectors that
!                    may be supplied in the input array argument
!                    BOUNDS.
!
! $ Exceptions
!
!     If an error is found, the output argument FOUND will be set to
!     .FALSE.
!
!
!     1)  If either of the semi-axes of the input ellipse is the
!         zero vector, the error SPICE(ZEROVECTOR) will be signaled.
!
!     2)  If the norm of the input ellipse's semi-minor axis is
!         zero after division by the maximum of the norms of the
!         semi-major axis, the ellipse's center, and the vertex of
!         the pyramid, the error SPICE(DEGENERATECASE) will be
!         signaled.
!
!     3)  If the vertex of the pyramid lies in the plane containing
!         the ellipse, at most the edge of the ellipse can be "seen"
!         from the vertex.  This case is not considered to be an
!         error.
!
!     4)  If the number of boundary vectors N is not at least 3,
!         or if the number exceeds MAXFOV, the error
!         SPICE(INVALIDCOUNT) will be signaled.
!
!     5)  If any boundary vector is the zero vector, the error
!         SPICE(ZEROVECTOR) will be signaled.
!
!     6)  If the axis is the zero vector, the error SPICE(ZEROVECTOR)
!         will be signaled.
!
!     7)  If any boundary vector has angular separation of at least
!         pi/2 radians from AXIS, the error SPICE(INVALIDFOV)
!         will be signaled.
!
!     8)  If any boundary vector has angular separation of zero
!         radians from one of its neighbors, the error SPICE(INVALIDFOV)
!         will be signaled.
!
!     9)  No test is done to ensure that the input boundary vectors
!         define a non-self-intersecting polygon via their intersection
!         with a plane normal to AXIS.  If the boundary vectors don't
!         meet this condition, the results of this routine are
!         unreliable.
!
!     10) The pyramidal field of view and the input ellipse must not
!         differ too radically in scale, or great loss of precision
!         will result, making the results of this routine unreliable.
!         For example, if the ratio of the norm of the semi-minor axis
!         of the ellipse to the distance from VERTEX to the center of
!         the ellipse is less than double precision epsilon on the host
!         system, a meaningful result can't be computed.
!
!         This routine does not attempt to judge the minimum
!         acceptable level of accuracy.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine is useful for determining whether an ellipsoidal
!     body is in the field of view of a remote-sensing instrument
!     with a field of view having polygonal cross section.
!
! $ Examples
!
!     Test an ellipse for intersection with a square field
!     of view.
!
!
!           PROGRAM EX1
!           IMPLICIT NONE
!
!           INTEGER               MAXN
!           PARAMETER           ( MAXN   = 4 )
!
!           INTEGER               UBEL
!           PARAMETER           ( UBEL   = 9 )
!
!           DOUBLE PRECISION      AXIS   ( 3 )
!           DOUBLE PRECISION      CENTER ( 3 )
!           DOUBLE PRECISION      ELLIPS ( UBEL )
!           DOUBLE PRECISION      FOV    ( 3, MAXN )
!           DOUBLE PRECISION      SMAJOR ( 3 )
!           DOUBLE PRECISION      SMINOR ( 3 )
!           DOUBLE PRECISION      VERTEX ( 3 )
!
!           INTEGER               N
!
!           LOGICAL               FOUND
!
!     C
!     C     The FOV (field of view) "looks" in the -x direction:
!     C     the axis of the FOV is parallel to the x axis.
!     C     The FOV intersects the plane of the ellipse in a
!     C     square having height and width 4 units.  The edges
!     C     of the square are parallel to the y and z axes.
!     C
!           N = 4
!
!           CALL VPACK ( -1.D0,  -1.D0, -1.D0,  FOV(1,1) )
!           CALL VPACK ( -1.D0,   1.D0, -1.D0,  FOV(1,2) )
!           CALL VPACK ( -1.D0,   1.D0,  1.D0,  FOV(1,3) )
!           CALL VPACK ( -1.D0,  -1.D0,  1.D0,  FOV(1,4) )
!
!           CALL VPACK ( -1.D0,   0.D0,  0.D0,  AXIS     )
!           CALL VPACK (  1.D0,   0.D0,  0.D0,  VERTEX   )
!
!     C
!     C     The ellipse is oriented with the major axis
!     C     vertical and is parallel to the x-z plane.  The ellipse
!     C     lies in the plane defined by x = -1.  The ellipse
!     C     ever-so-slightly overlaps the bottom edge of the FOV.
!     C
!           CALL VPACK (  0.D0,   0.D0,   1.D0,           SMAJOR )
!           CALL VPACK (  0.D0,   5.D-1,  0.D0,           SMINOR )
!           CALL VPACK ( -1.D0,   0.D0,  -3.D0 + 1.D-12,  CENTER )
!
!     C
!     C     Create a SPICELIB ellipse from the center and semi-axes.
!     C
!           CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )
!
!     C
!     C     Test for intersection.  We expect an intersection to be
!     C     found.
!     C
!           CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
!
!           WRITE (*,*) 'Case 1: FOUND = ', FOUND
!
!     C
!     C     Shift the ellipse center to move the ellipse outside of
!     C     the FOV, then repeat the test.  We expect FOUND to be
!     C     .FALSE.
!     C
!           CALL VPACK ( -1.D0,   0.D0,  -3.D0 - 1.D-12,  CENTER )
!
!           CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLIPS )
!
!           CALL ZZELVUPY ( ELLIPS, VERTEX, AXIS, N, FOV, FOUND )
!
!           WRITE (*,*) 'Case 2: FOUND = ', FOUND
!
!           END
!
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1] `Calculus and Analytic Geometry', Thomas and Finney.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 28-FEB-2008 (BVS)
!
!        Corrected the contents of the Required_Reading section.
!
!-    SPICELIB Version 1.0.0, 10-AUG-2005 (NJB)
!
!-&

! $ Index_Entries
!
!     test whether pyramid intersects ellipse
!     test whether ellipse is in pyramidal field of view
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      INTEGER               ZZWIND

      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!

!
!     Local variables
!
      CHARACTER*(LMSGLN)    ERRMSG

      DOUBLE PRECISION      A
      DOUBLE PRECISION      APEX   ( 3 )
      DOUBLE PRECISION      ASEP
      DOUBLE PRECISION      B
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CONSEP
      DOUBLE PRECISION      CP     ( 3 )
      DOUBLE PRECISION      CTRVEC ( 3 )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      EASIZE
      DOUBLE PRECISION      EBSCTR ( 3 )
      DOUBLE PRECISION      EDGE1  ( 3 )
      DOUBLE PRECISION      EDGE2  ( 3 )
      DOUBLE PRECISION      ELLSCL ( UBEL )
      DOUBLE PRECISION      EPLANE ( UBPL )
      DOUBLE PRECISION      FOVPLN ( UBPL )
      DOUBLE PRECISION      GV1    ( 3 )
      DOUBLE PRECISION      GV2    ( 3 )
      DOUBLE PRECISION      HAFEDG
      DOUBLE PRECISION      HAFSEC
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      PASIZE
      DOUBLE PRECISION      PLANE  ( UBPL )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      VBSCTR ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      VXPT1  ( 3 )
      DOUBLE PRECISION      VXPT2  ( 3 )
      DOUBLE PRECISION      WORK   ( 3, MAXFOV )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XPT1   ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
      DOUBLE PRECISION      Y

      INTEGER               I
      INTEGER               J
      INTEGER               NXPTS


!
!     Saved variables
!
      SAVE                 ORIGIN
      SAVE                 WORK

!
!     Initial values
!
      DATA                 ORIGIN /  3 * 0.D0 /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZELVUPY' )

!
!     We start out by checking the inputs.
!
!     The next step will be to look for an intersection of the ellipse
!     and pyramid.  There are three intersection cases:
!
!        1) The ellipse is completely contained in the pyramid.
!
!        2) The ellipse "contains" the field of view in the sense
!           that the intersection of the pyramid and the plane of the
!           ellipse is contained in the region bounded by the ellipse.
!
!        3) One or more sides of the pyramid intersect the ellipse.
!
!     There is also a non-intersection case:  this is when cones
!     bounding the ellipse and pyramid and having their apexes in
!     common with that of the pyramid intersect only in that common
!     apex.  Before test (1), we perform this non-intersection test,
!     since it can be done quickly.
!
!     No intersection has been found so far.  Set the default value
!     of the FOUND flag here so it won't have to be set in every error
!     checking block below.
!
      FOUND = .FALSE.

!
!     Validate the ellipse.  First find the center and the semi-axes
!     of the ellipse.
!
      CALL EL2CGV ( ELLIPS, CENTER, GV1,    GV2    )
      CALL SAELGV ( GV1,    GV2,    SMAJOR, SMINOR )

!
!     Check the semi-axis lengths.
!
!     If the semi-major axis is the zero vector, we'd expect
!     the semi-minor axis to be the zero vector as well.  But
!     round-off error could conceivably violate this assumption.
!
      IF ( VZERO(SMAJOR) .OR. VZERO(SMINOR) ) THEN

         CALL SETMSG ( 'Input ellipse has semi-major axis '   // &
                       'length # and semi-minor axis length ' // &
                       '#.  Both vectors are required to be ' // &
                       'non-zero.'                            )
         CALL ERRDP  ( '#', VNORM(SMAJOR)                     )
         CALL ERRDP  ( '#', VNORM(SMINOR)                     )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                    )
         CALL CHKOUT ( 'ZZELVUPY'                             )
         RETURN

      END IF

!
!     Scale the vectors defining the ellipse and the vertex of the
!     pyramid so that the largest of these vectors has unit length.
!
      SCALE = 1.D0 / MAX ( VNORM(CENTER), VNORM(SMAJOR), VNORM(VERTEX) )

      DO I = 1, 3

         CENTER(I) = SCALE * CENTER(I)
         SMAJOR(I) = SCALE * SMAJOR(I)
         SMINOR(I) = SCALE * SMINOR(I)
         APEX  (I) = SCALE * VERTEX(I)

      END DO

!
!     Create a scaled ellipse.  We'll perform the FOV side-ellipse
!     intersection computations using this ellipse.
!
      CALL CGV2EL ( CENTER, SMAJOR, SMINOR, ELLSCL )

!
!     After scaling, make sure the semi-axes have sufficient length to
!     prevent numerical problems.  Let A and B be the scaled semi-axis
!     lengths of the ellipse.
!
      A = VNORM ( SMAJOR )
      B = VNORM ( SMINOR )

      IF ( B .EQ. 0.D0 ) THEN

         CALL SETMSG ( 'Scaled ellipse''s semi-minor axis length = 0.' )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'                         )
         CALL CHKOUT ( 'ZZELVUPY'                                      )
         RETURN

      END IF

!
!     Validate the input pyramid.
!
!     The axis must not be the zero vector.
!
      IF (  VZERO(AXIS)  ) THEN

         CALL SETMSG ( 'The pyramid''s axis the zero vector.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                    )
         CALL CHKOUT ( 'ZZELVUPY'                             )
         RETURN

      END IF

!
!     There must be at least three boundary vectors.
!
      IF ( N .LT. 3 ) THEN

         CALL SETMSG ( 'The number of boundary vectors was #; ' // &
                       'this number must be at least 3.'        )
         CALL ERRINT ( '#',  N                                  )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'ZZELVUPY'                               )
         RETURN

      END IF

!
!     There must be no more than MAXFOV boundary vectors.
!
      IF ( N .GT. MAXFOV ) THEN

         CALL SETMSG ( 'The number of boundary vectors was #; ' // &
                       'this number must not exceed #.'         )
         CALL ERRINT ( '#',  N                                  )
         CALL ERRINT ( '#',  MAXFOV                             )
         CALL SIGERR ( 'SPICE(INVALIDCOUNT)'                    )
         CALL CHKOUT ( 'ZZELVUPY'                               )
         RETURN

      END IF

!
!     We must initialize certain variables before continuing with
!     the checks.
!
!     Let CTRVEC be the vector from the apex to the center of the
!     ellipse.  This vector will be used in several places later;
!     it's convenient to compute it here.
!
      CALL VSUB ( CENTER, APEX, CTRVEC )

!
!     Compute PASIZE:  an upper bound on the angular radius of a
!     circular cone whose axis is the input central axis.  While
!     we're at it, check the angular separation of the boundary
!     vectors from the central axis and from each other.
!
      PASIZE = 0.D0

      DO I = 1, N
!
!        Each boundary vector must have angular separation from the
!        axis of less than pi/2 radians.  Keep track of the maximum
!        angular separation PASIZE as we go.  We'll use this variable
!        later in a non-intersection test.
!
         ASEP = VSEP ( AXIS, BOUNDS(1,I) )

         IF ( ASEP .GE. PI()/2  ) THEN

            CALL SETMSG ( 'The angular separation of boundary '   // &
                          'vector # from the axis is #. This '    // &
                          'number must less than pi/2.'           )
            CALL ERRINT ( '#',  I                                 )
            CALL ERRDP  ( '#',  ASEP                              )
            CALL SIGERR ( 'SPICE(INVALIDFOV)'                     )
            CALL CHKOUT ( 'ZZELVUPY'                              )
            RETURN

         END IF

         PASIZE = MAX ( PASIZE, ASEP )

!
!        Each boundary vector must have non-zero angular separation
!        from its neighbors.
!
         IF ( I .LT. N ) THEN
            J  = I + 1
         ELSE
            J  = 1
         END IF

         CALL UCRSS ( BOUNDS(1,I), BOUNDS(1,J), CP )

         IF ( VZERO(CP) ) THEN
!
!           The cross product may be zero because one of the
!           boundary vectors is zero.  Check this first.
!
            IF (  VZERO( BOUNDS(1,J) ) .OR. VZERO( BOUNDS(1,I) ) ) THEN

               ERRMSG = 'The # boundary vector is the zero vector.'

               IF (  VZERO( BOUNDS(1,I) )  ) THEN
                  J = I
               END IF

               CALL REPMOT ( ERRMSG, '#', J, 'L', ERRMSG )
               CALL SETMSG ( ERRMSG                      )
               CALL SIGERR ( 'SPICE(ZEROVECTOR)'         )

            ELSE

               CALL SETMSG ( 'The angular separation of boundary '   // &
                             'vector # from vector # is 0.'          // &
                             'This number must be positive.'         )
               CALL ERRINT ( '#',  I                                 )
               CALL ERRINT ( '#',  J                                 )
               CALL SIGERR ( 'SPICE(INVALIDFOV)'                     )

            END IF

            CALL CHKOUT ( 'ZZELVUPY' )
            RETURN

         END IF

      END DO

!
!     That's it for the error checks.  We'll now answer the question
!     this routine is meant to answer:  does the ellipse or the region
!     it bounds intersect the pyramid?
!
!     We'll start out with a simple check to rule out intersection
!     when the ellipse and pyramid are contained in disjoint right
!     circular cones with a common apex.
!
!     Find the angular radius (that is, one-half of the angular extent)
!     of a bounding cone of the ellipse as seen from the apex.  The
!     cone circumscribes a sphere of radius A centered at the ellipse's
!     center, where A is the length of the semi-major axis.  Note that
!     the cone does not in general circumscribe the ellipse itself.
!
!     The test can be performed only if the apex of the FOV is outside
!     of the sphere of radius A centered at the ellipse center.
!
      D  =  VDIST ( CENTER, APEX )

      IF ( A .LT. D ) THEN

         EASIZE  =  ASIN ( A / D )

!
!        The variable PASIZE already contains the angular radius of a
!        bounding cone of the pyramid as seen from the pyramid's apex.
!        The angular radius is the maximum of the angular separations
!        of each pyramid edge from the pyramid's axis. Check whether
!        the bounding cones of ellipse and pyramid are disjoint. Recall
!        CTRVEC is the vector from the apex to the center of the
!        ellipse.  If the angular separation of CTRVEC and AXIS exceeds
!        the sum of the angular radii of the ellipse's and pyramid's
!        bounding cones, there can be no intersection.
!
         CONSEP = VSEP ( CTRVEC, AXIS )  -  ( EASIZE + PASIZE )

         IF ( CONSEP .GT. 0.D0 ) THEN

            CALL CHKOUT ( 'ZZELVUPY' )
            RETURN
         END IF

      END IF

!
!     At this point, we have to take a more detailed look at the
!     possible intersection of ellipse and pyramid.  First check
!     whether the center of the ellipse is contained in the pyramid.
!     If the ellipse is completely contained in the pyramid, this
!     check will yield a positive result.
!
!     The center of the ellipse is inside the pyramid if a plane
!     containing this point and normal to the axis vector
!     chops the pyramid in a polygon that has non-zero winding
!     number about the center.
!
!     The center of the ellipse must lie in the correct half-space
!     for this test to be applicable.
!
      IF (  VDOT( AXIS, CTRVEC ) .GT. 0.D0 ) THEN
!
!        Construct the plane and find the polygon.
!
         CALL NVP2PL ( AXIS, CTRVEC, FOVPLN )

!
!        Create the planar FOV boundary using the intersections
!        of the FOV boundary vectors with FOVPLN.
!
         DO I = 1, N

            CALL INRYPL ( ORIGIN,  BOUNDS(1,I),  FOVPLN, &
                          NXPTS,   WORK  (1,I)           )

!
!           We expect to have a single point of intersection for each
!           boundary vector.
!
            IF ( NXPTS .NE. 1 ) THEN

               CALL SETMSG ( 'NXPTS = # for boundary vector #/FOV ' // &
                             'plane intersection.'                  )
               CALL ERRINT ( '#',  NXPTS                            )
               CALL ERRINT ( '#',  I                                )
               CALL SIGERR ( 'SPICE(BUG)'                           )
               CALL CHKOUT ( 'ZZELVUPY'                             )
               RETURN

            END IF

         END DO

!
!        Now WORK contains the polygon representing the intersection of
!        the pyramid with the plane FOVPLN. If the winding number of
!        the polygon about the ellipse center is non-zero, we conclude
!        the center is in the pyramid.
!
         IF (   ZZWIND( FOVPLN, N, WORK, CTRVEC )  .NE.  0 ) THEN
!
!           The center of the ellipse is inside the pyramid.  We're
!           done.
!
            FOUND = .TRUE.

            CALL CHKOUT ( 'ZZELVUPY' )
            RETURN

         END IF

      END IF

!
!     Check whether the ray defined by APEX and the first boundary
!     vector of the pyramid (the "boundary ray") intersects the plane
!     region bounded by the ellipse.  If the intersection of the
!     pyramid and the plane of the ellipse is completely contained in
!     the region bounded by the ellipse, this check will yield a
!     positive result.
!
!     First find the intersection of the boundary ray and the plane
!     containing the ellipse; represent this plane using the SPICELIB
!     plane EPLANE.

!     We don't check FAILED() here because the spanning vectors
!     are orthogonal, and because PSV2PL (via a call to UCRSS)
!     does scaling to prevent underflow.
!
      CALL PSV2PL ( CENTER,  SMAJOR,       SMINOR,  EPLANE )

      CALL INRYPL ( APEX,    BOUNDS(1,1),  EPLANE,  NXPTS,  XPT )

!
!     The routine INRYPL can return the NXPTS values 1, 0, or INF---a
!     code indicating an infinite number of intersection points of ray
!     and plane.  If the value is 1, the boundary ray may intersect
!     the region bounded by the ellipse.
!
      IF ( NXPTS .EQ. 1 ) THEN
!
!        The boundary ray intersects the plane of the ellipse in a
!        single point. Decide whether this point is inside the ellipse.
!        To test for containment, find the "coordinates" of the
!        center-to-point vector relative to the two-dimensional basis
!        formed by the semi-axes of the ellipse.  Call this
!        center-to-point vector OFFSET.  Recall A and B are the
!        semi-axis lengths of the ellipse. Let X and Y be the
!        coordinates of OFFSET in the two-dimensional reference frame
!        whose basis consists of normalized versions of SMAJOR and
!        SMINOR.
!
!        Note that we could have the special case in which the vertex
!        of the pyramid lies in the plane of the ellipse, in which case
!        the FOV "sees" the ellipse edge-on.  However, since NXPTS is
!        not INF, the boundary vector does not lie in the plane of the
!        ellipse.  So in this special case, APEX would be in the region
!        bounded by the ellipse.
!
         CALL VSUB ( XPT, CENTER, OFFSET )

         X  =  VDOT  ( OFFSET, SMAJOR ) / A
         Y  =  VDOT  ( OFFSET, SMINOR ) / B

         IF (  ( (X/A)**2.D0  +  (Y/B)**2.D0 ) .LE.  1.D0  ) THEN
!
!           The boundary-vector-plane intercept lies in the
!           topologically closed region bounded by the ellipse.
!
            FOUND = .TRUE.

            CALL CHKOUT ( 'ZZELVUPY' )
            RETURN

         END IF

      END IF

!
!     Check whether one of the pyramid's sides intersects the ellipse.
!     For each side, we first test whether the plane containing that
!     side intersects the ellipse.  If it does, the intersection is
!     a (possibly degenerate) line segment with endpoints on the
!     ellipse.  The triangle (or segment) defined by the pyramid's
!     apex and this segment (point) is then checked for intersection
!     with the currently considered side of the pyramid.
!
      I  =  1

      DO WHILE (  ( I .LE. N ) .AND. ( .NOT. FOUND )  )
!
!        Create a SPICELIB plane containing the Ith side of the
!        pyramid.
!
         IF ( I .LT. N ) THEN
            J  = I + 1
         ELSE
            J  = 1
         END IF

!
!        Although PSV2PL can signal an error if the spanning
!        vectors are linearly dependent, it won't do so here
!        because we've already ensured the cross product of
!        these vectors is non-zero.
!
         CALL PSV2PL ( APEX, BOUNDS(1,I), BOUNDS(1,J), PLANE )
!
!        Find the intersection of the plane and the ellipse.
!
         CALL INELPL ( ELLSCL, PLANE, NXPTS, XPT1, XPT2 )

!
!        If the ellipse-plane intersection is non-empty, test it to see
!        whether it has non-empty intersection with the current side of
!        the pyramid.
!
         IF ( NXPTS .GT. 0 ) THEN
!
!           Let EDGE1 and EDGE2 be the unit length boundary vectors
!           forming the edges of the currently considered side of the
!           pyramid.
!
            CALL VHAT ( BOUNDS(1,I), EDGE1 )
            CALL VHAT ( BOUNDS(1,J), EDGE2 )

!
!           Let EBSCTR ("pyramid edge bisector") be a bisector of the
!           sector bounded by EDGE1 and EDGE2.
!
            CALL VLCOM ( 0.5D0, EDGE1, 0.5D0, EDGE2, EBSCTR )

!
!           Let HAFEDG be half of the angular measure of this sector.
!
            HAFEDG = VSEP ( EDGE1, EDGE2 ) / 2.D0

!
!           Let VXPT1 and VXPT2 be the unit vectors pointing from the
!           pyramid's apex to the points of intersection of the ellipse
!           and the plane containing the currently considered side of
!           the pyramid.
!
            CALL VSUB ( XPT1,  APEX,  VTEMP )
            CALL VHAT ( VTEMP,        VXPT1 )

            CALL VSUB ( XPT2,  APEX,  VTEMP )
            CALL VHAT ( VTEMP,        VXPT2 )

!
!           At this point we'll introduce a bit of terminology. We're
!           going to work with plane regions defined by pairs of
!           vectors with a common endpoint.  We'll abuse standard
!           terminology a bit and call the region bounded by such a
!           vector pair a "sector."  Strictly speaking, sectors refer
!           only to subsets of a disc.
!
!           When it's convenient, we'll also identify "sectors" with
!           regions of the unit circle.  This will make it possible
!           to talk about intersections of sectors in terms of
!           intersections of the associated arcs on the unit circle.
!           By the "endpoints" of a sector we mean the endpoints
!           of the arc associated with the sector on the unit circle.
!
!           Let VBSCTR ("VXPT bisector") be a bisector of the sector
!           bounded by VXPT1 and VXPT2.
!
            CALL VLCOM ( 0.5D0, VXPT1, 0.5D0, VXPT2, VBSCTR )

!
!           Let HAFSEC be half of the angular measure of the sector
!           bounded by VXPT1 and VXPT2.
!
            HAFSEC = VSEP ( VXPT1, VXPT2 ) / 2.D0

!
!           EDGE1, EDGE2, VXPT1, and VXPT2 are four co-planar vectors
!           emanating from APEX.  We want to find out whether the
!           sector bounded by EDGE1 and EDGE2 intersects the sector
!           bounded by VXPT1 and VXPT2.  If there's an intersection, at
!           least one endpoint of one sector is contained in the other
!           sector.
!
!           Because of potential round-off problems when the sectors
!           are nearly coincident, we perform the precautionary check
!           (case 3) on the angle bisector of the sector defined by
!           VXPT1 and VXPT2.
!
!           If the sector defined by VXPT1 and VXPT2 has no endpoint
!           contained in the other sector, it's possible that the
!           former sector contains the latter.  In that case the
!           angular bisector of the latter sector is contained in the
!           former (case 4).
!
!           We test a vector's containment in a sector by comparing the
!           vector's angular separation from the sector's angle
!           bisector to one-half of the angular measure of the sector.
!
!              Case 1:  VXPT1 lies between EDGE1 and EDGE2.
!              Case 2:  VXPT2 lies between EDGE1 and EDGE2.
!              Case 3:  VBSCTR lies between EDGE1 and EDGE2.
!              Case 4:  EBSCTR lies between VXPT1 and VXPT2.
!
            IF      ( VSEP(VXPT1,  EBSCTR) .LE. HAFEDG ) THEN

               FOUND = .TRUE.

            ELSE IF ( VSEP(VXPT2,  EBSCTR) .LE. HAFEDG ) THEN

               FOUND = .TRUE.

            ELSE IF ( VSEP(VBSCTR, EBSCTR) .LE. HAFEDG ) THEN

               FOUND = .TRUE.

            ELSE IF ( VSEP(EBSCTR, VBSCTR) .LE. HAFSEC ) THEN

               FOUND = .TRUE.

            END IF

            IF ( FOUND ) THEN
!
!              We've found an intersection.  We're done.
!
               CALL CHKOUT ( 'ZZELVUPY' )
               RETURN

            END IF

         END IF

!
!        If no intersection was found, look at the next side of the
!        pyramid.
!
         I = I + 1

      END DO

!
!     If we got this far, the ellipse is not in view.  FOUND has
!     already been set to .FALSE.
!

      CALL CHKOUT ( 'ZZELVUPY' )
      RETURN
      END


