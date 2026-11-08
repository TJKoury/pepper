!$Procedure      ZZELVUPY ( Is ellipse in polygonal field of view? )

      subroutine zzelvupy ( ellips, vertex, axis, n, bounds, found )
      use pepper_kinds, only: ip, wp
      implicit none

! $ Abstract
!
!     Determine whether a specified ellipse intersects the pyramid
!     defined by a polygonal field of view.
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

      include 'errhnd.inc'

      integer(ip)           ubel
      parameter           ( ubel = 9 )

      integer(ip)           ubpl
      parameter           ( ubpl = 4 )

      integer(ip)           maxfov
      parameter           ( maxfov = 10000 )

      real(wp)              ellips ( ubel )
      real(wp)              vertex ( 3 )
      real(wp)              axis   ( 3 )
      integer(ip)           n
      real(wp)              bounds ( 3, n )
      logical               found

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
      real(wp)              pi
      real(wp)              vdist
      real(wp)              vdot
      real(wp)              vnorm
      real(wp)              vsep

      integer(ip)           zzwind

      logical               return
      logical               vzero

!
!     Local parameters
!

!
!     Local variables
!
      character(lmsgln)     errmsg

      real(wp)              a
      real(wp)              apex   ( 3 )
      real(wp)              asep
      real(wp)              b
      real(wp)              center ( 3 )
      real(wp)              consep
      real(wp)              cp     ( 3 )
      real(wp)              ctrvec ( 3 )
      real(wp)              d
      real(wp)              easize
      real(wp)              ebsctr ( 3 )
      real(wp)              edge1  ( 3 )
      real(wp)              edge2  ( 3 )
      real(wp)              ellscl ( ubel )
      real(wp)              eplane ( ubpl )
      real(wp)              fovpln ( ubpl )
      real(wp)              gv1    ( 3 )
      real(wp)              gv2    ( 3 )
      real(wp)              hafedg
      real(wp)              hafsec
      real(wp)              offset ( 3 )
      real(wp)              origin ( 3 )
      real(wp)              pasize
      real(wp)              plane  ( ubpl )
      real(wp)              scale
      real(wp)              smajor ( 3 )
      real(wp)              sminor ( 3 )
      real(wp)              vbsctr ( 3 )
      real(wp)              vtemp  ( 3 )
      real(wp)              vxpt1  ( 3 )
      real(wp)              vxpt2  ( 3 )
      real(wp)              work   ( 3, maxfov )
      real(wp)              x
      real(wp)              xpt    ( 3 )
      real(wp)              xpt1   ( 3 )
      real(wp)              xpt2   ( 3 )
      real(wp)              y

      integer(ip)           i
      integer(ip)           j
      integer(ip)           nxpts


!
!     Saved variables
!
      save                 origin
      save                 work

!
!     Initial values
!
      data                 origin /  3 * 0.0_wp /


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZELVUPY' )

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
      found = .false.

!
!     Validate the ellipse.  First find the center and the semi-axes
!     of the ellipse.
!
      call el2cgv ( ellips, center, gv1,    gv2    )
      call saelgv ( gv1,    gv2,    smajor, sminor )

!
!     Check the semi-axis lengths.
!
!     If the semi-major axis is the zero vector, we'd expect
!     the semi-minor axis to be the zero vector as well.  But
!     round-off error could conceivably violate this assumption.
!
      if ( vzero(smajor) .or. vzero(sminor) ) then

         call setmsg ( 'Input ellipse has semi-major axis '   // &
                       'length # and semi-minor axis length ' // &
                       '#.  Both vectors are required to be ' // &
                       'non-zero.'                            )
         call errdp  ( '#', vnorm(smajor)                     )
         call errdp  ( '#', vnorm(sminor)                     )
         call sigerr ( 'SPICE(ZEROVECTOR)'                    )
         call chkout ( 'ZZELVUPY'                             )
         return

      end if

!
!     Scale the vectors defining the ellipse and the vertex of the
!     pyramid so that the largest of these vectors has unit length.
!
      scale = 1.0_wp / max ( vnorm(center), vnorm(smajor), vnorm(vertex) )

      do i = 1, 3

         center(i) = scale * center(i)
         smajor(i) = scale * smajor(i)
         sminor(i) = scale * sminor(i)
         apex  (i) = scale * vertex(i)

      end do

!
!     Create a scaled ellipse.  We'll perform the FOV side-ellipse
!     intersection computations using this ellipse.
!
      call cgv2el ( center, smajor, sminor, ellscl )

!
!     After scaling, make sure the semi-axes have sufficient length to
!     prevent numerical problems.  Let A and B be the scaled semi-axis
!     lengths of the ellipse.
!
      a = vnorm ( smajor )
      b = vnorm ( sminor )

      if ( b == 0.0_wp ) then

         call setmsg ( 'Scaled ellipse''s semi-minor axis length = 0.' )
         call sigerr ( 'SPICE(DEGENERATECASE)'                         )
         call chkout ( 'ZZELVUPY'                                      )
         return

      end if

!
!     Validate the input pyramid.
!
!     The axis must not be the zero vector.
!
      if (  vzero(axis)  ) then

         call setmsg ( 'The pyramid''s axis the zero vector.' )
         call sigerr ( 'SPICE(ZEROVECTOR)'                    )
         call chkout ( 'ZZELVUPY'                             )
         return

      end if

!
!     There must be at least three boundary vectors.
!
      if ( n < 3 ) then

         call setmsg ( 'The number of boundary vectors was #; ' // &
                       'this number must be at least 3.'        )
         call errint ( '#',  n                                  )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                    )
         call chkout ( 'ZZELVUPY'                               )
         return

      end if

!
!     There must be no more than MAXFOV boundary vectors.
!
      if ( n > maxfov ) then

         call setmsg ( 'The number of boundary vectors was #; ' // &
                       'this number must not exceed #.'         )
         call errint ( '#',  n                                  )
         call errint ( '#',  maxfov                             )
         call sigerr ( 'SPICE(INVALIDCOUNT)'                    )
         call chkout ( 'ZZELVUPY'                               )
         return

      end if

!
!     We must initialize certain variables before continuing with
!     the checks.
!
!     Let CTRVEC be the vector from the apex to the center of the
!     ellipse.  This vector will be used in several places later;
!     it's convenient to compute it here.
!
      call vsub ( center, apex, ctrvec )

!
!     Compute PASIZE:  an upper bound on the angular radius of a
!     circular cone whose axis is the input central axis.  While
!     we're at it, check the angular separation of the boundary
!     vectors from the central axis and from each other.
!
      pasize = 0.0_wp

      do i = 1, n
!
!        Each boundary vector must have angular separation from the
!        axis of less than pi/2 radians.  Keep track of the maximum
!        angular separation PASIZE as we go.  We'll use this variable
!        later in a non-intersection test.
!
         asep = vsep ( axis, bounds(1,i) )

         if ( asep >= pi()/2  ) then

            call setmsg ( 'The angular separation of boundary '   // &
                          'vector # from the axis is #. This '    // &
                          'number must less than pi/2.'           )
            call errint ( '#',  i                                 )
            call errdp  ( '#',  asep                              )
            call sigerr ( 'SPICE(INVALIDFOV)'                     )
            call chkout ( 'ZZELVUPY'                              )
            return

         end if

         pasize = max ( pasize, asep )

!
!        Each boundary vector must have non-zero angular separation
!        from its neighbors.
!
         if ( i < n ) then
            j  = i + 1
         else
            j  = 1
         end if

         call ucrss ( bounds(1,i), bounds(1,j), cp )

         if ( vzero(cp) ) then
!
!           The cross product may be zero because one of the
!           boundary vectors is zero.  Check this first.
!
            if (  vzero( bounds(1,j) ) .or. vzero( bounds(1,i) ) ) then

               errmsg = 'The # boundary vector is the zero vector.'

               if (  vzero( bounds(1,i) )  ) then
                  j = i
               end if

               call repmot ( errmsg, '#', j, 'L', errmsg )
               call setmsg ( errmsg                      )
               call sigerr ( 'SPICE(ZEROVECTOR)'         )

            else

               call setmsg ( 'The angular separation of boundary '   // &
                             'vector # from vector # is 0.'          // &
                             'This number must be positive.'         )
               call errint ( '#',  i                                 )
               call errint ( '#',  j                                 )
               call sigerr ( 'SPICE(INVALIDFOV)'                     )

            end if

            call chkout ( 'ZZELVUPY' )
            return

         end if

      end do

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
      d  =  vdist ( center, apex )

      if ( a < d ) then

         easize  =  asin ( a / d )

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
         consep = vsep ( ctrvec, axis )  -  ( easize + pasize )

         if ( consep > 0.0_wp ) then

            call chkout ( 'ZZELVUPY' )
            return
         end if

      end if

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
      if (  vdot( axis, ctrvec ) > 0.0_wp ) then
!
!        Construct the plane and find the polygon.
!
         call nvp2pl ( axis, ctrvec, fovpln )

!
!        Create the planar FOV boundary using the intersections
!        of the FOV boundary vectors with FOVPLN.
!
         do i = 1, n

            call inrypl ( origin,  bounds(1,i),  fovpln, &
                          nxpts,   work  (1,i)           )

!
!           We expect to have a single point of intersection for each
!           boundary vector.
!
            if ( nxpts /= 1 ) then

               call setmsg ( 'NXPTS = # for boundary vector #/FOV ' // &
                             'plane intersection.'                  )
               call errint ( '#',  nxpts                            )
               call errint ( '#',  i                                )
               call sigerr ( 'SPICE(BUG)'                           )
               call chkout ( 'ZZELVUPY'                             )
               return

            end if

         end do

!
!        Now WORK contains the polygon representing the intersection of
!        the pyramid with the plane FOVPLN. If the winding number of
!        the polygon about the ellipse center is non-zero, we conclude
!        the center is in the pyramid.
!
         if (   zzwind( fovpln, n, work, ctrvec )  /=  0 ) then
!
!           The center of the ellipse is inside the pyramid.  We're
!           done.
!
            found = .true.

            call chkout ( 'ZZELVUPY' )
            return

         end if

      end if

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
      call psv2pl ( center,  smajor,       sminor,  eplane )

      call inrypl ( apex,    bounds(1,1),  eplane,  nxpts,  xpt )

!
!     The routine INRYPL can return the NXPTS values 1, 0, or INF---a
!     code indicating an infinite number of intersection points of ray
!     and plane.  If the value is 1, the boundary ray may intersect
!     the region bounded by the ellipse.
!
      if ( nxpts == 1 ) then
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
         call vsub ( xpt, center, offset )

         x  =  vdot  ( offset, smajor ) / a
         y  =  vdot  ( offset, sminor ) / b

         if (  ( (x/a)**2.0_wp  +  (y/b)**2.0_wp ) <=  1.0_wp  ) then
!
!           The boundary-vector-plane intercept lies in the
!           topologically closed region bounded by the ellipse.
!
            found = .true.

            call chkout ( 'ZZELVUPY' )
            return

         end if

      end if

!
!     Check whether one of the pyramid's sides intersects the ellipse.
!     For each side, we first test whether the plane containing that
!     side intersects the ellipse.  If it does, the intersection is
!     a (possibly degenerate) line segment with endpoints on the
!     ellipse.  The triangle (or segment) defined by the pyramid's
!     apex and this segment (point) is then checked for intersection
!     with the currently considered side of the pyramid.
!
      i  =  1

      do while (  ( i <= n ) .and. ( .not. found )  )
!
!        Create a SPICELIB plane containing the Ith side of the
!        pyramid.
!
         if ( i < n ) then
            j  = i + 1
         else
            j  = 1
         end if

!
!        Although PSV2PL can signal an error if the spanning
!        vectors are linearly dependent, it won't do so here
!        because we've already ensured the cross product of
!        these vectors is non-zero.
!
         call psv2pl ( apex, bounds(1,i), bounds(1,j), plane )
!
!        Find the intersection of the plane and the ellipse.
!
         call inelpl ( ellscl, plane, nxpts, xpt1, xpt2 )

!
!        If the ellipse-plane intersection is non-empty, test it to see
!        whether it has non-empty intersection with the current side of
!        the pyramid.
!
         if ( nxpts > 0 ) then
!
!           Let EDGE1 and EDGE2 be the unit length boundary vectors
!           forming the edges of the currently considered side of the
!           pyramid.
!
            call vhat ( bounds(1,i), edge1 )
            call vhat ( bounds(1,j), edge2 )

!
!           Let EBSCTR ("pyramid edge bisector") be a bisector of the
!           sector bounded by EDGE1 and EDGE2.
!
            call vlcom ( 0.5_wp, edge1, 0.5_wp, edge2, ebsctr )

!
!           Let HAFEDG be half of the angular measure of this sector.
!
            hafedg = vsep ( edge1, edge2 ) / 2.0_wp

!
!           Let VXPT1 and VXPT2 be the unit vectors pointing from the
!           pyramid's apex to the points of intersection of the ellipse
!           and the plane containing the currently considered side of
!           the pyramid.
!
            call vsub ( xpt1,  apex,  vtemp )
            call vhat ( vtemp,        vxpt1 )

            call vsub ( xpt2,  apex,  vtemp )
            call vhat ( vtemp,        vxpt2 )

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
            call vlcom ( 0.5_wp, vxpt1, 0.5_wp, vxpt2, vbsctr )

!
!           Let HAFSEC be half of the angular measure of the sector
!           bounded by VXPT1 and VXPT2.
!
            hafsec = vsep ( vxpt1, vxpt2 ) / 2.0_wp

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
            if      ( vsep(vxpt1,  ebsctr) <= hafedg ) then

               found = .true.

            else if ( vsep(vxpt2,  ebsctr) <= hafedg ) then

               found = .true.

            else if ( vsep(vbsctr, ebsctr) <= hafedg ) then

               found = .true.

            else if ( vsep(ebsctr, vbsctr) <= hafsec ) then

               found = .true.

            end if

            if ( found ) then
!
!              We've found an intersection.  We're done.
!
               call chkout ( 'ZZELVUPY' )
               return

            end if

         end if

!
!        If no intersection was found, look at the next side of the
!        pyramid.
!
         i = i + 1

      end do

!
!     If we got this far, the ellipse is not in view.  FOUND has
!     already been set to .FALSE.
!

      call chkout ( 'ZZELVUPY' )
      return
      end


