!$Procedure INCNSG ( Intersection of cone and line segment )

      subroutine incnsg ( apex,   axis,  angle, endpt1, &
                          endpt2, nxpts, xpt1,  xpt2   )
      use pepper_kinds, only: ip, wp

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

      implicit none

      real(wp)              apex   ( 3 )
      real(wp)              axis   ( 3 )
      real(wp)              angle
      real(wp)              endpt1 ( 3 )
      real(wp)              endpt2 ( 3 )
      integer(ip)           nxpts
      real(wp)              xpt1   ( 3 )
      real(wp)              xpt2   ( 3 )

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
!        Example code begins here.
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
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
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
      real(wp)              halfpi
      real(wp)              pi
      real(wp)              vdist
      real(wp)              vdot

      logical               failed
      logical               return

!
!     Local parameters
!
      real(wp), parameter :: costol = 1.0e-10_wp

      real(wp), parameter :: hsptol = 1.0e-14_wp

      integer(ip), parameter :: ubpl = 4

!
!     Local variables
!
      real(wp)              a
      real(wp)              axmag
      real(wp)              b
      real(wp)              c
      real(wp)              ca2
      real(wp)              colat
      real(wp)              cosang
      real(wp)              coserr
      real(wp)              dir    ( 3 )
      real(wp)              dmag
      real(wp)              dp1
      real(wp)              dp2
      real(wp)              locang
      real(wp)              maxlat
      real(wp)              maxp   ( 3 )
      real(wp)              minlat
      real(wp)              minp   ( 3 )
      real(wp)              nrmpln ( ubpl )
      real(wp)              off1   ( 3 )
      real(wp)              off2   ( 3 )
      real(wp)              origin ( 3 )
      real(wp)              plnx   ( 3 )
      real(wp)              s1
      real(wp)              s2
      real(wp)              uaxis  ( 3 )
      real(wp)              udir   ( 3 )
      real(wp)              uoff1  ( 3 )
      real(wp)              uoff2  ( 3 )
      real(wp)              uuax
      real(wp)              uv1    ( 3 )
      real(wp)              uv2    ( 3 )
      real(wp)              v1     ( 3 )
      real(wp)              v1mag
      real(wp)              v2     ( 3 )
      real(wp)              v2mag
      real(wp)              vtemp  ( 3 )
      real(wp)              vtemp2 ( 3 )
      real(wp)              w2
      real(wp)              wu
      real(wp)              wuax
      real(wp)              x      ( 3 )
      real(wp)              xoff1  ( 3 )
      real(wp)              xoff2  ( 3 )
      real(wp)              xform  ( 3, 3 )
      real(wp)              y      ( 3 )
      real(wp)              z      ( 3 )

      integer(ip)           i
      integer(ip)           n
      integer(ip)           nplnx

      logical               in1
      logical               in2
      logical               isbrck
      logical               neg1
      logical               neg2

!
!     Saved values
!
      save                  origin
      save                  y
      save                  z

!
!     Initial values
!
      data                  origin / 3 * 0.0_wp         /
      data                  y      / 0.0_wp, 1.0_wp, 0.0_wp /
      data                  z      / 0.0_wp, 0.0_wp, 1.0_wp /

!
!     Use quasi- We'll check in before
!     code sections that can generate SPICE errors, and check
!     out afterward. When those code sections are skipped,
!     we avoid traceback participation.
!
      if ( return() ) return

!
!     No intersection was found so far.
!
      nxpts = 0

!
!     The cone's axis vector must be non-zero.
!
      call unorm ( axis, uaxis, axmag )

      if ( axmag == 0.0_wp ) then

         call setmsg ( 'The cone''s axis vector must be non-zero ' &
         //            'but sadly, it failed to meet this criterion.' )
         call sigerr ( 'SPICE(ZEROVECTOR)'                            )
         return

      end if

!
!     The cone's angular radius must be non-negative.
!
      if ( angle < 0.0_wp ) then

         call setmsg ( 'The cone''s angular radius must be ' &
         //            ' non-negative but was # (radians).' )
         call errdp  ( '#', angle                           )
         call sigerr ( 'SPICE(INVALIDANGLE)'                )
         return

      end if

!
!     The endpoints of the segment must be distinct. Check this after
!     computing a unit direction vector for the line segment.
!
      call vsub  ( endpt2, endpt1, dir )

      call unorm ( dir, udir, dmag )

      if ( dmag == 0.0_wp ) then

         call setmsg ( 'The distance between the segment''s ' &
         //            'endpoints was zero. First endpoint: ' &
         //            '(# # #).'                            )
         call errdp  ( '#', endpt1(1)                        )
         call errdp  ( '#', endpt1(2)                        )
         call errdp  ( '#', endpt1(3)                        )
         call sigerr ( 'SPICE(ENDPOINTSMATCH)'               )
         return

      end if

!
!     Store the cosine of the cone's angular radius. We'll treat all
!     cases with COSANG equal to 0 as though the cone is actually a
!     plane normal to the axis and containing the apex.
!
      cosang = cos( angle )
      locang = angle
!
!     We'll work with a local axis that has angular separation of
!     no more than pi/2 from the nappe.
!
      if ( cosang < 0.0_wp ) then

         cosang   = - cosang
         locang   =   pi() - angle

         uaxis(1) = - uaxis(1)
         uaxis(2) = - uaxis(2)
         uaxis(3) = - uaxis(3)

      end if

!
!     Compute the offsets of the endpoints of the segment from
!     the cone's apex.
!
      call vsub ( endpt1, apex, off1 )
      call vsub ( endpt2, apex, off2 )

!
!     Deal with some of the simple cases first.
!
      call vhat ( off1, uoff1 )
      call vhat ( off2, uoff2 )

      dp1  = vdot( uoff1, uaxis )
      dp2  = vdot( uoff2, uaxis )

!
!     The given axis is inside the nappe defined by the angular radius.
!
!     There's no intersection if both endpoints are in the interior of
!     the nappe of the cone (since the nappe is convex).
!
      in1  = dp1 >= cosang
      in2  = dp2 >= cosang
!
!     If the line segment lies on the far side of the plane that
!     contains the apex and is orthogonal to the axis, there's no
!     intersection.
!
      neg1 = dp1 < 0.0_wp
      neg2 = dp2 < 0.0_wp

      if (      ( in1  .and. in2  ) &
           .or. ( neg1 .and. neg2 ) ) then
!
!        The segment is in the interior of the cone or
!        on the far side of the plane.
!
         nxpts = 0

         return

      end if

!
!     Here's where we handle the half-space case.
!
      if ( abs(cosang) < hsptol ) then
!
!        See whether the ray emanating from the first endpoint and
!        having direction UDIR hits the plane normal to the axis and
!        containing the apex. We'll call this plane NRMPLN.
!
!        NVP2PL can signal an error only if the input axis is the
!        zero vector. We've ensured that it isn't.
!
         call nvp2pl ( uaxis,  apex, nrmpln )
         call inrypl ( endpt1, udir, nrmpln, nplnx, plnx )

!
!        If the ray doesn't hit the plane, we're done. Otherwise,
!        check the intercept.
!
         if ( nplnx == 1 ) then
!
!           The ray does hit the plane. If the intersection is on the
!           line segment, we have a solution.
!
            if ( vdist(plnx, endpt1) <= dmag ) then
!
!              The intercept is not further along the ray than the
!              second endpoint. It's a valid solution.
!
               nxpts = 1
               call vequ ( plnx, xpt1 )

            end if

         end if
!
!        This is the end of the half-space case.
!
         return

      end if

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
!         =  0                                                       (7)
!
!      Before continuing, we observe that the only non-unit vector
!      in (7) is W. So the coefficients in (7) have no possibility
!      of overflowing unless the vertex of the ray is very far from
!      the apex of the cone.
!
!      W has been computed above as OFF1.
!
!         [ Consider adding check on OFF1 here. ]
!
!     Intermediate values:
!
      uuax = vdot( udir, uaxis )
      wuax = vdot( off1, uaxis )
      wu   = vdot( off1, udir  )
      w2   = vdot( off1, off1  )
      ca2  = cosang * cosang

!
!     Quadratic coefficients:
!
      a    = ( uuax * uuax ) - ca2

      b    = 2 * (  ( wuax * uuax ) - ( ca2 * wu )  )

      c    = ( wuax * wuax ) - ( w2 * ca2 )

!
!     We're not interested in solutions that lie outside
!     of the line segment. The length of the segment is
!     DMAG.
!
!     Solve the equation, using DMAG as an upper bound
!     on the magnitude of the roots.
!
      call zzcnquad ( a, b, c, dmag, n, s1, s2 )

!
!     Compute the possible intersection points and test them
!     to make sure they really are solutions.
!
      if ( n > 0 ) then
!
!        Start with the solution closest to the ray's vertex.
!        Compute XPT1 and make sure it's on the correct nappe
!        of the cone.
!
         if ( s1 >= 0.0_wp ) then

            xpt1(1) = endpt1(1) + s1*udir(1)
            xpt1(2) = endpt1(2) + s1*udir(2)
            xpt1(3) = endpt1(3) + s1*udir(3)

            call vsub  ( xpt1, apex, v1 )

!
!           See whether V1 is on the cone.
!
            call unorm ( v1, uv1, v1mag )

            if ( v1mag > 0.0_wp ) then

               coserr = abs( vdot(uv1,uaxis) - cosang )
            else
               coserr = 0.0_wp
            end if

            if (      ( v1mag  == 0.0_wp   ) &
                 .or. ( coserr < costol ) ) then
!
!              The root is on the cone (on the apex if V1MAG is zero).
!
!              We accept this root. Update NXPTS. Note that this is
!              not necessarily the final value of NXPTS; that
!              depends on the validity of the second root.

               nxpts = 1

            end if

         end if

         if ( n == 2 ) then
!
!           Check the second root.
!
            if ( s2 >= 0.0_wp ) then

               xpt2(1) = endpt1(1) + s2*udir(1)
               xpt2(2) = endpt1(2) + s2*udir(2)
               xpt2(3) = endpt1(3) + s2*udir(3)

               call vsub  ( xpt2, apex, v2 )

!
!              See whether V2 is on the cone.
!
               call unorm ( v2, uv2, v2mag )

               if ( v2mag > 0.0_wp ) then

                  coserr = abs( vdot(uv2,uaxis) - cosang )
               else
                  coserr = 0.0_wp
               end if

               if (      ( v2mag  == 0.0_wp   ) &
                    .or. ( coserr < costol ) ) then
!
!                 The root is on the cone (on the apex if V2MAG is
!                 zero).
!
!                 We accept this root.
!
                  nxpts = nxpts + 1

                  if ( nxpts == 1 ) then
!
!                    This is the only valid root; overwrite XPT1.
!
                     call vequ ( xpt2, xpt1 )

                  end if

               end if

            end if

         end if

      end if

!
!     We're not done yet. If we have fewer roots than we should, we'll
!     need to solve the problem by an alternate method.
!
!     If we have two roots, we're in good shape. Otherwise we must
!     determine how many roots should be found.
!
      if ( nxpts < 2 ) then
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
         if (     ( in1 .and. ( .not. in2 ) ) &
             .or. ( in2 .and. ( .not. in1 ) )  ) then
!
!           There's supposed to be one root. If we found none, find one
!           now.
!
            if ( nxpts == 0 ) then
!
!              ZZCXBRUT signals an error if the axis is the zero
!              vector, but not otherwise. We've already ruled out this
!              situation. Therefore, we don't check in before the
!              following call.
!
               call zzcxbrut ( apex,   uaxis,  locang, &
                               endpt1, endpt2, xpt1,  isbrck )

               if ( isbrck ) then
!
!                 As long as the root was bracketed, XPT1 is a
!                 solution.
!
                  nxpts = 1

               end if

            end if

         else

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
            nxpts = 0

            call frame ( uaxis, x, y )

            do i = 1, 3

               xform(1,i) = x(i)
               xform(2,i) = y(i)
               xform(3,i) = uaxis(i)

            end do

            call mxv ( xform, off1, xoff1 )
            call mxv ( xform, off2, xoff2 )

            call zzsglatx ( xoff1, xoff2, minlat, minp, maxlat, maxp )

            if ( failed() ) return

!
!           COLAT is the colatitude of the point of maximum latitude.
!
            colat = halfpi() - maxlat

            if ( colat < locang ) then
!
!              MAXP is inside the cone. There should be an intersection
!              on the segment between XOFF1 and MAXP and another
!              between MAXP and XOFF2.
!
               call zzcxbrut ( origin, z,    locang, &
                               xoff1,  maxp, vtemp,  isbrck )

               if ( isbrck ) then
!
!                 Convert VTEMP to the original frame, then translate
!                 it so that it's represented as an offset from the
!                 origin.
!
                  call mtxv ( xform,  vtemp, vtemp2 )
                  call vadd ( vtemp2, apex,  xpt1   )

                  nxpts = 1

               end if

               call zzcxbrut ( origin, z,     locang, &
                               maxp,   xoff2, vtemp,  isbrck )

               if ( isbrck ) then
!
!                 Convert VTEMP to the original frame, then translate
!                 it so that it's represented as an offset from the
!                 origin.
!
                  call mtxv ( xform,  vtemp, vtemp2 )
                  call vadd ( vtemp2, apex,  xpt2   )

                  if ( nxpts == 1 ) then
!
!                    Both roots are valid.
!
                     nxpts = 2

                  else
!
!                    The second root is the only valid root. Move it
!                    into XPT1.
!
                     call vequ ( xpt2, xpt1 )

                     nxpts = 1

                  end if

               end if

            else if ( colat == locang ) then
!
!              The root corresponds to a point of tangency of
!              the segment and cone. This occurs at the point
!              having maximum latitude: MAXP.
!
               call vequ ( maxp, xpt1 )

               nxpts = 1

!
!           Note that if COLAT > LOCANG, there are no roots.
!
            end if

         end if
!
!        This is the end of portion of the "brute force" branch in
!        which both endpoints are outside the cone.
!
      end if

!
!     NXPTS  has been set.
!
!     If NXPTS is 1, then XPT1 is set.
!
!     If NXPTS is 2, then both XPT1 and XPT2 are set.
!

      return
      end
