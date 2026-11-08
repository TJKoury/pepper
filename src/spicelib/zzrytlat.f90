!$Procedure ZZRYTLAT ( DSK, ray touches latitudinal element )

      subroutine zzrytlat ( vertex, raydir, bounds, &
                            margin, nxpts,  xpt    )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find nearest intersection to a given ray's vertex of that ray and
!     at latitudinal volume element. If the vertex is inside the
!     element, the vertex is considered to be the solution.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     GEOMETRY
!     INTERCEPT
!     INTERSECTION
!     RAY
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      include 'dsktol.inc'

      real(wp)              vertex ( 3 )
      real(wp)              raydir ( 3 )
      real(wp)              bounds ( 2, 3 )
      real(wp)              margin
      integer(ip)           nxpts
      real(wp)              xpt    ( 3 )

      integer(ip)           lonidx
      parameter           ( lonidx = 1 )

      integer(ip)           latidx
      parameter           ( latidx = 2 )

      integer(ip)           radidx
      parameter           ( radidx = 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     BOUNDS     I   Bounds of latitudinal volume element.
!     MARGIN     I   Margin used for element expansion.
!     NXPTS      O   Number of intercept points.
!     XPT        O   Intercept.
!     LONIDX     P   Longitude index.
!     LATIDX     P   Latitude index.
!     RADIDX     P   Radius index.
!
! $ Detailed_Input
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector of
!                the ray to be used in the intercept computation.
!
!                Both the vertex and ray direction must be represented
!                in the reference frame to which the volume element
!                boundaries correspond. The vertex is considered to be
!                an offset from the center of the reference frame
!                associated with the volume element.
!
!     BOUNDS     is a 2x3 array containing the bounds of a latitudinal
!                volume element. Normally this is the coverage boundary
!                of a DSK segment. In the element
!
!                   BOUNDS(I,J)
!
!                J is the coordinate index. J is one of
!
!                   { LONIDX, LATIDX, RADIDX }
!
!                I is the bound index.
!
!                   I = 1   ->   lower bound
!                   I = 2   ->   upper bound
!
!                If the longitude upper bound is not greater than the
!                longitude lower bound, a value greater than the upper
!                bound by 2*pi is used for the comparison.
!
!
!     MARGIN     is a scale factor used to effectively expand the
!                segment boundaries so as to include intersections
!                that lie slightly outside the volume element.
!
!
! $ Detailed_Output
!
!     XPT        is the intercept of the ray on the surface described
!                by the input coordinate bounds, if such an intercept
!                exists. If the ray intersects the surface at multiple
!                points, the one closest to the ray's vertex is
!                selected. XPT is valid if and only if FOUND is .TRUE.
!
!                XPT is expressed in the reference frame associated
!                with the inputs VERTEX and RAYDIR. XPT represents
!                an offset from the origin of the coordinate system.
!
!                XPT is valid only if NXPTS is set to 1.
!
!
!     NXPTS      is the number of intercept points of the ray and
!                the volume element.
!
!                Currently there are only two possible values for
!                NXPTS:
!
!                   1 for an intersection
!                   0 for no intersection
!
!                If the vertex is inside the element, NXPTS is
!                set to 1.
!
! $ Parameters
!
!
!     LONIDX     is the index of longitude in the second dimension of
!                BOUNDS.
!
!     LATIDX     is the index of latitude in the second dimension of
!                BOUNDS.
!
!     RADIDX     is the index of radius in the second dimension of
!                BOUNDS.
! $ Exceptions
!
!     For the sake of run-time efficiency, this routine does not
!     participate in call tracing. However, it's possible for
!     routines called by this routine to signal errors.
!
! $ Files
!
!     None. However, the input volume element boundaries normally have
!     been obtained from a segment in a loaded DSK file.
!
! $ Particulars
!
!     This routine sits on top of data DSK type-specific ray-segment
!     intercept routines such as DSKX02.
!
! $ Examples
!
!     See usage in ZZDSKBUX.
!
! $ Restrictions
!
!     This is a private routine. It is meant to be used only by the DSK
!     subsystem.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 18-JAN-2017 (NJB)
!
!        Based on ZZRYXLAT 20-MAR-2015 (NJB)
!
!        Original 01-OCT-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     find intercept of ray on latitudinal volume element
!
!-&



!
!     SPICELIB functions
!
      real(wp)              dpmax
      real(wp)              halfpi
      real(wp)              vdist
      real(wp)              vdot
      real(wp)              vnorm

      logical               failed

!
!     Local parameters
!

!
!     Radius expansion factor:
!
      real(wp)              radfac
      parameter           ( radfac = 1.1_wp )


!
!     Element boundary indices:
!
      integer(ip)           west
      parameter           ( west   = 1 )

      integer(ip)           east
      parameter           ( east   = 2 )

      integer(ip)           south
      parameter           ( south  = 1 )

      integer(ip)           north
      parameter           ( north  = 2 )

      integer(ip)           inner
      parameter           ( inner  = 1 )

      integer(ip)           outer
      parameter           ( outer  = 2 )


!
!     Local variables
!
      real(wp)              angle
      real(wp)              dist
      real(wp)              eastb  ( 3 )
      real(wp)              eback  ( 3 )
      real(wp)              endpt2 ( 3 )
      real(wp)              loncov
      real(wp)              maxlat
      real(wp)              maxlon
      real(wp)              maxr
      real(wp)              minlat
      real(wp)              minlon
      real(wp)              minr
      real(wp)              mndist
      real(wp)              negdir ( 3 )
      real(wp)              origin ( 3 )
      real(wp)              s
      real(wp)              srfx   ( 3 )
      real(wp)              udir   ( 3 )
      real(wp)              vlat
      real(wp)              vlon
      real(wp)              vr
      real(wp)              wback  ( 3 )
      real(wp)              westb  ( 3 )
      real(wp)              xpt2   ( 3 )
      real(wp)              z      ( 3 )

      integer(ip)           nx

      logical               found
      logical               inside
      logical               xin

!
!     Saved variables
!
      save                  origin
      save                  z

!
!     Initial values
!
      data                  origin / 3 *   0.0_wp /
      data                  z      / 0.0_wp, 0.0_wp, 1.0_wp /


!
!     CAUTION: this routine doesn't check in, so it won't
!     appear in the traceback if a lower-level routine
!     signals an error.
!

!
!     Determine whether the vertex is inside the element.
!     Use double the margin for this test, since we don't
!     want to have false negative tests for rays having
!     vertices lying on the expanded element boundary.
!
      call zzinlat ( vertex, bounds, 2*margin, 0, inside )

      if ( failed() ) return

      if ( inside ) then
!
!        We know the answer.
!
         nxpts = 1

         call vequ ( vertex, xpt )

         return

      end if

!
!     Extract the segment's coordinate bounds into easily
!     readable variables.
!
      minr = bounds( inner, radidx )
      maxr = bounds( outer, radidx )
!
!     Normalize the longitude bounds. After this step, the bounds will
!     be in order and differ by no more than 2*pi.
!
      call zznrmlon( bounds(west,lonidx), bounds(east,lonidx), angmrg, &
                     minlon,              maxlon                       )

      if ( failed() ) return

      minlat = bounds( south, latidx )
      maxlat = bounds( north, latidx )

!
!     The vertex is outside the element.
!
!     Indicate no intersection to start.
!
      nxpts  = 0
!
!     We'll use a unit length copy of the ray's direction vector.
!
      call vhat ( raydir, udir )
!
!     Initialize the distance to the closest solution. We'll keep track
!     of this quantity in order to compare competing solutions.
!
      mndist = dpmax()
!
!     Find the intersection of the ray and outer bounding sphere, if
!     possible. Often this intersection is the closest to the vertex.
!     If the intersection exists and is on the boundary of the element,
!     it's a winner.
!
      call zzryxsph ( vertex, udir, maxr, srfx, found )

      if ( .not. found ) then
!
!        There are no intersections. The ray cannot hit the
!        volume element.
!
         return

      end if
!
!     Get the latitudinal coordinates of the ray's vertex.
!
      call reclat ( vertex, vr, vlon, vlat )

!
!     The ray hits the outer bounding sphere. See whether
!     the longitude and latitude are within bounds, taking
!     the margin into account. Exclude the radius coordinate
!     from testing.
!
      call zzinlat ( srfx, bounds, margin, radidx, xin )

      if ( failed() ) return


      if ( xin ) then
!
!        This solution is a candidate.
!
         call vequ ( srfx, xpt )
         nxpts  = 1

         if ( vr > maxr ) then
!
!           The vertex is outside this sphere, and the segment
!           lies within the sphere.
!
!           No other intersection can be closer to the vertex;
!           we don't need to check the other surfaces.
!
            return

         else
!
!           We have a possible solution.
!
            mndist = vdist( vertex, xpt )

         end if

      end if

!
!     So far there may be a candidate solution. We'll try the latitude
!     boundaries next.
!
!     For testing intersections with the latitude boundaries, we'll
!     need a far endpoint for the line segment on which to perform the
!     test.
!
      s = vnorm(vertex) + radfac * maxr

      call vlcom ( 1.0_wp, vertex, s, udir, endpt2 )

!
!     Now try the upper latitude bound. We can skip this test
!     if the upper bound is pi/2 radians.
!
      if ( maxlat < halfpi() ) then
!
!        Let ANGLE be the angular separation of the surface of latitude
!        MAXLAT and the +Z axis. Note that the surface might be the
!        lower nappe of the cone.
!
         angle = max ( 0.0_wp,  halfpi() - maxlat )

         call incnsg ( origin, z,  angle, vertex, &
                       endpt2, nx, srfx,  xpt2    )

         if ( failed() ) return

         if ( nx >= 1 ) then
!
!           See whether SRFX is in the element.
!
            call zzinlat ( srfx, bounds, margin, latidx, xin )

            if ( failed() ) return

            if ( xin ) then
!
!              SRFX is a candidate solution.
!
               dist = vdist( vertex, srfx )

               if ( dist < mndist ) then

                  call vequ ( srfx, xpt )

                  nxpts  = 1

                  if ( vlat > maxlat ) then
!
!                    Since the latitude of the vertex is greater than
!                    MAXLAT, this is the best solution, since the
!                    volume element is on the other side of the maximum
!                    latitude boundary.
!
                     return

                  end if
!
!                 This is the best solution seen so far.
!
                  mndist = dist

               end if

            end if

            if ( nx == 2 ) then
!
!              Check the second solution as well.
!
               call zzinlat ( xpt2, bounds, margin, latidx, xin )

               if ( failed() ) return


               if ( xin ) then
!
!                 XPT2 is a candidate solution.
!
                  dist = vdist( vertex, xpt2 )

                  if ( dist < mndist ) then

                     call vequ ( xpt2, xpt )

                     nxpts  = 1
                     mndist = dist
!
!                    This is the best solution seen so far.
!                    However, it's not necessarily the best
!                    solution. So we continue.
!
                  end if

               end if

            end if
!
!           We've handled the second root, if any.
!
         end if
!
!        We're done with the upper latitude boundary.
!
      end if

!
!     Try the lower latitude bound. We can skip this test if the lower
!     bound is -pi/2 radians.
!
      if (  minlat > -halfpi() ) then
!
!        Let ANGLE be the angular separation of the surface
!        of latitude MINLAT and the +Z axis. Note that the
!        surface might be the lower nappe of the cone.
!
         angle = halfpi() - minlat

         call incnsg ( origin, z,  angle, vertex, &
                       endpt2, nx, srfx,  xpt2    )

         if ( failed() ) return


         if ( nx >= 1 ) then
!
!           See whether SRFX is in the element.
!
            call zzinlat ( srfx, bounds, margin, latidx, xin )

            if ( failed() ) return

            if ( xin ) then
!
!              SRFX is a candidate solution.
!
               dist = vdist( vertex, srfx )

               if ( dist < mndist ) then

                  call vequ ( srfx, xpt )

                  nxpts  = 1

                  if ( vlat < minlat ) then
!
!                    Since the latitude of the vertex is less than
!                    MINLAT, this is the best solution, since the
!                    volume element is on the other side of the minimum
!                    latitude boundary.
!
                     return

                  end if
!
!                 This is the best solution seen so far.
!
                  mndist = dist

               end if

            end if

            if ( nx == 2 ) then
!
!              Check the second solution as well.
!
               call zzinlat ( xpt2, bounds, margin, latidx, xin )

               if ( failed() ) return

               if ( xin ) then
!
!                 XPT2 is a candidate solution.
!
                  dist = vdist( vertex, xpt2 )

                  if ( dist < mndist ) then

                     call vequ ( xpt2, xpt )

                     nxpts  = 1
                     mndist = dist
!
!                    This is the best solution seen so far.
!                    However, it's not necessarily the best
!                    solution. So we continue.
!
                     return

                  end if

               end if

            end if

         end if
!
!        We're done with the lower latitude boundary.
!
      end if

!
!     Perform longitude boundary checks if the coverage is not
!     2*pi radians. Note that MAXLON > MINLON at this point.
!
      loncov = maxlon - minlon

      if ( cos(loncov) < 1.0_wp ) then
!
!        We have distinct longitude boundaries. Go to work.
!
!
!        Check the longitude boundaries. Try the plane of western
!        longitude first.
!
!        The vector WESTB is an outward-facing vector normal to
!        the west boundary.
!
         call vpack ( sin(minlon), -cos(minlon), 0.0_wp, westb )


         s = radfac * ( vnorm(vertex) + maxr )


         call zzinrypl ( vertex, udir, westb, 0.0_wp, s, nx, srfx )

         if ( nx == 1 ) then
!
!           We have one point of intersection. Determine whether it's a
!           candidate solution.  Don't use longitude in the following
!           inclusion test. Note that we'll perform a separate check
!           later in place of the longitude check.
!
            call zzinlat ( srfx, bounds, margin, lonidx, xin )

            if ( failed() ) return


            if ( xin ) then
!
!              Make sure the intercept is not too far on the
!              "wrong" side of the Z axis.
!
               call ucrss ( westb, z, wback )

               if (  vdot(srfx, wback) < (margin*maxr)  ) then
!
!                 The intercept is either on the same side of the Z
!                 axis as the west face of the segment, or is very
!                 close to the Z axis.
!
                  dist = vdist( vertex, srfx )

                  if ( dist < mndist ) then
!
!                    Record the intercept, distance, and surface index.
!
                     call vequ ( srfx, xpt )

                     nxpts  = 1
                     mndist = dist

                  end if

               end if

            end if

         end if
!
!        We're done with the western boundary.
!
!
!        Try the plane of eastern longitude next.
!
!        The vector EASTB is an outward-facing vector normal to
!        the east boundary.

         call vpack ( -sin(maxlon), cos(maxlon), 0.0_wp, eastb )

         call zzinrypl ( vertex, udir, eastb, 0.0_wp, s, nx, srfx )

         if ( nx == 1 ) then
!
!           We have one point of intersection. Determine whether it's a
!           candidate solution.
!
            call zzinlat ( srfx, bounds, margin, lonidx, xin )

            if ( failed() ) return

            if ( xin ) then
!
!              Make sure the intercept is not too far on the "wrong"
!              side of the Z axis.
!
               call ucrss ( z, eastb, eback )

               if (  vdot(srfx, eback) < (margin*maxr)  ) then
!
!                 The intercept is either on the same side of the Z
!                 axis as the east face of the segment, or is very
!                 close to the Z axis.
!
                  dist = vdist( vertex, srfx )

                  if ( dist < mndist ) then
!
!                    Record the intercept, distance, and surface index.
!
                     call vequ ( srfx, xpt )

                     nxpts  = 1
                     mndist = dist

                  end if

               end if

            end if

         end if

      end if
!
!     End of longitude boundary checks.
!

!
!     Find the intersection of the ray and inner bounding
!     sphere, if possible.
!
      if ( minr > 0.0_wp ) then

         call zzryxsph ( vertex, udir, minr, srfx, found )

         if ( found ) then
!
!           See whether this solution is in the element.
!
            call zzinlat ( srfx, bounds, margin, radidx, xin )

            if ( failed() ) return

            if ( xin ) then

               dist = vdist( vertex, srfx )

               if ( dist < mndist ) then
!
!                 Record the intercept, distance, and surface index.
!
                  call vequ ( srfx, xpt )

                  nxpts  = 1
                  mndist = dist

               end if

            end if

         end if

!
!        Unlike the outer sphere, either intersection of the ray with
!        the inner sphere can be an optimal solution. We'll test for
!        the case where the intercept further from the ray's vertex is
!        the correct solution.
!
         call vminus( udir, negdir )

         call zzryxsph ( endpt2, negdir, minr, srfx, found )

         if ( found ) then

            call zzinlat ( srfx, bounds, margin, radidx, xin )

            if ( failed() ) return

            if ( xin ) then

               dist = vdist( vertex, srfx )

               if ( dist < mndist ) then
!
!                 Record the intercept, distance, and surface index.
!
                  call vequ ( srfx, xpt )

                  nxpts  = 1
                  mndist = dist

               end if

            end if

         end if

      end if

!
!     NXPTS and XPT are set.
!
      end
