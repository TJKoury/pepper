!$Procedure ZZRYTPDT ( DSK, ray touches planetodetic element )

      subroutine zzrytpdt ( vertex, raydir, bounds, &
                            corpar, margin, nxpts,  xpt )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find nearest intersection to a given ray's vertex of the ray and
!     a planetodetic volume element. If the vertex is inside the
!     element, the vertex is considered to be the solution.
!
!     In the computation performed by this routine, ellipsoidal
!     surfaces are used, instead of surfaces of constant altitude, to
!     define boundaries of planetodetic volume elements. The element
!     defined by the input boundaries is contained in the element
!     bounded by the input latitude and longitude boundaries and by the
!     ellipsoidal surfaces.
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
      real(wp)              corpar ( * )
      real(wp)              margin
      integer(ip)           nxpts
      real(wp)              xpt    ( 3 )

      integer(ip)           lonidx
      parameter           ( lonidx = 1 )

      integer(ip)           latidx
      parameter           ( latidx = 2 )

      integer(ip)           altidx
      parameter           ( altidx = 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     BOUNDS     I   Bounds of planetodetic volume element.
!     CORPAR     I   Coordinate parameters.
!     MARGIN     I   Margin used for element expansion.
!     NXPTS      O   Number of intercept points.
!     XPT        O   Intercept.
!     LONIDX     P   Longitude index.
!     LATIDX     P   Latitude index.
!     ALTIDX     P   Altitude index.
!
! $ Detailed_Input
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector of
!                the ray to be used in the intercept computation.
!
!                Both the vertex and ray direction must be represented
!                in the reference frame to which the planetodetic
!                volume element boundaries correspond. The vertex is
!                considered to be an offset from the center of the
!                reference frame associated with the element.
!
!     BOUNDS     is a 2x3 array containing the bounds of a planetodetic
!                volume element. Normally this is the coverage boundary
!                of a DSK segment. In the element
!
!                   BOUNDS(I,J)
!
!                J is the coordinate index. J is one of
!
!                   { LONIDX, LATIDX, ALTIDX }
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
!     RE,
!     F          are, respectively, the equatorial radius and
!                flattening coefficient associated with the
!                planetodetic coordinate system in which the input
!                volume element is described.
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
!                by the segment, if such an intercept exists. If the
!                ray intersects the surface at multiple points, the
!                one closest to the ray's vertex is selected. XPT is
!                valid if and only if FOUND is .TRUE.
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
!     LONIDX     is the index of longitude in the second dimension of
!                BOUNDS.
!
!     LATIDX     is the index of latitude in the second dimension of
!                BOUNDS.
!
!     ALTIDX     is the index of altitude in the second dimension of
!                BOUNDS.
! $ Exceptions
!
!     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     2)  If the input ray direction vector is zero, the error
!         SPICE(ZEROVECTOR) will be signaled.
!
!     3)  Any errors that occur while calculating the ray-surface
!         intercept will be signaled by routines in the call tree
!         of this routine.
!
! $ Files
!
!     None. However, the input segment boundaries normally have
!     been obtained from a loaded DSK file.
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
!-    SPICELIB Version 1.0.0, 19-JAN-2017 (NJB)
!
!-&

! $ Index_Entries
!
!     find intercept of ray on planetodetic volume element
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
      real(wp)              vsep

      logical               failed
      logical               return
      logical               vzero
      logical               zzpdpltc

!
!     Local parameters
!

!
!     Altitude expansion factor:
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

      integer(ip)           lower
      parameter           ( lower  = 1 )

      integer(ip)           upper
      parameter           ( upper  = 2 )

      integer(ip)           none
      parameter           ( none   = 0 )

!
!     Local variables
!
      real(wp)              amnalt
      real(wp)              amxalt
      real(wp)              angle
      real(wp)              apex   ( 3 )
      real(wp)              dist
      real(wp)              eastb  ( 3 )
      real(wp)              eback  ( 3 )
      real(wp)              emax
      real(wp)              emin
      real(wp)              endpt2 ( 3 )
      real(wp)              f
      real(wp)              loncov
      real(wp)              maxalt
      real(wp)              maxlat
      real(wp)              maxlon
      real(wp)              maxr
      real(wp)              minalt
      real(wp)              minlat
      real(wp)              minlon
      real(wp)              mndist
      real(wp)              negdir ( 3 )
      real(wp)              pmax
      real(wp)              pmin
      real(wp)              re
      real(wp)              rp
      real(wp)              s
      real(wp)              srfx   ( 3 )
      real(wp)              udir   ( 3 )
      real(wp)              vtxang
      real(wp)              vtxlvl
      real(wp)              vtxoff ( 3 )
      real(wp)              wback  ( 3 )
      real(wp)              westb  ( 3 )
      real(wp)              xpt2   ( 3 )
      real(wp)              xincpt
      real(wp)              yincpt
      real(wp)              z      ( 3 )

      integer(ip)           nx

      logical               found
      logical               inside
      logical               xin
      logical               xval1
      logical               xval2

!
!     Saved variables
!
      save                  z

!
!     Initial values
!
      data                  z      / 0.0_wp, 0.0_wp, 1.0_wp /



      if ( return() ) then
         return
      end if

      call chkin ( 'ZZRYTPDT' )


      if ( margin .lt. 0.0_wp ) then

         call setmsg ( 'Margin must be non-negative but was #.' )
         call errdp  ( '#', margin                              )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )
         call chkout ( 'ZZRYTPDT'                               )
         return

      end if

      if ( vzero(raydir) ) then

         call setmsg ( 'The ray''s direction was the zero vector.' )
         call sigerr ( 'SPICE(ZEROVECTOR)'                         )
         call chkout ( 'ZZRYTPDT'                                  )
         return

      end if


!
!     Determine whether the vertex is inside the element.
!
      call zzinpdt ( vertex, bounds, corpar, margin, none, inside )

      if ( failed() ) then
         call chkout ( 'ZZRYTPDT' )
         return
      end if

      if ( inside ) then
!
!        We know the answer.
!
         nxpts = 1

         call vequ ( vertex, xpt )

         call chkout ( 'ZZRYTPDT' )
         return

      end if

!
!     Get semi-axis lengths of the reference spheroid.
!
      re = corpar(1)
      f  = corpar(2)
      rp = re * ( 1.0_wp - f )

!
!     Extract the segment's coordinate bounds into easily
!     readable variables.
!
      minalt = bounds( lower, altidx )
      maxalt = bounds( upper, altidx )
!
!     Normalize the longitude bounds. After this step, the bounds will
!     be in order and differ by no more than 2*pi.
!
      call zznrmlon( bounds(west,lonidx), bounds(east,lonidx), angmrg, &
                     minlon,              maxlon                      )

      if ( failed() ) then
         call chkout ( 'ZZRYTPDT' )
         return
      end if

      minlat = bounds( south, latidx )
      maxlat = bounds( north, latidx )

!
!     Compute adjusted altitude bounds, taking margin into
!     account.
!
      amnalt = minalt  -  margin * abs(minalt)
      amxalt = maxalt  +  margin * abs(maxalt)

!
!     Generate semi-axis lengths of inner and outer bounding
!     ellipsoids.
!
      if ( re .ge. rp ) then
!
!        The reference spheroid is oblate.
!
         call zzellbds ( re,   rp,   amxalt, amnalt, &
                         emax, pmax, emin,   pmin   )

      else
!
!        The reference spheroid is prolate.
!
         call zzellbds ( rp,   re,   amxalt, amnalt, &
                         pmax, emax, pmin,   emin   )

      end if

      if ( failed() ) then
         call chkout ( 'ZZRYTPDT' )
         return
      end if

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
!     Find the intersection of the ray and outer bounding ellipsoid, if
!     possible. Often this intersection is the closest to the vertex.
!     If the intersection exists and is on the boundary of the element,
!     it's a winner.
!
      call surfpt ( vertex, udir, emax, emax, pmax, srfx, found )

      if ( failed() ) then
         call chkout ( 'ZZRYTPDT' )
         return
      end if

      if ( .not. found ) then
!
!        There are no intersections. The ray cannot hit the volume
!        element.
!
         call chkout ( 'ZZRYTPDT' )
         return

      end if

!
!     The ray hits the outer bounding ellipsoid. See whether
!     the longitude and latitude are within bounds, taking
!     the margin into account. Exclude the altitude coordinate
!     from testing.
!
      call zzinpdt ( srfx, bounds, corpar, margin, altidx, xin )

      if ( failed() ) then
         call chkout ( 'ZZRYTPDT' )
         return
      end if

      if ( xin ) then
!
!        This solution is a candidate.
!
         call vequ ( srfx, xpt )
         nxpts  = 1

!
!        Find the level surface parameter of the vertex relative
!        to the adjusted outer bounding ellipsoid.
!
         vtxlvl =     ( vertex(1)/emax )**2 &
                   +  ( vertex(2)/emax )**2 &
                   +  ( vertex(3)/pmax )**2

         if ( vtxlvl .gt. 1.0_wp ) then
!
!           The vertex is outside this ellipsoid, and the DSK segment
!           lies within the ellipsoid.
!
!           No other intersection can be closer to the vertex;
!           we don't need to check the other surfaces.
!
            call chkout ( 'ZZRYTPDT' )
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
      maxr = max ( emax, pmax )

      s    = vnorm(vertex) + radfac * maxr

      call vlcom ( 1.0_wp, vertex, s, udir, endpt2 )

!
!     Now try the upper latitude bound. We can skip this test
!     if the upper bound is pi/2 radians.
!
      if ( maxlat .lt. halfpi() ) then
!
!        Let ANGLE be the angular separation of the surface of latitude
!        MAXLAT and the +Z axis. Note that the surface might be the
!        lower nappe of the cone.
!
         angle = max ( 0.0_wp,  halfpi() - maxlat )

!
!        Compute the Z coordinate of the apex of the latitude cone.
!
         call zzelnaxx ( re, rp, maxlat, xincpt, yincpt )

         if ( failed() ) then
            call chkout ( 'ZZRYTPDT' )
            return
         end if

         apex(1) = 0.0_wp
         apex(2) = 0.0_wp
         apex(3) = yincpt

!
!        Find the offset of the ray's vertex from the cone's apex,
!        and find the angular separation of the offset from the +Z
!        axis. This separation enables us to compare the latitude of
!        the vertex to the latitude boundary without making a RECGEO
!        call to compute the planetodetic coordinates of the vertex.
!
!        (The comparison will be done later.)
!
         call vsub ( vertex, apex, vtxoff )

         vtxang = vsep ( vtxoff, z )

!
!        Check for intersection of the ray with the latitude cone.
!
         call incnsg ( apex, z, angle, vertex, endpt2, nx, srfx, xpt2 )

         if ( failed() ) then
            call chkout ( 'ZZRYTPDT' )
            return
         end if

!
!        Unlike the case of latitudinal coordinates, for planetodetic
!        coordinates, the surface of the latitude cone does not
!        coincide with the set of points having that latitude (which is
!        equal to pi/2 - the cone's angular separation from the +Z
!        axis). The subset of the cone having the specified latitude is
!        truncated by the X-Y plane. If we ignore round-off errors, we
!        can assert that the Z-coordinate of a point having the given
!        planetodetic latitude must match the direction of the nappe of
!        the cone: positive if ANGLE < pi/2, negative if ANGLE > pi/2,
!        and 0 if ANGLE = pi/2.
!
!        However, we cannot ignore round-off errors. For a cone having
!        angle from its central axis of nearly pi/2, it's possible for
!        a valid ray-cone intercept to be on the "wrong" side of the
!        X-Y plane due to round-off errors. So we use a more robust
!        check to determine whether an intercept should be considered
!        to have the same latitude as the cone.
!
!        Check all intercepts.
!
         if ( nx .gt. 0 ) then
!
!           Check the first intercept.
!
            xval1 =  zzpdpltc( re, f, srfx, maxlat )
            xval2 =  .false.

            if ( nx .eq. 2 ) then
!
!              Check the second intercept.
!
               xval2 =  zzpdpltc( re, f, xpt2, maxlat )

            end if

            if ( xval1 .and. ( .not. xval2 ) ) then

               nx = 1

            else if (  xval2  .and.  (.not. xval1 )  ) then
!
!              Only the second solution is valid. Overwrite
!              the first.
!
               nx = 1

               call vequ( xpt2, srfx )

            else if (  ( .not. xval1 )  .and.  ( .not. xval2 )  ) then
!
!              Neither solution is valid.
!
               nx = 0

            end if

         end if




         if ( nx .ge. 1 ) then
!
!           The ray intercept SRFX lies on the upper latitude boundary.
!
!           See whether SRFX meets the longitude and proxy altitude
!           constraints.
!
            call zzinpdt ( srfx, bounds, corpar, margin, latidx, xin )

            if ( failed() ) then
               call chkout ( 'ZZRYTPDT' )
               return
            end if


            if ( xin ) then
!
!              SRFX is a candidate solution.
!
               dist = vdist( vertex, srfx )

               if ( dist .lt. mndist ) then

                  call vequ ( srfx, xpt )

                  nxpts  = 1


                  if ( vtxang .lt. angle ) then

                     if (      ( maxlat    .lt. 0.0_wp ) &
                          .or. ( vertex(3) .gt. 0.0_wp )  ) then
!
!                       If MAXLAT is negative, the vertex offset
!                       being outside the cone is enough to
!                       guarantee the planetodetic latitude of the
!                       vertex is greater than that of the cone.
!
!                       If MAXLAT is non-negative, the angle of the
!                       vertex offset relative to the +Z axis is not
!                       enough; we need the vertex to lie above the
!                       X-Y plane as well.
!
!                       Getting here means one of these conditions
!                       was met.
!
!                       Since the latitude of the vertex is greater
!                       than MAXLAT, this is the best solution, since
!                       the volume element is on the other side of the
!                       maximum latitude boundary.
!
                        call chkout ( 'ZZRYTPDT' )
                        return

                     end if

                  end if
!
!                 This is the best solution seen so far, but we
!                 need to check the remaining boundaries.
!
                  mndist = dist

               end if

            end if

            if ( nx .eq. 2 ) then
!
!              Check the second solution as well.
!
               call zzinpdt ( xpt2,   bounds, corpar, &
                              margin, latidx, xin    )

               if ( failed() ) then
                  call chkout ( 'ZZRYTPDT' )
                  return
               end if

               if ( xin ) then
!
!                 XPT2 is a candidate solution.
!
                  dist = vdist( vertex, xpt2 )

                  if ( dist .lt. mndist ) then

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
      if (  minlat .gt. -halfpi() ) then
!
!        Let ANGLE be the angular separation of the surface
!        of latitude MINLAT and the +Z axis. Note that the
!        surface might be the lower nappe of the cone.
!
         angle = halfpi() - minlat


!        Compute the Z coordinate of the apex of the latitude cone.
!
         call zzelnaxx ( re, rp, minlat, xincpt, yincpt )

         if ( failed() ) then
            call chkout ( 'ZZRYTPDT' )
            return
         end if

         apex(1) = 0.0_wp
         apex(2) = 0.0_wp
         apex(3) = yincpt

         call incnsg ( apex,   z,  angle, vertex, &
                       endpt2, nx, srfx,  xpt2    )

         if ( failed() ) then
            call chkout ( 'ZZRYTPDT' )
            return
         end if

!
!        Find the offset of the ray's vertex from the cone's apex,
!        and find the angular separation of the offset from the +Z
!        axis. This separation enables us to compare the latitude of
!        the vertex to the latitude boundary without making a RECGEO
!        call to compute the planetodetic coordinates of the vertex.
!
!        (The comparison will be done later.)
!
         call vsub ( vertex, apex, vtxoff )

         vtxang = vsep ( vtxoff, z )

!
!        Check whether the latitude of the intercept can be
!        considered to match that of the cone.
!
         if ( nx .gt. 0 ) then
!
!           Check the first intercept.
!
            xval1 =  zzpdpltc( re, f, srfx, minlat )
            xval2 =  .false.

            if ( nx .eq. 2 ) then
!
!              Check the second intercept.
!
               xval2 =  zzpdpltc( re, f, xpt2, minlat )

            end if

            if ( xval1 .and. ( .not. xval2 ) ) then

               nx = 1

            else if (  xval2  .and.  (.not. xval1 )  ) then
!
!              Only the second solution is valid. Overwrite
!              the first.
!
               nx = 1

               call vequ( xpt2, srfx )

            else if (  ( .not. xval1 )  .and.  ( .not. xval2 )  ) then
!
!              Neither solution is valid.
!
               nx = 0

            end if

         end if


         if ( nx .ge. 1 ) then
!
!           The ray intercept SRFX lies on the lower latitude boundary.
!
!           See whether SRFX meets the longitude and proxy altitude
!           constraints.
!
            call zzinpdt ( srfx, bounds, corpar, margin, latidx, xin )

            if ( failed() ) then
               call chkout ( 'ZZRYTPDT' )
               return
            end if

            if ( xin ) then
!
!              SRFX is a candidate solution.
!
               dist = vdist( vertex, srfx )

               if ( dist .lt. mndist ) then

                  call vequ ( srfx, xpt )

                  nxpts  = 1

                  if ( vtxang .gt. angle ) then

                     if (      ( minlat    .gt. 0.0_wp ) &
                          .or. ( vertex(3) .lt. 0.0_wp )  ) then
!
!                       If MINLAT is positive, the vertex offset
!                       being outside the cone is enough to
!                       guarantee the planetodetic latitude of the
!                       vertex is less than that of the cone.
!
!                       If MINLAT is non-positive, the angle of the
!                       vertex offset relative to the +Z axis is not
!                       enough; we need the vertex to lie below the
!                       X-Y plane as well.
!
!                       Getting here means one of these conditions
!                       was met.
!
!                       Since the latitude of the vertex is less than
!                       than MINLAT, this is the best solution, since
!                       the volume element is on the other side of the
!                       minimum latitude boundary.
!
                        call chkout ( 'ZZRYTPDT' )
                        return

                     end if

                  end if
!
!                 This is the best solution seen so far, but we
!                 need to check the remaining boundaries.

                  mndist = dist

               end if

            end if

            if ( nx .eq. 2 ) then
!
!              Check the second solution as well.
!
               call zzinpdt ( xpt2,   bounds, corpar, &
                              margin, latidx, xin    )

               if ( failed() ) then
                  call chkout ( 'ZZRYTPDT' )
                  return
               end if


               if ( xin ) then
!
!                 XPT2 is a candidate solution.
!
                  dist = vdist( vertex, xpt2 )

                  if ( dist .lt. mndist ) then

                     call vequ ( xpt2, xpt )

                     nxpts  = 1
                     mndist = dist
!
!                    This is the best solution seen so far.
!                    However, it's not necessarily the best
!                    solution. So we continue.
!
                     call chkout ( 'ZZRYTPDT' )
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

      if ( cos(loncov) .lt. 1.0_wp ) then
!
!        We have distinct longitude boundaries. Go to work.
!
!
!        Check the longitude boundaries. Try the plane of western
!        longitude first.
!
         call vpack ( sin(minlon), -cos(minlon), 0.0_wp, westb )


         s = radfac * ( vnorm(vertex) + maxr )


         call zzinrypl ( vertex, udir, westb, 0.0_wp, s, nx, srfx )

         if ( nx .eq. 1 ) then
!
!           We have one point of intersection. Determine whether it's a
!           candidate solution.  Don't use longitude in the following
!           inclusion test. Note that we'll perform a separate check
!           later in place of the longitude check.
!
            call zzinpdt ( srfx, bounds, corpar, margin, lonidx, xin )

            if ( failed() ) then
               call chkout ( 'ZZRYTPDT' )
               return
            end if


            if ( xin ) then
!
!              Make sure the intercept is not too far on the
!              "wrong" side of the Z axis.
!
               call ucrss ( westb, z, wback )

               if (  vdot(srfx, wback) .lt. (margin*maxr)  ) then
!
!                 The intercept is either on the same side of the Z
!                 axis as the west face of the segment, or is very
!                 close to the Z axis.
!
                  dist = vdist( vertex, srfx )

                  if ( dist .lt. mndist ) then
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
         call vpack ( -sin(maxlon), cos(maxlon), 0.0_wp, eastb )

         call zzinrypl ( vertex, udir, eastb, 0.0_wp, s, nx, srfx )

         if ( nx .eq. 1 ) then
!
!           We have one point of intersection. Determine whether it's a
!           candidate solution.
!
            call zzinpdt ( srfx, bounds, corpar, margin, lonidx, xin )

            if ( failed() ) then
               call chkout ( 'ZZRYTPDT' )
               return
            end if


            if ( xin ) then
!
!              Make sure the intercept is not too far on the "wrong"
!              side of the Z axis.
!
               call ucrss ( z, eastb, eback )

               if (  vdot(srfx, eback) .lt. (margin*maxr)  ) then
!
!                 The intercept is either on the same side of the Z
!                 axis as the east face of the segment, or is very
!                 close to the Z axis.
!
                  dist = vdist( vertex, srfx )

                  if ( dist .lt. mndist ) then
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
!     Find the intersection of the ray and lower bounding
!     ellipsoid, if possible.
!
      call surfpt ( vertex, udir, emin, emin, pmin, srfx, found )

      if ( failed() ) then
         call chkout ( 'ZZRYTPDT' )
         return
      end if


      if ( found ) then
!
!        See whether this solution is in the element.
!
         call zzinpdt ( srfx, bounds, corpar, margin, altidx, xin )

         if ( failed() ) then
            call chkout ( 'ZZRYTPDT' )
            return
         end if


         if ( xin ) then

            dist = vdist( vertex, srfx )

            if ( dist .lt. mndist ) then
!
!              Record the intercept, distance, and surface index.
!
               call vequ ( srfx, xpt )

               nxpts  = 1
               mndist = dist

            end if

         end if

      end if

!
!     Unlike the outer ellipsoid, either intersection of the ray with
!     the inner ellipsoid might be a valid solution. We'll test for the
!     case where the intersection farther from the ray's vertex is the
!     correct one.
!
      call vminus( udir, negdir )

      call surfpt ( endpt2, negdir, emin, emin, pmin, srfx, found )

      if ( failed() ) then
         call chkout ( 'ZZRYTPDT' )
         return
      end if


      if ( found ) then

         call zzinpdt ( srfx, bounds, corpar, margin, altidx, xin )

         if ( failed() ) then
            call chkout ( 'ZZRYTPDT' )
            return
         end if


         if ( xin ) then

            dist = vdist( vertex, srfx )

            if ( dist .lt. mndist ) then
!
!              Record the intercept, distance, and surface index.
!
               call vequ ( srfx, xpt )

               nxpts  = 1
!
!              There's no need to update MNDIST at this point.
!
            end if

         end if

      end if

!
!     NXPTS and XPT are set.
!
      call chkout ( 'ZZRYTPDT' )
      return
      end
