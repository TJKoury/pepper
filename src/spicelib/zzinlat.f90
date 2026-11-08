!$Procedure ZZINLAT ( DSK, in latitudinal element? )

      subroutine zzinlat ( p, bounds, margin, exclud, inside )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Test a point represented by a set of latitudinal coordinates for
!     inclusion in a specified latitudinal volume element. The test is
!     performed using margins for the element.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     DSK
!     GEOMETRY
!     INTERSECTION
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      include 'dsktol.inc'

      real(wp)              p      ( 3 )
      real(wp)              bounds ( 2, 3 )
      real(wp)              margin
      integer(ip)           exclud
      logical               inside

      integer(ip)           none
      parameter           ( none   = 0 )

      integer(ip)           lonidx
      parameter           ( lonidx = 1 )

      integer(ip)           latidx
      parameter           ( latidx = 2 )

      integer(ip)           radidx
      parameter           ( radidx = 3 )

      real(wp)              latmrg
      parameter           ( latmrg = 1.0e-8_wp )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     P          I   Input point.
!     BOUNDS     I   Coordinate bounds of element.
!     MARGIN     I   Margin used for inclusion testing.
!     EXCLUD     I   Index of coordinate to exclude from test.
!     INSIDE     O   Flag indicating whether point is in element.
!     LATMRG     P   Margin for latitude near the poles.
!     LONALI     P   Longitude shift margin.
!     ANGMRG     P   Angular rounding margin.
!     NONE       P   Meaning: exclude nothing.
!     LONIDX     P   Longitude index. Also used for exclusion.
!     LATIDX     P   Latitude index. Also used for exclusion.
!     RADIDX     P   Radius index. Also used for exclusion.
!
! $ Detailed_Input
!
!     P          is a point expressed in Cartesian coordinates. The
!                point is to be checked to determine whether it is
!                inside the volume element specified by BOUNDS.
!
!     BOUNDS     is an 2x3 array containing the bounds of a volume
!                element expressed in latitudinal coordinates. BOUNDS
!                defines the volume element used in the comparison. In
!                the element
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
!                If necessary, the longitude bounds are normalized
!                for the comparison. The normalized bounds differ
!                by no more than 2*pi and are in increasing order.
!                See ZZNRMLON for details.
!
!                The aliasing margin LONALI, the greedy margin, and the
!                angular rounding margin ANGMRG are used for longitude
!                comparisons. ANGMRG is used if its magnitude exceeds
!                that of the greedy margin. See the Parameters section
!                below for further information.
!
!
!     MARGIN     is a fraction used to expand the volume element for
!                inclusion testing.
!
!                   - The outer radius is scaled by (1+MARGIN).
!                     The inner radius is scaled by (1-MARGIN).
!
!                   - Latitude bounds are extended by MARGIN radians,
!                     within the interval -pi/2 : pi/2.
!
!                   - For any input point having latitude "not close"
!                     to pi/2 or -pi/2, the element's longitude
!                     interval is extended by
!
!                        MARGIN / cos(LAT)
!
!                     where LAT is the latitude of the input point.
!
!                     Here "close to" means "within LATMRG radians of."
!
!                   - For any input point having latitude close to
!                     pi/2 or -pi/2, comparison against the longitude
!                     bounds of the volume element is omitted if
!                     MARGIN > 0.
!
!
!
!     EXCLUD     is either a coordinate index or the parameter NONE.
!
!                If EXCLUD is set to one of
!
!                   { LONIDX, LATIDX, RADIDX }
!
!                then the indicated coordinate is excluded from
!                comparison with the corresponding volume element
!                boundaries.
!
!                If EXCLUD is set to NONE, all coordinates are
!                compared.
!
!                Exclusion of coordinates is used in cases where a
!                point is known to be on a level surface of a given
!                coordinate. For example, if a point is on the sphere
!                of radius equal to the upper radius bound, radius need
!                not be used in the comparison and in fact can't be
!                meaningfully compared, due to round-off errors.
!
! $ Detailed_Output
!
!     INSIDE     is a logical flag that is set to .TRUE. if and
!                only if the input coordinates represent a
!                point inside or on the surface of the volume
!                element, according to the comparisons that are
!                performed.
!
!                The value of INSIDE is not affected by the value
!                of any excluded coordinate.
!
! $ Parameters
!
!
!     LATMRG     Margin for determining whether the input latitude is
!                "close" to the poles. Units are radians. This value
!                should be kept in sync with the default "greedy"
!                margin.
!
!     LONALI     is a longitude shift margin. If the input longitude
!                coordinate LON is less than the longitude lower bound
!                by more than this amount, the value LON + 2*pi is
!                used in the longitude comparison, provided that the
!                comparison is not excluded.
!
!                See dsktol.inc for details.
!
!     ANGMRG     is an angular rounding margin. If the input longitude
!                is outside of the longitude bounds by less than this
!                amount, the longitude is considered to be within the
!                bounds. ANGMRG is used when the magnitude of the
!                "greedy" margin is less than ANGMRG. ANGMRG is not
!                used for computations involving coordinates other than
!                longitude.
!
!                This margin is distinct from the "greedy" segment
!                selection margin, which is used to expand a volume
!                element for the purpose of determining whether a given
!                point lies in that element.
!
!                See dsktol.inc for details.
!
!     NONE       when used as a value of the input argument EXCLUD,
!                indicates that no coordinates should be excluded from
!                comparison.
!
!     LONIDX     is the index of longitude in the second dimension of
!                BOUNDS. When used as a value of the input argument
!                EXCLUD, indicates that longitude should be excluded
!                from comparison.
!
!     LATIDX     is the index of latitude in the second dimension of
!                BOUNDS. When used as a value of the input argument
!                EXCLUD, indicates that latitude should be excluded
!                from comparison.
!
!
!     RADIDX     is the index of radius in the second dimension of
!                BOUNDS. When used as a value of the input argument
!                EXCLUD, indicates that radius should be excluded
!                from comparison.
!
! $ Exceptions
!
!     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     2)  If EXCLUD is outside of the range 0:3, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     See usage in ZZRYTLAT.
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
!-    SPICELIB Version 1.0.0, 03-JUN-2016 (NJB)
!
!        Original version 03-OCT-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     test point against latitudinal element using margin
!
!-&




!
!     SPICELIB functions
!
      real(wp)              halfpi
      real(wp)              twopi

      logical               return

!
!     Local parameters
!

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
      real(wp)              amaxr
      real(wp)              aminr
      real(wp)              amaxlt
      real(wp)              aminlt
      real(wp)              amaxlo
      real(wp)              aminlo
      real(wp)              dlon
      real(wp)              hpi
      real(wp)              lat
      real(wp)              lon
      real(wp)              lonmrg
      real(wp)              maxlat
      real(wp)              maxlon
      real(wp)              maxr
      real(wp)              minlat
      real(wp)              minlon
      real(wp)              minr
      real(wp)              pi2
      real(wp)              r
      real(wp)              smax
      real(wp)              smin

      logical               first


      save                  first
      save                  pi2
      save                  hpi

      data                  first / .true. /
      data                  pi2   / -1.0_wp  /
      data                  hpi   / -1.0_wp  /

!
!     Use discovery check-in.
!
      if ( return() ) then
         return
      end if

      if ( first ) then

         pi2   = twopi()
         hpi   = halfpi()

         first = .false.

      end if

!
!     Get the latitudinal coordinates of the input point.
!
      call reclat ( p, r, lon, lat )


!
!     Handle the simpler zero-margin case separately.
!
      if ( margin .eq. 0.0_wp ) then

         call zzinlat0 ( r, lon, lat, bounds, exclud, inside )

         return

      else if ( margin .lt. 0.0_wp ) then

         call chkin  ( 'ZZINLAT'                                )
         call setmsg ( 'Margin must be non-negative but was #.' )
         call errdp  ( '#', margin                              )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )
         call chkout ( 'ZZINLAT'                                )
         return

      end if


      if (  ( exclud .lt. 0 ) .or. ( exclud .gt. 3 )  ) then

         call chkin  ( 'ZZINLAT'                                    )
         call setmsg ( 'EXCLUD must be in the range 0:3 but was #.' )
         call errint ( '#', exclud                                  )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                     )
         call chkout ( 'ZZINLAT'                                    )
         return

      end if

!
!     Special case: if the input point is within distance MARGIN
!     from the origin, and the minimum radius of the volume element
!     is less than or equal to MARGIN, the point is inside.
!
      if ( r .le. margin ) then

         if ( bounds(inner, radidx) .le. margin ) then

            inside = .true.

            return

         end if

      end if


!
!     Assume the point is outside to start. This allows us
!     to skip setting INSIDE when we find a boundary test
!     failure.
!
      inside = .false.

!
!     Get local copies of the coordinate bounds. Don't normalize the
!     longitude bounds until we know we need them.
!
      minr   = bounds( inner, radidx )
      maxr   = bounds( outer, radidx )

      minlat = bounds( south, latidx )
      maxlat = bounds( north, latidx )

!
!     Compare coordinates to adjusted latitude and radius
!     boundaries.
!
      if ( exclud .ne. radidx ) then
!
!        Create adjusted radius bounds.
!
         smax  = 1.0_wp + margin
         smin  = 1.0_wp - margin

         aminr = max( 0.0_wp,  minr*smin )
         amaxr = maxr * smax

         if ( ( r .lt. aminr ) .or. ( r .gt. amaxr ) ) then
             return
         end if

      end if


      if ( exclud .ne. latidx ) then
!
!        Create adjusted latitude bounds.
!
         aminlt = max( -hpi, minlat - margin )
         amaxlt = min(  hpi, maxlat + margin )

         if ( ( lat .lt. aminlt ) .or. ( lat .gt. amaxlt ) ) then
            return
         end if

      end if

!
!     At this point, the input radius and latitude are within the
!     adjusted bounds, if their tests haven't been excluded by
!     the caller.
!
!     Perform longitude tests, unless they're excluded by the
!     caller.
!
      if ( exclud .ne. lonidx ) then

         call zznrmlon ( bounds(west,lonidx), bounds(east,lonidx), &
                         angmrg,              minlon, &
                         maxlon                                   )
!
!        Set the margin to be used for longitude interval
!        inclusion tests.
!
         lonmrg = max( abs(angmrg), abs(margin) )

!
!        We have a special case for segments that include the poles. If
!        the input point is close enough to a pole contained in the
!        segment, we consider the point to be included in the segment,
!        regardless of the point's longitude. All other points get the
!        normal longitude test.
!
         if (       ( lat .le. (  hpi - latmrg ) ) &
              .and. ( lat .ge. ( -hpi + latmrg ) )  ) then
!
!           This is the usual case: the latitude of the input point
!           is bounded away from the poles.
!
!           Check the point's longitude against the segment's
!           longitude bounds.
!
!           We'll scale the longitude margin to compensate for the
!           latitude of the input point. Note that the division
!           below is safe; presuming a reasonable value of MARGIN;
!           we know that
!
!              DLON << 1
!
            dlon   = lonmrg / max( abs(cos(lat)), latmrg )

            aminlo = minlon - dlon
            amaxlo = maxlon + dlon

!
!           Now move the input point's longitude into range, if
!           necessary.
!
            if ( lon .lt. aminlo ) then

               if (  lon  .lt. ( aminlo - lonali )  ) then
!
!                 See whether an aliased version of LON is a match.
!
                  lon = lon + pi2

               else
!
!                 Consider LON to be a match with the lower bound.
!
                  lon = aminlo

               end if

            else if ( lon .gt. amaxlo ) then

               if (  lon  .gt. ( amaxlo + lonali )  ) then
!
!                 See whether an aliased version of LON is a match.
!
                  lon = lon - pi2

               else
!
!                 Consider LON to be a match with the upper bound.
!
                  lon = amaxlo

               end if

            end if

!
!           Compare the adjusted longitude of the input point to the
!           adjusted longitude bounds.
!
            if ( ( lon .lt. aminlo ) .or. ( lon .gt. amaxlo ) ) then

               return

            end if


         else
!
!           The latitude of the input point is close to one of the
!           poles.
!
!           This is a no-op case.
!
!           The input point has already passed whichever of the radius
!           and latitude tests that were not excluded.
!
!           If the element has a non-degenerate latitude boundary
!           having the same sign as the latitude of the input point,
!           and if latitude is excluded because the input point is
!           already nominally on that boundary, then passing the radius
!           check implies that the point is close to the element.
!
!           If the element has a degenerate latitude boundary having
!           the same sign as the latitude of the input point---namely,
!           the element contains the pole, and latitude is excluded,
!           then then passing the radius check implies that the point
!           is close to the portion of the Z-axis contained in the
!           element.
!
!           If the radius check has been excluded because the point is
!           already nominally on one of the element's radius boundaries,
!           the passing the latitude test implies the point is close
!           to the element.
!
!           In all cases, as long as EXCLUD has been set appropriately,
!           the point is close to the element. We consider the point to
!           be in the expanded element.
!
         end if

      end if

!
!     All tests that were commanded have been passed. The input
!     point is considered to be contained in the expanded volume
!     element.
!
      inside = .true.

      end
