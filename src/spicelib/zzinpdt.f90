!$Procedure ZZINPDT ( DSK, in planetodetic element? )

      subroutine zzinpdt ( p, bounds, corpar, margin, exclud, inside )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Test a point represented by a set of planetodetic coordinates for
!     inclusion in a specified planetodetic volume element. Ellipsoidal
!     surfaces are used in place of surfaces of constant altitude. The
!     test is performed using margins for the element.
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
      real(wp)              corpar ( * )
      real(wp)              margin
      integer(ip)           exclud
      logical               inside

      integer(ip), parameter :: none = 0

      integer(ip), parameter :: lonidx = 1

      integer(ip), parameter :: latidx = 2

      integer(ip), parameter :: altidx = 3

      real(wp), parameter :: latmrg = 1.0e-8_wp

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     P          I   Input point.
!     BOUNDS     I   Coordinate bounds of element.
!     CORPAR     I   Coordinate system parameters.
!     MARGIN     I   Margin used for inclusion testing.
!     EXCLUD     I   Index of coordinate to exclude from test.
!     INSIDE     O   Flag indicating whether point is in element.
!     LONALI     P   Longitude shift margin.
!     NONE       P   Meaning: exclude nothing.
!     LONIDX     P   Longitude index. Also used for exclusion.
!     LATIDX     P   Latitude index. Also used for exclusion.
!     ALTIDX     P   Altitude index. Also used for exclusion.
!     LATMRG     P   High/low latitude margin.
!
! $ Detailed_Input
!
!     P          is a point expressed in Cartesian coordinates. The
!                point is to be checked to determine whether it is
!                inside the volume element specified by BOUNDS.
!
!     BOUNDS     is an 2x3 array containing the bounds of a volume
!                element expressed in planetodetic coordinates. BOUNDS
!                defines the volume element used in the comparison. In
!                the element
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
!     CORPAR     is an array of coordinate parameters; normally these
!                are obtained from a DSK descriptor. The first element
!                is the equatorial radius of the reference spheroid.
!                The second element is the flattening coefficient.
!                Any other elements are ignored by this routine.
!
!     MARGIN     is a fraction used to expand the volume element for
!                inclusion testing.
!
!                   - The upper altitude bound is increased by
!
!                        MARGIN*ABS( BOUNDS(2,3) )
!
!                     The lower altitude bound is decreased by
!
!                        MARGIN*ABS( BOUNDS(1,3) )
!
!                     Note that these bounds are used as inputs to
!                     compute the radii of upper and lower bounding
!                     ellipsoids. The input point's position is tested
!                     for inclusion within or exclusion from these
!                     ellipsoids.
!
!                   - Latitude bounds are extended by MARGIN radians,
!                     within the interval -pi/2 : pi/2.
!
!                   - For any input point having latitude "not close"
!                     to pi/2 or -pi/2, the element's longitude
!                     interval is extended by
!
!                        MARGIN / cos(LAT) radians
!
!                     where LAT is the planetocentric [sic] latitude of
!                     the input point.
!
!                     Here "close to" means "within LATMRG radians of."
!
!                   - For any input point having planetocentric
!                     latitude close to (within LATMRG radians of) pi/2
!                     or -pi/2, comparison against the longitude bounds
!                     of the volume element is omitted if MARGIN > 0.
!
!     EXCLUD     is either a coordinate index or the parameter NONE.
!
!                If EXCLUD is set to one of
!
!                   { LONIDX, LATIDX, ALTIDX }
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
!                coordinate. For example, if a point is on the outer
!                bounding ellipsoid, the level surface parameter of the
!                point with respect to the ellipsoid need not be used
!                in the comparison and in fact can't be meaningfully
!                compared, due to round-off errors.
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
!     LONALI     is a longitude shift margin. If the input longitude
!                coordinate LON is less than the longitude lower bound
!                by more than this amount, the value LON + 2*pi is
!                used in the longitude comparison, provided that the
!                comparison is not excluded.
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
!     ALTIDX     is the index of altitude in the second dimension of
!                BOUNDS. When used as a value of the input argument
!                EXCLUD, indicates that altitude should be excluded
!                from comparison.
!
!     LATMRG     is the margin used to determine whether the input
!                latitude is "close" to the poles. Units are radians.
!                The value of this parameter must be kept in sync
!                with the "greedy" margin declared in dsktol.inc.
!
! $ Exceptions
!
!     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     2)  If EXCLUD is outside of the range 0:3, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     3)  If an error occurs while determining the planetodetic
!         coordinates of the input point, the error will be signaled by
!         a routine in the call tree of this routine.
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
!     See usage in ZZRYTPDT.
!
! $ Restrictions
!
!     1)  This is a private routine. It is meant to be used only by the
!         DSK subsystem.
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
!-    SPICELIB Version 1.0.0, 13-FEB-2017 (NJB)
!
!        03-JUN-2016 (NJB)
!
!           Added standard traceback participation. Added check for
!           invalid values of EXCLUD.
!
!        05-MAR-2016 (NJB)
!
!           Original version 03-OCT-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     test point against planetodetic element using margin
!
!-&

!
!     SPICELIB functions
!
      real(wp)              halfpi
      real(wp)              pi
      real(wp)              twopi

      logical               failed
      logical               return

!
!     Local parameters
!

!
!     Element boundary indices:
!
      integer(ip), parameter :: west = 1

      integer(ip), parameter :: east = 2

      integer(ip), parameter :: south = 1

      integer(ip), parameter :: north = 2

      integer(ip), parameter :: lower = 1

      integer(ip), parameter :: upper = 2

!
!     Numeric relation codes returned by ZZPDCMPL:
!
      integer(ip), parameter :: lt = -1

      integer(ip), parameter :: gt = 1

!
!     The code EQ can be returned by ZZPDCMPL, but we make no
!     references to it, so it's not declared here. For the
!     record, EQ is set to 0.
!

!
!     Local variables
!
      real(wp)              altbds ( 2, 3 )
      real(wp)              amnalt
      real(wp)              amnlat
      real(wp)              amnlon
      real(wp)              amxalt
      real(wp)              amxlat
      real(wp)              amxlon
      real(wp)              dlon
      real(wp)              f
      real(wp)              hpi
      real(wp)              lon
      real(wp)              lonmrg
      real(wp)              maxalt
      real(wp)              maxlat
      real(wp)              maxlon
      real(wp)              minalt
      real(wp)              minlat
      real(wp)              minlon
      real(wp)              pcnlat
      real(wp)              pi2
      real(wp)              r
      real(wp)              re

      integer(ip)           relmin
      integer(ip)           relmax

      logical               alpass
      logical               first

!
!     Saved variables
!
      save                  altbds
      save                  first
      save                  hpi
      save                  pi2

!
!     Initial values
!
      data                  first / .true. /
      data                  hpi   / -1.0_wp  /

      if ( return() ) return

      if ( first ) then

         hpi   = halfpi()
         pi2   = twopi()
!
!        Initialize the local array used for altitude checks.
!
         altbds( south, latidx ) =  - halfpi()
         altbds( north, latidx ) =    halfpi()
         altbds( west,  lonidx ) =  - pi()
         altbds( east,  lonidx ) =    pi()
         altbds( lower, altidx ) =    0.0_wp
         altbds( upper, altidx ) =    0.0_wp

         first = .false.

      end if

      if (  ( exclud < 0 ) .or. ( exclud > 3 )  ) then

         call setmsg ( 'EXCLUD must be in the range 0:3 but was #.' )
         call errint ( '#', exclud                                  )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                     )
         return

      end if

!
!     Get the planetocentric [sic] coordinates of the input point. The
!     latitude we obtain will be planetocentric. To emphasize this, we
!     use the name "PCNLAT."
!
      call reclat ( p, r, lon, pcnlat )
!
!     RECLAT is error free, so we don't call FAILED() here.
!

      if ( margin == 0.0_wp ) then
!
!        ZZINPDT0 contains the logic required to determine whether
!        the input point is contained in the element.
!
         call zzinpdt0 ( p,      lon, &
                         bounds, corpar, exclud, inside )

         return

      else if ( margin < 0.0_wp ) then

         call setmsg ( 'Margin must be non-negative but was #.' )
         call errdp  ( '#', margin                              )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )
         return

      end if

!
!     At this point a more detailed analysis is needed.
!
!     Assume the point is outside to start. This allows us
!     to skip setting INSIDE when we find a boundary test
!     failure.
!
      inside = .false.

!
!     We'll use the shape parameters for latitude and altitude
!     comparisons.
!
      re = corpar(1)
      f  = corpar(2)

!
!     Get local copies of the coordinate bounds. Don't normalize the
!     longitude bounds until we know we need them.
!
      minlat = bounds( south, latidx )
      maxlat = bounds( north, latidx )

      minalt = bounds( lower, altidx )
      maxalt = bounds( upper, altidx )

!
!     Compare coordinates to adjusted latitude boundaries.
!
      if ( exclud /= latidx ) then
!
!        Create adjusted latitude bounds.
!
         amnlat = max( -hpi-angmrg, minlat - margin )
         amxlat = min(  hpi+angmrg, maxlat + margin )

!
!        Compare the latitude of the input point to the bounds.
!
         call zzpdcmpl ( re, f, p, amnlat, relmin )
         call zzpdcmpl ( re, f, p, amxlat, relmax )

         if ( failed() ) return

         if ( ( relmin == lt ) .or. ( relmax == gt ) ) then
!
!           The latitude of P is strictly outside of the element's
!           latitude bounds.
!
            return
         end if

      end if

!
!     Test the point for inclusion in the region between the bounding
!     ellipsoids that act as proxies for altitude boundaries.
!
      if ( exclud /= altidx ) then
!
!        Extract altitude bounds from the segment descriptor.
!
         minalt = bounds(lower, altidx)
         maxalt = bounds(upper, altidx)
!
!        Adjust altitude bounds to account for the margin.
!
         amnalt = minalt  -  margin * abs(minalt)
         amxalt = maxalt  +  margin * abs(maxalt)
!
!        Set up a "boundary" array so that we can use ZZINPDT0
!        to do the altitude check for us. We'll exclude longitude
!        tests; the latitude test is set up for an automatic pass
!        (the latitude range of ALTBDS is [-pi/2, pi/2]).
!
         altbds( lower, altidx ) = amnalt
         altbds( upper, altidx ) = amxalt

         call zzinpdt0 ( p, lon, altbds, corpar, lonidx, alpass )

         if ( .not. alpass ) return

      end if

!
!     At this point, the input altitude and latitude are within the
!     adjusted bounds, if their tests haven't been excluded by
!     the caller.
!
!     Perform longitude tests, unless they're excluded by the
!     caller.
!
      if ( exclud /= lonidx ) then
!
!        Start out by normalizing the element's longitude bounds.
!
         call zznrmlon ( bounds(lower,lonidx),  bounds(upper,lonidx), &
                         angmrg,       minlon,  maxlon               )

         if ( failed() ) return
!
!        Set the margin to be used for longitude interval
!        inclusion tests.
!
         lonmrg = max( abs(angmrg), abs(margin) )

!
!        We have a special case for segments that include the poles. If
!        the latitude and altitude of the input point are within
!        bounds, and if the latitude of the point is close enough to a
!        pole we consider the point to be included in the segment,
!        regardless of the point's longitude. All other points get the
!        normal longitude test.
!
!        We use planetocentric latitude to determine whether the point
!        is "close" to a pole.
!
         call zzpdcmpl ( re, f, p,  hpi-latmrg, relmax )
         call zzpdcmpl ( re, f, p, -hpi+latmrg, relmin )

         if ( failed() ) return

         if (       ( relmax /= gt ) &
              .and. ( relmin /= lt )  ) then
!
!           This is the usual case: the latitude of the input point is
!           bounded away from the poles.
!
!           Check the point's longitude against the segment's longitude
!           bounds.
!
!           We'll scale the longitude margin to compensate for the
!           latitude of the input point. Note that the division
!           below is safe; presuming a reasonable value of MARGIN,
!           we know that
!
!              DLON << 1
!
!           Note that we use planetocentric latitude for scaling the
!           longitude margin. This substitution (for planetodetic
!           latitude) is adequate for this purpose.
!
            dlon   =  lonmrg / max( abs(cos(pcnlat)), latmrg )

            amnlon = minlon - dlon
            amxlon = maxlon + dlon

!
!           Now move the input point's longitude into range, if
!           necessary, so we can make a valid comparison against
!           the longitude bounds.
!
            if ( lon < amnlon ) then

               if (  lon  < ( amnlon - lonali )  ) then
!
!                 See whether an aliased version of LON is a match.
!
                  lon = lon + pi2

               else
!
!                 Consider LON to be a match with the lower bound.
!
                  lon = amnlon

               end if

            else if ( lon > amxlon ) then

               if (  lon  > ( amxlon + lonali )  ) then
!
!                 See whether an aliased version of LON is a match.
!
                  lon = lon - pi2

               else
!
!                 Consider LON to be a match with the upper bound.
!
                  lon = amxlon

               end if

            end if

            if ( ( lon < amnlon ) .or. ( lon > amxlon ) ) return

         else

!
!           The latitude of the input point is close to one of the
!           poles.
!
!           This is a no-op case.
!
!           The input point has already passed whichever of the
!           altitude and latitude tests that were not excluded.
!
!           If the element has a non-degenerate latitude boundary
!           having the same sign as the latitude of the input point,
!           and if latitude is excluded because the input point is
!           already nominally on that boundary, then passing the
!           altitude check implies that the point is close to the
!           element.
!
!           If the element has a degenerate latitude boundary having
!           the same sign as the latitude of the input point---namely,
!           the element contains the pole, and latitude is excluded,
!           then then passing the altitude check implies that the point
!           is close to the portion of the Z-axis contained in the
!           element.
!
!           If the altitude check has been excluded because the point
!           is already nominally on one of the element's altitude
!           boundaries, the passing the latitude test implies the point
!           is close to the element.
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

      return
      end
