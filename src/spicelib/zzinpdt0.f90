!$Procedure ZZINPDT0 ( DSK, in planetodetic element, w/o margin? )

      subroutine zzinpdt0 ( p, lon, bounds, corpar, exclud, inside )
      use pepper_kinds, only: ip, wp
      use pepper_constants, only: twopi

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Test a point represented by a set of planetodetic coordinates for
!     inclusion in a specified planetodetic volume element. Ellipsoidal
!     surfaces are used in place of surfaces of constant altitude. The
!     test is performed without using the "greedy" margins for the
!     element. The built-in angular rounding tolerance is still used
!     for the longitude comparison.
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
      real(wp)              lon
      real(wp)              bounds ( 2, 3 )
      real(wp)              corpar ( * )
      integer(ip)           exclud
      logical               inside


      integer(ip), parameter :: none = 0

      integer(ip), parameter :: lonidx = 1

      integer(ip), parameter :: latidx = 2

      integer(ip), parameter :: altidx = 3

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     P          I   Cartesian coordinates of point.
!     LON        I   Planetocentric longitude of point.
!     BOUNDS     I   Coordinate bounds of element.
!     CORPAR     I   Coordinate parameters.
!     EXCLUD     I   Index of coordinate to exclude from test.
!     INSIDE     O   Flag indicating whether point is in element.
!     ANGMRG     P   Angular rounding margin.
!     LONALI     P   Longitude alias margin.
!     NONE       P   Meaning: exclude nothing.
!     LONIDX     P   Longitude index. Also used for exclusion.
!     LATIDX     P   Latitude index. Also used for exclusion.
!     ALTIDX     P   Altitude index. Also used for exclusion.
!
! $ Detailed_Input
!
!     P          is a point expressed in Cartesian coordinates. The
!                reference frame is that of the segment of interest.
!                The point is to be checked to determine whether it is
!                inside the volume element specified by BOUNDS.
!
!
!     LON        is the planetocentric longitude of the point P.
!
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
!                If necessary, the longitude bounds are normalized
!                for the comparison. The normalized bounds differ
!                by no more than 2*pi and are in increasing order.
!                See ZZNRMLON for details.
!
!                The aliasing margin LONALI and the angular rounding
!                margin ANGMRG are used for longitude comparisons.
!                However, the greedy margin is not used. See the
!                Parameters section below for further information.
!
!
!     CORPAR     is an array of coordinate parameters; normally these
!                are obtained from a DSK descriptor. The first element
!                is the equatorial radius of the reference spheroid.
!                The second element is the flattening coefficient.
!                Any other elements are ignored by this routine.
!
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
!                coordinate. For example, if a point is on the surface
!                of altitude equal to the upper altitude bound,
!                altitude need not be used in the comparison and in
!                fact can't be meaningfully compared, due to round-off
!                errors.
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
!                by more than this amount, the value LON + 2*pi is used
!                in the longitude comparison. If the input longitude
!                exceeds the upper longitude bound by this amount, the
!                value LON - 2*pi is used in the comparison.
!
!     ANGMRG     is an angular rounding margin. If the input longitude
!                is outside of the longitude bounds by less than this
!                amount, the longitude is considered to be within the
!                bounds. The margin is used analogously for latitude.
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
!     ALTIDX     is the index of altitude in the second dimension of
!                BOUNDS. When used as a value of the input argument
!                EXCLUD, indicates that altitude should be excluded
!                from comparison.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine uses ellipsoidal bounding surfaces, in place of
!     surfaces of constant altitude, to represent the "outer" and
!     "inner" segment boundaries. These surfaces must be consistent
!     with those used by the ray-planetodetic volume element intercept
!     routine ZZRYTPDT.
!
!     The radii of these ellipsoidal surfaces are those produced by
!     the routine ZZELLBDS.
!
!     Note that a margin is used for longitude shifting, although
!     none is used to expand the volume element.
!
! $ Examples
!
!     See usage in ZZINPDT.
!
! $ Restrictions
!
!     1)  Validity of the ellipsoid shape parameters must be ensured by
!         the caller.
!
!     2)  This is a private routine. It is meant to be used only by the
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
!-    SPICELIB Version 1.0.0, 16-AUG-2016 (NJB)
!
!        Now uses ANGMRG for latitude comparisons.
!
!        25-MAY-2016 (NJB)
!
!           Original version.
!
!-&

! $ Index_Entries
!
!     test point against planetodetic element without margin
!
!-&


!
!     SPICELIB functions
!
      real(wp)              halfpi

      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: lower = 1

      integer(ip), parameter :: upper = 2

!
!     Numeric relation codes returned by ZZPDCMPL:
!
      integer(ip), parameter :: lt = -1

      integer(ip), parameter :: eq = 0

      integer(ip), parameter :: gt = 1


!
!     Local variables
!
      real(wp)              emax
      real(wp)              emin
      real(wp)              f
      real(wp)              level
      real(wp)              loclon
      real(wp)              maxlat
      real(wp)              maxlon
      real(wp)              maxalt
      real(wp)              minlat
      real(wp)              minlon
      real(wp)              minalt
      real(wp)              pmax
      real(wp)              pmin
      real(wp)              re
      real(wp)              rp

      integer(ip)           maxrel
      integer(ip)           minrel




!
!     Saved variables
!

!
!     Initial values
!
!
!     Constants (SPICELIB computed them on the first call).
!
      real(wp), parameter :: pi2 = twopi


      if ( return() ) return



!
!     Unpack the shape parameters. Set the polar axis length for
!     later use.
!
      re = corpar(1)
      f  = corpar(2)
      rp = re * ( 1.0_wp - f )

!
!     Assume the point is outside to start. This allows us
!     to skip setting INSIDE when we find a boundary test
!     failure.
!
      inside = .false.
!
!     Compare coordinates of the input point and the segment
!     bounds. Deal with altitude last, since it involves the
!     most work. We may be able to exit before performing the
!     altitude tests.
!
      if ( exclud /= latidx ) then
!
!        Compare the point's latitude to the segment's latitude bounds.
!
         minlat = max( bounds(lower, latidx) - angmrg, -halfpi() )
         maxlat = min( bounds(upper, latidx) + angmrg,  halfpi() )

         call zzpdcmpl ( re, f, p, minlat, minrel )
         call zzpdcmpl ( re, f, p, maxlat, maxrel )

         if ( failed() ) return


         if (      ( minrel == lt ) &
              .or. ( maxrel == gt )  ) then
!
!           The point's latitude is outside of the segment's range.
!
            return

         end if

      end if

!
!     Move the longitude of the input point into the interval
!
!        [ MINLON, MAXLON ]
!
!     if necessary and if possible.
!
      if ( exclud /= lonidx ) then
!
!        Put the local longitude bounds in order, if necessary.
!
         call zznrmlon ( bounds(lower,lonidx),  bounds(upper,lonidx), &
                         angmrg,       minlon,  maxlon               )
!
!        Compare the point's longitude to the segment's longitude
!        bounds.
!
         loclon = lon

         if ( lon < ( minlon - lonali ) ) then
!
!           If the point's longitude is less than the segment's
!           longitude by more than a small margin, shift the longitude
!           right by 2*pi.

            loclon = lon + pi2

         else if ( lon > ( maxlon + lonali ) ) then
!
!           If the point's longitude is greater than the segment's
!           longitude by more than a small margin, shift the longitude
!           left by 2*pi.

            loclon = lon - pi2

         end if


         if (        ( loclon < (minlon-angmrg) ) &
                .or. ( loclon > (maxlon+angmrg) )  ) then
!
!           The point's longitude, adjusted if necessary for
!           comparison, is outside of the segment's range.
!
            return

         end if


      end if

      if ( exclud /= altidx ) then
!
!        Extract altitude bounds from the segment descriptor.
!
         minalt = bounds(lower, altidx)
         maxalt = bounds(upper, altidx)
!
!        Find the semi-axes of the bounding spheroids.
!
         if ( f >= 0.0_wp ) then
!
!           This is the oblate case. Get the semi-axis lengths
!           for the inner and outer bounding spheroids.
!
            call zzellbds ( re,   rp,   maxalt, minalt, &
                            emax, pmax, emin,   pmin   )

         else
!
!           This is the prolate case. RP is the longer axis. Get the
!           semi-axis lengths for the inner and outer bounding
!           spheroids.
!
!           In this call, we'll store the radii associated with
!           the longer axis in PMAX and PMIN.
!
            call zzellbds ( rp,   re,   maxalt, minalt, &
                            pmax, emax, pmin,   emin   )

         end if

!
!        Compute the input point's level surface parameters
!        for the inner and outer bounding ellipsoids. Do these
!        computations one at a time, so the second one can be
!        skipped if the first one shows the point is outside
!        the outer ellipsoid.
!
         level = ( p(1)/emax )**2 + ( p(2)/emax )**2 + ( p(3)/pmax )**2

         if ( level > 1.0_wp ) then
!
!           The point is outside of the outer bounding ellipsoid.
!
            return

         end if


         level = ( p(1)/emin )**2 + ( p(2)/emin )**2 + ( p(3)/pmin )**2

         if ( level < 1.0_wp ) then
!
!           The point is inside the inner bounding ellipsoid, which
!           implies it is outside of the segment's boundaries.
!
            return

         end if

      end if

!
!     Getting to this point means the input point is inside
!     the segment. Being on the boundary counts as inside.
!
      inside = .true.


      return
      end
