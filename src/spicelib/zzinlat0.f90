!$Procedure ZZINLAT0 ( DSK, in latitudinal element, w/o margin? )

      subroutine zzinlat0 ( r, lon, lat, bounds, exclud, inside )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Test a point represented by a set of latitudinal coordinates for
!     inclusion in a specified latitudinal volume element. The test is
!     performed without using the "greedy" margins for the element.
!     The built-in angular rounding tolerance is still used for the
!     longitude comparison.
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

      double precision      r
      double precision      lon
      double precision      lat
      double precision      bounds ( 2, 3 )
      integer               exclud
      logical               inside

      integer               none
      parameter           ( none   = 0 )

      integer               lonidx
      parameter           ( lonidx = 1 )

      integer               latidx
      parameter           ( latidx = 2 )

      integer               radidx
      parameter           ( radidx = 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     R          I   Radius of point.
!     LON        I   Planetocentric longitude of point.
!     LAT        I   Planetocentric latitude of point.
!     BOUNDS     I   Coordinate bounds of element.
!     EXCLUD     I   Index of coordinate to exclude from test.
!     INSIDE     O   Flag indicating whether point is in element.
!     ANGMRG     P   Angular rounding margin.
!     LONALI     P   Longitude shift margin.
!     NONE       P   Meaning: exclude nothing.
!     LONIDX     P   Longitude index. Also used for exclusion.
!     LATIDX     P   Latitude index. Also used for exclusion.
!     RADIDX     P   Radius index. Also used for exclusion.
!
! $ Detailed_Input
!
!     R,
!     LON,
!     LAT        are, respectively, the radius, longitude, and
!                latitude of a point. The point is to be checked
!                to determine whether it is inside the volume
!                element specified by BOUNDS.
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
!                The aliasing margin LONALI and the angular rounding
!                margin ANGMRG are used for longitude comparisons.
!                However, the greedy margin is not used. See the
!                Parameters section below for further information.
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
!     INSIDE     is a logical flag that is set to .TRUE. if and only if
!                the input coordinates represent a point inside or on
!                the surface of the volume element, according to the
!                comparisons that are performed.
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
!                See dsktol.inc for details.
!
!     ANGMRG     is an angular rounding margin. If the input longitude
!                is outside of the longitude bounds by less than this
!                amount, the longitude is considered to be within the
!                bounds.
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
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Note that a margin is used for longitude shifting, although
!     none is used to expand the volume element.
!
! $ Examples
!
!     See usage in ZZINLAT.
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
!-    SPICELIB Version 1.0.0, 07-AUG-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     test point against latitudinal element without margin
!
!-&


!
!     SPICELIB functions
!
      double precision      twopi

!
!     Local parameters
!
      integer               lower
      parameter           ( lower = 1 )

      integer               upper
      parameter           ( upper = 2 )

!
!     Local variables
!
      double precision      loclon
      double precision      maxlat
      double precision      maxlon
      double precision      maxr
      double precision      minlat
      double precision      minlon
      double precision      minr
      double precision      pi2

      logical               first

!
!     Saved variables
!
      save                  first
      save                  pi2

!
!     Initial values
!
      data                  first / .true. /
      data                  pi2   / -1.d0  /



      if ( first ) then

         pi2   = twopi()
         first = .false.

      end if

!
!     Assume the point is outside to start. This allows us
!     to skip setting INSIDE when we find a boundary test
!     failure.
!
      inside = .false.
!
!     Compare coordinates of the input point and the segment
!     bounds.
!
!     Special case: if the input point is at the origin, and the
!     volume element contains the origin, the point is inside.
!
      if ( r .eq. 0.d0 ) then

         if ( bounds(lower, radidx) .eq. 0.d0 ) then

            inside = .true.

            return

         end if

      end if



      if ( exclud .ne. radidx ) then
!
!        Compare the point's radius to the segment's radius bounds.
!
         minr = bounds(lower, radidx)
         maxr = bounds(upper, radidx)

         if ( ( r .lt. minr ) .or. ( r .gt. maxr ) ) then
!
!           The point's radius is outside of the segment's range.
!
            return

         end if

      end if


      if ( exclud .ne. latidx ) then
!
!        Compare the point's latitude to the segment's latitude bounds.
!
         minlat = bounds(lower, latidx)
         maxlat = bounds(upper, latidx)

         if ( ( lat .lt. minlat ) .or. ( lat .gt. maxlat ) ) then
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
      if ( exclud .ne. lonidx ) then
!
!        Put the local longitude bounds in order, if necessary.
!
         call zznrmlon ( bounds(1,lonidx),  bounds(2,lonidx), &
                         angmrg,   minlon,  maxlon           )
!
!        Compare the point's longitude to the segment's longitude
!        bounds.
!
         loclon = lon


         if ( lon .lt. ( minlon - lonali ) ) then
!
!           If the point's longitude is less than the segment's
!           longitude by more than a small margin, shift the longitude
!           right by 2*pi.

            loclon = lon + pi2

         else if ( lon .gt. ( maxlon + lonali ) ) then
!
!           If the point's longitude is greater than the segment's
!           longitude by more than a small margin, shift the longitude
!           left by 2*pi.

            loclon = lon - pi2

         end if


         if (        ( loclon .lt. (minlon-angmrg) ) &
                .or. ( loclon .gt. (maxlon+angmrg) )  ) then
!
!           The point's longitude, adjusted if necessary for
!           comparison, is outside of the segment's range.
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
