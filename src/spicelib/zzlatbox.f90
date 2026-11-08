!$Procedure ZZLATBOX (Bounding box for latitudinal volume element)

      subroutine zzlatbox ( bounds, center, lr, lt, lz, radius )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Create a bounding box for a latitudinal volume element.
!
!     The outputs are the box's center, dimensions in the radial,
!     tangential, and vertical directions, and the box's radius.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     GEOMETRY
!     MATH
!
! $ Declarations

      implicit none

      real(wp)              bounds ( 2, 3 )
      real(wp)              center ( 3 )
      real(wp)              lr
      real(wp)              lt
      real(wp)              lz
      real(wp)              radius

      real(wp)              margin
      parameter           ( margin = 1.0e-12_wp )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BOUNDS     I   Array of coordinate bounds for segment.
!     CENTER     O   Center of bounding box.
!     LR         O   Extent of box in the cylindrical radial direction.
!     LT         O   Extent of box in the tangential direction.
!     LZ         O   Extent of box in the Z direction.
!     RADIUS     O   Radius of box.
!     MARGIN     P   Angular margin.
!
! $ Detailed_Input
!
!     BOUNDS     is a 2 x 3 double precision array containing bounds
!                on the latitudinal coordinates of the spatial region
!                (volume element) covered by a DSK segment. The
!                contents of BOUNDS are:
!
!                   BOUNDS(*,1)              Longitude bounds
!                   BOUNDS(*,2)              Latitude bounds
!                   BOUNDS(*,3)              Radius bounds
!
!                Elements (1,*) are lower bounds; elements (2,*) are
!                upper bounds. Angular units are radians.
!
! $ Detailed_Output
!
!     CENTER     is a double precision 3-vector representing the center
!                of a box tangent to and containing the volume
!                specified by BOUNDS. The sides of the box are parallel
!                to the radial, tangential, and Z directions, where the
!                radial direction is a vector orthogonal to the Z axis,
!                having longitude equal to the midpoint of the
!                segment's longitude range. The tangential direction is
!                orthogonal to the radial and Z directions.
!
!     LR,
!     LT,
!     LZ         are, respectively, the extents (edge lengths) of the
!                bounding box in the radial, tangential, and Z
!                directions.
!
!     RADIUS     is the radius of the sphere that circumscribes the
!                box. RADIUS is equal to the length of a line segment
!                connecting the center of the box to any corner.
!
! $ Parameters
!
!     MARGIN     is used for latitude range validity tests. Latitude
!                values can be out of range by as much as MARGIN.
!
! $ Exceptions
!
!     1) If the minimum longitude exceeds the maximum by more than
!        2*pi, the error SPICE(BADLONGITUDERANGE) is signaled.
!
!     2) If the latitude bounds are out of order, the error
!        SPICE(BADLATITUDEBOUNDS) is signaled.
!
!     3) If the minimum latitude is less than -pi/2 - MARGIN, or
!        if the maximum latitude is greater than pi/2 + MARGIN,
!        the error SPICE(BADLATITUDERANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides a simple way to compute the center and
!     radius for an outer bounding sphere for the volume covered
!     by a DSK segment using latitudinal coordinates.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 03-FEB-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     bounding box for latitudinal volume element
!
!-&

!
!     SPICELIB functions
!
      real(wp)              halfpi
      real(wp)              twopi
      real(wp)              vnorm

      logical               return

!
!     Local parameters
!

!
!     Local variables
!
      real(wp)              diag   ( 3 )
      real(wp)              hdlon
      real(wp)              inrad
      real(wp)              maxlat
      real(wp)              maxlon
      real(wp)              maxr
      real(wp)              maxz
      real(wp)              midlon
      real(wp)              midr
      real(wp)              minlat
      real(wp)              minlon
      real(wp)              minr
      real(wp)              minz
      real(wp)              outrad

!
!     This routine uses discovery check-in. We check RETURN in order to
!     avoid performing math operations using invalid operands.
!
      if ( return() ) then
         return
      end if

!
!     Get local copies of the bounds of the volume element.
!
      minlon = bounds(1,1)
      maxlon = bounds(2,1)

      if ( maxlon <= minlon ) then
         maxlon = maxlon + twopi()
      end if

      if ( maxlon <= minlon) then

         call chkin  ( 'ZZLATBOX'                               )
         call setmsg ( 'Longitude bounds are #:#. Minimum ' &
         //            'longitude exceeds maximum by more than ' &
         //            '2 pi.'                                  )
         call errdp  ( '#', minlon                              )
         call errdp  ( '#', bounds(2,1)                         )
         call sigerr ( 'SPICE(BADLONGITUDERANGE)'               )
         call chkout ( 'ZZLATBOX'                               )
         return

      end if

      minlat = bounds(1,2)
      maxlat = bounds(2,2)

      minr   = bounds(1,3)
      maxr   = bounds(2,3)

      if ( minlat > maxlat ) then

         call chkin  ( 'ZZLATBOX'                              )
         call setmsg ( 'Latitude bounds #:# are out of order.' )
         call errdp  ( '#', minlat                             )
         call errdp  ( '#', maxlat                             )
         call sigerr ( 'SPICE(BADLATITUDEBOUNDS)'              )
         call chkout ( 'ZZLATBOX'                              )
         return

      end if

      if ( minlat < ( -halfpi() - margin )  ) then

         call chkin  ( 'ZZLATBOX'                               )
         call setmsg ( 'Minimum latitude # is less than -pi/2.' )
         call errdp  ( '#', minlat                              )
         call sigerr ( 'SPICE(BADLATITUDERANGE)'                )
         call chkout ( 'ZZLATBOX'                               )
         return

      end if

      if ( maxlat > ( halfpi() + margin )  ) then

         call chkin  ( 'ZZLATBOX'                               )
         call setmsg ( 'Maximum latitude # is more than -pi/2.' )
         call errdp  ( '#', maxlat                              )
         call sigerr ( 'SPICE(BADLATITUDERANGE)'                )
         call chkout ( 'ZZLATBOX'                               )
         return

      end if

      minlat = max( minlat, -halfpi() )
      maxlat = min( maxlat,  halfpi() )

!
!     Let INRAD and OUTRAD be, respectively, the radii of the
!     orthogonal projections onto the X-Y plane of the element's arcs
!     of minimum and maximum distance from the Z axis.
!
!     If the element lies on or above the X-Y plane, the outer radius
!     is that of the lower latitude bound on the outer bounding sphere,
!     and the inner radius is that of the upper latitude bound on the
!     inner bounding sphere.
!
!     These relationships are reversed for elements that lie on or
!     below the X-Y plane.
!
!     For elements that span the X-Y plane, the outer radius is that of
!     the outer bounding sphere. The inner radius is that of the
!     latitude circle on the inner bounding sphere for which the
!     absolute value of the latitude is maximum.
!
      if ( minlat >= 0.0_wp ) then

         outrad = cos( minlat ) * maxr
         inrad  = cos( maxlat ) * minr

      else if ( maxlat <= 0.0_wp ) then

         outrad = cos( maxlat ) * maxr
         inrad  = cos( minlat ) * minr

      else

         outrad = maxr
         inrad  = cos(  max( abs(maxlat), abs(minlat) )  ) * minr

      end if

!
!     Let MIDLON be the longitude of the midpoint of the element's
!     longitude coverage. Let HDLON be one half of the extent of the
!     longitude coverage.
!
      hdlon  = ( maxlon - minlon ) / 2
      midlon =   minlon + hdlon

!
!     LR is the length of the bounding box in the radial direction,
!     where "radius" is defined as the distance from the Z axis.
!
      if ( hdlon <= halfpi() ) then

         lr = outrad - ( inrad * cos(hdlon) )

      else

         lr = ( 1.0_wp - cos(hdlon) ) * outrad

      end if

!
!     The tangential length of bounding box depends on the longitude
!     extent. For any extent larger than Pi radians, the width
!     is just that of the outer radius.
!
      if ( hdlon > halfpi() ) then

         lt = 2.0_wp * outrad
      else
         lt = 2.0_wp * outrad * sin( hdlon )
      end if

!
!     The height bounds are derived from the lowest and highest points
!     on the element.
!
      if ( minlat >= 0.0_wp ) then

         maxz = maxr * sin(maxlat)
         minz = minr * sin(minlat)

      else if ( maxlat <= 0.0_wp ) then

         maxz = minr * sin(maxlat)
         minz = maxr * sin(minlat)

      else

         maxz = maxr * sin(maxlat)
         minz = maxr * sin(minlat)

      end if

      lz = maxz - minz

!
!     Make sure all dimensions are non-negative.
!
      lr = max( 0.0_wp, lr )
      lt = max( 0.0_wp, lt )
      lz = max( 0.0_wp, lz )

!
!     Compute the coordinates of the center of the box.
!
!     The box center lies on the meridian of central
!     longitude. The outer tangential edge is at radius
!     OUTRAD. Let MIDR be the radius of the center.
!
      midr = outrad - (lr/2)

      call cylrec ( midr, midlon, minz+(lz/2), center )

!
!     The radius is the distance from the center of the box
!     to any corner.
!
      call vpack ( lr/2, lt/2, lz/2, diag )

      radius = vnorm( diag )

      return
      end
