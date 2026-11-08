!$Procedure ZZPDPLTC (Planetodetic coordinates, point latitude check)

      logical function zzpdpltc ( re, f, p, lat )
      use pepper_kinds, only: wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Indicate whether a given point on a planetodetic latitude cone
!     has the correct latitude sign.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     COORDINATES
!     GEOMETRY
!     PLANETODETIC
!     LATITUDE
!     MATH
!
! $ Declarations

      implicit none

      real(wp)              re
      real(wp)              f
      real(wp)              p   ( 3 )
      real(wp)              lat

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     RE         I   Equatorial radius.
!     F          I   Flattening coefficient.
!     P          I   Three-dimensional point.
!     LAT        I   Planetodetic latitude.
!
!     The function returns .TRUE. if the sign of the planetodetic
!     latitude of the input point matches that of the input
!     planetodetic latitude.
!
! $ Detailed_Input
!
!     RE,
!     F          are, respectively, the equatorial radius
!                and flattening coefficient of a biaxial
!                spheroid.
!
!                The polar radius RP of the spheroid is
!
!                   RP = RE * ( 1 - F )
!
!                RP may be less than, equal to, or greater than RE.
!
!     P          is a point (equivalently, a vector) in
!                three-dimensional space. P is expressed in Cartesian
!                coordinates. P must lie on the planetodetic
!                latitude cone corresponding to LAT.
!
!                The units of P must be consistent with those of RE.
!
!     LAT        is a planetodetic latitude value defining a cone.
!                Units are radius.
!
! $ Detailed_Output
!
!     The function returns .TRUE. if any of the following
!     are true:
!
!        - The input spheroid is prolate or spherical.
!
!        - The input latitude is zero.
!
!        - The input point is determined to have planetodetic
!          latitude of the same sign as the input latitude.
!
!    Otherwise the function returns .FALSE.
!
!    See Particulars below for details.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If either the equatorial radius or flattening coefficient is
!         invalid, the error SPICE(VALUEOUTOFRANGE) will be signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports computation of the intersection between
!     a ray and a planetodetic volume element. The routine is used
!     to determine whether the intersection of the ray and latitude
!     cone is actually on the element's boundary.
!
!     This function serves as a "macro" that executes a logical test
!     that would be awkward to perform in-line.
!
!     For a given reference spheroid, all points having a given
!     planetodetic latitude lie on a cone. The axis of the cone
!     coincides with the Z axis of the coordinate system.
!
!     The possibility that a point on a given latitude can have a Z
!     coordinate with the wrong sign exists for oblate reference
!     spheroids. For these shapes, the vertex of a positive latitude
!     cone has a negative Z component, and the vertex of a negative
!     latitude cone has a positive Z component.
!
!     The purpose of the function is to determine, for a point that
!     lies on a given planetodetic latitude cone, whether that point is
!     on the correct side of the X-Y plane: that is, the side
!     corresponding to the sign of the input latitude value.
!
!     This check is not as simple as checking the sign of the Z
!     component of the input point. For values of LAT that are non-zero
!     but have very small magnitude, points that are nominally on the
!     corresponding latitude cone may have Z components of the wrong
!     sign due to round-off errors that occurred in the process of
!     computing those points. For such cases, the input point is
!     checked by comparing its distance from the Z axis to the X
!     intercept of a line normal to the reference spheroid at a point
!     having planetodetic latitude LAT and longitude 0. This check can
!     be performed accurately even when the Z component of the input
!     point consists of noise.
!
!     This routine does not test the input point to determine whether
!     it lies on the latitude cone defined by the input argument LAT.
!     The caller must ensure that this is the case.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     This routine does not test the input point to determine whether
!     it lies on the latitude cone defined by the input argument LAT.
!     The caller must ensure that this is the case.
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
!-    SPICELIB Version 1.0.0, 13-JAN-2017 (NJB)
!
!-&

! $ Index_Entries
!
!     is point on planetodetic latitude boundary
!
!-&

!
!     SPICELIB functions
!
      logical               failed
      logical               return

!
!     Local parameters
!
      real(wp), parameter :: limit = 1.0e-2_wp

!
!     Local variables
!
      real(wp)              a
      real(wp)              b
      real(wp)              r
      real(wp)              r2
      real(wp)              xxpt
      real(wp)              yxpt

!
!     Give the function a default value.
!
      zzpdpltc = .false.

      if ( return() ) return

!
!     The equatorial radius must be greater than zero.
!
      if ( re <= 0.0_wp ) then

          call setmsg ( 'Equatorial radius was *.' )
          call errdp  ( '*', re                    )
          call sigerr ( 'SPICE(VALUEOUTOFRANGE)'   )
          return

      end if

!
!     If the flattening coefficient is greater than one, the polar
!     radius computed below is negative. If it's equal to one, the
!     polar radius is zero. Either case is a problem, so signal an
!     error.
!
      if ( f >= 1 ) then

          call setmsg ( 'Flattening coefficient was *.'  )
          call errdp  ( '*', f                           )
          call sigerr ( 'SPICE(VALUEOUTOFRANGE)'         )
          return

      end if

!
!     The input point is assumed to be on the cone
!     corresponding to the input latitude.
!
!     If the reference spheroid is prolate or spherical, there's
!     nothing to do: the point is automatically on the correct side of
!     the X-Y plane.
!
      if ( f <= 0.0_wp ) then

         zzpdpltc = .true.

      else
!
!        This is the oblate case.
!
!        If the point is on the "correct" side of the X-Y plane---
!        that is, its Z component as the same sign as LAT, the
!        point is considered to have the correct latitude.
!
!        If the point is on the X-Y plane, or if LAT is zero, the point
!        is considered to have the indicated latitude. We condense
!        these cases by requiring that
!
!              LAT * P(3) >= 0
!
!           rather than
!
!              LAT * P(3) > 0
!
         if (  ( p(3) * lat )  >=  0.0_wp  ) then

            zzpdpltc = .true.

         else if ( abs(lat) >= limit ) then
!
!           Ideally, the input point is considered to have the given
!           latitude if the point is on the side of the X-Y plane
!           corresponding to the sign of the input latitude. The
!           problem with this criterion is that it can't be applied
!           correctly when LAT has very small magnitude.
!
!           If the magnitude of LAT is above the limit, it's ok to
!           use the sign of P(3) to determine whether the point
!           has the given latitude.
!
!           The point has the indicated latitude if both LAT and P(3)
!           have the same sign. In this case, we know they have the
!           opposite sign.
!
            zzpdpltc = .false.

         else
!
!           At this point we know LAT is non-zero, so the cone
!           corresponding to LAT has its vertex on the opposite side of
!           the X-Y plane from any point having latitude LAT. So it's
!           possible for a point to be on the cone but not have the
!           correct latitude.
!
!           We're in the special case where the point's Z component
!           has the opposite sign as LAT, and the magnitude of LAT
!           is below the limit. We don't automatically reject the
!           point in this case: we'll accept it if it is far enough
!           from the Z axis to be outside the portion of the latitude
!           cone on the wrong side of the X-Y plane.
!
            a = re
            b = a * ( 1.0_wp - f )

!
!           Compute the intercepts of a normal vector of a point
!           at latitude LAT, longitude 0, with the X and Y axes.
!
            call zzelnaxx ( a, b, lat, xxpt, yxpt )

            if ( failed() ) return

!
!           We check the point's distance from the Z axis. This can be
!           done accurately even when the Z component of P consists of
!           noise.
!
!           The point is considered to have the correct latitude when
!           it is farther from the Z axis than the intercept on the X
!           axis of a normal line passing through a point having
!           latitude LAT and longitude 0. Ideally, a point that is on
!           the latitude cone and that satisfies this criterion must be
!           on the correct side of the X-Y plane.
!

            r2       = ( p(1) * p(1) ) +  ( p(2) * p(2) )

            r        = sqrt(  max( r2, 0.0_wp )  )

            zzpdpltc = r >= xxpt

         end if

      end if

      return
      end
