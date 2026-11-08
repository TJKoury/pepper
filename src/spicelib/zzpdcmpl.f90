!$Procedure ZZPDCMPL (Planetodetic coordinates, compare latitudes )

      subroutine zzpdcmpl ( re, f, p, lat, rel )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Compare the planetodetic latitude of a point in 3-dimensional
!     space against a specified value, without converting the point to
!     planetodetic coordinates.
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
      integer(ip)           rel

      integer(ip), parameter :: lt = -1

      integer(ip), parameter :: eq = 0

      integer(ip), parameter :: gt = 1

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     RE         I   Equatorial radius.
!     F          I   Flattening coefficient.
!     P          I   Three-dimensional point.
!     LAT        I   Planetodetic latitude.
!     REL        O   Relation code.
!     LT         P   Code indicating latitude of P < LAT.
!     EQ         P   Code indicating latitude of P = LAT.
!     GT         P   Code indicating latitude of P > LAT.
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
!
!     P          is a point (equivalently, a vector) in
!                three-dimensional space. P is expressed in Cartesian
!                coordinates.
!
!                The units of P must be consistent with those of RE.
!
!
!     LAT        is a planetodetic latitude value to be compared
!                against the planetodetic latitude of P. Units
!                are radians.
!
! $ Detailed_Output
!
!     REL        is an integer code that indicates the order
!                relation between the planetodetic latitude of P
!                and LAT. The planetodetic coordinate system is
!                defined by the inputs RE and F.
!
!                The code <rel> indicates that the relation
!
!                   <latitude of P>  <rel>  LAT
!
!                is true. See the Parameters section below for
!                the parameter names.
!
!
! $ Parameters
!
!     LT,
!     EQ,
!     GT         are, respectively, codes indicating the relationship
!                of the planetodetic latitude of the input vector to
!                the input latitude value. Let LP represent the
!                planetodetic latitude of P.
!
!                   Code LT indicates   LP < LAT
!                   Code EQ indicates   LP = LAT
!                   Code GT indicates   LP > LAT
!
! $ Exceptions
!
!     1)  If either the equatorial radius or flattening coefficient
!         is invalid, the error will be signaled by a routine in the
!         call tree of this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine performs a planetodetic latitude comparison more
!     efficiently than can be done using a rectangular-to-planetodetic
!     coordinate conversion.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     There are some cases for which this routine cannot be applied.
!     See the SPICELIB routine ZZPDPLTC and its usage in ZZRYTPDT.
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
!-    SPICELIB Version 1.0.0, 19-JAN-2017 (NJB)
!
!       Original version 22-AUG-2015 (NJB)
!
!-&

! $ Index_Entries
!
!     compare planetodetic latitude of vector against value
!     compare planetodetic latitude of point against value
!
!-&

!
!     SPICELIB functions
!
      real(wp)              halfpi

      logical               failed
      logical               return

!
!     Local variables
!
      real(wp)              apex   ( 3 )
      real(wp)              lon
      real(wp)              offpcl
      real(wp)              offset ( 3 )
      real(wp)              r
      real(wp)              rp
      real(wp)              xincpt
      real(wp)              yincpt

!
!     Saved variables
!
      save                  apex

!
!     Initial values
!
      data                  apex / 3  *  0.0_wp       /


      if ( return() ) return


!
!     Treat points on the Z axis as a special case. The
!     computations performed in the general case may introduce
!     round-off errors that will lead to false results for
!     this case.
!
      if (  ( p(1) == 0.0_wp ) .and. ( p(2) == 0.0_wp )  ) then

         if ( p(3) > 0.0_wp ) then

            if ( lat == halfpi() ) then

               rel = eq
            else
               rel = gt
            end if

         else if ( p(3) == 0.0_wp ) then
!
!           We consider the latitude of P to be zero.
!
            if ( lat > 0.0_wp ) then

               rel = lt

            else if ( lat == 0.0_wp ) then

               rel = eq
            else
               rel = gt
            end if

         else
!
!           P(3) < 0.
!
            if ( lat == -halfpi() ) then

               rel = eq
            else
               rel = lt
            end if

         end if

         return

      end if

!
!     Latitude zero is a special case. The planetodetic latitude of the
!     input point has the same sign as the Z component of the point.
!
      rp = re * ( 1.0_wp - f )
!
!     Get the y-intercept of the latitude cone for LAT. Note that a
!     result is defined for LAT = +/- pi/2.
!
      call zzelnaxx ( re, rp, lat, xincpt, yincpt )

      if ( failed() ) return

!
!     Ideally YINCPT is zero if and only if LAT is zero.
!     We'll group these conditions together.
!
      if (  ( lat == 0.0_wp ) .or. ( yincpt == 0.0_wp )  ) then

         if ( p(3) > 0.0_wp ) then

            rel = gt

         else if ( p(3) == 0.0_wp ) then

            rel = eq
         else
            rel = lt
         end if

         return

      end if

!
!     This is the normal case.
!
!     Find the offset of the point from the latitude cone's apex.
!     Create a unit-length copy of the offset vector.
!
      apex(3) = yincpt

      call vsub ( p, apex, offset )

!     We'll use the planetocentric [sic] latitude of the offset
!     vector for comparison.
!
      call reclat ( offset, r, lon, offpcl )


      if ( lat > 0.0_wp ) then

         if ( yincpt > 0 ) then
!
!           This is the prolate case.
!
            if ( offpcl > lat ) then

               rel = gt

            else if ( offpcl == lat ) then

               rel = eq
            else
               rel = lt
            end if

         else
!
!           YINCPT = 0 was handled previously, so YINCPT < 0.
!
!           This is the oblate case.
!
!           In addition to the comparison of angles, we need to know
!           the input point is above the X-Y plane in order for the
!           GT or EQ relations to hold.
!
            if ( p(3) > 0.0_wp ) then

               if ( offpcl > lat ) then

                  rel = gt

               else if ( offpcl == lat ) then

                  rel = eq
               else
                  rel = lt
               end if

            else
!
!              The input latitude is positive, while the point
!              is on or below the X-Y plane.
!
               rel = lt

            end if

         end if


      else
!
!        LAT < 0, since the case LAT = 0 has already been handled.
!
         if ( yincpt < 0.0_wp ) then
!
!           This is the prolate case.
!
            if ( offpcl > lat ) then

               rel = gt

            else if ( offpcl == lat ) then

               rel = eq
            else
               rel = lt
            end if

         else
!
!           YINCPT > 0, since the case YINCPT = 0 was handled
!           previously.
!
!           This is the oblate case.
!
            if ( p(3) < 0.0_wp ) then

               if ( offpcl > lat ) then

                  rel = gt

               else if ( offpcl == lat ) then

                  rel = eq
               else
                  rel = lt
               end if

            else
!
!              The input latitude is negative, while the point
!              is on or above the X-Y plane.
!
               rel = gt

            end if

         end if

      end if

      return
      end
