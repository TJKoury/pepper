!$Procedure ZZDSKSGR ( DSK, return segment bounding radius )

      real(wp) function zzdsksgr ( dskdsc )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return a radius of a bounding sphere for the spatial region
!     described by a DSK segment descriptor. The radius is measured
!     from the origin of the coordinate system associated from the
!     segment.
!
! $ Required_Reading
!
!     DAS
!     DSK
!
! $ Keywords
!
!     DSK
!     GEOMETRY
!     UTILITY
!
! $ Declarations

      implicit none

      include 'dskdsc.inc'

      real(wp)              dskdsc ( * )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DSKDSC     I   is a DSK descriptor.
!
!     The function returns the maximum radius of the segment.
!
! $ Detailed_Input
!
!     DSKDSC         is a DSK descriptor.
!
! $ Detailed_Output
!
!     The function returns the maximum radius attained on the
!     boundary of the coverage region of the segment associated
!     with the input descriptor. The radius is measured from
!     the origin of the coordinate system in which the coverage
!     bounds are given.
!
!     Units are km.
!
! $ Parameters
!
!     See the INCLUDE file dskdsc.inc for parameter declarations and
!     documentation.
!
! $ Exceptions
!
!     1)  If the coordinate system code in the descriptor is not
!         recognized, the error SPICE(NOTSUPPORTED) is signaled.
!
!     2)  If an invalid radius is found, the error
!         SPICE(INVALIDVALUE) is signaled.
!
!     3)  If an invalid flattening coefficient is found, the error
!         SPICE(INVALIDVALUE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes an upper bound on the radius of
!     the coverage region of a DSK segment. The radius at
!     a point on the boundary of the coverage region is
!     measured from the origin of the coordinate system.
!
!     This routine can be used to generate a bounding sphere for
!     a surface described by one or more DSK segments. Note that
!     the DSK segments describing the surface of an object don't
!     necessarily have a common central body, so offsets between
!     segments' central bodies must be taken into account when
!     computing a bounding sphere.
!
! $ Examples
!
!     1) Get a bounding radius for a DSK segment using the
!        latitudinal coordinate system.
!
!        Example code begins here.
!
!                 PROGRAM DSKGSR_EX1
!                 IMPLICIT NONE
!
!                 INCLUDE 'dskdsc.inc'
!
!                 DOUBLE PRECISION      ZZDSKSGR
!                 DOUBLE PRECISION      PI
!
!                 DOUBLE PRECISION      DSKDSC( DSKDSZ )
!
!           C
!           C     Latitudinal coordinates:
!           C
!                 CALL CLEARD( DSKDSZ, DSKDSC )
!
!                 DSKDSC( SYSIDX ) = LATSYS
!
!           C
!           C     Fill in the coordinate bounds in the descriptor.
!           C
!           C     Longitude:
!           C
!                 DSKDSC(MN1IDX) =  - PI()/2
!                 DSKDSC(MX1IDX) =    PI()/2
!           C
!           C     Latitude:
!           C
!                 DSKDSC(MN2IDX) =    0
!                 DSKDSC(MX2IDX) =    PI()/4
!           C
!           C     Radius:
!           C
!                 DSKDSC(MN3IDX) =    1.D3
!                 DSKDSC(MX3IDX) =    1.1D3
!
!                 WRITE (*,*) 'Radius bound for latitudinal section: ',
!                .            ZZDSKSGR( DSKDSC )
!
!                 END
!
!        When run on a PC/Linux/gfortran platform, the output
!        from this program was:
!
!           Radius bound for latitudinal section:    1100.0000000000000
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
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 13-FEB-2017 (NJB)
!
!        Added case for planetodetic coordinates; deleted case for
!        cylindrical coordinates. Recoded algorithm for rectangular
!        coordinates.
!
!        22-JUL-2016 (NJB)
!
!           Renamed to ZZDSKSGR.
!
!        14-MAY-2010 (NJB)
!
!-&

! $ Index_Entries
!
!     Return upper bound on dsk segment coverage radius
!
!-&

!
!     SPICELIB functions
!
      real(wp)              vnorm

!
!     Local variables
!
      real(wp)              bds    ( 2, 3 )
      real(wp)              f
      real(wp)              mag    ( 3 )
      real(wp)              minr
      real(wp)              re
      real(wp)              rp

      integer(ip)           b
      integer(ip)           corsys
      integer(ip)           i
      integer(ip)           j

!
!     Set an initial return value.
!
      zzdsksgr = -1.0_wp

!
!     The radius calculation depends on the coordinate system.
!
      corsys = nint( dskdsc(sysidx) )

      if ( corsys == latsys ) then
!
!        Fetch the minimum radius from the descriptor.
!
         minr = dskdsc(mn3idx)

         if ( minr <= 0.0_wp ) then

            call setmsg ( 'Minimum radius was *.'   )
            call errdp  ( '*', minr                 )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'  )
            return

         end if

!
!        This is as simple as it gets. The radius bounds
!        correspond to the third coordinate in the descriptor.
!
         zzdsksgr = dskdsc( mx3idx )

      else if ( corsys == pdtsys ) then
!
!        Fetch the equatorial radius from the descriptor.
!
         re = dskdsc(paridx)

         if ( re <= 0.0_wp ) then

            call setmsg ( 'Equatorial radius was *.'  )
            call errdp  ( '*', re                     )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'    )
            return

         end if

!
!        Fetch the flattening coefficient from the descriptor.
!
         f = dskdsc(paridx+1)

         if ( ( f >= 0.0_wp ) .and. ( f < 1.0_wp ) ) then
!
!           This is the oblate case.
!
!           The maximum radius of an oblate planetodetic boundary
!           occurs on the X-Y plane at the maximum height.
!
            zzdsksgr = dskdsc(mx3idx) +  re

         else if ( f < 0.0_wp ) then
!
!           This is the prolate case.
!
!           The maximum radius of an prolate planetodetic boundary
!           occurs on the poles at the maximum height.
!
            re       = dskdsc(paridx)
            rp       = re  * ( 1.0_wp - f )
            zzdsksgr = dskdsc(mx3idx) +  rp

         else
!
!           We have an invalid flattening coefficient.
!
!           If the flattening coefficient is greater than one, the
!           polar radius computed below is negative. If it's equal to
!           one, the polar radius is zero. Either case is a problem, so
!           signal an error.
!
            call setmsg ( 'Flattening coefficient was *.'  )
            call errdp  ( '*', f                           )
            call sigerr ( 'SPICE(VALUEOUTOFRANGE)'         )
            return

         end if

      else if ( corsys == recsys ) then
!
!        The bounding cell of the segment has its extreme value at
!        a corner. Just take the maximum of these values.
!
!        First copy the bounds into an appropriately dimensioned
!        array.
!
         call moved ( dskdsc(mn1idx), 6, bds )

          b = mn1idx - 1

          do i = 1, 3

             j      = b + (2*i) - 1

             mag(i) = max(  abs(dskdsc(j)), abs(dskdsc(j+1))  )

          end do

          zzdsksgr  = vnorm( mag )

      else
!
!        Never heard of this coordinate system.
!
         call setmsg( 'The coordinate system code # is not recognized.')
         call errint( '#',  corsys                                     )
         call sigerr( 'SPICE(NOTSUPPORTED)'                            )
         return

      end if

      end
