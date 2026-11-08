!$Procedure ZZRECBOX (Bounding box for rectangular volume element)

      subroutine zzrecbox ( bounds, center, lx, ly, lz, radius )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Compute a bounding box center and radius for a rectangular volume
!     element.
!
!     The outputs are the box's center, dimensions in the X, Y, and Z
!     directions, and the box's radius. The box itself coincides with
!     the input box.
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
      real(wp)              lx
      real(wp)              ly
      real(wp)              lz
      real(wp)              radius

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     BOUNDS     I   Array of coordinate bounds for segment.
!     CENTER     O   Center of bounding box.
!     LX         O   Extent of box in the X direction.
!     LY         O   Extent of box in the Y direction.
!     LZ         O   Extent of box in the Z direction.
!     RADIUS     O   Radius of box.
!
! $ Detailed_Input
!
!     BOUNDS     is a 2 x 3 double precision array containing bounds
!                on the rectangular coordinates of the spatial region
!                (volume element) covered by a DSK segment. The
!                contents of BOUNDS are:
!
!                   BOUNDS(*,1)              X bounds
!                   BOUNDS(*,2)              Y bounds
!                   BOUNDS(*,3)              Z bounds
!
!                Elements (1,*) are lower bounds; elements (2,*) are
!                upper bounds.
!
! $ Detailed_Output
!
!     CENTER     is a double precision 3-vector representing the center
!                of the volume specified by BOUNDS.
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
!     None.
!
! $ Exceptions
!
!     1) If the minimum value of of any coordinate exceeds the maximum,
!        the error SPICE(BOUNDSOUTOFORDER) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports the DSK ray-surface intercept segment
!     selection algorithms.
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
!-    SPICELIB Version 1.0.0, 04-JUN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     bounding box radius for rectangular volume element
!
!-&

!
!     SPICELIB functions
!
      real(wp)              vnorm

      logical               return

!
!     Local parameters
!
!
!     Element boundary indices:
!
      integer(ip), parameter :: lower = 1

      integer(ip), parameter :: upper = 2

!
!     Local variables
!
      real(wp)              diag   ( 3 )
      real(wp)              l      ( 3 )
      real(wp)              maxcor ( 3 )
      real(wp)              mincor ( 3 )

      integer(ip)           i

!
!     This routine uses discovery check-in. We check RETURN in order to
!     avoid performing math operations using invalid operands.
!
      if ( return() ) return

!
!     Get local copies of the bounds of the volume element.
!
      do i = 1, 3

         mincor(i) = bounds(lower,i)
         maxcor(i) = bounds(upper,i)
         l(i)      = maxcor(i) - mincor(i)

         if ( l(i) <= 0.0_wp ) then

            call setmsg ( 'Coordinate # bounds were #:#; bounds ' &
            //            'must be strictly increasing.'           )
            call errint ( '#',  i                                  )
            call errdp  ( '#',  mincor(i)                          )
            call errdp  ( '#',  maxcor(i)                          )
            call sigerr ( 'SPICE(BOUNDSOUTOFORDER)'                )
            return

         end if

      end do

!
!     Set output box dimensions.
!
      lx = l(1)
      ly = l(2)
      lz = l(3)

!
!     Compute the coordinates of the center of the box.
!
      do i = 1, 3
         center(i) = mincor(i) + ( l(i)/2 )
      end do

!
!     The radius is the distance from the center of the box
!     to any corner.
!
      call vpack ( lx/2, ly/2, lz/2, diag )

      radius = vnorm( diag )

      return
      end
