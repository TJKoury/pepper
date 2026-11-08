!$Procedure ZZINREC ( DSK, in rectangular element? )

      subroutine zzinrec ( p, bounds, margin, exclud, inside )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Test a point represented by a set of rectangular coordinates for
!     inclusion in a specified rectangular volume element. The test is
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

      real(wp)              p      ( 3 )
      real(wp)              bounds ( 2, 3 )
      real(wp)              margin
      integer(ip)           exclud
      logical               inside

      integer(ip)           none
      parameter           ( none   = 0 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     P          I   Input point.
!     BOUNDS     I   Coordinate bounds of element.
!     MARGIN     I   Element expansion margin.
!     EXCLUD     I   Index of coordinate to exclude from test.
!     INSIDE     O   Flag indicating whether point is in element.
!     NONE       P   Meaning: exclude nothing.
!
! $ Detailed_Input
!
!     P          is a point expressed in Cartesian coordinates. The
!                point is to be checked to determine whether it is
!                inside the volume element specified by BOUNDS.
!
!
!     BOUNDS     is an 2x3 array containing the bounds of a volume
!                element expressed in rectangular coordinates. BOUNDS
!                defines the volume element used in the comparison. In
!                the element
!
!                   BOUNDS(I,J)
!
!                J is the coordinate index. J is one of
!
!                   { 1, 2, 3 }
!
!                I is the bound index.
!
!                   I = 1   ->   lower bound
!                   I = 2   ->   upper bound
!
!
!     MARGIN     is a scale factor used to expand the volume element
!                (box) described by the input bounds. Each side of the
!                element is scaled by MARGIN, and the coordinates of
!                the corners of the element are moved outward by the
!                resulting distance to create an expanded element. The
!                input point is tested for containment within this
!                expanded element.
!
!
!     EXCLUD     is either a coordinate index or the parameter NONE.
!
!                If EXCLUD is set to one of
!
!                   { 1, 2, 3 }
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
!                coordinate. For example, if a point is on the plane
!                equal to the upper Z bound, the point's Z coordinate
!                need not be used in the comparison and in fact can't
!                be meaningfully compared, due to round-off errors.
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
!     NONE       when used as a value of the input argument EXCLUD,
!                indicates that no coordinates should be excluded from
!                comparison.
!
! $ Exceptions
!
!     1)  If MARGIN is negative, the error SPICE(VALUEOUTOFRANGE)
!         is signaled.
!
!     2) If EXCLUD is less than NONE or greater than 3, the error
!        SPICE(INDEXOUTOFRANGE) is signaled.
!
!     3) If any coordinate upper bound is less than the corresponding
!        lower bound, the error SPICE(BOUNDSOUTOFORDER) is signaled.
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
!     See usage in ZZRYTREC.
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
!-    SPICELIB Version 1.0.0, 16-MAY-2016 (NJB)
!
!        Original version 03-OCT-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     test point against rectangular element using margin
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Element boundary indices:
!
      integer(ip)           lower
      parameter           ( lower  = 1 )

      integer(ip)           upper
      parameter           ( upper  = 2 )

!
!     Local variables
!
      real(wp)              amncor ( 3 )
      real(wp)              amxcor ( 3 )
      real(wp)              delta  ( 3 )
      real(wp)              maxcor ( 3 )
      real(wp)              mincor ( 3 )
      real(wp)              l      ( 3 )

      integer(ip)           i


!
!     Check-in is discovery style.
!
      if ( return() ) then
         return
      end if

!
!     Assume the point is outside to start. This allows us
!     to skip setting INSIDE when we find a boundary test
!     failure.
!
      inside = .false.


!
!     Reject negative margins.
!
      if ( margin .lt. 0.0_wp ) then

         call chkin  ( 'ZZINREC'                                )
         call setmsg ( 'Margin must be non-negative but was #.' )
         call errdp  ( '#', margin                              )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                 )
         call chkout ( 'ZZINREC'                                )
         return

      end if

!
!     Check the exclusion index.
!
      if ( ( exclud .lt. none ) .or. ( exclud .gt. 3 ) ) then

         call chkin  ( 'ZZINREC'                             )
         call setmsg ( 'EXCLUD was #; allowed range is 0:3.' )
         call errint ( '#', exclud                           )
         call sigerr ( 'SPICE(INDEXOUTOFRANGE)'              )
         call chkout ( 'ZZINREC'                             )
         return

      end if

!
!     Get local copies of the coordinate bounds.
!
      do i = 1, 3

         mincor(i) = bounds( lower, i)
         maxcor(i) = bounds( upper, i)
         l     (i) = maxcor(i) - mincor(i)

         if ( l(i) .lt. 0.0_wp ) then

            call chkin  ( 'ZZINREC'                              )
            call setmsg ( 'Bounds are out of order for index #; ' &
            //            'bounds are #:#.'                      )
            call errdp  ( '#', bounds(lower,i)                   )
            call errdp  ( '#', bounds(upper,i)                   )
            call sigerr ( 'SPICE(BOUNDSOUTOFORDER)'              )
            call chkout ( 'ZZINREC'                              )
            return

         end if

      end do

!
!     Compare coordinates to adjusted coordinate
!     boundaries.
!
      do i = 1, 3

         if ( exclud .ne. i ) then
!
!           Create adjusted bounds for the Ith coordinate.
!
            delta(i)  = margin * abs( l(i) )

            amncor(i) = mincor(i) - delta(i)
            amxcor(i) = maxcor(i) + delta(i)

            if (      ( p(i) .lt. amncor(i) ) &
                 .or. ( p(i) .gt. amxcor(i) ) ) then

                return

            end if

         end if

      end do

!
!     All tests that were commanded have been passed. The input
!     point is considered to be contained in the expanded volume
!     element.
!
      inside = .true.

      return
      end
