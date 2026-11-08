!$Procedure   ZZFOVAXI ( Generate an axis vector for polygonal FOV )

      subroutine zzfovaxi ( inst, n, bounds, axis )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Generate an axis of an instrument's polygonal FOV such that all
!     of the FOV's boundary vectors have angular separation of strictly
!     less than pi/2 radians from this axis.
!
! $ Required_Reading
!
!     CK
!     FRAMES
!     GF
!     IK
!     KERNEL
!
! $ Keywords
!
!     FOV
!     GEOMETRY
!     INSTRUMENT
!
! $ Declarations

      implicit none

      real(wp)              margin
      parameter           ( margin = 1.0e-12_wp )

      character(*)          inst
      integer(ip)           n
      real(wp)              bounds ( 3, n )
      real(wp)              axis   ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MARGIN     P   Minimum complement of FOV cone angle.
!     INST       I   Instrument name.
!     N          I   Number of FOV boundary vectors.
!     BOUNDS     I   FOV boundary vectors.
!     AXIS       O   Instrument FOV axis vector.
!
! $ Detailed_Input
!
!     INST       is the name of an instrument with which the field of
!                view (FOV) of interest is associated. This name is
!                used only to generate long error messages.
!
!     N          is the number of boundary vectors in the array
!                BOUNDS.
!
!     BOUNDS     is an array of N vectors emanating from a common
!                vertex and defining the edges of a pyramidal region in
!                three-dimensional space: this the region within the
!                FOV of the instrument designated by INST. The Ith
!                vector of BOUNDS resides in elements (1:3,I) of this
!                array.
!
!                The vectors contained in BOUNDS are called the
!                "boundary vectors" of the FOV.
!
!                The boundary vectors must satisfy the constraints:
!
!                   1)  The boundary vectors must be contained within
!                       a right circular cone of angular radius less
!                       than than (pi/2) - MARGIN radians; in other
!                       words, there must be a vector A such that all
!                       boundary vectors have angular separation from
!                       A of less than (pi/2)-MARGIN radians.
!
!                   2)  There must be a pair of vectors U, V in BOUNDS
!                       such that all other boundary vectors lie in
!                       the same half space bounded by the plane
!                       containing U and V. Furthermore, all other
!                       boundary vectors must have orthogonal
!                       projections onto a plane normal to this plane
!                       such that the projections have angular
!                       separation of at least 2*MARGIN radians from
!                       the plane spanned by U and V.
!
!                Given the first constraint above, there is plane PL
!                such that each of the set of rays extending the
!                boundary vectors intersects PL. (In fact, there is an
!                infinite set of such planes.) The boundary vectors
!                must be ordered so that the set of line segments
!                connecting the intercept on PL of the ray extending
!                the Ith vector to that of the (I+1)st, with the Nth
!                intercept connected to the first, form a polygon (the
!                "FOV polygon") constituting the intersection of the
!                FOV pyramid with PL. This polygon may wrap in either
!                the positive or negative sense about a ray emanating
!                from the FOV vertex and passing through the plane
!                region bounded by the FOV polygon.
!
!                The FOV polygon need not be convex; it may be
!                self-intersecting as well.
!
!                No pair of consecutive vectors in BOUNDS may be
!                linearly dependent.
!
!                The boundary vectors need not have unit length.
!
!
! $ Detailed_Output
!
!     AXIS       is a unit vector normal to a plane containing the
!                FOV polygon. All boundary vectors have angular
!                separation from AXIS of not more than
!
!                   ( pi/2 ) - MARGIN
!
!                radians.
!
!                This routine signals an error if it cannot find
!                a satisfactory value of AXIS.
!
! $ Parameters
!
!     MARGIN     is a small positive number used to constrain the
!                orientation of the boundary vectors. See the two
!                constraints described in the Detailed_Input section
!                above for specifics.
!
! $ Exceptions
!
!     1)  In the input vector count N is not at least 3, the error
!         SPICE(INVALIDCOUNT) is signaled.
!
!     2)  If any pair of consecutive boundary vectors has cross
!         product zero, the error SPICE(DEGENERATECASE) is signaled.
!         For this test, the first vector is considered the successor
!         of the Nth.
!
!     3)  If this routine can't find a face of the convex hull of
!         the set of boundary vectors such that this face satisfies
!         constraint (2) of the Detailed_Input section above, the
!         error SPICE(FACENOTFOUND) is signaled.
!
!     4)  If any boundary vectors have longitude too close to 0
!         or too close to pi radians in the face frame (see discussion
!         of the search algorithm's steps 3 and 4 in Particulars
!         below), the respective errors SPICE(NOTSUPPORTED) or
!         SPICE(FOVTOOWIDE) are signaled.
!
!     5)  If any boundary vectors have angular separation of more than
!         (pi/2)-MARGIN radians from the candidate FOV axis, the
!         error SPICE(FOVTOOWIDE) is signaled.
!
! $ Files
!
!     The boundary vectors input to this routine are typically
!     obtained from an IK file.
!
! $ Particulars
!
!     Normally implementation is not discussed in SPICE headers, but we
!     make an exception here because this routine's implementation and
!     specification are deeply intertwined.
!
!     This routine first computes the average of the unitized input
!     boundary vectors; if this vector satisfies the angular separation
!     constraint (1) in Detailed_Input, a unit length copy of this
!     vector is returned as the FOV axis.
!
!     If the procedure above fails, an algorithm based on selection
!     of a suitable face of the boundary vector's convex hull is tried.
!     See the routine ZZHULLAX for details.
!
!     If the second approach fails, an error is signaled.
!
!     Note that it's easy to construct FOVs where the average of the
!     boundary vectors doesn't yield a viable axis: a FOV of angular
!     width nearly equal to pi radians, with a sufficiently large
!     number of boundary vectors on one side and few boundary vectors
!     on the other, is one such example. This routine can find an
!     axis for many such intractable FOVs---that's why ZZHULLAX
!     is called after the simple approach fails.
!
! $ Examples
!
!     See SPICELIB private routine ZZGFFVIN.
!
! $ Restrictions
!
!     1) This is a SPICE private routine. User applications should not
!        call this routine.
!
!     2) There may "reasonable" polygonal FOVs that cannot be handled
!        by this routine. See the discussions in Detailed_Input,
!        Exceptions, and Particulars above for restrictions on the
!        input set of FOV boundary vectors.
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
!-    SPICELIB Version 1.0.0, 05-MAR-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!
      real(wp)              halfpi
      real(wp)              vsep

      logical               failed
      logical               return
      logical               vzero

!
!     Local parameters
!

!
!     Local variables
!
      real(wp)              cp     ( 3 )
      real(wp)              limit
      real(wp)              sep
      real(wp)              v      ( 3 )
      real(wp)              uvec   ( 3 )

      integer(ip)           i
      integer(ip)           next

      logical               ok


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZFOVAXI' )

!
!     We must have at least 3 boundary vectors.
!
      if ( n < 3 ) then

         call setmsg ( 'Polygonal FOV requires at least 3 ' &
         //            'boundary vectors but number ' &
         //            'supplied for # was #.'             )
         call errch  ( '#', inst                           )
         call errint ( '#', n                              )
         call sigerr ( 'SPICE(INVALIDCOUNT)'               )
         call chkout ( 'ZZFOVAXI'                          )
         return

      end if


!
!     Check for linearly dependent consecutive boundary vectors.
!
      do i = 1, n
!
!        Set the index of the next ray. When we get to the
!        last boundary vector, the next ray is the first.
!
         if ( i == n ) then
            next = 1
         else
            next = i + 1
         end if

!
!        Find the cross product of the first ray with the
!        second. Depending on the ordering of the boundary
!        vectors, this could be an inward or outward normal,
!        in the case the current face is is exterior.
!
         call vcrss ( bounds(1,i), bounds(1,next), cp )

!
!        We insist on consecutive boundary vectors being
!        linearly independent.
!
         if ( vzero(cp) ) then

            call setmsg ( 'Polygonal FOV must have linearly ' &
            //            'independent consecutive boundary ' &
            //            'but vectors at indices # and # ' &
            //            'have cross product equal to the ' &
            //            'zero vector. Instrument is #.'     )
            call errint ( '#', i                              )
            call errint ( '#', next                           )
            call errch  ( '#', inst                           )
            call sigerr ( 'SPICE(DEGENERATECASE)'             )
            call chkout ( 'ZZFOVAXI'                          )
            return

         end if

      end do

!
!     First try the average of the FOV unit boundary vectors as
!     a candidate axis. In many cases, this simple approach
!     does the trick.
!
      call cleard ( 3, axis )

      do i = 1, n

         call vhat ( bounds(1,i), uvec    )
         call vadd ( uvec,        axis, v )
         call vequ ( v,           axis    )

      end do

      call vsclip ( 1.0_wp/n, axis )

!
!     If each boundary vector has sufficiently small
!     angular separation from AXIS, we're done.
!
      limit = halfpi() - margin

      ok    = .true.
      i     = 1

      do while (  ( i <= n ) .and.  ok  )

         sep = vsep( bounds(1,i), axis )

         if ( sep >  limit  ) then
            ok = .false.
         else
            i  =  i + 1
         end if

      end do


      if ( .not. ok ) then
!
!        See whether we can find an axis using a
!        method based on finding a face of the convex
!        hull of the FOV. ZZHULLAX signals an error
!        if it doesn't succeed.
!
         call zzhullax ( inst, n, bounds, axis )

         if ( failed() ) then
            call chkout ( 'ZZFOVAXI' )
            return
         end if

      end if

!
!     At this point AXIS is valid. Make the axis vector unit length.
!
      call vhatip ( axis )

      call chkout ( 'ZZFOVAXI' )
      return
      end
