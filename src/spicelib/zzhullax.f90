!$Procedure   ZZHULLAX ( Pyramidal FOV convex hull to FOV axis )

      subroutine zzhullax ( inst, n, bounds, axis )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Identify a face of the convex hull of an instrument's
!     polygonal FOV, and use this face to generate an axis of the
!     FOV.
!
! $ Required_Reading
!
!     CK
!     FRAMES
!     GF
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

      character*(*)         inst
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
!                The boundary vectors  must satisfy the constraints:
!
!                   1)  The boundary vectors  must be contained within
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
!     This routine produces an "axis" for a polygonal FOV using the
!     following approach:
!
!        1)  Test pairs of consecutive FOV boundary vectors to see
!            whether there's a pair such that the plane region bounded
!            by these vectors is
!
!            a)  part of the convex hull of the set of boundary vectors
!
!            b)  such that all other boundary vectors have angular
!                separation of at least MARGIN from the plane
!                containing these vectors
!
!            This search has O(N**2) run time dependency on N.
!
!            If this test produces a candidate face of the convex hull,
!            proceed to step 3.
!
!
!        2)  If step (1) fails, repeat the search for a candidate
!            convex hull face, but this time search over every pair of
!            distinct boundary vectors.
!
!            This search has O(N**3) run time dependency on N.
!
!            If this search fails, signal an error.
!
!
!        3)  Produce a set of basis vectors for a reference frame,
!            which we'll call the "face frame," using as the +X axis
!            the angle bisector of the vectors bounding the candidate
!            face, the +Y axis the inward normal vector to this face,
!            and the +Z axis completing a right-handed basis.
!
!
!        4)  Transform each boundary vector, other than the two vectors
!            defining the selected convex hull face, to the face frame
!            and compute the vector's longitude in that frame. Find the
!            maximum and minimum longitudes of the vectors in the face
!            frame.
!
!            If any vector's longitude is less than 2*MARGIN or greater
!            than pi - 2*MARGIN radians, signal an error.
!
!
!        5)  Let DELTA be the difference between pi and the maximum
!            longitude found in step (4). Rotate the +Y axis (which
!            points in the inward normal direction relative to the
!            selected face) by -DELTA/2 radians about the +Z axis of
!            the face frame. This rotation aligns the +Y axis with the
!            central longitude of the set of boundary vectors. The
!            resulting vector is our candidate FOV axis.
!
!
!        6)  Check the angular separation of the candidate FOV axis
!            against each boundary vector. If any vector has angular
!            separation of more than (pi/2)-MARGIN radians from the
!            axis, signal an error.
!
!
!     Note that there are reasonable FOVs that cannot be handled by the
!     algorithm described here. For example, any FOV whose cross
!     section is a regular convex polygon can be made unusable by
!     adding boundary vectors aligned with the angle bisectors of each
!     face of the pyramid defined by the FOV's boundary vectors. The
!     resulting set of boundary vectors has no face in its convex hull
!     such that all other boundary vectors have positive angular
!     separation from that face.
!
!     Because of this limitation, this algorithm should be used only
!     after a simple FOV axis-finding approach, such as using as the
!     FOV axis the average of the boundary vectors, has been tried
!     unsuccessfully.
!
!     Note that it's easy to construct FOVs where the average of the
!     boundary vectors doesn't yield a viable axis: a FOV of angular
!     width nearly equal to pi radians, with a sufficiently large
!     number of boundary vectors on one side and few boundary vectors
!     on the other, is one such example. This routine can find an
!     axis for many such intractable FOVs---that's why this routine
!     should be called after the simple approach fails.
!
! $ Examples
!
!     See SPICELIB private routine ZZFOVAXI.
!
! $ Restrictions
!
!     1) This is a SPICE private routine. User applications should not
!        call this routine.
!
!     2) There are "reasonable" polygonal FOVs that cannot be handled
!        by this routine. See the discussion in Particulars above.
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
!-    SPICELIB 1.0.1, 24-NOV-2021 (BVS)
!
!        Removed IK from Required Reading list.
!
!-    SPICELIB 1.0.0, 05-MAR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     Create axis vector for polygonal FOV
!
!-&



!
!     SPICELIB functions
!
      real(wp)              halfpi
      real(wp)              pi
      real(wp)              vsep

      logical               return
      logical               vzero

!
!     Local parameters
!

!
!     Local variables
!
      real(wp)              cp     ( 3 )
      real(wp)              delta
      real(wp)              lat
      real(wp)              lon
      real(wp)              maxlon
      real(wp)              minlon
      real(wp)              r
      real(wp)              ray1   ( 3 )
      real(wp)              ray2   ( 3 )
      real(wp)              sep
      real(wp)              trans  ( 3, 3 )
      real(wp)              v      ( 3 )
      real(wp)              xvec   ( 3 )
      real(wp)              yvec   ( 3 )
      real(wp)              zvec   ( 3 )

      integer(ip)           i
      integer(ip)           m
      integer(ip)           maxix
      integer(ip)           minix
      integer(ip)           next
      integer(ip)           xidx

      logical               found
      logical               ok
      logical               pass1


      if ( return() ) then
         return
      end if

      call chkin ( 'ZZHULLAX' )

!
!     Nothing found yet.
!
      found = .false.
      xidx  =  0

!
!     We must have at least 3 boundary vectors.
!
      if ( n .lt. 3 ) then

         call setmsg ( 'Polygonal FOV requires at least 3 ' &
         //            'boundary vectors but number ' &
         //            'supplied for # was #.'             )
         call errch  ( '#', inst                           )
         call errint ( '#', n                              )
         call sigerr ( 'SPICE(INVALIDCOUNT)'               )
         call chkout ( 'ZZHULLAX'                          )
         return

      end if

!
!     Find an exterior face of the pyramid defined by the
!     input boundary vectors. Since most polygonal FOVs will have
!     an exterior face bounded by two consecutive rays, we'll
!     try pairs of consecutive rays first. If this fails, we'll
!     try each pair of rays.
!
      i = 1

      do while ( ( i .le. n ) .and. ( .not. found ) )
!
!        Set the index of the next ray. When we get to the
!        last boundary vector, the next ray is the first.
!
         if ( i .eq. n ) then
            next = 1
         else
            next = i + 1
         end if

!
!        Find the cross product of the first ray with the
!        second. Depending on the ordering of the boundary
!        vectors, this could be an inward or outward normal,
!        in the case the current face is exterior.
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
            call chkout ( 'ZZHULLAX'                          )
            return

         end if

!
!        See whether the other boundary vectors have angular
!        separation of at least MARGIN from the plane containing
!        the current face.
!
         pass1 =  .true.
         ok    = .true.
         m     =  1

         do while (  ( m .le. n ) .and. ok  )
!
!           Find the angular separation of CP and the Mth vector if the
!           latter is not an edge of the current face.
!
            if (  ( m .ne. i ) .and. ( m .ne. next )  ) then

               sep = vsep(  cp,  bounds(1,m)  )

               if ( pass1 ) then
!
!                 Adjust CP if necessary so that it points
!                 toward the interior of the pyramid.
!
                  if ( sep .gt. halfpi() ) then
!
!                    Invert the cross product vector and adjust SEP
!                    accordingly. Within this "M" loop, all other
!                    angular separations will be computed using the new
!                    value of CP.
!
                     call vsclip ( -1.0_wp, cp )

                     sep = pi() - sep

                  end if

                  pass1 = .false.

               end if

               ok  = sep .lt. ( halfpi() - margin )

            end if

            if ( ok ) then
!
!              Consider the next boundary vector.
!
               m = m + 1

            end if

         end do

!
!        We've tested each boundary vector against the current face, or
!        else the loop terminated early because a vector with
!        insufficient angular separation from the plane containing the
!        face was found.
!
         if ( ok ) then
!
!           The current face is exterior. It's bounded by rays I and
!           NEXT.
!
            xidx  = i
            found = .true.

         else
!
!           Look at the next face of the pyramid.
!
            i = i + 1

         end if

      end do

!
!     If we didn't find an exterior face, we'll have to look at each
!     face bounded by a pair of rays, even if those rays are not
!     adjacent. (This can be a very slow process is N is large.)
!
      if ( .not. found ) then

         i = 1

         do while ( ( i .le. n ) .and. ( .not. found ) )
!
!           Consider all ray pairs (I,NEXT) where NEXT > I.
!
            next = i + 1

            do while (  ( next .le. n ) .and. ( .not. found )  )
!
!              Find the cross product of the first ray with the second.
!              If the current face is exterior, CP could be an inward
!              or outward normal, depending on the ordering of the
!              boundary vectors.
!
               call vcrss ( bounds(1,i), bounds(1,next), cp )

!
!              It's allowable for non-consecutive boundary vectors to
!              be linearly dependent, but if we have such a pair,
!              it doesn't define an exterior face.
!
               if ( .not. vzero(cp) ) then
!
!                 The rays having direction vectors indexed I and NEXT
!                 define a semi-infinite sector of a plane that might
!                 be of interest.
!
!                 Check whether all of the boundary vectors that are
!                 not edges of the current face have angular separation
!                 of at least MARGIN from the plane containing the
!                 current face.
!
                  pass1 = .true.
                  ok    = .true.
                  m     =  1

                  do while (  ( m .le. n ) .and. ok  )
!
!                    Find the angular separation of CP and the Mth
!                    vector if the latter is not an edge of the current
!                    face.
!
                     if (  ( m .ne. i ) .and. ( m .ne. next )  ) then

                        sep = vsep(  cp,  bounds(1,m)  )

                        if ( pass1 ) then
!
!                          Adjust CP if necessary so that it points
!                          toward the interior of the pyramid.
!
                           if ( sep .gt. halfpi() ) then
!
!                             Invert the cross product vector and
!                             adjust SEP accordingly. Within this "M"
!                             loop, all other angular separations will
!                             be computed using the new value of CP.
!

                              call vsclip ( -1.0_wp, cp )

                              sep = pi() - sep

                           end if

                           pass1 = .false.

                        end if

                        ok  = sep .lt. ( halfpi() - margin )

                     end if

                     if ( ok ) then
!
!                       Consider the next boundary vector.
!
                        m = m + 1

                     end if

                  end do

!
!                 We've tested each boundary vector against the current
!                 face, or else the loop terminated early because a
!                 vector with insufficient angular separation from the
!                 plane containing the face was found.
!
                  if ( ok ) then
!
!                    The current face is exterior. It's bounded by rays
!                    I and NEXT.

                     xidx  = i
                     found = .true.

                  end if
!
!                 End of angular separation test block.
!
               end if
!
!              End of non-zero cross product block.
!

               if ( .not. found ) then
!
!                 Look at the face bounded by the rays
!                 at indices I and NEXT+1.
!
                  next = next + 1

               end if

            end do
!
!           End of NEXT loop.
!
            if ( .not. found ) then
!
!              Look at the face bounded by the pairs of rays
!              including the ray at index I+1.
!
               i = i + 1

            end if

         end do
!
!        End of I loop.
!
      end if
!
!     End of search for exterior face using each pair of rays.
!
!     If we still haven't found an exterior face, we can't continue.
!
      if ( .not. found ) then

         call setmsg ( 'Unable to find face of convex hull ' &
         //            'of FOV of instrument #.'            )
         call errch  ( '#', inst                            )
         call sigerr ( 'SPICE(FACENOTFOUND)'                )
         call chkout ( 'ZZHULLAX'                           )
         return

      end if

!
!     Arrival at this point means that the rays at indices
!     XIDX and NEXT define a plane such that all boundary
!     vectors lie in a half-space bounded by that plane.
!
!     We're now going to define a set of orthonormal basis vectors:
!
!        +X  points along the angle bisector of the bounding vectors
!            of the exterior face.
!
!        +Y  points along CP.
!
!        +Z  is the cross product of +X and +Y.
!
!     We'll call the reference frame having these basis vectors
!     the "face frame."
!
!
      call vhat ( bounds(1,i),    ray1 )
      call vhat ( bounds(1,next), ray2 )

      call vlcom  ( 0.5_wp, ray1, 0.5_wp, ray2, xvec )

      call vhatip ( xvec )
      call vhat   ( cp,   yvec )
      call ucrss  ( xvec, yvec, zvec )

!
!     Create a transformation matrix to map the input boundary
!     vectors into the face frame.
!
      do i = 1, 3

         trans(1,i) = xvec(i)
         trans(2,i) = yvec(i)
         trans(3,i) = zvec(i)

      end do
!
!     Now we're going to compute the longitude of each boundary in the
!     face frame. The vectors with indices XIDX and NEXT are excluded.
!     We expect all longitudes to be between MARGIN and pi - MARGIN.
!
      minlon =  pi()
      maxlon =  0.0_wp
      minix  =  1
      maxix  =  1

      do i = 1, n

         if ( ( i .ne. xidx ) .and. ( i .ne. next )  ) then
!
!           The current vector is not a boundary of our edge,
!           so find its longitude.
!
            call mxv ( trans, bounds(1,i), v )

            call reclat ( v, r, lon, lat )
!
!           Update the longitude bounds.
!
            if ( lon .lt. minlon ) then
               minix  = i
               minlon = lon
            end if

            if ( lon .gt. maxlon ) then
               maxix  = i
               maxlon = lon
            end if

         end if

      end do

!
!     If the longitude bounds are not as expected, don't try
!     to continue.
!
      if (  minlon  .lt.  2*margin  ) then

         call setmsg ( 'Minimum boundary vector longitude in ' &
         //            'exterior face frame is # radians. ' &
         //            'Minimum occurs at index #. This FOV ' &
         //            'does not conform to the requirements ' &
         //            'of this routine. Instrument is #.'          )
         call errdp  ( '#', minlon                                  )
         call errint ( '#', minix                                   )
         call errch  ( '#', inst                                    )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                        )
         call chkout ( 'ZZHULLAX'                                   )
         return

      else if ( maxlon  .gt.  pi()-2*margin  ) then

         call setmsg ( 'Maximum boundary vector longitude in ' &
         //            'exterior face frame is # radians. ' &
         //            'Maximum occurs at index #. This FOV ' &
         //            'does not conform to the requirements ' &
         //            'of this routine. Instrument is #.'          )
         call errdp  ( '#', maxlon                                  )
         call errint ( '#', maxix                                   )
         call errch  ( '#', inst                                    )
         call sigerr ( 'SPICE(FOVTOOWIDE)'                          )
         call chkout ( 'ZZHULLAX'                                   )
         return

      end if

!
!     Let delta represent the amount we can rotate the exterior
!     face clockwise about +Z without contacting another boundary
!     vector.
!
      delta = pi() - maxlon

!
!     Rotate +Y by -DELTA/2 about +Z. The result is our candidate
!     FOV axis. Make the axis vector unit length.
!
      call vrotv  ( yvec, zvec, -delta/2, axis )
      call vhatip ( axis )

!
!     If we have a viable result, ALL boundary vectors have
!     angular separation less than HALFPI-MARGIN from AXIS.
!
      do i = 1, n

         sep = vsep( bounds(1,i), axis )

         if (  sep  .gt. ( halfpi() - margin )  ) then

            call setmsg ( 'Boundary vector at index # has ' &
            //            'angular separation of # radians ' &
            //            'from candidate FOV axis. This FOV ' &
            //            'does not conform to the requirements ' &
            //            'of this routine. Instrument is #.'    )
            call errint ( '#', i                                 )
            call errdp  ( '#', sep                               )
            call errch  ( '#', inst                              )
            call sigerr ( 'SPICE(FOVTOOWIDE)'                    )
            call chkout ( 'ZZHULLAX'                             )
            return

         end if

      end do

      call chkout ( 'ZZHULLAX' )
      return
      end
