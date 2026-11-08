!$Procedure ZZRAYBOX ( Ray-box intercept )

      subroutine zzraybox ( vertex, raydir, boxori, extent, xpt, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Find the surface intercept of a ray on a specified box in
!     three-dimensional space.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     MATH
!     GEOMETRY
!
! $ Declarations

      implicit none

      real(wp)              vertex ( 3 )
      real(wp)              raydir ( 3 )
      real(wp)              boxori ( 3 )
      real(wp)              extent ( 3 )
      real(wp)              xpt    ( 3 )
      logical               found

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Vertex of ray.
!     RAYDIR     I   Direction vector of ray.
!     BOXORI     I   Box origin: corner having minimum coordinates.
!     EXTENT     I   Box extents in X, Y, Z directions.
!     XPT        O   Ray-box intercept.
!     FOUND      O   Flag indicating whether intercept was found.
!
! $ Detailed_Input
!
!     VERTEX     is the vertex of a ray in three dimensional space.
!
!     RAYDIR     is the direction vector of the input ray.
!
!     BOXORI     is a vector representing the "origin" of a box in
!                three-dimensional space. BOXORI is the corner of
!                the box at which each coordinate attains its
!                minimum value.
!
!     EXTENT     is an array containing the box's edge lengths in
!                the X, Y, and Z directions. All lengths must be
!                positive.
!
! $ Detailed_Output
!
!     XPT        is the point of intersection closest to VERTEX
!                of the ray and the box. If VERTEX lies within the
!                box, XPT is set equal to VERTEX.
!
!                XPT is undefined if FOUND is .FALSE.
!
!
!     FOUND      is set to .TRUE. if and only if the ray intersects
!                the box.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input ray is the zero vector, the error
!         SPICE(ZEROVECTOR) is signaled.
!
!     2)  If any element of EXTENT is non-positive, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
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
!     See use in DSKX02.
!
! $ Restrictions
!
!     This is a private routine; its interface or functionality
!     may be changed without notice. SPICE user applications
!     should not call this routine directly.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     J.A. Bytof      (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 06-JUN-2014 (NJB)
!
!-&

! $ Index_Entries
!
!     compute ray-box intercept
!
!-&

!
!     SPICELIB functions
!
      real(wp)              vnorm

      logical               return
      logical               vzero

!
!     Local parameters
!
      integer(ip)           left
      parameter           ( left   = 1 )

      integer(ip)           middle
      parameter           ( middle = 2 )

      integer(ip)           right
      parameter           ( right  = 3 )

!
!     Local variables
!
      real(wp)              center ( 3 )
      real(wp)              limit
      real(wp)              maxt
      real(wp)              near   ( 3 )
      real(wp)              offset ( 3 )
      real(wp)              plndst ( 3 )
      real(wp)              r
      real(wp)              sphxpt ( 3 )
      real(wp)              sphvtx ( 3 )
      real(wp)              t      ( 3 )
      real(wp)              udir   ( 3 )
      real(wp)              vtemp  ( 3 )

      integer(ip)           sector ( 3 )
      integer(ip)           i
      integer(ip)           maxidx

      logical               sphfnd


!
!     Use discovery check-in.
!
      if ( return() ) return

!
!     No intercept has been found yet.
!
      found = .false.

!
!     Check for a zero ray direction vector.
!
      if ( vzero(raydir) ) then

         call setmsg ( 'Input ray direction was the zero vector; this ' &
         //            'vector must be non-zero.'                      )
         call sigerr ( 'SPICE(ZEROVECTOR)'                             )
         return

      end if

      call vhat ( raydir, udir )

!
!     Check the box extents.
!
      if (  min( extent(1), extent(2), extent(3) ) <= 0.0_wp ) then

         call setmsg ( 'All box extents should be strictly positive ' &
         //            'but the extents were #, #, #.'               )
         call errdp  ( '#',  extent(1)                               )
         call errdp  ( '#',  extent(2)                               )
         call errdp  ( '#',  extent(3)                               )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'                      )
         return

      end if

!
!     Compute the coordinates of the center of the box, and compute
!     the offset of the ray's vertex from the center.
!
      do i = 1, 3
         center(i) = boxori(i) + extent(i)/2
      end do

      call vsub  ( vertex, center, offset )

!
!     If the ray's vertex is inside the box, we consider the
!     vertex to be the intercept.
!
      if (       ( abs( offset(1) ) <= extent(1)/2 ) &
           .and. ( abs( offset(2) ) <= extent(2)/2 ) &
           .and. ( abs( offset(3) ) <= extent(3)/2 )  ) then

         call vequ ( vertex, xpt )
         found = .true.

         return

      end if
!
!     Compute the intercept of the ray on the surface of a bounding
!     sphere that contains the box. Let R be the radius of this sphere.
!
      r = 0.5_wp * (1.0_wp + 1.0e-3_wp) * vnorm( extent )

      if ( vnorm(offset) < r ) then
!
!        The vertex is already inside the bounding sphere.
!
         call vequ ( offset, sphxpt )

      else

         call surfpt ( offset, udir, r, r, r, sphxpt, sphfnd )

         if ( .not. sphfnd ) then
!
!           The ray misses the bounding sphere.
!
            return

         end if

      end if

!
!     Shift the sphere intercept so as to be relative to the
!     box's origin. From this point on, we'll treat BOXORI
!     as the origin of the reference frame.
!
      do i = 1, 3
         sphvtx(i) = sphxpt(i) + center(i) - boxori(i)
      end do

!
!     Classify the position of the vertex relative to the planes
!     bounding the box: each coordinate will be classified as
!     "left," "middle," or "right" depending on whether it is
!     less than the lower bound for that coordinate, between
!     the bounds, or greater than the upper bound.
!
      do i = 1, 3

         if ( sphvtx(i) < 0.0_wp ) then

            sector(i) = left
            near(i)   = 0.0_wp

         else if ( sphvtx(i) > extent(i) ) then

            sector(i) = right
            near(i)   = extent(i)

         else
            sector(i) = middle
            near(i)   = 0.0_wp
         end if

      end do

!
!     At this point, SPHVTX is a point on the ray that is outside,
!     but close to, the box. SPHVTX is an offset from BOXORI we'll
!     need to add BOXORI to it to obtain the corresponding point in
!     the input reference frame.
!
!     We'll use SPHVTX as the new ray vertex.
!
!     Find the distances of the vertex's components from the nearest
!     bounding planes of the box; find the corresponding distances
!     along the ray that would be traveled in order to move each
!     component from the vertex to the nearest bounding plane. Call the
!     latter distance for the Ith coordinate T(I). We're only
!     interested in the vertex components that are "outside" the
!     bounding planes. Mark the values of T(I) for components in the
!     "middle" using the value -1.
!
!     Find the index of the maximum T value while we're at it. If
!     there's an intercept, it occurs at the point on the ray
!     corresponding to the maximum value of T.
!
      maxidx =  1
      maxt   = -1.0_wp


      do i = 1, 3

         t(i) = -1.0_wp

         if ( sector(i) /= middle ) then

            plndst(i) = near(i) - sphvtx(i)

!
!           Prepare for a "safe" division.
!
            limit = 2 * r * abs( udir(i) )

            if ( abs( plndst(i) ) > limit ) then
!
!              The ray can't get to the nearest bounding plane
!              before exiting the bounding sphere. No intersection
!              is possible.
!
               return

            end if
!
!           The magnitude of the following quotient is bounded by 2R.
!
            t(i) = plndst(i) / udir(i)

            if ( t(i) < 0.0_wp ) then
!
!              This component of the ray is going in the wrong
!              direction. No intersection is possible.
!
               return

            end if

            if ( t(i) > maxt ) then

               maxidx = i
               maxt   = t(i)

            end if

         end if

      end do

!
!     We should have a positive value of T for at least one
!     coordinate. However, if we don't, there's no intersection.
!
      if ( maxt < 0.0_wp ) return

!
!     Compute the candidate intercept. Note that we're now working
!     in a frame centered at the box origin.
!
      call vlcom ( 1.0_wp, sphvtx, maxt, udir, xpt )

!
!     Decide whether XPT is actually on the surface of the box.
!     Sharpen XPT as part of the process.
!
      do i = 1, 3

         if ( i == maxidx ) then
!
!           XPT is supposed to lie exactly on the bounding plane
!           orthogonal to the Ith axis and nearest to SPHVTX.
!
            xpt(i) = near(i)

         else

            if ( sector(i) == middle ) then
!
!              The Ith component of the vertex is between the
!              bounding planes for the Ith coordinate. If the
!              Ith component of XPT is outside these bounds,
!              the ray misses the box.
!
               if (      ( xpt(i) < 0.0_wp      ) .or. ( xpt(i) > extent(i) ) ) return

            else
!
!              The Ith component of the vertex SPHVTX is outside of the
!              bounding planes for the Ith coordinate. Since T(MAXIDX)
!              is greater than or equal to T(I), XPT(I) should be on or
!              past the bounding plane closest to SPHVTX(I). Sharpen
!              XPT(I) if necessary. If XPT(I) is beyond the bounding
!              plane farthest from SPHVTX(I), no intersection can
!              exist.
!
               if ( sector(i) == left ) then
!
!                 Sharpen the Ith component of XPT.
!
                  xpt(i) = max( xpt(i), 0.0_wp )

                  if ( xpt(i) > extent(i) ) then
!
!                    The ray hits the MAXIDX face too far away from
!                    SPHVTX(I). There's no intersection with the box.
!
                     return

                  end if


               else
!
!                 SECTOR(I) .EQ. RIGHT
!
!                 Sharpen the Ith component of XPT.
!
                  xpt(i) = min( xpt(i), extent(i) )

                  if ( xpt(i) < 0.0_wp ) then
!
!                    The ray hits the MAXIDX face too far away from
!                    SPHVTX(I). There's no intersection with the box.
!
                     return

                  end if

               end if
!
!              End of block in which the Ith component of XPT is
!              either sharpened or found to be off the surface of the
!              box. This block deals with the components other than
!              MAXIDX.
!
            end if
!
!           End of block in which the Ith component of XPT is either
!           sharpened or found to be off the surface of the box. This
!           block deals with all components.
!
         end if

      end do
!
!     End of loop in which XPT is either sharpened or found to be off
!     the surface of the box. Getting here means XPT is valid.
!
!     Shift XPT to the input reference frame.
!
      call vadd ( xpt,   boxori, vtemp )
      call vequ ( vtemp,         xpt   )

      found = .true.

      return
      end
