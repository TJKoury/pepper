!$Procedure ZZRYTREC ( DSK, ray touches rectangular element )

      subroutine zzrytrec ( vertex, raydir, bounds, &
                            margin, nxpts,  xpt    )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Find nearest intersection to ray's vertex of ray and
!     rectangular volume element. If the vertex is inside
!     the element, the vertex is considered to be the solution.
!
! $ Required_Reading
!
!     DSK
!
! $ Keywords
!
!     GEOMETRY
!     INTERCEPT
!     INTERSECTION
!     RAY
!     SURFACE
!     TOPOGRAPHY
!
! $ Declarations

      implicit none

      real(wp)              vertex ( 3 )
      real(wp)              raydir ( 3 )
      real(wp)              bounds ( 2, 3 )
      real(wp)              margin
      integer(ip)           nxpts
      real(wp)              xpt    ( 3 )


! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VERTEX     I   Ray's vertex.
!     RAYDIR     I   Ray's direction vector.
!     BOUNDS     I   Bounds of rectangular volume element.
!     MARGIN     I   Margin used for element expansion.
!     NXPTS      O   Number of intercept points.
!     XPT        O   Intercept.
!
! $ Detailed_Input
!
!     VERTEX,
!     RAYDIR     are, respectively, the vertex and direction vector of
!                the ray to be used in the intercept computation.
!
!                Both the vertex and ray direction must be represented
!                in the reference frame of the segment to which the
!                volume element boundaries correspond. The vertex is
!                considered to be an offset from the center of the
!                reference frame associated with the segment.
!
!     BOUNDS     is a 2x3 array containing the bounds of a rectangular
!                volume element. Normally this is the coverage boundary
!                of a DSK segment. In the element
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
! $ Detailed_Output
!
!     XPT        is the intercept of the ray on the boundary of the
!                input volume element, if such an intercept exists. If
!                the ray's vertex is inside the element, XPT is set
!                equal to the vertex. XPT is valid if and only if FOUND
!                is .TRUE.

!                XPT is expressed in the reference frame associated
!                with the inputs VERTEX and RAYDIR. XPT represents
!                an offset from the origin of the coordinate system.
!
!                XPT is valid only if NXPTS is set to 1.
!
!
!     NXPTS      is the number of intercept points of the ray and
!                the volume element.
!
!                Currently there are only two possible values for
!                NXPTS:
!
!                   1 for an intersection
!                   0 for no intersection
!
!                If the vertex is inside the element, NXPTS is
!                set to 1.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the minimum value of any coordinate exceeds the maximum,
!        the error will be signaled by a routine in the call tree of
!        this routine.
!
! $ Files
!
!     None. However, the input segment boundaries normally have
!     been obtained from a loaded DSK file.
!
! $ Particulars
!
!     This routine sits on top of data DSK type-specific ray-segment
!     intercept routines such as DSKX02.
!
! $ Examples
!
!     See usage in ZZDSKBUX.
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
!-    SPICELIB Version 1.0.0, 01-MAR-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     find intercept of ray on rectangular volume element
!
!-&

!
!     SPICELIB functions
!
      logical               return

!
!     Local parameters
!
      integer(ip), parameter :: none = 0

!
!     Element boundary indices:
!
      integer(ip), parameter :: lower = 1

      integer(ip), parameter :: upper = 2

!
!     Local variables
!
      real(wp)              boxori ( 3 )
      real(wp)              extent ( 3 )
      real(wp)              delta  ( 3 )
      real(wp)              l      ( 3 )

      integer(ip)           i

      logical               found
      logical               inside


      if ( return() ) return

!
!     Compute the original volume edge lengths from the coordinate
!     bounds.
!
      do i = 1, 3

         l(i) = bounds(upper, i) - bounds(lower, i)

         if ( l(i) <= 0.0_wp ) then

            call setmsg ( 'Coordinate # bounds were #:#; bounds ' &
            //            'must be strictly increasing.'           )
            call errint ( '#',  i                                  )
            call errdp  ( '#',  bounds(lower,i)                    )
            call errdp  ( '#',  bounds(upper,i)                    )
            call sigerr ( 'SPICE(BADCOORDBOUNDS)'                  )
            return

         end if

      end do

!
!     Determine whether the vertex is inside the element.
!     Use double the margin for this test, since we don't
!     want to have false negative tests for rays having
!     vertices lying on the expanded element boundary.
!
      nxpts = 0

      call zzinrec ( vertex, bounds, 2*margin, none, inside )

      if ( inside ) then
!
!        We know the answer.
!
         nxpts = 1

         call vequ ( vertex, xpt )

         return

      end if

!
!     Expand the box using the specified margin.
!
      do i = 1, 3

         delta(i)  = margin * abs( l(i) )

         boxori(i) = bounds(lower,i) - delta(i)

         extent(i) = l(i) + ( 2*delta(i) )

      end do

!
!     Find the ray-surface intercept on the expanded element,
!     if the intercept exists.
!
      call zzraybox ( vertex, raydir, boxori, extent, xpt, found )

      if ( found ) then
         nxpts = 1
      end if

      return
      end
