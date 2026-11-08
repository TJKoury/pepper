!$Procedure ZZRYXSPH ( Intersection of ray and sphere )

      subroutine zzryxsph ( vertex, udir, r, xpt, found )
      use pepper_kinds, only: wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Quickly find the intersection of a ray and a sphere, without
!     performing normal error handling.
!
! $ Required_Reading
!
!     PLANES
!
! $ Keywords
!
!     GEOMETRY
!
! $ Declarations

      implicit none

      real(wp)              vertex ( 3 )
      real(wp)              udir   ( 3 )
      real(wp)              r
      real(wp)              xpt    ( 3 )
      logical               found

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     VERTEX,
!     UDIR       I   Vertex and unit length direction vector of ray.
!     R          I   Radius of sphere.
!     XPT        O   Intersection point, if NXPTS = 1.
!     FOUND      O   Flag indicating whether intersection exists.
!
! $ Detailed_Input
!
!     VERTEX,
!     UDIR           are a point and unit-length direction vector that
!                    define a ray in three-dimensional space. The ray
!                    is the set of points in 3-dimensional space
!
!                       { X : X = VERTEX + s*UDIR, s >= 0 }
!
!
!     R              is the radius of a sphere. The sphere is centered
!                    at the origin.
!
! $ Detailed_Output
!
!     XPT            is the point of intersection nearest to the ray's
!                    vertex of the input ray and sphere, if the
!                    intersection exists. Otherwise, XPT is undefined.
!
!     FOUND          is a local flag that is .TRUE. if and only if
!                    the input ray intersects the sphere.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  The ray's direction vector must have unit length. The
!         outputs of this routine will be invalid otherwise.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine performs a simplified version of the ray-ellipsoid
!     intersection computation performed by SURFPT; this routine works
!     with spheres only. This routine dispenses with error handling and
!     nuanced handling of near- singular geometry in return for speed.
!
!     On a PC/Linux/gfortran/64bit platform on which this routine
!     was tested, it ran about 5 times faster than the public
!     SPICELIB routine SURFPT.
!
!     This routine is meant to be used only by the DSK subsystem.
!
! $ Examples
!
!     See usage in ZZRYXLAT.
!
! $ Restrictions
!
!     1) All inputs must be checked by the caller; they are not checked
!        here.
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
!-    SPICELIB Version 1.0.0, 11-JAN-2016 (NJB)
!
!-&

! $ Index_Entries
!
!     intersection of ray and sphere
!
!-&

!
!     SPICELIB functions
!
      real(wp)              vdot

!
!     Local variables
!
      real(wp)              cpar
      real(wp)              perp   ( 3 )
      real(wp)              pmag2
      real(wp)              r2
      real(wp)              s
      real(wp)              vmag2

      found = .false.

!
!     Find the component of VERTEX orthogonal to UDIR. If the magnitude
!     of this component exceeds R, there's no intercept.
!
      cpar = vdot ( vertex, udir )

      call vlcom ( 1.0_wp, vertex, -cpar, udir, perp )

      pmag2 = vdot( perp, perp )
      r2    = r * r

!
!     Compare squares of magnitudes, rather than magnitudes, for
!     efficiency.
!
      if ( pmag2 > r2 ) then
         return
      end if

      s     = sqrt (  max( 0.0_wp,  r2-pmag2 )  )

      vmag2 = vdot(vertex,vertex)

      if ( vmag2 > r2 ) then
!
!        If the magnitude of the vertex exceeds R, the vertex is
!        outside the sphere. Above, we have compared squares of
!        magnitudes for efficiency.
!
         if ( cpar > 0.0_wp ) then
!
!           The ray points away from the sphere; there can be no
!           intersection.
!
            return

         end if
!
!        Given that an intercept exists, we can find it between VERTEX
!        and VPERP by following -UDIR from PERP towards VERTEX.
!
         xpt(1) = perp(1) - s*udir(1)
         xpt(2) = perp(2) - s*udir(2)
         xpt(3) = perp(3) - s*udir(3)

      else if ( vmag2 < r2 ) then
!
!        The vertex is inside the sphere. We can calculate the exit
!        point by using PERP as a vertex.
!
         xpt(1) = perp(1) + s*udir(1)
         xpt(2) = perp(2) + s*udir(2)
         xpt(3) = perp(3) + s*udir(3)

      else
!
!        PERP is the sole intercept.
!
         xpt(1) = perp(1)
         xpt(2) = perp(2)
         xpt(3) = perp(3)

      end if

      found = .true.

      end
