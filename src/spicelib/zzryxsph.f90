!$Procedure ZZRYXSPH ( Intersection of ray and sphere )

      SUBROUTINE ZZRYXSPH ( VERTEX, UDIR, R, XPT, FOUND )

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

      IMPLICIT NONE

      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      XPT    ( 3 )
      LOGICAL               FOUND

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
      DOUBLE PRECISION      VDOT

!
!     Local variables
!
      DOUBLE PRECISION      CPAR
      DOUBLE PRECISION      PERP   ( 3 )
      DOUBLE PRECISION      PMAG2
      DOUBLE PRECISION      R2
      DOUBLE PRECISION      S
      DOUBLE PRECISION      VMAG2

      FOUND = .FALSE.

!
!     Find the component of VERTEX orthogonal to UDIR. If the magnitude
!     of this component exceeds R, there's no intercept.
!
      CPAR = VDOT ( VERTEX, UDIR )

      CALL VLCOM ( 1.D0, VERTEX, -CPAR, UDIR, PERP )

      PMAG2 = VDOT( PERP, PERP )
      R2    = R * R

!
!     Compare squares of magnitudes, rather than magnitudes, for
!     efficiency.
!
      IF ( PMAG2 .GT. R2 ) THEN
         RETURN
      END IF

      S     = SQRT (  MAX( 0.D0,  R2-PMAG2 )  )

      VMAG2 = VDOT(VERTEX,VERTEX)

      IF ( VMAG2 .GT. R2 ) THEN
!
!        If the magnitude of the vertex exceeds R, the vertex is
!        outside the sphere. Above, we have compared squares of
!        magnitudes for efficiency.
!
         IF ( CPAR .GT. 0.D0 ) THEN
!
!           The ray points away from the sphere; there can be no
!           intersection.
!
            RETURN

         END IF
!
!        Given that an intercept exists, we can find it between VERTEX
!        and VPERP by following -UDIR from PERP towards VERTEX.
!
         XPT(1) = PERP(1) - S*UDIR(1)
         XPT(2) = PERP(2) - S*UDIR(2)
         XPT(3) = PERP(3) - S*UDIR(3)

      ELSE IF ( VMAG2 .LT. R2 ) THEN
!
!        The vertex is inside the sphere. We can calculate the exit
!        point by using PERP as a vertex.
!
         XPT(1) = PERP(1) + S*UDIR(1)
         XPT(2) = PERP(2) + S*UDIR(2)
         XPT(3) = PERP(3) + S*UDIR(3)

      ELSE
!
!        PERP is the sole intercept.
!
         XPT(1) = PERP(1)
         XPT(2) = PERP(2)
         XPT(3) = PERP(3)

      END IF

      FOUND = .TRUE.

      END
