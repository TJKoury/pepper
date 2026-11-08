!$Procedure ZZINRYPL ( Simplified intersection of ray and plane )

      subroutine zzinrypl ( vertex, udir, uplnml, &
                            const,  maxd, nxpts,  xpt )
      use pepper_kinds, only: ip, wp
      implicit none

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Quickly find the intersection of a ray and a plane, without
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

      real(wp)              vertex ( 3 )
      real(wp)              udir   ( 3 )
      real(wp)              uplnml ( 3 )
      real(wp)              const
      real(wp)              maxd
      integer(ip)           nxpts
      real(wp)              xpt    ( 3 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     VERTEX,
!     UDIR       I   Vertex and unit length direction vector of ray.
!     UPLNML     I   Unit plane normal vector.
!     CONST      I   Plane constant.
!     MAXD       I   Maximum distance of intersection from vertex.
!     NXPTS      O   Number of intersection points of ray and plane.
!     XPT        O   Intersection point, if NXPTS = 1.
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
!     UPLNML,
!     CONST          are a unit-length plane normal vector and plane
!                    constant. The plane is the set of points in
!                    3-dimensional space
!
!                       { X :  < X, UPLNML > = CONST }
!
!
!     MAXD           is the maximum length of a ray-plane intercept
!                    from the ray's vertex. If an intercept exists but
!                    has distance from VERTEX greater than or equal to
!                    MAXD, the intercept will be considered NOT to
!                    exist.
!
! $ Detailed_Output
!
!     NXPTS          is the number of points of intersection of the
!                    input ray and plane.  Values and meanings of
!                    NXPTS are:
!
!                       0     Either no intersection, or the ray
!                             lies in the plane.
!
!                       1     One point of intersection. Note that
!                             this case may occur when the ray's
!                             vertex is in the plane.
!
!
!     XPT            is the point of intersection of the input ray
!                    and plane, when there is exactly one point of
!                    intersection. Otherwise, XPT is the zero vector.
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
!     2)  The plane's normal vector must have unit length, or
!         else the outputs of this routine will be invalid.
!
!     3)  If an intercept exists but is too far from the ray's
!         vertex, no intersection will be considered to exist.
!
!     4)  If the input ray lies in the input plane, no intersection
!         is considered to exist.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine performs a simplified version of the ray-plane
!     intersection computation performed by INRYPL. This routine
!     dispenses with error handling and nuanced handling of near-
!     singular geometry in return for speed.
!
!     On a PC/Linux/gfortran/64bit platform on which this routine
!     was tested, it ran about 8 times faster than the public
!     SPICELIB routine INRYPL.
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
!     simplified intersection of ray and plane
!
!-&

!
!     SPICELIB functions
!
      real(wp)              vdot

!
!     Local variables
!
      real(wp)              dircon
      real(wp)              h
      real(wp)              lpar
      real(wp)              s
      real(wp)              vtxcon

!
!     Start out indicating no intersection.
!
      nxpts = 0

!
!     VTXCON is the plane constant of the ray's vertex.
!
      vtxcon = vdot( vertex, uplnml )
!
!     DIRCON is the length of the component of the ray's
!     direction vector in the direction of UPLNML.
!
      dircon = vdot( udir, uplnml )

!
!     Dispose of the easy non-intersection cases. (The ray
!     lying in the plane is considered a non-intersection case,
!     by the way.)
!
      if ( ( vtxcon .gt. const ) .and. ( dircon .gt. 0.0_wp ) ) then
         return
      end if

      if ( ( vtxcon .lt. const ) .and. ( dircon .lt. 0.0_wp ) ) then
         return
      end if


      if ( vtxcon .eq. const ) then
!
!        The ray's vertex lies in the plane.
!
         if ( dircon .ne. 0.0_wp ) then
!
!           The ray does not lie in the plane. The
!           intercept is the ray's vertex.
!
            nxpts = 1
            call vequ ( vertex, xpt )

         end if

         return

      end if

!
!     Let UPAR and UPERP be, respectively, the components of UDIR
!     parallel to and perpendicular to UPLNML.
!
!     Compute the maximum allowed length of UPERP.
!
!
      h     = abs( vtxcon - const )
      lpar  = abs( dircon )

!
!     To prevent overflow, we require
!
!          H
!        ----  <= MAXD
!        LPAR
!
!     or equivalently
!
!        H  <=  MAXD * LPAR
!
      if ( h .gt. (maxd * lpar) ) then
         return
      end if

!
!     For safety, return if we could have a divide-by-zero error.
!
      if ( lpar .eq. 0.0_wp ) then
         return
      end if

!
!     Still being here means we can compute XPT, provided
!     the given value of MAXD was reasonable.
!
!     Note that the earlier tests we performed should
!     rule out the case
!
!        DIRCON = 0
!
!     We have also ruled out overflow in the computation below.
!
      s      = h / lpar

      xpt(1) = vertex(1) + s*udir(1)
      xpt(2) = vertex(2) + s*udir(2)
      xpt(3) = vertex(3) + s*udir(3)

      nxpts  = 1

      end
