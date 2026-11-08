!$Procedure ZZINRYPL ( Simplified intersection of ray and plane )

      SUBROUTINE ZZINRYPL ( VERTEX, UDIR, UPLNML, &
                            CONST,  MAXD, NXPTS,  XPT )
      IMPLICIT NONE

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Quickly find the intersection of a ray and a plane, without
!     performing normal error handling.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      UDIR   ( 3 )
      DOUBLE PRECISION      UPLNML ( 3 )
      DOUBLE PRECISION      CONST
      DOUBLE PRECISION      MAXD
      INTEGER               NXPTS
      DOUBLE PRECISION      XPT    ( 3 )

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
      DOUBLE PRECISION      VDOT

!
!     Local variables
!
      DOUBLE PRECISION      DIRCON
      DOUBLE PRECISION      H
      DOUBLE PRECISION      LPAR
      DOUBLE PRECISION      S
      DOUBLE PRECISION      VTXCON

!
!     Start out indicating no intersection.
!
      NXPTS = 0

!
!     VTXCON is the plane constant of the ray's vertex.
!
      VTXCON = VDOT( VERTEX, UPLNML )
!
!     DIRCON is the length of the component of the ray's
!     direction vector in the direction of UPLNML.
!
      DIRCON = VDOT( UDIR, UPLNML )

!
!     Dispose of the easy non-intersection cases. (The ray
!     lying in the plane is considered a non-intersection case,
!     by the way.)
!
      IF ( ( VTXCON .GT. CONST ) .AND. ( DIRCON .GT. 0.D0 ) ) THEN
         RETURN
      END IF

      IF ( ( VTXCON .LT. CONST ) .AND. ( DIRCON .LT. 0.D0 ) ) THEN
         RETURN
      END IF


      IF ( VTXCON .EQ. CONST ) THEN
!
!        The ray's vertex lies in the plane.
!
         IF ( DIRCON .NE. 0.D0 ) THEN
!
!           The ray does not lie in the plane. The
!           intercept is the ray's vertex.
!
            NXPTS = 1
            CALL VEQU ( VERTEX, XPT )

         END IF

         RETURN

      END IF

!
!     Let UPAR and UPERP be, respectively, the components of UDIR
!     parallel to and perpendicular to UPLNML.
!
!     Compute the maximum allowed length of UPERP.
!
!
      H     = ABS( VTXCON - CONST )
      LPAR  = ABS( DIRCON )

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
      IF ( H .GT. (MAXD * LPAR) ) THEN
         RETURN
      END IF

!
!     For safety, return if we could have a divide-by-zero error.
!
      IF ( LPAR .EQ. 0.D0 ) THEN
         RETURN
      END IF

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
      S      = H / LPAR

      XPT(1) = VERTEX(1) + S*UDIR(1)
      XPT(2) = VERTEX(2) + S*UDIR(2)
      XPT(3) = VERTEX(3) + S*UDIR(3)

      NXPTS  = 1

      END
