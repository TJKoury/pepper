!$Procedure PSV2PL ( Point and spanning vectors to plane )

      subroutine psv2pl ( point, span1, span2, plane )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Make a SPICE plane from a point and two spanning vectors.
!
! $ Required_Reading
!
!     PLANES
!
! $ Keywords
!
!     GEOMETRY
!     MATH
!     PLANE
!
! $ Declarations

      implicit none

      integer(ip)           ubpl
      parameter           ( ubpl   =   4 )

      real(wp)              point (    3 )
      real(wp)              span1 (    3 )
      real(wp)              span2 (    3 )
      real(wp)              plane ( ubpl )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     POINT,
!     SPAN1,
!     SPAN2      I   A point and two spanning vectors defining a plane.
!     PLANE      O   An array representing the plane.
!
! $ Detailed_Input
!
!     POINT,
!     SPAN1,
!     SPAN2    are, respectively, a point and two spanning vectors that
!              define a geometric plane in three-dimensional space. The
!              plane is the set of vectors
!
!                 POINT   +   s * SPAN1   +   t * SPAN2
!
!              where `s' and `t' are real numbers. The spanning vectors
!              SPAN1 and SPAN2 must be linearly independent, but they
!              need not be orthogonal or unitized.
!
! $ Detailed_Output
!
!     PLANE    is a SPICE plane that represents the geometric plane
!              defined by POINT, SPAN1, and SPAN2.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If SPAN1 and SPAN2 are linearly dependent, i.e. the vectors
!         POINT, SPAN1, and SPAN2 do not define a plane, the error
!         SPICE(DEGENERATECASE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     SPICELIB geometry routines that deal with planes use the `plane'
!     data type to represent input and output planes. This data type
!     makes the subroutine interfaces simpler and more uniform.
!
!     The SPICELIB routines that produce SPICE planes from data that
!     define a plane are:
!
!        NVC2PL ( Normal vector and constant to plane )
!        NVP2PL ( Normal vector and point to plane    )
!        PSV2PL ( Point and spanning vectors to plane )
!
!     The SPICELIB routines that convert SPICE planes to data that
!     define a plane are:
!
!        PL2NVC ( Plane to normal vector and constant )
!        PL2NVP ( Plane to normal vector and point    )
!        PL2PSV ( Plane to point and spanning vectors )
!
!     Any of these last three routines may be used to convert this
!     routine's output, PLANE, to another representation of a
!     geometric plane.
!
! $ Examples
!
!     1)  Project a vector V orthogonally onto a plane defined by
!         POINT, SPAN1, and SPAN2. PROJ is the projection we want; it
!         is the closest vector in the plane to V.
!
!            CALL PSV2PL ( POINT,   SPAN1,   SPAN2,   PLANE )
!            CALL VPRJP  ( V,       PLANE,   PROJ           )
!
!
!     2)  Find the plane determined by a spacecraft's position vector
!         relative to a central body and the spacecraft's velocity
!         vector. We assume that all vectors are given in the same
!         coordinate system.
!
!            C
!            C     POS is the spacecraft's position, relative to
!            C     the central body. VEL is the spacecraft's velocity
!            C     vector. POS is a point (vector, if you like) in
!            C     the orbit plane, and it is also one of the spanning
!            C     vectors of the plane.
!            C
!                  CALL PSV2PL ( POS, POS, VEL, PLANE )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  G. Thomas and R. Finney, "Calculus and Analytic Geometry,"
!          7th Edition, Addison Wesley, 1988.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 24-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 31-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VMINUS call.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 01-NOV-1990 (NJB)
!
!-&


! $ Index_Entries
!
!     point and spanning vectors to plane
!
!-&


!
!     SPICELIB functions
!
      real(wp)              vdot

      logical               return
      logical               vzero

!
!     Local parameters
!

!
!     The contents of SPICE planes are as follows:
!
!        Elements NMLPOS through NMLPOS + 2 contain a unit normal
!        vector for the plane.
!
!        Element CONPOS contains a constant for the plane;  every point
!        X in the plane satisfies
!
!           < X, PLANE(NMLPOS) >  =  PLANE(CONPOS).
!
!        The plane constant is the distance of the plane from the
!        origin; the normal vector, scaled by the constant, is the
!        closest point in the plane to the origin.
!
!
      integer(ip)           nmlpos
      parameter           ( nmlpos = 1 )

      integer(ip)           conpos
      parameter           ( conpos = 4 )

!
!     Local variables
!
      real(wp)              tmpvec ( 3 )


!
!     This routine checks in only if an error is discovered.
!
      if ( return () ) return

!
!     Find the unitized cross product of SPAN1 and SPAN2; this is our
!     unit normal vector, or possibly its inverse.
!
      call ucrss (  span1,  span2,  plane(nmlpos)  )

      if (  vzero ( plane(nmlpos) )  ) then
         call setmsg ( 'Spanning vectors are parallel.' )
         call sigerr ( 'SPICE(DEGENERATECASE)'          )
         return
      end if

!
!     Find the plane constant corresponding to the unit normal
!     vector we've found.
!
      plane(conpos)  =  vdot ( plane(nmlpos), point )

!
!     The constant should be the distance of the plane from the
!     origin.  If the constant is negative, negate both it and the
!     normal vector.
!
      if ( plane(conpos) < 0.0_wp ) then

         plane(conpos) =   -plane(conpos)
         call vminus     (  plane(nmlpos), tmpvec        )
         call vequ       (  tmpvec,        plane(nmlpos) )

      end if

      return
      end
