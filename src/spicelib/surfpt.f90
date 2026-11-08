!$Procedure SURFPT ( Surface point on an ellipsoid )

      subroutine surfpt ( positn, u, a, b, c, point, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Determine the intersection of a line-of-sight vector with the
!     surface of an ellipsoid.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ELLIPSOID
!     GEOMETRY
!
! $ Declarations

      implicit none

      real(wp)            positn ( 3 )
      real(wp)            u      ( 3 )
      real(wp)            a
      real(wp)            b
      real(wp)            c
      real(wp)            point  ( 3 )
      logical             found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     POSITN     I   Position of the observer in body-fixed frame.
!     U          I   Vector from the observer in some direction.
!     A          I   Length of ellipsoid semi-axis along the x-axis.
!     B          I   Length of ellipsoid semi-axis along the y-axis.
!     C          I   Length of ellipsoid semi-axis along the z-axis.
!     POINT      O   Point on the ellipsoid pointed to by U.
!     FOUND      O   Flag indicating if U points at the ellipsoid.
!
! $ Detailed_Input
!
!     POSITN   is a 3-vector giving the position of an observer with
!              respect to the center of an ellipsoid. The vector is
!              expressed in a body-fixed reference frame. The semi-axes
!              of the ellipsoid are aligned with the X, Y, and Z-axes of
!              the body-fixed frame.
!
!     U        is a pointing 3-vector emanating from the observer.
!
!     A        is the length of the semi-axis of the ellipsoid that is
!              parallel to the X-axis of the body-fixed reference frame.
!
!     B        is the length of the semi-axis of the ellipsoid that is
!              parallel to the Y-axis of the body-fixed reference frame.
!
!     C        is the length of the semi-axis of the ellipsoid that is
!              parallel to the Z-axis of the body-fixed reference frame.
!
! $ Detailed_Output
!
!     POINT    is the position of the intercept of the input ray,
!              defined by the direction vector U emanating from POSITN,
!              on the surface of the input ellipsoid.
!
!              If the ray intersects the ellipsoid, POINT will be
!              returned with the body-fixed coordinates of the point
!              where the ray first meets the ellipsoid. Otherwise,
!              POINT will be returned as (0, 0, 0).
!
!     FOUND    is a logical flag indicating whether or not the ray from
!              POSITN with direction U actually intersects the
!              ellipsoid. If the ray does intersect the ellipsoid, FOUND
!              will be returned as .TRUE. If the ray misses the
!              ellipsoid, FOUND will be returned as .FALSE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input vector is the zero vector, the error
!         SPICE(ZEROVECTOR) is signaled.
!
!     2)  If any of the body's axes is zero, the error
!         SPICE(BADAXISLENGTH) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine assumes that an ellipsoid having semi-axes of
!     length A, B and C is given. Moreover, it is assumed that these
!     axes are parallel to the X-, Y-, and Z-axes of a reference frame
!     whose origin is the geometric center of the ellipsoid---this is
!     called the body-fixed reference frame.
!
! $ Examples
!
!     A typical use of SURFPT would be to obtain the planetocentric
!     coordinates of the point at which the optic axis of a
!     spacecraft-mounted instrument intersects the surface of a target
!     body, given the following items.
!
!        1) The epoch (ET) of observation, and the inertial
!           pointing (VPNT) of the instrument at this epoch.
!
!        2) The apparent position (VTARG) of the center of the
!           target body as seen from the spacecraft at the epoch
!           of observation, and the one-way light time (TAU)
!           from the target to the spacecraft.
!
!     In order to find the point of intersection, the following
!     items are also needed.
!
!        3) The transformation (TIBF) from inertial
!           to body-fixed coordinates at epoch ET-TAU.
!
!        4) The radii (R) of the tri-axial ellipsoid
!           used to model the target body.
!
!     These may be obtained from the kernel pool via calls to PXFORM
!     and BODVRD or BODVCD respectively.
!
!     The position of the observer is just the negative of the
!     spacecraft-target vector, VTARG, computed using the VMINUS
!     module. (Note that this is NOT the same as the apparent position
!     of the spacecraft as seen from the target!) Both vectors must be
!     specified in the body-fixed reference frame. The point of
!     intersection is found as follows:
!
!         CALL VMINUS ( VTARG, VPOS )
!         CALL MXV    ( TIBF,  VPOS,  VPOS )
!         CALL MXV    ( TIBF,  VPNT,  VPNT )
!
!         CALL SURFPT ( VPOS, VPNT, R(1), R(2), R(3), VSURF, FOUND )
!
!     Note that VSURF may or may not be a point of intersection,
!     depending on whether FOUND is .TRUE. or .FALSE. Note also that
!     VSURF is a vector from the center to the surface of the
!     target, in body-fixed coordinates, which may be converted
!     directly to planetocentric latitude, longitude, and radius:
!
!         CALL RECLAT ( VSURF, RADIUS, LONG, LAT )
!
!     To get the inertial vector from the spacecraft to the
!     surface point, you must subtract VPOS from VSURF, and rotate
!     the resulting vector back to inertial coordinates:
!
!         CALL VSUB ( VSURF, VPOS,  VSURF )
!         CALL MTXV ( TIBF,  VSURF, VSURF )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.4.0, 25-MAY-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Improved
!        "POINT" argument description.
!
!-    SPICELIB Version 1.3.0, 03-APR-2006 (NJB)
!
!        Bug fix: intercept point is now always set to the
!        ray's vertex when the vertex is on the ellipsoid's
!        surface. This routine now uses discovery check-in.
!
!-    SPICELIB Version 1.2.2, 24-OCT-2005 (NJB)
!
!        Updated header to refer to BODVRD and BODVCD instead of
!        BODVAR.
!
!-    SPICELIB Version 1.2.1, 27-JUL-2003 (NJB) (CHA)
!
!        Various header corrections were made. The example program
!        was upgraded to use real kernels, and the program's output is
!        shown.
!
!-    SPICELIB Version 1.2.0, 28-NOV-2002 (NJB)
!
!        Re-implemented intercept computation to reduce loss of
!        precision.
!
!        Changed SAVE statement to save only the error message.
!        Previously all local variables were saved.
!
!-    SPICELIB Version 1.1.0, 07-AUG-1996 (WLT)
!
!        Added a SAVE statement so that the error message will
!        not be lost between separate invocations of the routine.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     line of sight intercept with body
!     point of intersection between ray and ellipsoid
!     surface point of intersection of ray and ellipsoid
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.2.0, 28-NOV-2002 (NJB)
!
!        Re-implemented intercept computation to reduce loss of
!        precision. New algorithm maps input ellipsoid to unit
!        sphere, finds closest point on input ray to the origin,
!        then finds the offset from this point to the surface.
!
!-    Beta Version 2.0.0, 9-JAN-1988 (WLT)
!
!      Short error message SPICE(ZEROAXISLENGTH) changed to
!      SPICE(BADAXISLENGTH)
!
!-&


!
!     SPICELIB functions
!
      real(wp)              vdot
      real(wp)              vnorm

      logical               return
      logical               vzero

!
!     Local parameters
!
      integer(ip)           msgdim
      parameter           ( msgdim = 7 )

      integer(ip)           msglen
      parameter           ( msglen = 32 )

!
!     Local variables
!
      character*(msglen)    mssg  ( msgdim )

      real(wp)              p     ( 3 )
      real(wp)              pmag
      real(wp)              scale
      real(wp)              sign
      real(wp)              ux    ( 3 )
      real(wp)              x     ( 3 )
      real(wp)              y     ( 3 )
      real(wp)              ymag
      real(wp)              yproj ( 3 )

      integer(ip)           bad

!
!     Saved variables
!
      save                  mssg

!
!     Initial values
!
      data                  mssg  / 'Axis A was nonpositive.', &
                                    'Axis B was nonpositive.', &
                                    'Axes A and B were nonpositive.', &
                                    'Axis C was nonpositive.', &
                                    'Axes A and C were nonpositive.', &
                                    'Axes B and C were nonpositive.', &
                                    'All three axes were nonpositive.' /


!
!     Use discovery check-in.
!
      if ( return () ) then
         return
      end if

!
!     Check the input vector to see if its the zero vector. If it is
!     signal an error and return.
!
      if  ( vzero(u) )  then

         call chkin  ( 'SURFPT'                                       )
         call setmsg ( 'SURFPT: The input vector is the zero vector.' )
         call sigerr ( 'SPICE(ZEROVECTOR)'                            )
         call chkout ( 'SURFPT'                                       )
         return

      end if

!
!     Check the axis to make sure that none of them is less than or
!     equal to zero. If one is, signal an error and return.
!
      bad = 0

      if ( a .le. 0 ) &
         bad = bad + 1

      if ( b .le. 0 ) &
         bad = bad + 2

      if ( c .le. 0 ) &
         bad = bad + 4


      if ( bad .gt. 0 ) then

         call chkin  ( 'SURFPT'                           )
         call setmsg ( mssg (bad)                         // &
                       ' ? '                              )
         call errch  ( ' ? ', 'The A,B, and C axes were ' // &
                              '#, #, and # respectively.' )

         call errdp  ( '#', a )
         call errdp  ( '#', b )
         call errdp  ( '#', c )

         call sigerr ( 'SPICE(BADAXISLENGTH)' )
         call chkout ( 'SURFPT' )
         return

      end if

!
!     We're done with the error checks.  Set the outputs to the
!     appropriate values for the "no intersection" case.
!
      found = .false.

      call cleard ( 3, point )

!
!     Apply a linear transformation to the point, direction vector,
!     and ellipsoid to transform the problem to one having the unit
!     sphere as the target ellipsoid.  (The transformation of the
!     ellipsoid is implicit.)
!
      x(1)  = u(1) / a
      x(2)  = u(2) / b
      x(3)  = u(3) / c

      y(1)  = positn(1) / a
      y(2)  = positn(2) / b
      y(3)  = positn(3) / c


!
!     Find the component P of Y (the ray's vertex) orthogonal to X
!     (the ray's direction).
!
      call vperp ( y, x, p )

!
!     Find the component of Y parallel to X.
!
      call vsub ( y, p, yproj )

!
!     Find the magnitudes of Y and P.
!
      ymag = vnorm(y)
      pmag = vnorm(p)

!
!     Get a unitized copy of X.
!
      call vhat ( x, ux )

!
!     Now determine whether there's an intersection.  Consider
!     the case where Y is outside the sphere first.
!
      if ( ymag .gt. 1 ) then
!
!        If P is outside of the sphere, there can be no intersection.
!
         if ( pmag .gt. 1.0_wp ) then
            return
         end if

!
!        If X points in the same direction as YPROJ, then the ray
!        is pointing away from the sphere, and there is no
!        intersection.
!
         if (  vdot(yproj, x) .gt. 0.0_wp ) then
            return
         end if

!
!        At this point we know there's an intersection.
!
         if ( pmag .eq. 1.0_wp ) then
!
!           The vector P we've found is the singleton point of
!           intersection.  All we have to do is transform P by
!           applying the inverse of our original linear transformation.
!
            point(1) = p(1) * a
            point(2) = p(2) * b
            point(3) = p(3) * c

            found    = .true.
            return

         end if

!
!        At this point we know there's a non-trivial intersection.
!
!        Set the sign of the coefficient of UX (a unitized copy
!        of X) that will be used to compute the intercept point.
!        In this case the coefficient of UX has negative sign because
!        the vector we're adding to P points toward Y.
!
         sign = -1.0_wp


      else if ( ymag .eq. 1 ) then
!
!        The ray's vertex is on the surface of the ellipsoid.
!        The vertex is the first point of intersection.
!
         call vequ ( positn, point )

         found = .true.
         return

      else
!
!        Y is inside the sphere, so there's definitely an intersection.
!        In this case, the intercept is obtained by adding a positive
!        multiple of UX to P.
!
         sign =  1.0_wp

      end if

!
!
!     We have a small amount of work to do:  we'll find the multiple
!     of X that when added to P yields the desired intercept point.
!
!     The magnitude of the half-chord connecting P and the surface
!     is just
!             ____________
!           \/ 1 - PMAG**2
!
!
      scale = sqrt (  max ( 0.0_wp, 1-pmag*pmag )  )

!
!     Find the intercept point on the unit sphere.
!
      call vlcom (  1.0_wp,  p,  sign * scale,  ux,  point  )

!
!     Undo our linear transformation.
!
      point(1) = point(1) * a
      point(2) = point(2) * b
      point(3) = point(3) * c

      found    = .true.

      return
      end
