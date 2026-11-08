!$Procedure ZZDNPT ( Derivative of ellipsoid near point )

      subroutine zzdnpt ( state, nearp, a, b, c, dnear, dalt, found )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Compute the velocity of an ellipsoid surface point nearest to a
!     specified position.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     DERIVATIVE
!     ELLIPSOID
!     GEOMETRY
!
! $ Declarations

      implicit none
      real(wp)              state ( 6 )
      real(wp)              nearp ( 3 )
      real(wp)              a
      real(wp)              b
      real(wp)              c
      real(wp)              dnear ( 3 )
      real(wp)              dalt
      logical               found

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STATE      I   State of an object in body-fixed coordinates.
!     NEARP      I   Near point on ellipsoid.
!     A          I   Length of semi-axis parallel to x-axis.
!     B          I   Length of semi-axis parallel to y-axis.
!     C          I   Length on semi-axis parallel to z-axis.
!     DNEAR      O   Derivative of the nearest point on the ellipsoid.
!     DALT       O   Derivative of altitude.
!     FOUND      O   Tells whether DNEAR is degenerate.
!
! $ Detailed_Input
!
!     STATE    is a 6-vector giving the position and velocity of some
!              object in the body-fixed coordinates of the ellipsoid.
!
!     NEARP    the calculated/derived coordinates of the point on the
!              ellipsoid closest to STATE.
!
!              In body-fixed coordinates, the semi-axes of the ellipsoid
!              are aligned with the X, Y, and Z-axes of the coordinate
!              system.
!
!     A        is the length of the semi-axis of the ellipsoid that is
!              parallel to the X-axis of the body-fixed coordinate
!              system.
!
!     B        is the length of the semi-axis of the ellipsoid that is
!              parallel to the Y-axis of the body-fixed coordinate
!              system.
!
!     C        is the length of the semi-axis of the ellipsoid that is
!              parallel to the Z-axis of the body-fixed coordinate
!              system.
!
! $ Detailed_Output
!
!     DNEAR    is the 3-vector giving the velocity in
!              body-fixed coordinates of the point on the ellipsoid,
!              closest to the object whose position and velocity are
!              represented by STATE.
!
!              While the position component of DNEAR is always
!              meaningful, the velocity component of DNEAR will be
!              meaningless if FOUND if .FALSE. (See the discussion of
!              the meaning of FOUND below.)
!
!     DALT     is rate of change of the altitude, i.e. the range rate.
!
!              Note that the rate of change of altitude is meaningful if
!              and only if FOUND is .TRUE. (See the discussion of the
!              meaning of FOUND below.)
!
!     FOUND    is a logical flag indicating whether or not the velocity
!              portion of DNEAR is meaningful. If the velocity portion
!              of DNEAR is meaningful FOUND will be returned with a
!              value of .TRUE. Under very rare circumstance the velocity
!              of the near point is undefined. Under these circumstances
!              FOUND will be returned with the value .FALSE.
!
!              FOUND can be .FALSE. only for states whose position
!              components are inside the ellipsoid and then only at
!              points on a special surface contained inside the
!              ellipsoid called the focal set of the ellipsoid.
!
!              A point in the interior is on this special surface only
!              if there are two or more points on the ellipsoid that are
!              closest to it. The origin is such a point and the only
!              such point if the ellipsoid is a sphere. For
!              non-spheroidal ellipsoids the focal set contains small
!              portions of the planes of symmetry of the ellipsoid.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the axes are non-positive, an error is signaled by a
!         routine in the call tree of this routine.
!
!     2)  If an object is passing through the interior of an ellipsoid
!         there are points at which there is more than 1 point on
!         the ellipsoid that is closest to the object.  At these
!         points the velocity of the near point is undefined. (See
!         the description of the output variable FOUND).
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine computes DNEAR from STATE. In addition it returns
!     the range rate of altitude.
!
!     Note that this routine can compute DNEAR for STATES outside,
!     on, or inside the ellipsoid.  However, DNEAR and derivative
!     of altitude do not exist for a "small" set of STATES in the
!     interior of the ellipsoid. See the discussion of FOUND above
!     for a description of this set of points.
!
! $ Examples
!
!     None.
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
!     N.J. Bachman    (JPL)
!     W.L. Taber      (JPL)
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 30-SEP-2021 (EDW)
!
!-&

! $ Index_Entries
!
!     Velocity of the nearest point on an ellipsoid
!     Rate of change of the altitude over an ellipsoid
!     Derivative of altitude over an ellipsoid
!     Range rate of altitude over an ellipsoid
!     Velocity of a ground track
!
!-&

!
!     SPICELIB functions
!
      logical               return

      real(wp)              vdot
      real(wp)              vtmv

!
!     Local Variables
!
      real(wp)              denom
      real(wp)              dterm  ( 3 )
      real(wp)              grad   ( 3 )
      real(wp)              l
      real(wp)              length
      real(wp)              lprime
      real(wp)              norml  ( 3 )
      real(wp)              temp   ( 3 )
      real(wp)              zenith ( 3 )


      real(wp)              gradm  ( 3, 3 )
      real(wp)              m      ( 3, 3 )

      integer(ip)           i
!
!     Saved Variables
!
      save                  gradm
      save                  m

!
!     Initial Values
!
      data   gradm / 1.0_wp, 0.0_wp, 0.0_wp, &
                     0.0_wp, 1.0_wp, 0.0_wp, &
                     0.0_wp, 0.0_wp, 1.0_wp  /

      data   m     / 1.0_wp, 0.0_wp, 0.0_wp, &
                     0.0_wp, 1.0_wp, 0.0_wp, &
                     0.0_wp, 0.0_wp, 1.0_wp  /
!
!     Standard SPICE error handling.
!
      if ( return() ) return

!
!     Until we have reason to believe otherwise, we set FOUND to TRUE.
!
      found = .true.

!
!     Now for the work of this routine.  We need to compute the
!     velocity component of NEARP.
!
!     In all of the discussions below we let <,> stand for the
!     dot product (inner product).
!
!     Let P be the position (first three components) of STATE
!     and let N be the position (first three components) of NEARP.
!
!     The surface of the ellipsoid is described as the level set
!     f(x,y,z) = 1 for the function f defined by
!
!                    x**2 + y**2 + z**2
!         f(x,y,z) = ----   ----   ----
!                    A**2   B**2   C**2
!
!     Let GRAD be the "half" gradient of f,
!
!         (NABLA * f)/2
!
!     with NABLA the operator
!
!         (Dx, Dy, Dz)
!
!     ("D" indicating partial derivative). Then for some L
!
!           N + L * GRAD = P                         ( 1 )
!
!     Solve for L
!
!           L * GRAD = P - N
!
!      Apply <,GRAD> to LHS and RHS of expression
!
!           <L * GRAD, GRAD> = < P - N, GRAD >
!
!           L * < GRAD, GRAD > = < P - N, GRAD >
!
!     So that
!                < P - N, GRAD >
!           L =  --------------
!                < GRAD , GRAD >
!
!      Recall
!
!            < X, X > = |X|**2 , X in Rn, R3 in this case
!
!                          GRAD
!             =  < P - N, ------ >  /  | GRAD |
!                         |GRAD|
!
!     Since GRAD is computed at a point on the level set f(x,y,z) = 1
!     we don't have to worry about the magnitude of |GRAD| being
!     so small that underflow can occur (mostly).
!
!     Note that the half gradient of f can be computed by simple
!     vector multiplication
!
!                       [ 1/A**2    0       0    ] [ x ]
!        GRAD(x,y,z)  = |   0     1/B**2    0    | | y |
!                       [   0       0     1/C**2 ] [ z ]
!
!     We call the matrix above GRADM.  The correct off
!     diagonal values have been established in the data statement
!     following the declaration section of this routine.
!

      gradm(1,1) = 1.0_wp/(a*a)
      gradm(2,2) = 1.0_wp/(b*b)
      gradm(3,3) = 1.0_wp/(c*c)

      call vsub  ( state,  nearp, zenith )

      call mxv   ( gradm,  nearp,    grad   )
      call unorm ( grad,   norml,    length )

      l  = vdot  ( zenith, norml ) / length

!
!     We can rewrite equation (1) as
!
!        P = N + L * GRADM * N
!
!     from this it follows that
!
!        P' =  N' + L' * GRADM * N
!                 + L  * GRADM * N'
!
!           = ( IDENT + L*GRADM ) * N'   + L' * GRADM * N
!
!           = ( IDENT + L*GRADM ) * N'   + L' * GRAD
!
!     where IDENT is the 3x3 identity matrix.
!
!     Let M be the inverse of the matrix IDENT + L*GRADM. (Provided
!     of course that all of the diagonal entries are non-zero).
!
!     If we multiply both sides of the equation above by M
!     we have
!
!
!        M*P'  = N'  + L'* M * GRAD                      ( 2 )
!
!
!     Recall now that N' is orthogonal to GRAD (N' lies in the
!     tangent plane to the ellipsoid at N and GRAD is normal
!     to this tangent plane).  Thus
!
!        < GRAD, M*P' > = L' < GRAD, M * GRAD >
!
!     and
!
!                 < GRAD, M*P'   >
!        L'   =   -----------------
!                 < GRAD, M*GRAD >
!
!
!             =   VTMV ( GRAD, M, P' ) / VTMV ( GRAD, M, GRAD )
!
!     Let's pause now to compute M and L'.
!
!        This is where things could go bad.  M might not exist (which
!        indicates STATE is on the focal set of the ellipsoid).  In
!        addition it is conceivable that VTMV ( GRAD, M, GRAD ) is
!        zero.  This turns out not to be possible.  However, the
!        demonstration of this fact requires delving into the details
!        of how N was computed by NEARPT.  Rather than spending a
!        lot of time explaining the details we will make an
!        unnecessary but inexpensive check that we don't divide by
!        zero when computing L'.
!

      do i = 1, 3
         dterm(i) = 1.0_wp + l*gradm(i,i)
      end do

      do i = 1, 3

         if ( dterm(i) /= 0.0_wp ) then
            m(i,i) = 1.0_wp / dterm(i)
         else
            found = .false.
            return
         end if

      end do

      denom = vtmv ( grad, m, grad )

      if ( denom == 0.0_wp ) then
         found = .false.
         return
      end if

      lprime = vtmv( grad, m, state(4) ) / denom

!
!     Now that we have L' we can easily compute N'. Rewriting
!     equation (2) from above we have.
!
!        N'  = M * ( P' - L'*GRAD )
!
      call vlcom ( 1.0_wp, state(4), -lprime, grad, temp     )
      call mxv   ( m,     temp,                    dnear )


!
!     Only one thing left to do. Compute the derivative
!     of the altitude ALT. This quantity equals the range rate of the
!     vector from the near point, N, to the observer object, P.
!
!                               ^
!     Range rate in R3 equals < r, v >. In this case, NORML defines
!     the unit vector from N to P. The velocity of P with respect
!     to N,
!
!        V = d(P - N) = P' - N'
!            --
!            dt
!
!     But as we discussed earlier, N' is orthogonal to NORML (GRAD).
!     Thus
!
!          ^
!        < r, v > = < NORML, P' - N' >
!                 = < NORML, P'> - < NORML, N'>
!                 = < NORML, P'>
!
!        dALT/dt = < NORML, P'>
!
!     Given P' = STATE(4,5,6)
!
      dalt = vdot ( norml, state(4) )


      return
      end
