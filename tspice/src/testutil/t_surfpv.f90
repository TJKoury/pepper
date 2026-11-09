!$Procedure T_SURFPV ( Surface intercept position and velocity )

      SUBROUTINE T_SURFPV ( STVRTX, STDIR, A, B, C, STX, FOUND )

! $ Abstract
!
!     Find the state (position and velocity) of the surface intercept
!     defined by a specified ray, ray velocity, and ellipsoid.
!
! $ Required_Reading
!
!     ROTATION
!     SPK
!     TIME
!
! $ Keywords
!
!     FRAME
!     MATRIX
!     ROTATION
!     STATE
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.D2 )

      DOUBLE PRECISION      STVRTX ( 6 )
      DOUBLE PRECISION      STDIR  ( 6 )
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      STX    ( 6 )
      LOGICAL               FOUND

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     MARGIN     P   Margin used for numeric bounds.
!     STVRTX     I   State of ray's vertex.
!     STDIR      I   State of ray's direction vector.
!     A          I   Length of ellipsoid semi-axis along the x-axis.
!     B          I   Length of ellipsoid semi-axis along the y-axis.
!     C          I   Length of ellipsoid semi-axis along the z-axis.
!     STX        O   State of surface intercept.
!     FOUND      O   Flag indicating whether intercept state was found.
!
! $ Detailed_Input
!
!     STVRTX         is the state of a ray's vertex. The first three
!                    components of STVRTX are the vertex's x, y, and z
!                    position components; the vertex's x, y, and z
!                    velocity components follow.
!
!                    The reference frame relative to which STVRTX is
!                    specified has axes aligned with with those of a
!                    triaxial ellipsoid. See the description below of
!                    the arguments A, B, and C.
!
!                    The vertex may be inside or outside of this
!                    ellipsoid, but not on it.
!
!                    No assumption is made about the units of length
!                    and time, but these units must be consistent with
!                    those of the other inputs.
!
!
!     STDIR          is the state of the input ray's direction vector.
!                    The first three components of STDIR are a non-zero
!                    vector giving the x, y, and z components of the
!                    ray's direction; the direction vector's x, y, and
!                    z velocity components follow.
!
!                    STDIR is specified relative to the same reference
!                    frame as is STVRTX.
!
!
!     A,
!     B,
!     C              are, respectively, the lengths of a triaxial
!                    ellipsoid's semi-axes lying along the x, y, and
!                    z axes of the reference frame relative to which
!                    STVRTX and STDIR are specified.
!
! $ Detailed_Output
!
!     STX            is the state of the intercept of the input ray on
!                    the surface of the input ellipsoid. The first
!                    three components of STX are the intercept's x, y,
!                    and z position components; the intercept's x, y,
!                    and z velocity components follow.
!
!                    STX is specified relative to the same reference
!                    frame as are STVRTX and STDIR.
!
!                    STX is defined if and only if both the intercept
!                    and its velocity are computable, as indicated by
!                    the output argument FOUND.
!
!
!     FOUND          is a logical flag indicating whether STX is
!                    defined. FOUND is .TRUE. if and only if both the
!                    intercept and its velocity are computable. Note
!                    that in some cases the intercept may computable
!                    while the velocity is not; this can happen for
!                    near-tangency cases.
!
! $ Parameters
!
!     MARGIN         is a parameter used to derive numeric
!                    bounds:
!
!                       - The minimum allowed semi-axis length
!                         is MARGIN / DPMAX()
!
!                       - The maximum magnitude of dX/dt,
!                         where X is image of the surface
!                         intercept after the ellipsoid and
!                         ray are transformed so that
!                         the ellipsoid maps to the unit
!                         sphere, is MARGIN / DPMAX().
!
!                       - The maximum magnitude of STX(4:6),
!                         the actual velocity of the surface
!                         intercept, is MARGIN / DPMAX().
!
! $ Exceptions
!
!     1) If the input ray's direction vector is zero, the error
!        SPICE(ZEROVECTOR) is signaled.
!
!     2) If any input axis length is less than MARGIN / DPMAX(),
!        the error SPICE(BADAXISLENGTH) is signaled.
!
!     3) If the vertex of the ray is on the ellipsoid,
!        the error SPICE(INVALIDVERTEX) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine provides an alternative, analytic ray-ellipsoid
!     surface intercept state computation algorithm meant to duplicate
!     the results of the algorithm used in SURFPV.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 30-MAY-2008 (NJB)
!
!-&

! $ Index_Entries
!
!     state of surface intercept point on ellipsoid
!     surface intercept and velocity on ellipsoid
!
!-&



!
!     SPICELIB functions
!
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      TOUCHD
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!
      INTEGER               MSGDIM
      PARAMETER           ( MSGDIM = 7 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 32 )

!
!     Local variables
!
      CHARACTER*(MSGLEN)    MSSG  ( MSGDIM )

      DOUBLE PRECISION      BOUND
      DOUBLE PRECISION      DIFF
      DOUBLE PRECISION      DS
      DOUBLE PRECISION      DU     ( 3 )
      DOUBLE PRECISION      DV     ( 3 )
      DOUBLE PRECISION      DVPAR  ( 3 )
      DOUBLE PRECISION      DVPERP ( 3 )
      DOUBLE PRECISION      DVPMAG
      DOUBLE PRECISION      DX     ( 3 )
      DOUBLE PRECISION      INVMAT ( 3, 3 )
      DOUBLE PRECISION      LEVEL
      DOUBLE PRECISION      MAXMAG
      DOUBLE PRECISION      MINRAD
      DOUBLE PRECISION      MINS
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SBOUND
      DOUBLE PRECISION      SCLMAT ( 3, 3 )
      DOUBLE PRECISION      U      ( 3 )
      DOUBLE PRECISION      UPERPN ( 3 )
      DOUBLE PRECISION      UST    ( 6 )
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      VDU
      DOUBLE PRECISION      VPAR   ( 3 )
      DOUBLE PRECISION      VPERPN ( 3 )
      DOUBLE PRECISION      VPMAG
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      XSDIR  ( 6 )

      INTEGER               BAD

      LOGICAL               EXTER

!
!     Saved variables
!
      SAVE                  MSSG

!
!     Initial values
!
      DATA                  MSSG  / 'Axis A was nonpositive.', &
                                    'Axis B was nonpositive.', &
                                    'Axes A and B were nonpositive.', &
                                    'Axis C was nonpositive.', &
                                    'Axes A and C were nonpositive.', &
                                    'Axes B and C were nonpositive.', &
                                    'All three axes were nonpositive.' /


      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_SURFPV' )

!
!     No result has been found.
!
      FOUND = .FALSE.

!
!     Reject the direction vector if it's zero.
!
      IF ( VZERO(STDIR) ) THEN

         CALL SETMSG ( 'Ray''s direction vector was zero.' )
         CALL SIGERR ( 'SPICE(ZEROVECTOR)'                 )
         CALL CHKOUT ( 'T_SURFPV'                          )
         RETURN

      END IF

!
!     Check the ellipsoid radii.
!
      MINRAD = MARGIN / DPMAX()

      BAD    = 0

      IF ( A .LE. MINRAD ) THEN
         BAD = BAD + 1
      END IF

      IF ( B .LE. MINRAD ) THEN
         BAD = BAD + 2
      END IF

      IF ( C .LE. MINRAD ) THEN
         BAD = BAD + 4
      END IF

      IF ( BAD .GT. 0 ) THEN

         CALL SETMSG ( MSSG (BAD) // ' ? '              )
         CALL ERRCH  ( '?', 'The A,B, and C axes were ' &
         //                 '#, #, and # respectively.' )
         CALL ERRDP  ( '#', A                           )
         CALL ERRDP  ( '#', B                           )
         CALL ERRDP  ( '#', C                           )
         CALL SIGERR ( 'SPICE(BADAXISLENGTH)'           )
         CALL CHKOUT ( 'T_SURFPV'                       )
         RETURN

      END IF

!
!     Form the transformation matrix that maps the ellipsoid
!     to the unit sphere.
!
      CALL CLEARD ( 9, SCLMAT )

      SCLMAT(1,1) = 1.D0 / A
      SCLMAT(2,2) = 1.D0 / B
      SCLMAT(3,3) = 1.D0 / C

!
!     Transform the input states using SCLMAT. Since SCLMAT is
!     a constant matrix, we can scale positions and velocities
!     individually.
!
      CALL MXV ( SCLMAT, STVRTX,    V  )
      CALL MXV ( SCLMAT, STVRTX(4), DV )

      CALL MXV ( SCLMAT, STDIR,     XSDIR     )
      CALL MXV ( SCLMAT, STDIR(4),  XSDIR(4)  )

!
!     Reject the vertex if it's on the ellipsoid.
!     We check this by determining whether the transformed
!     vertex is on or in the unit sphere.
!
      LEVEL =  V(1)**2 + V(2)**2 + V(3)**2

      IF ( LEVEL .EQ. 1.D0 ) THEN

         CALL SETMSG ( 'Ray''s scaled vertex (# # #) has ' &
         //            'level surface parameter #. Vertex ' &
         //            'must not be on the ellipsoid.'     )
         CALL ERRDP  ( '#', V(1)                           )
         CALL ERRDP  ( '#', V(2)                           )
         CALL ERRDP  ( '#', V(3)                           )
         CALL ERRDP  ( '#', LEVEL                          )
         CALL SIGERR ( 'SPICE(INVALIDVERTEX)'              )
         CALL CHKOUT ( 'T_SURFPV'                          )
         RETURN

      END IF

!
!     The vertex may be either outside or inside the ellipsoid.
!     Classify the cases based on the level surface upon which
!     the vertex lies.
!
      EXTER = LEVEL .GT. 1.D0

!
!     Map the direction state to the state of a unit vector.
!
      CALL DVHAT ( XSDIR,  UST )

      CALL VEQU  ( UST,    U  )
      CALL VEQU  ( UST(4), DU )

!
!     Find the component of V parallel to the unit ray direction
!     vector. If U is the direction vector and V is the ray's vertex,
!     then the component we seek is
!
!        VPAR = < V, U > U                                           (1)
!
!     and the time derivative of VPAR is
!
!        d(VPAR)/dt =     ( < dV/dt, U > + < V, dU/dt > ) U
!                      +  < V, U> * dU/dt                            (2)
!
!     Let VPERPN be the component of V perpendicular to U;
!     then
!
!        VPERPN = V - VPAR                                           (3)
!
!     and the time derivative DVPERP of VPERPN is
!
!        d(VPERPN)/dt = dV/dt - d(VPAR)/dt                           (4)
!
!     When the vertex is outside the unit sphere, if there is an
!     intercept X, the intercept is defined by
!
!        X = VPERPN  -  s * U                                        (5)
!
!     where
!                            2   1/2
!        s = ( 1 - ||VPERPN||  )                                     (6)
!
!     and the velocity of X is
!
!        dX/dt = d(VPERPN)/dt  -  ds/dt * U  -  s * dU/dt            (7)
!
!     The time derivative DS of s is
!
!        ds/dt =  - (  ||VPERPN|| * d( ||VPERPN|| )/dt  ) / s        (8)
!
!     and the time derivative DVPMAG of ||VPERPN|| is
!
!        d( ||VPERPN|| )/dt = < d(VPERPN)/dt, VPERPN/||VPERPN|| >    (9)
!
!     If the vertex is inside the sphere, the sign of s is negated,
!     so for the interior vertex case we have
!
!        X     = VPERPN  +  s * U                                   (10)
!
!        dX/dt = d(VPERPN)/dt  +  ds/dt * U  +  s * dU/dt           (11)
!
!     For the interior case, we simply negate S and proceed as
!     in the exterior case.
!
!     We now compute the intermediate results required to
!     compute the velocity of X.
!
!     Start by computing the components of V parallel to and
!     perpendicular to U. We do this in-line because we'll re-use the
!     intermediate results. At this point we can check for the
!     exterior case where the ray points away from the target.
!
      VDU = VDOT ( V, U )

      IF ( EXTER ) THEN

         IF ( VDU .GE. 0.D0 ) THEN
!
!           The ray's vertex is outside the unit sphere and
!           the ray points into the half-space that doesn't
!           contain the sphere.
!
            CALL CHKOUT ( 'T_SURFPV' )
            RETURN

         END IF

      END IF

      CALL VSCL  ( VDU, U,    VPAR   )
      CALL VSUB  ( V,   VPAR, VPERPN )

!
!     Decide whether we have an intercept. We actually exclude
!     the tangency case as well, since the intercept is not
!     differentiable there.
!
      VPMAG = VNORM( VPERPN )

      IF ( VPMAG .GE. 1.D0 ) THEN
!
!        There's no intercept, or the intercept is a tangency point.
!
         CALL CHKOUT ( 'T_SURFPV' )
         RETURN

      END IF

!
!     Compute DVPERP and DVPMAG.
!
      CALL VLCOM ( VDOT(DV,U) + VDOT(V,DU),  U,  VDU,  DU,  DVPAR )

      CALL VSUB ( DV, DVPAR, DVPERP )

      CALL VHAT ( VPERPN, UPERPN )

      DVPMAG = VDOT( DVPERP, UPERPN )

!
!     We're still here, so compute the scale factor s. Use MIN for
!     safety here; we expect that VPMAG is less than 1.
!
      S = SQRT (  1.D0  -  MIN( 1.D0, VPMAG**2 )  )

!
!     For the interior case, S is negative.
!
      IF ( .NOT. EXTER ) THEN
         S = -S
      END IF

!
!     We're almost ready to compute ds/dt. However, this
!     computation, as well as the computation on the right
!     side of equation (7), can overflow. We must ensure that we
!     can compute dX/dt safely. Let DIFF be the maximum magnitude
!     that ds/dt can have, based on (7) and on our choice of
!     margin.
!
      DIFF = DPMAX()/MARGIN   -   VNORM(DVPERP)   -   ABS(S) * VNORM(DU)

!
!     If DIFF is not positive, we have a very, very large rate of
!     change of the pointing direction. Reject this case.
!
      IF (  DIFF  .LE. 0.D0  ) THEN

         CALL CHKOUT ( 'T_SURFPV' )
         RETURN

      END IF

!
!     Let BOUND be the upper bound we impose on the magnitude
!     of ds/dt.
!
      BOUND = MAX ( DIFF, 1.D0 )

!
!     Now apply (8). If DVPMAG is greater than 1, we use it
!     in computing the minimum allowable magnitude of s.
!
      IF ( DVPMAG .GT. 1.D0 ) THEN

         MINS = 0.5D0 * DVPMAG / BOUND
      ELSE
         MINS = 0.5D0 / BOUND
      END IF

!
!     Don't compute the derivative if s is too small.
!
      IF (  TOUCHD( ABS(S) )  .LT.  TOUCHD( MINS )  ) THEN

         CALL CHKOUT ( 'T_SURFPV' )
         RETURN

      END IF

!
!     If we got this far, X and dX/dt are computable. Start
!     by applying (5) to compute X.
!
      CALL VLCOM ( 1.D0,  VPERPN,  -S,  U,  X )

!
!     Next compute ds/dt using
!
!        ds/dt =  - (  ||VPERPN|| * d( ||VPERPN|| )/dt  ) / s        (8)
!
      DS =  - VPMAG * DVPMAG / S

!
!     Now compute dX/dt using (7), which we copy below.
!
!        dX/dt = d(VPERPN)/dt  -  ds/dt * U  -  s * dU/dt            (7)

      CALL VLCOM3 ( 1.D0, DVPERP,  -DS, U,  -S, DU,   DX )

!
!     Check that we can safely invert the scale transformation.
!
      MAXMAG = MAX ( ABS(A), ABS(B), ABS(C) )

      IF ( MAXMAG .GT. 1.D0 ) THEN

         SBOUND = DPMAX() / MARGIN / MAXMAG
      ELSE
         SBOUND = DPMAX() / MARGIN
      END IF

!
!     If either the intercept or the intercept velocity
!     have magnitude too large to allow scaling, return
!     here.
!
      IF (      ( VNORM(X)  .GT. SBOUND ) &
           .OR. ( VNORM(DX) .GT. SBOUND )  ) THEN

         CALL CHKOUT ( 'T_SURFPV' )
         RETURN

      END IF

!
!     Transform the intercept state back to the original
!     vector space.
!
      CALL CLEARD ( 9, INVMAT )

      INVMAT(1,1) = A
      INVMAT(2,2) = B
      INVMAT(3,3) = C

      CALL MXV ( INVMAT, X,  STX    )
      CALL MXV ( INVMAT, DX, STX(4) )

      FOUND = .TRUE.

      CALL CHKOUT ( 'T_SURFPV' )
      RETURN
      END


