!$Procedure ELLTOF ( Elliptic time of flight )

      SUBROUTINE ELLTOF ( MA, ECC, E )

! $ Abstract
!
!     Solve the time of flight equation MA = E - e sin(E) for the
!     elliptic eccentric anomaly E, given mean anomaly the MA and
!     the eccentricity ECC.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONIC
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      MA
      DOUBLE PRECISION      ECC
      DOUBLE PRECISION      E

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MA         I   Mean anomaly at epoch.
!     ECC        I   Eccentricity.
!     E          O   Elliptic eccentric anomaly.
!
! $ Detailed_Input
!
!     MA       is the elliptic mean anomaly of an orbiting body at
!              some epoch t,
!
!                                3 1/2
!                 MA = (t-T)(mu/a )
!
!              where T is the time of periapsis passage, a is
!              the semi-major axis of the orbit, and mu is the
!              gravitational parameter of the primary body.
!
!     ECC      is the eccentricity of the orbit.
!
! $ Detailed_Output
!
!     E        is the corresponding eccentric anomaly. This is the
!              solution to the time of flight equation
!
!                 MA = E - e sin(E)
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the eccentricity (ECC) is outside the range [0,1),
!         the error SPICE(WRONGCONIC) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Iterate to solve
!
!        f(E,MA,e) = E - e sin(E) - MA = 0
!
! $ Examples
!
!     ELLTOF, HYPTOF, and PARTOF are used by CONICS.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  R. Bate, D. Mueller, and J. White, "Fundamentals of
!          Astrodynamics," Dover Publications Inc., 1971.
!
!     [2]  E. W. Ng, "A General Algorithm for the Solution of Kepler's
!          Equation for Elliptic Orbits", Cel. Mech. 20, pp.243-249,
!          1979.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
!
!        The declaration for the SPICELIB function PI is now
!        preceded by an EXTERNAL statement declaring PI to be an
!        external function. This removes a conflict with any
!        compilers that have a PI intrinsic function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     elliptic time of flight
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 8-JAN-1989 (IMU)
!
!        The routine now verifies that the eccentricity is in the
!        proper range---[0,1)---before proceeding.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      DOUBLE PRECISION      DCBRT
      DOUBLE PRECISION      HALFPI
      EXTERNAL              PI
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

!
!     Local parameters
!
      DOUBLE PRECISION      TOL
      PARAMETER           ( TOL = 1.D-15 )

!
!     Local variables
!
      DOUBLE PRECISION      M
      DOUBLE PRECISION      MPRIME
      INTEGER               N

      DOUBLE PRECISION      M0
      DOUBLE PRECISION      A
      DOUBLE PRECISION      B
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      R
      DOUBLE PRECISION      QR
      DOUBLE PRECISION      Y

      DOUBLE PRECISION      FN
      DOUBLE PRECISION      DERIV
      DOUBLE PRECISION      DERIV2
      DOUBLE PRECISION      CHANGE



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'ELLTOF' )
      END IF

      IF ( ECC .LT. 0.D0  .OR.  ECC .GE. 1.D0 ) THEN
         CALL SIGERR ( 'SPICE(WRONGCONIC)' )
         CALL CHKOUT ( 'ELLTOF'            )
         RETURN
      END IF


!
!     For reasons of numerical stability, we would like to restrict
!     our solution to the interval [0,pi]. Because E, M, and sin E
!     are always positive or negative together, we can pretend that M
!     is positive and adjust the sign of the result. And for M, E > pi,
!     we can define
!
!           M = 2n pi + M'     and    E = 2n pi + E'
!
!     where M' and E' are in the interval [-pi,pi]. Solving for E'
!     gives us E.
!
!     So, we begin by reducing the input mean anomaly to [0,pi].
!
      M = ABS ( MA )

      IF ( M .GT. PI() ) THEN
         N      = INT ( (M-PI()) / TWOPI() ) + 1
         MPRIME = M - N * TWOPI()
      ELSE
         N      = 0
         MPRIME = M
      END IF

      M = ABS ( MPRIME )

!
!     The convergence of the iterative scheme below depends on a good
!     initial estimate for E.
!
!     For small eccentricity, the initial estimate E = M is sufficient.
!     However, as the eccentricity increases, so does the number of
!     iterations required for convergence. For sufficiently large
!     eccentricity, this estimate leads to divergence.
!
!     Ng [2] notes that the function y(M,e)
!
!           E - M
!          -------  =  sin(e y + M)
!             e
!
!     increases and decreases monotonically when M is in the ranges
!     [0,M0] and [m0,pi], respectively.
!
!     When M0 < M < pi, where M0 = (pi/2) - e, the cubic
!             -   -
!
!                              pi - M  2        pi - M    pi - M
!           B(M,e) = 1 - (1 -  -------)  (1 + 2 ------- - -------)
!                              pi - M0          pi - M0    1 + e
!
!     provides a good initial estimate of y for all values of e.
!
!
      M0 = HALFPI() - ECC

      IF ( M .GE. M0 ) THEN

         A = PI() - M
         B = PI() - M0

         Y = 1.D0 - (1.D0 - A/B)**2 * (1.D0 + 2.D0*A/B - A/(1.D0+ECC))
         E = ECC * SIN(ECC*Y + M) + M

!
!     The situation is a little more troublesome, however, when M < M0.
!     For small eccentricity, the cubic
!
!                                  2
!           A(M,e) = 1 - (1 - M/M0)  (1 + 2M/M0 - M/(1-e) )
!
!     gives a reasonable first estimate of y. However, as e -> 1,
!     successive approximations of the form
!
!                             k           k
!           C (M,e) = 1 - (-1)  (1 - M/M0)
!            k
!
!     are used, where k = 4 for e > 0.7, and k = 8 for e > 0.85.
!
!     For high eccentricity (e > 0.96) and low mean anomaly (M < 0.05),
!     these successive approximations eventually fail. Fortunately, in
!     just these cases, the cubic
!
!                           3    2  1/3           3    2  1/3
!           D(M,e) = [r + (q  + r )]     + [r - (q  + r )]
!
!     where
!
!           r = 3M/e,   q = (2/e)(1 - e)
!
!     provides a reasonable estimate of E directly.
!
!
      ELSE IF ( ECC .LE. 0.7D0 ) THEN

         Y = 1.D0 - (1.D0 - M/M0)**2 * (1.D0 + 2.D0*M/M0 - M/(1.D0-ECC))
         E = ECC * SIN(ECC*Y + M) + M

      ELSE IF ( ECC .LE. 0.85D0 ) THEN

         Y = 1.D0 - (1.D0 - M/M0)**4
         E = ECC * SIN(ECC*Y + M) + M

      ELSE IF ( ECC .LE. 0.96D0 .OR. M .GT. 0.05D0 ) THEN

         Y = 1.D0 - (1.D0 - M/M0)**8
         E = ECC * SIN(ECC*Y + M) + M

      ELSE

         Q  = (2.D0/ECC) * (1.D0 - ECC)
         R  = 3.D0 * (M/ECC)
         QR = SQRT ( Q**3 + R**2 )

         E  = DCBRT ( R + QR ) + DCBRT ( R - QR )

      END IF


!
!     Use the Newton second-order method,
!
!                                           2
!          E    = E  - (f/f')*(1 + f*f''/2f' )
!           i+1    i
!
!     where
!
!          f   = E - e sin(E) - M
!          f'  = 1 - e cos(E)
!          f'' =     e sin(E)
!
      CHANGE = 1.D0

      DO WHILE ( ABS ( CHANGE ) .GT. TOL )

         FN     = E    - ECC * SIN(E) - M
         DERIV  = 1.D0 - ECC * COS(E)
         DERIV2 =        ECC * SIN(E)

         CHANGE = (FN/DERIV) * ( 1.D0 + (FN*DERIV2) / (2.D0*DERIV**2) )
         E      = E - CHANGE

      END DO

!
!     "Unwrap" E' into the actual value of E.
!
      IF ( MPRIME .LT. 0 ) THEN
         E = -E
      END IF

      IF ( N .GT. 0 ) THEN
         E = E + N * TWOPI()
      END IF

      IF ( MA .LT. 0 ) THEN
         E = -E
      END IF


      CALL CHKOUT ( 'ELLTOF' )
      RETURN
      END
