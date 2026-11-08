!$Procedure PROP2B ( Propagate a two-body solution )

      SUBROUTINE PROP2B ( GM, PVINIT, DT, PVPROP )

! $ Abstract
!
!     Compute the state of a massless body at time t_0 + DT by applying
!     the two-body force model to a given central mass and a given body
!     state at time t_0.
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
!     None.
!
! $ Keywords
!
!     CONIC
!     EPHEMERIS
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      GM
      DOUBLE PRECISION      PVINIT ( 6 )
      DOUBLE PRECISION      DT
      DOUBLE PRECISION      PVPROP ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     GM         I   Gravity of the central mass.
!     PVINIT     I   Initial state from which to propagate a state.
!     DT         I   Time offset from initial state to propagate to.
!     PVPROP     O   The propagated state.
!
! $ Detailed_Input
!
!     GM       is the gravitational constant G times the mass M of the
!              central body.
!
!     PVINIT   is the state at some specified time relative to the
!              central mass. The mass of the object is assumed to
!              be negligible when compared to the central mass.
!
!     DT       is a offset in time from the time of the initial
!              state to which the two-body state should be
!              propagated. (The units of time and distance must be
!              the same in GM, PVINIT, and DT).
!
! $ Detailed_Output
!
!     PVPROP   is the two-body propagation of the initial state
!              DT units of time past the epoch of the initial state.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If GM is not positive, the error SPICE(NONPOSITIVEMASS) is
!         signaled.
!
!     2)  If the position of the initial state is the zero vector, the
!         error SPICE(ZEROPOSITION) is signaled.
!
!     3)  If the velocity of the initial state is the zero vector, the
!         error SPICE(ZEROVELOCITY) is signaled.
!
!     4)  If the cross product of the position and velocity of PVINIT
!         has squared length of zero, the error SPICE(NONCONICMOTION)
!         is signaled.
!
!     5)  If DT is so large that there is a danger of floating point
!         overflow during computation, the error SPICE(DTOUTOFRANGE) is
!         signaled and a message is generated describing the problem.
!         The value of DT must be "reasonable". In other words, DT
!         should be less than 10**20 seconds for realistic solar system
!         orbits specified in the MKS system. (The actual bounds on DT
!         are much greater but require substantial computation.) The
!         "reasonableness" of DT is checked at run-time.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine uses a universal variables formulation for the
!     two-body motion of an object in orbit about a central mass. It
!     propagates an initial state to an epoch offset from the
!     epoch of the initial state by time DT.
!
!     This routine does not suffer from the finite precision
!     problems of the machine that are inherent to classical
!     formulations based on the solutions to Kepler's equation:
!
!           n( t - T ) = E - e Sin(E)         elliptic case
!           n( t - T ) = e sinh(F) - F        hyperbolic case
!
!     The derivation used to determine the propagated state is a
!     slight variation of the derivation in Danby's book
!     "Fundamentals of Celestial Mechanics" [1].
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) When the eccentricity of an orbit is near 1, and the epoch
!        of classical elements is near the epoch of periapse, classical
!        formulations that propagate a state from elements tend to
!        lack robustness due to the finite precision of floating point
!        machines. In those situations it is better to use a universal
!        variables formulation to propagate the state.
!
!        By using this routine, you need not go from a state to
!        elements and back to a state. Instead, you can get the state
!        from an initial state.
!
!        If PVINIT is your initial state and you want the state 3600
!        seconds later, the following call will suffice.
!
!           Look up GM somewhere
!
!           DT = 3600.0D0
!
!           CALL PROP2B ( GM, PVINIT, DT, PVPROP )
!
!        After the call, PVPROP will contain the state of the
!        object 3600 seconds after the time it had state PVINIT.
!
!     2) Use the two-body force model to propagate the state of a
!        massless body orbiting the Earth at 100,000,000 km after half
!        a period.
!
!        In circular two-body motion, the orbital speed is
!
!           s     = sqrt(mu/r)
!
!        where mu is the central mass. After tau/2 = pi*r/s seconds
!        (half period), the state should equal the negative of the
!        original state.
!
!        Example code begins here.
!
!
!              PROGRAM PROP2B_EX2
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      PI
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      MU
!              DOUBLE PRECISION      PVINIT ( 6 )
!              DOUBLE PRECISION      R
!              DOUBLE PRECISION      SPEED
!              DOUBLE PRECISION      STATE  ( 6 )
!              DOUBLE PRECISION      T
!
!        C
!        C     Initial values.
!        C
!              MU    =  3.9860043543609598D+05
!              R     =  1.0D+08
!              SPEED =  SQRT( MU / R )
!              T     =  PI( )*R/SPEED
!
!              PVINIT(1) =  0.0D0
!              PVINIT(2) =  R/SQRT(2.0D0)
!              PVINIT(3) =  R/SQRT(2.0D0)
!              PVINIT(4) =  0.0D0
!              PVINIT(5) = -SPEED/SQRT(2.0D0)
!              PVINIT(6) =  SPEED/SQRT(2.0D0)
!
!        C
!        C     Calculate the state of the body at 0.5 period
!        C     after the epoch.
!        C
!              CALL PROP2B ( MU, PVINIT, T, STATE )
!
!        C
!        C     The `state' vector should equal -pvinit
!        C
!              WRITE(*,*) 'State at t0:'
!              WRITE(*,'(A,3F17.5)') '   R   (km):',
!             .                 PVINIT(1), PVINIT(2), PVINIT(3)
!              WRITE(*,'(A,3F17.5)') '   V (km/s):',
!             .                 PVINIT(4), PVINIT(5), PVINIT(6)
!
!              WRITE(*,*) ' '
!              WRITE(*,*) 'State at tau/2:'
!              WRITE(*,'(A,3F17.5)') '   R   (km):',
!             .                    STATE(1), STATE(2), STATE(3)
!              WRITE(*,'(A,3F17.5)') '   V (km/s):',
!             .                    STATE(4), STATE(5), STATE(6)
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         State at t0:
!           R   (km):          0.00000   70710678.11865   70710678.11865
!           V (km/s):          0.00000         -0.04464          0.04464
!
!         State at tau/2:
!           R   (km):         -0.00000  -70710678.11865  -70710678.11865
!           V (km/s):          0.00000          0.04464         -0.04464
!
!
! $ Restrictions
!
!     1)  Users should be sure that GM, PVINIT and DT are all in the
!         same system of units ( for example MKS ).
!
! $ Literature_References
!
!     [1]  J. Danby, "Fundamentals of Celestial Mechanics," 2nd Edition,
!          pp 168-180, Willman-Bell, 1988.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.2.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Removed unnecessary $Revisions section.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!-    SPICELIB Version 2.1.0, 31-AUG-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VSCL call.
!
!-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
!
!        Corrected ENDIF to END IF.
!
!-    SPICELIB Version 2.0.0, 16-MAY-1995 (WLT)
!
!        The initial guess at a solution to Kepler's equation was
!        modified slightly and a loop counter was added to the
!        bisection loop together with logic that will force termination
!        of the bisection loop.
!
!-    SPICELIB Version 1.0.0, 10-MAR-1992 (WLT)
!
!-&


! $ Index_Entries
!
!     Propagate state vector using two-body force model
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      DPMAX
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      INTEGER               BRCKTI

      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local Parameters
!
      INTEGER               BUFSIZ
      PARAMETER           ( BUFSIZ = 3 )

      INTEGER               MAXBIT
      PARAMETER           ( MAXBIT = 64 )

!
!     Local variables
!
!
!     The following quantities are needed in the solution of Kepler's
!     equation and in the propagation of the input state.  They are
!     described as they are introduced in the code below.
!
      DOUBLE PRECISION      B
      DOUBLE PRECISION      B2RV
      DOUBLE PRECISION      BQ
      DOUBLE PRECISION      BR
      DOUBLE PRECISION      BR0
      DOUBLE PRECISION      C0
      DOUBLE PRECISION      C1
      DOUBLE PRECISION      C2
      DOUBLE PRECISION      C3
      DOUBLE PRECISION      E
      DOUBLE PRECISION      EQVEC  ( 3 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      FX2
      DOUBLE PRECISION      H2
      DOUBLE PRECISION      HVEC   ( 3 )
      DOUBLE PRECISION      KFUN
      DOUBLE PRECISION      KFUNL
      DOUBLE PRECISION      KFUNU
      DOUBLE PRECISION      LOGBND
      DOUBLE PRECISION      LOWER
      DOUBLE PRECISION      OLDX
      DOUBLE PRECISION      PC
      DOUBLE PRECISION      PCDOT
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      QOVR0
      DOUBLE PRECISION      R0
      DOUBLE PRECISION      RV
      DOUBLE PRECISION      TMPVEC ( 3 )
      DOUBLE PRECISION      UPPER
      DOUBLE PRECISION      VC
      DOUBLE PRECISION      VCDOT
      DOUBLE PRECISION      VEL    ( 3 )
      DOUBLE PRECISION      X
      DOUBLE PRECISION      X2
      DOUBLE PRECISION      X3

!
!     The variables below store intermediate results that can be
!     reused if PVINIT is supplied more than once to this routine.
!     In this way, the number of redundant computations can be reduced.
!
      DOUBLE PRECISION      SAVEPV ( 6, BUFSIZ )
      DOUBLE PRECISION      SAVEGM (    BUFSIZ )
      DOUBLE PRECISION      SBOUND (    BUFSIZ )
      DOUBLE PRECISION      SB2RV  (    BUFSIZ )
      DOUBLE PRECISION      SBQ    (    BUFSIZ )
      DOUBLE PRECISION      SBR0   (    BUFSIZ )
      DOUBLE PRECISION      SF     (    BUFSIZ )
      DOUBLE PRECISION      SQOVR0 (    BUFSIZ )

      INTEGER               NEWEST (    BUFSIZ )
      INTEGER               NSAVED

!
!     Variables used to bracket X in our solution of Kepler's equation.
!
      DOUBLE PRECISION      BOUND
      DOUBLE PRECISION      FIXED
      DOUBLE PRECISION      LOGDPM
      DOUBLE PRECISION      LOGF
      DOUBLE PRECISION      LOGMXC
      DOUBLE PRECISION      MAXC
      DOUBLE PRECISION      ROOTF

      INTEGER               BUMPED
      INTEGER               I
      INTEGER               K
      INTEGER               MOSTC
      INTEGER               LCOUNT

      LOGICAL               NEW

!
!     Save everything.
!
      SAVE

!
!     Initial values
!
      DATA                  NSAVED  / 0       /
      DATA                  NEWEST  / 1, 2, 3 /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PROP2B' )
      END IF

!
!     Life will be easier if we use POS and VEL to hold the state.
!
      POS(1) = PVINIT(1)
      POS(2) = PVINIT(2)
      POS(3) = PVINIT(3)

      VEL(1) = PVINIT(4)
      VEL(2) = PVINIT(5)
      VEL(3) = PVINIT(6)

!
!     If we propagate many states from the same initial state,
!     most of the variables used to propagate the state will
!     not change in value.
!
!     To save time needed to compute these variables, we recompute
!     variables that depend upon the initial state only when the
!     initial state is not one of those already buffered by this
!     routine.
!
!     Determine whether or not this GM and state are the same as the
!     one of those already buffered.  Note that we look through the
!     saved states and GM from the most recently input values of PVINIT
!     and GM to the oldest saved state and GM.
!
!     NEWEST(1)  contains the most recently input initial conditions
!     NEWEST(2)  contains the next most recently input initial
!                conditions etc.
!
!     Also note that when this routine starts up there will be no
!     buffered states or GMs.  Every time we encounter a new state, we
!     will increment the number of saved states NSAVED until we have
!     BUFSIZ states buffered.  From that point on, when a new state is
!     encountered we will overwrite the oldest buffered state.
!
      I   =  0
      NEW = .TRUE.

      DO WHILE ( ( I .LT. NSAVED ) .AND. NEW )

         I   = I + 1
         K   = NEWEST(I)

         NEW =     ( PVINIT(1) .NE. SAVEPV(1,K) ) &
              .OR. ( PVINIT(2) .NE. SAVEPV(2,K) ) &
              .OR. ( PVINIT(3) .NE. SAVEPV(3,K) ) &
              .OR. ( PVINIT(4) .NE. SAVEPV(4,K) ) &
              .OR. ( PVINIT(5) .NE. SAVEPV(5,K) ) &
              .OR. ( PVINIT(6) .NE. SAVEPV(6,K) ) &
              .OR. ( GM        .NE. SAVEGM(  K) )

      END DO


      IF ( .NOT. NEW ) THEN
!
!        We update the order vector NEWEST so that the state being
!        used this time becomes the "youngest" state.
!
         K      = I
         BUMPED = NEWEST(K)

         DO I = K, 2, -1
            NEWEST(I) = NEWEST(I-1)
         END DO

         NEWEST(1) = BUMPED
         K         = BUMPED

!
!        Now look up all of the other saved quantities.
!
         B2RV   = SB2RV (K)
         BOUND  = SBOUND(K)
         BQ     = SBQ   (K)
         BR0    = SBR0  (K)
         F      = SF    (K)
         QOVR0  = SQOVR0(K)

      ELSE

!
!        We have a new state, new GM or both.  First let's make sure
!        there is nothing obviously wrong with them.  (We buffer
!        only states, GMs and intermediate values that are "good.")
!        First check for nonpositive mass.
!
         IF ( GM .LE. 0.0D0 ) THEN
            CALL SIGERR ( 'SPICE(NONPOSITIVEMASS)'   )
            CALL CHKOUT ( 'PROP2B'                   )
            RETURN
         END IF

!
!        Next for a zero position vector
!
         IF ( VZERO(POS) ) THEN
            CALL SIGERR ( 'SPICE(ZEROPOSITION)'   )
            CALL CHKOUT ( 'PROP2B'                )
            RETURN
         END IF

!
!        Finally for a zero velocity vector
!
         IF ( VZERO(VEL) ) THEN
            CALL SIGERR ( 'SPICE(ZEROVELOCITY)'   )
            CALL CHKOUT ( 'PROP2B'                )
            RETURN
         END IF

!
!        Obvious problems have been checked. Here are the relevant
!        equations. Let ...
!
!           GM        be the gravitational attraction of the central
!                     mass.
!
!           POS and   be the initial position and velocity respectively
!           VEL       of the orbiting object.
!
!           R0       be the magnitude of the position vector POS
!
!           RV       be the value of the dot product  POS * VEL
!
         R0 = VNORM ( POS      )
         RV = VDOT  ( POS, VEL )

!
!        Let HVEC be the specific angular momentum vector and let Q be
!        the distance at periapse.
!
!                   1)    HVEC  =   POS  x  VEL
!
!                                       2
!                   2)    H2    = |HVEC|  =  GM*(1+E)*Q
!
!
         CALL VCRSS ( POS,  VEL,     HVEC )
         H2 = VDOT  ( HVEC, HVEC          )


!
!        Let's make sure we are not in the pathological case of
!        rectilinear motion.
!
         IF ( H2 .EQ. 0 ) THEN
            CALL SIGERR ( 'SPICE(NONCONICMOTION)'   )
            CALL CHKOUT ( 'PROP2B'                  )
            RETURN
         END IF

!
!        Let E be the eccentricity of the orbit.
!
!        Let QVEC be the unit vector that points toward perihelion, and
!        let EQVEC be QVEC scaled by E.
!
!                                   VEL X HVEC      POS
!                    1)  E*QVEC  =  ----------  -   ---
!                                       GM           R0
!
!
!                                         VEL X HVEC      POS
!                    2)  E       = NORM ( ----------  -   --- )
!                                            GM            R0
!
!
         CALL VCRSS ( VEL,       HVEC,                      TMPVEC  )
         CALL VLCOM ( 1.0D0/GM,  TMPVEC,  -1.0D0/R0,  POS,  EQVEC   )

         E  = VNORM ( EQVEC )

!
!        Solve the equation H2 = GM*Q*(1+E) for Q.
!
         Q  = H2 / ( GM * (1+E) )

!
!        From the discussion of the universal variables formulation in
!        Danby's book on pages 174 and 175 (see the reference listed
!        above) you can show that by making the substitutions
!
!              F  =  1 - E
!
!        and
!
!                       _____
!                      /  Q
!              S  =   / -----    X   = B * X
!                   \/   GM
!
!
!        that DT satisfies the universal variables Kepler's equation:
!
!                                   2     2     2        2
!              DT =  B*R0*X*C_1( F*X ) + B *RV*X C_2( F*X )
!
!                                               3        2
!                                      +   B*Q*X C_3( F*X )
!
!                 =  KFUN( X )
!
!        (where C_k is used to denote the Stumpff functions. This is
!        the universal variables formulation of Kepler's equation.
!        KFUN is our abbreviation for "Kepler function.")
!
!        (One might wonder, "Why make such a change of variables?"
!        By making this substitution early in the derivation supplied
!        in Danby's book, you can always deal with physically
!        meaningful quantities --- the pure numeric value of F and the
!        distance of periapse.  Thus one does not need to be concerned
!        about infinite or negative semi-major axes or with discussing
!        how to interpret these somewhat artificial artifacts of the
!        classical derivations for two body motion.)
!
!        Given the unique X for which this Kepler's equation is
!        satisfied, we can compute the state of the orbiting object
!        at a time DT past the epoch of the state POS and VEL.
!        Evidently we will need the constants:
!
         F      = 1.0D0 - E
         B      = DSQRT ( Q / GM )
         BR0    = B*R0
         B2RV   = B*B*RV
         BQ     = B*Q

!
!        The state corresponding to the value of X that solves this
!        equation is given by
!
!              PC * POS + VC * VEL              ( position )
!
!        and
!
!              PCDOT * POS + VCDOT * VEL        ( velocity )
!
!        where
!                                            2        2
!           ( 1 )    PC    =  1  -  ( Q/R0 )X C_2( F*X )
!
!                                            3        2
!           ( 2 )    VC    =  DT -  ( B*Q  )X C_3( F*X )
!
!
!                                       Q               2
!           ( 3 )    PCDOT =     -  ( ------ ) X C_1( F*X )
!                                     B*R*R0
!
!                                      B*Q     2        2
!           ( 4 )    VCDOT =  1  -  (  ---  ) X C_2( F*X )
!                                      B*R
!
!        Here R denotes the distance from the center of CP*POS + CV*VEL
!        It turns out that R can be computed as:
!
!                                        2     2             2
!           ( 5 )   B*R    = B*R0 C_0(F*X ) + B *RV X C_1(F*X )
!
!                                                 2       2
!                                        +   B*Q X C_2(F*X )
!
!
!        Therefore we will also need the constant
!
         QOVR0   = Q / R0

!
!        We will have to find the unique value of X such that
!
!             DT = KFUN ( X )
!
!        where KFUN stands for the "Kepler function" defined by the
!        equation below:
!
!                                   2
!        KFUN(X) =   B*R0*X * C_1(FX )
!
!                   2     2        2
!                + B *RV*X * C_2(FX )
!
!                         3        2
!                +   B*Q*X * C_3(FX )
!
!
!        (There is a unique solution to this equation. KFUN(X) is
!        unbounded above and below and is an increasing function
!        over all real X for all non-rectilinear orbits. To see this
!        we note that the variable X is a function of DT and is given
!        by the integral from 0 to DT of the differential:
!
!                   dt
!                 ------
!                 B*R(t)
!
!        where R(t) is the range of the body as a function of time.
!        Therefore X is an increasing function of DT, and DT must
!        also be an increasing function of X.
!
!        Thus, there is a unique value of X  that solves this
!        equation).
!
!        If F is less than zero, we can have the computation of C0,...
!        overflow.  This is because for X < 0
!
!
!               C_0(X) = COSH( DSQRT(-X) )
!
!               C_1(X) = SINH( DSQRT(-X) )
!                        -----------------
!                              DSQRT(-X)
!
!
!
!        and from the recursion relationship we know that
!
!
!               C_2(X) =  ( 1/0! - C_0(X) ) / X
!
!               C_3(X) =  ( 1/1! - C_1(X) ) / X
!
!
!                         1 - COSH( DSQRT(-X) )
!               C_2(X) = ------------------------
!                                  X
!
!                         1  - SINH( DSQRT(-X) ) / DSQRT(-X)
!               C_3(X) = -----------------------------------
!                                    X
!
!        Clearly for negative values of F*X*X having large magnitude,
!        it is easy to get an overflow.
!
!        In the case when F is less than 0 we choose X so that we can
!        compute all of the following:
!
!               | COEF_0 * X**0 * C_0(FX**2) |
!
!               | COEF_1 * X**1 * C_1(FX**2) |
!
!               | COEF_2 * X**2 * C_2(FX**2) |
!
!               | COEF_3 * X**3 * C_3(FX**2) |
!
!
!         where COEF_n are coefficients that will be used in forming
!         linear combinations of X**n C_n(FX**2) terms.
!
!         The variable portion of the last 3 terms above can be
!         rewritten as:
!
!
!                                   SINH ( DSQRT(-F)*|X| )
!        | X**1 * C_1(FX**2) |  =   ----------------------
!                                          DSQRT(-F)
!
!
!
!                                   1 - COSH( DSQRT(-F)*|X| )
!        | X**2 * C_2(FX**2) |  =  ----------------------------
!                                             -F
!
!
!                                  DSQRT(-F)*|X|   - SINH(DSQRT(-F)*|X|)
!        | X**3 * C_3(FX**2) |  =  -------------------------------------
!                                              F*DSQRT(-F)
!
!
!        For large |X| the absolute values of these expressions are well
!        approximated by
!
!                                         0.0
!               COSH( DSQRT(-F)|X| ) * |F|
!
!                                         -0.5
!               SINH( DSQRT(-F)|X| ) * |F|
!
!                                         -1.0
!               COSH( DSQRT(-F)|X| ) * |F|
!
!                                         -1.5
!               SINH( DSQRT(-F)|X| ) * |F|
!
!
!        For large |X| the logarithms of these expressions are well
!        approximated by:
!
!
!               DSQRT(-F)|X| - LOG(2) - 0.0*LOG(-F)
!
!               DSQRT(-F)|X| - LOG(2) - 0.5*LOG(-F)
!
!               DSQRT(-F)|X| - LOG(2) - 1.0*LOG(-F)
!
!               DSQRT(-F)|X| - LOG(2) - 1.5*LOG(-F)
!
!        respectively.
!
!
!        To ensure that we can form a linear combination of these terms
!        we will require that:
!
!
!           |COEF_N*X**N * C_N(FX**2)| < DPMAX / 4
!
!
!
!        for N=0,1,2,3.  This is equivalent to
!
!              LOG ( X**N * C_N(FX**2) )   <      LOG ( DPMAX )
!            + LOG (|COEF_N|)                   - 2 LOG ( 2     )
!
!
!
!        or
!
!              LOG ( X**N * C_N(FX**2) )   <      LOG ( DPMAX    )
!                                             -   LOG ( |COEF_N| )
!                                             - 2*LOG ( 2        ).
!
!
!        Replacing the left hand side with the magnitude expressions
!        computed above we have:
!
!            DSQRT(-F)|X| - LOG(2) - N*0.5*LOG( -F )  <   LOG ( DPMAX  )
!                                                      -  LOG (|COEF_N|)
!                                                      -2*LOG ( 2      )
!
!         So that:
!
!
!            |X|  <    {   LOG ( DPMAX  )
!                        - LOG (|COEF_N|)
!                        - LOG (  2     )
!                        + LOG ( -F     )*N*0.5 } / DSQRT(-F)
!
!         Let MAXC be the maximum of 1.0D0 and the various coefficients
!         of the Stumpff functions.  We can then set our absolute value
!         bound on X to be:
!
!
!             MIN        LOG(DPMAX/2) - LOG(MAXC) + (n/2)LOG(-F)
!            n = 0,3  {  -----------------------------------------  }
!                               DSQRT(-F)
!
!        (Actually we know that the minimum must occur for n = 0 or
!        for n = 3).
!
!
         MAXC   = MAX   ( 1.0D0, DABS(BR0), &
                                 DABS(B2RV), &
                                 DABS(BQ), &
                                 DABS(QOVR0/BQ) )

         IF ( F .LT. 0 ) THEN

            LOGMXC = LOG   ( MAXC            )
            LOGDPM = LOG   ( DPMAX() / 2.0D0 )

            FIXED  = LOGDPM  - LOGMXC

            ROOTF  = DSQRT ( - F             )
            LOGF   = LOG   ( - F             )

            BOUND  = MIN (  (FIXED              )/ROOTF, &
                            (FIXED + 1.5D0*LOGF )/ROOTF )

!
!           Note that in the above, we can always perform the division
!           by ROOTF.  To see this we note that -F is at least the
!           machine precision (we got it by subtracting E from 1.)
!           Thus its square root is a reasonably large number (if F is
!           10**-N then ROOTF is 10**(-N/2) )  The value of FIXED is
!           about 3*M where M is the largest exponent such that 2**M
!           is representable on the host machine.  Thus BOUND is at
!           worst M*10**(N/2)  This will always be computable.
!
        ELSE

!
!
!           In the case when F is non-negative we must be sure we
!           can compute all of the following.
!
!               | COEF_0 * X**0 * C_0(FX**2) | < | COEF_0          |
!
!               | COEF_1 * X**1 * C_1(FX**2) | < | COEF_1*|X|      |
!
!               | COEF_2 * X**2 * C_2(FX**2) | < | COEF_2*X**2 / 2 |
!
!               | COEF_3 * X**3 * C_3(FX**2) | < | COEF_3*X**3 / 6 |
!
!           If we assume that COEF_0 is computable, all of these are
!           bounded above by:
!
!                       | MAX(COEF_1,...COEF_3) * X**3 / 6 |
!
!           We want to make sure we can add these terms so we need to
!           make sure that
!
!              | MAX(COEF_1,...,COEF_3) * X**3 / 6 | < DPMAX() / 4.
!
!           Thus we need:
!
!              |X**3| <          1.5*DPMAX / MAX(COEF_1,...,COEF_3)
!              |X|    <  DCBRT ( 1.5*DPMAX / MAX(COEF_1,...,COEF_3) )
!
!           (We'll use logarithms to compute the upper bound for |X|.)
!
            LOGBND =      ( DLOG(1.5D0) + DLOG(DPMAX()) - DLOG(MAXC) ) &
                     /      3.0D0

            BOUND  = DEXP ( LOGBND )

         END IF
!
!        All the obvious problems have been checked, move everybody
!        on the list down and put the new guy on top of the list.
!

         NSAVED = BRCKTI ( NSAVED+1, 1, BUFSIZ )
         BUMPED = NEWEST ( NSAVED              )

         DO I = NSAVED, 2, -1
            NEWEST(I) = NEWEST(I-1)
         END DO

         NEWEST(1) = BUMPED
         K         = BUMPED

         SAVEPV(1,K) = PVINIT(1)
         SAVEPV(2,K) = PVINIT(2)
         SAVEPV(3,K) = PVINIT(3)
         SAVEPV(4,K) = PVINIT(4)
         SAVEPV(5,K) = PVINIT(5)
         SAVEPV(6,K) = PVINIT(6)
         SAVEGM(  K) = GM

!
!        Finally we save the results of all of the above
!        computations so that we won't have to do them again,
!        if this initial state and GM are entered again.
!
         SB2RV (K) = B2RV
         SBOUND(K) = BOUND
         SBQ   (K) = BQ
         SBR0  (K) = BR0
         SF    (K) = F
         SQOVR0(K) = QOVR0

      END IF
!
!
!     We are now ready to find the unique value of X such that
!
!             DT = KFUN ( X )
!
!     First we must bracket the root. The basic idea is this:
!
!     1) KFUN(0) = 0 so we will let one endpoint of our initial
!        guess of a bracketing interval be 0.
!
!     2) We get our initial guess at the other endpoint of the
!        bracketing interval by recalling that
!
!                   dt
!         dX  =   ------
!                 B*R(t)
!
!        From this observation it follows that
!
!                   DT
!          X  <  -------
!                   B*Q
!
!        Thus the solution to
!
!             DT = KFUN ( X )
!
!        Satisfies
!
!                     DT
!         0 < X  <  -------
!                    B*Q
!
!
!        We now have a guess at a bracketing interval. In the case
!        DT is positive it looks like
!
!                0        X
!         -------[--------]-----------------------------
!
!        This is ok mathematically, but due to rounding etc it is
!        conceivable that we might not have bracketed the root.
!        We check and if not we will double the
!        endpoint farthest from zero and call this X, and make
!        the other endpoint the old value of X.
!
!
!                0
!         -------+--------[--------]--------------------
!
!
!        We continue this process ...
!
!                0
!         -------+-----------------[-----------------]--
!
!        ...until the root is bracketed. (One shift is certain
!        to do the job).
!
!        If we perform this interval shift, we will have to take
!        care that X does not run out of the domain for which
!        we can safely compute KFUN.  Thus we will make sure that
!        the endpoints of these shifted intervals always stay safely
!        inside the domain for which KFUN can be computed.
!
      X    = DT / BQ
      X    = BRCKTD ( X, -BOUND, BOUND )
      FX2  = F*X*X

      CALL STMP03 ( FX2, C0, C1, C2, C3 )

      KFUN =   X*(BR0*C1 + X*( B2RV*C2 + X*(BQ*C3) ) )

      IF ( DT .LT. 0 ) THEN

         UPPER = 0
         LOWER = X

         DO WHILE ( KFUN .GT. DT )

            UPPER = LOWER
            LOWER = LOWER * 2.0D0
            OLDX  = X
            X     = BRCKTD ( LOWER, -BOUND, BOUND )

!
!           Make sure we are making progress. (In other words make sure
!           we don't run into the boundary of values that X can assume.
!           If we do run into the boundary, X will be unchanged and
!           there's nothing further we can do.  We'll have to call it
!           quits and tell the user what happened.)
!
            IF ( X .EQ. OLDX ) THEN

               FX2 = F*BOUND*BOUND

               CALL STMP03 ( FX2, C0, C1, C2, C3 )

               KFUNL = -BOUND*(BR0*C1 - BOUND*(B2RV*C2 - BOUND*BQ*C3))
               KFUNU =  BOUND*(BR0*C1 + BOUND*(B2RV*C2 + BOUND*BQ*C3))

               CALL SETMSG ( 'The input delta time (DT) has a '       // &
                             'value of #.  This is beyond the '       // &
                             'range of DT for which we can '          // &
                             'reliably propagate states. The limits ' // &
                             'for this GM and initial state are '     // &
                             'from # to #. '                          )
               CALL ERRDP  ( '#', DT                                  )
               CALL ERRDP  ( '#', KFUNL                               )
               CALL ERRDP  ( '#', KFUNU                               )
               CALL SIGERR ( 'SPICE(DTOUTOFRANGE)'                    )
               CALL CHKOUT ( 'PROP2B'                                 )
               RETURN
            END IF

            FX2   = F*X*X

            CALL STMP03 ( FX2, C0, C1, C2, C3 )

            KFUN = X*(BR0*C1 + X*( B2RV*C2 + X*(BQ*C3) ) )

         END DO


      ELSE IF ( DT .GT. 0 ) THEN

         LOWER = 0
         UPPER = X

         DO WHILE ( KFUN .LT. DT )

            LOWER = UPPER
            UPPER = UPPER * 2.0D0
            OLDX  = X
            X     = BRCKTD ( UPPER, -BOUND, BOUND )
!
!           Make sure we are making progress.
!
            IF ( X .EQ. OLDX ) THEN

               FX2 = F*BOUND*BOUND

               CALL STMP03 ( FX2, C0, C1, C2, C3 )

               KFUNL = -BOUND*(BR0*C1 - BOUND*(B2RV*C2 - BOUND*BQ*C3))
               KFUNU =  BOUND*(BR0*C1 + BOUND*(B2RV*C2 + BOUND*BQ*C3))

               CALL SETMSG ( 'The input delta time (DT) has a '       // &
                             'value of #.  This is beyond the '       // &
                             'range of DT for which we can '          // &
                             'reliably propagate states. The limits ' // &
                             'for this GM and initial state are '     // &
                             'from # to #. '                          )
               CALL ERRDP  ( '#', DT                                  )
               CALL ERRDP  ( '#', KFUNL                               )
               CALL ERRDP  ( '#', KFUNU                               )
               CALL SIGERR ( 'SPICE(DTOUTOFRANGE)'                    )
               CALL CHKOUT ( 'PROP2B'                                 )
               RETURN
            END IF

            FX2  = F*X*X

            CALL STMP03 ( FX2, C0, C1, C2, C3 )

            KFUN = X*(BR0*C1 + X*( B2RV*C2 + X*BQ*C3 ) )

         END DO

      ELSE

         CALL VEQUG  ( PVINIT, 6, PVPROP )
         CALL CHKOUT ( 'PROP2B'          )
         RETURN

      END IF

!
!     Ok. We've bracketed the root.  Now for lack of anything more
!     clever, we just bisect to find the solution.
!
!     We add a loop counter so that we can ensure termination of the
!     loop below.
!
!     On some systems the computed midpoint is stored in an extended
!     precision register.  Thus the midpoint is always different from
!     UPPER and LOWER.  Yet when the new value of LOWER and UPPER
!     are assigned UPPER and LOWER do not change and hence the
!     loop fails to terminate.  With the loop counter we force
!     termination of the loop.
!
      X    = MIN ( UPPER, MAX ( LOWER, (LOWER + UPPER)/2.0D0) )
      FX2  = F*X*X

      CALL STMP03 ( FX2, C0, C1, C2, C3 )
      LCOUNT = 0
      MOSTC  = 1000

      DO WHILE (         X      .GT. LOWER &
                 .AND.   X      .LT. UPPER &
                 .AND.   LCOUNT .LT. MOSTC )

         KFUN = X*(BR0*C1 + X*( B2RV*C2 + X*BQ*C3 ) )

         IF ( KFUN .GT. DT ) THEN
            UPPER = X
         ELSE IF ( KFUN .LT. DT ) THEN
            LOWER = X
         ELSE
            UPPER = X
            LOWER = X
         END IF
!
!        As soon as the bracketing values move away from
!        zero we can modify the count limit.
!
         IF ( MOSTC .GT. MAXBIT ) THEN

            IF ( UPPER .NE. 0.0D0 .AND. LOWER .NE. 0.0D0 ) THEN
               MOSTC   = MAXBIT
               LCOUNT = 0
            END IF

         END IF

         X    = MIN ( UPPER, MAX ( LOWER, (LOWER + UPPER)/2.0D0 ) )
         FX2  = F*X*X

         CALL STMP03 ( FX2, C0, C1, C2, C3 )

         LCOUNT = LCOUNT + 1

      END DO


!
!     With X in hand we simply compute BR, PC, VC, PCDOT and VCDOT
!     described in equations (1) --- (5) above. (Note, by our choice
!     of BOUND above, one can show that none of the computations
!     below can cause an overflow).
!
      X2    = X *X
      X3    = X2*X
      BR    = BR0*C0 + X*( B2RV*C1  + X*(BQ*C2) )


      PC    = 1.0D0     -      QOVR0        * X2 * C2
      VC    = DT        -      BQ           * X3 * C3
      PCDOT =           -     (QOVR0 / BR ) * X  * C1
      VCDOT = 1.0D0     -     (BQ    / BR ) * X2 * C2


!
!     ... and compute the linear combinations needed to get PVPROP
!
      CALL VLCOM ( PC,    POS, VC,    VEL,       PVPROP(1) )
      CALL VLCOM ( PCDOT, POS, VCDOT, VEL,       PVPROP(4) )

      CALL CHKOUT ( 'PROP2B' )
      RETURN

      END
