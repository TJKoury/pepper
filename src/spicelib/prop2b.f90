!$Procedure PROP2B ( Propagate a two-body solution )

      subroutine prop2b ( gm, pvinit, dt, pvprop )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Compute the state of a massless body at time t_0 + DT by applying
!     the two-body force model to a given central mass and a given body
!     state at time t_0.
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

      implicit none

      real(wp)              gm
      real(wp)              pvinit ( 6 )
      real(wp)              dt
      real(wp)              pvprop ( 6 )

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
      real(wp)              brcktd
      real(wp)              dpmax
      real(wp)              vdot
      real(wp)              vnorm

      integer(ip)           brckti

      logical               return
      logical               vzero

!
!     Local Parameters
!
      integer(ip)           bufsiz
      parameter           ( bufsiz = 3 )

      integer(ip)           maxbit
      parameter           ( maxbit = 64 )

!
!     Local variables
!
!
!     The following quantities are needed in the solution of Kepler's
!     equation and in the propagation of the input state.  They are
!     described as they are introduced in the code below.
!
      real(wp)              b
      real(wp)              b2rv
      real(wp)              bq
      real(wp)              br
      real(wp)              br0
      real(wp)              c0
      real(wp)              c1
      real(wp)              c2
      real(wp)              c3
      real(wp)              e
      real(wp)              eqvec  ( 3 )
      real(wp)              f
      real(wp)              fx2
      real(wp)              h2
      real(wp)              hvec   ( 3 )
      real(wp)              kfun
      real(wp)              kfunl
      real(wp)              kfunu
      real(wp)              logbnd
      real(wp)              lower
      real(wp)              oldx
      real(wp)              pc
      real(wp)              pcdot
      real(wp)              pos    ( 3 )
      real(wp)              q
      real(wp)              qovr0
      real(wp)              r0
      real(wp)              rv
      real(wp)              tmpvec ( 3 )
      real(wp)              upper
      real(wp)              vc
      real(wp)              vcdot
      real(wp)              vel    ( 3 )
      real(wp)              x
      real(wp)              x2
      real(wp)              x3

!
!     The variables below store intermediate results that can be
!     reused if PVINIT is supplied more than once to this routine.
!     In this way, the number of redundant computations can be reduced.
!
      real(wp)              savepv ( 6, bufsiz )
      real(wp)              savegm (    bufsiz )
      real(wp)              sbound (    bufsiz )
      real(wp)              sb2rv  (    bufsiz )
      real(wp)              sbq    (    bufsiz )
      real(wp)              sbr0   (    bufsiz )
      real(wp)              sf     (    bufsiz )
      real(wp)              sqovr0 (    bufsiz )

      integer(ip)           newest (    bufsiz )
      integer(ip)           nsaved

!
!     Variables used to bracket X in our solution of Kepler's equation.
!
      real(wp)              bound
      real(wp)              fixed
      real(wp)              logdpm
      real(wp)              logf
      real(wp)              logmxc
      real(wp)              maxc
      real(wp)              rootf

      integer(ip)           bumped
      integer(ip)           i
      integer(ip)           k
      integer(ip)           mostc
      integer(ip)           lcount

      logical               new

!
!     Save everything.
!
      save

!
!     Initial values
!
      data                  nsaved  / 0       /
      data                  newest  / 1, 2, 3 /

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Life will be easier if we use POS and VEL to hold the state.
!
      pos(1) = pvinit(1)
      pos(2) = pvinit(2)
      pos(3) = pvinit(3)

      vel(1) = pvinit(4)
      vel(2) = pvinit(5)
      vel(3) = pvinit(6)

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
      i   =  0
      new = .true.

      do while ( ( i < nsaved ) .and. new )

         i   = i + 1
         k   = newest(i)

         new =     ( pvinit(1) /= savepv(1,k) ) &
              .or. ( pvinit(2) /= savepv(2,k) ) &
              .or. ( pvinit(3) /= savepv(3,k) ) &
              .or. ( pvinit(4) /= savepv(4,k) ) &
              .or. ( pvinit(5) /= savepv(5,k) ) &
              .or. ( pvinit(6) /= savepv(6,k) ) &
              .or. ( gm        /= savegm(  k) )

      end do


      if ( .not. new ) then
!
!        We update the order vector NEWEST so that the state being
!        used this time becomes the "youngest" state.
!
         k      = i
         bumped = newest(k)

         do i = k, 2, -1
            newest(i) = newest(i-1)
         end do

         newest(1) = bumped
         k         = bumped

!
!        Now look up all of the other saved quantities.
!
         b2rv   = sb2rv (k)
         bound  = sbound(k)
         bq     = sbq   (k)
         br0    = sbr0  (k)
         f      = sf    (k)
         qovr0  = sqovr0(k)

      else

!
!        We have a new state, new GM or both.  First let's make sure
!        there is nothing obviously wrong with them.  (We buffer
!        only states, GMs and intermediate values that are "good.")
!        First check for nonpositive mass.
!
         if ( gm <= 0.0_wp ) then
            call sigerr ( 'SPICE(NONPOSITIVEMASS)'   )
            return
         end if

!
!        Next for a zero position vector
!
         if ( vzero(pos) ) then
            call sigerr ( 'SPICE(ZEROPOSITION)'   )
            return
         end if

!
!        Finally for a zero velocity vector
!
         if ( vzero(vel) ) then
            call sigerr ( 'SPICE(ZEROVELOCITY)'   )
            return
         end if

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
         r0 = vnorm ( pos      )
         rv = vdot  ( pos, vel )

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
         call vcrss ( pos,  vel,     hvec )
         h2 = vdot  ( hvec, hvec          )


!
!        Let's make sure we are not in the pathological case of
!        rectilinear motion.
!
         if ( h2 == 0 ) then
            call sigerr ( 'SPICE(NONCONICMOTION)'   )
            return
         end if

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
         call vcrss ( vel,       hvec,                      tmpvec  )
         call vlcom ( 1.0_wp/gm,  tmpvec,  -1.0_wp/r0,  pos,  eqvec   )

         e  = vnorm ( eqvec )

!
!        Solve the equation H2 = GM*Q*(1+E) for Q.
!
         q  = h2 / ( gm * (1+e) )

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
         f      = 1.0_wp - e
         b      = dsqrt ( q / gm )
         br0    = b*r0
         b2rv   = b*b*rv
         bq     = b*q

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
         qovr0   = q / r0

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
         maxc   = max   ( 1.0_wp, dabs(br0), &
                                 dabs(b2rv), &
                                 dabs(bq), &
                                 dabs(qovr0/bq) )

         if ( f < 0 ) then

            logmxc = log   ( maxc            )
            logdpm = log   ( dpmax() / 2.0_wp )

            fixed  = logdpm  - logmxc

            rootf  = dsqrt ( - f             )
            logf   = log   ( - f             )

            bound  = min (  (fixed              )/rootf, &
                            (fixed + 1.5_wp*logf )/rootf )

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
        else

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
            logbnd =      ( dlog(1.5_wp) + dlog(dpmax()) - dlog(maxc) ) &
                     /      3.0_wp

            bound  = dexp ( logbnd )

         end if
!
!        All the obvious problems have been checked, move everybody
!        on the list down and put the new guy on top of the list.
!

         nsaved = brckti ( nsaved+1, 1, bufsiz )
         bumped = newest ( nsaved              )

         do i = nsaved, 2, -1
            newest(i) = newest(i-1)
         end do

         newest(1) = bumped
         k         = bumped

         savepv(1,k) = pvinit(1)
         savepv(2,k) = pvinit(2)
         savepv(3,k) = pvinit(3)
         savepv(4,k) = pvinit(4)
         savepv(5,k) = pvinit(5)
         savepv(6,k) = pvinit(6)
         savegm(  k) = gm

!
!        Finally we save the results of all of the above
!        computations so that we won't have to do them again,
!        if this initial state and GM are entered again.
!
         sb2rv (k) = b2rv
         sbound(k) = bound
         sbq   (k) = bq
         sbr0  (k) = br0
         sf    (k) = f
         sqovr0(k) = qovr0

      end if
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
      x    = dt / bq
      x    = brcktd ( x, -bound, bound )
      fx2  = f*x*x

      call stmp03 ( fx2, c0, c1, c2, c3 )

      kfun =   x*(br0*c1 + x*( b2rv*c2 + x*(bq*c3) ) )

      if ( dt < 0 ) then

         upper = 0
         lower = x

         do while ( kfun > dt )

            upper = lower
            lower = lower * 2.0_wp
            oldx  = x
            x     = brcktd ( lower, -bound, bound )

!
!           Make sure we are making progress. (In other words make sure
!           we don't run into the boundary of values that X can assume.
!           If we do run into the boundary, X will be unchanged and
!           there's nothing further we can do.  We'll have to call it
!           quits and tell the user what happened.)
!
            if ( x == oldx ) then

               fx2 = f*bound*bound

               call stmp03 ( fx2, c0, c1, c2, c3 )

               kfunl = -bound*(br0*c1 - bound*(b2rv*c2 - bound*bq*c3))
               kfunu =  bound*(br0*c1 + bound*(b2rv*c2 + bound*bq*c3))

               call setmsg ( 'The input delta time (DT) has a '       // &
                             'value of #.  This is beyond the '       // &
                             'range of DT for which we can '          // &
                             'reliably propagate states. The limits ' // &
                             'for this GM and initial state are '     // &
                             'from # to #. '                          )
               call errdp  ( '#', dt                                  )
               call errdp  ( '#', kfunl                               )
               call errdp  ( '#', kfunu                               )
               call sigerr ( 'SPICE(DTOUTOFRANGE)'                    )
               return
            end if

            fx2   = f*x*x

            call stmp03 ( fx2, c0, c1, c2, c3 )

            kfun = x*(br0*c1 + x*( b2rv*c2 + x*(bq*c3) ) )

         end do


      else if ( dt > 0 ) then

         lower = 0
         upper = x

         do while ( kfun < dt )

            lower = upper
            upper = upper * 2.0_wp
            oldx  = x
            x     = brcktd ( upper, -bound, bound )
!
!           Make sure we are making progress.
!
            if ( x == oldx ) then

               fx2 = f*bound*bound

               call stmp03 ( fx2, c0, c1, c2, c3 )

               kfunl = -bound*(br0*c1 - bound*(b2rv*c2 - bound*bq*c3))
               kfunu =  bound*(br0*c1 + bound*(b2rv*c2 + bound*bq*c3))

               call setmsg ( 'The input delta time (DT) has a '       // &
                             'value of #.  This is beyond the '       // &
                             'range of DT for which we can '          // &
                             'reliably propagate states. The limits ' // &
                             'for this GM and initial state are '     // &
                             'from # to #. '                          )
               call errdp  ( '#', dt                                  )
               call errdp  ( '#', kfunl                               )
               call errdp  ( '#', kfunu                               )
               call sigerr ( 'SPICE(DTOUTOFRANGE)'                    )
               return
            end if

            fx2  = f*x*x

            call stmp03 ( fx2, c0, c1, c2, c3 )

            kfun = x*(br0*c1 + x*( b2rv*c2 + x*bq*c3 ) )

         end do

      else

         call vequg  ( pvinit, 6, pvprop )
         return

      end if

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
      x    = min ( upper, max ( lower, (lower + upper)/2.0_wp) )
      fx2  = f*x*x

      call stmp03 ( fx2, c0, c1, c2, c3 )
      lcount = 0
      mostc  = 1000

      do while (         x      > lower &
                 .and.   x      < upper &
                 .and.   lcount < mostc )

         kfun = x*(br0*c1 + x*( b2rv*c2 + x*bq*c3 ) )

         if ( kfun > dt ) then
            upper = x
         else if ( kfun < dt ) then
            lower = x
         else
            upper = x
            lower = x
         end if
!
!        As soon as the bracketing values move away from
!        zero we can modify the count limit.
!
         if ( mostc > maxbit ) then

            if ( upper /= 0.0_wp .and. lower /= 0.0_wp ) then
               mostc   = maxbit
               lcount = 0
            end if

         end if

         x    = min ( upper, max ( lower, (lower + upper)/2.0_wp ) )
         fx2  = f*x*x

         call stmp03 ( fx2, c0, c1, c2, c3 )

         lcount = lcount + 1

      end do


!
!     With X in hand we simply compute BR, PC, VC, PCDOT and VCDOT
!     described in equations (1) --- (5) above. (Note, by our choice
!     of BOUND above, one can show that none of the computations
!     below can cause an overflow).
!
      x2    = x *x
      x3    = x2*x
      br    = br0*c0 + x*( b2rv*c1  + x*(bq*c2) )


      pc    = 1.0_wp     -      qovr0        * x2 * c2
      vc    = dt        -      bq           * x3 * c3
      pcdot =           -     (qovr0 / br ) * x  * c1
      vcdot = 1.0_wp     -     (bq    / br ) * x2 * c2


!
!     ... and compute the linear combinations needed to get PVPROP
!
      call vlcom ( pc,    pos, vc,    vel,       pvprop(1) )
      call vlcom ( pcdot, pos, vcdot, vel,       pvprop(4) )

      return

      end
