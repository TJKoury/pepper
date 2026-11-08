!$Procedure ZZTANSLV ( Private --- tangent point solver )

      SUBROUTINE ZZTANSLV ( UDCOND, UDSTEP, UDREFN, &
                            CSTEP,  STEP,   START,  FINISH, &
                            TOL,    RESULT, POINTS, ENDFLG  )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine finds tangent points of rays on a target surface,
!     where the rays are confined to a specified half-plane. It may
!     be used for limb and terminator computations.
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
!     ROOT
!     SEARCH
!     WINDOWS
!
! $ Declarations

      IMPLICIT NONE

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      EXTERNAL              UDCOND
      EXTERNAL              UDSTEP
      EXTERNAL              UDREFN
      LOGICAL               CSTEP
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      START
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      RESULT ( LBCELL : * )
      DOUBLE PRECISION      POINTS ( 3, * )
      LOGICAL               ENDFLG ( 2 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UDCOND     I   Name of the routine that compares the current
!                    state and ray intercept.
!     UDSTEP     I   Name of the routine that computes a step
!     UDREFN     I   Name of the routine that computes a refined input.
!     CSTEP      I   Logical indicating constant step size.
!     STEP       I   Constant step size for finding geometric events.
!     START      I   Beginning of the search interval.
!     FINISH     I   End of the search interval.
!     TOL        I   Maximum error in detection of state transitions.
!     RESULT    I-O  SPICE window containing results.
!     POINTS     O   Array of points associated with transitions.
!     ENDFLG     O   Endpoint transition flags.
!
! $ Detailed_Input
!
!     For the purpose of solving for ray tangency points on a target
!     body, the independent variable can be considered to be angular
!     separation of a ray from an axis vector. For a limb computation,
!     the axis vector points from the target body center toward the
!     observer. For a terminator computation, the axis vector points
!     from the target body center towards the center of the
!     illumination source. The "system state" for these computations is
!     the condition of the ray intersecting the target body: if an
!     intersection exists, the state is "true."
!
!     The discussion below is more general; we'll use the terms
!     "abscissa" or "x-value" rather than "angle" to describe the
!     independent variable. The "system state" is simply a boolean
!     function of the independent variable.
!
!     The first three inputs to this routine are names of subroutines
!     that this routine will call.
!
!     These routines should meet the following specifications.
!
!     UDCOND     the routine that determines if the system state
!                satisfies some constraint at a given independent
!                variable value X.
!
!                The calling sequence:
!
!                   CALL UDCOND ( X, IN_CON, POINT )
!
!                where:
!
!                   X        a double precision value at which to
!                            evaluate the state.
!
!                   IN_CON   a logical value indicating whether
!                            or not the quantity satisfies the
!                            constraint at X (TRUE) or not (FALSE).
!
!                   POINT    is a 3-vector associated with X. POINT
!                            is defined if and only if IN_CON is .TRUE.
!
!
!     UDSTEP     the routine that computes a step in an attempt to
!                find a transition of the state of the specified
!                coordinate. In the context of this routine's algorithm,
!                a "state transition" occurs where the geometric state
!                changes from being in the desired geometric condition
!                event to not, or vice versa.
!
!                This routine relies on UDSTEP returning step sizes
!                small enough so that state transitions within the
!                interval [START, FINISH] are not overlooked.  There
!                must never be two roots A and B separated by less than
!                STEP, where STEP is the minimum step size returned by
!                UDSTEP for any value of X in the interval [A, B].
!
!                The calling sequence for UDSTEP is:
!
!                   CALL UDSTEP ( X, STEP )
!
!                where:
!
!                   X       a double precision value from which the
!                           algorithm is to search forward for a state
!                           transition.
!
!                   STEP    is the output step size. STEP indicates how
!                           far to advance X so that X and X+STEP may
!                           bracket a state transition and definitely
!                           do not bracket more than one state
!                           transition.
!
!                If a constant step size is desired, the routine
!
!                   GFSTEP
!
!                may be used. This is the default option. If using
!                GFSTEP, the step size must be set by calling
!
!                   GFSSTP(STEP)
!
!                prior to calling this routine.
!
!
!     UDREFN     the routine that computes a refinement in the abscissa
!                values that bracket a transition point. In other
!                words, once a pair of abscissa values have been
!                detected such that the system is in different states
!                at each of the two values, UDREFN selects an
!                intermediate abscissa value which should be closer to
!                the transition state than one of the two known X
!                values. The calling sequence for UDREFN is:
!
!                   CALL UDREFN ( X1, X2, S1, S2, T )
!
!                where the inputs are:
!
!                   X1    an X (abscissa) value at which the system is
!                         in state S1.
!
!                   X2    an X value at which the system is in state
!                         S2. X2 is assumed to be larger than X1.
!
!                   S1    a logical indicating the state of the system
!                         at X1.
!
!                   S2    a logical indicating the state of the system
!                         at X2.
!
!                UDREFN may use or ignore the S1 and S2 values.
!
!                The output is:
!
!                   T     an X value to check for a state transition
!                         between X1 and X2.
!
!                If a simple bisection method is desired, the routine
!                GFREFN may be used. This is the default option.
!
!     CSTEP      is a logical indicating whether or not the step size
!                used in searching is constant.  If it is, the value
!                STEP is used. Note that even if UDSTEP has the value
!                GFSTEP, i.e. the public, constant step routine, CSTEP
!                should still be .FALSE., in which case STEP is ignored.
!
!     STEP       is the step size to be used in the search. STEP must
!                be short enough for a search using this step size to
!                locate the intervals where the geometric event
!                function is monotone increasing or decreasing.
!                However, STEP must not be *too* short, or the search
!                will take an unreasonable amount of time.
!
!                The choice of STEP affects the completeness but not
!                the precision of solutions found by this routine;
!                precision is controlled by the convergence the
!                tolerance, TOL.
!
!     START      is the beginning of the interval over which the state
!                is to be detected.
!
!     FINISH     is the end of the interval over which the state is
!                to be detected.
!
!     TOL        is a tolerance value used to determine convergence of
!                root-finding operations. TOL is measured in the units
!                of the independent variable and is greater than zero.
!
!     RESULT     is an initialized SPICE window. RESULT must be large
!                enough to hold all of the intervals found by the
!                search.
!
! $ Detailed_Output
!
!     RESULT     is a SPICE window containing the results of the
!                search. With the exception of the first and last
!                endpoints of the window, the endpoints of the
!                intervals in RESULT always are abscissa values of
!                state transitions. The first and last endpoints may or
!                may not correspond to state transitions.
!
!                The first and last endpoints are abscissa values of
!                state transitions if and only if the state function is
!                .FALSE. at those points.
!
!                Note that, in the special case where the state function
!                is .TRUE. at the first endpoint and .FALSE. in a
!                half-neighborhood to the right of that endpoint, it is
!                possible for this function to find a transition at that
!                endpoint and assign it to the right endpoint of the
!                degenerate interval
!
!                    [ left endpoint,  left endpoint ]
!
!                Analogously, it is possible to find a state transition
!                at the last endpoint if the state function is .TRUE.
!                at that endpoint and .FALSE. in a half-neighborhood
!                to the left of that point.
!
!                The output ENDFLG indicates whether the first and
!                last endpoints of RESULT are transitions.
!
!
!     POINTS     is an array of 3-vectors associated with the endpoints
!                of the intervals of RESULT. Elements
!
!                   POINTS(J,I), J = 1 .. 3
!
!                constitute a vector associated with
!
!                   RESULT(I)
!
!                The first and last vectors of POINTS are valid if
!                and only if the corresponding elements of ENDFLG
!                are .TRUE.
!
!                Presuming this routine is used to solve for tangent
!                points on a target body, the vectors contained in
!                POINTS, when valid, are such tangent points.
!
!                POINTS must be declared with size at least 3 times
!                that of RESULT.
!
!
!     ENDFLG     is an array of two logical flags that indicate
!                whether state transitions occur at the initial
!                and final endpoints of the result window. Element
!                1 of this array is .TRUE. if and only if
!                there is a state transition at the first endpoint of
!                RESULT (in element RESULT(1)); element 2 is .TRUE.
!                if and only if there is a state transition at the
!                last element of RESULT.
!
!
! $ Parameters
!
!     LBCELL     is the SPICELIB cell lower bound.
!
! $ Exceptions
!
!     1)  If TOL is negative, the error SPICE(INVALIDTOLERANCE)
!         will be signaled.
!
!     2)  If START +/- TOL is indistinguishable from START or
!         FINISH +/- TOL is indistinguishable from FINISH, the
!         error SPICE(INVALIDTOLERANCE) will be signaled.
!
!     3)  If START is greater than FINISH, the error
!         SPICE(BOUNDSOUTOFORDER) will be signaled.
!
!     4)  If a constant step is used, the step must be positive and
!         have magnitude large enough so that, when added to a value in
!         the range [START, FINISH], it will yield a distinct value.
!         Otherwise, the error SPICE(INVALIDCONSTSTEP) will be
!         signaled.
!
!     5)  If the step function is used and it returns a value less than
!         the preceding value, the error SPICE(INVALIDSTEP) will
!         be signaled.
!
!     6)  If the inner convergence loop fails to converge to TOL within
!         MXLOOP iterations, the error SPICE(NOCONVERGENCE) will be
!         signaled.
!
!     7)  If the POINTS array doesn't have enough room to store
!         the points associated with state transitions, the error
!         SPICE(ARRAYTOOSMALL) will be signaled.
!
!     8)  If the result window doesn't have enough room to store
!         the abscissas associated with state transitions, the error
!         will be signaled by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     Kernels used by this routine are those needed by the input
!     routines
!
!        UDCOND
!        UDGETP
!        UDSTEP
!        UDREFN
!
! $ Particulars
!
!     This routine supports limb and terminator point detection.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     It is important that the user understand how the routines UDCOND,
!     UDSTEP and UDREFN are to be used and that the calling sequences
!     match precisely with the descriptions given here.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     L.S. Elson     (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 30-JUN-2016 (NJB) (EDW)
!
!        Updated logic for placement of points in output POINTS
!        array. If the first element of RESULT is equal to BEGIN,
!        then space will be reserved in the first element of POINTS,
!        so as to keep the output points synced with the elements
!        of RESULT.
!
!        Updated short error messages.
!
!        Updated header I/O sections.
!
!        12-FEB-2016 (NJB) (EDW)
!
!        Derived from ZZGFSOLV Version 1.1.0, 24-OCT-2010 (EDW)
!
!-&

! $ Index_Entries
!
!     find tangent points on target
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      TOUCHD

      INTEGER               SIZED

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local variables
!
      CHARACTER*(256)       CONTXT

      DOUBLE PRECISION      BEGIN
      DOUBLE PRECISION      CURX
      DOUBLE PRECISION      MAXMAG
      DOUBLE PRECISION      PRVPNT ( 3 )
      DOUBLE PRECISION      SVDX
      DOUBLE PRECISION      T
      DOUBLE PRECISION      X1
      DOUBLE PRECISION      X2
      DOUBLE PRECISION      XSTEP
      DOUBLE PRECISION      TRNSTN
      DOUBLE PRECISION      XPOINT ( 3 )

      INTEGER               NLOOP
      INTEGER               ROOM
      INTEGER               TO

      LOGICAL               CURSTA
      LOGICAL               INSTAT
      LOGICAL               S
      LOGICAL               STATE1
      LOGICAL               STATE2
      LOGICAL               SAVSTA

      LOGICAL               PRVSET


!
!     The maximum number of search loop iterations to execute.
!     The default refinement method is bisection, a very slow
!     method to convergence. Since 2**1000 ~ 10**301,
!     1000 loop iterations represents enough effort to assume
!     either the search will not converge or that the refinement
!     function operates slower than would bisection, in which
!     case the user should use the default GFREFN function.
!
      INTEGER               MXLOOP
      PARAMETER           ( MXLOOP = 1000 )

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZTANSLV' )


!
!     Check the convergence tolerance.
!
      IF ( TOL .LE. 0.D0 ) THEN

         CALL SETMSG ( 'Tolerance must be positive but was #.' )
         CALL ERRDP  ( '#',  TOL                               )
         CALL SIGERR ( 'SPICE(INVALIDTOLERANCE)'               )
         CALL CHKOUT ( 'ZZTANSLV'                              )
         RETURN

      END IF

!
!     Make sure that START is not greater than FINISH. Signal an
!     error for START > FINISH.
!
      IF ( START .GT. FINISH ) THEN

         CALL SETMSG ( 'Bad input interval: ' &
         //            'START = # > FINISH = #.' )
         CALL ERRDP  ( '#', START                )
         CALL ERRDP  ( '#', FINISH               )
         CALL SIGERR ( 'SPICE(BOUNDSOUTOFORDER)' )
         CALL CHKOUT ( 'ZZTANSLV'                )
         RETURN

      END IF

!
!     Make sure that TOL is not too small, i.e. that neither
!     START + TOL nor START - TOL equals START.
!
      IF (  ( TOUCHD (START - TOL) .EQ. START ) &
           .OR. &
            ( TOUCHD (START + TOL) .EQ. START ) ) THEN

         CALL SETMSG ('TOL has value #1. '                      // &
                      'This value is too small to distinguish ' // &
                      'START - TOL or START + TOL from '        // &
                      'START, #2.'                               )
         CALL ERRDP  ( '#1', TOL                                 )
         CALL ERRDP  ( '#2', START                               )
         CALL SIGERR ( 'SPICE(INVALIDTOLERANCE)'                 )
         CALL CHKOUT ( 'ZZTANSLV'                                )
         RETURN

      END IF


!
!     Make sure that TOL is not too small, i.e. that neither
!     FINISH + TOL nor FINISH - TOL equals FINISH.
!
      IF (  ( TOUCHD (FINISH - TOL) .EQ. FINISH ) &
           .OR. &
            ( TOUCHD (FINISH + TOL) .EQ. FINISH ) ) THEN

         CALL SETMSG ('TOL has value #1. '                      // &
                      'This value is too small to distinguish ' // &
                      'FINISH - TOL or FINISH + TOL from '      // &
                      'FINISH, #2.'                              )
         CALL ERRDP  ( '#1', TOL                                 )
         CALL ERRDP  ( '#2', FINISH                              )
         CALL SIGERR ( 'SPICE(INVALIDTOLERANCE)'                 )
         CALL CHKOUT ('ZZTANSLV'                                 )
         RETURN

      END IF

!
!     Make sure that STEP is not too small: it must be greater
!     than TOL.
!
      IF ( CSTEP ) THEN

         IF ( STEP .LE. 0.D0 ) THEN

            CALL SETMSG ( 'STEP has value #1. The search step ' &
            //            'must be positive.'                 )
            CALL ERRDP  ( '#1', STEP                          )
            CALL SIGERR ( 'SPICE(INVALIDCONSTSTEP)'           )
            CALL CHKOUT ('ZZTANSLV'                           )
            RETURN

         END IF

         MAXMAG = MAX( ABS(START), ABS(FINISH) )


         IF ( TOUCHD(MAXMAG+STEP) .EQ. MAXMAG ) THEN

            CALL SETMSG ( 'STEP has value #1. This value ' &
            //            'is too small to guarantee that ' &
            //            'the search will advance.'        )
            CALL ERRDP  ( '#1', STEP                        )
            CALL SIGERR ( 'SPICE(INVALIDCONSTSTEP)'         )
            CALL CHKOUT ('ZZTANSLV'                         )
            RETURN

         END IF

      END IF

!
!     This algorithm determines those intervals when a given state is
!     observed to occur within a specified search interval.
!
!     Pairs of X values are recorded. The first member of each pair
!     denotes the X value at which the system changes to the state of
!     interest. The second denotes a transition out of that state.
!
!     If the state is .TRUE. at the beginning of the interval, the
!     beginning of the X interval will be recorded. This may or may not
!     be a transition point.
!
!     Similarly if the state is .TRUE. at the end of the interval, the
!     end of the interval will be recorded. Again, this may or may not
!     be a transition point.
!
!     Initially the current X value is the beginning of the search
!     interval.
!
      CURX   = START

      TO     = 1
      ROOM   = SIZED( RESULT )
      PRVSET = .FALSE.

!
!     Determine if the state at the current X value satisfies the
!     constraint.
!
      CALL UDCOND ( CURX, CURSTA, XPOINT )

      IF ( FAILED() ) THEN
         CALL CHKOUT (  'ZZTANSLV' )
         RETURN
      END IF

      IF ( CURSTA ) THEN
         CALL VEQU ( XPOINT, PRVPNT )
         PRVSET = .TRUE.
      END IF

!
!     If the system is in the state of interest, record the initial
!     X value of the search interval. The variable BEGIN will be
!     used to store the starting point of an interval over which
!     the state is .TRUE.
!
      IF ( CURSTA ) THEN

         INSTAT    = .TRUE.
         BEGIN     = CURX
         ENDFLG(1) = .FALSE.

!
!        BEGIN will be the first element of RESULT, presuming
!        a state transition is found later. We'll shift the
!        pointer for the output point so the Ith point will
!        correspond to the Ith element of RESULT.
!
!        We don't have to check ROOM yet because we're not
!        inserting anything into POINTS.
!
         TO   = TO   + 1
         ROOM = ROOM - 1

      ELSE

         INSTAT    = .FALSE.
         ENDFLG(1) = .TRUE.

      END IF

!
!     If the step size is constant, use the value supplied.
!
      IF ( CSTEP ) THEN
         XSTEP = STEP
      END IF

!
!     Save the current X value and state.
!
      SVDX   = CURX
      SAVSTA = CURSTA

!
!     Once initializations have been performed keep working
!     until the search interval has been exhausted.
!
!     While the last X value precedes the end of the interval:
!
      DO WHILE ( SVDX .LT. FINISH )
!
!        Attempt to bracket a state change.
!
!        Using the current window and internally stored information
!        about the current state, select a new current X.
!
         IF ( .NOT. CSTEP ) THEN

            CALL UDSTEP ( CURX, XSTEP )

            IF ( FAILED() ) THEN
               CALL CHKOUT (  'ZZTANSLV' )
               RETURN
            END IF

         END IF

!
!        Add the X step to the current X.  Make sure that the
!        X does not move beyond the end of the search interval.
!
         CURX = MIN ( TOUCHD(CURX + XSTEP), FINISH )

!
!        Compute the state at CURX.
!
         CALL UDCOND ( CURX, CURSTA, XPOINT )

         IF ( FAILED() ) THEN
            CALL CHKOUT (  'ZZTANSLV' )
            RETURN
         END IF

         IF ( CURSTA ) THEN
            CALL VEQU ( XPOINT, PRVPNT )
            PRVSET = .TRUE.
         END IF

!
!        While the state remains unchanged and the interval has not
!        been completely searched ...
!
         DO WHILE (       ( SAVSTA .EQV. CURSTA ) &
                    .AND. ( SVDX   .LT.  FINISH )  )
!
!           Save the current X and state.
!
            SVDX   = CURX
            SAVSTA = CURSTA

!
!           Compute a new current X so that we will not step
!           past the end of the interval.
!
            IF ( .NOT. CSTEP ) THEN

               CALL UDSTEP ( CURX, XSTEP )

               IF ( FAILED() ) THEN
                  CALL CHKOUT (  'ZZTANSLV' )
                  RETURN
               END IF

            END IF


            CURX = MIN ( TOUCHD(CURX + XSTEP), FINISH )
!
!           Compute the current state.
!
            CALL UDCOND ( CURX, CURSTA, XPOINT )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZTANSLV' )
               RETURN
            END IF

            IF ( CURSTA ) THEN
!
!              Save the associated vector for the X value CURX. In
!              normal usage, XPOINT is a surface intercept point.
!
               CALL VEQU ( XPOINT, PRVPNT )
               PRVSET = .TRUE.

            END IF
!
!           Loop back to see if the state has changed.
!
         END DO
!
!        At this point, SVDX and CURX are the X-values at the previous
!        and latest steps, respectively. SAVSTA and CURSTA are the
!        states at these X-values, respectively.
!
!        If we have detected a state change and not merely run out
!        of the search interval...
!
         IF ( SAVSTA .NEQV. CURSTA ) THEN
!
!           Call the previous state STATE1.
!           Call the current state STATE2.
!
!           Let X1 be the X value at state STATE1.
!           Let X2 be the X value at state STATE2.
!
!           Save the current X.
!
            STATE1 = SAVSTA
            STATE2 = CURSTA
            X1     = SVDX
            X2     = CURX

!
!           Make sure that X1 is not greater than X2. Signal an
!           error for X1 > X2.
!
            IF ( X1 .GT. X2 ) THEN

               CALL SETMSG ( 'Bad x interval result: X1 = # > X2 = #.' )
               CALL ERRDP  ( '#',  X1                                  )
               CALL ERRDP  ( '#',  X2                                  )
               CALL SIGERR ( 'SPICE(INVALIDSTEP)'                      )
               CALL CHKOUT ( 'ZZTANSLV'                                )
               RETURN

            END IF

!
!           Update the saved X and state values to those on the
!           right side of the bracketing interval. We'll use these
!           values for the next bracketing step after a root is
!           found.
!
            SVDX   = CURX
            SAVSTA = CURSTA

!
!           X1 and X2 bracket the X value of transition. Squeeze this
!           interval down until it is less than some tolerance in
!           length. Do it as described below...
!
!           Loop while the difference between the X values X1 and X2
!           exceeds a specified tolerance.
!
            NLOOP = 0

            DO WHILE ( TOUCHD (X2 - X1) .GT. TOL )

               NLOOP = NLOOP + 1
!
!              This loop count error exists to catch pathologies
!              in the refinement function. The default bisection
!              refinement will converge before 1000 iterations if
!              a convergence is numerically possible. Any other
!              refinement function should require fewer iterations
!              compared to bisection. If not, the user should
!              probably use bisection.
!
               IF ( NLOOP .GE. MXLOOP ) THEN

                  CALL SETMSG ( 'Loop run exceeds maximum loop count. ' &
               //               'Unable to converge to TOL value #1 ' &
               //               'within MXLOOP value #2 iterations.')
                  CALL ERRDP  ( '#1', TOL              )
                  CALL ERRINT ( '#2', MXLOOP           )
                  CALL SIGERR ( 'SPICE(NOCONVERGENCE)' )
                  CALL CHKOUT ( 'ZZTANSLV'             )
                  RETURN

               END IF

!
!              Select an X value T, between X1 and X2 (possibly based
!              on the state values).
!
               CALL UDREFN ( X1, X2, STATE1, STATE2, T )

!
!              Check for an error signal. The default refinement
!              routine, GFREFN, does not include error checks.
!
               IF ( FAILED() ) THEN
                  CALL CHKOUT (  'ZZTANSLV' )
                  RETURN
               END IF

!
!              Check whether T is between X1 and X2.  If
!              not then assume that we have gone as far as
!              we can in refining our estimate of the transition
!              point. Set X1 and X2 equal to T.
!

               T = BRCKTD ( T, X1, X2 )

               IF ( T .EQ. X1 ) THEN
!
!                 This assignment may break the invariant that
!                 the state at X2 is STATE2. This is allowed
!                 because we'll exit the loop immediately.
!
                  X2 = T

               ELSE IF ( T .EQ. X2 ) THEN
!
!                 This assignment may break the invariant that
!                 the state at X1 is STATE1. This is allowed
!                 because we'll exit the loop immediately.

                  X1 = T

               ELSE
!
!                 Compute the state at X value T. If this state, S,
!                 equals STATE1, set X1 to T, otherwise set X2 to T.
!
                  CALL UDCOND ( T, S, XPOINT )

                  IF ( S ) THEN
!
!                    Save the latest point associated with a
!                    .TRUE. state.
!
                     CALL VEQU ( XPOINT, PRVPNT )
                     PRVSET = .TRUE.

                  END IF

!
!                 Narrow the interval. Either increase X1 or decrease
!                 X2 by setting one of these endpoints to T. Maintain
!                 the invariant that the state is STATE1 at X1 and
!                 STATE2 at X2.
!
                  IF ( S .EQV. STATE1 ) THEN

                     X1 = T

                  ELSE

                     X2 = T

                  END IF

               END IF

            END DO

!
!           Let TRNSTN be the midpoint of [X1, X2]. Record this
!           abscissa value as marking the transition from STATE1 to
!           STATE2.
!
            TRNSTN = BRCKTD( (X1 + X2)*0.5D0, X1, X2 )

!
!           In state-of-interest or not? INSTAT indicates that STATE1
!           was .TRUE. We record intervals where the state is .TRUE.
!           when we detect the right hand endpoints of these intervals.
!
            IF ( INSTAT ) THEN
!
!              We were in the state of interest. TRNSTN marks the point
!              on the X-axis when the state changed to .FALSE. We need
!              to record the interval from BEGIN to FINISH and note
!              that the state has become .FALSE.
!
!              Add an interval starting at BEGIN and ending at TRNSTN
!              to the result window.
!
               CONTXT = 'Adding interval [BEGIN,TRNSTN] to RESULT. ' &
               //       'TRNSTN represents time of passage out of the ' &
               //       'state-of-interest.'

               CALL ZZWNINSD ( BEGIN, TRNSTN, CONTXT, RESULT )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'ZZTANSLV' )
                  RETURN
               END IF

            ELSE
!
!              The previous state was .FALSE. As a result TRNSTN marks
!              the point where the state becomes .TRUE. Note that we
!              have transitioned to the state of interest and record
!              the X-value at which the transition occurred.
!
               BEGIN = TRNSTN

            END IF

!
!           A transition occurred either from from in-state to
!           out-of-state or the inverse. Reverse the value of the
!           INSTAT flag to signify the transition event.
!
            INSTAT = .NOT. INSTAT

!
!           For all state transitions, record the last point found
!           by the state function.
!
            IF ( ROOM .GT. 0 ) THEN
!
!              Add the last point found during the transition search to
!              the POINTS array.
!
               IF ( PRVSET ) THEN

                  CALL VEQU ( PRVPNT, POINTS(1,TO) )

                  TO     = TO   + 1
                  ROOM   = ROOM - 1
                  PRVSET = .FALSE.

               ELSE

                  CALL SETMSG ( 'PRVPNT should always be set when a ' &
                  //            'transition is detected. We found a ' &
                  //            'transition at #, but PRVSET ' &
                  //            'indicates we don''t have a previous ' &
                  //            'point saved.'                        )
                  CALL ERRDP  ( '#', TRNSTN                           )
                  CALL SIGERR ( 'SPICE(BUG)'                          )
                  CALL CHKOUT ( 'ZZTANSLV'                            )
                  RETURN

               END IF


            ELSE
!
!              We ran out of room in the output point array. Note that
!              this error can occur before the result window insertion
!              fails, since that insertion takes place when the state
!              becomes .FALSE.
!
               CALL SETMSG ( 'Out of room in the POINTS array. Room ' &
               //            'is assumed to be adequate for SIZED(' &
               //            'RESULT) 3-vectors; this size is #.'    )
               CALL ERRINT ( '#', SIZED(RESULT)                      )
               CALL SIGERR ( 'SPICE(ARRAYTOOSMALL)'                  )
               CALL CHKOUT ( 'ZZTANSLV'                              )
               RETURN

            END IF
!
!           That's it for this detection of state change.
!
         END IF
!
!        Continue if the search interval extends to the right
!        of the latest step.
!
!        SVDX and SAVSTA are already set to the values at the
!        right side of the bracketing interval.
!
      END DO

!
!     Check if in-state at this abscissa value (FINISH). INSTAT is the
!     latest state value. If so record the interval.
!
      IF ( INSTAT ) THEN
!
!        The state is .TRUE. at FINISH.
!
!        Add an interval starting at BEGIN and ending at FINISH to the
!        window.
!
         CONTXT = 'Adding interval [BEGIN,FINISH] to RESULT. FINISH ' &
         //       'represents end of the search interval.'

         CALL ZZWNINSD ( BEGIN, FINISH, CONTXT, RESULT )

         ENDFLG(2) = .FALSE.
      ELSE
         ENDFLG(2) = .TRUE.
      END IF

      CALL CHKOUT (  'ZZTANSLV' )
      RETURN
      END

