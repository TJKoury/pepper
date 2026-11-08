!$Procedure ZZTANSLV ( Private --- tangent point solver )

      subroutine zztanslv ( udcond, udstep, udrefn, &
                            cstep,  step,   start,  finish, &
                            tol,    result, points, endflg  )
      use pepper_kinds, only: ip, wp

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

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      external              udcond
      external              udstep
      external              udrefn
      logical               cstep
      real(wp)              step
      real(wp)              start
      real(wp)              finish
      real(wp)              tol
      real(wp)              result ( lbcell : * )
      real(wp)              points ( 3, * )
      logical               endflg ( 2 )

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
      real(wp)              brcktd
      real(wp)              touchd

      integer(ip)           sized

      logical               failed
      logical               return

!
!     Local variables
!
      character(256)        contxt

      real(wp)              begin
      real(wp)              curx
      real(wp)              maxmag
      real(wp)              prvpnt ( 3 )
      real(wp)              svdx
      real(wp)              t
      real(wp)              x1
      real(wp)              x2
      real(wp)              xstep
      real(wp)              trnstn
      real(wp)              xpoint ( 3 )

      integer(ip)           nloop
      integer(ip)           room
      integer(ip)           to

      logical               cursta
      logical               instat
      logical               s
      logical               state1
      logical               state2
      logical               savsta

      logical               prvset


!
!     The maximum number of search loop iterations to execute.
!     The default refinement method is bisection, a very slow
!     method to convergence. Since 2**1000 ~ 10**301,
!     1000 loop iterations represents enough effort to assume
!     either the search will not converge or that the refinement
!     function operates slower than would bisection, in which
!     case the user should use the default GFREFN function.
!
      integer(ip)           mxloop
      parameter           ( mxloop = 1000 )

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin  ( 'ZZTANSLV' )


!
!     Check the convergence tolerance.
!
      if ( tol <= 0.0_wp ) then

         call setmsg ( 'Tolerance must be positive but was #.' )
         call errdp  ( '#',  tol                               )
         call sigerr ( 'SPICE(INVALIDTOLERANCE)'               )
         call chkout ( 'ZZTANSLV'                              )
         return

      end if

!
!     Make sure that START is not greater than FINISH. Signal an
!     error for START > FINISH.
!
      if ( start > finish ) then

         call setmsg ( 'Bad input interval: ' &
         //            'START = # > FINISH = #.' )
         call errdp  ( '#', start                )
         call errdp  ( '#', finish               )
         call sigerr ( 'SPICE(BOUNDSOUTOFORDER)' )
         call chkout ( 'ZZTANSLV'                )
         return

      end if

!
!     Make sure that TOL is not too small, i.e. that neither
!     START + TOL nor START - TOL equals START.
!
      if (  ( touchd (start - tol) == start ) &
           .or. &
            ( touchd (start + tol) == start ) ) then

         call setmsg ('TOL has value #1. '                      // &
                      'This value is too small to distinguish ' // &
                      'START - TOL or START + TOL from '        // &
                      'START, #2.'                               )
         call errdp  ( '#1', tol                                 )
         call errdp  ( '#2', start                               )
         call sigerr ( 'SPICE(INVALIDTOLERANCE)'                 )
         call chkout ( 'ZZTANSLV'                                )
         return

      end if


!
!     Make sure that TOL is not too small, i.e. that neither
!     FINISH + TOL nor FINISH - TOL equals FINISH.
!
      if (  ( touchd (finish - tol) == finish ) &
           .or. &
            ( touchd (finish + tol) == finish ) ) then

         call setmsg ('TOL has value #1. '                      // &
                      'This value is too small to distinguish ' // &
                      'FINISH - TOL or FINISH + TOL from '      // &
                      'FINISH, #2.'                              )
         call errdp  ( '#1', tol                                 )
         call errdp  ( '#2', finish                              )
         call sigerr ( 'SPICE(INVALIDTOLERANCE)'                 )
         call chkout ('ZZTANSLV'                                 )
         return

      end if

!
!     Make sure that STEP is not too small: it must be greater
!     than TOL.
!
      if ( cstep ) then

         if ( step <= 0.0_wp ) then

            call setmsg ( 'STEP has value #1. The search step ' &
            //            'must be positive.'                 )
            call errdp  ( '#1', step                          )
            call sigerr ( 'SPICE(INVALIDCONSTSTEP)'           )
            call chkout ('ZZTANSLV'                           )
            return

         end if

         maxmag = max( abs(start), abs(finish) )


         if ( touchd(maxmag+step) == maxmag ) then

            call setmsg ( 'STEP has value #1. This value ' &
            //            'is too small to guarantee that ' &
            //            'the search will advance.'        )
            call errdp  ( '#1', step                        )
            call sigerr ( 'SPICE(INVALIDCONSTSTEP)'         )
            call chkout ('ZZTANSLV'                         )
            return

         end if

      end if

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
      curx   = start

      to     = 1
      room   = sized( result )
      prvset = .false.

!
!     Determine if the state at the current X value satisfies the
!     constraint.
!
      call udcond ( curx, cursta, xpoint )

      if ( failed() ) then
         call chkout (  'ZZTANSLV' )
         return
      end if

      if ( cursta ) then
         call vequ ( xpoint, prvpnt )
         prvset = .true.
      end if

!
!     If the system is in the state of interest, record the initial
!     X value of the search interval. The variable BEGIN will be
!     used to store the starting point of an interval over which
!     the state is .TRUE.
!
      if ( cursta ) then

         instat    = .true.
         begin     = curx
         endflg(1) = .false.

!
!        BEGIN will be the first element of RESULT, presuming
!        a state transition is found later. We'll shift the
!        pointer for the output point so the Ith point will
!        correspond to the Ith element of RESULT.
!
!        We don't have to check ROOM yet because we're not
!        inserting anything into POINTS.
!
         to   = to   + 1
         room = room - 1

      else

         instat    = .false.
         endflg(1) = .true.

      end if

!
!     If the step size is constant, use the value supplied.
!
      if ( cstep ) then
         xstep = step
      end if

!
!     Save the current X value and state.
!
      svdx   = curx
      savsta = cursta

!
!     Once initializations have been performed keep working
!     until the search interval has been exhausted.
!
!     While the last X value precedes the end of the interval:
!
      do while ( svdx < finish )
!
!        Attempt to bracket a state change.
!
!        Using the current window and internally stored information
!        about the current state, select a new current X.
!
         if ( .not. cstep ) then

            call udstep ( curx, xstep )

            if ( failed() ) then
               call chkout (  'ZZTANSLV' )
               return
            end if

         end if

!
!        Add the X step to the current X.  Make sure that the
!        X does not move beyond the end of the search interval.
!
         curx = min ( touchd(curx + xstep), finish )

!
!        Compute the state at CURX.
!
         call udcond ( curx, cursta, xpoint )

         if ( failed() ) then
            call chkout (  'ZZTANSLV' )
            return
         end if

         if ( cursta ) then
            call vequ ( xpoint, prvpnt )
            prvset = .true.
         end if

!
!        While the state remains unchanged and the interval has not
!        been completely searched ...
!
         do while (       ( savsta .eqv. cursta ) &
                    .and. ( svdx   <  finish )  )
!
!           Save the current X and state.
!
            svdx   = curx
            savsta = cursta

!
!           Compute a new current X so that we will not step
!           past the end of the interval.
!
            if ( .not. cstep ) then

               call udstep ( curx, xstep )

               if ( failed() ) then
                  call chkout (  'ZZTANSLV' )
                  return
               end if

            end if


            curx = min ( touchd(curx + xstep), finish )
!
!           Compute the current state.
!
            call udcond ( curx, cursta, xpoint )

            if ( failed() ) then
               call chkout ( 'ZZTANSLV' )
               return
            end if

            if ( cursta ) then
!
!              Save the associated vector for the X value CURX. In
!              normal usage, XPOINT is a surface intercept point.
!
               call vequ ( xpoint, prvpnt )
               prvset = .true.

            end if
!
!           Loop back to see if the state has changed.
!
         end do
!
!        At this point, SVDX and CURX are the X-values at the previous
!        and latest steps, respectively. SAVSTA and CURSTA are the
!        states at these X-values, respectively.
!
!        If we have detected a state change and not merely run out
!        of the search interval...
!
         if ( savsta .neqv. cursta ) then
!
!           Call the previous state STATE1.
!           Call the current state STATE2.
!
!           Let X1 be the X value at state STATE1.
!           Let X2 be the X value at state STATE2.
!
!           Save the current X.
!
            state1 = savsta
            state2 = cursta
            x1     = svdx
            x2     = curx

!
!           Make sure that X1 is not greater than X2. Signal an
!           error for X1 > X2.
!
            if ( x1 > x2 ) then

               call setmsg ( 'Bad x interval result: X1 = # > X2 = #.' )
               call errdp  ( '#',  x1                                  )
               call errdp  ( '#',  x2                                  )
               call sigerr ( 'SPICE(INVALIDSTEP)'                      )
               call chkout ( 'ZZTANSLV'                                )
               return

            end if

!
!           Update the saved X and state values to those on the
!           right side of the bracketing interval. We'll use these
!           values for the next bracketing step after a root is
!           found.
!
            svdx   = curx
            savsta = cursta

!
!           X1 and X2 bracket the X value of transition. Squeeze this
!           interval down until it is less than some tolerance in
!           length. Do it as described below...
!
!           Loop while the difference between the X values X1 and X2
!           exceeds a specified tolerance.
!
            nloop = 0

            do while ( touchd (x2 - x1) > tol )

               nloop = nloop + 1
!
!              This loop count error exists to catch pathologies
!              in the refinement function. The default bisection
!              refinement will converge before 1000 iterations if
!              a convergence is numerically possible. Any other
!              refinement function should require fewer iterations
!              compared to bisection. If not, the user should
!              probably use bisection.
!
               if ( nloop >= mxloop ) then

                  call setmsg ( 'Loop run exceeds maximum loop count. ' &
               //               'Unable to converge to TOL value #1 ' &
               //               'within MXLOOP value #2 iterations.')
                  call errdp  ( '#1', tol              )
                  call errint ( '#2', mxloop           )
                  call sigerr ( 'SPICE(NOCONVERGENCE)' )
                  call chkout ( 'ZZTANSLV'             )
                  return

               end if

!
!              Select an X value T, between X1 and X2 (possibly based
!              on the state values).
!
               call udrefn ( x1, x2, state1, state2, t )

!
!              Check for an error signal. The default refinement
!              routine, GFREFN, does not include error checks.
!
               if ( failed() ) then
                  call chkout (  'ZZTANSLV' )
                  return
               end if

!
!              Check whether T is between X1 and X2.  If
!              not then assume that we have gone as far as
!              we can in refining our estimate of the transition
!              point. Set X1 and X2 equal to T.
!

               t = brcktd ( t, x1, x2 )

               if ( t == x1 ) then
!
!                 This assignment may break the invariant that
!                 the state at X2 is STATE2. This is allowed
!                 because we'll exit the loop immediately.
!
                  x2 = t

               else if ( t == x2 ) then
!
!                 This assignment may break the invariant that
!                 the state at X1 is STATE1. This is allowed
!                 because we'll exit the loop immediately.

                  x1 = t

               else
!
!                 Compute the state at X value T. If this state, S,
!                 equals STATE1, set X1 to T, otherwise set X2 to T.
!
                  call udcond ( t, s, xpoint )

                  if ( s ) then
!
!                    Save the latest point associated with a
!                    .TRUE. state.
!
                     call vequ ( xpoint, prvpnt )
                     prvset = .true.

                  end if

!
!                 Narrow the interval. Either increase X1 or decrease
!                 X2 by setting one of these endpoints to T. Maintain
!                 the invariant that the state is STATE1 at X1 and
!                 STATE2 at X2.
!
                  if ( s .eqv. state1 ) then

                     x1 = t

                  else

                     x2 = t

                  end if

               end if

            end do

!
!           Let TRNSTN be the midpoint of [X1, X2]. Record this
!           abscissa value as marking the transition from STATE1 to
!           STATE2.
!
            trnstn = brcktd( (x1 + x2)*0.5_wp, x1, x2 )

!
!           In state-of-interest or not? INSTAT indicates that STATE1
!           was .TRUE. We record intervals where the state is .TRUE.
!           when we detect the right hand endpoints of these intervals.
!
            if ( instat ) then
!
!              We were in the state of interest. TRNSTN marks the point
!              on the X-axis when the state changed to .FALSE. We need
!              to record the interval from BEGIN to FINISH and note
!              that the state has become .FALSE.
!
!              Add an interval starting at BEGIN and ending at TRNSTN
!              to the result window.
!
               contxt = 'Adding interval [BEGIN,TRNSTN] to RESULT. ' &
               //       'TRNSTN represents time of passage out of the ' &
               //       'state-of-interest.'

               call zzwninsd ( begin, trnstn, contxt, result )

               if ( failed() ) then
                  call chkout ( 'ZZTANSLV' )
                  return
               end if

            else
!
!              The previous state was .FALSE. As a result TRNSTN marks
!              the point where the state becomes .TRUE. Note that we
!              have transitioned to the state of interest and record
!              the X-value at which the transition occurred.
!
               begin = trnstn

            end if

!
!           A transition occurred either from from in-state to
!           out-of-state or the inverse. Reverse the value of the
!           INSTAT flag to signify the transition event.
!
            instat = .not. instat

!
!           For all state transitions, record the last point found
!           by the state function.
!
            if ( room > 0 ) then
!
!              Add the last point found during the transition search to
!              the POINTS array.
!
               if ( prvset ) then

                  call vequ ( prvpnt, points(1,to) )

                  to     = to   + 1
                  room   = room - 1
                  prvset = .false.

               else

                  call setmsg ( 'PRVPNT should always be set when a ' &
                  //            'transition is detected. We found a ' &
                  //            'transition at #, but PRVSET ' &
                  //            'indicates we don''t have a previous ' &
                  //            'point saved.'                        )
                  call errdp  ( '#', trnstn                           )
                  call sigerr ( 'SPICE(BUG)'                          )
                  call chkout ( 'ZZTANSLV'                            )
                  return

               end if


            else
!
!              We ran out of room in the output point array. Note that
!              this error can occur before the result window insertion
!              fails, since that insertion takes place when the state
!              becomes .FALSE.
!
               call setmsg ( 'Out of room in the POINTS array. Room ' &
               //            'is assumed to be adequate for SIZED(' &
               //            'RESULT) 3-vectors; this size is #.'    )
               call errint ( '#', sized(result)                      )
               call sigerr ( 'SPICE(ARRAYTOOSMALL)'                  )
               call chkout ( 'ZZTANSLV'                              )
               return

            end if
!
!           That's it for this detection of state change.
!
         end if
!
!        Continue if the search interval extends to the right
!        of the latest step.
!
!        SVDX and SAVSTA are already set to the values at the
!        right side of the bracketing interval.
!
      end do

!
!     Check if in-state at this abscissa value (FINISH). INSTAT is the
!     latest state value. If so record the interval.
!
      if ( instat ) then
!
!        The state is .TRUE. at FINISH.
!
!        Add an interval starting at BEGIN and ending at FINISH to the
!        window.
!
         contxt = 'Adding interval [BEGIN,FINISH] to RESULT. FINISH ' &
         //       'represents end of the search interval.'

         call zzwninsd ( begin, finish, contxt, result )

         endflg(2) = .false.
      else
         endflg(2) = .true.
      end if

      call chkout (  'ZZTANSLV' )
      return
      end

