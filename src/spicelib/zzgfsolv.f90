!$Procedure ZZGFSOLV ( Private --- GF, event finding routine )

      subroutine zzgfsolv ( udcond,   udstep,  udrefn,  bail, &
                            udbail,   cstep,   step,    start, &
                            finish,   tol,     rpt,     udrepu, &
                            result )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This routine is a root finding general purpose event location
!     routine. Most of the HARD work has been delegated to other
!     routines (In particular, how the dynamic step size is chosen).
!
!     Sister routine to ZZGFSOLVX. Copy any edits to ZZGFSOLV or
!     ZZGFSOLVX to the sister routine.
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

      integer(ip), parameter :: lbcell = -5

      external              udcond
      external              udstep
      external              udrefn
      logical               udbail
      logical               bail
      external              udbail
      logical               cstep
      real(wp)              step
      logical               rpt
      external              udrepu
      real(wp)              start
      real(wp)              finish
      real(wp)              tol
      real(wp)              result  ( lbcell : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     UDCOND     I   Name of the routine that compares the current state
!                    condition with-respect-to a constraint.
!     UDSTEP     I   Name of the routine that computes a time step
!     UDREFN     I   Name of the routine that computes a refined time.
!     BAIL       I   Logical indicating program interrupt monitoring.
!     UDBAIL     I   Name of a routine that signals a program interrupt.
!     CSTEP      I   Logical indicating constant step size.
!     STEP       I   Constant step size in seconds for finding geometric
!                    events.
!     START      I   Beginning of the search interval.
!     FINISH     I   End of the search interval.
!     TOL        I   Maximum error in detection of state transitions.
!     RPT        I   Progress reporter on ( .TRUE.) or off ( .FALSE. )
!     UDREPU     I   Function that updates the progress report.
!     RESULT    I-O   SPICE window containing results.
!
! $ Detailed_Input
!
!     The first three inputs to this routine are names of
!     subroutines that this routine will call.  These routines
!     should meet the following specifications.
!
!     UDCOND     the routine that determines if the system state
!                satisfies some constraint condition at epoch ET.
!
!                The calling sequence:
!
!                   CALL UDCOND ( ET, IN_CON )
!
!                where:
!
!                   ET       a double precision value representing
!                            ephemeris time, expressed as seconds past
!                            J2000 TDB, at which to evaluate the state.
!
!                   IN_CON   a logical value indicating whether
!                            or not the quantity satisfies the
!                            constraint at ET (TRUE) or not (FALSE).
!
!     UDSTEP     the routine that computes a time step in an attempt to
!                find a transition of the state of the specified
!                coordinate. In the context of this routine's algorithm,
!                a "state transition" occurs where the geometric state
!                changes from being in the desired geometric condition
!                event to not, or vice versa.
!
!                This routine relies on UDSTEP returning step sizes
!                small enough so that state transitions within the
!                confinement window are not overlooked.  There must
!                never be two roots A and B separated by less than
!                STEP, where STEP is the minimum step size returned by
!                UDSTEP for any value of ET in the interval [A, B].
!
!                The calling sequence for UDSTEP is:
!
!                   CALL UDSTEP ( ET, STEP )
!
!                where:
!
!                   ET      a double precision value representing
!                           ephemeris time, expressed as seconds past
!                           J2000 TDB, from which the algorithm is to
!                           search forward for a state transition.
!
!                   STEP    is the output step size. STEP indicates
!                           how far to advance ET so that ET and
!                           ET+STEP may bracket a state transition and
!                           definitely do not bracket more than one
!                           state transition. Units are TDB seconds.
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
!     UDREFN     the routine that computes a refinement in the times
!                that bracket a transition point. In other words, once
!                a pair of times have been detected such that the system
!                is in different states at each of the two times, UDREFN
!                selects an intermediate time which should be closer to
!                the transition state than one of the two known times.
!                The calling sequence for UDREFN is:
!
!                   CALL UDREFN ( T1, T2, S1, S2, T )
!
!                where the inputs are:
!
!                   T1    a time when the system is in state S1.
!
!                   T2    a time when the system is in state S2. T2
!                         is assumed to be larger than T1.
!
!                   S1    a logical indicating the state of the system
!                         at time T1.
!
!                   S2    a logical indicating the state of the system
!                         at time T2.
!
!                UDREFN may use or ignore the S1 and S2 values.
!
!                The output is:
!
!                   T     a time to check for a state transition
!                         between T1 and T2.
!
!                If a simple bisection method is desired, the routine
!                GFREFN may be used. This is the default option.
!
!     BAIL       is a logical indicating whether or not interrupt
!                signaling is enabled. When `bail' is set to TRUE,
!                the input function UDBAIL (see description below)
!                is used to determine whether an interrupt has been
!                issued.
!
!     UDBAIL     the routine that indicates whether an interrupt signal
!                has been issued (for example, from the keyboard).
!                UDBAIL has no arguments and returns a logical.
!                The return value is .TRUE. if an interrupt has
!                been issued; otherwise the value is .FALSE.
!
!                ZZGFSOLVX uses UDBAIL only when BAIL (see above) is set
!                to .TRUE., indicating that interrupt handling is
!                enabled. When interrupt handling is enabled, ZZGFSOLVX
!                and will call UDBAIL to determine whether to terminate
!                processing and return immediately.
!
!                If interrupt handing is not enabled, a logical
!                function must still be passed as an input argument.
!                The function
!
!                   GFBAIL
!
!                may be used for this purpose.
!
!     CSTEP      is a logical indicating whether or not the step size
!                used in searching is constant.  If it is, the value
!                STEP is used. Note that even if UDSTEP has the value
!                GFSTEP, i.e. the public, constant step routine, CSTEP
!                should still be .FALSE., in which case STEP is ignored.
!
!     STEP       is the step size to be used in the search. STEP must
!                be short enough for a search using this step size
!                to locate the time intervals where the geometric
!                event function is monotone increasing or decreasing.
!                However, STEP must not be *too* short, or the
!                search will take an unreasonable amount of time.
!
!                The choice of STEP affects the completeness but not
!                the precision of solutions found by this routine;
!                precision is controlled by the convergence
!                the tolerance, TOL.
!
!                STEP has units of TDB seconds.
!
!     START      is the beginning of the interval over which the state
!                is to be detected.
!
!     FINISH     is the end of the interval over which the state is
!                to be detected.
!
!     TOL        is a tolerance value used to determine convergence of
!                root-finding operations. TOL is measured in seconds
!                and is greater than zero.
!
!     RPT        is a logical variable which controls whether the
!                progress reporter is enabled. When RPT is TRUE,
!                progress reporting is enabled and the routine
!                UDREPU (see description  below) reports progress.
!
!     UDREPU     the routine that updates the progress report for a
!                search. The calling sequence of UDREPU is
!
!                   UDREPU (IVBEG, IVEND, ET )
!
!                   DOUBLE PRECISION      ET
!                   DOUBLE PRECISION      IVBEG
!                   DOUBLE PRECISION      IVEND
!
!                where ET is an epoch belonging to the confinement
!                window, IVBEG and IVEND are the start and stop times,
!                respectively of the current confinement window
!                interval.  The ratio of the measure of the portion
!                of CNFINE that precedes ET to the measure of CNFINE
!                would be a logical candidate for the searches
!                completion percentage; however the method of
!                measurement is up to the user.
!
!                If the user doesn't wish to provide a custom set of
!                progress reporting functions, the routine
!
!                   GFREPU
!
!                may be used.
!
!     RESULT     is an initialized SPICE window. RESULT may not be empty
!                on entry and must be large enough to hold all of the
!                intervals found by the search.
!
! $ Detailed_Output
!
!     RESULT     is a SPICE window containing the intersection of the
!                results of the search and the contents of RESULT
!                on entry.
!
!                When RESULT is empty on input, the intervals of the
!                output window stored in RESULT represent times when
!                the state function UDCOND returns the value .TRUE.
!
! $ Parameters
!
!     LBCELL     is the SPICELIB cell lower bound.
!
! $ Exceptions
!
!     1)  If TOL is negative, the error SPICE(VALUEOUTOFRANGE)
!         will signal.
!
!     2)  If START +/- TOL is indistinguishable from START or
!         FINISH +/- TOL is indistinguishable from FINISH, the
!         error SPICE(INVALIDVALUE) will signal.
!
!     3)  If START is greater than FINISH or SVDTIM is greater than
!         CURTIM, SPICE(BADTIMECASE) will signal.
!
!     4) If the inner convergence loop fails to converge to TOL
!        within MXLOOP iterations, the error SPICE(NOCONVERG)
!        will signal.
!
! $ Files
!
!     This routine computes states using SPK files that have been
!     loaded into the SPICE system, normally via the kernel loading
!     interface routine FURNSH. See the routine FURNSH and the SPK
!     and KERNEL Required Reading for further information on loading
!     (and unloading) kernels.
!
! $ Particulars
!
!     This routine implements a strategy for searching for geometric
!     state events important for planning solar system observations.
!     The actual details of selecting time steps while searching for
!     a state change as well as the scheme used for zeroing in on the
!     actual time of transition are handled by lower level routines.
!
!     By delegating the work of selecting search time steps and the
!     process of refining a transition time estimate to lower level
!     routines, the common work of the search can be isolated here.
!     The routines that do the decision making, can be modified
!     and made smarter as time permits.
!
! $ Examples
!
!      See GFOCCE and ZZGFREL.
!
! $ Restrictions
!
!      It is important that the user understand how the routines
!      UDCOND, UDSTEP and UDREFN are to be used and that the
!      calling sequences match precisely with the descriptions given
!      here.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     L. S. Elson    (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.0,  31-JAN-2017 (NJB) (EDW)
!
!        Restrictions on the input tolerance have been loosened:
!        it is no longer required that the tolerance must yield
!        a new value when it is added to, or subtracted from,
!        either of the input interval bounds. The tolerance
!        still must be strictly positive.
!
!-    SPICELIB Version 1.1.0,  24-OCT-2010 (EDW)
!
!       TOL error check now returns SPICE(INVALIDTOLERANCE) instead of
!       previous return SPICE(VALUEOUTOFRANGE).
!
!-    SPICELIB Version 1.0.1  21-DEC-2009 (EDW)
!
!        Edit to Abstract to document sister routine ZZGFSOLVX. Added
!        N.J. Bachman citation to Author_and_Institution section.
!
!-    SPICELIB Version 1.0.0, 17-MAR-2009 (EDW)(LSE)(NJB)
!
!-&

! $ Index_Entries
!
!     find times of an event
!
!-&

!
!     SPICELIB functions.
!

      real(wp)              brcktd
      real(wp)              dpmax
      real(wp)              touchd

      logical               failed
      logical               return

!
!     Local variables
!
      real(wp)              begin
      real(wp)              curtim
      real(wp)              diff
      real(wp)              prvdif
      real(wp)              svdtim
      real(wp)              t
      real(wp)              t1
      real(wp)              t2
      real(wp)              timest
      real(wp)              trnstn

      logical               curste
      logical               instat
      logical               s
      logical               state1
      logical               savst
      logical               l1
      logical               l2

      character(256)       contxt

      integer(ip)          nloop

!
!     The maximum number of search loop iterations to execute.
!     The default refinement method is bisection, a very slow
!     method to convergence. Since 2**1000 ~ 10**301,
!     1000 loop iterations represents enough effort to assume
!     either the search will not converge or that the refinement
!     function operates slower than would bisection, in which
!     case the user should use the default GFREFN function.
!
      integer(ip), parameter :: mxloop = 1000

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Check the convergence tolerance.
!
      if ( tol <= 0.0_wp ) then

         call setmsg ( 'Tolerance must be positive but was #.' )
         call errdp  ( '#',  tol                               )
         call sigerr ( 'SPICE(INVALIDTOLERANCE)'               )
         return

      end if

!
!     Make sure that START is not greater than FINISH. Signal an
!     error for START > FINISH.
!
      if ( start > finish) then

         call setmsg ( 'Bad time interval result, '      // &
                       'START > FINISH.' )
         call sigerr ( 'SPICE(BADTIMECASE)' )
         return

      end if

!
!     If active, update the progress reporter.
!
      if ( rpt ) then
         call udrepu ( start, finish, start )
      end if

!
!     This algorithm determines those intervals when a given state
!     is observed to occur within a specified search interval.
!
!     Pairs of times are recorded.  The first member of each pair
!     denotes the time when the system changes to the state of
!     interest.  The second denotes a transition out of that state.
!
!     If the system is in the state of interest at the beginning of
!     the interval, the beginning of the time interval will be
!     recorded.  This may or may not be a transition point.
!
!     Similarly if the system is in the state of interest at the end
!     of the interval, the end of the interval will be recorded.
!     Again, this may or may not be a transition point.
!

!
!     Initially the current time is the beginning of the search
!     interval.
!
      curtim = start

!
!     Determine if the state at the current time satisfies some
!     constraint. This constraint may indicate only existence of
!     a state.
!
      call udcond ( curtim, curste )

      if ( failed() ) return

!
!     If the system is in the state of interest, record the initial
!     time of the search interval.
!
      if ( curste ) then

         instat = .true.
         begin  = curtim

      else

         instat = .false.

      end if

!
!     If the step size is constant, use the value supplied.
!
      if ( cstep ) then
         timest = step
      end if

!
!     Save the current time and state somewhere.
!
      svdtim  = curtim
      savst   = curste

!
!     Once initializations have been performed keep working
!     until the search interval has been exhausted.
!
!     While time remains in the search interval.
!
      do while ( svdtim < finish )

!
!        Using the current window and internally stored
!        information about the current state, select a new current
!        time.
!
         if ( .not. cstep ) then

            call udstep ( curtim, timest )

            if ( failed() ) return

         end if

!
!        Add the time step to the current time.  Make sure that the
!        time does not move beyond the end of the search interval.
!

         curtim = min ( curtim + timest, finish )

!
!        Compute the state at time CURTIM.
!
         call udcond ( curtim,  curste )

         if ( failed() ) return

!
!        While the state remains unchanged and the interval is not
!        completely searched ...
!

         do while ( ( savst .eqv. curste ) &
                    .and. &
                    ( svdtim < finish ) )

!
!           First check for an interrupt signal if checking is enabled.
!
            if ( bail )  then
               if ( udbail () ) return
            end if

!
!           Report the current time to the monitoring utility, if
!           appropriate.
!
            if ( rpt ) then
               call udrepu ( start, finish, svdtim )
            end if

!
!           Save the current time and state somewhere.
!
            svdtim  = curtim
            savst   = curste

!
!           Compute a new current time so that we will not step
!           past the end of the interval.  This time will be
!           based on:
!
!                 1. The kind of event we are looking for.
!                 2. The objects and observer class.
!                 3. Transition times already found.
!                 4. A minimum time step allowed.
!
            if ( .not. cstep ) then

               call udstep ( curtim, timest )

               if ( failed() ) return

            end if

            curtim = min ( curtim + timest, finish )

!
!           Compute the current state
!
            call udcond ( curtim,  curste )

            if ( failed() ) return

!
!           Loop back to see if the state has changed.
!

         end do

!
!        If we have detected a state change and not merely run out
!        of the search interval...
!

         if ( savst .neqv. curste ) then

!
!           Call the previous state STATE1
!           Call the current  state STATE2
!
!           Call the time at state STATE1, T1
!           Call the time at state STATE2, T2
!
!           Save the current time.
!
            state1 = savst
            t1     = svdtim
            t2     = curtim

!
!           Set the states at T1 and T2 for use by the refinement
!           function, in case the caller has passed in a function
!           that uses them.
!
            l1     = savst
            l2     = curste

!
!           Make sure that T1 is not greater than T2. Signal an
!           error for T1 > T2.
!

            if ( t1 > t2) then
               call setmsg ( 'Bad time interval result, T1 > T2.' )
               call sigerr ( 'SPICE(BADTIMECASE)' )
               return
            end if

            svdtim  =  curtim
            savst   =  curste

!
!           T1 and T2 bracket the time of transition.  Squeeze this
!           interval down until it is less than some tolerance in
!           length.  Do it as described below...
!
!           Loop while the difference between the times T1 and T2
!           exceeds a specified tolerance, and while the magnitude
!           of the difference is decreasing from one loop iteration
!           to the next.
!
            prvdif = dpmax()
            diff   = touchd( abs(t2 - t1) )
            nloop  = 0

            do while (       ( diff > tol    ) &
                       .and. ( diff < prvdif )  )

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
                  call errdp  ( '#1', tol          )
                  call errint ( '#2', mxloop       )
                  call sigerr ( 'SPICE(NOCONVERG)' )
                  return
               end if

               if ( bail )  then
                  if ( udbail () ) return
               end if

!
!              Select a time T, between T1 and T2 (possibly based on the
!              values of L1 and L2).
!
               call udrefn ( t1, t2, l1, l2, t )

!
!              Check for an error signal. The default refinement
!              routine, GFREFN, does not include error checks.
!
               if ( failed() ) return

!
!              Check whether T is between T1 and T2.  If
!              not then assume that we have gone as far as
!              we can in refining our estimate of the transition
!              point. Set T1 and T2 equal to T.
!

               t = brcktd ( t, t1, t2 )

               if ( t == t1 ) then

                  t2 = t

               else if ( t == t2 ) then

                  t1 = t

               else

!
!                 Compute the state time T. If this state, S,
!                 equals STATE1, set T1 to T, otherwise set
!                 T2 to T.
!
                  call udcond ( t,  s )

                  if ( s .eqv. state1 ) then

                     t1  = t
                     l1  = s

                  else

                     t2 = t
                     l2 = s

                  end if

               end if

!
!              Update PRVDIF and DIFF for the next loop termination
!              test.
!
               prvdif = diff
               diff   = touchd( abs(t2 - t1) )

            end do

!
!           Let TRNSTN be the midpoint of [T1, T2].  Record this
!           time as marking the transition from STATE1 to STATE2.
!
            trnstn = brcktd( (t1 + t2)*0.5_wp, t1, t2 )

!
!           In state-of-interest or not?
!
            if ( instat )  then
!
!              We were in the state of interest, TRNSTN marks the
!              point in time when the state changed to "not of
!              interest" We need to record the interval from BEGIN to
!              FINISH and note that we are no longer in the state of
!              interest.
!

!
!              Add an interval starting at BEGIN and ending at TRNSTN
!              to the result window.
!

               contxt = 'Adding interval [BEGIN,TRNSTN] to RESULT. ' &
               //       'TRNSTN represents time of passage out of the ' &
               //       'state-of-interest.'

               call zzwninsd ( begin, trnstn, contxt, result )

            else
!
!              We were not in the state of interest.  As a result
!              TRNSTN marks the point where we are changing to
!              the state of interest.  Note that we have transitioned
!              to the state of interest and record the time at
!              which the transition occurred.
!
               begin  = trnstn

            end if

!
!           A transition occurred either from from in-state to
!           out-of-state or the inverse. Reverse the value of the
!           INSTAT flag to signify the transition event.
!
            instat = .not. instat

!
!        That's it for this detection of state change.
!
         end if

!
!        Continue if there is more time in the search interval.
!
      end do

!
!     Check if in-state at this time (FINISH). If so record the
!     interval.
!

      if ( instat ) then

!
!        Add an interval starting at BEGIN and ending at FINISH to the
!        window.
!
         contxt = 'Adding interval [BEGIN,FINISH] to RESULT. FINISH ' &
               // 'represents end of the search interval.'

         call zzwninsd ( begin, finish, contxt, result )

      end if

!
!     If active, update the progress reporter before exiting this
!     routine.
!

      if ( rpt ) then
         call udrepu ( start, finish, finish )
      end if

!
!     Check-out then return.
!
      return
      end
