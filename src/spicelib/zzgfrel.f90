!$Procedure ZZGFREL ( Private --- GF, geometric relation finder )

      subroutine zzgfrel ( udstep,   udrefn,  udqdec,  udcond, &
                           udfunc,   udqref,  relate,  refval, &
                           tol,      adjust,  cnfine,  mw, &
                           nw,       work,    rpt,     udrepi, &
                           udrepu,   udrepf,  rptpre,  rptsuf, &
                           bail,     udbail,  result           )

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     This routine determines time intervals when the value of some
!     geometric quantity related to one or more objects and an observer
!     satisfies a user specified constraint within time intervals
!     specified by the window CNFINE.
!
!     Sister routine to ZZGFRELX. Copy any edits to ZZGFREL or ZZGFRELX
!     to the sister routine.
!
! $ Required_Reading
!
!     SPK
!     TIME
!     NAIF_IDS
!     FRAMES
!
! $ Keywords
!
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations

      implicit none

      include               'gf.inc'

      integer               lbcell
      parameter           ( lbcell = -5 )

      integer               nwreq
      parameter           ( nwreq  = 5 )

      external              udstep
      external              udrefn
      external              udqdec
      external              udcond
      external              udfunc
      external              udqref
      character*(*)         relate
      double precision      refval
      double precision      tol
      double precision      adjust
      double precision      cnfine ( lbcell : * )
      integer               mw
      integer               nw
      double precision      work ( lbcell : mw, nw )
      logical               rpt
      external              udrepi
      external              udrepu
      external              udrepf
      character*(*)         rptpre ( * )
      character*(*)         rptsuf ( * )
      logical               udbail
      logical               bail
      external              udbail
      double precision      result ( lbcell : * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LBCELL     P   SPICELIB cell lower bound.
!     NWREQ      P   Minimum number of workspace windows.
!     UDSTEP     I   Name of the routine that computes and returns a
!                    time step.
!     UDREFN     I   Name of the routine that computes a refined time.
!     UDQDEC     I   Name of the routine that computes whether the
!                    geometric quantity is decreasing.
!     UDCOND     I   Name of the routine that computes the geometric
!                    condition with-respect-to the constraint.
!     UDFUNC     I   The routine that computes the geometric quantity of
!                    interest.
!     UDQREF     I   Name of the routine that resets the current value
!                    of REFVAL.
!     RELATE     I   Operator that either looks for an extreme value
!                    (max, min, local, absolute) or compares the
!                    geometric quantity value and a number.
!     REFVAL     I   Value used as reference for geometric quantity
!                    condition.
!     TOL        I   Convergence tolerance in seconds.
!     ADJUST     I   Allowed variation for absolute extremal
!                    geometric conditions.
!     CNFINE     I   Confinement schedule
!     MW         I   Size of workspace windows.
!     NW         I   Number of workspace windows.
!     WORK       I   Array containing workspace windows
!     RPT        I   Progress reporter on ( .TRUE.) or off ( .FALSE. )
!     UDREPI     I   Function that initializes progress reporting.
!     UDREPU     I   Function that updates the progress report.
!     UDREPF     I   Function that finalizes progress reporting.
!     RPTPRE     I   Progress reporter beginning message.
!     RPTSUF     I   Progress reporter ending message.
!     BAIL       I   Logical indicating program interrupt monitoring.
!     UDBAIL     I   Name of a routine that signals a program interrupt.
!     RESULT    I-O  SPICE window containing results.
!
!
! $ Detailed_Input
!
!     UDSTEP     the routine that computes a time step in an attempt to
!                find a transition of the state of the specified
!                coordinate. In the context of this routine's algorithm,
!                a "state transition" occurs where the coordinate value
!                changes from "decreasing" to "not decreasing" or vice
!                versa.
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
!                   ET      is the input start time from which the
!                           algorithm is to search forward for a state
!                           transition. ET is expressed as seconds past
!                           J2000 TDB. ET is a DOUBLE PRECISION number.
!
!                   STEP    is the output step size.  STEP indicates
!                           how far to advance ET so that ET and
!                           ET+STEP may bracket a state transition and
!                           definitely do not bracket more than one
!                           state transition.  STEP is a DOUBLE
!                           PRECISION number.  Units are TDB seconds.
!
!                If a constant step size is desired, the routine
!                GFSTEP may be used. This is the default option.
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
!     UDQDEC     the routine that determines if the geometric quantity
!                is decreasing.
!
!                The calling sequence:
!
!                   CALL UDQDEC ( ET, ISDECR )
!
!                where:
!
!                   ET       a double precision value representing
!                            ephemeris time, expressed as seconds past
!                            J2000 TDB, at which to determine the time
!                            derivative of the geometric quantity.
!
!                   ISDECR   a logical return indicating whether
!                            or not the geometric quantity
!                            is decreasing. ISDECR returns true if the
!                            time derivative of the geometric quantity
!                            at ET is negative.
!
!     UDCOND     the routine that determines if the geometric quantity
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
!                            J2000 TDB, at which to evaluate the
!                            geometric quantity.
!
!                   IN_CON   a logical value indicating whether or
!                            not the geometric quantity satisfies the
!                            constraint at ET (TRUE) or not (FALSE).
!
!     UDFUNC     the routine that returns the value of the geometric
!                quantity at the time of interest. The calling sequence
!                for UDFUNC is:
!
!                   CALL UDFUNC ( TIME, VALUE )
!
!                where:
!
!                   TIME    a double precision value representing
!                           ephemeris time, expressed as seconds past
!                           J2000 TDB, at which  to determine
!                           the value of the geometric quantity.
!
!                   VALUE   is the value of the geometric quantity at
!                           time TIME.
!
!     UDQREF     the routine that resets the current value of REFVAL.
!                The calling sequence for UDQREF is:
!
!                   CALL UDQREF ( REFER2 )
!
!                where REFER2 is a new value of REFVAL.
!
!     RELATE     is a comparison operator, indicating the numeric
!                constraint of interest. Values are:
!
!                '>'   value of geometric quantity greater than some
!                      reference (REFVAL).
!
!                '='   value of geometric quantity equal to some
!                      reference (REFVAL).
!
!                '<'   value of geometric quantity less than some
!                      reference (REFVAL).
!
!                ABSMAX-the geometric quantity is at an absolute
!                maximum.
!
!                ABSMIN-the geometric quantity is at an absolute
!                minimum.
!
!                LOCMAX-the geometric quantity is at an local maximum.
!
!                LOCMIN-the geometric quantity is at an local minimum.
!
!     REFVAL     Reference value for geometric quantity (in
!                radians, radians/sec, km, or km/sec as appropriate).
!
!     TOL        is a tolerance value used to determine convergence of
!                root-finding operations.  TOL is measured in seconds
!                and is greater than zero.
!
!     ADJUST     The amount by which the numerical quantity is
!                allowed to vary from an absolute extremum. If ADJUST
!                is non-zero, the resulting schedule contains
!                intervals when the geometric quantity has
!                values either between ABSMIN and ABSMIN + ADJUST
!                or between ABSMAX and ABSMAX - ADJUST. ADJUST must
!                not be negative.
!
!     CNFINE     is a SPICE window that confines the bounds of the
!                search. Note that like all windows (see windows.req)
!                CNFINE can contain multiple time intervals. See the
!                Examples section for information on how to create this
!                window.
!
!     MW         is the cell size of the windows in the workspace array
!                WORK.
!
!     NW         is the number of windows in the workspace array WORK.
!                NW must be at least as large as the parameter NWREQ.
!
!     WORK       is an array used to store workspace windows. This
!                array has dimensions WORK (-5 : MW, NW).
!
!     RPT        is a logical variable which controls whether the
!                progress reporter is on or off. The progress reporter
!                writes to the user's terminal.
!
!     UDREPI     the routine that initializes a progress report.
!                When progress reporting is enabled, UDREPI
!                is called at the start of a search.  The calling
!                sequence of UDREPI is:
!
!                   UDREPI ( CNFINE, RPTPRE, RPTSUF )
!
!                   DOUBLE PRECISION    CNFINE ( LBCELL : * )
!                   CHARACTER*(*)       RPTPRE
!                   CHARACTER*(*)       RPTSUF
!
!                where
!
!                   CNFINE
!
!                is the confinement window passed into ZZGFRELX, and
!
!                   RPTPRE
!                   RPTSUF
!
!                are prefix and suffix strings used in the progress
!                report:  these strings are intended to bracket a
!                representation of the fraction of work done.
!
!                If the user has no progress reporting initialization
!                routine, the SPICELIB routine GFRPIN may be used. This
!                is the default option.
!
!     UDREPU     the routine that updates the progress report for a
!                search.  The calling sequence of UDREPU is:
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
!                would be a logical candidate for the search's
!                completion percentage; however the method of
!                measurement is up to the user.
!
!                If the user has no progress reporting update routine,
!                the SPICELIB routine GFRPUD may be used. This is the
!                default option.
!
!     UDREPF     the routine that finalizes a progress report. UDREPF
!                has no arguments.
!
!                If the user has no progress reporting finalizing
!                routine, the SPICELIB routine GFRPEN may be used. This
!                is the default option.
!
!     RPTPRE     is an array of strings containing the prefixes of
!                the output messages reported by the progress reporter.
!                The Ith element of RPTPRE is the prefix for the
!                message corresponding to the Ith traversal of the
!                confinement window executed by this routine; such
!                traversals are called "passes." The number of passes
!                executed depends on the relational operator RELATE.
!                Searches for local extrema and unadjusted absolute
!                extrema require one pass; searches for adjusted
!                absolute extrema, equalities, and inequalities require
!                two passes.
!
!                An example of the contents of RPTPRE for a distance
!                equality search:
!
!                   RPTPRE(1) = 'Distance pass 1 of 2'
!                   RPTPRE(2) = 'Distance pass 2 of 2'
!
!     RPTSUF     is an array of strings containing the suffixes of
!                the output messages reported by the progress reporter.
!                The Ith element of RPTSUF is the suffix for the
!                message corresponding to the Ith pass.
!
!                An example of the contents of RPTSUF for a distance
!                equality search:
!
!                   RPTSUF(1) = 'done.'
!                   RPTSUF(2) = 'done.'
!
!                For this search, the complete progress report message
!                for the Ith pass has the form
!
!                   'Distance pass I of 2 xxx.xx% done.'
!
!     BAIL       is a logical indicating whether or not interrupt
!                signaling is enabled.
!
!     UDBAIL     the routine that checks to see whether an interrupt
!                signal has been issued from, e.g. the keyboard. If
!                this capability is not to be used, a dummy function,
!                ZZGFBAIL must be supplied.
!
!     RESULT     is an initialized SPICE window. RESULT is large
!                enough to hold all of the intervals, within the
!                confinement window, on which the specified condition
!                is met.
!
! $ Detailed_Output
!
!     RESULT     is a SPICE window containing the time intervals within
!                the confinement window, over which the specified
!                condition is met.
!
!                RESULT is emptied before new values are assigned to
!                it.
!
! $ Parameters
!
!     LBCELL     is the SPICELIB cell lower bound.
!
!     NWREQ      is the required number of workspace windows; the
!                input argument NW must not be less than NWREQ.
!
! $ Exceptions
!
!     1)  A negative value for ADJUST causes the routine to signal
!         the error SPICE(VALUEOUTOFRANGE). A non-zero value for ADJUST
!         when RELATE has any value other than "ABSMIN" or "ABSMAX",
!         causes the routine to signal the error SPICE(INVALIDVALUE).
!
!     2)  If an improper comparison operator is specified, the error
!         SPICE(NOTRECOGNIZED) is signaled.
!
!     3)  If TOL is not greater than zero, the error
!         SPICE(VALUEOUTOFRANGE) will be signaled by routines called
!         from this routine.
!
!     4)  If the number of workspace windows is less than NWREQ, the
!         error SPICE(TOOFEWWINDOWS) is signaled.
!
!     5)  If the window size MW is less than 2, the error
!         SPICE(INVALIDDIMENSION) will be signaled.
!
!     6)  If the output SPICE window RESULT has insufficient capacity
!         to contain the number of intervals on which the specified
!         visibility condition is met, the error will be diagnosed
!         by a routine in the call tree of this routine. If the result
!         window has size less than 2, the error SPICE(WINDOWTOOSMALL)
!         will be signaled by this routine.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine determines time intervals when the value of some
!     geometric quantity related to one or more objects and an observer
!     satisfies a user specified constraint. It puts these times in a
!     result window called RESULT. It does this by first finding
!     schedules (windows) when the quantity of interest is either
!     monotonically increasing or decreasing. These schedules are then
!     manipulated to give the final result. Note that the determination
!     of "=" involves finding intervals where the quantity is "less
!     than" to a tolerance of TOL. This means that the end points of
!     these intervals are within TOL of being equal to the value.
!
! $ Examples
!
!     See GFEVNT.
!
! $ Restrictions
!
!     The kernel files to be used by ZZGFREL must be loaded (normally
!     via the SPICELIB routine FURNSH) before ZZGFREL is called.
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
!-    SPICELIB Version 1.0.1  21-DEC-2009 (EDW)
!
!        Edit to Abstract to document sister routine ZZGFRELX.
!
!-    SPICELIB Version 1.0.0  21-FEB-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&

! $ Index_Entries
!
!     determine when a scalar quantity satisfies a condition
!
!-&

!
!     SPICELIB functions
!
      integer               cardd
      integer               isrchc
      integer               sized
      integer               wncard

      logical               failed
      logical               return

!
!     Local parameters
!
      integer               left
      parameter           ( left   = 1 )

      integer               right
      parameter           ( right  = 2 )

      integer               maxcln
      parameter           ( maxcln = 80 )

!
!     Workspace window indices:
!
      integer               confin
      parameter           ( confin = 3 )

      integer               copy
      parameter           ( copy   = 4 )

      integer               decres
      parameter           ( decres = 2 )

      integer               incres
      parameter           ( incres = 1 )

      integer               tempw
      parameter           ( tempw  = 5 )


!
!     Number of supported comparison operators.
!
      integer               nc
      parameter           ( nc     = 7 )

!     One-letter alias for LBCELL to make references to the workspace
!     array tolerable:
!
      integer               b
      parameter           ( b      = lbcell )

!
!     Context string length:
!
      integer               ctxlen
      parameter           ( ctxlen = 500 )

!
!     Local variables
!

      character*(maxcln)    cnames ( nc )
      character*(ctxlen)    contxt
      character*(maxcln)    locrel

      double precision      addl
      double precision      addr
      double precision      endpt  (  2 )
      double precision      extrem
      double precision      finish
      double precision      refer2
      double precision      start
      double precision      step
      double precision      value

      integer               case
      integer               count
      integer               i
      integer               maxat
      integer               minat
      integer               name    (  2 )
      integer               pass
      integer               qcnum
      integer               want
      integer               winsiz

      logical               cstep
      logical               need


!
!     Saved variables
!
      save                  cnames

!
!     Below we initialize the list of comparison operator names.
!
      data                  cnames / '<', &
                                     '=', &
                                     '>', &
                                     'LOCMIN', &
                                     'ABSMIN', &
                                     'LOCMAX', &
                                     'ABSMAX'  /


!
!     Set constant step parameter to .FALSE..
!
      data                  cstep / .false. /

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin  ( 'ZZGFREL' )

!
!     Make sure we have enough workspace windows.
!
      if ( nw .lt. nwreq ) then

         call setmsg ( 'The number of workspace windows (#) is less ' &
         //            'than the minimum #.'                         )
         call errint ( '#',  nw                                      )
         call errint ( '#',  nwreq                                   )
         call sigerr ( 'SPICE(TOOFEWWINDOWS)'                        )
         call chkout ( 'ZZGFREL'                                     )
         return

      end if

!
!     Make sure the workspace windows can contain at least one interval.
!
      if ( mw .lt. 2 ) then

         call setmsg ( 'Workspace window size was #; size must be ' &
         //            'at least 2.'                               )
         call errint ( '#',  mw                                    )
         call sigerr ( 'SPICE(INVALIDDIMENSION)'                   )
         call chkout ( 'ZZGFREL'                                   )
         return

      end if

!
!     Check the result window size.
!
      if ( sized(result) .lt. 2 ) then

         call setmsg ( 'Result window size was #; size must be ' &
         //            'at least 2.'                             )
         call errint ( '#',  sized(result)                       )
         call sigerr ( 'SPICE(INVALIDDIMENSION)'                 )
         call chkout ( 'ZZGFREL'                                 )
         return

      end if

!
!     Make sure the requested comparison is one we recognize.
!
      call ljust ( relate, locrel )
      call ucase ( locrel, locrel )

      qcnum = isrchc ( locrel, nc, cnames )

      if ( qcnum .eq. 0 ) then

        call setmsg ('The comparison operator, # is not '            // &
                     'recognized.  Supported quantities are: '       // &
                     '<, '                                           // &
                     '=, '                                           // &
                     '>, '                                           // &
                     'LOCMIN, '                                      // &
                     'ABSMIN, '                                      // &
                     'LOCMAX, '                                      // &
                     'ABSMAX.'                                        )
         call errch  ('#', relate                                     )
         call sigerr ('SPICE(NOTRECOGNIZED)'                          )
         call chkout ('ZZGFREL'                                       )
         return

      end if

!
!     Confirm ADJUST is non-negative.
!
      if ( adjust .lt. 0.d0 ) then

         call setmsg ( 'ADJUST was #; must be non-negative.' )
         call errdp  ( '#',  adjust                          )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'              )
         call chkout ( 'ZZGFREL'                             )
         return

      end if

!
!    Confirm ADJUST equals zero unless LOCREL (RELATE) has value
!    "ABSMAX" or "ABSMIN."
!
      if(  (locrel .ne. 'ABSMIN')  .and. (locrel .ne. 'ABSMAX' ) ) then

         if ( adjust .ne. 0.d0 ) then

            call setmsg ( 'ADJUST should have value zero for all '  // &
                          'comparison operators except ABSMAX and ' // &
                          'ABSMIN' )
            call sigerr ( 'SPICE(INVALIDVALUE)'                     )
            call chkout ( 'ZZGFREL'                                 )
            return

         end if

      end if

!
!     If the confinement window is empty, the result window must
!     be empty as well.  In this case, there's not much to do.
!
      if ( cardd(cnfine) .eq. 0 ) then

         call scardd ( 0, result )
         call chkout ('ZZGFREL' )
         return

      end if

!
!     We need to set up several working windows, one each for
!     increasing and decreasing schedules, one for the confining
!     schedule and one for copying.
!
      call ssized ( mw, work(b,decres) )
      call ssized ( mw, work(b,incres) )
      call ssized ( mw, work(b,confin) )
      call ssized ( mw, work(b,copy  ) )
      call ssized ( mw, work(b,tempw ) )

      name(1) = decres
      name(2) = incres

      if (  failed() ) then
         call chkout (  'ZZGFREL' )
         return
      end if

!
!     For equality constraints, we work with a somewhat expanded
!     version of the confinement window so we can find equality
!     solutions that lie on the boundary of the original confinement
!     window. The expansion amount is ADDWIN. For other cases the
!     expansion amount is set to zero.
!
      if ( relate .eq.  '=' ) then

         addl = addwin
         addr = addwin
      else
         addl = 0.0d0
         addr = 0.0d0
      end if

      call copyd  ( cnfine,     work(b,confin) )
      call wnexpd ( addl, addr, work(b,confin) )

      if (  failed() ) then
         call chkout (  'ZZGFREL' )
         return
      end if

!
!     Make a local copy of the reference value.
!
      refer2 = refval

!
!     Set the pass number for progress reporting.
!
      pass = 1

!
!     Initialize the work in progress reporter.
!
      if ( rpt ) then
         call udrepi ( work(b,confin), rptpre(pass), rptsuf(pass) )
      end if

!
!     Look up the size of the confinement schedule...
!
      count = wncard ( work(b,confin) )

!
!     Start the window that contains intervals when the quantity of
!     interest is decreasing. The result will contain all intervals in
!     (expanded) CNFINE when the selected geometric quantity function
!     is decreasing, since this is how ZZGFSOLV is configured.
!
      do i = 1, count
!
!        Locate the bounds for the I'th interval of the confinement
!        schedule. Results are accumulated in the WORK array.
!
         call wnfetd ( work(b,confin), i, start, finish  )

         call zzgfsolv ( udqdec,  udstep, udrefn,  bail, &
                         udbail,  cstep,  step,    start, &
                         finish,  tol,    rpt,     udrepu, &
                         work(b,decres) )

         if (  failed()  ) then
            call chkout ( 'ZZGFREL'  )
            return
         end if

         if ( bail ) then

            if ( udbail () ) then

               if ( rpt ) then
                  call udrepf
               end if

               call chkout ( 'ZZGFREL' )
               return

            end if

         end if

      end do

      if ( rpt ) then
         call udrepf
      end if


!
!     Let's think about what we have now. We have the intervals in the
!     confinement window when a value of some kind is decreasing.
!
!     The left endpoints are points at which the quantity begins
!     decreasing, thus they are times when the quantity is at a local
!     maximum (at least in the interior of the confinement window).
!
!     The right endpoints are where the quantity stops decreasing. Thus
!     those endpoints in the interior of the confinement window are
!     local minima of the quantity.
!
!     The complement relative to the confinement window is the set of
!     intervals within the confinement window for which the quantity is
!     increasing. At the left endpoints of the complement the
!     function is increasing. Thus the interior left endpoints are
!     local minima within the confinement window. The interior right
!     endpoints are local maxima within the confinement window.
!
!     Moreover, to within our ability to detect local extrema, there
!     are no local extrema within any of the intervals. Thus, the
!     function may be regarded as monotone within each of
!     the intervals of these windows. Thus for any desired value of the
!     quantity, there is at most one time within each of the intervals
!     that the desired value is achieved.
!
      if ( locrel .eq. 'LOCMIN' ) then
!
!        We are interested in only interior minima of the quantity.
!        These occur at right endpoints of the intervals in TEMPW
!        that are interior points of CNFINE. First extract the right
!        endpoints. Then find those that are contained in the initial
!        confinement schedule, excluding endpoints.
!
         call wnextd ( 'R', work(b,decres) )

         call zzgfwsts ( work(b,decres), cnfine, '()', result )

         call chkout (  'ZZGFREL' )
         return


      else if ( locrel .eq. 'LOCMAX' ) then
!
!        We are interested in only interior maxima of the quantity.
!        These occur at right endpoints of the intervals in TEMPW
!        that are interior points of CNFINE.
!
         call wnextd ( 'L', work(b,decres) )

         call zzgfwsts ( work(b,decres), cnfine,  '()',  result )

         call chkout (  'ZZGFREL' )
         return

      end if

!
!     We will need the intervals when the quantity of interest is
!     increasing in value.
!
      if ( ( locrel .eq. 'ABSMIN') .or. ( locrel .eq. 'ABSMAX') ) then
!
!        We need an absolute max or min over the schedule CNFINE.
!        But we have decreasing values in WORK(B,DECRES).
!        Make a copy of WORK(B,DECRES) then compute the schedules
!        of decreasing or increasing quantity over the schedule CNFINE.
!
         call copyd  ( work(b,decres), work(b,copy) )

         call wnintd ( cnfine,         work(b,decres), work(b,tempw ) )
         call copyd  ( work(b,tempw),                  work(b,decres) )

         call wndifd ( cnfine,         work(b,decres), work(b,tempw ) )
         call copyd  ( work(b,tempw),                  work(b,incres) )

!
!        Here's what we plan to do, we want to look over two schedules
!        DECREASING and INCREASING to search for the absolute max or
!        min.  We start with DECREASING.  In this schedule the max is
!        always at the left endpoint,  The min is at the right
!        endpoint.  In the INCREASING schedule the min is at the LEFT
!        endpoint of an interval, the max is at the RIGHT endpoint of
!        an interval
!
         minat = right
         maxat = left

!
!        As yet we still need to compute our first extremum.
!
         need  = .true.

!
!        The extrema search is logically the same for both
!        maximum and minimum. We just need to keep track of
!        our extremum and when we find a more extreme value
!        replace it. DECREASING is first.
!
         do case = 1, 2

            if ( locrel .eq. 'ABSMIN' ) then

               want  = minat

            else if ( locrel .eq. 'ABSMAX' ) then

               want  = maxat

            end if


            winsiz = wncard (  work( b, name(case) )  )

            do i = 1, winsiz

               call wnfetd ( work( b,  name(case) ), i, &
                             endpt(1), endpt(2)  )

               call udfunc ( endpt(want), value )

               if (  failed() ) then
                  call chkout (  'ZZGFREL' )
                  return
               end if

!
!              Initialize the extreme value. This step will
!              be executed on the first pass through the
!              DECREASING interval.
!
               if ( need ) then

                  need   = .false.
                  extrem = value

               end if

!
!              Check to see if current VALUE is more extreme than
!              EXTREM.
!
               if ( locrel .eq. 'ABSMIN' ) then

                  if (       ( adjust .eq. 0.0d0  ) &
                       .and. ( value  .le. extrem )  ) then
!
!                    Let's save the epoch in case it's that of the
!                    absolute min. Add this endpoint as a singleton
!                    interval to the RESULT window.
!
                     call scardd ( 0, result )

                     contxt = 'Saving current candidate epoch at ' &
                     //       'which an absolute minimum may occur.'

                     call zzwninsd ( endpt(want), endpt(want), &
                                     contxt,      result      )

                  end if

                  extrem = min ( extrem, value )


               else

                  if (       ( adjust .eq. 0.0d0  ) &
                       .and. ( value  .ge. extrem )  ) then
!
!                    Let's save the epoch in case it's that of the
!                    absolute max. Add this endpoint as a singleton
!                    interval to the RESULT window.
!
                     call scardd ( 0, result )

                     contxt = 'Saving current candidate epoch at ' &
                     //       'which an absolute maximum may occur.'

                     call zzwninsd ( endpt(want), endpt(want), &
                                     contxt,      result      )

                  end if

                  extrem = max ( extrem, value )

               end if

            end do

            if (  failed() ) then
               call chkout (  'ZZGFREL' )
               return
            end if

!
!           When we go to the next schedule, the min and max are at
!           opposite ends of the intervals.
!
            call swapi ( minat, maxat )

         end do

!
!        If the adjustment is zero, we're done.
!
         if ( adjust .eq. 0.0d0 ) then

            call chkout (  'ZZGFREL' )
            return

         end if

!
!        We have a non-zero adjustment. we have the extreme value. Now
!        we need to find the epochs when the extreme value is achieved,
!        allowing for adjustment.
!
         if ( locrel  .eq. 'ABSMIN' ) then

            refer2 = extrem + adjust

         else
!
!           The only other possible value of LOCREL within this block
!           is 'ABSMAX'.
!
            refer2 = extrem - adjust

         end if
!
!        If we reach this point, we need to re-establish the
!        original expanded coverage of 'DECREASING' and 'INCREASING'.
!
         call copyd ( work(b,copy), work(b,decres) )

      end if

      call wndifd ( work(b,confin), work(b,decres), work(b,incres) )

      if (  failed() ) then
         call chkout (  'ZZGFREL' )
         return
      end if


!
!     We have some kind of greater than, less than, or equal to
!     relation to solve for. Note that ABSMAX and ABSMIN are for case
!     where there is a non-zero adjustment. Reset the reference value,
!     which may have been changed in the ABSOLUTE MAX or MIN blocks
!     above.
!
      call udqref ( refer2 )

!
!     If progress reporting is enabled, initialize the progress
!     reporter for a second pass over the confinement window.
!
      if ( rpt ) then
!
!        Note that the window passed to UDREPI need not contain the
!        same intervals as those passed to UDREPU; the window passed to
!        UPREPI need only have the correct measure. From UDREPI's
!        perspective, the sole purpose of this window is to convey to
!        the progress reporting system the sum of the measures of the
!        increasing and decreasing windows.
!
         pass = 2

         call udrepi ( work(b,confin), rptpre(pass), rptsuf(pass) )

      end if

!
!     Find those intervals when the geometric quantity is less than
!     REFER2.
!
      call scardd ( 0, result )

      do case = 1, 2

         winsiz = wncard (  work(b, name(case))  )

!
!        Search each interval of the window identified by NAME(CASE) for
!        times when the quantity is less than the reference value.
!
         do i = 1, winsiz

            call wnfetd ( work( b, name(case)), i, start, finish  )
!
!           For each interval, accumulate the result in RESULT.
!
!           Note we know that the behavior of the quantity is monotonic
!           within each window, so the step size can be large. In fact,
!           we use the interval length as the step size.
!
            step = finish - start

            call zzgfsolv ( udcond, udstep,  udrefn,  bail, &
                            udbail, .true.,  step,    start, &
                            finish, tol,     rpt,     udrepu, &
                            result                           )

            if (  failed()  ) then
               call chkout ( 'ZZGFREL'  )
               return
            end if

            if ( bail ) then
               if ( udbail () ) then
                  call chkout ( 'ZZGFREL' )
                  return
               end if
            end if

         end do

      end do


      if ( rpt ) then
!
!        Finish the progress report for the second pass.
!
         call udrepf

      end if

!
!     RESULT is the window, within the expanded confinement window,
!     over which the function of interest is less than the reference
!     value. We can use this window to get whatever was requested.
!
      if ( ( locrel .eq. '<' ) .or. ( locrel .eq. 'ABSMIN' ) ) then
!
!        We simply need to restrict our result to the original
!        confinement schedule. Note that the ABSMIN search with
!        non-zero adjustment is now a search for values less than the
!        adjusted absolute minimum. Same for ABSMAX below.
!
         call wnintd ( cnfine,         result,  work(b,tempw) )
         call copyd  ( work(b,tempw),           result        )


      else if ( ( locrel .eq. '>' ) .or. ( locrel .eq. 'ABSMAX' ) ) then
!
!        Subtract from the confinement window the window where the
!        quantity is less than the reference value: the remainder is
!        the portion of the confinement window on which the quantity is
!        greater than or equal to the reference value.
!
         call wndifd ( cnfine,         result,  work(b,tempw) )
         call copyd  ( work(b,tempw),           result        )

      else
!
!        This is the branch for the relational operator '='.
!
!        Create a window of singleton intervals from the endpoints
!        of RESULT.
!
         call scardd ( 0, work(b,tempw) )

         do i = 1, cardd(result)

            contxt = 'Inserting endpoints of result window into ' &
            //       'workspace window WORK(B,TEMPW). These points ' &
            //       'are candidate epochs that may satisfy an ' &
            //       'equality constraint.'

            call zzwninsd ( result(i), result(i), &
                            contxt,    work(b,tempw) )

            if ( failed() ) then
               call chkout ( 'ZZGFREL' )
               return
            end if

         end do

!
!        The window WORK(B,TEMPW) contains singleton intervals where
!        either the equality constraint is met, or where a boundary
!        point of the expanded confinement window is located. We're not
!        interested in the boundary points; these are likely not
!        solution points and in any case are outside the original
!        confinement window.
!
!        Keep only the endpoints of RESULT that are contained in the
!        original confinement window CNFINE; these are by construction
!        interior points of the expanded confinement window.
!
         call wnintd ( cnfine,  work(b,tempw),  result )

      end if

      call chkout ( 'ZZGFREL' )
      return
      end
