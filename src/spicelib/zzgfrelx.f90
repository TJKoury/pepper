!$Procedure ZZGFRELX ( Private --- GF, geometric relation finder )

      SUBROUTINE ZZGFRELX ( UDSTEP,   UDREFN,  UDQDEC,  UDCOND, &
                            UDFUNC,   RELATE,  REFVAL, &
                            TOL,      ADJUST,  CNFINE,  MW, &
                            NW,       WORK,    RPT,     UDREPI, &
                            UDREPU,   UDREPF,  RPTPRE,  RPTSUF, &
                            BAIL,     UDBAIL,  RESULT           )

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
!     Sister routine to ZZGFREL. Copy any edits to ZZGFREL or ZZGFRELX
!     to the sister routine.
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

      IMPLICIT NONE

      INCLUDE               'gf.inc'

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NWREQ
      PARAMETER           ( NWREQ  = 5 )

      EXTERNAL              UDSTEP
      EXTERNAL              UDREFN
      EXTERNAL              UDQDEC
      EXTERNAL              UDCOND
      EXTERNAL              UDFUNC
      CHARACTER*(*)         RELATE
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : * )
      INTEGER               MW
      INTEGER               NW
      DOUBLE PRECISION      WORK ( LBCELL : MW, NW )
      LOGICAL               RPT
      EXTERNAL              UDREPI
      EXTERNAL              UDREPU
      EXTERNAL              UDREPF
      CHARACTER*(*)         RPTPRE ( * )
      CHARACTER*(*)         RPTSUF ( * )
      LOGICAL               UDBAIL
      LOGICAL               BAIL
      EXTERNAL              UDBAIL
      DOUBLE PRECISION      RESULT ( LBCELL : * )

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
!                    scalar quantity is decreasing.
!     UDCOND     I   Name of the routine that computes the scalar
!                    quantity condition with-respect-to the constraint.
!     UDFUNC     I   The routine that computes the scalar quantity of
!                    interest.
!     RELATE     I   Operator that either looks for an extreme value
!                    (max, min, local, absolute) or compares the
!                    scalar quantity value and a number.
!     REFVAL     I   Value used as reference for scalar quantity
!                    condition.
!     TOL        I   Convergence tolerance in seconds.
!     ADJUST     I   Allowed variation for absolute extremal
!                    scalar conditions.
!     CNFINE     I   Confinement window
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
!                find a transition of the scalar quantity. In the
!                context of this routine's algorithm, a "transition"
!                occurs where the scalar quantity value changes from
!                "decreasing" to "not decreasing" or vice versa.
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
!     UDQDEC     the routine that determines if the scalar quantity
!                calculated by UDFUNC is decreasing.
!
!                The calling sequence:
!
!                   CALL UDQDEC ( UDFUNC, ET, ISDECR )
!
!                where:
!
!                   ET       a double precision value representing
!                            ephemeris time, expressed as seconds past
!                            J2000 TDB, at which to determine the time
!                            derivative of UDFUNC.
!
!                   ISDECR   a logical return indicating whether
!                            or not the scalar value returned by UDFUNC
!                            is decreasing. ISDECR returns true if the
!                            time derivative of UDFUNC at ET is
!                            negative.
!
!     UDCOND     the routine that determines if UDFUNC satisfies
!                some constraint condition at epoch ET.
!
!                The calling sequence:
!
!                   CALL UDCOND ( UDFUNC, ET, IN_CON )
!
!                where:
!
!                   ET       a double precision value representing
!                            ephemeris time, expressed as seconds past
!                            J2000 TDB, at which to evaluate UDFUNC.
!
!                   IN_CON   a logical value indicating whether
!                            or not UDFUNC satisfies the constraint
!                            at ET (TRUE) or not (FALSE).
!
!     UDFUNC     the routine that returns the value of the scalar
!                quantity of interest at time ET. The calling sequence
!                for UDFUNC is:
!
!                   CALL UDFUNC ( ET, VALUE )
!
!                where:
!
!                   ET      a double precision value representing
!                           ephemeris time, expressed as seconds past
!                           J2000 TDB, at which to determine the scalar
!                           value.
!
!                   VALUE   the double precision value of the scalar
!                           quantity at ET.
!
!     RELATE     is a comparison operator, indicating the numeric
!                constraint of interest. Values are:
!
!                '>'   value of scalar quantity greater than REFVAL.
!
!                '='   value of scalar quantity equal to REFVAL.
!
!                '<'   value of scalar quantity less than REFVAL.
!
!                ABSMAX-the scalar quantity is at an absolute
!                maximum.
!
!                ABSMIN-the scalar quantity is at an absolute
!                minimum.
!
!                LOCMAX-the scalar quantity is at an local maximum.
!
!                LOCMIN-the scalar quantity is at an local minimum.
!
!     REFVAL     reference value for scalar quantity (in
!                radians, radians/sec, km, or km/sec as appropriate).
!
!     TOL        is a tolerance value used to determine convergence of
!                root-finding operations.  TOL is measured in seconds
!                and is greater than zero.
!
!     ADJUST     the amount by which the numerical quantity is
!                allowed to vary from an absolute extremum. If ADJUST
!                is non-zero, the resulting window contains
!                intervals when the scalar quantity has
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
!                routine, the SPICELIB routine GFREPI may be used. This
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
!                the SPICELIB routine GFREPU may be used. This is the
!                default option.
!
!     UDREPF     the routine that finalizes a progress report. UDREPF
!                has no arguments.
!
!                If the user has no progress reporting finalizing
!                routine, the SPICELIB routine GFREPF may be used. This
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
!                GFBAIL must be supplied.
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
!     scalar quantity related to one or more objects and an observer
!     satisfies a user specified constraint. It puts these times in a
!     result window called RESULT. It does this by first finding
!     windows when the quantity of interest is either
!     monotonically increasing or decreasing. These windows are then
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
!     The kernel files to be used by ZZGFRELX must be loaded (normally
!     via the SPICELIB routine FURNSH) before ZZGFRELX is called.
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
!-    SPICELIB Version 1.2.0  03-OCT-2021 (NJB)
!
!        Variable CSTEP is now saved. Corrected typo in comment.
!
!-    SPICELIB Version 1.1.1  08-DEC-2010  (EDW)
!
!        Edit to replace term "schedule" with "window." Edit to
!        Procedure line text.
!
!        Argument list changed, removing the argument for
!        the reference value update routine, UDQREF. Setting and
!        updating the reference value now occurs in this
!        (ZZGFRELX) routine.
!
!        Edit to Detailed I/O description for UDREPI, UDREPU, UDREPF,
!        and UDBAIL. The descriptions stated the wrong name for the
!        default GF functions corresponding to these arguments.
!
!-    SPICELIB Version 1.1.0  16-FEB-2010  (EDW)
!
!        Modified version of ZZGFREL. This version calls ZZGFSOLVX.
!
!-    SPICELIB Version 1.0.0  21-FEB-2009 (NJB) (LSE) (WLT) (IMU) (EDW)
!
!-&

! $ Index_Entries
!
! numeric scalar quantity satisfies a condition
!
!-&

!
!     SPICELIB functions
!
      INTEGER               CARDD
      INTEGER               ISRCHC
      INTEGER               SIZED
      INTEGER               WNCARD

      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               LEFT
      PARAMETER           ( LEFT   = 1 )

      INTEGER               RIGHT
      PARAMETER           ( RIGHT  = 2 )

      INTEGER               MAXCLN
      PARAMETER           ( MAXCLN = 80 )

!
!     Workspace window indices:
!
      INTEGER               CONFIN
      PARAMETER           ( CONFIN = 3 )

      INTEGER               COPY
      PARAMETER           ( COPY   = 4 )

      INTEGER               DECRES
      PARAMETER           ( DECRES = 2 )

      INTEGER               INCRES
      PARAMETER           ( INCRES = 1 )

      INTEGER               TEMPW
      PARAMETER           ( TEMPW  = 5 )


!
!     Number of supported comparison operators.
!
      INTEGER               NC
      PARAMETER           ( NC     = 7 )

!     One-letter alias for LBCELL to make references to the workspace
!     array tolerable:
!
      INTEGER               B
      PARAMETER           ( B      = LBCELL )

!
!     Context string length:
!
      INTEGER               CTXLEN
      PARAMETER           ( CTXLEN = 500 )

!
!     Local variables
!

      CHARACTER*(MAXCLN)    CNAMES ( NC )
      CHARACTER*(CTXLEN)    CONTXT
      CHARACTER*(MAXCLN)    LOCREL

      DOUBLE PRECISION      ADDL
      DOUBLE PRECISION      ADDR
      DOUBLE PRECISION      ENDPT  (  2 )
      DOUBLE PRECISION      EXTREM
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      REFER2
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      VALUE

      INTEGER               CASE
      INTEGER               COUNT
      INTEGER               I
      INTEGER               MAXAT
      INTEGER               MINAT
      INTEGER               NAME    (  2 )
      INTEGER               PASS
      INTEGER               QCNUM
      INTEGER               WANT
      INTEGER               WINSIZ

      LOGICAL               CSTEP
      LOGICAL               NEED


!
!     Saved variables
!
      SAVE                  CNAMES
      SAVE                  CSTEP

!
!     Below we initialize the list of comparison operator names.
!
      DATA                  CNAMES / '<', &
                                     '=', &
                                     '>', &
                                     'LOCMIN', &
                                     'ABSMIN', &
                                     'LOCMAX', &
                                     'ABSMAX'  /


!
!     Set constant step parameter to .FALSE..
!
      DATA                  CSTEP / .FALSE. /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFRELX' )

!
!     Make sure we have enough workspace windows.
!
      IF ( NW .LT. NWREQ ) THEN

         CALL SETMSG ( 'The number of workspace windows (#) is less ' &
         //            'than the minimum #.'                        )
         CALL ERRINT ( '#',  NW                                     )
         CALL ERRINT ( '#',  NWREQ                                  )
         CALL SIGERR ( 'SPICE(TOOFEWWINDOWS)'                       )
         CALL CHKOUT ( 'ZZGFRELX'                                   )
         RETURN

      END IF

!
!     Make sure the workspace windows can contain at least one interval.
!
      IF ( MW .LT. 2 ) THEN

         CALL SETMSG ( 'Workspace window size was #; size must be ' &
         //            'at least 2.'                               )
         CALL ERRINT ( '#',  MW                                    )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                   )
         CALL CHKOUT ( 'ZZGFRELX'                                  )
         RETURN

      END IF

!
!     Check the result window size.
!
      IF ( SIZED(RESULT) .LT. 2 ) THEN

         CALL SETMSG ( 'Result window size was #; size must be ' &
         //            'at least 2.'                             )
         CALL ERRINT ( '#',  SIZED(RESULT)                       )
         CALL SIGERR ( 'SPICE(INVALIDDIMENSION)'                 )
         CALL CHKOUT ( 'ZZGFRELX'                                 )
         RETURN

      END IF

!
!     Make sure the requested comparison is one we recognize.
!
      CALL LJUST ( RELATE, LOCREL )
      CALL UCASE ( LOCREL, LOCREL )

      QCNUM = ISRCHC ( LOCREL, NC, CNAMES )

      IF ( QCNUM .EQ. 0 ) THEN

        CALL SETMSG ('The comparison operator, # is not '            // &
                     'recognized.  Supported quantities are: '       // &
                     '<, '                                           // &
                     '=, '                                           // &
                     '>, '                                           // &
                     'LOCMIN, '                                      // &
                     'ABSMIN, '                                      // &
                     'LOCMAX, '                                      // &
                     'ABSMAX.'                                        )
         CALL ERRCH  ('#', RELATE                                     )
         CALL SIGERR ('SPICE(NOTRECOGNIZED)'                          )
         CALL CHKOUT ('ZZGFRELX'                                       )
         RETURN

      END IF

!
!     Confirm ADJUST is non-negative.
!
      IF ( ADJUST .LT. 0.D0 ) THEN

         CALL SETMSG ( 'ADJUST was #; must be non-negative.' )
         CALL ERRDP  ( '#',  ADJUST                          )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)'              )
         CALL CHKOUT ( 'ZZGFRELX'                             )
         RETURN

      END IF

!
!    Confirm ADJUST equals zero unless LOCREL (RELATE) has value
!    "ABSMAX" or "ABSMIN."
!
      IF(  (LOCREL .NE. 'ABSMIN')  .AND. (LOCREL .NE. 'ABSMAX' ) ) THEN

         IF ( ADJUST .NE. 0.D0 ) THEN

            CALL SETMSG ( 'ADJUST should have value zero for all '  // &
                          'comparison operators except ABSMAX and ' // &
                          'ABSMIN' )
            CALL SIGERR ( 'SPICE(INVALIDVALUE)'                     )
            CALL CHKOUT ( 'ZZGFRELX'                                 )
            RETURN

         END IF

      END IF

!
!     If the confinement window is empty, the result window must
!     be empty as well. In this case, there's not much to do.
!
      IF ( CARDD(CNFINE) .EQ. 0 ) THEN

         CALL SCARDD ( 0, RESULT )
         CALL CHKOUT ('ZZGFRELX' )
         RETURN

      END IF

!
!     We need to set up several working windows, one each for
!     increasing and decreasing windows, one for the confining
!     window and one for copying.
!
      CALL SSIZED ( MW, WORK(B,DECRES) )
      CALL SSIZED ( MW, WORK(B,INCRES) )
      CALL SSIZED ( MW, WORK(B,CONFIN) )
      CALL SSIZED ( MW, WORK(B,COPY  ) )
      CALL SSIZED ( MW, WORK(B,TEMPW ) )

      NAME(1) = DECRES
      NAME(2) = INCRES

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFRELX' )
         RETURN
      END IF

!
!     For equality constraints, we work with a somewhat expanded
!     version of the confinement window so we can find equality
!     solutions that lie on the boundary of the original confinement
!     window. The expansion amount is ADDWIN. For other cases the
!     expansion amount is set to zero.
!
      IF ( RELATE .EQ.  '=' ) THEN

         ADDL = ADDWIN
         ADDR = ADDWIN
      ELSE
         ADDL = 0.0D0
         ADDR = 0.0D0
      END IF

      CALL COPYD  ( CNFINE,     WORK(B,CONFIN) )
      CALL WNEXPD ( ADDL, ADDR, WORK(B,CONFIN) )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFRELX' )
         RETURN
      END IF

!
!     Set the reference value.
!
      CALL ZZGFREF( REFVAL )

!
!     Make a local copy of the reference value.
!
      REFER2 = REFVAL


!
!     Set the pass number for progress reporting.
!
      PASS = 1

!
!     Initialize the work in progress reporter.
!
      IF ( RPT ) THEN
         CALL UDREPI ( WORK(B,CONFIN), RPTPRE(PASS), RPTSUF(PASS) )
      END IF

!
!     Look up the size of the confinement window...
!
      COUNT = WNCARD ( WORK(B,CONFIN) )

!
!     Start the window that contains intervals when the quantity of
!     interest is decreasing. The result will contain all intervals in
!     (expanded) CNFINE when the selected scalar quantity function
!     is decreasing, since this is how ZZGFSOLVX is configured.
!
      DO I = 1, COUNT
!
!        Locate the bounds for the I'th interval of the confinement
!        window. Results are accumulated in the WORK array.
!
         CALL WNFETD ( WORK(B,CONFIN), I, START, FINISH  )

         CALL ZZGFSOLVX ( UDFUNC,  UDQDEC, UDSTEP, UDREFN,  BAIL, &
                          UDBAIL,  CSTEP,  STEP,   START, &
                          FINISH,  TOL,    RPT,    UDREPU, &
                          WORK(B,DECRES) )

         IF (  FAILED()  ) THEN
            CALL CHKOUT ( 'ZZGFRELX'  )
            RETURN
         END IF

         IF ( BAIL ) THEN

            IF ( UDBAIL () ) THEN

               IF ( RPT ) THEN
                  CALL UDREPF
               END IF

               CALL CHKOUT ( 'ZZGFRELX' )
               RETURN

            END IF

         END IF

      END DO

      IF ( RPT ) THEN
         CALL UDREPF
      END IF


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
      IF ( LOCREL .EQ. 'LOCMIN' ) THEN
!
!        We are interested in only interior minima of the quantity.
!        These occur at right endpoints of the intervals in TEMPW
!        that are interior points of CNFINE. First extract the right
!        endpoints. Then find those that are contained in the initial
!        confinement window, excluding endpoints.
!
         CALL WNEXTD ( 'R', WORK(B,DECRES) )

         CALL ZZGFWSTS ( WORK(B,DECRES), CNFINE, '()', RESULT )

         CALL CHKOUT (  'ZZGFRELX' )
         RETURN


      ELSE IF ( LOCREL .EQ. 'LOCMAX' ) THEN
!
!        We are interested in only interior maxima of the quantity.
!        These occur at right endpoints of the intervals in TEMPW
!        that are interior points of CNFINE.
!
         CALL WNEXTD ( 'L', WORK(B,DECRES) )

         CALL ZZGFWSTS ( WORK(B,DECRES), CNFINE,  '()',  RESULT )

         CALL CHKOUT (  'ZZGFRELX' )
         RETURN

      END IF

!
!     We will need the intervals when the quantity of interest is
!     increasing in value.
!
      IF ( ( LOCREL .EQ. 'ABSMIN') .OR. ( LOCREL .EQ. 'ABSMAX') ) THEN
!
!        We need an absolute max or min over the window CNFINE.
!        But we have decreasing values in WORK(B,DECRES).
!        Make a copy of WORK(B,DECRES) then compute the windows
!        of decreasing or increasing quantity over the window CNFINE.
!
         CALL COPYD  ( WORK(B,DECRES), WORK(B,COPY) )

         CALL WNINTD ( CNFINE,         WORK(B,DECRES), WORK(B,TEMPW ) )
         CALL COPYD  ( WORK(B,TEMPW),                  WORK(B,DECRES) )

         CALL WNDIFD ( CNFINE,         WORK(B,DECRES), WORK(B,TEMPW ) )
         CALL COPYD  ( WORK(B,TEMPW),                  WORK(B,INCRES) )

!
!        Here's what we plan to do, we want to look over two windows
!        DECREASING and INCREASING to search for the absolute max or
!        min.  We start with DECREASING.  In this window the max is
!        always at the left endpoint,  The min is at the right
!        endpoint.  In the INCREASING window the min is at the LEFT
!        endpoint of an interval, the max is at the RIGHT endpoint of
!        an interval
!
         MINAT = RIGHT
         MAXAT = LEFT

!
!        As yet we still need to compute our first extremum.
!
         NEED  = .TRUE.

!
!        The extrema search is logically the same for both
!        maximum and minimum. We just need to keep track of
!        our extremum and when we find a more extreme value
!        replace it. DECREASING is first.
!
         DO CASE = 1, 2

            IF ( LOCREL .EQ. 'ABSMIN' ) THEN

               WANT  = MINAT

            ELSE IF ( LOCREL .EQ. 'ABSMAX' ) THEN

               WANT  = MAXAT

            END IF


            WINSIZ = WNCARD (  WORK( B, NAME(CASE) )  )

            DO I = 1, WINSIZ

               CALL WNFETD ( WORK( B,  NAME(CASE) ), I, &
                             ENDPT(1), ENDPT(2)  )

               CALL UDFUNC ( ENDPT(WANT), VALUE )

               IF (  FAILED() ) THEN
                  CALL CHKOUT (  'ZZGFRELX' )
                  RETURN
               END IF

!
!              Initialize the extreme value. This step will
!              be executed on the first pass through the
!              DECREASING interval.
!
               IF ( NEED ) THEN

                  NEED   = .FALSE.
                  EXTREM = VALUE

               END IF

!
!              Check to see if current VALUE is more extreme than
!              EXTREM.
!
               IF ( LOCREL .EQ. 'ABSMIN' ) THEN

                  IF (       ( ADJUST .EQ. 0.0D0  ) &
                       .AND. ( VALUE  .LE. EXTREM )  ) THEN
!
!                    Let's save the epoch in case it's that of the
!                    absolute min. Add this endpoint as a singleton
!                    interval to the RESULT window.
!
                     CALL SCARDD ( 0, RESULT )

                     CONTXT = 'Saving current candidate epoch at ' &
                     //       'which an absolute minimum may occur.'

                     CALL ZZWNINSD ( ENDPT(WANT), ENDPT(WANT), &
                                     CONTXT,      RESULT      )

                  END IF

                  EXTREM = MIN ( EXTREM, VALUE )


               ELSE

                  IF (       ( ADJUST .EQ. 0.0D0  ) &
                       .AND. ( VALUE  .GE. EXTREM )  ) THEN
!
!                    Let's save the epoch in case it's that of the
!                    absolute max. Add this endpoint as a singleton
!                    interval to the RESULT window.
!
                     CALL SCARDD ( 0, RESULT )

                     CONTXT = 'Saving current candidate epoch at ' &
                     //       'which an absolute maximum may occur.'

                     CALL ZZWNINSD ( ENDPT(WANT), ENDPT(WANT), &
                                     CONTXT,      RESULT      )

                  END IF

                  EXTREM = MAX ( EXTREM, VALUE )

               END IF

            END DO

            IF (  FAILED() ) THEN
               CALL CHKOUT (  'ZZGFRELX' )
               RETURN
            END IF

!
!           When we go to the next window, the min and max are at
!           opposite ends of the intervals.
!
            CALL SWAPI ( MINAT, MAXAT )

         END DO

!
!        If the adjustment is zero, we're done.
!
         IF ( ADJUST .EQ. 0.0D0 ) THEN

            CALL CHKOUT (  'ZZGFRELX' )
            RETURN

         END IF

!
!        We have a non-zero adjustment. we have the extreme value. Now
!        we need to find the epochs when the extreme value is achieved,
!        allowing for adjustment.
!
         IF ( LOCREL .EQ. 'ABSMIN' ) THEN

            REFER2 = EXTREM + ADJUST

         ELSE

!
!           The only other possible value of LOCREL within this block
!           is 'ABSMAX'.
!
            REFER2 = EXTREM - ADJUST

         END IF

!
!        If we reach this point, we need to re-establish the
!        original expanded coverage of 'DECREASING' and 'INCREASING'.
!
         CALL COPYD ( WORK(B,COPY), WORK(B,DECRES) )

      END IF

      CALL WNDIFD ( WORK(B,CONFIN), WORK(B,DECRES), WORK(B,INCRES) )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFRELX' )
         RETURN
      END IF


!
!     We have some kind of greater than, less than, or equal to
!     relation to solve for. Note that ABSMAX and ABSMIN are for case
!     where there is a non-zero adjustment. Reset the reference value,
!     which may have been changed in the ABSOLUTE MAX or MIN blocks
!     above.
!
      CALL ZZGFREF ( REFER2 )


!
!     If progress reporting is enabled, initialize the progress
!     reporter for a second pass over the confinement window.
!
      IF ( RPT ) THEN

!
!        Note that the window passed to UDREPI need not contain the
!        same intervals as those passed to UDREPU; the window passed to
!        UPREPI need only have the correct measure. From UDREPI's
!        perspective, the sole purpose of this window is to convey to
!        the progress reporting system the sum of the measures of the
!        increasing and decreasing windows.
!
         PASS = 2

         CALL UDREPI ( WORK(B,CONFIN), RPTPRE(PASS), RPTSUF(PASS) )

      END IF

!
!     Find those intervals when the scalar quantity is less than
!     REFER2.
!
      CALL SCARDD ( 0, RESULT )

      DO CASE = 1, 2

         WINSIZ = WNCARD (  WORK(B, NAME(CASE))  )

!
!        Search each interval of the window identified by NAME(CASE) for
!        times when the quantity is less than the reference value.
!
         DO I = 1, WINSIZ

            CALL WNFETD ( WORK( B, NAME(CASE)), I, START, FINISH  )
!
!           For each interval, accumulate the result in RESULT.
!
!           Note we know that the behavior of the quantity is monotonic
!           within each window, so the step size can be large. In fact,
!           we use the interval length as the step size.
!
            STEP = FINISH - START

            CALL ZZGFSOLVX ( UDFUNC, UDCOND,  UDSTEP,  UDREFN,  BAIL, &
                             UDBAIL, .TRUE.,  STEP,    START, &
                             FINISH, TOL,     RPT,     UDREPU, &
                             RESULT                           )

            IF (  FAILED()  ) THEN
               CALL CHKOUT ( 'ZZGFRELX'  )
               RETURN
            END IF

            IF ( BAIL ) THEN
               IF ( UDBAIL () ) THEN
                  CALL CHKOUT ( 'ZZGFRELX' )
                  RETURN
               END IF
            END IF

         END DO

      END DO


      IF ( RPT ) THEN
!
!        Finish the progress report for the second pass.
!
         CALL UDREPF

      END IF

!
!     RESULT is the window, within the expanded confinement window,
!     over which the function of interest is less than the reference
!     value. We can use this window to get whatever was requested.
!
      IF ( ( LOCREL .EQ. '<' ) .OR. ( LOCREL .EQ. 'ABSMIN' ) ) THEN
!
!        We simply need to restrict our result to the original
!        confinement window. Note that the ABSMIN search with
!        non-zero adjustment is now a search for values less than the
!        adjusted absolute minimum. Same for ABSMAX below.
!
         CALL WNINTD ( CNFINE,         RESULT,  WORK(B,TEMPW) )
         CALL COPYD  ( WORK(B,TEMPW),           RESULT        )


      ELSE IF ( ( LOCREL .EQ. '>' ) .OR. ( LOCREL .EQ. 'ABSMAX' ) ) THEN
!
!        Subtract from the confinement window the window where the
!        quantity is less than the reference value: the remainder is
!        the portion of the confinement window on which the quantity is
!        greater than or equal to the reference value.
!
         CALL WNDIFD ( CNFINE,         RESULT,  WORK(B,TEMPW) )
         CALL COPYD  ( WORK(B,TEMPW),           RESULT        )

      ELSE
!
!        This is the branch for the relational operator '='.
!
!        Create a window of singleton intervals from the endpoints
!        of RESULT.
!
         CALL SCARDD ( 0, WORK(B,TEMPW) )

         DO I = 1, CARDD(RESULT)

            CONTXT = 'Inserting endpoints of result window into ' &
            //       'workspace window WORK(B,TEMPW). These points ' &
            //       'are candidate epochs that may satisfy an ' &
            //       'equality constraint.'

            CALL ZZWNINSD ( RESULT(I), RESULT(I), &
                            CONTXT,    WORK(B,TEMPW) )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFRELX' )
               RETURN
            END IF

         END DO

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
         CALL WNINTD ( CNFINE,  WORK(B,TEMPW),  RESULT )

      END IF

      CALL CHKOUT ( 'ZZGFRELX' )
      RETURN
      END
