!$Procedure ZZGFRRU ( Private --- GF, range rate utility routine )

      subroutine zzgfrru ( target, abcorr, obsrvr, dt, &
                           udfunc, et,     decres, rvl, &
                           xtarg, xabcor, xobs, xdt )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the umbrella routine for the entry points needed by
!     GFEVNT in order to find range rate events.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     RANGE RATE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations

      implicit none

      include 'zzabcorr.inc'

      character*(*)         target
      character*(*)         abcorr
      character*(*)         obsrvr
      double precision      et
      double precision      dt
      logical               decres
      double precision      rvl
      integer               xtarg
      character*(*)         xabcor
      integer               xobs
      double precision      xdt

! $ Brief_I/O
!
!     VARIABLE  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     TARGET     I   ZZGFRRIN
!     ABCORR     I   ZZGFRRIN
!     OBSRVR     I   ZZGFRRIN
!     ET         I   ZZGFRRDC, ZZGFRRGQ
!     DT         I   ZZGFRRIN
!     UDFUNC     I   ZZGFRRDC
!     DECRES     O   ZZGFRRDC
!     RVL        O   ZZGFRRGQ
!     XTARG      O   ZZGFRRX
!     XABCOR     O   ZZGFRRX
!     XOBS       O   ZZGFRRX
!     XDT        O   ZZGFRRX
!
! $ Detailed_Input
!
!     TARGET   the string name of a target body.  Optionally, you may
!              supply the integer ID code for the object as an
!              integer string.  For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!              The target and observer define a position vector
!              that points from the observer to the target.
!
!     ABCORR   the string description of the aberration corrections to
!              apply to the state evaluations to account for one-way
!              light time and stellar aberration.
!
!              Any aberration correction accepted by the SPICE
!              routine SPKEZR is accepted here. See the header
!              of SPKEZR for a detailed description of the
!              aberration correction options. For convenience,
!              the options are listed below:
!
!                 'NONE'     Apply no correction. Returns the "true"
!                            geometric state.
!
!                 'LT'       "Reception" case:  correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'LT+S'     "Reception" case:  correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'CN'       "Reception" case:  converged
!                            Newtonian light time correction.
!
!                 'CN+S'     "Reception" case:  converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 'XLT'      "Transmission" case:  correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'XLT+S'    "Transmission" case:  correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'XCN'      "Transmission" case:  converged
!                            Newtonian light time correction.
!
!                 'XCN+S'    "Transmission" case:  converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!              The ABCORR string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     OBSRVR   the string name of an observing body.  Optionally, you
!              may supply the ID code of the object as an integer
!              string. For example, both 'EARTH' and '399' are
!              legitimate strings to indicate the observer as Earth.
!
!     ET       time in TDB seconds past J2000 at which to calculate
!              the value of or characteristic of the range rate of
!              the observer-target vector.
!
!     DT       a scalar double precision value representing half the
!              interval in TDB seconds separating the evaluation
!              epochs; the evaluations occur at epochs
!              (ET + DT) and (ET - DT).
!
!              DT may be negative but must be non-zero.
!
!     XTARG    SPICE ID value for the target body initialized
!              via ZZGFRRIN.
!
!     XABCOR   String value for the aberration correction initialized
!              via ZZGFRRIN.
!
!     XOBS     SPICE ID value for the observing body initialized
!              via ZZGFRRIN.
!
!     XDT      Saved value for DT.
!
!     For more information, see individual entry points.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine serves as the umbrella routine for three entry points
!     needed by GFEVNT in solving for range rate conditions.
!
!     The three entry points are
!
!        ZZGFRRIN --- an initialization routine that must be called
!                     prior to attempting to solve for any range
!                     rate event.
!
!        ZZGFRRDC --- determines whether or not range rate is
!                     decreasing at some time.
!
!        ZZGFRRGQ --- returns the range rate of the two objects
!                     of concern as a function of ET.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     ZZGFRRIN must be called prior to use of any of the other
!     entry points (think constructor).
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     L.S. Elson     (JPL)
!
! $ Version
!
!-    SPICELIB version 2.0.1 01-OCT-2021 (NJB)
!
!        Fixed typo in comments.
!
!-    SPICELIB version 2.0.0 21-APR-2014 (EDW)
!
!        Added BOGUSENTRY error check to call ZZGFRRU.
!
!        Added ZZGFRRX entry point for test family
!        retrieval of values saved by ZZGFRRIN.
!
!        Code edits to implement use of ZZGFRELX.
!        These edits include removal of unneeded routines:
!
!           ZZGFRRUR
!           ZZGFRRLT
!
!        and corresponding unused variables.
!
!        Update to header entries.
!
!-    SPICELIB version 1.0.1 08-JUL-2010 (EDW)
!
!        Minor typo corrections to headers.
!
!-    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW)
!
!-&

! $ Index_Entries
!
!     find range rate events
!
!-&

      external              udfunc

!
!     SPICELIB functions
!
      double precision      vdot

      logical               failed
      logical               return



!
!     Local Variables
!
      double precision      lt
      double precision      drvel
      double precision      srhat  ( 6 )
      double precision      state  ( 6 )
      double precision      dfdt   ( 6 )
      double precision      states ( 6, 2 )

      integer               n

      logical               attblk ( nabcor )
      logical               found


!
!     Saved Variables
!
      character*(corlen)    svabco
      save                  svabco

      integer               svtarg
      save                  svtarg

      character*(32)        svref
      save                  svref

      integer               svobs
      save                  svobs

      double precision      svdt
      save                  svdt

      call chkin  ( 'ZZGFRRU'           )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      call chkout ( 'ZZGFRRU'           )

      return



!$Procedure ZZGFRRIN ( Private --- GF, range rate initialization )

      entry zzgfrrin ( target, abcorr, obsrvr, dt )

! $ Abstract
!
!     This is the initialization entry point used for describing
!     the event that is to be solved for by ZZGFSOLV.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     RANGE RATE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!      CHARACTER*(*)         TARGET
!      CHARACTER*(*)         ABCORR
!      CHARACTER*(*)         OBSRVR
!      DOUBLE PRECISION      DT
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TARGET     I   Name of the target body
!     ABCORR     I   Aberration correction flag
!     OBSRVR     I   Name of the observing body
!     DT         I   Interval from ET for derivative calculation.
!
! $ Detailed_Input
!
!     TARGET   the string name of a target body.  Optionally, you may
!              supply the integer ID code for the object as an
!              integer string.  For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!              The target and observer define a position vector
!              that points from the observer to the target.
!
!     ABCORR   the string description of the aberration corrections to
!              apply to the state evaluations to account for one-way
!              light time and stellar aberration.
!
!              Any aberration correction accepted by the SPICE
!              routine SPKEZR is accepted here. See the header
!              of SPKEZR for a detailed description of the
!              aberration correction options. For convenience,
!              the options are listed below:
!
!                 'NONE'     Apply no correction. Returns the "true"
!                            geometric state.
!
!                 'LT'       "Reception" case:  correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'LT+S'     "Reception" case:  correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'CN'       "Reception" case:  converged
!                            Newtonian light time correction.
!
!                'CN+S'     "Reception" case:  converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!                 'XLT'      "Transmission" case:  correct for
!                            one-way light time using a Newtonian
!                            formulation.
!
!                 'XLT+S'    "Transmission" case:  correct for
!                            one-way light time and stellar
!                            aberration using a Newtonian
!                            formulation.
!
!                 'XCN'      "Transmission" case:  converged
!                            Newtonian light time correction.
!
!                 'XCN+S'    "Transmission" case:  converged
!                            Newtonian light time and stellar
!                            aberration corrections.
!
!              The ABCORR string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     OBSRVR   the string name of an observing body.  Optionally, you
!              may supply the ID code of the object as an integer
!              string. For example, both 'EARTH' and '399' are
!              legitimate strings to indicate the observer as Earth.
!
!     DT         a scalar double precision value representing half the
!                interval in TDB seconds separating the evaluation
!                epochs; the evaluations occur at epochs
!                (ET + DT) and (ET - DT).
!
!                DT may be negative but must be non-zero.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(IDCODENOTFOUND) signals if the object name for the
!        target, TARGET, is not a recognized name.
!
!     2) SPICE(IDCODENOTFOUND) signals if the object name for the
!        observer, OBSRVR, is not a recognized name.
!
!     3) SPICE(BODIESNOTDISTINCT) signals if the two objects
!        associated with a range rate search are not distinct.
!
!     4) SPICE(INVALIDVALUE) signals for the delta, DT, equal to zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
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
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     L.S. Elson     (JPL)
!
! $ Version
!
!-    SPICELIB version 2.0.0 18-SEP-2012 (EDW)
!
!        Added proper Exceptions section.
!
!        Added error check on value of DT to ensure non-zero value.
!
!        REFVAL removed from routine argument list due to the use
!        of ZZGFRELX to calculate the events.
!
!        Minor typo correction to header.
!
!-    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW)
!
!-&

! $ Index_Entries
!
!     range rate initialization routine.
!
!-&

      if ( return() ) then
         return
      end if

      call chkin ( 'ZZGFRRIN' )

!
!     Find NAIF IDs for TARGET and OBSRVR.
!
      call bods2c ( target, svtarg, found )

      if ( .not. found ) then

         call setmsg ( 'The target object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         call errch  ( '#', target                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFRRIN'                                 )
         return

      end if


      call bods2c ( obsrvr, svobs, found )

      if ( .not. found ) then

         call setmsg ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         call errch  ( '#', obsrvr                                )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         call chkout ( 'ZZGFRRIN'                                 )
         return

      end if

!
!     Make sure the observer and target are distinct.
!
      if ( svtarg .eq. svobs ) then

         call setmsg ( 'The observer and target must be ' &
         //            'distinct objects, but are not: ' &
         //            'OBSRVR = #; TARGET = #.'           )
         call errch  ( '#', obsrvr                         )
         call errch  ( '#', target                         )
         call sigerr ( 'SPICE(BODIESNOTDISTINCT)'          )
         call chkout ( 'ZZGFRRIN'                          )
         return

      end if

!
!     Squeeze all blanks out of the aberration correction
!     string; ensure the string is in upper case.
!
      call cmprss ( ' ', 0, abcorr, svabco )
      call ucase  ( svabco,         svabco )

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      call zzvalcor ( svabco, attblk )

      if ( failed() ) then

         call chkout ( 'ZZGFRRIN' )
         return

      end if

!
!     The "delta" argument for QDERIV, DT, must have a non-zero value.
!
      if ( dt .eq. 0.d0 ) then

         call setmsg ( 'Delta value for QDERIV is zero; a non-zero ' // &
                       'value is required.'                        )
         call sigerr ( 'SPICE(INVALIDVALUE)'                       )
         call chkout ( 'ZZGFRRIN' )
         return

      end if

!
!     Save the DT value.
!
      svref  = 'J2000'
      svdt   = dt

      call chkout ( 'ZZGFRRIN' )
      return




!$Procedure ZZGFRRDC (  Private --- GF, when range rate is decreasing )

      entry zzgfrrdc ( udfunc, et, decres )

! $ Abstract
!
!     Computes whether or not the range rate between the observer
!     and the target is decreasing at time ET.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     RANGE RATE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!     LOGICAL               DECRES
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     DECRES     O   true if range rate is decreasing, false
!                    otherwise.
!
! $ Detailed_Input
!
!     ET         time in seconds past J2000 at which to calculate
!                whether the range rate of the observer-target vector
!                is decreasing.
!
! $ Detailed_Output
!
!     DECRES     is true if the range rate between the objects
!                is decreasing, false otherwise.
!
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     A function f(x) is strictly decreasing at x0 if and only if there
!     exists some delta > 0 such that for all dx satisfying
!
!        0  <  dx  < delta
!
!     we have
!
!        f(x0)       <  f(x0 + dx)
!
!     and
!
!        f(x0 - dx)  <  f(x)
!
!     Note that a strictly decreasing function need not be
!     differentiable in a neighborhood of x0; it can have jump
!     discontinuities in any neighborhood of x0 and even at x0.
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
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     L.S. Elson     (JPL)
!
! $ Version
!
!-    SPICELIB version 2.0.0 18-FEB-2011 (EDW)
!
!        Added UDFUNC to argument list for use of ZZGFRELX when
!        calculating the events.
!
!        Minor typo correction to comments.
!
!-    SPICELIB version 1.0.0 09-OCT-2009 (LSE)(EDW)
!
!-&

! $ Index_Entries
!
!     when range rate is decreasing
!
!-&

!
!     Standard SPICE error handling.
!

      if ( return () ) then
         return
      end if

      call chkin  ( 'ZZGFRRDC' )

      n  = 6

!
!     The range rate of interest is of SVTARG relative to the SVOBS.
!     The function requires the acceleration of SVTARG relative
!     to SVOBS.
!

      call spkez (svtarg, et-svdt, svref, svabco, svobs, &
                                                  states(1,1), lt)
      call spkez (svtarg, et+svdt, svref, svabco, svobs, &
                                                  states(1,2), lt)

!
!     Approximate the derivative of the position and velocity by
!     finding the derivative of a quadratic approximating function.
!
!        DFDT(1) = Vx
!        DFDT(2) = Vy
!        DFDT(3) = Vz
!        DFDT(4) = Ax
!        DFDT(5) = Ay
!        DFDT(6) = Az
!
      call qderiv ( n, states(1,1), states(1,2), svdt, dfdt )

      call spkez ( svtarg, et, svref, svabco, svobs, state, lt )

      if (  failed() ) then
         call chkout (  'ZZGFRRDC' )
         return
      end if

!
!        d ||r||     ^
!        ------- = < r, v >
!        dt
!
!         2           ^          ^
!        d ||r||   < dr, v > + < r, dv >
!        ------- =   --             --
!          2
!        dt          dt             dt
!
      call dvhat ( state, srhat )

      drvel  =  vdot( dfdt(4), srhat ) + vdot( state(4), srhat(4) )
      decres =  drvel .lt. 0.d0

      call chkout ( 'ZZGFRRDC' )
      return




!$Procedure ZZGFRRGQ ( Private --- GF, get range rate between bodies )

      entry zzgfrrgq ( et, rvl )

! $ Abstract
!
!     Determine the range rate between the centers of the two
!     bodies.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     RANGE RATE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!      DOUBLE PRECISION      ET
!      DOUBLE PRECISION      RVL
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Ephemeris seconds past J2000 TDB.
!     RVL        O   Range rate at time ET.
!
! $ Detailed_Input
!
!     ET         time in ephemeris seconds past J2000 when the range
!                rate between the two bodies is to be computed.
!
! $ Detailed_Output
!
!     RVL        is the range rate of SVTARG as seen from SVOBS at
!                time ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
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
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!     L.S. Elson     (JPL)
!
! $ Version
!
!-    SPICELIB version 1.0.0 09-JUN-2009 (LSE)(EDW)
!
!-&

! $ Index_Entries
!
!     get range rate between two bodies
!
!-&

      call zzgfrrq ( et, svtarg, svobs, svabco, rvl )

      return




!$Procedure ZZGFRRX ( Private -- GF, retrieve ZZGFRRIN values )

      entry zzgfrrx ( xtarg, xabcor, xobs, xdt )

! $ Abstract
!
!     Retrieve values set in ZZGFRRIN.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations
!
!     None.
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     XTARG      O   Saved value for the target body.
!     XABCOR     O   Saved value for the aberration correction.
!     XOBS       O   Saved value for the observing body.
!     XDT        O   Saved value for DT.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     XTARG    SPICE ID value for the target body initialized
!              via ZZGFRRIN.
!
!     XABCOR   String value for the aberration correction initialized
!              via ZZGFRRIN.
!
!     XOBS     SPICE ID value for the observing body initialized
!              via ZZGFRRIN.
!
!     XDT      Saved value for DT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
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
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 1.0.0 16-SEP-2012 (EDW)
!
!-&

! $ Index_Entries
!
!     get saved range rate parameters
!
!-&

         xtarg  = svtarg
         xabcor = svabco
         xobs   = svobs
         xdt    = svdt

      return
      end
