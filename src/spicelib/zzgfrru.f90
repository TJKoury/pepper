!$Procedure ZZGFRRU ( Private --- GF, range rate utility routine )

      SUBROUTINE ZZGFRRU ( TARGET, ABCORR, OBSRVR, DT, &
                           UDFUNC, ET,     DECRES, RVL, &
                           XTARG, XABCOR, XOBS, XDT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the umbrella routine for the entry points needed by
!     GFEVNT in order to find range rate events.
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
!     RANGE RATE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      DT
      LOGICAL               DECRES
      DOUBLE PRECISION      RVL
      INTEGER               XTARG
      CHARACTER*(*)         XABCOR
      INTEGER               XOBS
      DOUBLE PRECISION      XDT

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

      EXTERNAL              UDFUNC

!
!     SPICELIB functions
!
      DOUBLE PRECISION      VDOT

      LOGICAL               FAILED
      LOGICAL               RETURN



!
!     Local Variables
!
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      DRVEL
      DOUBLE PRECISION      SRHAT  ( 6 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      DFDT   ( 6 )
      DOUBLE PRECISION      STATES ( 6, 2 )

      INTEGER               N

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               FOUND


!
!     Saved Variables
!
      CHARACTER*(CORLEN)    SVABCO
      SAVE                  SVABCO

      INTEGER               SVTARG
      SAVE                  SVTARG

      CHARACTER*(32)        SVREF
      SAVE                  SVREF

      INTEGER               SVOBS
      SAVE                  SVOBS

      DOUBLE PRECISION      SVDT
      SAVE                  SVDT

      CALL CHKIN  ( 'ZZGFRRU'           )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZGFRRU'           )

      RETURN



!$Procedure ZZGFRRIN ( Private --- GF, range rate initialization )

      ENTRY ZZGFRRIN ( TARGET, ABCORR, OBSRVR, DT )

! $ Abstract
!
!     This is the initialization entry point used for describing
!     the event that is to be solved for by ZZGFSOLV.
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

      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFRRIN' )

!
!     Find NAIF IDs for TARGET and OBSRVR.
!
      CALL BODS2C ( TARGET, SVTARG, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The target object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         CALL ERRCH  ( '#', TARGET                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFRRIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( OBSRVR, SVOBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFRRIN'                                 )
         RETURN

      END IF

!
!     Make sure the observer and target are distinct.
!
      IF ( SVTARG .EQ. SVOBS ) THEN

         CALL SETMSG ( 'The observer and target must be ' &
         //            'distinct objects, but are not: ' &
         //            'OBSRVR = #; TARGET = #.'           )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'          )
         CALL CHKOUT ( 'ZZGFRRIN'                          )
         RETURN

      END IF

!
!     Squeeze all blanks out of the aberration correction
!     string; ensure the string is in upper case.
!
      CALL CMPRSS ( ' ', 0, ABCORR, SVABCO )
      CALL UCASE  ( SVABCO,         SVABCO )

!
!     Check the aberration correction. If SPKEZR can't handle it,
!     neither can we.
!
      CALL ZZVALCOR ( SVABCO, ATTBLK )

      IF ( FAILED() ) THEN

         CALL CHKOUT ( 'ZZGFRRIN' )
         RETURN

      END IF

!
!     The "delta" argument for QDERIV, DT, must have a non-zero value.
!
      IF ( DT .EQ. 0.D0 ) THEN

         CALL SETMSG ( 'Delta value for QDERIV is zero; a non-zero ' // &
                       'value is required.'                        )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'                       )
         CALL CHKOUT ( 'ZZGFRRIN' )
         RETURN

      END IF

!
!     Save the DT value.
!
      SVREF  = 'J2000'
      SVDT   = DT

      CALL CHKOUT ( 'ZZGFRRIN' )
      RETURN




!$Procedure ZZGFRRDC (  Private --- GF, when range rate is decreasing )

      ENTRY ZZGFRRDC ( UDFUNC, ET, DECRES )

! $ Abstract
!
!     Computes whether or not the range rate between the observer
!     and the target is decreasing at time ET.
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

      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFRRDC' )

      N  = 6

!
!     The range rate of interest is of SVTARG relative to the SVOBS.
!     The function requires the acceleration of SVTARG relative
!     to SVOBS.
!

      CALL SPKEZ (SVTARG, ET-SVDT, SVREF, SVABCO, SVOBS, &
                                                  STATES(1,1), LT)
      CALL SPKEZ (SVTARG, ET+SVDT, SVREF, SVABCO, SVOBS, &
                                                  STATES(1,2), LT)

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
      CALL QDERIV ( N, STATES(1,1), STATES(1,2), SVDT, DFDT )

      CALL SPKEZ ( SVTARG, ET, SVREF, SVABCO, SVOBS, STATE, LT )

      IF (  FAILED() ) THEN
         CALL CHKOUT (  'ZZGFRRDC' )
         RETURN
      END IF

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
      CALL DVHAT ( STATE, SRHAT )

      DRVEL  =  VDOT( DFDT(4), SRHAT ) + VDOT( STATE(4), SRHAT(4) )
      DECRES =  DRVEL .LT. 0.D0

      CALL CHKOUT ( 'ZZGFRRDC' )
      RETURN




!$Procedure ZZGFRRGQ ( Private --- GF, get range rate between bodies )

      ENTRY ZZGFRRGQ ( ET, RVL )

! $ Abstract
!
!     Determine the range rate between the centers of the two
!     bodies.
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

      CALL ZZGFRRQ ( ET, SVTARG, SVOBS, SVABCO, RVL )

      RETURN




!$Procedure ZZGFRRX ( Private -- GF, retrieve ZZGFRRIN values )

      ENTRY ZZGFRRX ( XTARG, XABCOR, XOBS, XDT )

! $ Abstract
!
!     Retrieve values set in ZZGFRRIN.
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

         XTARG  = SVTARG
         XABCOR = SVABCO
         XOBS   = SVOBS
         XDT    = SVDT

      RETURN
      END
