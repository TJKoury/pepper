!$Procedure ZZGFPAU ( Private --- GF, phase angle utility routine )

      SUBROUTINE ZZGFPAU ( TARGET, ILLMN,  ABCORR, OBSRVR, &
                           UDFUNC, ET,     DECRES, RVL, &
                           XTARG,  XILLMN, XABCOR, XOBS, XABLK )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     This is the umbrella routine for the three entry points needed by
!     GFEVNT in order to find body centered phase angle events.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      CHARACTER*(*)         TARGET
      CHARACTER*(*)         ILLMN
      CHARACTER*(*)         ABCORR
      CHARACTER*(*)         OBSRVR
      DOUBLE PRECISION      ET
      LOGICAL               DECRES
      DOUBLE PRECISION      RVL
      INTEGER               XTARG
      INTEGER               XILLMN
      CHARACTER*(*)         XABCOR
      INTEGER               XOBS
      LOGICAL               XABLK  ( NABCOR )


! $ Brief_I/O
!
!     VARIABLE  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     TARGET    I-O  ZZGFPAIN, ZZGFPAX
!     ILLMN     I-O  ZZGFPAIN, ZZGFPAX
!     ABCORR    I-O  ZZGFPAIN, ZZGFPAX
!     OBSRVR    I-O  ZZGFPAIN, ZZGFPAX
!     ET         I   ZZGFPADC, ZZGFPAGQ
!     UDFUNC     I   ZZGFPADC
!     DECRES     O   ZZGFPADC
!     RVL        O   ZZGFPAGQ
!     XTARG      O   ZZGFPAX
!     XILLMN     O   ZZGFPAX
!     XABCOR     O   ZZGFPAX
!     XOBS       O   ZZGFPAX
!     XABLK      O   ZZGFPAX
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
!     ILLMN    the string name of the illuminating body. This will
!              normally be 'SUN' but the algorithm can use any
!              ephemeris object
!
!     ABCORR   the string description of the aberration corrections to
!              apply to the state evaluations to account for one-way
!              light time and stellar aberration.
!
!              Any signal receive aberration correction accepted by
!              the SPICE routine SPKEZR is accepted here. See the header
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
!              The ABCORR string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     OBSRVR   the string name of an observing body.  Optionally, you
!              may supply the ID code of the object as an integer
!              string. For example, both 'EARTH' and '399' are
!              legitimate strings to indicate the observer as Earth.
!
!     ET       the time in TDB seconds past J2000 at which to calculate
!              the value of or characteristic of the phase angle
!              between SVTARG and SVILLMN as seen from SVOBS.
!
!     For more information, see individual entry points.
!
! $ Detailed_Output
!
!     DECRES   is .TRUE. if the phase angle is decreasing at ET.
!              Otherwise it is .FALSE..
!
!     RVL      the phase angle between SVTARG and SVILLMN as seen
!              from SVOBS at time ET.
!
!     XTARG    SPICE ID value for the target body initialized
!              via ZZGFPAIN.
!
!     XILLMN   SPICE ID value for the illuminator body initialized
!              via ZZGFPAIN.
!
!     XABCOR   String value for the aberration correction initialized
!              via ZZGFPAIN.
!
!     XOBS     SPICE ID value for the observing body initialized
!              via ZZGFPAIN.
!
!     XABLK    Aberration correction attribute block as initialized
!              by ABCORR.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) SPICE(BOGUSENTRY) signals if a direct call to ZZGFPAU occurs.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine serves as the umbrella routine for three entry points
!     needed by GFEVNT in solving for phase angle conditions.
!
!     The three entry points are
!
!        ZZGFPAIN --- an initialization routine that must be called
!                     prior to attempting to solve for any range
!                     rate event.
!
!        ZZGFPADC --- determines whether or not phase angle is
!                     decreasing at some time.
!
!        ZZGFPAGQ --- returns the phase angle of the two objects
!                     of concern as a function of ET.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     ZZGFPAIN must be called prior to use of any of the other
!     entry points (think constructor).
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     E.D. Wright    (JPL)
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    SPICELIB version 1.0.0 16-SEP-2012 (EDW) (NJB)
!
!-&

! $ Index_Entries
!
!     find phase angle events
!
!-&

      EXTERNAL              UDFUNC

!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      DVSEP
      DOUBLE PRECISION      VDOT


!
!     Local parameters
!

!
!     The phase angle calculation is invariant with respect to
!     reference frame. Use J2000 for convenience.
!
      CHARACTER*(*)         REF
      PARAMETER           ( REF    = 'J2000' )


!
!     Local Variables
!
      CHARACTER*(CORLEN)    SVABCO

      DOUBLE PRECISION      DSEP
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      S1   (6)
      DOUBLE PRECISION      S2   (6)
      DOUBLE PRECISION      UVEC (3)

      INTEGER               SVILLM
      INTEGER               SVOBS
      INTEGER               SVTARG
      INTEGER               I

      LOGICAL               SVABLK ( NABCOR )
      LOGICAL               FOUND


!
!     Saved Variables
!
      SAVE                  SVABLK
      SAVE                  SVABCO
      SAVE                  SVTARG
      SAVE                  SVOBS
      SAVE                  SVILLM

      CALL CHKIN  ( 'ZZGFPAU'           )
      CALL SIGERR ( 'SPICE(BOGUSENTRY)' )
      CALL CHKOUT ( 'ZZGFPAU'           )

      RETURN



!$Procedure  ZZGFPAIN ( Private --- GF, phase angle initialization )

      ENTRY ZZGFPAIN ( TARGET, ILLMN, ABCORR, OBSRVR )

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
!     PHASE ANGLE
!     EPHEMERIS
!     GEOMETRY
!     SEARCH
!
! $ Declarations
!
!      CHARACTER*(*)         TARGET
!      CHARACTER*(*)         ABCORR
!      CHARACTER*(*)         OBSRVR
!
! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     TARGET     I   Name of the target body.
!     ILLMN      I   Name of the illuminating body.
!     ABCORR     I   Aberration correction flag.
!     OBSRVR     I   Name of the observing body.
!
! $ Detailed_Input
!
!     TARGET   the string name of a target body.  Optionally, you may
!              supply the integer ID code for the object as an
!              integer string.  For example both 'MOON' and '301'
!              are legitimate strings that indicate the moon is the
!              target body.
!
!              Case and leading or trailing blanks are not significant
!              in the string TARGET.
!
!     ILLMN    the string name of the illuminating body. This will
!              normally be 'SUN' but the algorithm can use any
!              ephemeris object
!
!              Case and leading or trailing blanks are not significant
!              in the string ILLMN.
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
!              Note that this routine accepts only reception mode
!              aberration corrections.
!
!              The ABCORR string lacks sensitivity to case, leading
!              and trailing blanks.
!
!     OBSRVR   the string name of an observing body.  Optionally, you
!              may supply the ID code of the object as an integer
!              string. For example, both 'EARTH' and '399' are
!              legitimate strings to indicate the observer as Earth.
!
!              Case and leading or trailing blanks are not significant
!              in the string OBSRVR.
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
!        illuminator, ILLMN, is not a recognized name.
!
!     3) SPICE(IDCODENOTFOUND) signals if the object name for the
!        observer, OBSRVR, is not a recognized name.
!
!     4) SPICE(BODIESNOTDISTINCT) signals if the three objects
!        associated with a PHASE ANGLE search are not distinct.
!
!     5) SPICE(INVALIDOPTION) signals for any transmit mode aberration
!        correction.
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
!-    SPICELIB version 1.0.0 19-OCT-2010 (EDW)(NJB)
!
!-&

! $ Index_Entries
!
!     phase angle initialization routine
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFPAIN' )

!
!     Find NAIF IDs for TARGET, ILLMN, and OBSRVR.
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
         CALL CHKOUT ( 'ZZGFPAIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( ILLMN, SVILLM, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The illuminator object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFPAIN'                                 )
         RETURN

      END IF


      CALL BODS2C ( OBSRVR, SVOBS, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The observer object, ' &
         //            '''#'', is not a recognized name for an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE toolkit. '           )
         CALL ERRCH  ( '#', OBSRVR                                )
         CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'                    )
         CALL CHKOUT ( 'ZZGFPAIN'                                 )
         RETURN

      END IF


!
!     Make sure the observer, illuminator, and target are distinct.
!
      IF ( (SVTARG .EQ. SVOBS)   .OR. &
           (SVTARG .EQ. SVILLM) .OR. &
           (SVOBS  .EQ. SVILLM)      ) THEN

         CALL SETMSG ( 'The observer, illuminator, and ' &
         //            'target must be distinct objects, but ' &
         //            'are not: OBSRVR = #, TARGET = #, ' &
         //            'are not: ILLMN= #.' )
         CALL ERRCH  ( '#', OBSRVR                         )
         CALL ERRCH  ( '#', TARGET                         )
         CALL ERRCH  ( '#', ILLMN                          )
         CALL SIGERR ( 'SPICE(BODIESNOTDISTINCT)'          )
         CALL CHKOUT ( 'ZZGFPAIN'                          )
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
      CALL ZZVALCOR ( SVABCO, SVABLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFPAIN' )
         RETURN
      END IF

!
!     Restrict correction to reception cases.
!

      IF( SVABLK(XMTIDX) ) THEN

         CALL SETMSG ( 'Invalid aberration correction ''#''. ' &
         //            'Phase angle geometry calculations currently ' &
         //            'restricted to reception cases.')
         CALL ERRCH  ( '#', ABCORR )
         CALL SIGERR ( 'SPICE(INVALIDOPTION)' )
         CALL CHKOUT ( 'ZZGFPAIN' )
         RETURN

      END IF


      CALL CHKOUT ( 'ZZGFPAIN' )
      RETURN




!$Procedure ZZGFPADC ( Private --- GF, when phase angle is decreasing )

      ENTRY ZZGFPADC ( UDFUNC, ET, DECRES )

! $ Abstract
!
!     Computes whether or not the phase angle between the observer
!     and the target is decreasing at time ET.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
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
!     DECRES     O   .TRUE. if phase angle is decreasing, .FALSE.
!                    otherwise.
!
! $ Detailed_Input
!
!     ET         the time in ephemeris seconds past J2000 TDB at which
!                to determine whether the phase angle between SVTARG
!                and SVILLMN as seen from SVOBS is decreasing.
!
! $ Detailed_Output
!
!     DECRES     is .TRUE. if the phase angle is decreasing at ET.
!                Otherwise it is .FALSE..
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
!     N.J. Bachman   (JPL)
!     E.D. Wright    (JPL)
!
! $ Version
!
!-    SPICELIB version 1.0.0 10-JAN-2012 (EDW)
!
!-&

! $ Index_Entries
!
!     when phase angle is decreasing
!
!-&

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN  ( 'ZZGFPADC' )


!
!     Get the state of the TARG object relative to OBS at ET.
!
      CALL SPKEZ ( SVTARG, ET, REF, SVABCO, SVOBS, S1, LT )

      IF  (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFPADC' )
         RETURN
      END IF


!
!     Get the state of the ILLMN object relative to TARG at ET
!     for no aberration correction, or ET - LT otherwise.
!
      IF ( SVABLK(GEOIDX) ) THEN

!
!        No correction, geometric.
!
         CALL SPKEZ ( SVILLM, ET,      REF, SVABCO, SVTARG, S2, LT )

      ELSE

         CALL SPKEZ ( SVILLM, ET - LT, REF, SVABCO, SVTARG, S2, LT )

         IF (  FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFPADC' )
            RETURN
         END IF

!
!        Correct velocity for time derivative of the observer target
!        light-time. We need to do this since the SPK evaluation occurs
!        at ET - LT.
!
!        d( ET - LT ) = (1 - d LT )
!        ------------        ----
!        dt                  dt
!
!        LT = ||R||
!             -----
!               C
!                                     ^
!        d LT    = 1   d ||R||  = 1 < R, V >
!        ----      -   -------    -
!        dt        C   dt         C
!
!
         CALL VHAT ( S1, UVEC )

         DLT = VDOT ( UVEC, S1(4) ) / CLIGHT()

!
!        Apply the correction to the velocity vector components.
!
         CALL VSCLIP ( 1.D0 - DLT,  S2(4) )

      END IF

      IF (  FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFPADC' )
         RETURN
      END IF

!
!                       ILLMN      OBS
!       ILLMN as seen      *       /
!       from TARG at       |      /
!       ET - LT.           |     /
!                         >|..../< phase angle
!                          |   /
!                        . |  /
!                      .   | /
!                     .     *     TARG as seen from OBS
!               SEP   .   TARG    at ET
!                      .  /
!                        /
!                       *
!
!     Calculate the derivative of the angle separating the vectors
!     relative to TARG.
!
!        PI = SEP + PHASE
!
!        dPHASE     dSEP
!        ------ = - ----
!         dt         dt
!
      DSEP   = DVSEP( S1, S2 )

      DECRES = -DSEP .LT. 0.D0

      CALL CHKOUT ( 'ZZGFPADC' )
      RETURN




!$Procedure ZZGFPAGQ ( Private --- GF, phase angle between two bodies )

      ENTRY ZZGFPAGQ ( ET, RVL )

! $ Abstract
!
!     Determine the phase angle between the centers of the two
!     bodies.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PHASE ANGLE
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
!     RVL        O   Phase angle at time ET.
!
! $ Detailed_Input
!
!     ET         the time in ephemeris seconds past J2000 TDB at which
!                to compute the phase angle.
!
! $ Detailed_Output
!
!     RVL        the phase angle between SVTARG and SVILLMN as seen
!                from SVOBS at time ET.
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
!-    SPICELIB version 1.0.0 19-OCT-2010 (EDW)
!
!-&

! $ Index_Entries
!
!     get phase angle between two bodies
!
!-&

      CALL ZZGFPAQ ( ET, SVTARG, SVILLM, SVOBS, SVABCO, RVL )

      RETURN




!$Procedure ZZGFPAX ( Private -- GF, retrieve ZZGFPAIN values )

      ENTRY ZZGFPAX ( XTARG, XILLMN, XABCOR, XOBS, XABLK )

! $ Abstract
!
!     Retrieve values set in ZZGFPAIN.
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
!     XILLMN     O   Saved value for the illuminator body.
!     XABCOR     O   Saved value for the aberration correction.
!     XOBS       O   Saved value for the observing body.
!     XABLK      O   Saved value for the aberration correction
!                    attribute block.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     XTARG    SPICE ID value for the target body initialized
!              via ZZGFPAIN.
!
!     XILLMN   SPICE ID value for the illuminator body initialized
!              via ZZGFPAIN.
!
!     XABCOR   String value for the aberration correction initialized
!              via ZZGFPAIN.
!
!     XOBS     SPICE ID value for the observing body initialized
!              via ZZGFPAIN.
!
!     XABLK    Aberration correction attribute block as initialized
!              by ABCORR.
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
!     get saved phase angle parameters
!
!-&

         XTARG  = SVTARG
         XILLMN = SVILLM
         XABCOR = SVABCO
         XOBS   = SVOBS

         DO I=1, ABATSZ
            XABLK(I) = SVABLK(I)
         END DO

      RETURN
      END


