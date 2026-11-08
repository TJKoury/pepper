!$Procedure      T_GETSA ( Test utility, get stellar aberration )

      SUBROUTINE T_GETSA ( TARG, ET, REF, ABCORR, OBS, PCORR, DPCORR )
      IMPLICIT NONE

! $ Abstract
!
!     Return the stellar aberration offset vector and the time
!     derivative of this offset for specified SPKEZR inputs.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     TEST
!     UTILITY
!
! $ Declarations

      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      INTEGER               OBS
      DOUBLE PRECISION      PCORR  ( 3 )
      DOUBLE PRECISION      DPCORR ( 3 )

      INCLUDE 'zzabcorr.inc'

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TARG       I   Target body ID code.
!     ET         I   Epoch.
!     REF        I   Reference frame name.
!     ABCORR     I   Aberration correction.
!     OBS        I   Observer ID code.
!     PCORR      O   Stellar aberration offset.
!     DPCORR     O   Velocity of stellar aberration offset.
!
! $ Detailed_Input
!
!     TARG           is the NAIF ID code of the target body.
!
!     ET             is the time at which to evaluate the state vector
!                    with which the specified stellar aberration
!                    correction is associated. ET is expressed as
!                    seconds past J2000 TDB.
!
!     REF            is the reference frame in which to compute a state
!                    and corresponding stellar aberration correction.
!
!                    REF need not be inertial.
!
!     ABCORR         is an aberration correction specification
!                    string. Any value acceptable to SPKEZ
!                    may be used.
!
!     OBS            is the NAIF ID of the observer body.
!
! $ Detailed_Output
!
!     PCORR       is the stellar aberration correction for the position
!                 component of the state defined by the input
!                 arguments. Adding PCORR to this position vector
!                 produces the input observer-target position,
!                 corrected for stellar aberration.
!
!                 Units are km.
!
!     DPCORR      is the stellar aberration correction for the velocity
!                 component of the state defined by the input
!                 arguments. Adding DPCORR to this velocity vector
!                 produces the input observer-target velocity,
!                 corrected for stellar aberration.
!
!                 Units are km/s.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) Any exceptions incurred by ZZPRSCOR may be incurred
!        by this routine.
!
!     2) Any exceptions incurred by SPKLTC may be incurred
!        by this routine.
!
!     3) Any exceptions incurred by ZZSTELAB may be incurred
!        by this routine.
!
! $ Files
!
!     See the Files section of the header of SPKEZ.
!
! $ Particulars
!
!     This routine mimics the aberration correction logic of SPKACS: the
!     acceleration of the observer with respect to the solar system
!     barycenter is computed, an optionally light time corrected state
!     is computed using SPKLTC, then this state and the acceleration of
!     the observer with respect to the solar system barycenter are used
!     as inputs to the stellar aberration correction computation.
!
! $ Examples
!
!     See the test utility package T_TRAJFN for usage
!     examples.
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
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 29-DEC-2007 (NJB)
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED

!
!     Local parameters
!
      INTEGER               SSB
      PARAMETER           ( SSB    = 0 )

!
!     Local variables
!
      DOUBLE PRECISION      ACC    ( 3 )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DLTCTR
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTCENT
      DOUBLE PRECISION      LTSIGN
      DOUBLE PRECISION      LTSSB
      DOUBLE PRECISION      SASTAT ( 6 )
      DOUBLE PRECISION      SSBOBS ( 6 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STCTR  ( 6 )
      DOUBLE PRECISION      STOBS  ( 6, 2 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      XFORM  ( 6, 6 )
      DOUBLE PRECISION      XSTATE ( 6 )

      INTEGER               CENTER
      INTEGER               I
      INTEGER               REQFRM
      INTEGER               TYPE
      INTEGER               TYPEID

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND
      LOGICAL               XMIT


      CALL CHKIN ( 'T_GETSA' )

!
!     Compute the corrections in the J2000 frame. We'll
!     transform them later to the requested frame.
!
      CALL ZZPRSCOR ( ABCORR, ATTBLK )
      XMIT = ATTBLK(XMTIDX)

      DELTA = 1.D0

!
!     Determine the acceleration of the observer with
!     respect to the solar system barycenter at ET.
!     Use a quadratic estimate.
!
      DO I = 1, 2

         T = ET + (2*I - 3)*DELTA

         CALL SPKGEO ( OBS, T, 'J2000', SSB, STOBS(1,I), LTSSB )

      END DO

      CALL QDERIV ( 3, STOBS(4,1), STOBS(4,2), DELTA, ACC )

!
!     Get the light-time corrected state of the target with respect
!     to the observer. Also get the geometric state of the observer
!     with respect to the solar system barycenter at ET.
!
      CALL SPKSSB ( OBS, ET,    'J2000', SSBOBS )

      CALL SPKLTC ( TARG,    ET,   'J2000', ABCORR, &
                    SSBOBS,  STATE, LT,     DLT    )

!
!     Get the stellar aberration correction and its time derivative.
!     Note that the input observer state relative to the solar system
!     barycenter was obtained from SPKLTC.
!
      CALL ZZSTELAB ( XMIT, ACC, SSBOBS(4), STATE, PCORR, DPCORR )

!
!     Get the transformation from J2000 to the request frame. This
!     transformation should be evaluated at the light-time corrected
!     epoch associated with the center of the request frame.
!
      CALL NAMFRM ( REF, REQFRM )

      IF ( REQFRM .EQ. 0 ) THEN

         CALL SETMSG ( 'The requested output frame ''#'' is ' &
         //            'not recognized by the reference frame ' &
         //            'subsystem.  Please check that the ' &
         //            'appropriate kernels have been loaded ' &
         //            'and that you have correctly entered ' &
         //            'the name of the output frame. '       )
         CALL ERRCH  ( '#', REF                               )
         CALL SIGERR ( 'SPICE(UNKNOWNFRAME)'                  )
         CALL CHKOUT ( 'T_GETSA'                              )
         RETURN

      END IF

!
!     If we reach this point, FOUND will be .TRUE. after
!     the following call to FRINFO:
!
      CALL FRINFO ( REQFRM, CENTER, TYPE, TYPEID, FOUND )

!
!     Get light time to the frame's center.
!
      IF ( CENTER .EQ. OBS ) THEN

         LTCENT = 0.0D0
         DLTCTR = 0.0D0

      ELSE IF ( CENTER .EQ. TARG ) THEN

         LTCENT = LT
         DLTCTR = DLT

      ELSE

         CALL SPKLTC ( CENTER, ET,    'J2000', ABCORR, &
                       SSBOBS, STCTR, LTCENT,  DLTCTR )
      END IF
!
!        If something went wrong (like we couldn't get the state of
!        the center relative to the observer) now it is time to quit.
!
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_GETSA' )
         RETURN
      END IF

!
!     If the aberration corrections are for transmission, make the sign
!     of the light time positive, since we wish to compute the
!     orientation of the non-inertial frame at an epoch later than ET
!     by the one-way light time.
!
      IF ( XMIT ) THEN

         LTSIGN =  1
      ELSE
         LTSIGN = -1
      END IF

!
!     Look up the transformation from J2000 to REF at the light time
!     corrected epoch.
!
      CALL SXFORM ( 'J2000', REF, ET + LTSIGN*LTCENT, XFORM )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_GETSA' )
         RETURN
      END IF

!
!     The frame rotation we wish to apply is of the form
!
!        R(t)
!
!     where
!
!        t = ET + LTSIGN*LTCENT(ET)
!
!     since the light time from the frame center to the
!     observer LTCENT is a function of ET. So
!
!                              |
!        d(R)/d(ET) = d(R)/d(t)|                  * (1 + d(t)/d(ET))
!                              |t=ET+LTSIGN*LTCENT
!
!
!                              |
!                   = d(R)/d(t)|                  * (1 + LTSIGN*DLTCTR)
!                              |t=ET+LTSIGN*LTCENT
!
!     Scale the derivative block of the transformation to account
!     for the rate of change of light time.
!
      DO I = 1, 3
         CALL VSCLIP ( 1.D0 + LTSIGN*DLTCTR, XFORM(4,I) )
      END DO

!
!     Now apply the transformation to the stellar aberration
!     correction position and velocity. We'll pack these
!     components together as a state to simplify the multiplication.
!
      CALL VEQU ( PCORR,  SASTAT    )
      CALL VEQU ( DPCORR, SASTAT(4) )

      CALL MXVG ( XFORM, SASTAT, 6, 6, XSTATE )

!
!     Now unpack the stellar aberration correction components.
!
      CALL VEQU ( XSTATE,    PCORR  )
      CALL VEQU ( XSTATE(4), DPCORR )

      CALL CHKOUT ( 'T_GETSA' )
      RETURN
      END




