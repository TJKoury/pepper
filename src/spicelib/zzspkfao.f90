!$Procedure ZZSPKFAO (SPK func., aberration corrected state, observer)

      SUBROUTINE ZZSPKFAO ( TARG,   ET,    REF, ABCORR, &
                            OBSSUB, STARG, LT,  DLT    )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the state (position and velocity) of a target body
!     relative to an observer, optionally corrected for light time and
!     stellar aberration, expressed relative to an inertial reference
!     frame. An input subroutine provides the state of the observer
!     relative to its center of motion.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      INTEGER               TARG
      DOUBLE PRECISION      ET
      CHARACTER*(*)         REF
      CHARACTER*(*)         ABCORR
      EXTERNAL              OBSSUB
      DOUBLE PRECISION      STARG    ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      DLT

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TARG       I   Target body.
!     ET         I   Observer epoch.
!     REF        I   Inertial reference frame of output state.
!     ABCORR     I   Aberration correction flag.
!     OBSSUB     I   Observer state subroutine.
!     STARG      O   State of target.
!     LT         O   One way light time between observer and target.
!     DLT        O   Derivative of light time with respect to time.
!
! $ Detailed_Input
!
!     TARG        is the NAIF ID code for a target body. The target
!                 and observer define a state vector whose position
!                 component points from the observer to the target.
!
!     ET          is the ephemeris time, expressed as seconds past
!                 J2000 TDB, at which the state of the target body
!                 relative to the observer is to be computed.  ET
!                 refers to time at the observer's location.
!
!     REF         is the inertial reference frame with respect to which
!                 the output state STARG is expressed. REF must be
!                 recognized by the SPICE Toolkit.  The acceptable
!                 frames are listed in the Frames Required Reading, as
!                 well as in the SPICELIB routine CHGIRF.
!
!                 Case and blanks are not significant in the string
!                 REF.
!
!     ABCORR      indicates the aberration corrections to be applied
!                 to the state of the target body to account for one-way
!                 light time and stellar aberration.  See the discussion
!                 in the Particulars section for recommendations on
!                 how to choose aberration corrections.
!
!                 ABCORR may be any of the following:
!
!                    'NONE'     Apply no correction. Return the
!                               geometric state of the target body
!                               relative to the observer.
!
!                 The following values of ABCORR apply to the
!                 "reception" case in which photons depart from the
!                 target's location at the light-time corrected epoch
!                 ET-LT and *arrive* at the observer's location at ET:
!
!                    'LT'       Correct for one-way light time (also
!                               called "planetary aberration") using a
!                               Newtonian formulation. This correction
!                               yields the state of the target at the
!                               moment it emitted photons arriving at
!                               the observer at ET.
!
!                               The light time correction uses an
!                               iterative solution of the light time
!                               equation (see Particulars for details).
!                               The solution invoked by the 'LT' option
!                               uses one iteration.
!
!                    'LT+S'     Correct for one-way light time and
!                               stellar aberration using a Newtonian
!                               formulation. This option modifies the
!                               state obtained with the 'LT' option to
!                               account for the observer's velocity
!                               relative to the solar system
!                               barycenter. The result is the apparent
!                               state of the target---the position and
!                               velocity of the target as seen by the
!                               observer.
!
!                    'CN'       Converged Newtonian light time
!                               correction. In solving the light time
!                               equation, the 'CN' correction iterates
!                               until the solution converges (three
!                               iterations on all supported platforms).
!                               Whether the 'CN+S' solution is
!                               substantially more accurate than the
!                               'LT' solution depends on the geometry
!                               of the participating objects and on the
!                               accuracy of the input data. In all
!                               cases this routine will execute more
!                               slowly when a converged solution is
!                               computed. See the Particulars section of
!                               SPKEZR for a discussion of precision of
!                               light time corrections.
!
!                    'CN+S'     Converged Newtonian light time
!                               correction and stellar aberration
!                               correction.
!
!
!                 The following values of ABCORR apply to the
!                 "transmission" case in which photons *depart* from
!                 the observer's location at ET and arrive at the
!                 target's location at the light-time corrected epoch
!                 ET+LT:
!
!                    'XLT'      "Transmission" case:  correct for
!                               one-way light time using a Newtonian
!                               formulation. This correction yields the
!                               state of the target at the moment it
!                               receives photons emitted from the
!                               observer's location at ET.
!
!                    'XLT+S'    "Transmission" case:  correct for
!                               one-way light time and stellar
!                               aberration using a Newtonian
!                               formulation  This option modifies the
!                               state obtained with the 'XLT' option to
!                               account for the observer's velocity
!                               relative to the solar system
!                               barycenter. The position component of
!                               the computed target state indicates the
!                               direction that photons emitted from the
!                               observer's location must be "aimed" to
!                               hit the target.
!
!                    'XCN'      "Transmission" case:  converged
!                               Newtonian light time correction.
!
!                    'XCN+S'    "Transmission" case:  converged
!                               Newtonian light time correction and
!                               stellar aberration correction.
!
!
!     OBSSUB      is the name of an external subroutine that returns
!                 the geometric state of the observer relative to a
!                 center of motion, expressed in the inertial reference
!                 frame REF, at the epoch ET.
!
!                 The calling sequence of OBSSSUB is
!
!                    SUBROUTINE OBSSUB ( ET, REF, OBSCTR, STATE )
!
!                    DOUBLE PRECISION      ET
!                    CHARACTER*(*)         REF
!                    INTEGER               OBSCTR
!                    DOUBLE PRECISION      STATE ( 6 )
!
!                    The inputs of OBSSUB are ET and REF; the outputs
!                    are OBSCTR and STATE. STATE is the geometric state
!                    of the observer relative to the returned center of
!                    motion at ET, expressed in the frame REF.
!
!                 The target designated by TARG and the observer whose
!                 ephemeris is given by OBSSUB define a state vector
!                 having a position component that points from the
!                 observer to the target.
!
! $ Detailed_Output
!
!     STARG       is a Cartesian state vector representing the position
!                 and velocity of the target body relative to the
!                 specified observer. STARG is corrected for the
!                 specified aberrations, and is expressed with respect
!                 to the specified inertial reference frame.  The first
!                 three components of STARG represent the x-, y- and
!                 z-components of the target's position; last three
!                 components form the corresponding velocity vector.
!
!                 The position component of STARG points from the
!                 observer's location at ET to the aberration-corrected
!                 location of the target. Note that the sense of the
!                 position vector is independent of the direction of
!                 radiation travel implied by the aberration
!                 correction.
!
!                 Units are always km and km/sec.
!
!     LT          is the one-way light time between the observer and
!                 target in seconds.  If the target state is corrected
!                 for aberrations, then LT is the one-way light time
!                 between the observer and the light time corrected
!                 target location.
!
!     DLT         is the derivative with respect to barycentric
!                 dynamical time of the one way light time between
!                 target and observer:
!
!                    DLT = d(LT)/d(ET)
!
!                 DLT can also be described as the rate of change of
!                 one way light time. DLT is unitless, since LT and
!                 ET both have units of TDB seconds.
!
!                 If the observer and target are at the same position,
!                 then DLT is set to zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the value of ABCORR is not recognized, the error
!        is diagnosed by a routine in the call tree of this
!        routine.
!
!     2) If the reference frame requested is not a recognized
!        inertial reference frame, the error SPICE(BADFRAME)
!        is signaled.
!
!     3) If the state of the target relative to the solar system
!        barycenter cannot be computed, the error will be diagnosed
!        by routines in the call tree of this routine.
!
!     4) If the observer and target are at the same position,
!        then DLT is set to zero. This situation could arise,
!        for example, when the observer is Mars and the target
!        is the Mars barycenter.
!
! $ Files
!
!     This routine computes states using SPK files that have been
!     loaded into the SPICE system, normally via the kernel loading
!     interface routine FURNSH.  Application programs typically load
!     kernels once before this routine is called, for example during
!     program initialization; kernels need not be loaded repeatedly.
!     See the routine FURNSH and the SPK and KERNEL Required Reading
!     for further information on loading (and unloading) kernels.
!
!     If any of the ephemeris data used to compute STARG are expressed
!     relative to a non-inertial frame in the SPK files providing those
!     data, additional kernels may be needed to enable the reference
!     frame transformations required to compute the state. Normally
!     these additional kernels are PCK files or frame kernels. Any
!     such kernels must already be loaded at the time this routine is
!     called.
!
! $ Particulars
!
!     This routine supports higher-level SPK API routines that can
!     perform both light time and stellar aberration corrections.
!     User applications normally will not need to call this routine
!     directly.
!
!     See the header of the routine SPKEZR for a detailed discussion
!     of aberration corrections.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1) This routine must not be called by routines of the SPICE
!        frame subsystem. It must not be called by any portion of
!        the SPK subsystem other than the private SPK function-based
!        component.
!
!     2) The input subroutine OBSSUB must not call this routine
!        or any of the supporting, private SPK routines
!
!           ZZSPKFAP
!           ZZSPKFLT
!
!     3) The kernel files to be used by ZZSPKFAO must be loaded
!        (normally by the SPICELIB kernel loader FURNSH) before
!        this routine is called.
!
!     4) Unlike most other SPK state computation routines, this
!        routine requires that the output state be relative to an
!        inertial reference frame.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 04-JUL-2014 (NJB)
!
!        Discussion of light time corrections was updated. Assertions
!        that converged light time corrections are unlikely to be
!        useful were removed.
!
!     Last update was 15-APR-2014 (NJB)
!
!        Previous update was Version 1.0.0, 22-FEB-2012
!
!-&

! $ Index_Entries
!
!     low-level aberration correction
!     aberration-corrected state from spk file
!     get light time and stellar aberration-corrected state
!
!-&



! $ Revisions
!
!     None.
!
!-&

!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      CHARACTER*(*)         RNAME
      PARAMETER           ( RNAME = 'ZZSPKFAO' )

      DOUBLE PRECISION      DELTA
      PARAMETER           ( DELTA = 1.D0 )

!
!     Local variables
!
      CHARACTER*(CORLEN)    PRVCOR

      DOUBLE PRECISION      ACC    ( 3 )
      DOUBLE PRECISION      SSBOBS ( 6 )
      DOUBLE PRECISION      STCTR  ( 6 )
      DOUBLE PRECISION      STCTRO ( 6 )
      DOUBLE PRECISION      STOBS  ( 6, 2 )
      DOUBLE PRECISION      T

      INTEGER               I
      INTEGER               OBSCTR
      INTEGER               REFID

      LOGICAL               ATTBLK ( NABCOR )
      LOGICAL               PASS1
      LOGICAL               USESTL

!
!     Saved variables
!
      SAVE                  PASS1
      SAVE                  PRVCOR
      SAVE                  USESTL

!
!     Initial values
!
      DATA                  PASS1   / .TRUE. /

      DATA                  PRVCOR  / ' ' /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( RNAME )


      IF (  PASS1  .OR.  ( ABCORR .NE. PRVCOR )  ) THEN
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any.  Analyze the new flag.
!
         CALL ZZVALCOR ( ABCORR, ATTBLK )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( RNAME )
            RETURN
         END IF

!
!        The aberration correction flag is recognized; save it.
!
         PRVCOR = ABCORR

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           USESTL is .TRUE. when stellar aberration correction is
!           specified.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         USESTL  =  ATTBLK ( STLIDX )

         PASS1   = .FALSE.

      END IF

!
!     See if the reference frame is a recognized inertial frame.
!
      CALL IRFNUM ( REF, REFID )

      IF ( REFID .EQ. 0 ) THEN

         CALL SETMSG ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         CALL ERRCH  ( '#', REF                             )
         CALL SIGERR ( 'SPICE(BADFRAME)'                    )
         CALL CHKOUT ( RNAME                                )
         RETURN

      END IF

!
!     Prepare to look up the apparent state of the target
!     as seen by the observer. We'll need the geometric
!     state of the observer relative to the solar system
!     barycenter. If we're using stellar aberration
!     corrections, we'll need the observer's acceleration
!     as well.
!
!     Get the geometric state of the observer relative to the SSB,
!     which we'll call SSBOBS.
!
!     Start by looking up the state of the observer with respect
!     to its center of motion, expressed in frame REF. Add to this
!     the state of the center with respect to the solar system
!     barycenter.
!
      CALL OBSSUB ( ET,     REF,   OBSCTR, STCTRO )
      CALL SPKSSB ( OBSCTR, ET,    REF,    STCTR  )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( RNAME )
         RETURN
      END IF

      CALL VADDG  ( STCTRO, STCTR, 6, SSBOBS )

      IF ( USESTL ) THEN
!
!        Numerically differentiate the observer velocity relative to
!        the SSB to obtain acceleration. We first evaluate the
!        geometric state of the observer relative to the solar system
!        barycenter at ET +/- DELTA.

         DO I = 1, 2

            T = ET + (2*I - 3)*DELTA

            CALL OBSSUB ( T,      REF,   OBSCTR, STCTRO     )
            CALL SPKSSB ( OBSCTR, T,     REF,    STCTR      )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( RNAME )
               RETURN
            END IF

            CALL VADDG  ( STCTRO, STCTR, 6, STOBS(1,I) )

         END DO

         CALL QDERIV ( 3, STOBS(4,1), STOBS(4,2), DELTA, ACC )

      ELSE

         CALL CLEARD ( 3, ACC )

      END IF

!
!     Look up the apparent state. The light time and light
!     rate are returned as well.
!
      CALL SPKAPS ( TARG,   ET,  REF,   ABCORR, &
                    SSBOBS, ACC, STARG, LT,     DLT )

      CALL CHKOUT ( RNAME )
      RETURN
      END
