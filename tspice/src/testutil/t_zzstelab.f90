!$Procedure T_ZZSTELAB ( Test utility, alternate stellar aberration )

      SUBROUTINE T_ZZSTELAB ( XMIT,  ACCOBS, VOBS, &
                              STARG, SCORR,  DSCORR  )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Return the stellar aberration offset vector and the time
!     derivative of this offset, using a different analytic
!     method from that used by ZZSTELAB.
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

      LOGICAL               XMIT
      DOUBLE PRECISION      ACCOBS ( 3 )
      DOUBLE PRECISION      VOBS   ( 3 )
      DOUBLE PRECISION      STARG  ( 6 )
      DOUBLE PRECISION      SCORR  ( 3 )
      DOUBLE PRECISION      DSCORR ( 3 )
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     XMIT       I   Reception/transmission flag.
!     ACCOBS     I   Observer acceleration relative to SSB.
!     VOBS       I   Observer velocity relative to to SSB.
!     STARG      I   State of target relative to observer.
!     SCORR      O   Stellar aberration correction for position.
!     DSCORR     O   Stellar aberration correction for velocity.
!
! $ Detailed_Input
!
!     XMIT        is a logical flag which is set to .TRUE. for the
!                 "transmission" case in which photons *depart* from
!                 the observer's location at an observation epoch ET
!                 and arrive at the target's location at the light-time
!                 corrected epoch ET+LT, where LT is the one-way light
!                 time between observer and target; XMIT is set to
!                 .FALSE. for "reception" case in which photons depart
!                 from the target's location at the light-time
!                 corrected epoch ET-LT and *arrive* at the observer's
!                 location at ET.
!
!                 Note that the observation epoch is not used in this
!                 routine.
!
!                 XMIT must be consistent with any light time
!                 corrections used for the input state STARG: if that
!                 state has been corrected for "reception" light time;
!                 XMIT must be .FALSE.; otherwise XMIT must be .TRUE.
!
!     ACCOBS      is the geometric acceleration of the observer
!                 relative to the solar system barycenter. Units are
!                 km/sec**2. ACCOBS must be expressed relative to
!                 an inertial reference frame.
!
!     VOBS        is the geometric velocity of the observer relative to
!                 the solar system barycenter. VOBS must be expressed
!                 relative to the same inertial reference frame as
!                 ACCOBS. Units are km/sec.
!
!     STARG       is the Cartesian state of the target relative to the
!                 observer. Normally STARG has been corrected for
!                 one-way light time, but this is not required. STARG
!                 must be expressed relative to the same inertial
!                 reference frame as ACCOBS. Components are
!                 (x, y, z, dx, dy, dz). Units are km and km/sec.
!
! $ Detailed_Output
!
!     SCORR       is the stellar aberration correction for the position
!                 component of STARG. Adding SCORR to this position
!                 vector produces the input observer-target position,
!                 corrected for stellar aberration.
!
!                 The reference frame of SCORR is the common frame
!                 relative to which the inputs ACCOBS, VOBS, and STARG
!                 are expressed. Units are km.
!
!     DSCORR      is the stellar aberration correction for the velocity
!                 component of STARG. Adding SCORR to this velocity
!                 vector produces the input observer-target velocity,
!                 corrected for stellar aberration.
!
!                 The reference frame of DSCORR is the common frame
!                 relative to which the inputs ACCOBS, VOBS, and STARG
!                 are expressed. Units are km/s.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If the angular separation of the input observer velocity
!        and observer-target position vectors is less than 1e-12
!        radians, the error SPICE(DEGENERATECASE) will be signaled.
!
!     2) Loss of precision will occur for geometric cases in which
!        VOBS is nearly parallel to the position component of STARG,
!        but for which exception (1) doesn't apply.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports testing of the SPICELIB utility ZZSTELAB by
!     attempting to produce the same results via a different analytic
!     method. For all but near-degenerate cases---those in which the
!     stellar aberration rotation angle is nearly zero---the outputs
!     from this routine should be very close to those from ZZSTELAB:
!     relative errors of both position and velocity offsets should be
!     on the order of 1e-14 or better.
!
! $ Examples
!
!     See the test family F_SPKCOR.
!
! $ Restrictions
!
!     1) This routine does not work for near-singular cases.
!        See exception (2) above.
!
! $ Literature_References
!
!     SPK Required Reading.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!
! $ Version
!
!-    TESTUTIL Version 1.0.0, 30-DEC-2007 (NJB)
!
!-&



!
!     SPICELIB functions
!

!
!     Local parameters
!
      DOUBLE PRECISION      SEPLIM
      PARAMETER           ( SEPLIM = 1.D-12 )

!
!     Local variables
!
      DOUBLE PRECISION      ACC    ( 3 )
      DOUBLE PRECISION      CPHI
      DOUBLE PRECISION      DPHI
      DOUBLE PRECISION      DPMAG
      DOUBLE PRECISION      DPU    ( 3 )
      DOUBLE PRECISION      DVP    ( 3 )
      DOUBLE PRECISION      DVPMAG
      DOUBLE PRECISION      DVPU   ( 3 )
      DOUBLE PRECISION      PMAG
      DOUBLE PRECISION      PU     ( 3 )
      DOUBLE PRECISION      PUSTAT ( 6 )
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SPHI
      DOUBLE PRECISION      T1     ( 3 )
      DOUBLE PRECISION      T2     ( 3 )
      DOUBLE PRECISION      T3     ( 3 )
      DOUBLE PRECISION      T4     ( 3 )
      DOUBLE PRECISION      V      ( 3 )
      DOUBLE PRECISION      VP     ( 3 )
      DOUBLE PRECISION      VPMAG
      DOUBLE PRECISION      VPSTAT ( 6 )
      DOUBLE PRECISION      VPU    ( 3 )
      DOUBLE PRECISION      VPUSTA ( 6 )


!
!     Reject inputs that define near-linearly dependent
!     observer-target position and SSB-observer velocity
!     vectors.
!
      SEP = VSEP ( VOBS, STARG )

      IF (  ( SEP .LT. SEPLIM ) .OR. ( PI()-SEP .LT. SEPLIM )  ) THEN

         CALL SETMSG ( 'Angular separation of position and ' &
         //            'is # degrees: too close to parallel' )
         CALL ERRDP  ( '#', DPR()*SEP                        )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'               )
         RETURN

      END IF

!
!     Get local copies of the observer's velocity and
!     acceleration relative to the solar system barycenter.
!     Negate these if the computation is for the transmission
!     case.
!
      IF ( XMIT ) THEN

         CALL VMINUS ( ACCOBS, ACC )
         CALL VMINUS ( VOBS,   V   )

      ELSE

         CALL VEQU   ( ACCOBS, ACC )
         CALL VEQU   ( VOBS,   V   )

      END IF

!
!     Find the unit target observer-target position vector PU
!     and the magnitude of the position vector PMAG.
!     Find the time derivative of PU; call this DPU.
!
      CALL DVHAT ( STARG,     PUSTAT )

      CALL VEQU  ( PUSTAT,    PU     )
      CALL VEQU  ( PUSTAT(4), DPU    )

!
!     Let PMAG be the norm of the observer-target position.
!
      PMAG = VNORM ( STARG )

!
!     Let DPMAG be the time derivative of PMAG.
!
      DPMAG = VDOT ( STARG(4), PU )

!
!     Find the component of the observer's velocity relative to the SSB
!     that is orthogonal to the observer-target position. Call this
!     component VP. Let VPMAG be the norm of VP. Let VPU be the unit
!     vector parallel to VP.
!
      CALL VPERP ( V,  STARG, VP )
      CALL UNORM ( VP, VPU,   VPMAG )

!
!     Find the time derivative of VP.
!
!     This one requires a bit of an explanation: we define
!     VP by
!
!        VP  = V - <V,PU>PU
!
!     so, differentiating both sides, we have
!
!        DVP = dV/dt - ( <dV/dt,PU> + <V,dPU/dt> )*PU
!                    -   <V,PU> * dPU/dt
!
!            = ACC   - ( <ACC,PU> + <V,DPU> ) * PU
!                    -   <V,  PU> * DPU
!
      CALL VLCOM3 ( 1.D0,                              ACC, &
                    - ( VDOT(ACC,PU) + VDOT(V,DPU) ),  PU, &
                    -   VDOT(V,  PU),                  DPU, &
                    DVP                                    )

!
!     Find the time derivative DVPU of VPU.
!
      CALL VEQU  ( VP,  VPSTAT    )
      CALL VEQU  ( DVP, VPSTAT(4) )

      CALL DVHAT ( VPSTAT,    VPUSTA )
      CALL VEQU  ( VPUSTA(4), DVPU   )

!
!     Find the sine and cosine of the correction angle PHI;
!     call these SPHI and CPHI respectively. Note that PHI
!     is close to zero for realistic geometries, so the
!     cosine is always positive.
!
      SPHI = VPMAG / CLIGHT()

      CPHI = SQRT (  MAX( 0.D0, 1-SPHI*SPHI )  )

!
!     Find the time derivative of VPMAG; call this DVPMAG.
!
      DVPMAG = VDOT ( DVP, VPU )

!
!     Find the time derivative of PHI; call this DPHI.
!
      DPHI = DVPMAG / ( CLIGHT() * CPHI )

!
!     Find the stellar aberration correction offset: this
!     is the vector to be added to the observer-target
!     position to obtain the corrected position.
!
!     To compute this offset, we rotate the observer-target
!     position vector by PHI and subtract off the original
!     vector. The formula is
!
!        SCORR = PMAG * ( (cos(phi)-1)*PU + sin(phi)*VPU )
!
!
      CALL VLCOM ( PMAG * (CPHI-1.D0),  PU, &
                   PMAG *  SPHI,        VPU,  SCORR )

!
!     Find the stellar aberration correction velocity offset: this is
!     the vector to be added to the observer-target velocity to obtain
!     the corrected position.
!
!     Differentiating the above formula for SCORR, we have
!
!        DSCORR = d(SCORR)/dt
!
!               =    PMAG *  (  - sin(phi)*d(phi)/dt * PU
!                               + ( cos(phi)-1 )     * DPU
!                               + cos(phi)*d(phi)/dt * VPU
!                               + sin(phi)           * DVPU )
!
!                 +  DPMAG * ( (cos(phi)-1)*PU + sin(phi)*VPU )
!
!
      CALL VLCOM ( -PMAG * SPHI * DPHI,  PU, &
                    PMAG * (CPHI-1.D0),  DPU,   T1 )

      CALL VLCOM (  PMAG * CPHI * DPHI,  VPU, &
                    PMAG * SPHI,         DVPU,  T2 )

      CALL VLCOM ( DPMAG * (CPHI-1.D0),  PU, &
                   DPMAG *  SPHI,        VPU,   T3 )

      CALL VADD  ( T1, T2, T4     )
      CALL VADD  ( T4, T3, DSCORR )

!
!     SCORR and DSCORR have been set.
!
      RETURN
      END

