C$Procedure T_ZZSTELAB ( Test utility, alternate stellar aberration )
  
      SUBROUTINE T_ZZSTELAB ( XMIT,  ACCOBS, VOBS, 
     .                        STARG, SCORR,  DSCORR  )
      IMPLICIT NONE

C$ Abstract
C
C     Return the stellar aberration offset vector and the time
C     derivative of this offset, using a different analytic
C     method from that used by ZZSTELAB.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     SPK
C
C$ Keywords
C
C     TEST
C     UTILITY
C
C$ Declarations

      LOGICAL               XMIT
      DOUBLE PRECISION      ACCOBS ( 3 )
      DOUBLE PRECISION      VOBS   ( 3 )
      DOUBLE PRECISION      STARG  ( 6 )
      DOUBLE PRECISION      SCORR  ( 3 )
      DOUBLE PRECISION      DSCORR ( 3 )
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     XMIT       I   Reception/transmission flag.
C     ACCOBS     I   Observer acceleration relative to SSB.
C     VOBS       I   Observer velocity relative to to SSB.
C     STARG      I   State of target relative to observer.
C     SCORR      O   Stellar aberration correction for position.
C     DSCORR     O   Stellar aberration correction for velocity.
C
C$ Detailed_Input
C
C     XMIT        is a logical flag which is set to .TRUE. for the
C                 "transmission" case in which photons *depart* from
C                 the observer's location at an observation epoch ET
C                 and arrive at the target's location at the light-time
C                 corrected epoch ET+LT, where LT is the one-way light
C                 time between observer and target; XMIT is set to
C                 .FALSE. for "reception" case in which photons depart
C                 from the target's location at the light-time
C                 corrected epoch ET-LT and *arrive* at the observer's
C                 location at ET.
C
C                 Note that the observation epoch is not used in this
C                 routine.
C
C                 XMIT must be consistent with any light time
C                 corrections used for the input state STARG: if that
C                 state has been corrected for "reception" light time;
C                 XMIT must be .FALSE.; otherwise XMIT must be .TRUE.
C
C     ACCOBS      is the geometric acceleration of the observer
C                 relative to the solar system barycenter. Units are
C                 km/sec**2. ACCOBS must be expressed relative to
C                 an inertial reference frame.
C
C     VOBS        is the geometric velocity of the observer relative to
C                 the solar system barycenter. VOBS must be expressed
C                 relative to the same inertial reference frame as
C                 ACCOBS. Units are km/sec.
C
C     STARG       is the Cartesian state of the target relative to the
C                 observer. Normally STARG has been corrected for
C                 one-way light time, but this is not required. STARG
C                 must be expressed relative to the same inertial
C                 reference frame as ACCOBS. Components are
C                 (x, y, z, dx, dy, dz). Units are km and km/sec.
C                 
C$ Detailed_Output
C
C     SCORR       is the stellar aberration correction for the position
C                 component of STARG. Adding SCORR to this position
C                 vector produces the input observer-target position,
C                 corrected for stellar aberration.
C
C                 The reference frame of SCORR is the common frame
C                 relative to which the inputs ACCOBS, VOBS, and STARG
C                 are expressed. Units are km.
C
C     DSCORR      is the stellar aberration correction for the velocity
C                 component of STARG. Adding SCORR to this velocity
C                 vector produces the input observer-target velocity,
C                 corrected for stellar aberration.
C
C                 The reference frame of DSCORR is the common frame
C                 relative to which the inputs ACCOBS, VOBS, and STARG
C                 are expressed. Units are km/s.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1) If the angular separation of the input observer velocity
C        and observer-target position vectors is less than 1e-12
C        radians, the error SPICE(DEGENERATECASE) will be signaled.
C
C     2) Loss of precision will occur for geometric cases in which
C        VOBS is nearly parallel to the position component of STARG,
C        but for which exception (1) doesn't apply.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine supports testing of the SPICELIB utility ZZSTELAB by
C     attempting to produce the same results via a different analytic
C     method. For all but near-degenerate cases---those in which the
C     stellar aberration rotation angle is nearly zero---the outputs
C     from this routine should be very close to those from ZZSTELAB:
C     relative errors of both position and velocity offsets should be
C     on the order of 1e-14 or better.
C
C$ Examples
C
C     See the test family F_SPKCOR.
C
C$ Restrictions
C
C     1) This routine does not work for near-singular cases.
C        See exception (2) above.
C
C$ Literature_References
C
C     SPK Required Reading.
C
C$ Author_and_Institution
C
C     N.J. Bachman    (JPL)
C
C$ Version
C
C-    TESTUTIL Version 1.0.0, 30-DEC-2007 (NJB)
C
C-&



C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

C
C     Local parameters
C
      DOUBLE PRECISION      SEPLIM
      PARAMETER           ( SEPLIM = 1.D-12 )

C
C     Local variables
C
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


C
C     Reject inputs that define near-linearly dependent
C     observer-target position and SSB-observer velocity
C     vectors.
C
      SEP = VSEP ( VOBS, STARG ) 

      IF (  ( SEP .LT. SEPLIM ) .OR. ( PI()-SEP .LT. SEPLIM )  ) THEN

         CALL SETMSG ( 'Angular separation of position and '
     .   //            'is # degrees: too close to parallel' )
         CALL ERRDP  ( '#', DPR()*SEP                        )
         CALL SIGERR ( 'SPICE(DEGENERATECASE)'               )
         RETURN

      END IF

C
C     Get local copies of the observer's velocity and
C     acceleration relative to the solar system barycenter.
C     Negate these if the computation is for the transmission
C     case.
C
      IF ( XMIT ) THEN

         CALL VMINUS ( ACCOBS, ACC )
         CALL VMINUS ( VOBS,   V   )

      ELSE

         CALL VEQU   ( ACCOBS, ACC )
         CALL VEQU   ( VOBS,   V   )

      END IF

C
C     Find the unit target observer-target position vector PU
C     and the magnitude of the position vector PMAG.
C     Find the time derivative of PU; call this DPU.
C
      CALL DVHAT ( STARG,     PUSTAT )

      CALL VEQU  ( PUSTAT,    PU     )
      CALL VEQU  ( PUSTAT(4), DPU    )
 
C
C     Let PMAG be the norm of the observer-target position.
C
      PMAG = VNORM ( STARG )

C
C     Let DPMAG be the time derivative of PMAG.
C
      DPMAG = VDOT ( STARG(4), PU )

C
C     Find the component of the observer's velocity relative to the SSB
C     that is orthogonal to the observer-target position. Call this
C     component VP. Let VPMAG be the norm of VP. Let VPU be the unit 
C     vector parallel to VP.
C
      CALL VPERP ( V,  STARG, VP )
      CALL UNORM ( VP, VPU,   VPMAG )

C
C     Find the time derivative of VP.
C
C     This one requires a bit of an explanation: we define
C     VP by
C
C        VP  = V - <V,PU>PU
C
C     so, differentiating both sides, we have
C
C        DVP = dV/dt - ( <dV/dt,PU> + <V,dPU/dt> )*PU
C                    -   <V,PU> * dPU/dt
C
C            = ACC   - ( <ACC,PU> + <V,DPU> ) * PU
C                    -   <V,  PU> * DPU
C
      CALL VLCOM3 ( 1.D0,                              ACC,
     .              - ( VDOT(ACC,PU) + VDOT(V,DPU) ),  PU,
     .              -   VDOT(V,  PU),                  DPU,  
     .              DVP                                    )

C
C     Find the time derivative DVPU of VPU.
C
      CALL VEQU  ( VP,  VPSTAT    )
      CALL VEQU  ( DVP, VPSTAT(4) ) 

      CALL DVHAT ( VPSTAT,    VPUSTA )
      CALL VEQU  ( VPUSTA(4), DVPU   )

C
C     Find the sine and cosine of the correction angle PHI;
C     call these SPHI and CPHI respectively. Note that PHI
C     is close to zero for realistic geometries, so the
C     cosine is always positive.
C
      SPHI = VPMAG / CLIGHT()

      CPHI = SQRT (  MAX( 0.D0, 1-SPHI*SPHI )  )

C
C     Find the time derivative of VPMAG; call this DVPMAG.
C
      DVPMAG = VDOT ( DVP, VPU )

C
C     Find the time derivative of PHI; call this DPHI.
C
      DPHI = DVPMAG / ( CLIGHT() * CPHI )

C
C     Find the stellar aberration correction offset: this
C     is the vector to be added to the observer-target
C     position to obtain the corrected position.
C
C     To compute this offset, we rotate the observer-target
C     position vector by PHI and subtract off the original
C     vector. The formula is
C
C        SCORR = PMAG * ( (cos(phi)-1)*PU + sin(phi)*VPU )
C
C
      CALL VLCOM ( PMAG * (CPHI-1.D0),  PU,
     .             PMAG *  SPHI,        VPU,  SCORR )

C
C     Find the stellar aberration correction velocity offset: this is
C     the vector to be added to the observer-target velocity to obtain
C     the corrected position.
C
C     Differentiating the above formula for SCORR, we have
C
C        DSCORR = d(SCORR)/dt 
C
C               =    PMAG *  (  - sin(phi)*d(phi)/dt * PU
C                               + ( cos(phi)-1 )     * DPU
C                               + cos(phi)*d(phi)/dt * VPU
C                               + sin(phi)           * DVPU )
C
C                 +  DPMAG * ( (cos(phi)-1)*PU + sin(phi)*VPU )
C                 
C                          
      CALL VLCOM ( -PMAG * SPHI * DPHI,  PU,
     .              PMAG * (CPHI-1.D0),  DPU,   T1 )

      CALL VLCOM (  PMAG * CPHI * DPHI,  VPU,
     .              PMAG * SPHI,         DVPU,  T2 )

      CALL VLCOM ( DPMAG * (CPHI-1.D0),  PU,
     .             DPMAG *  SPHI,        VPU,   T3 )

      CALL VADD  ( T1, T2, T4     )
      CALL VADD  ( T4, T3, DSCORR )

C
C     SCORR and DSCORR have been set.
C
      RETURN
      END

