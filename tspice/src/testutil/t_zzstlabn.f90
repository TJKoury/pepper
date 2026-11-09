!$Procedure T_ZZSTLABN ( Test utility, numeric stellar aberration )

      SUBROUTINE T_ZZSTLABN ( XMIT, ACCOBS, VOBS, STARG, SCORR, DSCORR )

! $ Abstract
!
!     SPICE test utility.
!
!     Return the stellar aberration offset vector and the time
!     derivative of this offset, using a different numeric
!     method from that used by ZZSTELAB for the near-degenerate
!     case.
!
! $ Required_Reading
!
!     SPK
!
! $ Keywords
!
!     EPHEMERIS
!     TEST
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE

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
!                 component of STARG. Adding DSCORR to this velocity
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
!     1) If attempt to divide by zero occurs, the error
!        SPICE(DIVIDEBYZERO) will be signaled. This case may occur
!        due to uninitialized inputs.
!
!     2) Loss of precision will occur for geometric cases in which
!        VOBS is nearly parallel to the position component of STARG.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine supports testing of the SPICELIB utility ZZSTELAB by
!     using the same algorithm, with one important difference: the
!     SEPLIM value is set so that the numeric branch is always taken.
!
!     Note to maintenance programmers: the implementation source code
!     of this routine should be kept in sync with that of ZZSTELAB,
!     except for the SEPLIM difference.
!
! $ Examples
!
!     See SPICELIB routine SPKACS.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     SPK Required Reading.
!
! $ Author_and_Institution
!
!     N.J. Bachman    (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 14-OCT-2021 (BVS)
!
!        Bug fix: fixed routine name in CHKIN/CHKOUT calls
!        (ZZSTLABN -> T_ZZSTLABN).
!
!-    SPICELIB Version 2.0.0, 15-APR-2014 (NJB)
!
!        Added RETURN test and discovery check-in.
!        Check for division by zero was added. This
!        case might occur due to uninitialized inputs.
!
!-    SPICELIB Version 1.0.1, 12-FEB-2009 (NJB)
!
!        Minor updates were made to the inline documentation.
!
!-    SPICELIB Version 1.0.0, 17-JAN-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               RETURN
      LOGICAL               VZERO

!
!     Local parameters
!
!
!     Let PHI be the (non-negative) rotation angle of the stellar
!     aberration correction; then SEPLIM is a limit on how close PHI
!     may be to zero radians while stellar aberration velocity is
!     computed analytically. When sin(PHI) is less than SEPLIM, the
!     velocity must be computed numerically.
!
      DOUBLE PRECISION      SEPLIM
      PARAMETER           ( SEPLIM = 2.D0 )

!
!     Let TDELTA be the time interval, measured in seconds,
!     used for numerical differentiation of the stellar
!     aberration correction, when this is necessary.
!
      DOUBLE PRECISION      TDELTA
      PARAMETER           ( TDELTA = 1.D0 )

!
!     Local variables
!
      DOUBLE PRECISION      C
      DOUBLE PRECISION      DPHI
      DOUBLE PRECISION      DPTMAG
      DOUBLE PRECISION      DRHAT  ( 3 )
      DOUBLE PRECISION      DVP    ( 3 )
      DOUBLE PRECISION      DVPHAT ( 3 )
      DOUBLE PRECISION      EPTARG ( 3 )
      DOUBLE PRECISION      EVOBS  ( 3 )
      DOUBLE PRECISION      LCACC  ( 3 )
      DOUBLE PRECISION      LCVOBS ( 3 )
      DOUBLE PRECISION      PTARG  ( 3 )
      DOUBLE PRECISION      PTGMAG
      DOUBLE PRECISION      RHAT   ( 3 )
      DOUBLE PRECISION      S
      DOUBLE PRECISION      SAOFF  ( 3, 2 )
      DOUBLE PRECISION      SGN
      DOUBLE PRECISION      SRHAT  ( 6 )
      DOUBLE PRECISION      SVP    ( 6 )
      DOUBLE PRECISION      SVPHAT ( 6 )
      DOUBLE PRECISION      TERM1  ( 3 )
      DOUBLE PRECISION      TERM2  ( 3 )
      DOUBLE PRECISION      TERM3  ( 3 )
      DOUBLE PRECISION      VP     ( 3 )
      DOUBLE PRECISION      VPHAT  ( 3 )
      DOUBLE PRECISION      VTARG  ( 3 )

      INTEGER               I

!
!     Use discovery check-in.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

!
!     In the discussion below, the dot product of vectors X and Y
!     is denoted by
!
!        <X,Y>
!
!     The speed of light is denoted by the lower case letter "c." BTW,
!     variable names used here are case-sensitive: upper case "C"
!     represents a different quantity which is unrelated to the speed
!     of light.
!
!     Variable names ending in "HAT" denote unit vectors. Variable
!     names starting with "D" denote derivatives with respect to time.
!
!     We'll compute the correction SCORR and its derivative with
!     respect to time DSCORR for the reception case. In the
!     transmission case, we perform the same computation with the
!     negatives of the observer velocity and acceleration.
!
!     In the code below, we'll store the position and velocity portions
!     of the input observer-target state STARG in the variables PTARG
!     and VTARG, respectively.
!
!     Let VP be the component of VOBS orthogonal to PTARG. VP
!     is defined as
!
!         VOBS - < VOBS, RHAT > RHAT                                 (1)
!
!     where RHAT is the unit vector
!
!         PTARG/||PTARG||
!
!     Then
!
!        ||VP||/c                                                    (2)
!
!     is the magnitude of
!
!        s = sin( phi )                                              (3)
!
!     where phi is the stellar aberration correction angle. We'll
!     need the derivative with respect to time of (2).
!
!     Differentiating (1) with respect to time yields the
!     velocity DVP, where, letting
!
!        DRHAT  =  d(RHAT) / dt
!        VPHAT  =  VP      / ||VP||
!        DVPMAG =  d( ||VP|| ) / dt
!
!     we have
!
!        DVP = d(VP)/dt
!
!            = ACCOBS - (  ( <VOBS,DRHAT> + <ACCOBS, RHAT> )*RHAT
!                        +   <VOBS,RHAT>  * DRHAT                 )  (4)
!
!     and
!
!        DVPMAG = < DVP, VPHAT >                                     (5)
!
!     Now we can find the derivative with respect to time of
!     the stellar aberration angle phi:
!
!        ds/dt = d(sin(phi))/dt = d(phi)/dt * cos(phi)               (6)
!
!     Using (2) and (5), we have for positive phi,
!
!        ds/dt = (1/c)*DVPMAG = (1/c)*<DVP, VPHAT>                   (7)
!
!     Then for positive phi
!
!        d(phi)/dt = (1/cos(phi)) * (1/c) * <DVP, VPHAT>             (8)
!
!     Equation (8) is well-defined as along as VP is non-zero:
!     if VP is the zero vector, VPHAT is undefined. We'll treat
!     the singular and near-singular cases separately.
!
!     The aberration correction itself is a rotation by angle phi
!     from RHAT towards VP, so the corrected vector is
!
!        ( sin(phi)*VPHAT + cos(phi)*RHAT ) * ||PTARG||
!
!     and  we can express the offset of the corrected vector from
!     PTARG, which is the output SCORR, as
!
!        SCORR =
!
!        ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * ||PTARG||          (9)
!
!     Let DPTMAG be defined as
!
!        DPTMAG  =  d ( ||PTARG|| ) / dt                            (10)
!
!     Then the derivative with respect to time of SCORR is
!
!        DSCORR =
!
!             (      sin(phi)*DVPHAT
!
!                +   cos(phi)*d(phi)/dt * VPHAT
!
!                +  (cos(phi) - 1) * DRHAT
!
!                +  ( -sin(phi)*d(phi)/dt ) * RHAT   ) * ||PTARG||
!
!           + ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * DPTMAG       (11)
!
!
!     Computations begin here:
!
!     Split STARG into position and velocity components. Compute
!
!        RHAT
!        DRHAT
!        VP
!        DPTMAG
!
      IF ( XMIT ) THEN
         CALL VMINUS ( VOBS,   LCVOBS )
         CALL VMINUS ( ACCOBS, LCACC  )
      ELSE
         CALL VEQU   ( VOBS,   LCVOBS )
         CALL VEQU   ( ACCOBS, LCACC  )
      END IF

      CALL VEQU  ( STARG,      PTARG )
      CALL VEQU  ( STARG(4),   VTARG )

      CALL DVHAT ( STARG,      SRHAT )
      CALL VEQU  ( SRHAT,      RHAT  )
      CALL VEQU  ( SRHAT(4),   DRHAT )

      CALL VPERP ( LCVOBS, RHAT, VP    )

      DPTMAG = VDOT ( VTARG, RHAT )

!
!     Compute sin(phi) and cos(phi), which we'll call S and C
!     respectively. Note that phi is always close to zero for
!     realistic inputs (for which ||VOBS|| << CLIGHT), so the
!     cosine term is positive.
!
      S = VNORM(VP) / CLIGHT()

      C = SQRT (  MAX( 0.D0,  1 - S*S )  )

      IF ( C .EQ. 0.D0 ) THEN
!
!        C will be used as a divisor later (in the computation
!        of DPHI), so we'll put a stop to the problem here.
!
         CALL CHKIN  ( 'T_ZZSTLABN'                             )
         CALL SETMSG ( 'Cosine of the aberration angle is 0; ' &
         //            'this cannot occur for realistic ' &
         //            'observer velocities. This case can ' &
         //            'arise due to uninitialized inputs. ' &
         //            'This cosine value is used as a divisor ' &
         //            'in a later computation, so it must not ' &
         //            'be equal to zero.'                      )
        CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                     )
        CALL CHKOUT ( 'T_ZZSTLABN'                              )
        RETURN

      END IF

!
!     Compute the unit vector VPHAT and the stellar
!     aberration correction. We avoid relying on
!     VHAT's exception handling for the zero vector.
!
      IF ( VZERO(VP) ) THEN
         CALL CLEARD ( 3,  VPHAT )
      ELSE
         CALL VHAT   ( VP, VPHAT )
      END IF

!
!     Now we can use equation (9) to obtain the stellar
!     aberration correction SCORR:
!
!        SCORR =
!
!           ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * ||PTARG||
!
!
      PTGMAG = VNORM( PTARG )

      CALL VLCOM ( PTGMAG * S,           VPHAT, &
                   PTGMAG * (C - 1.D0),  RHAT,   SCORR )


!
!     Now we use S as an estimate of PHI to decide if we're
!     going to differentiate the stellar aberration correction
!     analytically or numerically.
!
!     Note that S is non-negative by construction, so we don't
!     need to use the absolute value of S here.
!
      IF ( S .GE. SEPLIM ) THEN
!
!        This is the analytic case.
!
!        Compute DVP---the derivative of VP with respect to time.
!        Recall equation (4):
!
!        DVP = d(VP)/dt
!
!            = ACCOBS - (  ( <VOBS,DRHAT> + <ACCOBS, RHAT> )*RHAT
!                        +   <VOBS,RHAT>  * DRHAT                 )
!
         CALL VLCOM3 ( 1.D0,                                      LCACC, &
                       -VDOT(LCVOBS, DRHAT) - VDOT(LCACC, RHAT),  RHAT, &
                       -VDOT(LCVOBS, RHAT ),                      DRHAT, &
                       DVP                                             )

         CALL VHAT ( VP, VPHAT )

!
!        Now we can compute DVPHAT, the derivative of VPHAT:
!
         CALL VEQU  ( VP,        SVP    )
         CALL VEQU  ( DVP,       SVP(4) )

         CALL DVHAT ( SVP,       SVPHAT )
         CALL VEQU  ( SVPHAT(4), DVPHAT )

!
!        Compute the DPHI, the time derivative of PHI, using equation 8:
!
!           d(phi)/dt = (1/cos(phi)) * (1/c) * <DVP, VPHAT>
!
!
         DPHI = ( 1.D0 / ( C * CLIGHT() ) ) * VDOT( DVP, VPHAT )

!
!        At long last we've assembled all of the "ingredients" required
!        to compute DSCORR:
!
!           DSCORR =
!
!             (     sin(phi)*DVPHAT
!
!                +  cos(phi)*d(phi)/dt * VPHAT
!
!                +  (cos(phi) - 1) * DRHAT
!
!                +  ( -sin(phi)*d(phi)/dt ) * RHAT   ) * ||PTARG||
!
!                +  ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * DPTMAG
!
!
         CALL VLCOM  ( S,             DVPHAT, &
                       C * DPHI,      VPHAT,  TERM1 )

         CALL VLCOM  ( C - 1.D0,      DRHAT, &
                       (-S) * DPHI,   RHAT,   TERM2 )

         CALL VADD   ( TERM1,  TERM2, TERM3 )

         CALL VLCOM3 ( PTGMAG,                TERM3, &
                       DPTMAG * S,            VPHAT, &
                       DPTMAG * (C - 1.D0),   RHAT,    DSCORR )


      ELSE
!
!        This is the numeric case. We're going to differentiate
!        the stellar aberration correction offset vector using
!        a quadratic estimate.
!
         DO I = 1, 2
!
!           Set the sign of the time offset.
!
            IF ( I .EQ. 1 ) THEN
               SGN = -1.D0
            ELSE
               SGN =  1.D0
            END IF

!
!           Estimate the observer's velocity relative to the
!           solar system barycenter at the current epoch. We use
!           the local copies of the input velocity and acceleration
!           to make a linear estimate.
!
            CALL VLCOM ( 1.D0,        LCVOBS, &
                         SGN*TDELTA,  LCACC,     EVOBS )

!
!           Estimate the observer-target vector. We use the
!           observer-target state velocity to make a linear estimate.
!
            CALL VLCOM ( 1.D0,        STARG, &
                         SGN*TDELTA,  STARG(4),  EPTARG )

!
!           Let RHAT be the unit observer-target position.
!           Compute the component of the observer's velocity
!           that is perpendicular to the target position; call
!           this vector VP. Also compute the unit vector in
!           the direction of VP.
!
            CALL VHAT  ( EPTARG,  RHAT     )
            CALL VPERP ( EVOBS,   RHAT, VP )

            IF ( VZERO(VP) ) THEN
               CALL CLEARD ( 3,  VPHAT )
            ELSE
               CALL VHAT   ( VP, VPHAT )
            END IF

!
!           Compute the sine and cosine of the correction
!           angle.
!
            S = VNORM(VP) / CLIGHT()

            C = SQRT (  MAX( 0.D0,  1 - S*S )  )

!
!           Compute the vector offset of the correction.
!
            PTGMAG = VNORM( EPTARG )

            CALL VLCOM ( PTGMAG * S,           VPHAT, &
                         PTGMAG * (C - 1.D0),  RHAT,   SAOFF(1,I) )
         END DO

!
!        Now compute the derivative.
!
         CALL QDERIV ( 3, SAOFF(1,1), SAOFF(1,2), TDELTA, DSCORR )

      END IF

!
!     At this point the correction offset SCORR and its derivative
!     with respect to time DSCORR are both set.
!
      END

