!$Procedure ZZSTELAB ( Private --- stellar aberration correction )

      subroutine zzstelab ( xmit, accobs, vobs, starg, scorr, dscorr )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the state (position and velocity) of a target body
!     relative to an observing body, optionally corrected for light
!     time (planetary aberration) and stellar aberration.
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

      implicit none

      logical               xmit
      real(wp)              accobs ( 3 )
      real(wp)              vobs   ( 3 )
      real(wp)              starg  ( 6 )
      real(wp)              scorr  ( 3 )
      real(wp)              dscorr ( 3 )

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
!     This routine computes a Newtonian estimate of the stellar
!     aberration correction of an input state. Normally the input state
!     has already been corrected for one-way light time.
!
!     Since stellar aberration corrections are typically "small"
!     relative to the magnitude of the input observer-target position
!     and velocity, this routine avoids loss of precision by returning
!     the corrections themselves rather than the corrected state
!     vector. This allows the caller to manipulate (for example,
!     interpolate) the corrections with greater accuracy.
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
!
! $ Version
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
!     Note for the maintenance programmer
!     ===================================
!
!     The source code of the test utility T_ZZSTLABN must be
!     kept in sync with the source code of this routine. That
!     routine uses a value of SEPLIM that forces the numeric
!     branch of the velocity computation to be taken in all
!     cases. See the documentation of that routine for details.
!

!
!     SPICELIB functions
!
      real(wp)              clight
      real(wp)              vdot
      real(wp)              vnorm

      logical               return
      logical               vzero

!
!     Local parameters
!
!     Let PHI be the (non-negative) rotation angle of the stellar
!     aberration correction; then SEPLIM is a limit on how close PHI
!     may be to zero radians while stellar aberration velocity is
!     computed analytically. When sin(PHI) is less than SEPLIM, the
!     velocity must be computed numerically.
!
      real(wp), parameter :: seplim = 1.0e-6_wp

!
!     Let TDELTA be the time interval, measured in seconds,
!     used for numerical differentiation of the stellar
!     aberration correction, when this is necessary.
!
      real(wp), parameter :: tdelta = 1.0_wp

!
!     Local variables
!
      real(wp)              c
      real(wp)              dphi
      real(wp)              dptmag
      real(wp)              drhat  ( 3 )
      real(wp)              dvp    ( 3 )
      real(wp)              dvphat ( 3 )
      real(wp)              eptarg ( 3 )
      real(wp)              evobs  ( 3 )
      real(wp)              lcacc  ( 3 )
      real(wp)              lcvobs ( 3 )
      real(wp)              ptarg  ( 3 )
      real(wp)              ptgmag
      real(wp)              rhat   ( 3 )
      real(wp)              s
      real(wp)              saoff  ( 3, 2 )
      real(wp)              sgn
      real(wp)              srhat  ( 6 )
      real(wp)              svp    ( 6 )
      real(wp)              svphat ( 6 )
      real(wp)              term1  ( 3 )
      real(wp)              term2  ( 3 )
      real(wp)              term3  ( 3 )
      real(wp)              vp     ( 3 )
      real(wp)              vphat  ( 3 )
      real(wp)              vtarg  ( 3 )

      integer(ip)           i

      if ( return() ) return

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
!     Computations begin here:
!
!     Split STARG into position and velocity components. Compute
!
!        RHAT
!        DRHAT
!        VP
!        DPTMAG
!
      if ( xmit ) then
         call vminus ( vobs,   lcvobs )
         call vminus ( accobs, lcacc  )
      else
         call vequ   ( vobs,   lcvobs )
         call vequ   ( accobs, lcacc  )
      end if

      call vequ  ( starg,      ptarg )
      call vequ  ( starg(4),   vtarg )

      call dvhat ( starg,      srhat )
      call vequ  ( srhat,      rhat  )
      call vequ  ( srhat(4),   drhat )

      call vperp ( lcvobs, rhat, vp    )

      dptmag = vdot ( vtarg, rhat )

!
!     Compute sin(phi) and cos(phi), which we'll call S and C
!     respectively. Note that phi is always close to zero for
!     realistic inputs (for which ||VOBS|| << CLIGHT), so the
!     cosine term is positive.
!
      s = vnorm(vp) / clight()

      c = sqrt (  max( 0.0_wp,  1 - s*s )  )

      if ( c == 0.0_wp ) then
!
!        C will be used as a divisor later (in the computation
!        of DPHI), so we'll put a stop to the problem here.
!
         call setmsg ( 'Cosine of the aberration angle is 0; ' &
         //            'this cannot occur for realistic ' &
         //            'observer velocities. This case can ' &
         //            'arise due to uninitialized inputs. ' &
         //            'This cosine value is used as a divisor ' &
         //            'in a later computation, so it must not ' &
         //            'be equal to zero.'                      )
        call sigerr ( 'SPICE(DIVIDEBYZERO)'                     )
        return

      end if

!
!     Compute the unit vector VPHAT and the stellar
!     aberration correction. We avoid relying on
!     VHAT's exception handling for the zero vector.
!
      if ( vzero(vp) ) then
         call cleard ( 3,  vphat )
      else
         call vhat   ( vp, vphat )
      end if

!
!     Now we can use equation (9) to obtain the stellar
!     aberration correction SCORR:
!
!        SCORR =
!
!           ( sin(phi)*VPHAT + (cos(phi)-1)*RHAT ) * ||PTARG||
!
      ptgmag = vnorm( ptarg )

      call vlcom ( ptgmag * s,           vphat, &
                   ptgmag * (c - 1.0_wp),  rhat,   scorr )

!
!     Now we use S as an estimate of PHI to decide if we're
!     going to differentiate the stellar aberration correction
!     analytically or numerically.
!
!     Note that S is non-negative by construction, so we don't
!     need to use the absolute value of S here.
!
      if ( s >= seplim ) then
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
         call vlcom3 ( 1.0_wp,                                      lcacc, &
                       -vdot(lcvobs, drhat) - vdot(lcacc, rhat),  rhat, &
                       -vdot(lcvobs, rhat ),                      drhat, &
                       dvp                                             )

         call vhat ( vp, vphat )

!
!        Now we can compute DVPHAT, the derivative of VPHAT:
!
         call vequ  ( vp,        svp    )
         call vequ  ( dvp,       svp(4) )

         call dvhat ( svp,       svphat )
         call vequ  ( svphat(4), dvphat )

!
!        Compute the DPHI, the time derivative of PHI, using equation 8:
!
!           d(phi)/dt = (1/cos(phi)) * (1/c) * <DVP, VPHAT>
!
         dphi = ( 1.0_wp / ( c * clight() ) ) * vdot( dvp, vphat )

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
         call vlcom  ( s,             dvphat, &
                       c * dphi,      vphat,  term1 )

         call vlcom  ( c - 1.0_wp,      drhat, &
                       (-s) * dphi,   rhat,   term2 )

         call vadd   ( term1,  term2, term3 )

         call vlcom3 ( ptgmag,                term3, &
                       dptmag * s,            vphat, &
                       dptmag * (c - 1.0_wp),   rhat,    dscorr )

      else
!
!        This is the numeric case. We're going to differentiate
!        the stellar aberration correction offset vector using
!        a quadratic estimate.
!
         do i = 1, 2
!
!           Set the sign of the time offset.
!
            if ( i == 1 ) then
               sgn = -1.0_wp
            else
               sgn =  1.0_wp
            end if

!
!           Estimate the observer's velocity relative to the
!           solar system barycenter at the current epoch. We use
!           the local copies of the input velocity and acceleration
!           to make a linear estimate.
!
            call vlcom ( 1.0_wp,        lcvobs, &
                         sgn*tdelta,  lcacc,     evobs )

!
!           Estimate the observer-target vector. We use the
!           observer-target state velocity to make a linear estimate.
!
            call vlcom ( 1.0_wp,        starg, &
                         sgn*tdelta,  starg(4),  eptarg )

!
!           Let RHAT be the unit observer-target position.
!           Compute the component of the observer's velocity
!           that is perpendicular to the target position; call
!           this vector VP. Also compute the unit vector in
!           the direction of VP.
!
            call vhat  ( eptarg,  rhat     )
            call vperp ( evobs,   rhat, vp )

            if ( vzero(vp) ) then
               call cleard ( 3,  vphat )
            else
               call vhat   ( vp, vphat )
            end if

!
!           Compute the sine and cosine of the correction
!           angle.
!
            s = vnorm(vp) / clight()

            c = sqrt (  max( 0.0_wp,  1 - s*s )  )

!
!           Compute the vector offset of the correction.
!
            ptgmag = vnorm( eptarg )

            call vlcom ( ptgmag * s,           vphat, &
                         ptgmag * (c - 1.0_wp),  rhat,   saoff(1,i) )
         end do

!
!        Now compute the derivative.
!
         call qderiv ( 3, saoff(1,1), saoff(1,2), tdelta, dscorr )

      end if

!
!     At this point the correction offset SCORR and its derivative
!     with respect to time DSCORR are both set.
!
      end
