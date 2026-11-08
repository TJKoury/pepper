!$Procedure ZZSPKFLT ( SPK function, light time and rate )

      subroutine zzspkflt ( trgsub, et,    ref, abcorr, &
                            stobs,  starg, lt,  dlt     )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Return the state (position and velocity) of a target body
!     relative to an observer, optionally corrected for light time,
!     expressed relative to an inertial reference frame. An input
!     subroutine provides the state of the target relative to its
!     center of motion.
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

      include 'zzabcorr.inc'

      external              trgsub
      real(wp)              et
      character(*)          ref
      character(*)          abcorr
      real(wp)              stobs  ( 6 )
      real(wp)              starg  ( 6 )
      real(wp)              lt
      real(wp)              dlt

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TRGSUB     I   Target body state subroutine.
!     ET         I   Observer epoch.
!     REF        I   Inertial reference frame of output state.
!     ABCORR     I   Aberration correction flag.
!     STOBS      I   State of the observer relative to the SSB.
!     STARG      O   State of target.
!     LT         O   One way light time between observer and target.
!     DLT        O   Derivative of light time with respect to time.
!
! $ Detailed_Input
!
!     TRGSUB      is the name of an external subroutine that returns
!                 the geometric state of the target body relative to a
!                 center of motion, expressed in the inertial reference
!                 frame REF, at the epoch ET.
!
!                 The calling sequence of TRGSUB is
!
!                    SUBROUTINE TRGSUB ( ET, REF, TRGCTR, STATE )
!
!                    DOUBLE PRECISION      ET
!                    CHARACTER*(*)         REF
!                    INTEGER               TRGCTR
!                    DOUBLE PRECISION      STATE ( 6 )
!
!                    The inputs of TRGSUB are ET and REF; the outputs
!                    are TRGCTR and STATE. STATE is the geometric state
!                    of the target relative to the returned center of
!                    motion at ET, expressed in the frame REF.
!
!                 The target and observer define a state vector whose
!                 position component points from the observer to the
!                 target.
!
!     ET          is the ephemeris time, expressed as seconds past
!                 J2000 TDB, at which the state of the target body
!                 relative to the observer is to be computed. ET
!                 refers to time at the observer's location.
!
!     REF         is the inertial reference frame with respect to which
!                 the input state STOBS and the output state STARG are
!                 expressed. REF must be recognized by the SPICE
!                 Toolkit. The acceptable frames are listed in the
!                 Frames Required Reading, as well as in the SPICELIB
!                 routine CHGIRF.
!
!                 Case and blanks are not significant in the string
!                 REF.
!
!
!     ABCORR      indicates the aberration corrections to be applied to
!                 the state of the target body to account for one-way
!                 light time. See the discussion in the Particulars
!                 section for recommendations on how to choose
!                 aberration corrections.
!
!                 If ABCORR includes the stellar aberration correction
!                 symbol '+S', this flag is simply ignored. Aside from
!                 the possible presence of this symbol, ABCORR may be
!                 any of the following:
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
!                               The light time correction involves
!                               iterative solution of the light time
!                               equation. (See the Particulars section
!                               of SPKEZR for details.) The solution
!                               invoked by the 'LT' option uses one
!                               iteration.
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
!                    'XCN'      "Transmission" case:  converged
!                               Newtonian light time correction.
!
!
!                 Neither special nor general relativistic effects are
!                 accounted for in the aberration corrections applied
!                 by this routine.
!
!                 Case and blanks are not significant in the string
!                 ABCORR.
!
!
!     STOBS       is the geometric (uncorrected) state of the observer
!                 relative to the solar system barycenter at epoch ET.
!                 STOBS is a 6-vector: the first three components of
!                 STOBS represent a Cartesian position vector; the last
!                 three components represent the corresponding velocity
!                 vector. STOBS is expressed relative to the inertial
!                 reference frame designated by REF.
!
!                 Units are always km and km/sec.
!
! $ Detailed_Output
!
!     STARG       is a Cartesian state vector representing the position
!                 and velocity of the target body relative to the
!                 specified observer. STARG is corrected for the
!                 specified aberration, and is expressed with respect
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
!                 for light time, then LT is the one-way light time
!                 between the observer and the light time-corrected
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
!     1) For the convenience of the caller, the input aberration
!        correction flag can call for stellar aberration correction via
!        inclusion of the '+S' suffix. This portion of the aberration
!        correction flag is ignored if present.
!
!     2) If ABCORR calls for stellar aberration but not light
!        time corrections, the error SPICE(NOTSUPPORTED) is
!        signaled.
!
!     3) If ABCORR calls for relativistic light time corrections, the
!        error SPICE(NOTSUPPORTED) is signaled.
!
!     4) If the value of ABCORR is not recognized, the error
!        is diagnosed by a routine in the call tree of this
!        routine.
!
!     5) If the reference frame requested is not a recognized
!        inertial reference frame, the error SPICE(UNKNOWNFRAME)
!        is signaled.
!
!     6) If the state of the target relative to the solar system
!        barycenter cannot be computed, the error will be diagnosed
!        by routines in the call tree of this routine.
!
!     7) If the observer and target are at the same position,
!        then DLT is set to zero. This situation could arise,
!        for example, when the observer is Mars and the target
!        is the Mars barycenter.
!
!     8) If a division by zero error would occur in the computation
!        of DLT, the error SPICE(DIVIDEBYZERO) is signaled.
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
!     This routine supports higher-level routines that can
!     perform both light time and stellar aberration corrections
!     and that use target states provided by subroutines rather
!     than by the conventional, public SPK APIs. For example, this
!     routine can be used for objects having fixed positions
!     on the surfaces of planets.
!
! $ Examples
!
!     See usage in ZZSPKFAP.
!
! $ Restrictions
!
!     1) This routine must not be called by routines of the SPICE
!        frame subsystem. It must not be called by any portion of
!        the SPK subsystem other than the private SPK function-based
!        component.
!
!     2) The input subroutine TRGSUB must not call this routine.
!        or any of the supporting, private SPK routines
!
!     3)  When possible, the routine SPKGEO should be used instead of
!         this routine to compute geometric states. SPKGEO introduces
!         less round-off error when the observer and target have common
!         center that is closer to both objects than is the solar
!         system barycenter.
!
!     4)  Unlike most other SPK state computation routines, this
!         routine requires that the output state be relative to an
!         inertial reference frame.
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
!     Last update was 22-FEB-2012 (NJB)
!
!-&

! $ Index_Entries
!
!     low-level light time correction
!     light-time corrected state from spk file
!     get light-time corrected state
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
      real(wp)              clight
      real(wp)              touchd
      real(wp)              vdot
      real(wp)              vnorm


      logical               failed
      logical               return

!
!     Local parameters
!
      character(*), parameter :: rname = 'ZZSPKFLT'

!
!     TOL is the tolerance used for a division-by-zero test
!     performed prior to computation of DLT.
!
      real(wp), parameter :: tol = 1.0e-10_wp

!
!     Convergence limit:
!
      real(wp), parameter :: cnvlim = 1.0e-17_wp

!
!     Maximum number of light time iterations for any
!     aberration correction:
!
      integer(ip), parameter :: maxitr = 5

!
!     Local variables
!
      character(corlen)     prvcor

      real(wp)              a
      real(wp)              b
      real(wp)              c
      real(wp)              ctrssb ( 6 )
      real(wp)              dist
      real(wp)              epoch
      real(wp)              prvlt
      real(wp)              lterr
      real(wp)              ssbtrg ( 6 )
      real(wp)              sttctr ( 6 )

      integer(ip)           i
      integer(ip)           ltsign
      integer(ip)           numitr
      integer(ip)           refid
      integer(ip)           trgctr

      logical               attblk ( nabcor )
      logical               pass1
      logical               usecn
      logical               uselt
      logical               usestl
      logical               xmit

!
!     Saved variables
!
      save                  pass1
      save                  prvcor
      save                  usecn
      save                  uselt
      save                  xmit

!
!     Initial values
!
      data                  pass1   / .true. /
      data                  prvcor  / ' ' /

!
!     Standard SPICE error handling.
!
      if ( return () ) return



      if (  pass1  .or.  ( abcorr /= prvcor )  ) then
!
!        The aberration correction flag differs from the value it
!        had on the previous call, if any.  Analyze the new flag.
!
         call zzvalcor ( abcorr, attblk )

         if ( failed() ) return

!
!        The aberration correction flag is recognized; save it.
!
         prvcor = abcorr

!
!        Set logical flags indicating the attributes of the requested
!        correction:
!
!           XMIT is .TRUE. when the correction is for transmitted
!           radiation.
!
!           USELT is .TRUE. when any type of light time correction
!           (normal or converged Newtonian) is specified.
!
!           USECN indicates converged Newtonian light time correction.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         xmit    =  attblk ( xmtidx )
         uselt   =  attblk ( ltidx )
         usecn   =  attblk ( cnvidx )
         usestl  =  attblk ( stlidx )

         pass1   = .false.

      end if

!
!     See if the reference frame is a recognized inertial frame.
!
      call irfnum ( ref, refid )

      if ( refid == 0 ) then

         call setmsg ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         call errch  ( '#', ref                             )
         call sigerr ( 'SPICE(UNKNOWNFRAME)'                )
         return

      end if

!
!     Find the geometric state of the target body with respect to
!     the solar system barycenter. Subtract the state of the
!     observer to get the relative state. Use this to compute the
!     one-way light time.
!
      call trgsub ( et,     ref,    trgctr, sttctr )
      call spkssb ( trgctr, et,     ref,    ctrssb )

      if ( failed() ) return

      call vaddg  ( ctrssb, sttctr, 6,      ssbtrg )
      call vsubg  ( ssbtrg, stobs,  6,      starg  )

      dist = vnorm(starg)
      lt   = dist / clight()

      if ( lt == 0.0_wp ) then
!
!        This can happen only if the observer and target are at the
!        same position. We don't consider this an error, but we're not
!        going to compute the light time derivative.
!
         dlt = 0

         return

      end if


      if ( .not. uselt ) then
!
!        This is a special case: we're not using light time
!        corrections, so the derivative
!        of light time is just
!
!           (1/c) * d(VNORM(STARG))/dt
!
         dlt = vdot( starg, starg(4) ) / ( dist * clight() )

!
!        LT and DLT are both set, so we can return.
!
         return

      end if

!
!     To correct for light time, find the state of the target body
!     at the current epoch minus the one-way light time. Note that
!     the observer remains where it is.
!
!     Determine the sign of the light time offset.
!
      if ( xmit ) then
         ltsign =  1
      else
         ltsign = -1
      end if

!
!     Let NUMITR be the number of iterations we'll perform to
!     compute the light time.
!
      if ( usecn ) then
         numitr = maxitr
      else
         numitr = 1
      end if

      i      = 0
      lterr  = 1.0_wp

      do while (        ( i     < numitr ) &
                  .and. ( lterr > cnvlim )  )
!
!        LT was set either prior to this loop or
!        during the previous loop iteration.
!
         epoch = touchd(  et  +  ( ltsign * lt )  )

         call trgsub (         epoch,  ref, trgctr, sttctr )
         call spkssb ( trgctr, epoch,  ref,         ctrssb )

         if ( failed() ) return

         call vaddg  ( ctrssb, sttctr, 6,           ssbtrg )
         call vsubg  ( ssbtrg, stobs,  6,           starg  )

         prvlt = lt
         lt    = touchd(  vnorm ( starg ) / clight()  )

!
!        LTERR is the magnitude of the change between the current
!        estimate of light time and the previous estimate, relative to
!        the previous light time corrected epoch.
!
         lterr = touchd(    abs ( lt  - prvlt      ) &
                          / max ( 1.0_wp, abs(epoch) )  )
         i     = i + 1

      end do

!
!     At this point, STARG contains the light time corrected
!     state of the target relative to the observer.
!
!     Compute the derivative of light time with respect
!     to time: dLT/dt.  Below we derive the formula for
!     this quantity for the reception case. Let
!
!        POBS be the position of the observer relative to the
!        solar system barycenter.
!
!        VOBS be the velocity of the observer relative to the
!        solar system barycenter.
!
!        PTARG be the position of the target relative to the
!        solar system barycenter.
!
!        VTARG be the velocity of the target relative to the
!        solar system barycenter.
!
!        S be the sign of the light time correction. S is
!        negative for the reception case.
!
!     The light-time corrected position of the target relative to
!     the observer at observation time ET, given the one-way
!     light time LT is:
!
!         PTARG(ET+S*LT) - POBS(ET)
!
!     The light-time corrected velocity of the target relative to
!     the observer at observation time ET is
!
!         VTARG(ET+S*LT)*( 1 + S*d(LT)/d(ET) ) - VOBS(ET)
!
!     We need to compute dLT/dt. Below, we use the facts that,
!     for a time-dependent vector X(t),
!
!          ||X||     = <X,X> ** (1/2)
!
!        d(||X||)/dt = (1/2)<X,X>**(-1/2) * 2 * <X,dX/dt>
!
!                    = <X,X>**(-1/2) *  <X,dX/dt>
!
!                    = <X,dX/dt> / ||X||
!
!     Newtonian light time equation:
!
!        LT     =   (1/c) * || PTARG(ET+S*LT) - POBS(ET)||
!
!     Differentiate both sides:
!
!        dLT/dt =   (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
!
!                  * < PTARG(ET+S*LT) - POBS(ET),
!                      VTARG(ET+S*LT)*(1+S*d(LT)/d(ET)) - VOBS(ET) >
!
!
!               = (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
!
!                 * (  < PTARG(ET+S*LT) - POBS(ET),
!                        VTARG(ET+S*LT) - VOBS(ET) >
!
!                   +  < PTARG(ET+S*LT) - POBS(ET),
!                        VTARG(ET+S*LT)           > * (S*d(LT)/d(ET))  )
!
!     Let
!
!        A =   (1/c) * ( 1 / || PTARG(ET+S*LT) - POBS(ET) || )
!
!        B =   < PTARG(ET+S*LT) - POBS(ET), VTARG(ET+S*LT) - VOBS(ET) >
!
!        C =   < PTARG(ET+S*LT) - POBS(ET), VTARG(ET+S*LT) >
!
!     Then
!
!        d(LT)/d(ET) =  A * ( B  +  C * S*d(LT)/d(ET) )
!
!     which implies
!
!        d(LT)/d(ET) =  A*B / ( 1 - S*C*A )
!
!
!
      a   =  1.0_wp / ( clight()*vnorm(starg) )

      b   =  vdot ( starg, starg(4)  )

      c   =  vdot ( starg, ssbtrg(4) )

!
!     For physically realistic target velocities, S*C*A cannot equal 1.
!     We'll check for this case anyway.
!
      if (  ltsign*c*a  >  1.0_wp-tol ) then

         call setmsg ( 'Target range rate magnitude is ' &
      //               'approximately the speed of light. The ' &
      //               'light time derivative cannot be ' &
      //               'computed.'                            )
         call sigerr ( 'SPICE(DIVIDEBYZERO)'                  )
         return

      end if

!
!     Compute DLT: the rate of change of light time.
!
      dlt = (a*b)/( 1.0_wp - ltsign*c*a )

!
!     Overwrite the velocity portion of the output state
!     with the light-time corrected velocity.
!
      call vlcom ( 1.0_wp + ltsign*dlt, ssbtrg(4), &
                  -1.0_wp,              stobs(4),   starg(4) )


      return
      end
