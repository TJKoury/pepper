!$Procedure ZZSPKAC0 ( S/P Kernel, aberration corrected state )

      subroutine zzspkac0 ( targ, et, ref, abcorr, obs, starg, lt, dlt )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the state (position and velocity) of a target body
!     relative to an observer, optionally corrected for light time
!     and stellar aberration, expressed relative to an inertial
!     reference frame.
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

      integer(ip)           targ
      real(wp)              et
      character(*)          ref
      character(*)          abcorr
      integer(ip)           obs
      real(wp)              starg    ( 6 )
      real(wp)              lt
      real(wp)              dlt

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     TARG       I   Target body.
!     ET         I   Observer epoch.
!     REF        I   Inertial reference frame of output state.
!     ABCORR     I   Aberration correction flag.
!     OBS        I   Observer.
!     STARG      O   State of target.
!     LT         O   One way light time between observer and target.
!     DLT        O   Derivative of light time with respect to time.
!
! $ Detailed_Input
!
!     TARG        is the NAIF ID code for a target body.  The target
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
!     OBS         is the NAIF ID code for the observer body.  The
!                 target and observer define a state vector whose
!                 position component points from the observer to the
!                 target.
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
!    1) Look up a sequence of states of the Moon as seen from the
!       Earth. Use light time and stellar aberration corrections.
!       Compute the first state for the epoch 2000 JAN 1 12:00:00 TDB;
!       compute subsequent states at intervals of 1 hour. For each
!       epoch, display the states, the one way light time between
!       target and observer, and the rate of change of the one way
!       light time.
!
!       Use the following meta-kernel to specify the kernels to
!       load:
!
!          KPL/MK
!
!          This meta-kernel is intended to support operation of SPICE
!          example programs. The kernels shown here should not be
!          assumed to contain adequate or correct versions of data
!          required by SPICE-based user applications.
!
!          In order for an application to use this meta-kernel, the
!          kernels referenced here must be present in the user's
!          current working directory.
!
!          \begindata
!
!             KERNELS_TO_LOAD = ( 'de418.bsp',
!                                 'pck00008.tpc',
!                                 'naif0008.tls'  )
!
!          \begintext
!
!       The code example follows:
!
!           PROGRAM EX1
!           IMPLICIT NONE
!     C
!     C     Local constants
!     C
!     C     The meta-kernel name shown here refers to a file whose
!     C     contents are those shown above. This file and the kernels
!     C     it references must exist in your current working directory.
!     C
!     C
!           CHARACTER*(*)         META
!           PARAMETER           ( META   = 'example.mk' )
!     C
!     C     Use a time step of 1 hour; look up 5 states.
!     C
!           DOUBLE PRECISION      STEP
!           PARAMETER           ( STEP   = 3600.0D0 )
!
!           INTEGER               MAXITR
!           PARAMETER           ( MAXITR = 5 )
!     C
!     C     Local variables
!     C
!           DOUBLE PRECISION      DLT
!           DOUBLE PRECISION      ET
!           DOUBLE PRECISION      ET0
!           DOUBLE PRECISION      LT
!           DOUBLE PRECISION      STATE ( 6 )
!           INTEGER               I
!
!     C
!     C     Load the SPK and LSK kernels via the meta-kernel.
!     C
!           CALL FURNSH ( META )
!     C
!     C     Convert the start time to seconds past J2000 TDB.
!     C
!           CALL STR2ET ( '2000 JAN 1 12:00:00 TDB', ET0 )
!     C
!     C     Step through a series of epochs, looking up a
!     C     state vector at each one.
!     C
!           DO I = 1, MAXITR
!
!              ET = ET0 + (I-1)*STEP
!     C
!     C        Look up a state vector at epoch ET using the
!     C        following inputs:
!     C
!     C           Target:                 Moon (NAIF ID code 301)
!     C           Reference frame:        J2000
!     C           Aberration correction:  Light time and stellar
!     C                                   aberration ('LT+S')
!     C           Observer:               Earth (NAIF ID code 399)
!     C
!
!              CALL SPKACS ( 301, ET,    'J2000', 'LT+S',
!          .                 399, STATE, LT,      DLT     )
!
!              WRITE (*,*) 'ET = ', ET
!              WRITE (*,*) 'J2000 x-position (km):   ', STATE(1)
!              WRITE (*,*) 'J2000 y-position (km):   ', STATE(2)
!              WRITE (*,*) 'J2000 z-position (km):   ', STATE(3)
!              WRITE (*,*) 'J2000 x-velocity (km/s): ', STATE(4)
!              WRITE (*,*) 'J2000 y-velocity (km/s): ', STATE(5)
!              WRITE (*,*) 'J2000 z-velocity (km/s): ', STATE(6)
!              WRITE (*,*) 'One-way light time (s):  ', LT
!              WRITE (*,*) 'Light time rate:         ', DLT
!              WRITE (*,*) ' '
!
!           END DO
!
!           END
!
!     The output produced by this program will vary somewhat as
!     a function of the platform on which the program is built and
!     executed. On a PC/Linux/g77 platform, the following output
!     was produced:
!
!        ET =   0.
!        J2000 x-position (km):    -291584.614
!        J2000 y-position (km):    -266693.406
!        J2000 z-position (km):    -76095.6532
!        J2000 x-velocity (km/s):   0.643439157
!        J2000 y-velocity (km/s):  -0.666065874
!        J2000 z-velocity (km/s):  -0.301310063
!        One-way light time (s):    1.34231061
!        Light time rate:           1.07316909E-07
!
!        ET =   3600.
!        J2000 x-position (km):    -289256.459
!        J2000 y-position (km):    -269080.605
!        J2000 z-position (km):    -77177.3528
!        J2000 x-velocity (km/s):   0.64997032
!        J2000 y-velocity (km/s):  -0.660148253
!        J2000 z-velocity (km/s):  -0.299630418
!        One-way light time (s):    1.34269395
!        Light time rate:           1.05652599E-07
!
!        ET =   7200.
!        J2000 x-position (km):    -286904.897
!        J2000 y-position (km):    -271446.417
!        J2000 z-position (km):    -78252.9655
!        J2000 x-velocity (km/s):   0.656443883
!        J2000 y-velocity (km/s):  -0.654183552
!        J2000 z-velocity (km/s):  -0.297928533
!        One-way light time (s):    1.34307131
!        Light time rate:           1.03990457E-07
!
!        ET =   10800.
!        J2000 x-position (km):    -284530.133
!        J2000 y-position (km):    -273790.671
!        J2000 z-position (km):    -79322.4117
!        J2000 x-velocity (km/s):   0.662859505
!        J2000 y-velocity (km/s):  -0.648172247
!        J2000 z-velocity (km/s):  -0.296204558
!        One-way light time (s):    1.34344269
!        Light time rate:           1.02330665E-07
!
!        ET =   14400.
!        J2000 x-position (km):    -282132.378
!        J2000 y-position (km):    -276113.202
!        J2000 z-position (km):    -80385.612
!        J2000 x-velocity (km/s):   0.669216846
!        J2000 y-velocity (km/s):  -0.642114815
!        J2000 z-velocity (km/s):  -0.294458645
!        One-way light time (s):    1.3438081
!        Light time rate:           1.00673404E-07
!
! $ Restrictions
!
!     1) The kernel files to be used by SPKACS must be loaded
!        (normally by the SPICELIB kernel loader FURNSH) before
!        this routine is called.
!
!     2) Unlike most other SPK state computation routines, this
!        routine requires that the output state be relative to an
!        inertial reference frame.
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
!-    SPICELIB Version 1.1.0, 23-AUG-2021 (NJB)
!
!        Bug fix: aberration correction strings are now parsed using
!        ZZVALCOR. This improves error checking.
!
!-    SPICELIB Version 1.0.1, 04-JUL-2014 (NJB)
!
!        Discussion of light time corrections was updated. Assertions
!        that converged light time corrections are unlikely to be
!        useful were removed.
!
!-    SPICELIB Version 1.0.0, 11-JAN-2008 (NJB)
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
      logical               failed
      logical               return

!
!     Local parameters
!
      character(*), parameter :: rname = 'ZZSPKAC0'

      real(wp), parameter :: delta = 1.0_wp

      integer(ip), parameter :: ssb = 0

!
!     Local variables
!
      character(corlen)     prvcor

      real(wp)              acc    ( 3 )
      real(wp)              ltssb
      real(wp)              ssblt
      real(wp)              ssbobs ( 6 )
      real(wp)              stobs  ( 6, 2 )
      real(wp)              t

      integer(ip)           i
      integer(ip)           refid

      logical               attblk ( nabcor )
      logical               first
      logical               usestl

!
!     Saved variables
!
      save                  first
      save                  prvcor
      save                  usestl

!
!     Initial values
!
      data                  first   / .true. /

      data                  prvcor  / ' ' /

!
!     Standard SPICE error handling.
!
      if ( return () ) return

      if (  first  .or.  ( abcorr /= prvcor )  ) then
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
!           USESTL is .TRUE. when stellar aberration correction is
!           specified.
!
!        The above definitions are consistent with those used by
!        ZZVALCOR.
!
         usestl  =  attblk ( stlidx )

         first   = .false.

      end if

!
!     See if the reference frame is a recognized inertial frame.
!
      call irfnum ( ref, refid )

      if ( refid == 0 ) then

         call setmsg ( 'The requested frame ''#'' is not a ' &
         //            'recognized inertial frame. '        )
         call errch  ( '#', ref                             )
         call sigerr ( 'SPICE(BADFRAME)'                    )
         return

      end if

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
      call zzspkgo0 ( obs, et, ref, ssb, ssbobs, ssblt )

      if ( usestl ) then
!
!        Numerically differentiate the observer velocity relative to
!        the SSB to obtain acceleration. We first evaluate the
!        geometric state of the observer relative to the solar system
!        barycenter at ET +/- DELTA.

         do i = 1, 2

            t = et + (2*i - 3)*delta

            call zzspkgo0 ( obs, t, ref, ssb, stobs(1,i), ltssb )

         end do

         call qderiv ( 3, stobs(4,1), stobs(4,2), delta, acc )

      else

         call cleard ( 3, acc )

      end if

!
!     Look up the apparent state. The light time and light
!     rate are returned as well.
!
      call zzspkas0 ( targ,   et,  ref,   abcorr, &
                      ssbobs, acc, starg, lt,     dlt )

      return
      end
