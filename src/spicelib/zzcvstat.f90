!$Procedure ZZCVSTAT ( Constant velocity state )

      subroutine zzcvstat ( et, ref, center, state )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Umbrella routine for constant velocity state "put" and
!     "extrapolate" routines.
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

      real(wp)              et
      character*(*)         ref
      integer(ip)           center
      real(wp)              state ( 6 )

! $ Brief_I/O
!
!     Variable  I/O  Entry points
!     --------  ---  --------------------------------------------------
!     ET         I   ZZCVSSTA, ZZCVXSTA
!     REF        I   ZZCVSSTA, ZZCVXSTA
!     CENTER    I-O  ZZCVSSTA, ZZCVXSTA
!     STATE     I-O  ZZCVSSTA, ZZCVXSTA
!
! $ Detailed_Input
!
!     See the headers of the entry points for descriptions of
!     their inputs.
!
! $ Detailed_Output
!
!     See the headers of the entry points for descriptions of
!     their outputs.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     See the headers of the entry points for descriptions of
!     their exceptions.
!
! $ Files
!
!     See the entry points ZZCVXSTA.
!
! $ Particulars
!
!     This suite of routines stores and extrapolates a constant-velocity
!     ephemeris. There are two entry points:
!
!        ZZCVSSTA  {Store ephemeris parameters}
!        ZZCVXSTA  {Extrapolate state}
!
!     The "store" entry point stores parameters that specify a constant-
!     velocity ephemeris. The routine accepts as inputs an initial state
!     vector, a epoch, a center of motion, and a reference frame name.
!
!     The "extrapolate" entry point extrapolates the input state to a
!     given epoch ET via the linear formula
!
!        Final_state =      Initial_position
!                      +  ( ET - Initial_ET ) * Initial_velocity
!
!     Following extrapolation, the final state is transformed to the
!     output reference frame.
!
! $ Examples
!
!     See usage in CVOSTA, CVTSTA.
!
! $ Restrictions
!
!     1) This routine can store data for only one
!        constant-velocity ephemeris.
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
!-    SPICELIB Version 2.0.0, 11-JAN-2012 (NJB)
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
      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

!
!     Local variables
!
      character*(frnmln)    svref

      real(wp)              delta
      real(wp)              svet
      real(wp)              svstat ( 6 )
      real(wp)              xstate ( 6 )
      real(wp)              xf     ( 6, 6 )

      integer(ip)           svctr

!
!     Saved variables
!
      save                  svctr
      save                  svet
      save                  svref
      save                  svstat

!
!     This routine should never be called.
!
      call chkin  ( 'ZZCVSTAT'          )
      call sigerr ( 'SPICE(BOGUSENTRY)' )
      call chkout ( 'ZZCVSTAT'          )
      return





!$Procedure ZZCVXSTA ( Constant velocity state, fetch state )

      entry zzcvxsta ( et, ref, center, state )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute target state relative to its center of motion at
!     a specified time, in a specified reference frame.
!
! $ Required_Reading
!
!     FRAMES
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations
!
!     DOUBLE PRECISION      ET
!     CHARACTER*(*)         REF
!     INTEGER               CENTER
!     DOUBLE PRECISION      STATE ( 6 )
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     ET         I   Observer epoch.
!     REF        I   Inertial reference frame of output state.
!     CENTER     O   ID code of center of motion.
!     STATE      O   State vector.
!
! $ Detailed_Input
!
!     ET          is the ephemeris time, expressed as seconds past
!                 J2000 TDB, at which to evaluate the constant-velocity
!                 ephemeris defined by the latest call to ZZCVSSTA.
!
!     REF         is name of a reference frame with respect to which
!                 the output state vector STATE is to be expressed.
!
!                 Case and leading and trailing blanks in REF are not
!                 significant.
!
! $ Detailed_Output
!
!     CENTER      is the NAIF ID code of the center of motion
!                 associated with the output state vector.
!
!     STATE       is a state vector. The contents of STATE are
!                 compatible with output states returned by SPKEZR: the
!                 first three components of STARG represent the x-, y-
!                 and z-components of the target's position; the last
!                 three components form the corresponding velocity
!                 vector.
!
!                 Units are always km and km/sec.
!
!                 STATE is obtained by applying linear extrapolation
!                 to the state stored by the latest call to ZZCVSSTA.
!                 The extrapolation is performed using the difference
!                 between ET and the epoch stored with the saved state.
!
!                 STATE contains the result of this extrapolation,
!                 expressed in the reference frame designated by the
!                 input REF. This frame's orientation is evaluated at
!                 the epoch ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If REF is not recognized, the error will be diagnosed by
!         routines in the call tree of this routine.
!
!     2)  If the loaded kernels provide insufficient data to compute
!         the requested state vector, the deficiency will be diagnosed
!         by a routine in the call tree of this routine.
!
!     3)  If an error occurs while reading a kernel file, the error
!         will be diagnosed by a routine in the call tree of this
!         routine.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data may be required to transform the stored state
!     to the output frame REF:
!
!        - PCK data: rotation data will be needed for
!          for any frame in the chain connecting the frame
!          of the stored state to that of the output state.
!
!        - Frame data: any frame definition required to compute
!          the transformation to the output frame must be known
!          to SPICE. If the definition is not built in, it must
!          be provided via a frame kernel.
!
!        - CK data: if a CK frame is required to compute the
!          transformation to the output frame, at least one CK file
!          will be needed.
!
!        - SCLK data: if a CK file is needed, an associated SCLK
!          kernel is required to enable conversion between encoded SCLK
!          (used to time-tag CK data) and barycentric dynamical time
!          (TDB).
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     This routine returns the state vector stored by the entry point
!
!        ZZCVSSTA
!
!     after transforming the state to the reference frame REF.
!
! $ Examples
!
!     See usage in CVOSTA, CVTSTA.
!
! $ Restrictions
!
!     1) This routine can compute states based only on the most
!        recently stored state vector.
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
!-    SPICELIB Version 2.0.0, 11-JAN-2012 (NJB)
!
!-&

      if ( return() ) then
         return
      end if

      call chkin ( 'ZZCVXSTA' )

!
!     Extrapolate the saved state to the input time.
!
      delta = et - svet

      call vlcom ( 1.0_wp, svstat(1), delta, svstat(4), xstate )

      call vequ  ( svstat(4), xstate(4) )

!
!     Convert the extrapolated state to the requested frame
!     at ET.
!
      call sxform ( svref, ref, et, xf )

      if ( failed() ) then
         call chkout ( 'ZZCVXSTA' )
         return
      end if

      call mxvg ( xf, xstate, 6, 6,  state )

!
!     Set the output center of motion argument as well.
!
      center = svctr

      call chkout ( 'ZZCVXSTA' )
      return







!$Procedure ZZCVSSTA ( Constant velocity state, store parameters )

      entry zzcvssta ( state, center, et, ref )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Save an object's state and the center, time, and frame associated
!     with the state.
!
! $ Required_Reading
!
!     FRAMES
!     SPK
!     TIME
!
! $ Keywords
!
!     EPHEMERIS
!
! $ Declarations
!
!     DOUBLE PRECISION      STATE ( 6 )
!     INTEGER               CENTER
!     DOUBLE PRECISION      ET
!     CHARACTER*(*)         REF
!
! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     STATE      I   State vector.
!     CENTER     I   ID code of center of motion.
!     ET         I   Observer epoch.
!     REF        I   Inertial reference frame of output state.
!
! $ Detailed_Input
!
!     STATE       is a state vector. The contents of STATE are
!                 compatible with output states returned by SPKEZR.
!
!     CENTER      is the NAIF ID code of the center of motion associated
!                 with the input state vector.
!
!     ET          is the ephemeris time, expressed as seconds past
!                 J2000 TDB, associated with the input state.
!
!     REF         is name of a reference frame with respect to which
!                 the input STATE is expressed. This routine simply
!                 stores REF, so there are no restrictions on the
!                 contents of this string. However, at the time
!                 the companion entry point ZZCVXSTA is called, the
!                 frame designated by REF must be recognized by the
!                 SPICE system.
!
! $ Detailed_Output
!
!     None. This routine operates by side effects.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine stores its inputs so that they can be
!     used for linear extrapolation by the entry point
!
!        ZZCVXSTA
!
! $ Examples
!
!     See usage in CVOSTA, CVTSTA.
!
! $ Restrictions
!
!     1) This routine can store data for only one
!        constant-velocity ephemeris.
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
!-    SPICELIB Version 2.0.0, 11-JAN-2012 (NJB)
!
!-&

      if ( return() ) then
         return
      end if

!
!     No SPICE errors are detected here, so we don't check in.
!
!
!     Save all inputs.
!
      call moved ( state, 6, svstat )

      svctr = center
      svet  = et
      svref = ref

      return
      end



