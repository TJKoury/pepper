!$Procedure ZZILUSTA ( Illumination angle states )

      subroutine zzilusta ( method, target, illum,  et, &
                            fixref, abcorr, obsrvr, spoint, &
                            normal, phssta, incsta, emista  )
      use pepper_kinds, only: wp

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Compute illumination angles and their rates of change at a
!     surface point.
!
! $ Required_Reading
!
!     GF
!     PCK
!     TIME
!     SPK
!
! $ Keywords
!
!     ANGLE
!     GEOMETRY
!     SEARCH
!     UTILITY
!
! $ Declarations

      implicit none

      include 'zzabcorr.inc'

      character(*)          method
      character(*)          target
      character(*)          illum
      real(wp)              et
      character(*)          fixref
      character(*)          abcorr
      character(*)          obsrvr
      real(wp)              spoint ( 3 )
      real(wp)              normal ( 3 )
      real(wp)              phssta ( 2 )
      real(wp)              incsta ( 2 )
      real(wp)              emista ( 2 )

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     TARGET     I   Name of target body associated with surface point.
!     ILLUM      I   Name of illumination source.
!     ET         I   Observation epoch (TDB).
!     FIXREF     I   Body-fixed reference frame.
!     ABCORR     I   Aberration correction.
!     OBSRVR     I   Name of observer.
!     SPOINT     I   Surface point.
!     NORMAL     I   Outward normal vector at surface point.
!     PHSSTA     O   Phase angle state.
!     INCSTA     O   Solar incidence angle state.
!     EMISTA     O   Emission angle state.
!
! $ Detailed_Input
!
!     METHOD         is a string specifying the computation method to
!                    be used by this routine. The only value currently
!                    allowed is 'ELLIPSOID'. This indicates that the
!                    target body shape is modeled as an ellipsoid.
!
!                    Case and leading and trailing blanks are not
!                    significant in METHOD.
!
!     TARGET         is the name of the target body associated with the
!                    surface point SPOINT. TARGET may be a body name or
!                    an ID code provided as as string.
!
!     ILLUM          is the name of the illumination source used to
!                    define the illumination angles computed by this
!                    routine. ILLUM may be a body name or an ID code
!                    provided as as string.
!
!     ET             is the observation time. ET is expressed as
!                    seconds past J2000 TDB.
!
!     FIXREF         is the name of the body-centered, body-fixed
!                    reference frame relative to which SPOINT is
!                    specified. The frame's center must coincide with
!                    TARGET.
!
!     ABCORR         indicates the aberration corrections to be
!                    applied. Only reception corrections are supported.
!                    See the header of ILUMIN for a discussion of
!                    aberration corrections used in illumination angle
!                    computations.
!
!     OBSRVR         is the name of the observing body. OBSRVR may be a
!                    body name or an ID code provided as as string.
!
!     SPOINT         is a 3-vector containing the cartesian coordinates
!                    of the surface point at which the illumination
!                    angle states are to be computed. SPOINT is
!                    expressed in the body-centered, body-fixed frame
!                    designated by FIXREF (see description above).
!
!                    Units are km.
!
!     NORMAL         is an outward normal vector to be used for
!                    emission angle and solar incidence angle
!                    calculations. NORMAL should be orthogonal to the
!                    plane tangent at SPOINT to the target body's
!                    surface.
!
! $ Detailed_Output
!
!     PHSSTA         is the phase angle and its rate of change with
!                    respect to TDB, evaluated at ET.
!
!     INCSTA         is the solar incidence angle and its rate of
!                    change with respect to TDB, evaluated at ET.
!
!     EMISTA         is the emission angle and its rate of change with
!                    respect to TDB, evaluated at ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the computation method is not recognized, the error
!         SPICE(INVALIDMETHOD) is signaled.
!
!     2)  If ABCORR specifies a transmission aberration correction,
!         the error SPICE(INVALIDOPTION) is signaled.
!
!     3)  If an error occurs while looking up a state vector, the
!         error will be signaled by a routine in the call tree of
!         this routine.
!
!     4)  If the input normal vector is zero, the error
!         SPICE(ZEROVECTOR) is signaled.
!
! $ Files
!
!     See GFILUM.
!
! $ Particulars
!
!     The term "state" used in the name of this routine refers to
!     the combination of a function and its derivative with respect
!     to time.
!
!     This routine centralizes computation of illumination angles and
!     their rates of change. It also exposes the illumination angle
!     rates of change used by the GF system in order to allow these
!     rates to be tested using the TSPICE system.
!
!     See the SPICELIB routine ILUMIN for a description of the
!     illumination angles computed by this routine.
!
! $ Examples
!
!     See usage in ZZGFILDC.
!
! $ Restrictions
!
!     1) This routine is intended for use only by the GF subsystem.
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
!-    SPICELIB Version 1.0.0, 02-APR-2012 (NJB)
!
!       Previous version was dated 21-MAR-2012
!
!-&

!
!     SPICELIB functions
!
      real(wp)              clight
      real(wp)              dvsep
      real(wp)              vdot
      real(wp)              vsep

      logical               eqstr
      logical               failed
      logical               return
      logical               vzero

!
!     Local parameters
!
      character(*), parameter :: iref = 'J2000'

!
!     Local variables
!
      real(wp)              dlt
      real(wp)              etsurf
      real(wp)              fxnsta ( 6 )
      real(wp)              lt
      real(wp)              ltsrc
      real(wp)              nrmsta ( 6 )
      real(wp)              obssta ( 6 )
      real(wp)              srcsta ( 6 )
      real(wp)              starg  ( 6 )
      real(wp)              tmpxfm ( 6, 6 )
      real(wp)              uvec   ( 3 )
      real(wp)              xform  ( 6, 6 )

      logical               attblk ( abatsz )
      logical               uselt
      logical               xmit

      if ( return() ) return

!
!     For now, only ellipsoids are supported as target shapes.
!
      if (  .not.  eqstr( method, 'ELLIPSOID' )  ) then

         call setmsg ( 'The computation method # was not recognized. ' )
         call errch  ( '#',  method                                    )
         call sigerr ( 'SPICE(INVALIDMETHOD)'                          )
         return

      end if

!
!     Reject zero normal vectors.
!
      if ( vzero(normal) ) then

         call setmsg ( 'The input normal vector must not be zero, ' &
         //            'but sadly, it was.'                       )
         call sigerr ( 'SPICE(ZEROVECTOR)'                        )
         return

      end if

!
!     Look up the state of the target with respect to the
!     observer. We'll represent the state in an inertial
!     reference frame.
!
      call spkcpt ( spoint,   target, fixref, et,    iref, &
                    'TARGET', abcorr, obsrvr, starg, lt    )

!
!     Compute the epoch associated with the surface point.
!
      call zzcorepc ( abcorr, et, lt, etsurf )

!
!     Now let the surface point be the observer, let the observation
!     epoch be ETSURF, and find the apparent state of the illumination
!     source as seen from the surface point.
!
      call spkcpo ( illum,  etsurf, iref,   'OBSERVER', abcorr, &
                    spoint, target, fixref, srcsta,     ltsrc  )

      if ( failed() ) return

!
!     We will need to transform the state of the normal vector to
!     the inertial frame. The epoch at which the transformation must be
!     evaluated is that associated with the surface point.

      call sxform ( fixref, iref, etsurf, xform )

!
!     Correct the body-fixed to inertial frame transformation for the
!     rate of change with respect to ET of observer-surface point light
!     time, if light time corrections are used.
!
!     Start out by parsing ABCORR.
!
      call zzvalcor ( abcorr, attblk )

      if ( failed() ) return

      uselt = attblk( ltidx  )
      xmit  = attblk( xmtidx )

      if ( xmit ) then

         call setmsg ( 'Aberration correction # is for transmission; ' &
         //            'only reception corrections are supported by ' &
         //            'this routine.'                                )
         call errch  ( '#', abcorr                                    )
         call sigerr ( 'SPICE(INVALIDOPTION)'                         )
         return

      end if

      if ( uselt ) then
!
!        Compute the rate of change with respect to ET of the
!        observer-surface point light time. This rate is the range rate
!        divided by the speed of light.
!
         call vhat ( starg, uvec )

         dlt = vdot ( starg(4), uvec ) / clight()
!
!        Correct the state transformation.
!
         call zzcorsxf ( .false., dlt, xform, tmpxfm )

         call moved ( tmpxfm, 36, xform )

      end if

!
!     Create a body-fixed state vector for the normal vector.
!     Convert the normal vector to unit length for safety.
!
      call vhat   ( normal, fxnsta    )
      call cleard ( 3,      fxnsta(4) )

!
!     Transform the state of the normal vector to the inertial
!     frame.
!
      call mxvg ( xform,  fxnsta, 6, 6, nrmsta )

!
!     We also must adjust the state of the illumination source for the
!     rate of change with respect to ET of the observer-surface point
!     light time. The velocity portion of the state we've computed is
!     the derivative with respect to ETSURF (time at the surface point)
!     of the surface point-illumination source vector. We must convert
!     this to a derivative with respect to ET.
!
!     This code assumes reception corrections.
!
      if ( uselt ) then
!
!        ETSURF = ET - LT, so
!
!        d(ETSURF) / d(ET) = ( 1 - DLT )
!
         call vsclip ( 1.0_wp - dlt,  srcsta(4) )

      end if

!
!     The surface-point observer state we wish to use is the negative
!     of the observer-surface point state.
!
      call vminug ( starg, 6, obssta )

!
!     Compute the state (value and rate of change )
!     of the phase angle.
!
      phssta( 1 ) =  vsep  ( obssta, srcsta )
      phssta( 2 ) =  dvsep ( obssta, srcsta )

!
!     Compute the state of the illumination source
!     incidence angle.
!
      incsta( 1 ) =  vsep  ( nrmsta, srcsta )
      incsta( 2 ) =  dvsep ( nrmsta, srcsta )

!
!     Compute the state of the emission angle.
!
      emista( 1 ) =  vsep  ( nrmsta, obssta )
      emista( 2 ) =  dvsep ( nrmsta, obssta )

      return
      end
