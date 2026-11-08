!$Procedure      ZZGFSSOB ( GF, state of sub-observer point )

      subroutine zzgfssob ( method, trgid, et,    fixref, &
                            abcorr, obsid, radii, state  )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return the state of a sub-observer point used to define
!     coordinates referenced in a GF search.
!
! $ Required_Reading
!
!     GF
!     SPK
!     TIME
!     NAIF_IDS
!     FRAMES
!
! $ Keywords
!
!     GEOMETRY
!     PRIVATE
!     SEARCH
!
! $ Declarations

      implicit none

      include               'gf.inc'
      include               'zzgf.inc'
      include               'zzabcorr.inc'

      character(*)          method
      integer(ip)           trgid
      real(wp)              et
      character(*)          fixref
      character(*)          abcorr
      integer(ip)           obsid
      real(wp)              radii  ( 3 )
      real(wp)              state  ( 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     METHOD     I   Computation method.
!     TRGID      I   Target ID code.
!     ET         I   Computation epoch.
!     FIXREF     I   Reference frame name.
!     ABCORR     I   Aberration correction.
!     OBSID      I   Observer ID code.
!     RADII      I   Target radii.
!     STATE      O   State used to define coordinates.
!
! $ Detailed_Input
!
!     METHOD      is a short string providing parameters defining
!                 the computation method to be used. Any value
!                 supported by SUBPNT may be used.
!
!     TRGID      is the NAIF ID code of the target object.
!
!                *This routine assumes that the target is modeled
!                as a tri-axial ellipsoid.*
!
!     ET         is the time, expressed as ephemeris seconds past J2000
!                TDB, at which the specified state is to be computed.
!
!     FIXREF     is the name of the reference frame relative to which
!                the state of interest is specified.
!
!                FIXREF must be centered on the target body.
!
!                Case, leading and trailing blanks are not significant
!                in the string FIXREF.
!
!     ABCORR     indicates the aberration corrections to be applied to
!                the state of the target body to account for one-way
!                light time and stellar aberration. The orientation
!                of the target body will also be corrected for one-way
!                light time when light time corrections are requested.
!
!                Supported aberration correction options for
!                observation (case where radiation is received by
!                observer at ET) are:
!
!                   NONE           No correction.
!                   LT             Light time only.
!                   LT+S           Light time and stellar aberration.
!                   CN             Converged Newtonian (CN) light time.
!                   CN+S           CN light time and stellar aberration.
!
!                Supported aberration correction options for
!                transmission (case where radiation is emitted from
!                observer at ET) are:
!
!                   XLT            Light time only.
!                   XLT+S          Light time and stellar aberration.
!                   XCN            Converged Newtonian (CN) light time.
!                   XCN+S          CN light time and stellar aberration.
!
!                For detailed information, see the geometry finder
!                required reading, gf.req.  Also see the header of
!                SPKEZR, which contains a detailed discussion of
!                aberration corrections.
!
!                Case, leading and trailing blanks are not significant
!                in the string ABCORR.
!
!     OBSID      is the NAIF ID code of the observer.
!
!     RADII      is an array containing three radii defining
!                a reference ellipsoid for the target body.
!
! $ Detailed_Output
!
!     STATE     is the state of the sub-observer point at ET.
!               The first three components of STATE contain the
!               sub-observer point itself; the last three
!               components contain the derivative with respect to
!               time of the position. The state is expressed
!               relative to the body-fixed frame designated by
!               FIXREF.
!
!               Units are km and km/s.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the aberration correction ABCORR is not recognized,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     2)  If the frame FIXREF is not recognized by the frames
!         subsystem, the error will be diagnosed by routines in the
!         call tree of this routine.
!
!     3)  FIXREF must be centered on the target body; if it isn't,
!         the error will be diagnosed by routines in the call tree
!         of this routine.
!
!     4)  Any error that occurs while look up the state of the target
!         or observer will be diagnosed by routines in the call tree of
!         this routine.
!
!     5)  Any error that occurs while look up the orientation of
!         the target will be diagnosed by routines in the call tree of
!         this routine.
!
!     6)  If the input method is not recognized, the error
!         SPICE(NOTSUPPORTED) will be signaled.
!
! $ Files
!
!     Appropriate kernels must be loaded by the calling program before
!     this routine is called.
!
!     The following data are required:
!
!        - SPK data: ephemeris data for target and observer must be
!          loaded. If aberration corrections are used, the states of
!          target and observer relative to the solar system barycenter
!          must be calculable from the available ephemeris data.
!          Typically ephemeris data are made available by loading one
!          or more SPK files via FURNSH.
!
!        - PCK data: if the target body shape is modeled as an
!          ellipsoid, triaxial radii for the target body must be loaded
!          into the kernel pool. Typically this is done by loading a
!          text PCK file via FURNSH.
!
!        - Further PCK data: rotation data for the target body must be
!          loaded. These may be provided in a text or binary PCK file.
!
!        - Frame data: if a frame definition is required to convert the
!          observer and target states to the body-fixed frame of the
!          target, that definition must be available in the kernel
!          pool. Typically the definition is supplied by loading a
!          frame kernel via FURNSH.
!
!     In all cases, kernel data are normally loaded once per program
!     run, NOT every time this routine is called.
!
! $ Particulars
!
!     This routine isolates the computation of the sub-observer state
!     (that is, the sub-observer point and its derivative with respect
!     to time).
!
!     This routine is used by the GF coordinate utility routines in
!     order to solve for time windows on which specified mathematical
!     conditions involving coordinates are satisfied. The role of
!     this routine is to provide Cartesian state vectors enabling
!     the GF coordinate utilities to determine the signs of the
!     derivatives with respect to time of coordinates of interest.
!
! $ Examples
!
!     See ZZGFCOST.
!
! $ Restrictions
!
!     1)  This routine is restricted to use with ellipsoidal target
!         shape models.
!
!     2)  The computations performed by this routine are intended
!         to be compatible with those performed by the SPICE
!         routine SUBPNT. If that routine changes, this routine
!         may need to be updated.
!
!     3)  This routine presumes that error checking of inputs
!         has, where possible, already been performed by the
!         GF coordinate utility initialization routine.
!
!     4)  The interface and functionality of this set of routines may
!         change without notice. These routines should be called only
!         by SPICELIB routines.
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
!-    SPICELIB Version 2.1.0 09-OCT-2021 (NJB)
!
!        Bug fix: now returns after detecting bad aberration
!        correction string.
!
!        Bug fix: aberration correction strings are now parsed using
!        ZZVALCOR. This improves error checking.
!
!-    SPICELIB Version 2.0.0 12-MAY-2009 (NJB)
!
!        Upgraded to support targets and observers having
!        no names associated with their ID codes.
!
!-    SPICELIB Version 1.0.0 05-MAR-2009 (NJB)
!
!-&

! $ Index_Entries
!
!     sub-observer state
!
!-&

!
!     SPICELIB functions
!
      real(wp)              clight
      real(wp)              vdot

      logical               eqstr
      logical               failed
      logical               return

!
!     Local parameters
!
      real(wp), parameter :: tdelta = 1.0_wp

      integer(ip), parameter :: bdnmln = 36

!
!     Local variables
!
      character(bdnmln)     svobs
      character(bdnmln)     svtarg

      real(wp)              acc    ( 3 )
      real(wp)              corxfi ( 6, 6 )
      real(wp)              corxfm ( 6, 6 )
      real(wp)              dalt   ( 2 )
      real(wp)              dlt
      real(wp)              fxosta ( 6 )
      real(wp)              fxpsta ( 6 )
      real(wp)              fxpvel ( 3 )
      real(wp)              fxtsta ( 6 )
      real(wp)              lt
      real(wp)              obspnt ( 6 )
      real(wp)              obssta ( 6, 2 )
      real(wp)              obstrg ( 6 )
      real(wp)              pntsta ( 6 )
      real(wp)              raysta ( 6 )
      real(wp)              sa     ( 3 )
      real(wp)              sastat ( 6 )
      real(wp)              savel  ( 3 )
      real(wp)              scale
      real(wp)              spoint ( 3 )
      real(wp)              srfvec ( 3 )
      real(wp)              ssbobs ( 6 )
      real(wp)              ssbtg0 ( 6 )
      real(wp)              ssbtrg ( 6 )
      real(wp)              stemp  ( 6 )
      real(wp)              t
      real(wp)              trgepc
      real(wp)              upos   ( 3 )
      real(wp)              xform  ( 6, 6 )

      integer(ip)           center
      integer(ip)           clssid
      integer(ip)           frclss
      integer(ip)           frcode
      integer(ip)           i
      integer(ip)           prvobs
      integer(ip)           prvtrg

      logical               attblk ( abatsz )
      logical               first
      logical               fnd
      logical               found
      logical               geom
      logical               near
      logical               uselt
      logical               usestl
      logical               xmit

!
!     Saved variables
!
      save                  first
      save                  svobs
      save                  prvobs
      save                  prvtrg
      save                  svtarg

!
!     Initial values
!
      data                  first / .true. /

      data                  prvobs /  0 /
      data                  prvtrg /  0 /

      data                  svobs / ' ' /
      data                  svtarg / ' ' /

!
!     Standard SPICE error handling.
!
      if ( return () ) return

      if (  first  .or. ( trgid /= prvtrg )  ) then

         call bodc2s ( trgid, svtarg )

         prvtrg = trgid

      end if

      if (  first  .or. ( obsid /= prvobs )  ) then

         call bodc2s ( obsid, svobs )

         prvobs = obsid

      end if

      first = .false.

!
!     Parse the aberration correction specifier.
!
      call zzvalcor ( abcorr, attblk )

      if ( failed() ) return

      geom   = attblk( geoidx )
      uselt  = attblk( ltidx  )
      usestl = attblk( stlidx )
      xmit   = attblk( xmtidx )

!
!     Decide whether the sub-observer point is computed using
!     the "near point" or "surface intercept" method. Only
!     ellipsoids may be used a shape models for this computation.
!
      if (  eqstr( method, 'Near point: ellipsoid' )  ) then

         near = .true.

      else if (  eqstr( method, 'Intercept: ellipsoid' )  ) then

         near = .false.

      else

         call setmsg ( 'Sub-observer point computation method # ' &
         //            'is not supported by this routine.'       )
         call errch  ( '#', method                               )
         call sigerr ( 'SPICE(NOTSUPPORTED)'                     )
         return

      end if

      if ( geom ) then
!
!        This is the geometric case.
!
!        We need to check the body-fixed reference frame here.
!
         call namfrm ( fixref, frcode )
         call frinfo ( frcode, center, frclss, clssid, fnd )

         if ( failed() ) return

         if ( .not. fnd ) then

            call setmsg ( 'Input reference frame # was not ' &
            //            'recognized.'                     )
            call errch  ( '#', fixref                       )
            call sigerr ( 'SPICE(NOFRAME)'                  )
            return

         end if

         if ( center /= trgid ) then

            call setmsg ( 'Input reference frame # is centered ' &
            //            'on body # instead of body #.'        )
            call errch  ( '#', fixref                           )
            call errint ( '#', center                           )
            call errint ( '#', trgid                            )
            call sigerr ( 'SPICE(INVALIDFRAME)'                 )
            return

         end if

!
!        Get the state of the target with respect to the observer,
!        expressed relative to the target body-fixed frame. We don't
!        need to propagate states to the solar system barycenter in
!        this case.
!
         call spkgeo ( trgid, et, fixref, obsid, fxtsta, lt )

         if ( failed() ) return

!
!        Compute the state of the observer with respect to the target
!        in the body-fixed frame.
!
         call vminug ( fxtsta, 6, fxosta )

!
!        Now we can obtain the surface velocity of the sub-observer
!        point.
!
         if ( near ) then
!
!           The sub-observer point method is "near point."
!
            call dnearp ( fxosta, radii(1), radii(2), radii(3), &
                          fxpsta, dalt,     found              )

            if ( .not. found ) then

               call setmsg ( 'The sub-observer state could ' &
               //            'could not be computed because ' &
               //            'the velocity was not well ' &
               //            'defined. DNEARP returned "not found."' )
               call sigerr ( 'SPICE(DEGENERATECASE)'                 )
               return

            end if

         else
!
!           The sub-observer point method is "surface
!           intercept point." The ray direction is simply
!           the negative of the observer's position relative
!           to the target center.
!
            call vminug ( fxosta, 6, raysta )

            call surfpv ( fxosta,   raysta, &
                          radii(1), radii(2), radii(3), fxpsta, found )

!
!           Although in general it's not an error for SURFPV to
!           be unable to compute an intercept state, it *is*
!           an error in this case, since the ray points toward
!           the center of the target.
!
            if ( .not. found ) then

               call setmsg ( 'The sub-observer state could ' &
               //            'could not be computed because ' &
               //            'the velocity was not well ' &
               //            'defined. SURFPV returned "not found."' )
               call sigerr ( 'SPICE(DEGENERATECASE)'                 )
               return

            end if

         end if

      else if ( uselt ) then
!
!        Light time and possibly stellar aberration corrections are
!        applied.
!
!        Most our work consists of getting ready to call either of the
!        SPICELIB routines DNEARP or SURFPV. In order to make this
!        call, we'll need the velocity of the observer relative to the
!        target body's center in the target body-fixed frame. We must
!        evaluate the rotation state of the target at the correct
!        epoch, and account for the rate of change of light time, if
!        light time corrections are used. The algorithm we use depends
!        on the algorithm used in SUBPNT, since we're computing the
!        derivative with respect to time of the solution found by that
!        routine.
!
!        In this algorithm, we must take into account the fact that
!        SUBPNT performs light time and stellar aberration corrections
!        for the sub-observer point, not for the center of the target
!        body.
!
!        If light time and stellar aberration corrections are used,
!
!           - Find the aberration corrected sub-observer point and the
!             light time-corrected epoch TRGEPC associated with the
!             sub-observer point.
!
!           - Use TRGEPC to find the position of the target relative to
!             the solar system barycenter.
!
!           - Use TRGEPC to find the orientation of the target relative
!             to the J2000 reference frame.
!
!           - Find the light-time corrected position of the
!             sub-observer point; use this to compute the stellar
!             aberration offset that applies to the sub-observer point,
!             as well as the velocity of this offset.
!
!           - Find the corrected state of the target center as seen
!             from the observer, where the corrections are those
!             applicable to the sub-observer point.
!
!           - Negate the corrected target center state to obtain the
!             state of the observer relative to the target.
!
!           - Express the state of the observer relative to the target
!             in the target body fixed frame at TRGEPC.
!
!        Below, we'll use the convention that vectors expressed
!        relative to the body-fixed frame have names of the form
!
!           FX*
!
!        Note that SUBPNT will signal an error if FIXREF is not
!        actually centered on the target body.
!
         call subpnt ( method, svtarg, et,     fixref, &
                       abcorr, svobs,  spoint, trgepc, srfvec )

!
!        Get J2000-relative states of observer and target with respect
!        to the solar system barycenter at their respective epochs of
!        participation.
!
         call spkssb ( obsid, et,     'J2000', ssbobs )
         call spkssb ( trgid, trgepc, 'J2000', ssbtg0 )

!
!        Get the uncorrected J2000 to body-fixed to state
!        transformation at TRGEPC.
!
         call sxform ( 'J2000', fixref, trgepc, xform )

         if ( failed() ) return

!
!        Initialize the state of the sub-observer point in the
!        body-fixed frame. At this point we don't know the
!        point's velocity; set it to zero.
!
         call moved  ( spoint, 3, fxpsta    )
         call cleard (         3, fxpsta(4) )

         if ( usestl ) then
!
!           We're going to need the acceleration of the observer
!           relative to the SSB. Compute this now.
!
            do i = 1, 2
!
!              The epoch is ET -/+ TDELTA.
!
               t =  et  + (2*i-3)*tdelta

               call spkssb ( obsid,  t,  'J2000', obssta(1,i) )

            end do

            if ( failed() ) return

!
!           Compute the observer's acceleration using a quadratic
!           approximation.
!
            call qderiv ( 3, obssta(4,1), obssta(4,2), tdelta, acc )

         end if

!
!        The rest of the algorithm is iterative. On the first
!        iteration, we don't have a good estimate of the velocity
!        of the sub-observer point relative to the body-fixed
!        frame. Since we're using this velocity as an input
!        to the aberration velocity computations, we
!        expect that treating this velocity as zero on the first
!        pass yields a reasonable estimate. On the second pass,
!        we'll use the velocity derived on the first pass.
!
         call cleard ( 3, fxpvel )

!
!        We'll also estimate the rate of change of light time
!        as zero on the first pass.
!
         dlt = 0.0_wp

         do i = 1, 2
!
!           Correct the target's velocity for the rate of
!           change of light time.
!
            if ( xmit ) then
               scale = 1.0_wp + dlt
            else
               scale = 1.0_wp - dlt
            end if

!
!           Scale the velocity portion of the target state to
!           correct the velocity for the rate of change of light
!           time.
!
            call moved ( ssbtg0, 3,         ssbtrg    )
            call vscl  ( scale,  ssbtg0(4), ssbtrg(4) )

!
!           Get the state of the target with respect to the observer.
!
            call vsubg ( ssbtrg, ssbobs, 6, obstrg )

!
!           Correct the J2000 to body-fixed state transformation matrix
!           for the rate of change of light time.
!
            call zzcorsxf ( xmit, dlt, xform, corxfm )

!
!           Invert CORXFM to obtain the corrected
!           body-fixed to J2000 state transformation.
!
            call invstm ( corxfm, corxfi )

!
!           Convert the sub-observer point state to the J2000 frame.
!
            call mxvg ( corxfi, fxpsta, 6, 6, pntsta )

!
!           Find the J2000-relative state of the sub-observer
!           point with respect to the target.
!
            call vaddg ( obstrg, pntsta, 6, obspnt )

            if ( usestl ) then
!
!              Now compute the stellar aberration correction
!              applicable to OBSPNT. We need the velocity of
!              this correction as well.
!
               call zzstelab ( xmit, acc, ssbobs(4), obspnt, sa, savel )

               call moved ( sa,    3, sastat    )
               call moved ( savel, 3, sastat(4) )
!
!              Adding the stellar aberration state to the target center
!              state gives us the state of the target center with
!              respect to the observer, corrected for the aberrations
!              applicable to the sub-observer point.

               call vaddg ( obstrg, sastat, 6, stemp )

            else
               call moved ( obstrg, 6, stemp )
            end if

!
!           Convert STEMP to the body-fixed reference frame.
!
            call mxvg ( corxfm, stemp, 6, 6, fxtsta )

!
!           At long last, compute the state of the observer
!           with respect to the target in the body-fixed frame.
!
            call vminug ( fxtsta, 6, fxosta )

!
!           Now we can obtain the surface velocity of the
!           sub-observer point.
!
            if ( near ) then
!
!              The sub-observer point method is "near point."
!
               call dnearp ( fxosta, radii(1), radii(2), radii(3), &
                             fxpsta, dalt,     found              )

               if ( .not. found ) then

                  call setmsg ( 'The sub-observer state could ' &
                  //            'could not be computed because ' &
                  //            'the velocity was not well ' &
                  //            'defined.  DNEARP returned ' &
                  //            '"not found."'                  )
                  call sigerr ( 'SPICE(DEGENERATECASE)'         )
                  return

               end if

            else
!
!              The sub-observer point method is "surface intercept
!              point." The ray direction is simply the negative of the
!              observer's position relative to the target center.
!
               call vminug ( fxosta, 6, raysta )

               call surfpv ( fxosta,   raysta,   radii(1), &
                             radii(2), radii(3), fxpsta,   found )

!
!              Although in general it's not an error for SURFPV to be
!              unable to compute an intercept state, it *is* an error
!              in this case, since the ray points toward the center of
!              the target.
!
               if ( .not. found ) then

                  call setmsg ( 'The sub-observer state could ' &
                  //            'could not be computed because ' &
                  //            'the velocity was not well ' &
                  //            'defined. SURFPV returned ' &
                  //            '"not found."'                   )
                  call sigerr ( 'SPICE(DEGENERATECASE)'          )
                  return

               end if

            end if

!
!           At this point we can update the surface point
!           velocity and light time derivative estimates.
!
!           In order to compute the light time rate, we'll
!           need the J2000-relative velocity of the sub-observer
!           point with respect to the observer. First convert
!           the sub-observer state to the J2000 frame, then
!           add the result to the state of the target center
!           with respect to the observer.
!
            call mxvg  ( corxfi, fxpsta, 6, 6, pntsta )
            call vaddg ( obstrg, pntsta, 6,    obspnt )

!
!           Now that we have an improved estimate of the
!           sub-observer state, we can estimate the rate of
!           change of light time as
!
!              range rate
!              ----------
!                  c
!
!           If we're correcting for stellar aberration, *ideally* we
!           should remove that correction now, since the light time
!           rate is based on light time between the observer and the
!           light-time corrected sub-observer point. But the error made
!           by including stellar aberration is too small to make it
!           worthwhile to make this adjustment.
!
            call vhat ( obspnt, upos )

            dlt = vdot ( obspnt(4), upos ) / clight()

!
!           With FXPVEL and DLT updated, we'll repeat our
!           computations.
!
         end do

      else
!
!        We should never get here.
!
         call setmsg ( 'Aberration correction # was not ' &
         //            'recognized.'                      )
         call errch  ( '#',  abcorr                       )
         call sigerr ( 'SPICE(NOTSUPPORTED)'              )
         return

      end if

!
!     Copy the computed state to the output argument STATE.
!
      call moved ( fxpsta, 6, state )

      return
      end
