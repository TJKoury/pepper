!$Procedure      ZZGFSSIN ( GF, state of surface intercept point )

      SUBROUTINE ZZGFSSIN ( METHOD, TRGID, ET,   FIXREF, ABCORR, OBSID, &
                            DREF,   DCTR,  DVEC, RADII,  STATE,  FOUND )

! $ Abstract
!
!     SPICE private routine intended solely for the support of SPICE
!     routines. Users should not call this routine directly due to the
!     volatile nature of this routine.
!
!     Return the state of a ray-target surface intercept point used to
!     define coordinates referenced in a GF search.
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

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

      CHARACTER*(*)         METHOD
      INTEGER               TRGID
      DOUBLE PRECISION      ET
      CHARACTER*(*)         FIXREF
      CHARACTER*(*)         ABCORR
      INTEGER               OBSID
      CHARACTER*(*)         DREF
      INTEGER               DCTR
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      LOGICAL               FOUND

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
!     DREF       I   Reference frame of ray's direction vector.
!     DCTR       I   DREF's center ID code.
!     DVEC       I   Ray's direction vector.
!     RADII      I   Target radii.
!     STATE      O   State used to define coordinates.
!     FOUND      O   Flag indicating whether state was found.
!
! $ Detailed_Input
!
!     METHOD      is a short string providing parameters defining
!                 the computation method to be used. Any value
!                 supported by SUBPNT may be used.
!
!
!     TRGID      is the NAIF ID code of the target object.
!
!                *This routine assumes that the target is modeled
!                as a tri-axial ellipsoid.*
!
!
!     ET         is the time, expressed as ephemeris seconds past J2000
!                TDB, at which the specified state is to be computed.
!
!
!     FIXREF     is the name of the reference frame relative to which
!                the state of interest is specified.
!
!                FIXREF must be centered on the target body.
!
!                Case, leading and trailing blanks are not significant
!                in the string FIXREF.
!
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
!
!     OBSID      is the NAIF ID code of the observer.
!
!
!     DREF        is the name of the reference frame relative to which
!                 a ray's direction vector is expressed. This may be
!                 any frame supported by the SPICE system, including
!                 built-in frames (documented in the Frames Required
!                 Reading) and frames defined by a loaded frame kernel
!                 (FK). The string DREF is case-insensitive, and
!                 leading and trailing blanks in DREF are not
!                 significant.
!
!                 When DREF designates a non-inertial frame, the
!                 orientation of the frame is evaluated at an epoch
!                 dependent on the frame's center and, if the center is
!                 not the observer, on the selected aberration
!                 correction. See the description of the direction
!                 vector DVEC for details.
!
!
!     DCTR        is the NAIF ID code of the body at which the frame
!                 designated by DREF is centered. While DCTR can
!                 be obtained from the FRAMEX system, passing in
!                 the ID code is more efficient. DCTR should be looked
!                 up by the coordinate search utility initialization
!                 routine before a search is performed.
!
!
!     DVEC        Ray direction vector emanating from the observer. The
!                 intercept with the target body's surface of the ray
!                 defined by the observer and DVEC is sought.
!
!                 DVEC is specified relative to the reference frame
!                 designated by DREF.
!
!                 Non-inertial reference frames are treated as follows:
!                 if the center of the frame is at the observer's
!                 location, the frame is evaluated at ET. If the
!                 frame's center is located elsewhere, then letting
!                 LTCENT be the one-way light time between the observer
!                 and the central body associated with the frame, the
!                 orientation of the frame is evaluated at ET-LTCENT,
!                 ET+LTCENT, or ET depending on whether the requested
!                 aberration correction is, respectively, for received
!                 radiation, transmitted radiation, or is omitted.
!                 LTCENT is computed using the method indicated by
!                 ABCORR.
!
!
!     RADII      is an array containing three radii defining
!                a reference ellipsoid for the target body.
!
! $ Detailed_Output
!
!     STATE     is the state of the surface intercept point at ET.
!               The first three components of STATE contain the
!               surface intercept point itself; the last three
!               components contain the derivative with respect to
!               time of the intercept. The state is expressed
!               relative to the body-fixed frame designated by
!               FIXREF.
!
!               Units are km and km/s.
!
!               STATE is defined if and only if the output flag FOUND
!               is set to .TRUE.
!
!
!     FOUND     is a logical flag indicating whether the requested
!               state was found. FOUND is set to .TRUE. if and only
!               if
!
!                  - the surface intercept exists
!
!                  - the velocity of the surface intercept
!                    is computable
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
!     7)  The input ray direction frame center DCTR must be compatible
!         with the ray direction frame DREF. This routine *does not
!         check* the validity of DCTR.
!
!     8)  If the input ray's direction vector is the zero vector, the
!         error will be diagnosed by a routine in the call tree of this
!         routine.
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
!     This routine isolates the computation of the surface intercept
!     state (that is, the surface intercept point and its derivative
!     with respect to time).
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
!-    SPICELIB Version 2.2.0 09-OCT-2021 (NJB)
!
!        Bug fix: now returns after detecting bad aberration
!        correction string.
!
!        Bug fix: aberration correction strings are now parsed using
!        ZZVALCOR. This improves error checking.
!
!-    SPICELIB Version 2.1.0 08-MAR-2017 (NJB)
!
!        Bug fix: now returns after SINCPT call if FOUND is .FALSE.
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
!     surface intercept state
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VDOT

      LOGICAL               EQSTR
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      DOUBLE PRECISION      TDELTA
      PARAMETER           ( TDELTA = 1.D0 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

!
!     Local variables
!
      CHARACTER*(BDNMLN)    SVOBS
      CHARACTER*(BDNMLN)    SVTARG

      DOUBLE PRECISION      ACC    ( 3 )
      DOUBLE PRECISION      CORXFI ( 6, 6 )
      DOUBLE PRECISION      CORXFM ( 6, 6 )
      DOUBLE PRECISION      CTRSTA ( 6 )
      DOUBLE PRECISION      DCORXF ( 6, 6 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DLTCTR
      DOUBLE PRECISION      DRFEPC
      DOUBLE PRECISION      DRXFRM ( 6, 6 )
      DOUBLE PRECISION      FXDSTA ( 6 )
      DOUBLE PRECISION      FXOSTA ( 6 )
      DOUBLE PRECISION      FXPSTA ( 6 )
      DOUBLE PRECISION      FXPVEL ( 3 )
      DOUBLE PRECISION      FXTSTA ( 6 )
      DOUBLE PRECISION      J2DSTA ( 6 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTCTR
      DOUBLE PRECISION      LTSIGN
      DOUBLE PRECISION      OBSPNT ( 6 )
      DOUBLE PRECISION      OBSSTA ( 6, 2 )
      DOUBLE PRECISION      OBSTRG ( 6 )
      DOUBLE PRECISION      PNTSTA ( 6 )
      DOUBLE PRECISION      SA     ( 3 )
      DOUBLE PRECISION      SASTAT ( 6 )
      DOUBLE PRECISION      SAVEL  ( 3 )
      DOUBLE PRECISION      SCALE
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SSBOBS ( 6 )
      DOUBLE PRECISION      SSBTG0 ( 6 )
      DOUBLE PRECISION      SSBTRG ( 6 )
      DOUBLE PRECISION      STEMP  ( 6 )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      UPOS   ( 3 )
      DOUBLE PRECISION      XFORM  ( 6, 6 )

      INTEGER               CENTER
      INTEGER               CLSSID
      INTEGER               FRCODE
      INTEGER               FRCLSS
      INTEGER               I
      INTEGER               PRVOBS
      INTEGER               PRVTRG

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FIRST
      LOGICAL               FND
      LOGICAL               GEOM
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT

!
!     Saved variables
!
      SAVE                  FIRST
      SAVE                  SVOBS
      SAVE                  PRVOBS
      SAVE                  PRVTRG
      SAVE                  SVTARG

!
!     Initial values
!
      DATA                  FIRST / .TRUE. /

      DATA                  PRVOBS /  0 /
      DATA                  PRVTRG /  0 /

      DATA                  SVOBS / ' ' /
      DATA                  SVTARG / ' ' /



!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZGFSSIN' )

!
!     No result has been found.
!
      FOUND = .FALSE.


      IF (  FIRST  .OR. ( TRGID .NE. PRVTRG )  ) THEN

         CALL BODC2S ( TRGID, SVTARG )

         PRVTRG = TRGID

      END IF


      IF (  FIRST  .OR. ( OBSID .NE. PRVOBS )  ) THEN

         CALL BODC2S ( OBSID, SVOBS )

         PRVOBS = OBSID

      END IF

      FIRST = .FALSE.

!
!     Parse the aberration correction specifier.
!
      CALL ZZVALCOR ( ABCORR, ATTBLK )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'ZZGFSSIN' )
         RETURN
      END IF

      GEOM   = ATTBLK( GEOIDX )
      USELT  = ATTBLK( LTIDX  )
      USESTL = ATTBLK( STLIDX )
      XMIT   = ATTBLK( XMTIDX )

!
!     Set the sign associated with the light time correction.
!
      IF ( XMIT ) THEN
         LTSIGN =  1.D0
      ELSE
         LTSIGN = -1.D0
      END IF

!
!     Decide whether the surface intercept point is computed using
!     the "near point" or "surface intercept" method. Only
!     ellipsoids may be used a shape models for this computation.
!
      IF (  .NOT. EQSTR( METHOD, 'Ellipsoid' )  ) THEN

         CALL SETMSG ( 'Surface intercept point computation method # ' &
         //            'is not supported by this routine.'            )
         CALL ERRCH  ( '#', METHOD                                    )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                          )
         CALL CHKOUT ( 'ZZGFSSIN'                                     )
         RETURN

      END IF


      IF ( GEOM ) THEN
!
!        This is the geometric case.
!
!        No light time correction is involved, so all frames are
!        evaluated at the observation epoch.
!
!        Compute the state transformation from DREF to J2000.
!
         CALL SXFORM ( DREF, 'J2000', ET, DCORXF )

!
!        Transform the ray's direction vector from DREF to the J2000
!        frame. The velocity of DVEC in frame DREF is zero.
!
         CALL MOVED  ( DVEC, 3, STEMP    )
         CALL CLEARD (       3, STEMP(4) )

         CALL MXVG ( DCORXF, STEMP, 6, 6, J2DSTA )

!
!        We need to check the body-fixed reference frame here.
!
         CALL NAMFRM ( FIXREF, FRCODE )
         CALL FRINFO ( FRCODE, CENTER, FRCLSS, CLSSID, FND )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFSSIN' )
            RETURN
         END IF

         IF ( .NOT. FND ) THEN

            CALL SETMSG ( 'Input reference frame # was not ' &
            //            'recognized.'                     )
            CALL ERRCH  ( '#', FIXREF                       )
            CALL SIGERR ( 'SPICE(NOFRAME)'                  )
            CALL CHKOUT ( 'ZZGFSSIN'                        )
            RETURN

         END IF

         IF ( CENTER .NE. TRGID ) THEN

            CALL SETMSG ( 'Input reference frame # is centered ' &
            //            'on body # instead of body #.'        )
            CALL ERRCH  ( '#', FIXREF                           )
            CALL ERRINT ( '#', CENTER                           )
            CALL ERRINT ( '#', TRGID                            )
            CALL SIGERR ( 'SPICE(INVALIDFRAME)'                 )
            CALL CHKOUT ( 'ZZGFSSIN'                            )
            RETURN

         END IF


!        Get the state of the target with respect to the observer,
!        expressed relative to the target body-fixed frame. We don't
!        need to propagate states to the solar system barycenter in
!        this case.
!
         CALL SPKGEO ( TRGID, ET, FIXREF, OBSID, FXTSTA, LT )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFSSIN' )
            RETURN
         END IF

!
!        Compute the state of the observer with respect to the target
!        in the body-fixed frame.
!
         CALL VMINUG ( FXTSTA, 6, FXOSTA )

!
!        Transform the state of the direction vector from the J2000
!        frame to the target body-fixed frame at TRGEPC. Since no
!        light time corrections are involved, the state transformation
!        matrix from SXFORM works just fine.
!
         CALL SXFORM ( 'J2000', FIXREF, ET,   XFORM  )
         CALL MXVG   ( XFORM,   J2DSTA, 6, 6, FXDSTA )

!
!        Now we can obtain the surface velocity of the surface intercept
!        point.
!
         CALL SURFPV ( FXOSTA,   FXDSTA,   RADII(1), &
                       RADII(2), RADII(3), FXPSTA,   FOUND )

!
!        It's not an error for SURFPV to be unable to compute an
!        intercept state; return now if the state was not
!        computable.
!
         IF ( .NOT. FOUND ) THEN
            CALL CHKOUT ( 'ZZGFSSIN' )
            RETURN
         END IF


      ELSE IF ( USELT ) THEN
!
!        Light time and possibly stellar aberration corrections
!        are applied.
!
!        Compute the state transformation from DREF to J2000.
!
         IF ( OBSID .EQ. DCTR ) THEN
!
!           DREF is centered on the observer, so there's no light time
!           correction.
!
            CALL SXFORM ( DREF, 'J2000', ET, DCORXF )

         ELSE
!
!           Find the epoch DRFEPC associated with the input direction
!           vector's reference frame DREF. We use SPK rules for
!           determining the epoch, just as in SINCPT. Let DLTCTR be the
!           rate of change of light time between the frame center and
!           the observer.
!
!
!           Find the light time from the observer to the center of
!           frame DREF.
!
            CALL SPKACS ( DCTR,   ET,    'J2000', ABCORR, OBSID, &
                          CTRSTA, LTCTR, DLTCTR                 )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFSSIN' )
               RETURN
            END IF

            DRFEPC  =  ET  +  LTSIGN * LTCTR

!
!           Compute the state of the input direction vector in the
!           J2000 frame at DRFEPC. Correct the state transformation for
!           the rate of change of light time.
!
            CALL SXFORM   ( DREF, 'J2000', DRFEPC, DRXFRM )
            CALL ZZCORSXF ( XMIT, DLTCTR,  DRXFRM, DCORXF )

         END IF

!
!        The velocity of DVEC in frame DREF is zero.
!
         CALL MOVED  ( DVEC, 3, STEMP    )
         CALL CLEARD (       3, STEMP(4) )

         CALL MXVG ( DCORXF, STEMP, 6, 6, J2DSTA )

!
!        We'll transform J2DSTA to the target body-fixed frame at
!        the target epoch once we've computed the required
!        state transformation matrix. This occurs just before
!        we use this state vector in a call to SURFPV.
!
!        Most our work consists of getting ready to call the SPICELIB
!        routine SURFPV. In order to make this call, we'll need the
!        velocity of the observer relative to the target body's center
!        in the target body-fixed frame. We must evaluate the rotation
!        state of the target at the correct epoch, and account for the
!        rate of change of light time, if light time corrections are
!        used. The algorithm we use depends on the algorithm used in
!        SINCPT, since we're computing the derivative with respect to
!        time of the solution found by that routine.
!
!        In this algorithm, we must take into account the fact that
!        SINCPT performs light time and stellar aberration corrections
!        for the surface intercept point, not for the center of the
!        target body.
!
!        If light time and stellar aberration corrections are used,
!
!        - Find the aberration corrected surface intercept point and
!          the light time-corrected epoch TRGEPC associated
!          with the surface intercept point.
!
!        - Use TRGEPC to find the position of the target relative
!          to the solar system barycenter.
!
!        - Use TRGEPC to find the orientation of the target relative
!          to the J2000 reference frame.
!
!        - Find the light-time corrected position of the
!          surface intercept point; use this to compute the
!          stellar aberration offset that applies to the
!          surface intercept point, as well as the velocity of
!          this offset.
!
!        - Find the corrected state of the target center as seen
!          from the observer, where the corrections are those
!          applicable to the surface intercept point.
!
!        - Negate the corrected target center state to obtain
!          the state of the observer relative to the target.
!
!        - Express the state of the observer relative to the
!          target in the target body fixed frame at TRGEPC.
!
!
!        Below, we'll use the convention that vectors expressed
!        relative to the body-fixed frame have names of the form
!
!        FX*
!
!        Note that SINCPT will signal an error if FIXREF is not
!        actually centered on the target body.
!
         CALL SINCPT ( METHOD, SVTARG, ET,     FIXREF, ABCORR, SVOBS, &
                       DREF,   DVEC,   SPOINT, TRGEPC, SRFVEC, FOUND )
!
!        It's not an error for SINCPT to be unable to compute an
!        intercept point; return now if the intercept was not found.
!
         IF ( .NOT. FOUND ) THEN
            CALL CHKOUT ( 'ZZGFSSIN' )
            RETURN
         END IF

!
!        Get J2000-relative states of observer and target with respect
!        to the solar system barycenter at their respective epochs of
!        participation.
!
         CALL SPKSSB ( OBSID, ET,     'J2000', SSBOBS )
         CALL SPKSSB ( TRGID, TRGEPC, 'J2000', SSBTG0 )

!
!        Get the uncorrected J2000 to body-fixed to state
!        transformation at TRGEPC.
!
         CALL SXFORM ( 'J2000', FIXREF, TRGEPC, XFORM )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'ZZGFSSIN' )
            RETURN
         END IF

!
!        Initialize the state of the surface intercept point in the
!        body-fixed frame. At this point we don't know the point's
!        velocity; set it to zero.
!
         CALL MOVED  ( SPOINT, 3, FXPSTA    )
         CALL CLEARD (         3, FXPSTA(4) )

         IF ( USESTL ) THEN
!
!           We're going to need the acceleration of the observer
!           relative to the SSB. Compute this now.
!
            DO I = 1, 2
!
!              The epoch is ET -/+ TDELTA.
!
               T =  ET  + (2*I-3)*TDELTA

               CALL SPKSSB ( OBSID,  T,  'J2000', OBSSTA(1,I) )

            END DO

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'ZZGFSSIN' )
               RETURN
            END IF

!
!           Compute the observer's acceleration using a quadratic
!           approximation.
!
            CALL QDERIV ( 3, OBSSTA(4,1), OBSSTA(4,2), TDELTA, ACC )

         END IF

!
!        The rest of the algorithm is iterative. On the first
!        iteration, we don't have a good estimate of the velocity
!        of the surface intercept point relative to the body-fixed
!        frame. Since we're using this velocity as an input
!        to the aberration velocity computations, we
!        expect that treating this velocity as zero on the first
!        pass yields a reasonable estimate. On the second pass,
!        we'll use the velocity derived on the first pass.
!
         CALL CLEARD ( 3, FXPVEL )

!
!        We'll also estimate the rate of change of light time
!        as zero on the first pass.
!
         DLT = 0.D0

         DO I = 1, 3
!
!           Correct the target's velocity for the rate of
!           change of light time.
!
            IF ( XMIT ) THEN
               SCALE = 1.D0 + DLT
            ELSE
               SCALE = 1.D0 - DLT
            END IF

!
!           Scale the velocity portion of the target state to
!           correct the velocity for the rate of change of light
!           time.
!
            CALL MOVED ( SSBTG0, 3,         SSBTRG    )
            CALL VSCL  ( SCALE,  SSBTG0(4), SSBTRG(4) )

!
!           Get the state of the target with respect to the observer.
!
            CALL VSUBG ( SSBTRG, SSBOBS, 6, OBSTRG )

!
!           Correct the J2000 to body-fixed state transformation matrix
!           for the rate of change of light time.
!
            CALL ZZCORSXF ( XMIT, DLT, XFORM, CORXFM )

!
!           Invert CORXFM to obtain the corrected
!           body-fixed to J2000 state transformation.
!
            CALL INVSTM ( CORXFM, CORXFI )

!
!           Convert the surface intercept point state to the J2000
!           frame.
!
            CALL MXVG ( CORXFI, FXPSTA, 6, 6, PNTSTA )

!
!           Find the J2000-relative state of the surface intercept
!           point with respect to the target.
!
            CALL VADDG ( OBSTRG, PNTSTA, 6, OBSPNT )

            IF ( USESTL ) THEN
!
!              Now compute the stellar aberration correction
!              applicable to OBSPNT. We need the velocity of
!              this correction as well.
!
               CALL ZZSTELAB ( XMIT, ACC, SSBOBS(4), OBSPNT, SA, SAVEL )

               CALL MOVED ( SA,    3, SASTAT    )
               CALL MOVED ( SAVEL, 3, SASTAT(4) )
!
!              Adding the stellar aberration state to the target center
!              state gives us the state of the target center with
!              respect to the observer, corrected for the aberrations
!              applicable to the surface intercept point.
!
               CALL VADDG ( OBSTRG, SASTAT, 6, STEMP )

            ELSE
               CALL MOVED ( OBSTRG, 6, STEMP )
            END IF

!
!           Convert STEMP to the body-fixed reference frame.
!
            CALL MXVG ( CORXFM, STEMP, 6, 6, FXTSTA )

!
!           At long last, compute the state of the observer
!           with respect to the target in the body-fixed frame.
!
            CALL VMINUG ( FXTSTA, 6, FXOSTA )

!
!           Transform the state of the direction vector from the
!           J2000 frame to the target body-fixed frame at TRGEPC.
!
            CALL MXVG ( CORXFM, J2DSTA, 6, 6, FXDSTA )

!
!           Now we can obtain the surface velocity of the
!           surface intercept point.
!
            CALL SURFPV ( FXOSTA,   FXDSTA,   RADII(1), &
                          RADII(2), RADII(3), FXPSTA,   FOUND )

!
!           It's not an error for SURFPV to be unable to compute an
!           intercept state; return now if the state was not
!           computable.
!
            IF ( .NOT. FOUND ) THEN
               CALL CHKOUT ( 'ZZGFSSIN' )
               RETURN
            END IF

!
!           At this point we can update the surface point
!           velocity and light time derivative estimates.
!
!           In order to compute the light time rate, we'll
!           need the J2000-relative velocity of the surface intercept
!           point with respect to the observer. First convert
!           the surface intercept state to the J2000 frame, then
!           add the result to the state of the target center
!           with respect to the observer.
!
            CALL MXVG  ( CORXFI, FXPSTA, 6, 6, PNTSTA )
            CALL VADDG ( OBSTRG, PNTSTA, 6,    OBSPNT )

!
!           Now that we have an improved estimate of the
!           surface intercept state, we can estimate the rate of
!           change of light time as
!
!              range rate
!              ----------
!                  c
!
!
!           If we're correcting for stellar aberration, *ideally* we
!           should remove that correction now, since the light time
!           rate is based on light time between the observer and the
!           light-time corrected surface intercept point. But the error
!           made by including stellar aberration is too small to make
!           it worthwhile to make this adjustment.
!
            CALL VHAT ( OBSPNT, UPOS )

            DLT = VDOT ( OBSPNT(4), UPOS ) / CLIGHT()

!
!           With FXPVEL and DLT updated, we'll repeat our
!           computations.
!
         END DO

      ELSE
!
!        We should never get here.
!
         CALL SETMSG ( 'Aberration correction # was not ' &
         //            'recognized.'                      )
         CALL ERRCH  ( '#',  ABCORR                       )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'              )
         CALL CHKOUT ( 'ZZGFSSIN'                         )
         RETURN

      END IF

!
!     Copy the computed state to the output argument STATE.
!     FOUND has already been set to .TRUE. by SURFPV.
!
      CALL MOVED ( FXPSTA, 6, STATE )

      CALL CHKOUT  ( 'ZZGFSSIN' )
      RETURN
      END
