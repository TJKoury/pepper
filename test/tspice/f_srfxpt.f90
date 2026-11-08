
!$Procedure      F_SRFXPT ( SRFXPT family tests )

      SUBROUTINE F_SRFXPT ( OK )
      IMPLICIT NONE

      LOGICAL               OK

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        SRFXPT
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 23-FEB-2004 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               EQSTR

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK   = 'test_0008.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'srfxpt_spk.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2  = 'orbiter.bsp' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT  = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT   = 1.D-12 )

      DOUBLE PRECISION      MTIGHT
      PARAMETER           ( MTIGHT  = 1.D-10 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-8 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-5 )

      DOUBLE PRECISION      SLOPPY
      PARAMETER           ( SLOPPY = 1.D-3 )


      INTEGER               UBEL
      PARAMETER           ( UBEL    = 9 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF = 3 )

      INTEGER               NABC
      PARAMETER           ( NABC = 9 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 10 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NOBS
      PARAMETER           ( NOBS   = 2 )

      INTEGER               NGEOM
      PARAMETER           ( NGEOM  = 4 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499 )

!
!     Local variables
!
      CHARACTER*(ABCLEN)    ABCS    ( NABC )
      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(NAMLEN)    DREF
      CHARACTER*(LNSIZE)    GEOM
      CHARACTER*(LNSIZE)    GEOMS   ( NGEOM )
      CHARACTER*(NAMLEN)    OBSRVR
      CHARACTER*(NAMLEN)    OBSNMS  ( NOBS )
      CHARACTER*(NAMLEN)    REFS    ( NREF )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      DEPOCH
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DJ2     ( 3 )
      DOUBLE PRECISION      DJ2M    ( 3, 3 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DVEC    ( 3 )
      DOUBLE PRECISION      DVECFX  ( 3 )
      DOUBLE PRECISION      DVECJ2  ( 3 )
      DOUBLE PRECISION      DVECST  ( 3 )
      DOUBLE PRECISION      ELTS    ( 8 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETOL
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      J2OBS   ( 3 )
      DOUBLE PRECISION      LCENTR  ( 3 )
      DOUBLE PRECISION      LIMB    ( UBEL )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NEGJ2   ( 3 )
      DOUBLE PRECISION      NEGVEC  ( 3 )
      DOUBLE PRECISION      OBSJ2   ( 3 )
      DOUBLE PRECISION      OBSPOS  ( 3 )
      DOUBLE PRECISION      OBSVEC  ( 3 )
      DOUBLE PRECISION      RADII   ( 3 )
      DOUBLE PRECISION      REFPOS  ( 3 )
      DOUBLE PRECISION      RLT
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SMAJOR  ( 3 )
      DOUBLE PRECISION      SMINOR  ( 3 )
      DOUBLE PRECISION      SPJ2    ( 3 )
      DOUBLE PRECISION      SPLT    ( 3 )
      DOUBLE PRECISION      SPOINT  ( 3 )
      DOUBLE PRECISION      SPPOS   ( 3 )
      DOUBLE PRECISION      SPVEC   ( 3 )
      DOUBLE PRECISION      SSBOBS  ( 6 )
      DOUBLE PRECISION      SSBTRG  ( 6 )
      DOUBLE PRECISION      STATE0  ( 6 )
      DOUBLE PRECISION      TE
      DOUBLE PRECISION      TGT     ( 3 )
      DOUBLE PRECISION      TIPFX   ( 3 )
      DOUBLE PRECISION      TIPJ2   ( 3 )
      DOUBLE PRECISION      TIPM    ( 3, 3 )
      DOUBLE PRECISION      TLT
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGJ2M  ( 3, 3 )
      DOUBLE PRECISION      XDIST
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XFORM   ( 3, 3 )
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XOBSPS  ( 3 )
      DOUBLE PRECISION      XRAY    ( 3 )
      DOUBLE PRECISION      XRTRG   ( 3 )
      DOUBLE PRECISION      XSPNT   ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               CLS
      INTEGER               CLSSID
      INTEGER               DREFID
      INTEGER               FRCODE
      INTEGER               GEOIDX
      INTEGER               HANDLE ( 2 )
      INTEGER               I
      INTEGER               N
      INTEGER               NITR
      INTEGER               OBSCDE
      INTEGER               OBSIDX
      INTEGER               REFCTR
      INTEGER               REFIDX
      INTEGER               TRGCDE

      LOGICAL               FND
      LOGICAL               FOUND
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL
      LOGICAL               XMIT

!
!     Saved variables
!
      SAVE                  ABCS
      SAVE                  REFS

!
!     Initial values
!
      DATA                  ABCS   /  'None', &
                                      'Lt', &
                                      'Lt+s', &
                                      'Cn', &
                                      'Cn+s', &
                                      'Xlt', &
                                      'Xlt+s', &
                                      'Xcn', &
                                      'Xcn+s'  /

      DATA                  REFS   /  'J2000', &
                                      'IAU_MARS', &
                                      'IAU_EARTH'  /

      DATA                  GEOMS  /  'POINT_AT_CENTER', &
                                      'MISS_BACKWARD', &
                                      'LIMB_INSIDE_NEAR', &
                                      'MISS_LIMB_NEAR'    /

      DATA                  OBSNMS /  'Earth', &
                                      'MARS_ORBITER'    /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SRFXPT' )

      CALL TCASE ( 'Setup:  create SPK, PCK file.' )

      CALL TSTSPK ( SPK1, .TRUE., HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the PCK file, load it, and delete it.
!
      CALL T_PCK08 ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create LSK, load it, and delete it.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set time.
!
      UTC = '2004 FEB 17'
      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a Mars orbiter SPK file.
!
      CALL SPKOPN ( SPK2, SPK2, 0, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set up elements defining a state.  The elements expected
!     by CONICS are:
!
!        RP      Perifocal distance.
!        ECC     Eccentricity.
!        INC     Inclination.
!        LNODE   Longitude of the ascending node.
!        ARGP    Argument of periapse.
!        M0      Mean anomaly at epoch.
!        T0      Epoch.
!        MU      Gravitational parameter.
!
      ELTS(1) =  3800.D0
      ELTS(2) =     1.D-1
      ELTS(3) =    80.D0 * RPD()
      ELTS(4) =     0.D0
      ELTS(5) =    90.D0 * RPD()
      ELTS(6) =     0.D0
      ELTS(7) =    ET
      ELTS(8) = 42828.314D0

      CALL CONICS ( ELTS, ET, STATE0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKW05 ( HANDLE(2),    SCID,         499,      'MARSIAU', &
                    -10*JYEAR(),  10*JYEAR(),  'Mars orbiter', &
                    ELTS(8),      1,            STATE0,    ET        )

      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKCLS ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load the new SPK file.
!
      CALL SPKLEF ( SPK2, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add the orbiter's name/ID mapping to the kernel pool.
!
      CALL PCPOOL ( 'NAIF_BODY_NAME', 1,  OBSNMS(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL PIPOOL ( 'NAIF_BODY_CODE', 1,  SCID      )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add an incomplete frame definition to the kernel pool;
!     we'll need this later.
!
      CALL PIPOOL ( 'FRAME_BAD_NAME', 1, -666 )

!
!     Start out with consistency checks:  having found SPOINT,
!     find the aberration corrected location of SPOINT by
!     direct computation, and compare results to those from
!     SRFXPT.
!
!     Test cases for a distant viewer:  ray emanates from earth's
!     center, points towards Mars' center.


!
!     Set target.  Get target code, target body-fixed frame
!     name.
!
      TARGET = 'Mars'

      CALL BODN2C ( TARGET, TRGCDE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CNMFRM ( TARGET, FRCODE, TRGFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Get target radii.
!
      CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Loop over every choice of observer.
!
      DO OBSIDX = 1, NOBS

         OBSRVR = OBSNMS ( OBSIDX )
!
!        Set the observer ID code.
!
         CALL BODN2C ( OBSRVR, OBSCDE, FOUND )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Loop over every viewing geometry case.
!
         DO GEOIDX = 1, NGEOM

            GEOM = GEOMS ( GEOIDX )

!
!           Loop over every aberration correction choice.
!
            DO ABCIDX = 1, NABC

               ABCORR = ABCS ( ABCIDX )

!
!              Set up some logical variables describing the
!              attributes of the selected correction.
!
               USELT  = ABCORR .NE. 'None'
               XMIT   = ABCORR(1:1) .EQ. 'X'

               USECN  =       ( ABCORR(1:2) .EQ. 'Cn'  ) &
                         .OR. ( ABCORR(1:3) .EQ. 'Xcn' )

               USESTL = INDEX ( ABCORR, '+s' )  .NE.  0

!
!              Loop over every direction vector frame choice.
!
               DO REFIDX = 1, NREF

                  DREF = REFS ( REFIDX )
!
!                 Set light time RLT from observer to center of frame
!                 for the direction vector.
!
                  CALL NAMFRM ( DREF,   DREFID )
                  CALL FRINFO ( DREFID, REFCTR, CLS, CLSSID, FND )

                  CALL SPKEZP ( REFCTR, ET, 'J2000', ABCORR, OBSCDE, &
                                REFPOS, RLT                         )

!
!                 We'll need the epoch DEPOCH associated
!                 with the center of DREF.  RLT is the
!                 light time from DREF's center to the observer.
!
                  IF ( USELT ) THEN

                     IF ( XMIT ) THEN
                        DEPOCH = ET + RLT
                     ELSE
                        DEPOCH = ET - RLT
                     END IF

                  ELSE
                     DEPOCH = ET
                  END IF

!
!                 Look up the transformation from frame DREF to J2000.
!                 We don't need this right away, but we'll have
!                 occasion to use it later.
!
                  CALL PXFORM ( DREF, 'J2000', DEPOCH, DJ2M )


                  TITLE = 'Observer = #.  Geometry = #. ' // &
                          'ABCORR = #; DREF = #.'
                  CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                  CALL REPMC ( TITLE, '#', GEOM,   TITLE )
                  CALL REPMC ( TITLE, '#', ABCORR, TITLE )
                  CALL REPMC ( TITLE, '#', DREF,   TITLE )

                  CALL TCASE ( TITLE )

                  IF ( GEOM .EQ. 'POINT_AT_CENTER' ) THEN
!
!                    Look up direction vector using current frame and
!                    aberration correction.  The direction vector is
!                    going to point to the target's center, so we
!                    should hit the target.
!
                     CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, &
                                   OBSRVR, DVEC, DLT          )
                     CALL CHCKXC ( .FALSE., ' ', OK )


                  ELSE IF ( GEOM .EQ. 'MISS_BACKWARD' ) THEN
!
!                    Set the pointing direction to the inverse
!                    of that obtained in the 'POINT_AT_CENTER'
!                    case.
!
                     CALL SPKPOS ( TARGET, ET,     DREF, ABCORR, &
                                   OBSRVR, NEGVEC, DLT          )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL VMINUS ( NEGVEC, DVEC )


                  ELSE IF (     ( GEOM .EQ. 'LIMB_INSIDE_NEAR' ) &
                           .OR. ( GEOM .EQ. 'MISS_LIMB_NEAR'   ) ) THEN
!
!                    Find the limb of the target based on
!                    the aberration-corrected target center position.
!                    Select ray to hit limb along major axis, slightly
!                    inside or slightly outside the ellipse, depending
!                    on the geometry case.
!
!                    Note we're looking up the target state in
!                    the target's body-fixed frame, not in the
!                    DREF frame.
!
                     CALL SPKPOS ( TARGET, ET,     TRGFRM, ABCORR, &
                                   OBSRVR, NEGVEC, TLT            )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL VMINUS ( NEGVEC, OBSVEC )

!
!                    Get the limb's center and semi-axis vectors.
!
                     CALL EDLIMB ( RADII(1), RADII(2), RADII(3), &
                                   OBSVEC,   LIMB               )

                     CALL EL2CGV ( LIMB, LCENTR, SMAJOR, SMINOR )
!
!                    To get an accurate limb, we'll find the light
!                    time from observer to tip of semi-major axis
!                    and get an improved light time estimate.
!
                     CALL VADD ( LCENTR, SMAJOR, TIPFX )

                     IF ( USELT ) THEN

                        IF ( XMIT )  THEN
                           TE  =  ET + TLT
                        ELSE
                           TE  =  ET - TLT
                        END IF

                     ELSE
                        TE = ET
                     END IF

!
!                    Get the "tip" in the J2000 frame at epoch TE.
!
                     CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

                     CALL MTXV ( TIPM, TIPFX, TIPJ2 )

!
!                    Get state of observer relative to SSB at
!                    ET and state of target relative to SSB at
!                    TE.
!
                     CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOBS )
                     CALL SPKSSB ( TRGCDE, TE, 'J2000', SSBTRG )

!
!                    Get the position of the tip.  Compute a
!                    new light time value and target epoch.
!
                     CALL VADD ( SSBTRG, TIPJ2,  TGT    )

                     TLT = VDIST ( SSBOBS, TGT ) / CLIGHT()

                     IF ( USELT ) THEN

                        IF ( XMIT )  THEN
                           TE  =  ET + TLT
                        ELSE
                           TE  =  ET - TLT
                        END IF

                     ELSE
                        TE = ET
                     END IF

!
!                    Re-compute TIPM.
!
                     CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

!
!                    Get the observer position in the target body-fixed
!                    frame at TE.
!
                     CALL VSUB ( SSBOBS, SSBTRG, OBSJ2  )
                     CALL MXV  ( TIPM,   OBSJ2,  OBSVEC )

!
!                    Get the limb's center and semi-axis vectors.
!
                     CALL EDLIMB ( RADII(1), RADII(2), RADII(3), &
                                   OBSVEC,   LIMB               )

                     CALL EL2CGV ( LIMB, LCENTR, SMAJOR, SMINOR )

!
!                    Pick our target point near the limb.  The
!                    point is 1+/- DELTA of the semi-major axis length
!                    out from the center, along one of the semi-
!                    major axes.
!
                     DELTA = 0.001D0

                     IF ( GEOM(1:4) .EQ. 'MISS' ) THEN
                        FRAC = 1.D0 + DELTA
                     ELSE
                        FRAC = 1.D0 - DELTA
                     END IF

                     CALL VLCOM( 1.D0, LCENTR, FRAC, SMAJOR, TGT )

!
!                    Our ray extends from the observer to the target
!                    point.
!
                     CALL VSUB ( TGT, OBSVEC, DVECFX )

                     SEP = VSEP ( NEGVEC, DVECFX )

!
!                    Convert the ray from the target body fixed
!                    frame to J2000, then from J2000 to the DREF
!                    frame.  We need the target frame epoch TE
!                    to find the first transformation matrix TIPM.
!
                     IF ( USELT ) THEN

                        IF ( XMIT )  THEN
                           TE  =  ET + TLT
                        ELSE
                           TE  =  ET - TLT
                        END IF

                     ELSE
                        TE = ET
                     END IF

                     CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

                     CALL MTXV ( TIPM, DVECFX, DVECJ2 )

!
!                    If we're using stellar aberration correction,
!                    apply the correction to our ray direction.
!
                     IF ( USESTL ) THEN

                        IF ( XMIT ) THEN
                           CALL STLABX ( DVECJ2, SSBOBS(4), DVECST )
                        ELSE
                           CALL STELAB ( DVECJ2, SSBOBS(4), DVECST )
                        END IF

                        CALL VEQU ( DVECST, DVECJ2 )

                     END IF
!
!                    The matrix DJ2M maps from DREF to J2000, so
!                    apply the transpose of this matrix to obtain
!                    DVEC.
!
                     CALL MTXV ( DJ2M, DVECJ2, DVEC )

                  ELSE
!
!                    Oops!  Name mismatch.
!
                     CALL SIGERR ( 'SPICE(BUG)' )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                  END IF

!
!                 Find the surface intercept point.
!
                  CALL SRFXPT ( 'Ellipsoid', TARGET,  ET,      ABCORR, &
                                OBSRVR,      DREF,    DVEC,    SPOINT, &
                                DIST,        TRGEPC,  OBSPOS,  FOUND  )

                  CALL CHCKXC ( .FALSE., ' ', OK )


!
!                 Check the results.
!
                  IF ( .NOT. FOUND ) THEN

                     IF ( GEOM(1:4) .EQ. 'MISS' ) THEN
!
!                       FOUND should be .FALSE.; the other outputs
!                       are undefined.
!
                        CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

                     ELSE
!
!                       We're supposed to have an intercept.
!
                        CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     END IF

                  ELSE
!
!                    FOUND is true.
!
!                    Form the vector from the observer to the target
!                    intercept.
!
                     CALL VSUB ( SPOINT, OBSPOS, SPVEC )

!
!                    The length of SPVEC had better be DIST.
!
                     CALL CHCKSD ( 'DIST',       DIST,   '~', &
                                   VNORM(SPVEC), VTIGHT, OK  )

!
!                    The target epoch had better be consistent with
!                    DIST and ABCORR.
!
                     XLT  = DIST/CLIGHT()

                     IF ( USELT ) THEN

                        IF ( XMIT ) THEN
                           XEPOCH = ET + XLT
                        ELSE
                           XEPOCH = ET - XLT
                        END IF

                     ELSE

                        XEPOCH = ET

                     END IF

!
!                    This is a relative error check.
!
                     IF ( USECN ) THEN
                        ETOL = TIGHT
                     ELSE
                        ETOL = MEDIUM
                     END IF

                     CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', &
                                    XEPOCH,  ETOL,    OK  )

!
!                    Get the transformation from the target frame
!                    to J2000.
!
                     CALL PXFORM ( TRGFRM, 'J2000', TRGEPC, TRGJ2M )

!
!                    Now transform DVEC to the J2000 frame.
!
                     CALL MXV ( DJ2M, DVEC, DJ2 )

!
!                    The following check applies only to the case in
!                    which the pointing direction is toward the
!                    target's center.
!
                     IF ( GEOM .EQ. 'POINT_AT_CENTER' ) THEN
!
!                       The angular separation of -OBSPOS and DVEC
!                       should be pretty small when these vectors are
!                       compared in compatible reference frames.  We
!                       don't expect these vectors to be identical
!                       (even theoretically) because they've been
!                       computed with different target epochs.
!                       Furthermore, if stellar aberration correction
!                       is used, DVEC will be corrected for stellar
!                       aberration but OBSPOS will not, leading to even
!                       greater disparity.
!
!                       First step:  get -OBSPOS in the J2000 frame.
!

                        CALL VMINUS ( OBSPOS, NEGVEC )

                        CALL MXV    ( TRGJ2M, NEGVEC, NEGJ2 )

!
!                       Find the angular separation and make sure it's
!                       not too large.
!
                        SEP = VSEP ( NEGJ2, DJ2 )

                        CALL CHCKSD( 'DJ2 vs NEG2 SEP', SEP,   '~', &
                                      0.D0,             SLOPPY, OK )

                     END IF
!
!                    End of sanity check test for the POINT_AT_CENTER
!                    case.
!
!                    Having made it this far, we're ready for some more
!                    rigorous tests.  In particular, we're going treat
!                    SPOINT as an ephemeris object and find its
!                    aberration-corrected position relative to the
!                    observer in J2000 coordinates. This computation
!                    will allow us to derive expected values of TRGEPC,
!                    OBSPOS, the transformation from the J2000 frame to
!                    the target body-fixed frame at TRGEPC.  We will
!                    verify that the aberration-corrected location
!                    of SPOINT, lies on the ray DVEC:  this is the
!                    the criterion we used to define SPOINT.
!
!                    These tests make sense only when aberration
!                    corrections are used.
!
                     IF ( USELT ) THEN
!
!                       We're expecting to get good agreement between
!                       all of these items and their counterparts
!                       obtained from SRFXPT, especially when use use
!                       converged Newtonian corrections.
!
                        IF (  EQSTR(OBSRVR, 'EARTH') ) THEN

                           IF ( USECN ) THEN
                              TOL = TIGHT
                           ELSE
                              TOL = MEDIUM
                           END IF

                        ELSE
!
!                          Use looser tolerances for the Mars
!                          orbiter.  For the orbiter, small errors
!                          in SPOINT lead to larger relative errors
!                          in DIST and SEP.
!
                           IF ( USECN ) THEN
                              TOL = MTIGHT
                           ELSE
                              TOL = LOOSE
                           END IF

                        END IF

!
!                       Find the aberration-corrected location of
!                       SPOINT.
!
!                       We need the J2000 state of the observer relative
!                       to the solar system barycenter at ET.
!
                        CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOBS )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       The SPOINT re-computation is done iteratively.
!                       Since we're starting with a geometric target
!                       position, the light time value obtained from
!                       the normal light time correction corresponds to
!                       the light time found on the *second* iteration.
!                       We increment our iteration counts by 1 for both
!                       the normal and CN cases.
!
                        IF ( USECN ) THEN
                           NITR = 5
                        ELSE
                           NITR = 2
                        END IF

!
!                       The initial target position relative to the
!                       solar system barycenter is found by summing the
!                       target center position relative to the solar
!                       system barycenter at ET with SPOINT, after
!                       SPOINT has been converted to the J2000 frame at
!                       ET.
!
                        CALL SPKSSB ( TRGCDE, ET, 'J2000', SSBTRG )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL PXFORM (  'J2000', TRGFRM, ET,  TIPM )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL MTXV   ( TIPM,   SPOINT,  SPJ2  )
                        CALL VADD   ( SSBTRG, SPJ2,    SPPOS )

                        DO I = 1, NITR
!
!                          Make a new estimate of the target epoch XTE.
!
                           LT = VDIST ( SSBOBS, SPPOS ) / CLIGHT()

                           IF ( XMIT ) THEN
                              XTE  =  ET + LT
                           ELSE
                              XTE  =  ET - LT
                           END IF

!
!                          Compute the J2000 state of SPOINT relative
!                          to the solar system barycenter at XTE.
!
                           CALL SPKSSB ( TRGCDE, XTE, 'J2000', SSBTRG )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL PXFORM ( 'J2000', TRGFRM, XTE,  TIPM )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL MTXV   ( TIPM,   SPOINT,  SPJ2  )
                           CALL VADD   ( SSBTRG, SPJ2,    SPPOS )

                        END DO
!
!                       Compute the light-time corrected position of
!                       SPOINT as seen by the observer.
!
                        CALL VSUB ( SPPOS, SSBOBS, SPLT )
!
!                       Correct SPLT for stellar aberration, if ABCORR
!                       so indicates.
!
                        IF ( USESTL ) THEN

                           IF ( XMIT ) THEN
                              CALL STLABX ( SPLT, SSBOBS(4), XRAY )
                           ELSE
                              CALL STELAB ( SPLT, SSBOBS(4), XRAY )
                           END IF

                        ELSE
!
!                          XRAY is our expected result.
!
                           CALL VEQU ( SPLT, XRAY )

                        END IF

!
!                       Moment of truth:  XRAY is the J2000 vector from
!                       the observer to the aberration-corrected
!                       position of our "ephemeris object" located on
!                       the target surface at location SPOINT.  If
!                       SPOINT were correct in the first place, then
!                       XRAY should be lined up with our boresight
!                       direction DVEC, when DVEC is rotated to the
!                       J2000 frame.
!
!                       Actually, we computed DVEC in the J2000 frame
!                       long ago:  this vector is called DJ2.
!
                        CALL CHCKSD ( 'TRGEPC vs XTE', &
                                       TRGEPC, '~/', XTE, TOL, OK )

                        SEP = VSEP ( DJ2, XRAY )

                        CALL CHCKSD ( 'XRAY vs DJ2 sep', &
                                       SEP, '~', 0.D0, TOL, OK )

!
!                       Check DIST against its predicted value.
!
                        CALL CHCKSD ( 'DIST',      DIST, '~/', &
                                      VNORM(XRAY), TOL,   OK   )

!
!                       Create a predicted value for OBSPOS:  the
!                       difference of SSBOBS and SSBTRG from our last
!                       loop iteration gives us OBSPOS in the J2000
!                       frame.  Use XTE to get the transformation to
!                       target body-fixed coordinates at epoch XTE.
!
                        CALL VSUB   ( SSBOBS,  SSBTRG, J2OBS )
                        CALL PXFORM ( 'J2000', TRGFRM, XTE,   XFORM )
                        CALL MXV    ( XFORM,   J2OBS,  XOBSPS )

                        CALL CHCKAD ( 'OBSPOS', OBSPOS, '~/', &
                                      XOBSPS,   3,      TOL,  OK )

!
!                       The following test only makes sense
!                       when stellar aberration is not used.
!                       (When stellar aberration IS used, SPOINT
!                       does not lie on the ray; the image of
!                       SPOINT under the stellar aberration correction
!                       lies on the ray.
!
                        IF ( .NOT. USESTL ) THEN
!
!                          Create a predicted value for SPOINT:  convert
!                          XRAY to target body-fixed coordinates at
!                          epoch XTE and add to XOBSPS to form XSPNT.
!
                           CALL MXV  ( XFORM,  XRAY,   XRTRG )
                           CALL VADD ( XRTRG,  XOBSPS, XSPNT )

                           CALL CHCKAD ( 'SPOINT', SPOINT, '~/', &
                                       XSPNT,   3,      MEDIUM,  OK )

                        END IF

                     END IF
!
!                    We're finished with the consistency checks for
!                    the aberration correction cases.
!
                  END IF
!
!                 End of the checks for the intercept cases.
!
               END DO
!
!              End of the aberration correction loop.
!
            END DO
!
!           End of the reference frame loop.
!
         END DO
!
!        End of the geometry case loop.
!
      END DO
!
!     End of the observer loop.
!


!
!     Input handling tests:  make sure target and observer
!     can be identified using integer "names."
!
      CALL TCASE ( 'Use integer observer and target names.' )
!
!     Set up the ray first.
!
      CALL SPKPOS ( '499', ET,   DREF, ABCORR, &
                    '399', DVEC, DLT          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SRFXPT ( 'Ellipsoid', 'MARS',   ET,      ABCORR, &
                    'Earth',      DREF,    DVEC,    XSPNT, &
                     XDIST,       XEPOCH,  XOBSPS,  FOUND  )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL SRFXPT ( 'Ellipsoid', '499',    ET,      ABCORR, &
                    '399',        DREF,    DVEC,    SPOINT, &
                     DIST,        TRGEPC,  OBSPOS,  FOUND  )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT,  3, 0.D0, OK )
      CALL CHCKAD ( 'OBSPOS', OBSPOS, '=', XOBSPS, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )
      CALL CHCKSD ( 'DIST',   DIST,   '=', DIST,      0.D0, OK )


!
!     Error handling tests follow.
!
      CALL TCASE ( 'Target name not translated' )
!
!     Find the surface intercept point.
!
      CALL SRFXPT ( 'Ellipsoid', 'xyz',   ET,      ABCORR, &
                    OBSRVR,      DREF,    DVEC,    SPOINT, &
                    DIST,        TRGEPC,  OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



      CALL TCASE ( 'Observer name not translated' )
!
!     Find the surface intercept point.
!
      CALL SRFXPT ( 'Ellipsoid', TARGET,  ET,      ABCORR, &
                    'xyz',       DREF,    DVEC,    SPOINT, &
                    DIST,        TRGEPC,  OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



      CALL TCASE ( 'Observer coincides with target' )
!
!     Find the surface intercept point.
!
      CALL SRFXPT ( 'Ellipsoid', TARGET,  ET,      ABCORR, &
                    TARGET,      DREF,    DVEC,    SPOINT, &
                    DIST,        TRGEPC,  OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )



      CALL TCASE ( 'Unsupported computation method' )
!
!     Find the surface intercept point.
!
      CALL SRFXPT ( 'xyz',       TARGET,  ET,      ABCORR, &
                    OBSRVR,      DREF,    DVEC,    SPOINT, &
                    DIST,        TRGEPC,  OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)', OK )



      CALL TCASE ( 'No body-fixed frame associated with target' )
!
!     Find the surface intercept point.
!
      CALL SRFXPT ( 'Ellipsoid', 'Mars_orbiter',  ET,      ABCORR, &
                    'EARTH',     DREF,            DVEC,    SPOINT, &
                    DIST,        TRGEPC,          OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )




      CALL TCASE ( 'Frame name maps to code but FRINFO can''t ' // &
                   'find frame info.'                           )
!
!     Find the surface intercept point.  Use reference frame 'BAD'
!     for direction vector.
!
      CALL SRFXPT ( 'Ellipsoid', 'Mars_orbiter',  ET,      ABCORR, &
                    'EARTH',     'BAD',           DVEC,    SPOINT, &
                    DIST,        TRGEPC,          OBSPOS,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


!
!     Clean up.
!
      CALL SPKUEF ( HANDLE(1) )
      CALL DELFIL ( SPK1      )

      CALL SPKUEF ( HANDLE(2) )
      CALL DELFIL ( SPK2      )

      CALL T_SUCCESS ( OK )

      END
