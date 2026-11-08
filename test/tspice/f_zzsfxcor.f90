!$Procedure      F_ZZSFXCOR ( ZZSFXCOR family tests )

      SUBROUTINE F_ZZSFXCOR ( OK )

! $ Abstract
!
!     Exercise the private SPICELIB geometry routine ZZSFXCOR.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'zzabcorr.inc'

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK         is a logical that indicates the test status to the
!                caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     This routine does not generate any errors. Routines in its
!     call tree may generate errors that are either intentional and
!     trapped or unintentional and need reporting.  The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the SPICELIB private
!     geometry routine ZZSFXCOR.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 06-AUG-2016 (NJB)
!
!       Based on f_sincpt.for version 2.0.0, 02-APR-2012
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
      LOGICAL               EXISTS

!
!     External routines
!
      EXTERNAL              ZZMAXRAD
      EXTERNAL              ZZRAYNP
      EXTERNAL              ZZRAYSFX

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK   = 'test_0008.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'srfxpt_spk.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2  = 'orbiter.bsp' )


!      DOUBLE PRECISION      VTIGHT
!      PARAMETER           ( VTIGHT  = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT   = 1.D-12 )

      DOUBLE PRECISION      MTIGHT
      PARAMETER           ( MTIGHT  = 1.D-10 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-8 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 5.D-6 )

      DOUBLE PRECISION      SLOPPY
      PARAMETER           ( SLOPPY = 1.D-3 )


      INTEGER               UBEL
      PARAMETER           ( UBEL    = 9 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF = 4 )

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
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(LNSIZE)    GEOM
      CHARACTER*(LNSIZE)    GEOMS   ( NGEOM )
      CHARACTER*(NAMLEN)    OBSRVR
      CHARACTER*(NAMLEN)    OBSNMS  ( NOBS )
      CHARACTER*(NAMLEN)    REFS    ( NREF )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DEPOCH
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DJ2     ( 3 )
      DOUBLE PRECISION      DJ2M    ( 3, 3 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DVEC    ( 3 )
      DOUBLE PRECISION      DVECFX  ( 3 )
      DOUBLE PRECISION      DVECJ2  ( 3 )
      DOUBLE PRECISION      ELTS    ( 8 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETOL
      DOUBLE PRECISION      FIXCOR  ( 3 )
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      J2OBS   ( 3 )
      DOUBLE PRECISION      LCCOR   ( 3 )
      DOUBLE PRECISION      LCENTR  ( 3 )
      DOUBLE PRECISION      LIMB    ( UBEL )
      DOUBLE PRECISION      LT
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
      DOUBLE PRECISION      SRFVEC  ( 3 )
      DOUBLE PRECISION      SRFVJ2  ( 3 )
      DOUBLE PRECISION      SSBOBS  ( 6 )
      DOUBLE PRECISION      SSBTRG  ( 6 )
      DOUBLE PRECISION      STATE0  ( 6 )
      DOUBLE PRECISION      STLCOR  ( 3 )
      DOUBLE PRECISION      TE
      DOUBLE PRECISION      TGT     ( 3 )
      DOUBLE PRECISION      TIPCOR  ( 3 )
      DOUBLE PRECISION      TIPFX   ( 3 )
      DOUBLE PRECISION      TIPJ2   ( 3 )
      DOUBLE PRECISION      TIPM    ( 3, 3 )
      DOUBLE PRECISION      TIPVEC  ( 3 )
      DOUBLE PRECISION      TLT
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGJ2M  ( 3, 3 )
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XFORM   ( 3, 3 )
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XOBSPS  ( 3 )
      DOUBLE PRECISION      XRAY    ( 3 )
      DOUBLE PRECISION      XSPNT   ( 3 )
      DOUBLE PRECISION      XSRFVC  ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               DCENTR
      INTEGER               DCLASS
      INTEGER               DCLSID
      INTEGER               DREFID
      INTEGER               FRCODE
      INTEGER               GEOMIX
      INTEGER               HANDLE ( 2 )
      INTEGER               I
      INTEGER               N
      INTEGER               NITR
      INTEGER               OBSCDE
      INTEGER               OBSIDX
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
                                      'ECLIPJ2000', &
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
      CALL TOPEN ( 'F_ZZSFXCOR' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create SPK, PCK file.' )

      CALL TSTSPK ( SPK1, .TRUE., HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the PCK file, load it, and delete it.
!
      IF ( EXISTS(PCK) ) THEN
         CALL DELFIL ( PCK )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

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
      IF ( EXISTS(SPK2) ) THEN
         CALL DELFIL ( SPK2 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

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
!     ZZSFXCOR.
!
!     Test cases for a distant viewer:  ray emanates from earth's
!     center, points towards Mars' center.


!
!     Set target.  Get target code, target body-fixed frame
!     name.
!
      TARGET = 'Mars'
      FIXREF = 'IAU_MARS'

      CALL BODN2C ( TARGET, TRGCDE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CNMFRM ( TARGET, FRCODE, TRGFRM, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Initialize the surface intercept utilities. For
!     ellipsoidal target models, just the target body ID
!     is needed.
!
      CALL ZZSUELIN ( TRGCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get target radii.
!
      CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Top of main loop.' )

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
         DO GEOMIX = 1, NGEOM

            GEOM = GEOMS ( GEOMIX )

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
                  CALL FRINFO ( DREFID, DCENTR, DCLASS, DCLSID, FND )

                  CALL SPKEZP ( DCENTR, ET, 'J2000', ABCORR, OBSCDE, &
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

!
! --- Case: ------------------------------------------------------
!

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
!                    Find the limb of the target based on the
!                    aberration-corrected target center position.
!                    Select ray to hit limb plane along major axis,
!                    slightly inside or slightly outside the ellipse,
!                    depending on the geometry case.
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
!                    Improve the limb if we're using aberration
!                    corrections.
!
!                    To get an accurate limb, we'll find the light
!                    time from observer to tip of semi-major axis
!                    and get an improved light time estimate.
!
                     IF ( USELT ) THEN

                        IF ( USECN ) THEN
                           NITR = 4
                        ELSE
                           NITR = 1
                        END IF

                        IF ( XMIT )  THEN
                           TE  =  ET + TLT
                        ELSE
                           TE  =  ET - TLT
                        END IF

!
!                       Get state of observer relative to SSB at ET.
!
                        CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOBS )

                        DO I = 1, NITR
!
!                          Get the "tip" in the J2000 frame at epoch TE.
!
                           CALL VADD ( LCENTR, SMAJOR, TIPFX )

                           CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

                           CALL MTXV ( TIPM, TIPFX, TIPJ2 )

!
!                          Get state of target relative to SSB at TE.
!
                           CALL SPKSSB ( TRGCDE, TE, 'J2000', SSBTRG )

!
!                          Get the position of the tip.
!
                           CALL VADD ( SSBTRG, TIPJ2, TGT )

!
!                          Get the position of the tip.  Compute a
!                          new light time value and target epoch.
!

                           TLT = VDIST ( SSBOBS, TGT ) / CLIGHT()

                           IF ( XMIT )  THEN
                              TE  =  ET + TLT
                           ELSE
                              TE  =  ET - TLT
                           END IF
!
!                          Re-compute TIPM.
!
                           CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

!
!                          Get the observer position in the target
!                          body-fixed frame at TE.

                           CALL VSUB ( SSBOBS, SSBTRG, OBSJ2  )
                           CALL MXV  ( TIPM,   OBSJ2,  OBSVEC )

!
!                          Get the limb's center and semi-axis vectors.
!
                           CALL EDLIMB ( RADII(1), RADII(2), RADII(3), &
                                         OBSVEC,   LIMB               )

                           CALL EL2CGV ( LIMB, LCENTR, SMAJOR, SMINOR )

                        END DO
!
!                       Use stellar aberration at the axis tip to
!                       shift the center, if necessary.
!
                        IF ( USESTL ) THEN

                           CALL VADD ( LCENTR, SMAJOR, TIPFX )

                           CALL MTXV ( TIPM, TIPFX, TIPJ2 )

                           CALL VSUB ( TIPJ2, OBSJ2, TIPVEC )

                           IF ( XMIT ) THEN
                              CALL STLABX ( TIPVEC, SSBOBS(4), TIPCOR )
                           ELSE
                              CALL STELAB ( TIPVEC, SSBOBS(4), TIPCOR )
                           END IF
!
!                          Let STLCOR be the J2000 stellar aberration
!                          correction offset; let FIXCOR be the offset
!                          in the target frame.
!
                           CALL VSUB ( TIPCOR, TIPVEC, STLCOR )
                           CALL MXV  ( TIPM,   STLCOR, FIXCOR )

!
!                          Shift the limb center.
!
                           CALL VADD ( FIXCOR, LCENTR, LCCOR  )
                           CALL VEQU ( LCCOR,          LCENTR )

                        END IF
!
!                       We've compute our improved limb estimate.
!
                     END IF

!
!                    Pick our target point near the limb.  The
!                    point is 1+/- DELTA of the semi-major axis length
!                    out from the center, along one of the semi-
!                    major axes.
!
                     IF ( USECN ) THEN

                        DELTA = 1.D-5

                     ELSE IF ( USELT ) THEN

                        DELTA = 1.D-3

                     ELSE
                        DELTA = 1.D-9
                     END IF

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
                  CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, TRGCDE, &
                                  ET,      ABCORR,   USELT,    USECN, &
                                  USESTL,  XMIT,     FIXREF,   OBSCDE, &
                                  DREFID,  DCLASS,   DCENTR,   DVEC, &
                                  SPOINT,  TRGEPC,   SRFVEC,   FOUND  )

                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check the results.
!
                  IF ( GEOM(1:4) .EQ. 'MISS' ) THEN

                     CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

                  END IF

                  IF ( .NOT. FOUND ) THEN

                     IF ( GEOM(1:4) .EQ. 'MISS' ) THEN
!
!                       FOUND should be .FALSE.; the other outputs
!                       are undefined.
!
                        CALL CHCKSL ( 'FOUND', FOUND, .FALSE., OK )

                     ELSE
!
!                       We're supposed to have an intercept. Force
!                       an error signal.
!
                        CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                     END IF

                  ELSE
!
!                    FOUND is true.
!
!                    Compute the observer position relative to the
!                    target center, consistent with the aberration
!                    corrections applicable to the apparent intercept
!                    point. Also compute the distance of between the
!                    observer and the apparent intercept point.
!
                     CALL VSUB ( SPOINT, SRFVEC, OBSPOS )

!
!                    The let DIST be the length of SRFVEC.
!
                     DIST = VNORM ( SRFVEC )

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
                     CALL CHCKXC ( .FALSE., ' ', OK )

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
!                       The angular separation of SRFVEC and DVEC
!                       should be pretty small when these vectors are
!                       compared in compatible reference frames.  We
!                       don't expect these vectors to be identical
!                       (even theoretically) because they've been
!                       computed with different target epochs.
!
!                       First step:  get SRFVEC in the J2000 frame.
!
                        CALL MXV ( TRGJ2M, SRFVEC, SRFVJ2 )

!
!                       Find the angular separation and make sure it's
!                       not too large.
!
                        SEP = VSEP ( SRFVJ2, DJ2 )

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
!                    These tests are primarily of interest when
!                    aberration corrections are used, but they still
!                    serve as a consistency check for the geometric
!                    case.
!
!                    We're expecting to get good agreement between all
!                    of these items and their counterparts obtained
!                    from ZZSFXCOR, especially when we use converged
!                    Newtonian corrections.
!
                     IF (  EQSTR(OBSRVR, 'EARTH') ) THEN

                        IF ( USECN ) THEN
                           TOL = TIGHT
                        ELSE IF ( USELT ) THEN
                           TOL = MEDIUM
                        ELSE
                           TOL = TIGHT
                        END IF

                     ELSE
!
!                       Use looser tolerances for the Mars orbiter. For
!                       the orbiter, small errors in SPOINT lead to
!                       larger relative errors in DIST and SEP.
!
                        IF ( USECN ) THEN
                           TOL = MTIGHT
                        ELSE IF ( USELT ) THEN
                           TOL = LOOSE
                        ELSE
                           TOL = MTIGHT
                        END IF

                        IF ( GEOM .EQ. 'LIMB_INSIDE_NEAR' ) THEN
!
!                          Loosen the tolerance so the MAC/OSX/Native C
!                          platform can handle these tests.
!
                           TOL = TOL * 1.D2

                        END IF

                     END IF

!
!                    Find the aberration-corrected location of SPOINT.
!
!                    We need the J2000 state of the observer relative
!                    to the solar system barycenter at ET.
!
                     CALL SPKSSB ( OBSCDE, ET, 'J2000', SSBOBS )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    The SPOINT re-computation is done iteratively.
!                    Since we're starting with a geometric target
!                    position, the light time value obtained from the
!                    normal light time correction corresponds to the
!                    light time found on the *second* iteration. We
!                    increment our iteration counts by 1 for both the
!                    normal and CN cases.
!
                     IF ( USECN ) THEN

                        NITR = 5

                     ELSE IF ( USELT ) THEN

                        NITR = 2

                     ELSE

                        NITR = 0
!
!                       The expected target epoch is computed inside a
!                       loop below; the loop won't execute for the
!                       geometric case, so set the epoch here.
!
                        XTE  = ET

                     END IF

!
!                    The initial target position relative to the solar
!                    system barycenter is found by summing the target
!                    center position relative to the solar system
!                    barycenter at ET with SPOINT, after SPOINT has
!                    been converted to the J2000 frame at ET.
!
                     CALL SPKSSB ( TRGCDE, ET, 'J2000', SSBTRG )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL PXFORM (  'J2000', TRGFRM, ET,  TIPM )
                     CALL CHCKXC ( .FALSE., ' ', OK )

                     CALL MTXV   ( TIPM,   SPOINT,  SPJ2  )
                     CALL VADD   ( SSBTRG, SPJ2,    SPPOS )

                     DO I = 1, NITR
!
!                        Make a new estimate of the target epoch XTE.
!
                        LT = VDIST ( SSBOBS, SPPOS ) / CLIGHT()

                        IF ( XMIT ) THEN
                           XTE  =  ET + LT
                        ELSE
                           XTE  =  ET - LT
                        END IF

!
!                       Compute the J2000 state of SPOINT relative
!                       to the solar system barycenter at XTE.
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
!                    Compute the light-time corrected position of
!                    SPOINT as seen by the observer.
!
                     CALL VSUB ( SPPOS, SSBOBS, SPLT )
!
!                    Correct SPLT for stellar aberration, if ABCORR so
!                    indicates.
!
                     IF ( USESTL ) THEN

                        IF ( XMIT ) THEN
                           CALL STLABX ( SPLT, SSBOBS(4), XRAY )
                        ELSE
                           CALL STELAB ( SPLT, SSBOBS(4), XRAY )
                        END IF

                     ELSE
!
!                       XRAY is our expected result.
!
                        CALL VEQU ( SPLT, XRAY )

                     END IF

!
!                    Moment of truth: XRAY is the J2000 vector from the
!                    observer to the aberration-corrected position of
!                    our "ephemeris object" located on the target
!                    surface at location SPOINT. If SPOINT were
!                    correct in the first place, then XRAY should be
!                    lined up with our boresight direction DVEC, when
!                    DVEC is rotated to the J2000 frame.
!
!                    Actually, we computed DVEC in the J2000 frame long
!                    ago:  this vector is called DJ2.
!
                     CALL CHCKSD ( 'TRGEPC vs XTE', &
                                    TRGEPC, '~/', XTE, TOL, OK )

                     SEP = VSEP ( DJ2, XRAY )

                     CALL CHCKSD ( 'XRAY vs DJ2 sep', &
                                   SEP, '~', 0.D0, TOL, OK )

!
!                    Check DIST against its predicted value.
!
                     CALL CHCKSD ( 'DIST',      DIST, '~/', &
                                   VNORM(XRAY), TOL,   OK   )

!
!                    Create a predicted value for SRFVEC: convert XRAY
!                    to the target frame using XFORM.
!
                     CALL PXFORM ( 'J2000', TRGFRM, XTE, XFORM )

                     CALL MXV   ( XFORM, XRAY, XSRFVC )

                     CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', &
                                   XSRFVC,   3,      TOL,  OK )


!
!                    The following test only makes sense when stellar
!                    aberration is not used. (When stellar aberration
!                    IS used, SPOINT does not lie on the ray; the image
!                    of SPOINT under the stellar aberration correction
!                    lies on the ray.
!
                     IF ( .NOT. USESTL ) THEN
!
!                       Create a predicted value for SPOINT: convert
!                       the (optionally light-time corrected)
!                       target-observer vector to the target body-fixed
!                       frame at epoch XTE and add to SRFVEC to form
!                       XSPNT.
!
                        CALL VSUB ( SSBOBS, SSBTRG, J2OBS  )
                        CALL MXV  ( XFORM,  J2OBS,  XOBSPS )
                        CALL VADD ( XOBSPS, SRFVEC, XSPNT )

!
!                       Use absolute tolerances for this check: 1cm for
!                       converged light time; 1km for non-converged.
!
                        IF ( USECN ) THEN
                           TOL = 1.D-5
                        ELSE
                           TOL = 1.D0
                        END IF

                        CALL CHCKAD ( 'SPOINT', SPOINT, '~~', &
                                       XSPNT,   3,      TOL,  OK )

                     END IF

                  END IF
!
!                 We're finished with the consistency checks for
!                 the intercept cases.
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


!***********************************************************************
!
!     Error cases
!
!***********************************************************************


!
!     ZZSFXCOR does very little in-line error checking; it expects its
!     caller to handle this. To quote from the routine's header:
!
!     3)  This routine assumes robust input checking has been
!         performed by the caller. This routine may fail in
!         an unspecified manner if such checking has not
!         been performed.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer coincides with target' )

      OBSCDE = 399
      TRGCDE = 499
      FIXREF = 'IAU_MARS'
      ABCORR = 'XCN+S'
      XMIT   = .TRUE.
      USELT  = .TRUE.
      USECN  = .TRUE.
      USESTL = .TRUE.

      DREF   = FIXREF

      CALL NAMFRM ( DREF,   DREFID )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL FRINFO ( DREFID, DCENTR, DCLASS, DCLSID, FND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Initialize the surface intercept utilities. For
!     ellipsoidal target models, just the target body ID
!     is needed.
!
      CALL ZZSUELIN ( TRGCDE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, OBSCDE, &
                      ET,      ABCORR,   USELT,    USECN, &
                      USESTL,  XMIT,     FIXREF,   OBSCDE, &
                      DREFID,  DCLASS,   DCENTR,   DVEC, &
                      SPOINT,  TRGEPC,   SRFVEC,   FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOSEPARATION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction. ' )

!
!     To force this check to occur, we need to cause the
!     aberration correction string to be passed to SPKEZP.
!     This occurs when the direction vector frame is non-inertial
!     and is not centered on the observer.
!
      CALL NAMFRM ( FIXREF, DREFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, TRGCDE, &
                      ET,      'XYZ',    USELT,    USECN, &
                      USESTL,  XMIT,     FIXREF,   OBSCDE, &
                      DREFID,  DCLASS,   DCENTR,   DVEC, &
                      SPOINT,  TRGEPC,   SRFVEC,   FOUND  )


      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction: relativistic ' )
!
!     Find the surface intercept point. Use relativistic aberration
!     correction.
!
      CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, TRGCDE, &
                      ET,      'RLT',    USELT,    USECN, &
                      USESTL,  XMIT,     FIXREF,   OBSCDE, &
                      DREFID,  DCLASS,   DCENTR,   DVEC, &
                      SPOINT,  TRGEPC,   SRFVEC,   FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction: stellar ' &
      //           'aberration only'                    )

      CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, TRGCDE, &
                      ET,      'S',      USELT,    USECN, &
                      USESTL,  XMIT,     FIXREF,   OBSCDE, &
                      DREFID,  DCLASS,   DCENTR,   DVEC, &
                      SPOINT,  TRGEPC,   SRFVEC,   FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINVALIDOPTION)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Zero direction vector' )

      CALL CLEARD ( 3, DVEC )

      CALL ZZSFXCOR ( ZZRAYNP, ZZMAXRAD, ZZRAYSFX, TRGCDE, &
                      ET,      ABCORR,   USELT,    USECN, &
                      USESTL,  XMIT,     FIXREF,   OBSCDE, &
                      DREFID,  DCLASS,   DCENTR,   DVEC, &
                      SPOINT,  TRGEPC,   SRFVEC,   FOUND  )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
!     Clean up.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.' )

      CALL SPKUEF ( HANDLE(1) )
      CALL DELFIL ( SPK1      )

      CALL SPKUEF ( HANDLE(2) )
      CALL DELFIL ( SPK2      )

      CALL T_SUCCESS ( OK )

      END
