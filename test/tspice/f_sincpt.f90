!$Procedure      F_SINCPT ( SINCPT family tests )

      SUBROUTINE F_SINCPT ( OK )

! $ Abstract
!
!     Exercise the higher-level SPICELIB geometry routine SINCPT.
!     Use both DSK-based and ellipsoidal target shape models.
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

      INCLUDE 'dsk.inc'
      INCLUDE 'zzdsk.inc'
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
!     This routine tests the higher-level SPICELIB
!     geometry routine SINCPT.
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
!-    TSPICE Version 3.0.0 02-APR-2016 (NJB)
!
!        Updated to test DSK usage. Also more thoroughly
!        tests ellipsoid target cases.
!
!-    TSPICE Version 2.0.0, 02-APR-2012 (NJB)
!
!           Added cases for bad aberration correction values.
!
!-    TSPICE Version 1.0.0, 22-FEB-2008 (NJB)
!
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               EQSTR
      LOGICAL               EXISTS
      LOGICAL               MATCHI
!
!     Local parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0  = 'sincpt_dsk0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1  = 'sincpt_dsk1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2  = 'sincpt_dsk2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3  = 'sincpt_dsk3.bds' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK   = 'test_0008.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'sincpt_spk.bsp' )

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
      PARAMETER           ( LNSIZE = 160 )

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

      INTEGER               NTARG
      PARAMETER           ( NTARG  = 2 )

      INTEGER               NGEOM
      PARAMETER           ( NGEOM  = 4 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 4 )

      INTEGER               NMETH
      PARAMETER           ( NMETH  = 4 )

!
!     Local variables
!
      CHARACTER*(ABCLEN)    ABCS   ( NABC )
      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(NAMLEN)    DREF
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(LNSIZE)    GEOM
      CHARACTER*(LNSIZE)    GEOMS  ( NGEOM )
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(MTHLEN)    METHDS ( NMETH )
      CHARACTER*(NAMLEN)    OBSRVR
      CHARACTER*(NAMLEN)    OBSNMS ( NOBS )
      CHARACTER*(NAMLEN)    REFS   ( NREF, NTARG )
      CHARACTER*(NAMLEN)    SRFNMS ( NMAP )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(NAMLEN)    TRGNMS ( NTARG )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      AXISTA ( 6 )
      DOUBLE PRECISION      AXLT
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DEPOCH
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DJ2    ( 3 )
      DOUBLE PRECISION      DJ2M   ( 3, 3 )
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      DVECFX ( 3 )
      DOUBLE PRECISION      DVECJ2 ( 3 )
      DOUBLE PRECISION      ELTS   ( 8 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ETOL
      DOUBLE PRECISION      FRAC
      DOUBLE PRECISION      J2OBS  ( 3 )
      DOUBLE PRECISION      LCENTR ( 3 )
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LMBOFF ( 3 )
      DOUBLE PRECISION      NEGVEC ( 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      OBSVEC ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RAYSTA ( 6 )
      DOUBLE PRECISION      REFPOS ( 3 )
      DOUBLE PRECISION      RLT
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SPNTLT
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SRFVJ2 ( 3 )
      DOUBLE PRECISION      SSBOBS ( 6 )
      DOUBLE PRECISION      SSBTRG ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      TE
      DOUBLE PRECISION      TGT    ( 3 )
      DOUBLE PRECISION      TIPFX  ( 3 )
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TLT
      DOUBLE PRECISION      TMPVEC ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGJ2M ( 3, 3 )
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XLT
      DOUBLE PRECISION      XOBSPS ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XRAY   ( 3 )
      DOUBLE PRECISION      XSPNT  ( 3 )
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               BODYID
      INTEGER               CLS
      INTEGER               CLSSID
      INTEGER               DREFID
      INTEGER               FRCODE
      INTEGER               GEOMIX
      INTEGER               HANDLE ( 2 )
      INTEGER               MIX
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               OBSCDE
      INTEGER               OBSIDX
      INTEGER               REFCTR
      INTEGER               REFIDX
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )
      INTEGER               SURFID
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               TRGCDE
      INTEGER               TRGIDX

      LOGICAL               FND
      LOGICAL               FOUND
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL

!
!     Saved variables
!
      SAVE

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

!
!     REFS is a two-dimensional array. There's a set of
!     ray reference  frames for each target. Currently
!     there are only two targets: Mars and Phobos.
!
      DATA                  REFS   /  'J2000',      'J2000', &
                                      'ECLIPJ2000', 'ECLIPJ2000', &
                                      'IAU_MARS',   'IAU_PHOBOS', &
                                      'IAU_EARTH',  'IAU_EARTH'  /

      DATA                  GEOMS  /  'POINT_AT_CENTER', &
                                      'MISS_BACKWARD', &
                                      'LIMB_INSIDE_NEAR', &
                                      'MISS_LIMB_NEAR'    /


      DATA                  OBSNMS /  'Earth', &
                                      'MARS_ORBITER'    /


      DATA                  TRGNMS /  'Mars', &
                                      'PHOBOS'    /

!
!     Note that the last two method strings are identical. This
!     is done to test the logic that uses saved values obtained
!     by parsing method string.
!
      DATA                  METHDS / &
                           'ELLIPSOID', &
                           'dsk/unprioritized/surfaces="high-res"', &
                           'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"', &
                           'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"' &
                                   /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SINCPT' )

!
! --- Case: ------------------------------------------------------
!
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
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup: create DSK files.' )

!
!     For Mars, surface 1 is the "main" surface.
!
      TARGET = 'MARS'
      TRGCDE = 499
      FIXREF = 'IAU_MARS'

      BODYID = TRGCDE
      SURFID = 1
      NLON   = 1200
      NLAT   =  600

      IF ( EXISTS(DSK0) ) THEN
         CALL DELFIL( DSK0 )
      END IF

      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load main Mars DSK.
!
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Surface 2 for Mars is very low-res.
!
      BODYID = TRGCDE
      SURFID = 2
      NLON   = 40
      NLAT   = 20

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
      END IF

!
!     Create and load the second DSK.
!
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Surface 1 for Phobos is low-res.
!
      BODYID = 401
      SURFID = 1
      FIXREF = 'IAU_PHOBOS'

      NLON   = 200
      NLAT   = 100

      IF ( EXISTS(DSK2) ) THEN
         CALL DELFIL( DSK2 )
      END IF

!
!     Create and load the first Phobos DSK.
!
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Surface 2 for Phobos is lower-res.
!
      BODYID = 401
      SURFID = 2
      FIXREF = 'IAU_PHOBOS'

      NLON   =  80
      NLAT   =  40

      IF ( EXISTS(DSK3) ) THEN
         CALL DELFIL( DSK3 )
      END IF

!
!     Create and load the second Phobos DSK.
!
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Set up a surface name-ID map.
!
      SRFBOD(1) = 499
      SRFIDS(1) = 1
      SRFNMS(1) = 'high-res'

      SRFBOD(2) = 499
      SRFIDS(2) = 2
      SRFNMS(2) = 'low-res'

      SRFBOD(3) = 401
      SRFIDS(3) = 1
      SRFNMS(3) = 'high-res'

      SRFBOD(4) = 401
      SRFIDS(4) = 2
      SRFNMS(4) = 'low-res'



      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_SURFACE_CODE', NMAP, SRFIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_SURFACE_BODY', NMAP, SRFBOD )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
!
!     Main test loop follows.
!

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
!        Loop over every choice of target.
!
         DO TRGIDX = 1, NTARG

            TARGET = TRGNMS( TRGIDX )
!
!           Set the target ID code.
!
            CALL BODN2C ( TARGET, TRGCDE, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Get target body-fixed frame name.
!
            CALL CNMFRM ( TARGET, FRCODE, TRGFRM, FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )
            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!           Get target radii.
!
            CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
            CALL CHCKXC ( .FALSE., ' ', OK )


!
!           Loop over every viewing geometry case.
!
            DO GEOMIX = 1, NGEOM

               GEOM = GEOMS ( GEOMIX )

!
!              Loop over every aberration correction choice.
!
               DO ABCIDX = 1, NABC

                  ABCORR = ABCS ( ABCIDX )

!
!                 Set up some logical variables describing the
!                 attributes of the selected correction.
!
                  USELT  = ABCORR .NE. 'None'

                  USECN  =       ( ABCORR(1:2) .EQ. 'Cn'  ) &
                            .OR. ( ABCORR(1:3) .EQ. 'Xcn' )

                  USESTL = INDEX ( ABCORR, '+s' )  .NE.  0

!
!                 Loop over every direction vector frame choice.
!
                  DO REFIDX = 1, NREF

                     DREF = REFS ( REFIDX, TRGIDX )
!
!                    Set light time RLT from observer to center of
!                    frame for the direction vector.
!
                     CALL NAMFRM ( DREF,   DREFID )
                     CALL FRINFO ( DREFID, REFCTR, CLS, CLSSID, FND )

                     CALL SPKEZP ( REFCTR, ET, 'J2000', ABCORR, OBSCDE, &
                                   REFPOS, RLT                         )

!
!                    We'll need the epoch DEPOCH associated
!                    with the center of DREF.  RLT is the
!                    light time from DREF's center to the observer.
!
                     CALL ZZCOREPC ( ABCORR, ET, RLT, DEPOCH )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Look up the transformation from frame DREF to
!                    J2000. We don't need this right away, but we'll
!                    have occasion to use it later.
!
                     CALL PXFORM ( DREF, 'J2000', DEPOCH, DJ2M )
                     CALL CHCKXC ( .FALSE., ' ', OK )

!
!                    Loop over all method choices.
!
                     DO MIX = 1, NMETH

                        METHOD = METHDS( MIX )

!
! --- Case: ------------------------------------------------------
!

                        TITLE = 'Observer = #; Target = #; ' &
                        //      'Geometry = #; ABCORR = #; ' &
                        //      'DREF = #; METHOD = #.'
                        CALL REPMC ( TITLE, '#', OBSRVR, TITLE )
                        CALL REPMC ( TITLE, '#', TARGET, TITLE )
                        CALL REPMC ( TITLE, '#', GEOM,   TITLE )
                        CALL REPMC ( TITLE, '#', ABCORR, TITLE )
                        CALL REPMC ( TITLE, '#', DREF,   TITLE )
                        CALL REPMC ( TITLE, '#', METHOD, TITLE )

                        CALL TCASE ( TITLE )

                        IF ( GEOM .EQ. 'POINT_AT_CENTER' ) THEN
!
!                          Look up direction vector using current frame
!                          and aberration correction.  The direction
!                          vector is going to point to the target's
!                          center, so we should hit the target.
!
                           CALL SPKPOS ( TARGET, ET,   DREF, ABCORR, &
                                         OBSRVR, DVEC, DLT          )
                           CALL CHCKXC ( .FALSE., ' ', OK )


                        ELSE IF ( GEOM .EQ. 'MISS_BACKWARD' ) THEN
!
!                          Set the pointing direction to the inverse of
!                          that obtained in the 'POINT_AT_CENTER' case.
!
                           CALL SPKPOS ( TARGET, ET,     DREF, ABCORR, &
                                         OBSRVR, NEGVEC, DLT          )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL VMINUS ( NEGVEC, DVEC )


                        ELSE IF (     ( GEOM .EQ. 'LIMB_INSIDE_NEAR' ) &
                                 .OR. ( GEOM .EQ. 'MISS_LIMB_NEAR'   ) ) &
                        THEN
!
!                          Find the limb of the target based on the
!                          aberration-corrected target center position.
!                          Select ray to hit limb plane along major
!                          axis, slightly inside or slightly outside
!                          the ellipse, depending on the geometry case.
!
!                          Note we're looking up the target state in
!                          the target's body-fixed frame, not in the
!                          DREF frame.
!
                           CALL SPKPOS ( TARGET, ET,     TRGFRM, ABCORR, &
                                         OBSRVR, NEGVEC, TLT           )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL VMINUS ( NEGVEC, OBSVEC )

!
!                          Get the limb's center and semi-axis vectors.
!
                           CALL EDLIMB ( RADII(1), RADII(2), RADII(3), &
                                         OBSVEC,   LIMB               )

                           CALL EL2CGV ( LIMB, LCENTR, SMAJOR, SMINOR )

!
!                          Improve the limb if we're using aberration
!                          corrections.
!
!                          We'll treat one endpoint of the limb's major
!                          axis as an ephemeris object, and we'll
!                          compute the aberration-corrected position of
!                          this object.
!
!                          To get an accurate limb, we'll find the
!                          light time from observer to tip of
!                          semi-major axis and get an improved light
!                          time estimate.

                           IF ( USELT ) THEN

                              CALL VADD ( LCENTR, SMAJOR, TIPFX )
!
!                             Find the aberration-corrected position of
!                             the tip of the major axis. Compute the
!                             offset from the original position.
!
                              CALL SPKCPT ( TIPFX,  TARGET, TRGFRM, &
                                            ET,     TRGFRM, 'TARGET', &
                                            ABCORR, OBSRVR, AXISTA, &
                                            AXLT                     )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL VSUB ( AXISTA, TIPFX, LMBOFF )

!
!                             Shift the limb center by the offset we
!                             just computed.
!
                              CALL VADD ( LCENTR, LMBOFF, TMPVEC )
                              CALL VEQU ( TMPVEC,         LCENTR )

!
!                             We've compute our improved limb estimate.
!
                           END IF

!
!                          Pick our target point near the limb. The
!                          point is 1+/- DELTA of the semi-major axis
!                          length out from the center, along one of the
!                          semi- major axes.
!
!                          A challenge that occurs here, since our
!                          target is a DSK-based shape, is that the
!                          actual limb is a polygon inscribed in the
!                          ellipsoid limb. Depending on the orientation
!                          of the polygon, the semi-major axis of the
!                          ellipsoid limb may extend outside of the DSK
!                          shape, and our target point may define a ray
!                          that will miss the ellipsoid even for cases
!                          where an intersection should occur.
!
!                          We'll mitigate this problem for the
!                          geometric case by modifying the target
!                          point. We'll try to find a surface point on
!                          the DSK shape "below" the target point, and
!                          if we find it, we'll use it instead of the
!                          original target.
!
                           IF ( USECN ) THEN

                              DELTA = 1.D-5

                           ELSE IF ( USELT ) THEN

                              DELTA = 1.D-3

                           ELSE
!
!                             Value used for ellipsoidal targets is
!
!                                DELTA = 1.D-9
!
!                             We can still use this value if we're
!                             using the high-resolution DSK for Mars.
!                             Otherwise, we need a point deeper inside
!                             the ellipsoid, which corresponds to a
!                             larger value of DELTA.
!
                              IF (  MATCHI(METHOD, '*HIGH*', '*', '?') ) &
                              THEN

                                 IF ( TARGET .EQ. 'MARS' ) THEN

                                    DELTA = 1.D-9
                                 ELSE
                                    DELTA = 1.D-3
                                 END IF

                              ELSE
                                 DELTA = 1.D-2
                              END IF

                           END IF

                           IF ( GEOM(1:4) .EQ. 'MISS' ) THEN
                              FRAC = 1.D0 + DELTA
                           ELSE
                              FRAC = 1.D0 - DELTA
                           END IF

                           CALL VLCOM( 1.D0, LCENTR, FRAC, SMAJOR, TGT )

                           IF ( .NOT. USELT ) THEN
!
!                             Find the surface point "under" TGT. This
!                             can be done simply only for the geometric
!                             case, because the limb has not been
!                             translated to account for aberration
!                             corrections.
!
                              CALL VMINUS ( TGT, RAYDIR )

                              CALL DSKXV ( .FALSE., TARGET, 0,  SRFLST, &
                                           ET,      TRGFRM, 1,  TGT, &
                                           RAYDIR,  XPT,    FND        )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              IF ( FND ) THEN
!
!                                If an intercept was found, use it
!                                instead of TGT. If an intercept was
!                                not found, TGT is already inside the
!                                tessellated ellipsoid.
!
                                 CALL VEQU ( XPT, TGT )

                              END IF

                           END IF

!
!                          Our ray extends from the observer through
!                          the target point.
!
                           CALL VSUB ( TGT, OBSVEC, DVECFX )

                           SEP = VSEP ( NEGVEC, DVECFX )

!
!                          Convert the ray from the target body fixed
!                          frame to J2000, then from J2000 to the DREF
!                          frame. We need the target frame epoch TE to
!                          find the first transformation matrix TIPM.
!
                           CALL ZZCOREPC ( ABCORR, ET, TLT, TE )

                           CALL PXFORM ( 'J2000', TRGFRM, TE, TIPM )

                           CALL MTXV ( TIPM, DVECFX, DVECJ2 )

!
!                          The matrix DJ2M maps from DREF to J2000, so
!                          apply the transpose of this matrix to obtain
!                          DVEC.
!
                           CALL MTXV ( DJ2M, DVECJ2, DVEC )

                        ELSE
!
!                          Oops!  Name mismatch.
!
                           CALL SIGERR ( 'SPICE(BUG)' )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                        END IF

!
!                       Find the surface intercept point.
!
                        CALL SINCPT ( METHOD, TARGET,  ET,     TRGFRM, &
                                      ABCORR, OBSRVR,  DREF,   DVEC, &
                                      SPOINT, TRGEPC,  SRFVEC, FOUND )

                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       Check the results.
!
                        IF ( .NOT. FOUND ) THEN

                           IF ( GEOM(1:4) .EQ. 'MISS' ) THEN
!
!                             FOUND should be .FALSE.; the other outputs
!                             are undefined.
!
                              CALL CHCKSL( 'FOUND', FOUND, .FALSE., OK )

                           ELSE
!
!                             We're supposed to have an intercept.
!                             Force an error signal.
!
                              CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                           END IF

                        ELSE
!
!                          FOUND is true.
!
!                          Compute the observer position relative to
!                          the target center, consistent with the
!                          aberration corrections applicable to the
!                          apparent intercept point. Also compute the
!                          distance of between the observer and the
!                          apparent intercept point.

                           CALL VSUB ( SPOINT, SRFVEC, OBSPOS )

!
!                          Let DIST be the length of SRFVEC.
!
                           DIST = VNORM ( SRFVEC )

!
!                          The target epoch had better be consistent
!                          with DIST and ABCORR.
!
                           XLT  = DIST/CLIGHT()

                           CALL ZZCOREPC ( ABCORR, ET, XLT, XEPOCH )

!
!                          This is a relative error check.
!
                           IF ( USECN ) THEN
                              ETOL = TIGHT
                           ELSE
                              ETOL = MEDIUM
                           END IF

                           CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', &
                                         XEPOCH,   ETOL,    OK  )

!
!                          Get the transformation from the target frame
!                          to J2000.

                           CALL PXFORM ( TRGFRM, 'J2000', &
                                         TRGEPC, TRGJ2M )
                           CALL CHCKXC ( .FALSE., ' ', OK )

!
!                          Now transform DVEC to the J2000 frame.
!
                           CALL MXV ( DJ2M, DVEC, DJ2 )

!
!                          The following check applies only to the case
!                          in which the pointing direction is toward
!                          the target's center.
!
                           IF ( GEOM .EQ. 'POINT_AT_CENTER' ) THEN
!
!                             The angular separation of SRFVEC and DVEC
!                             should be pretty small when these vectors
!                             are compared in compatible reference
!                             frames. We don't expect these vectors to
!                             be identical (even theoretically) because
!                             they've been computed with different
!                             target epochs.
!
!                             First step: get SRFVEC in the J2000
!                             frame.
!
                              CALL MXV ( TRGJ2M, SRFVEC, SRFVJ2 )

!
!                             Find the angular separation and make sure
!                             it's not too large.
!
                              SEP = VSEP ( SRFVJ2, DJ2 )

                              CALL CHCKSD( 'DJ2 vs NEG2 SEP', SEP, &
                                           '~', 0.D0, SLOPPY, OK )
                           END IF

!
!                          End of sanity check test for the
!                          POINT_AT_CENTER case.
!
!                          Having made it this far, we're ready for
!                          some more rigorous tests. In particular,
!                          we're going treat SPOINT as an ephemeris
!                          object and find its aberration-corrected
!                          position relative to the observer in J2000
!                          coordinates. This computation will allow us
!                          to derive expected values of TRGEPC, OBSPOS,
!                          the transformation from the J2000 frame to
!                          the target body-fixed frame at TRGEPC. We
!                          will verify that the aberration-corrected
!                          location of SPOINT, lies on the ray DVEC:
!                          this is the criterion we used to define
!                          SPOINT.
!
!                          These tests are primarily of interest when
!                          aberration corrections are used, but they
!                          still serve as a consistency check for the
!                          geometric case.
!
!                          We're expecting to get good agreement
!                          between all of these items and their
!                          counterparts obtained from SINCPT,
!                          especially when we use converged Newtonian
!                          corrections.

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
!                             Use looser tolerances for the Mars
!                             orbiter. For the orbiter, small errors in
!                             SPOINT lead to larger relative errors in
!                             DIST and SEP.

                              IF ( USECN ) THEN
                                 TOL = MTIGHT
                              ELSE IF ( USELT ) THEN
                                 TOL = LOOSE
                              ELSE
                                 TOL = MTIGHT
                              END IF

                              IF ( GEOM .EQ. 'LIMB_INSIDE_NEAR' ) THEN
!
!                                Loosen the tolerance so the
!                                MAC/OSX/Native C platform can handle
!                                these tests.
!
                                 TOL = TOL * 1.D2

                              END IF

                           END IF

!
!                          Find the aberration-corrected location of
!                          SPOINT.
!
!                          Prior to N0065, we had to do this manually.
!                          Now we can call a SPICE routine to do the
!                          job.
!
                           CALL SPKCPT ( SPOINT,  TARGET,   TRGFRM, ET, &
                                         'J2000', 'TARGET', ABCORR, &
                                         OBSRVR,  RAYSTA,   SPNTLT    )
                           CALL CHCKXC ( .FALSE., ' ', OK )

!
!                          XRAY is our expected result.
!
                           CALL VEQU ( RAYSTA, XRAY )

!
!                          Compute the expected target epoch.
!
                           CALL ZZCOREPC ( ABCORR, ET, SPNTLT, XTE )
                           CALL CHCKXC ( .FALSE., ' ', OK )

!
!                          Moment of truth: XRAY is the J2000 vector
!                          from the observer to the
!                          aberration-corrected position of our
!                          "ephemeris object" located on the target
!                          surface at location SPOINT. If SPOINT were
!                          correct in the first place, then XRAY should
!                          be lined up with our boresight direction
!                          DVEC, when DVEC is rotated to the J2000
!                          frame.
!
!                          Actually, we computed DVEC in the J2000
!                          frame long ago: this vector is called DJ2.
!
                           CALL CHCKSD ( 'TRGEPC vs XTE', &
                                         TRGEPC, '~/', XTE, TOL, OK )

                           SEP = VSEP ( DJ2, XRAY )

                           CALL CHCKSD ( 'XRAY vs DJ2 sep', &
                                         SEP, '~', 0.D0, TOL, OK )

!
!                          Check DIST against its predicted value.
!
                           CALL CHCKSD ( 'DIST',      DIST, '~/', &
                                         VNORM(XRAY), TOL,   OK   )

!
!                          Create a predicted value for SRFVEC: convert
!                          XRAY to the target frame using XFORM.
!
                           CALL PXFORM ( 'J2000', TRGFRM, XTE, XFORM )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL MXV   ( XFORM, XRAY, XSRFVC )

                           CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', &
                                         XSRFVC,   3,      TOL,  OK )

!
!                          The following test only makes sense when
!                          stellar aberration is not used. (When
!                          stellar aberration IS used, SPOINT does not
!                          lie on the ray; the image of SPOINT under
!                          the stellar aberration correction lies on
!                          the ray.
!
                           IF ( .NOT. USESTL ) THEN
!
!                             Create a predicted value for SPOINT:
!                             convert the (optionally light-time
!                             corrected) target-observer vector to the
!                             target body-fixed frame at epoch XTE and
!                             add to SRFVEC to form XSPNT.
!
                              CALL SPKSSB( OBSCDE, ET,  'J2000', SSBOBS)
                              CALL SPKSSB( TRGCDE, XTE, 'J2000', SSBTRG)
                              CALL CHCKXC( .FALSE., ' ', OK )

                              CALL VSUB ( SSBOBS, SSBTRG, J2OBS  )
                              CALL MXV  ( XFORM,  J2OBS,  XOBSPS )
                              CALL VADD ( XOBSPS, SRFVEC, XSPNT )
!
!                             Use absolute tolerances for this check:
!                             1cm for converged light time; 1km for
!                             non-converged.
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
!                       We're finished with the consistency checks for
!                       the intercept cases.
!

                     END DO
!
!                    End of the method loop.
!
                  END DO
!
!                 End of the reference frame loop.
!
               END DO
!
!              End of the aberration correction loop.
!
            END DO
!
!           End of the geometry case loop.
!
         END DO
!
!        End of the target loop.
!
      END DO
!
!     End of the observer loop.
!

!***********************************************************************
!
!     Normal case: input handling
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
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

      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )

!
!     Set target and target-fixed frame.
!
      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'

      CALL SINCPT ( 'Ellipsoid', 'MARS',  ET,      FIXREF, &
                    ABCORR,      'Earth', DREF,    DVEC, &
                    XSPNT,       XEPOCH,  XSRFVC,  FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL SINCPT ( 'Ellipsoid', '499',    ET,      FIXREF, &
                    ABCORR,      '399',    DREF,    DVEC, &
                    SPOINT,      TRGEPC,   SRFVEC,  FOUND )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT,  3, 0.D0, OK )
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )
      CALL CHCKSD ( 'DIST',   DIST,   '=', DIST,      0.D0, OK )


!***********************************************************************
!
!     Normal case: state change detection
!
!***********************************************************************

!
!     Certain subsystem state changes must be detected and responded to
!     by SINCPT. The subsystems (or structures) having states that must
!     be monitored are:
!
!        - Target name-ID mapping
!
!        - Observer name-ID mapping
!
!        - Surface name-ID mapping
!
!        - Ray direction frame definition
!
!        - Target body-fixed frame definition
!
!        - ZZDSKBSR state
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Target name changed to JUPITER for ID code 499.' )

!
!     First, get expected intercept.
!
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
      DREF   = 'J2000'
!
!     Set up the ray first.
!
      CALL SPKPOS ( '499', ET,   DREF, ABCORR, &
                    '399', DVEC, DLT          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SINCPT ( 'Ellipsoid', TARGET, ET,      FIXREF, &
                    ABCORR,      OBSRVR, DREF,    DVEC, &
                    XSPNT,       XEPOCH, XSRFVC,  FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )


      CALL BODDEF ( 'JUPITER', 499 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SINCPT ( 'Ellipsoid', 'JUPITER', ET,     FIXREF, &
                    ABCORR,      OBSRVR,    DREF,   DVEC, &
                    SPOINT,      TRGEPC,    SRFVEC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )

!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT, 3, 0.D0, OK )


!     Restore original mapping.
!
      CALL BODDEF ( 'JUPITER', 599 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer name changed to SUN for ID code 399.' )

      CALL BODDEF ( 'SUN', 399 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SINCPT ( 'Ellipsoid', TARGET, ET,     FIXREF, &
                    ABCORR,      'SUN',  DREF,   DVEC, &
                    SPOINT,      TRGEPC, SRFVEC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )


!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT, 3, 0.D0, OK )

!
!     Restore original mapping.
!
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Mars high-res surface name changed to ' &
      //           'AAAbbb.'                               )

!
!     Get expected results first.
!
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
      DREF   = 'J2000'

      CALL SPKPOS ( '499', ET,   DREF, ABCORR, &
                    '399', DVEC, DLT          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      METHOD = 'dsk/unprioritized/surfaces = 1'

      CALL SINCPT ( METHOD, TARGET, ET,      FIXREF, &
                    ABCORR, OBSRVR, DREF,    DVEC, &
                    XSPNT,  XEPOCH, XSRFVC,  FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )


      SRFNMS(1) = 'AAAbbb'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )



      METHOD = 'dsk/unprioritized/surfaces = AAAbbb'

      CALL SINCPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, DREF,   DVEC, &
                    SPOINT, TRGEPC, SRFVEC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )


!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT, 3, 0.D0, OK )


!
!     Restore original mapping.
!
      SRFNMS(1) = 'high-res'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unload Mars high-res DSK.'  )

!
!     Get reference result using low-res Mars DSK.
!
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
      DREF   = 'J2000'

      CALL SPKPOS ( '499', ET,   DREF, ABCORR, &
                    '399', DVEC, DLT          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      METHOD = 'dsk/unprioritized/surfaces = low-res'

      CALL SINCPT ( METHOD, TARGET, ET,      FIXREF, &
                    ABCORR, OBSRVR, DREF,    DVEC, &
                    XSPNT,  XEPOCH, XSRFVC,  FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )

!
!     Unload the high-res DSK; set METHOD to remove
!     surface specification.
!
      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'dsk/unprioritized'

      CALL SINCPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, DREF,   DVEC, &
                    SPOINT, TRGEPC, SRFVEC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )

!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT, 3, 0.D0, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Unload Mars low-res DSK; ' &
      //           'reload Mars high-res DSK.'  )

!
!     Restore DSK, unload low-res DSK, and repeat computation.
!
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'dsk/unprioritized'

      CALL SINCPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, DREF,   DVEC, &
                    SPOINT, TRGEPC, SRFVEC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(1) FOUND', FOUND, .TRUE., OK )

!
!     Make sure the result matches that obtained with the
!     high-res DSK specified.
!
      METHOD = 'dsk/unprioritized/ SURFACES = "HIGH-RES" '

      CALL SINCPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, DREF,   DVEC, &
                    XSPNT,  XEPOCH, XSRFVC, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( '(0) FOUND', FOUND, .TRUE., OK )

!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSPNT, 3, 0.D0, OK )



!***********************************************************************
!
!     Error handling tests follow.
!
!***********************************************************************
!
! --- Case: ------------------------------------------------------
!
       CALL TCASE ( 'Target name not translated' )
!
!     Find the surface intercept point.
!
      CALL SINCPT ( 'Ellipsoid', 'xyz',   ET,      FIXREF, &
                    ABCORR,      OBSRVR,  DREF,    DVEC, &
                    SPOINT,      TRGEPC,  SRFVEC,  FOUND   )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Observer name not translated' )
!
!     Find the surface intercept point.
!
      CALL SINCPT ( 'Ellipsoid', TARGET,  ET,      FIXREF, &
                    ABCORR,      'xyz',   DREF,    DVEC, &
                    SPOINT,      TRGEPC,  SRFVEC,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Observer coincides with target' )
!
!     Find the surface intercept point.
!
      CALL SINCPT ( 'Ellipsoid', TARGET,  ET,      FIXREF, &
                    ABCORR,      TARGET,  DREF,    DVEC, &
                    SPOINT,      TRGEPC,  SRFVEC,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Unsupported computation method' )
!
!     Find the surface intercept point.
!
      CALL SINCPT ( 'xyz',       TARGET,  ET,      FIXREF, &
                    ABCORR,      OBSRVR,  DREF,    DVEC, &
                    SPOINT,      TRGEPC,  SRFVEC,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Body-fixed frame is not centered on target. ' )
!
!     Find the surface intercept point.  Use reference frame 'BAD'
!     for direction vector.
!
      CALL SINCPT ( 'Ellipsoid', 'Mars_orbiter',  ET,      FIXREF, &
                    ABCORR,      'EARTH',         'BAD',   DVEC, &
                    SPOINT,      TRGEPC,          SRFVEC,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Bad aberration correction. ' )
!
!     Find the surface intercept point. Use unrecognized aberration
!     correction.
!
      CALL SINCPT ( 'Ellipsoid', 'Mars_orbiter',  ET,          FIXREF, &
                    'ZZZ',       'EARTH',         'IAU_EARTH', DVEC, &
                    SPOINT,      TRGEPC,          SRFVEC,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Bad aberration correction: relativistic ' )
!
!     Find the surface intercept point. Use relativistic aberration
!     correction.
!
      CALL SINCPT ( 'Ellipsoid', 'Mars',  ET,    FIXREF, &
                    'RL',       'EARTH',         'IAU_EARTH', DVEC, &
                    SPOINT,      TRGEPC,          SRFVEC,  FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


      CALL TCASE ( 'Bad aberration correction: stellar ' &
      //           'aberration only'                    )
!
!     Find the surface intercept point. Use stellar aberration
!     correction w/o light time correction.
!
      CALL SINCPT ( 'Ellipsoid', 'MARS',  ET,          FIXREF, &
                    'XS',       'EARTH',  'IAU_EARTH', DVEC, &
                    SPOINT,      TRGEPC,   SRFVEC,     FOUND  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )



!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'Zero direction vector' )

      CALL CLEARD ( 3, DVEC )
      CALL SINCPT ( 'Ellipsoid', TARGET,  ET,      FIXREF, &
                    ABCORR,      OBSRVR,  DREF,    DVEC, &
                    SPOINT,      TRGEPC,  SRFVEC,  FOUND )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!

      CALL TCASE ( 'No loaded DSKs.' )


      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get reference result using low-res Mars DSK.
!
      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
      DREF   = 'J2000'

      CALL SPKPOS ( '499', ET,   DREF, ABCORR, &
                    '399', DVEC, DLT          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      METHOD = 'dsk/unprioritized/surfaces = low-res'

      CALL SINCPT ( METHOD, TARGET, ET,      FIXREF, &
                    ABCORR, OBSRVR, DREF,    DVEC, &
                    XSPNT,  XEPOCH, XSRFVC,  FOUND )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )




!
! --- Case: ------------------------------------------------------
!

!
!     Clean up.
!
      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK1      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK2      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK0      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK1      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK2      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK3      )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END
