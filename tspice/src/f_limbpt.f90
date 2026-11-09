!$Procedure      F_LIMBPT ( LIMBPT family tests )

      SUBROUTINE F_LIMBPT ( OK )
      use pepper_inc_dsk
      use pepper_inc_dskdsc
      use pepper_inc_zzdsk
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Exercise the higher-level SPICELIB geometry routine LIMBPT.
!     Use DSK-based and ellipsoidal target shape models.
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
!     geometry routine LIMBPT.
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
!-    TSPICE Version 2.0.0 12-JUN-2021 (NJB)
!
!        Added tests for computations using transmission aberration
!        corrections. Deleted error test case for transmission
!        corrections.
!
!-    TSPICE Version 1.0.0 29-AUG-2016 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM

      LOGICAL               EQSTR
      LOGICAL               EXISTS
      LOGICAL               MATCHI
      LOGICAL               ODD
!
!     Local parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0  = 'limbpt_dsk0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1  = 'limbpt_dsk1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2  = 'limbpt_dsk2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3  = 'limbpt_dsk3.bds' )

      CHARACTER*(*)         DSK4
      PARAMETER           ( DSK4  = 'limbpt_dsk4.bds' )


      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0  = 'test_0008.tpc' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1  = 'nat.tpc' )


      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'limbpt_spk.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2  = 'orbiter.bsp' )

      CHARACTER*(*)         SPK3
      PARAMETER           ( SPK3  = 'nat.bsp' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      MTIGHT
      PARAMETER           ( MTIGHT = 1.D-10 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 5.D-6 )


      INTEGER               ALPHA
      PARAMETER           ( ALPHA  = 1000 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF   = 1 )

      INTEGER               NABC
      PARAMETER           ( NABC   = 9 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 10 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NOBS
      PARAMETER           ( NOBS   = 2 )

      INTEGER               NTARG
      PARAMETER           ( NTARG  = 2 )

      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 5 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499 )

      INTEGER               NCROSS
      PARAMETER           ( NCROSS = 400 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 4 )

      INTEGER               NMETH
      PARAMETER           ( NMETH  = 7 )

      INTEGER               NPOLYV
      PARAMETER           ( NPOLYV = 100 )

      INTEGER               NTANGT
      PARAMETER           ( NTANGT = 4 )

      INTEGER               NLOC
      PARAMETER           ( NLOC   = 2 )

      INTEGER               MAXCUT
      PARAMETER           ( MAXCUT = 1000 )

      INTEGER               MAXPNT
      PARAMETER           ( MAXPNT = MAXCUT * 20 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

!
!     Local variables
!
      CHARACTER*(ABCLEN)    ABCS   ( NABC )
      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(NAMLEN)    CORLCS ( NLOC )
      CHARACTER*(NAMLEN)    CORLOC
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(NAMLEN)    LABEL
      CHARACTER*(MTHLEN)    METHD2
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(MTHLEN)    METHDS ( NMETH )
      CHARACTER*(NAMLEN)    OBSRVR
      CHARACTER*(NAMLEN)    OBSNMS ( NOBS )
      CHARACTER*(NAMLEN)    REFS   ( NREF, NTARG )
      CHARACTER*(NAMLEN)    SPKLOC
      CHARACTER*(NAMLEN)    SRFNMS ( NMAP )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(NAMLEN)    TRGFRM
      CHARACTER*(NAMLEN)    TRGNMS ( NTARG )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    UTC

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CUTNML ( 3 )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      ELTS   ( 8 )
      DOUBLE PRECISION      EPOCHS ( MAXCUT )
      DOUBLE PRECISION      EPNTS  ( 3, MAXPNT )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ETNGTS ( 3, MAXPNT )
      DOUBLE PRECISION      HTOL
      DOUBLE PRECISION      ISTATE ( 6 )
      DOUBLE PRECISION      ITANVC ( 3 )
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LPLANE ( UBPL )
      DOUBLE PRECISION      LPOINT ( 3 )
      DOUBLE PRECISION      LPROJ  ( 3 )
      DOUBLE PRECISION      LPTEPC
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PLNVEC ( 3 )
      DOUBLE PRECISION      PMCOEF ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      POINTS ( 3, MAXPNT )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RCROSS
      DOUBLE PRECISION      REFVEC ( 3 )
      DOUBLE PRECISION      ROLSTP
      DOUBLE PRECISION      SCHSTP
      DOUBLE PRECISION      SMAJOR ( 3 )
      DOUBLE PRECISION      SMINOR ( 3 )
      DOUBLE PRECISION      SOLTOL
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      TANGTS ( 3, MAXPNT )
      DOUBLE PRECISION      TANVEC ( 3 )
      DOUBLE PRECISION      TDELTA
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGLT
      DOUBLE PRECISION      TRGPOS ( 3 )
      DOUBLE PRECISION      UTAN   ( 3 )
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XEPOCH ( MAXPNT )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XISRFV ( 3 )
      DOUBLE PRECISION      XPOINT ( 3, MAXPNT )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               BODYID
      INTEGER               HANDLE ( 3 )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               LOCIDX
      INTEGER               MAXN
      INTEGER               MIX
      INTEGER               N
      INTEGER               NCUTS
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NPTS   ( MAXCUT )
      INTEGER               NSURF
      INTEGER               OBSCDE
      INTEGER               OBSIDX
      INTEGER               REFIDX
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )
      INTEGER               SURFID
      INTEGER               TIMIDX
      INTEGER               TRGCDE
      INTEGER               TRGIDX
      INTEGER               UB

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND
      LOGICAL               ISDSK
      LOGICAL               ISELL
      LOGICAL               ISHI
      LOGICAL               ISTAN
      LOGICAL               USECN
      LOGICAL               USELT

!
!     Saved variables
!
      SAVE

!
!     Initial values
!
      DATA                  ABCS   /  'None', &
                                      'Cn', &
                                      'Cn+s', &
                                      'Lt', &
                                      'Lt+s', &
                                      'XCN', &
                                      'xcn+S', &
                                      'lT', &
                                      'LT+S'   /


!
!     This array is two-dimensional. The second dimension
!     is indexed by the input METHOD. This makes it simpler
!     to control the combinations of methods and locus
!     values.
!
      DATA                  CORLCS /  'ELLIPSOID LIMB', &
                                      'CENTER'          /


      DATA                  REFS   /  'IAU_MARS',   'IAU_PHOBOS' /


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
            'ELLIPSOID / tangent', &
            'tangent/ellipsoid', &
            'tangent/dsk/unprioritized/surfaces="high-res"', &
            'tangent/dsk/unprioritized/surfaces="LOW-RES"', &
            'guided/ellipsoid', &
            'dsk/ guided /unprioritized/surfaces="high-res"', &
            'dsk/ guided /unprioritized/surfaces="LOW-RES"'   /



!
!     Begin every test family with an open call.q
!
      CALL TOPEN ( 'F_LIMBPT' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create SPK, PCK file.' )


      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL ( SPK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTSPK ( SPK1, .TRUE., HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the PCK file, and load it. Do not delete it.
!
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL ( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL T_PCK08 ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Optionally scale Mars rotation.
!
      CALL BODVRD ( 'MARS', 'PM', 3, N, PMCOEF )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Leave as is for delivery.
!
      PMCOEF(2) = PMCOEF(2)*1

      CALL PDPOOL ( 'BODY499_PM', 3, PMCOEF )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Create LSK, load it, and delete it.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set initial time.
!
      UTC = '2004 FEB 17'
      CALL STR2ET ( UTC, ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET     = ET0

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
      NLON   =  220
      NLAT   =  110

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
!     Surface 2 for Mars has slightly lower resolution.
!
      BODYID = TRGCDE
      SURFID = 2
      NLON   = 190
      NLAT   =  95

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
!     Surface 1 for Phobos is high-res.
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
!     This loop uses a single epoch. The set of tests using
!     nested tori, located further down in this file, does
!     vary the input times.
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
!           Get target radii.
!
            CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Loop over every aberration correction locus.
!
            DO LOCIDX = 1, NLOC

               CORLOC = CORLCS ( LOCIDX )
!
!              Loop over every aberration correction choice.
!
               DO ABCIDX = 1, NABC

                  ABCORR = ABCS ( ABCIDX )

                  CALL ZZPRSCOR ( ABCORR, ATTBLK )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Set up some logical variables describing the
!                 attributes of the selected correction.
!
                  USELT  = ATTBLK( LTIDX  )
                  USECN  = ATTBLK( CNVIDX )

!
!                 Loop over every target body-fixed frame choice.
!
                  DO REFIDX = 1, NREF

                     TRGFRM = REFS ( REFIDX, TRGIDX )
!
!                    Loop over all method choices that are
!                    compatible with the choice of CORLOC.
!
!                    The "center" locus is required  in order
!                    to use the "guided" methods.
!
                     IF (  EQSTR( CORLOC, 'CENTER' )  ) THEN

                        UB = NMETH

                     ELSE
!
!                       The locus is 'ELLIPSOID LIMB'. Only
!                       the "tangent" methods can be used.
!
                        UB = NTANGT

                     END IF


                     DO MIX = 1, UB

                        METHOD = METHDS( MIX )

                        ISELL  =  MATCHI( METHOD, '*ELLIPSOID*', &
                                          '*',    '?'           )

                        ISDSK  = .NOT. ISELL

                        ISTAN  =  MATCHI( METHOD, '*TANGENT*', &
                                          '*',    '?'           )

                        ISHI   =  MATCHI( METHOD, '*HIGH*', &
                                          '*',    '?'           )


!
! --- Case: ------------------------------------------------------
!

                        TITLE = 'Observer = #; Target = #; ' &
                        //      'ABCORR = #; TRGFRM = #; ' &
                        //      'METHOD = #; CORLOC = #.'
                        CALL REPMC ( TITLE, '#', OBSRVR,     TITLE )
                        CALL REPMC ( TITLE, '#', TARGET,     TITLE )
                        CALL REPMC ( TITLE, '#', ABCORR,     TITLE )
                        CALL REPMC ( TITLE, '#', TRGFRM,     TITLE )
                        CALL REPMC ( TITLE, '#', METHOD,     TITLE )
                        CALL REPMC ( TITLE, '#', CORLOC,     TITLE )

                        CALL TCASE ( TITLE )

!
!                       Start off by computing the set of limb points
!                       We'll then check the results.
!
                        NCUTS  = 4

                        ROLSTP = TWOPI() / NCUTS
                        CALL VPACK ( 0.D0, 0.D0, 1.D0, REFVEC )

!
!                       We expect a single limb point in each half
!                       plane, so we can use a large step.
!
                        SCHSTP = 4.D0

!
!                       Derive the solution tolerance from the height
!                       error tolerance and the observer-target center
!                       distance.
!
                        CALL SPKPOS ( TARGET, ET,     TRGFRM, ABCORR, &
                                      OBSRVR, TRGPOS, TRGLT          )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        DIST   = VNORM( TRGPOS )

                        HTOL   = 1.D-6
                        SOLTOL = HTOL / DIST

                        MAXN   = MAXPNT

!
!                       If the shape is DSK and the locus is ELLIPSOID
!                       limb, make a preliminary call to obtain the
!                       expected epochs.
!
                        IF ( ISDSK .AND. EQSTR( CORLOC, &
                                                'ELLIPSOID LIMB') ) THEN
!
!                          Create a version of the method string with
!                          an ellipsoid shape specification.
!
                           METHD2 = 'ELLIPSOID/TANGENT'
!
!                          Get the expected epochs for the call below
!                          that uses a DSK target shape.
!
                           CALL LIMBPT ( METHD2, TARGET, ET,     TRGFRM, &
                                         ABCORR, CORLOC, OBSRVR, REFVEC, &
                                         ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                                         MAXN,   NPTS,   EPNTS,  XEPOCH, &
                                         ETNGTS                        )
                        END IF


                        CALL LIMBPT ( METHOD, TARGET, ET,     TRGFRM, &
                                      ABCORR, CORLOC, OBSRVR, REFVEC, &
                                      ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                                      MAXN,   NPTS,   POINTS, EPOCHS, &
                                      TANGTS                          )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       Check the outputs.
!
                        K = 1

                        DO I = 1, NCUTS
!
!                          For convex shapes that include the origin,
!                          there should be just one limb point per
!                          cutting half-plane.
!
                           LABEL = 'NPTS(#)'
                           CALL REPMI ( LABEL, '#', I, LABEL )
                           CALL CHCKXC ( .FALSE., ' ', OK )

                           CALL CHCKSI ( LABEL, NPTS(I), '=', 1, 0, OK )


                           DO J = 1, NPTS(I)
!
!                             We'll treat the Jth limb point as an
!                             ephemeris object and find its position
!                             relative to the observer.
!
                              CALL VEQU ( POINTS(1,K), LPOINT )
                              CALL VEQU ( TANGTS(1,K), TANVEC )
                              LPTEPC = EPOCHS(I)

!
!                             Get an inertially referenced version
!                             of the tangent vector.
!
                              CALL PXFORM ( 'J2000', TRGFRM, &
                                            LPTEPC,  XFORM  )
                              CALL CHCKXC ( .FALSE., ' ', OK )

                              CALL MTXV ( XFORM, TANVEC, ITANVC )

!
!                             Set the SPK lookup locus to be compatible
!                             with the aberration correction locus used
!                             with the limb point.
!
                              IF ( CORLOC .EQ. 'CENTER' ) THEN
!
!                                In this case we're using aberration
!                                corrections for the target body
!                                center. We must compute the expected
!                                state of the limb point "manually."
!
                                 CALL SPKPOS ( TARGET,  ET,     TRGFRM, &
                                               ABCORR,  OBSRVR, TRGPOS, &
                                               TRGLT                   )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL VADD ( TRGPOS, LPOINT, XSRFVC )
!
!                                For the 'CENTER' locus, epochs
!                                associated with limb points be the
!                                epoch associated with the target
!                                body's center.
!
                                 LT = TRGLT
!
!                                Get an inertially referenced version
!                                of the expected tangent vector.
!
                                 CALL PXFORM ( 'J2000', TRGFRM, &
                                               LPTEPC,   XFORM  )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL MTXV ( XFORM, XSRFVC, XISRFV )


                              ELSE
!
!                                This is the 'ellipsoid limb' case.
!
                                 SPKLOC  = 'TARGET'

                                 CALL SPKCPT ( LPOINT, TARGET, TRGFRM, &
                                               ET,     TRGFRM, SPKLOC, &
                                               ABCORR, OBSRVR, STATE, &
                                               LT                     )
                                 CALL CHCKXC ( .FALSE., ' ', OK )


                                 CALL VEQU ( STATE, XSRFVC )


                                 CALL SPKCPT ( LPOINT, TARGET,  TRGFRM, &
                                               ET,     'J2000', SPKLOC, &
                                               ABCORR, OBSRVR,  ISTATE, &
                                               LT                     )
                                 CALL CHCKXC ( .FALSE., ' ', OK )

                                 CALL VEQU ( ISTATE, XISRFV )

                              END IF

!
!                             If LPOINT is correct, then the position
!                             of LPOINT relative to the observer should
!                             be equal to TANVEC. The light time
!                             obtained from SPKCPT or computed manually
!                             should match that implied by LPTEPC.
!
                              TOL = VTIGHT

!
!                             If the light time correction is
!                             non-converged, we can't expect tight
!                             agreement.
!
                              IF ( USELT .AND. (.NOT. USECN) ) THEN

                                 TOL = TOL * 1.D2

                              END IF

                              IF ( ISDSK   .AND. &
                                   (CORLOC .EQ. 'ELLIPSOID LIMB') ) THEN
!
!                                Set the expected epoch using that
!                                obtained using the ellipsoid target
!                                shape.
!
                                 XTE = XEPOCH(I)

                              ELSE
!
!                                Compute the expected epoch from the
!                                SPKCPT or SPKPOS call.
!
                                 CALL ZZCOREPC ( ABCORR, ET, LT, XTE )
                                 CALL CHCKXC   ( .FALSE., ' ', OK )

                              END IF

                              LABEL = 'LPTEPC PLANE #, PT #'
                              CALL REPMI ( LABEL, '#', I, LABEL )
                              CALL REPMI ( LABEL, '#', J, LABEL )

                              CALL CHCKSD ( LABEL, LPTEPC, '~/', &
                                            XTE,   TOL,    OK    )


                              IF ( USELT ) THEN

                                 IF ( USECN ) THEN
                                    TOL = MTIGHT
                                 ELSE
                                    TOL = LOOSE
                                 END IF

                              ELSE
                                 TOL = VTIGHT
                              END IF

                              IF ( ISDSK ) THEN
!
!                                Use looser tolerances, since the
!                                aberration correction performed by
!                                LIMBPT is for the corresponding
!                                ellipsoid limb point.
!
                                 TOL = TOL * 1.D3

                              END IF


                              LABEL = 'TANVEC PLANE #, PT #'
                              CALL REPMI ( LABEL, '#', I, LABEL )
                              CALL REPMI ( LABEL, '#', J, LABEL )


                              IF ( EQSTR( OBSRVR, 'MARS_ORBITER' ) ) &
                              THEN
!
!                                Check the absolute distance error for
!                                this case. Use a 5m tolerance.
!
                                 CALL CHCKAD ( LABEL,  TANVEC, '~~', &
                                               XSRFVC, 3,      5.D-3, &
                                               OK )
                              ELSE
!
!                                Compare the inertially referenced
!                                tangent vectors.
!
                                 LABEL = 'ITANVC PLANE #, PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

!
!                                Use a tighter tolerance for the
!                                inertial vector test.
!
                                 CALL CHCKAD ( LABEL,  ITANVC, '~~/', &
                                               XISRFV, 3,      TOL/10, &
                                               OK)

!
!                                Check the relative error for the
!                                body-fixed vectors.
!
                                 CALL CHCKAD ( LABEL,  TANVEC, '~~/', &
                                               XSRFVC, 3,      TOL, &
                                               OK)

                              END IF


                              IF ( .NOT. OK ) THEN
!
!                                Perform the following test just to
!                                display the absolute magnitude of
!                                the error.
!
                                 LABEL = 'TANVEC PLANE #, PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

                                 CALL CHCKAD ( LABEL,  TANVEC, '~~', &
                                               XSRFVC, 3,      1.D-6, &
                                               OK )

                                 LABEL = 'ITANVC PLANE #, PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

                                 CALL CHCKAD ( LABEL,  ITANVC, '~~', &
                                               XISRFV, 3,      1.D-6, &
                                               OK )
                              END IF

                              IF ( ( ABCIDX .LE. 3 ) .AND. ISELL ) THEN
!
!                                The correction is 'NONE', 'CN', or
!                                'CN+S'. The shape is 'ELLIPSOID'. Look
!                                for millimeter-level agreement.

                                 CALL CHCKAD ( LABEL,  TANVEC, '~~', &
                                               XSRFVC, 3,      1.D-6, &
                                               OK )
                              END IF

!
!                             We've checked the consistency of LPOINT,
!                             TANVEC, and LPTEPC, but we haven't done
!                             anything to show that LPOINT is a limb
!                             point. Do this now.
!
!                             We need to verify that
!
!                                1) The limb point is on the target
!                                   body's surface.
!
!                                2) For the "tangent" methods, the
!                                   limb point is a point of tangency
!                                   on the target of a ray emanating
!                                   from the observer.
!
!                                   For the "guided" methods, the limb
!                                   point lies on a ray emanating from
!                                   the center of the limb of the
!                                   reference ellipsoid.
!
!                                   When the target shape is an
!                                   ellipsoid, there should be no
!                                   difference between results obtained
!                                   using the tangent and guided
!                                   methods.
!
!                                3) The limb point lies in the
!                                   correct half-plane.
!
!
!                             Check the limb point's distance from the
!                             target surface.
!
                              IF ( ISELL ) THEN
!
!                                Find the altitude of the limb point
!                                with respect to the reference
!                                ellipsoid.
!
                                 CALL NEARPT ( LPOINT,   RADII(1), &
                                               RADII(2), RADII(3), &
                                               PNEAR,    ALT      )
                                 CALL CHCKXC ( .FALSE., ' ', OK   )

!
!                                For ellipsoidal targets, one micron
!                                is a generous tolerance.
!
                                 TOL = 1.D-9


                                 LABEL = 'LIMBPT ALT, PLANE #, PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

                                 CALL CHCKSD ( LABEL, ALT, '~', &
                                               0.D0,  TOL, OK  )

                              ELSE
!
!                                This is the DSK case.
!
!                                We'll create an inward-pointing ray
!                                that ideally passes through the limb
!                                point, and we'll find the ray-surface
!                                intercept. This intercept should be
!                                close to the original limb point.

                                 CALL VSCL   ( 10.D0,   LPOINT, VERTEX )
                                 CALL VMINUS ( VERTEX, RAYDIR )


                                 NSURF = 1

                                 IF ( ISHI ) THEN

                                    SURFID = 1
                                 ELSE
                                    SURFID = 2
                                 END IF


                                 CALL DSKXV  ( .FALSE., TARGET, NSURF, &
                                               SURFID,  ET,     TRGFRM, &
                                               1, &
                                               VERTEX,  RAYDIR, XPT, &
                                               FOUND          )
                                 CALL CHCKXC ( .FALSE., ' ', OK   )

                                 CALL CHCKSL ( 'DSKXV FOUND', FOUND, &
                                               .TRUE.,         OK    )

!
!                                Our ray should hit the DSK surface very
!                                close to the limb point. One micron is
!                                enough margin.
!
                                 TOL   = 1.D-9

                                 DIST  = VDIST( XPT, LPOINT )


                                 LABEL = 'LIMBPT DIST, PLANE #, PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

                                 CALL CHCKSD ( LABEL, DIST, '~', &
                                               0.D0,  TOL, OK  )

                              END IF

!
!                             Verify that the putative limb point is
!                             really on the limb.
!
!                             Create the central axis and normal to the
!                             current cutting half-plane. We'll use
!                             these for the DSK case below, and for the
!                             next set of tests, in which the limb
!                             point is tested for inclusion in the
!                             cutting half- plane.

                              CALL VSUB ( LPOINT, TANVEC, AXIS )

                              CALL VROTV ( REFVEC,       AXIS, &
                                           (I-1)*ROLSTP, PLNVEC )

                              CALL CHCKXC ( .FALSE., ' ', OK   )

                              CALL UCRSS ( AXIS, PLNVEC, CUTNML )

!
                              IF ( ISELL ) THEN
!
!                                This is the ellipsoid case.
!
!                                If LPOINT is all it's claimed to be,
!                                the outward normal at LPOINT should be
!                                orthogonal to the observer-limb point
!                                vector.
!
                                 CALL SURFNM ( RADII(1), RADII(2), &
                                               RADII(3), LPOINT, &
                                               NORMAL             )
                                 CALL CHCKXC ( .FALSE., ' ', OK   )

                                 CALL VHAT ( TANVEC, UTAN )

                                 D   = VDOT( NORMAL, UTAN )

                                 TOL = VTIGHT


                                 LABEL = '<LIMBPT, TANGNT>, PLANE #, ' &
                                 //      'PT #'
                                 CALL REPMI ( LABEL, '#', I, LABEL )
                                 CALL REPMI ( LABEL, '#', J, LABEL )

                                 CALL CHCKSD ( LABEL, D,   '~', &
                                               0.D0,  TOL, OK  )

                              ELSE
!
!                                This is the DSK case.
!
!                                We could check the relative orientation
!                                of the outward normal and the
!                                associated tangent vector, but we
!                                can't expect these vectors to be
!                                orthogonal.
!
!                                We can perform a more accurate check
!                                by determining whether a small
!                                rotation of the ray defined by the
!                                observer and the tangent vector,
!                                performed within the cutting
!                                half-plane, will move the ray off of
!                                the target.

                                 IF ( ISTAN ) THEN
!
!                                   This is the DSK tangent case.
!
!                                   Rotate the tangent ray away from
!                                   the axis by twice the angular
!                                   search tolerance. Presuming the ray
!                                   was nearly tangential, it should
!                                   now point off the target.
!
                                    THETA = -2 * SOLTOL

                                    CALL VROTV ( TANVEC, CUTNML, &
                                                 THETA,  RAYDIR )

                                    NSURF = 1

                                    IF ( ISHI ) THEN

                                       SURFID = 1
                                    ELSE
                                       SURFID = 2
                                    END IF

                                    CALL DSKXV ( .FALSE., TARGET, NSURF, &
                                                 SURFID,  ET, &
                                                 TRGFRM,  1, &
                                                 AXIS,    RAYDIR, &
                                                 XPT,     FOUND       )
                                    CALL CHCKXC ( .FALSE., ' ', OK   )

                                    LABEL = 'Ray hit PLANE #, ' &
                                    //      'PT #'
                                    CALL REPMI ( LABEL, '#', I, LABEL )
                                    CALL REPMI ( LABEL, '#', J, LABEL )

                                    CALL CHCKSL ( LABEL,   FOUND, &
                                                  .FALSE., OK    )

                                 ELSE
!
!                                   This is the DSK "guided" case.
!
!                                   We've already checked that the limb
!                                   point is on the DSK surface; we
!                                   need to make sure it's in the
!                                   reference ellipsoid's limb plane.
!                                   The last check we'll do will ensure
!                                   the point is in the correct cutting
!                                   half-plane.
!
!                                   Create the ellipsoid limb and
!                                   the limb plane.
!
                                    CALL EDLIMB ( RADII(1), RADII(2), &
                                                  RADII(3), AXIS, LIMB )
                                    CALL CHCKXC ( .FALSE., ' ', OK   )


                                    CALL EL2CGV ( LIMB,   CENTER, &
                                                  SMAJOR, SMINOR )
                                    CALL CHCKXC ( .FALSE., ' ', OK   )

                                    CALL PSV2PL ( CENTER, SMAJOR, &
                                                  SMINOR, LPLANE )
!
!                                   Project the guided DSK limb point
!                                   onto the limb plane.
!
                                    CALL VPRJP ( LPOINT, LPLANE, LPROJ )

                                    DIST = VDIST ( LPOINT, LPROJ )
!
!                                   One micron is a generous tolerance
!                                   for the distance of the limb point
!                                   from the ellipsoid limb plane.
!
                                    TOL = 1.D-9


                                    LABEL = 'LIMBPT ERROR, PLANE #, ' &
                                    //      'PT #'
                                    CALL REPMI ( LABEL, '#', I, LABEL )
                                    CALL REPMI ( LABEL, '#', J, LABEL )

                                    CALL CHCKSD ( LABEL, ALT, '~', &
                                                  0.D0,  TOL, OK  )
                                 END IF

                              END IF

!
!                             Verify that the limb point is in the
!                             cutting half-plane. This test applies to
!                             both ellipsoids and DSKs.
!
!                             The plane containing the cutting
!                             half-plane contains the origin, so the
!                             distance from the plane of a point is
!                             given by the dot product of the point
!                             with the plane's unit normal vector.
!
                              D   = VDOT( CUTNML, LPOINT )

                              TOL = 1.D-6


                              LABEL = '<LIMBPT, CUTNML>, PLANE #, ' &
                              //      'PT #'
                              CALL REPMI ( LABEL, '#', I, LABEL )
                              CALL REPMI ( LABEL, '#', J, LABEL )

                              CALL CHCKSD ( LABEL, D,   '~', &
                                            0.D0,  TOL, OK  )


!
!                             Update K to point to the next limb point.
!
                              K = K + 1

                           END DO

                        END DO
!
!                       We're finished with the consistency checks.
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
!           End of the correction locus loop.
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
!     Normal case: nested tori as target shape
!
!***********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Torus test setup' )

!
!     Create and load Nat's SPK and PCK.
!
      CALL NATSPK ( SPK3, .TRUE., HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATPCK ( PCK1, .TRUE., .TRUE. )


!
!     Create DSK containing two nested tori.
!
      IF ( EXISTS(DSK4) ) THEN
         CALL DELFIL( DSK4 )
      END IF

      SURFID = 3
      TRGFRM = 'ALPHA_VIEW_XY'

      R      = 7.0D4
      RCROSS = 1.0D4

      CALL VPACK ( 0.D0, 0.D0, 0.D0, CENTER )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, NORMAL )

      CALL T_TORUS( ALPHA, SURFID, TRGFRM, NPOLYV, NCROSS, &
                    R,     RCROSS, CENTER, NORMAL, DSK4   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R      = 4.0D4

      CALL VPACK ( 0.D0, 0.D0, 0.D0, CENTER )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, NORMAL )

      CALL T_TORUS( ALPHA, SURFID, TRGFRM, NPOLYV, NCROSS, &
                    R,     RCROSS, CENTER, NORMAL, DSK4   )


      CALL FURNSH ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )




!
!     Prepare LIMBPT call.
!
      OBSRVR = 'SUN'
      TARGET = 'ALPHA'
      CORLOC = 'CENTER'
      METHOD = 'DSK/TANGENT/UNPRIORITIZED'
      ET0    = 0
      TDELTA = 1.0D5
!
!     Loop over epochs.
!
      DO TIMIDX = 1, NTIMES

         ET = ET0 + (TIMIDX-1)*TDELTA

!
!        Loop over the aberration correction choices. For this
!        test, we use only 'NONE' and 'CN'.
!
         DO ABCIDX = 1, 2
!
!--- Case: ------------------------------------------------------
!
            ABCORR = ABCS( ABCIDX )

            TITLE = 'Observer = #; Target = #; ' &
            //      'ABCORR = #; TRGFRM = #; ' &
            //      'METHOD = #; CORLOC = #; ET = #.'
            CALL REPMC ( TITLE, '#', OBSRVR,     TITLE )
            CALL REPMC ( TITLE, '#', TARGET,     TITLE )
            CALL REPMC ( TITLE, '#', ABCORR,     TITLE )
            CALL REPMC ( TITLE, '#', TRGFRM,     TITLE )
            CALL REPMC ( TITLE, '#', METHOD,     TITLE )
            CALL REPMC ( TITLE, '#', CORLOC,     TITLE )
            CALL REPMD ( TITLE, '#', ET,     14, TITLE )
            CALL TCASE ( TITLE )

!
!           Start off by computing the set of limb points
!           We'll then check the results.
!
            NCUTS  = 1
            ROLSTP = TWOPI() / NCUTS
            CALL VPACK ( 0.D0, 0.D0, 1.D0, REFVEC )

!
!           We expect a multiple limb points in each half
!           plane, so we must use a small step.
!
            SCHSTP = 1.D-4

!
!           Derive the solution tolerance from the height
!           error tolerance and the observer-target center
!           distance.
!
            CALL SPKPOS ( TARGET, ET,     TRGFRM, ABCORR, &
                          OBSRVR, TRGPOS, TRGLT          )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DIST   = VNORM( TRGPOS )

            HTOL   = 1.D-6
            SOLTOL = HTOL / DIST

            MAXN   = MAXPNT

            CALL LIMBPT ( METHOD, TARGET, ET,     TRGFRM, &
                          ABCORR, CORLOC, OBSRVR, REFVEC, &
                          ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                          MAXN,   NPTS,   POINTS, EPOCHS, &
                          TANGTS                          )
            CALL CHCKXC ( .FALSE., ' ', OK )


!
!           Check the outputs.
!
            K = 1

            DO I = 1, NCUTS

!
!              There should be 4 limb points in each cutting
!              half-plane.
!
               LABEL = 'NPTS HALF-PLANE #,'
               CALL REPMI ( LABEL, '#', I, LABEL )

               CALL CHCKSI ( LABEL, NPTS(I), '=', 4, 0, OK )


               DO J = 1, NPTS(I)
!
!                 We'll treat the Jth limb point as an
!                 ephemeris object and find its position
!                 relative to the observer.
!
                  CALL VEQU ( POINTS(1,K), LPOINT )
                  CALL VEQU ( TANGTS(1,K), TANVEC )
                  LPTEPC = EPOCHS(I)

                  SPKLOC  = 'TARGET'

                  CALL SPKCPT ( LPOINT, TARGET, TRGFRM, &
                                ET,     TRGFRM, SPKLOC, &
                                ABCORR, OBSRVR, STATE, &
                                LT                     )
                  CALL CHCKXC ( .FALSE., ' ', OK )

                  CALL VEQU ( STATE, XSRFVC )

!
!                 Since we're using the 'CENTER' locus, the light time
!                 obtained from SPKPOS should match that implied by
!                 LPTEPC.
!
                  TOL = VTIGHT

                  CALL ZZCOREPC ( ABCORR, ET, TRGLT, XTE )
                  CALL CHCKXC   ( .FALSE., ' ', OK )

                  LABEL = 'LPTEPC PLANE #, PT #'
                  CALL REPMI ( LABEL, '#', I, LABEL )
                  CALL REPMI ( LABEL, '#', J, LABEL )

                  CALL CHCKSD ( LABEL, LPTEPC, '~/', &
                                XTE,   TOL,    OK    )

!
!                 If LPOINT is correct, then the position
!                 of LPOINT relative to the observer should
!                 be equal to TANVEC.
!
                  IF ( ABCORR .EQ. 'NONE' ) THEN

                     TOL = VTIGHT

                  ELSE
!
!                    Use looser tolerances, since the aberration
!                    correction performed by LIMBPT is for the
!                    target center.

                     TOL = TIGHT

                  END IF


                  LABEL = 'TANVEC PLANE #, PT #'
                  CALL REPMI ( LABEL, '#', I, LABEL )
                  CALL REPMI ( LABEL, '#', J, LABEL )

!
!                 Check the absolute distance error for
!                 this case. Use a 5mm tolerance.
!
                  CALL CHCKAD ( LABEL,  TANVEC, '~~', &
                                XSRFVC, 3,      5.D-6, &
                                OK )

!
!                 Check the relative error for this case.
!
                  CALL CHCKAD ( LABEL,  TANVEC, '~~/', &
                                XSRFVC, 3,      TOL,  OK)


                  IF ( .NOT. OK ) THEN

!
!                    Perform the following test just to
!                    display the absolute magnitude of
!                    the error.
!
                     CALL CHCKAD ( LABEL,  TANVEC, '~~', &
                                   XSRFVC, 3,      1.D-6, &
                                   OK )
                  END IF

!
!                 We've checked the consistency of LPOINT, TANVEC, and
!                 LPTEPC, but we haven't done anything to show that
!                 LPOINT is a limb point. Do this now.
!
!                 Check the limb point's distance from the target
!                 surface.
!
!                 We'll create an inward-pointing ray that ideally
!                 passes through the limb point, and we'll find the
!                 ray-surface intercept. This intercept should be close
!                 to the original limb point.
!
!                 Create the ray's vertex using the outward normal at
!                 the surface point. The ray's direction will be the
!                 negative of the normal.

                  CALL SRFNRM ( 'DSK/UNPRIORITIZED', &
                                TARGET, ET, TRGFRM, 1, LPOINT, NORMAL )

                  CALL VLCOM  ( 1.0D0,  LPOINT, 0.1D0, NORMAL, VERTEX )
                  CALL VMINUS ( NORMAL, RAYDIR )


                  NSURF = 1

                  CALL DSKXV  ( .FALSE., TARGET, NSURF, &
                                SURFID,  ET,     TRGFRM, &
                                1, &
                                VERTEX,  RAYDIR, XPT, &
                                FOUND          )
                  CALL CHCKXC ( .FALSE., ' ', OK   )

                  CALL CHCKSL ( 'DSKXV FOUND', FOUND, &
                                .TRUE.,         OK    )

!
!                 Our ray should hit the DSK surface very
!                 close to the limb point. One micron is
!                 enough margin.
!
                  TOL   = 1.D-9

                  DIST  = VDIST( XPT, LPOINT )


                  LABEL = 'LIMBPT DIST, PLANE #, PT #'
                  CALL REPMI ( LABEL, '#', I, LABEL )
                  CALL REPMI ( LABEL, '#', J, LABEL )

                  CALL CHCKSD ( LABEL, DIST, '~', &
                                0.D0,  TOL, OK  )

!
!                 Verify that the putative limb point is
!                 really on the limb.
!
!                 Create the central axis and normal to the
!                 current cutting half-plane. We'll use these
!                 for the DSK case below, and for the next
!                 set of tests, in which the limb point is
!                 tested for inclusion in the cutting half-
!                 plane.
!
                  CALL VSUB ( LPOINT, TANVEC, AXIS )

                  CALL VROTV ( REFVEC,       AXIS, &
                               (I-1)*ROLSTP, PLNVEC )

                  CALL CHCKXC ( .FALSE., ' ', OK   )

                  CALL UCRSS ( AXIS, PLNVEC, CUTNML )

!
!                 This is the DSK tangent case.
!
!                 Rotate the tangent ray to move it off of the target
!                 surface. The rotation is within the cutting
!                 half-plane. The direction of rotation depends on the
!                 placement of the surface point: for points having
!                 outward normals pointing away from the axis, the
!                 rotation is away from the axis; for the other points,
!                 the direction is inward. For the nested tori, the
!                 points having and odd index are the ones for which
!                 the rotation is outward.
!
!                 Outward rotation about CUTNML corresponds to a
!                 negative rotation angle.
!
!                 The rotation angle is set to twice the angular search
!                 tolerance. Presuming the ray was nearly tangential,
!                 it should now point off the target.
!
                  IF ( ODD(J) ) THEN
                     THETA = - 2 * SOLTOL
                  ELSE
                     THETA =   2 * SOLTOL
                  END IF

                  CALL VROTV ( TANVEC, CUTNML, THETA, RAYDIR )


                  NSURF = 1

                  CALL DSKXV ( .FALSE., TARGET, NSURF, &
                               SURFID,  ET, &
                               TRGFRM,  1, &
                               AXIS,    RAYDIR, &
                               XPT,     FOUND       )
                  CALL CHCKXC ( .FALSE., ' ', OK   )

                  LABEL = 'Ray hit PLANE #, ' &
                  //      'PT #'
                  CALL REPMI ( LABEL, '#', I, LABEL )
                  CALL REPMI ( LABEL, '#', J, LABEL )

                  CALL CHCKSL ( LABEL, FOUND, .FALSE., OK )

!
!                 Verify that the limb point is in the cutting
!                 half-plane. This test applies to both ellipsoids and
!                 DSKs.
!
!                 The plane containing the cutting half-plane contains
!                 the origin, so the distance from the plane of a point
!                 is given by the dot product of the point with the
!                 plane's unit normal vector.
!
                  D   = VDOT( CUTNML, LPOINT )

                  TOL = 1.D-6


                  LABEL = '<LIMBPT, CUTNML>, PLANE #, ' &
                  //      'PT #'
                  CALL REPMI ( LABEL, '#', I, LABEL )
                  CALL REPMI ( LABEL, '#', J, LABEL )

                  CALL CHCKSD ( LABEL, D,   '~', &
                                0.D0,  TOL, OK  )

!
!                 Update K to point to the next limb point.
!
                  K = K + 1

               END DO

            END DO

!
!           We're finished with the consistency checks.
!
         END DO
!
!        End of the aberration correction loop.
!
      END DO
!
!     End of the time loop.
!

!
!     Unload the torus DSK. Leaving it loaded would interfere
!     with some of the following tests.
!
      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


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
      NCUTS  = 1
      METHOD = 'DSK/UNPRIORITIZED/GUIDED'
      CORLOC = 'CENTER'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   XPOINT, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODDEF ( 'JUPITER', 499 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'POINTS', POINTS, '=', XPOINT, 3, 0.D0, OK )


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


      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'POINTS', POINTS, '=', XPOINT, 3, 0.D0, OK )

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

      METHOD = 'DSK/UNPRIORITIZED/GUIDED/surfaces = 1'



      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   XPOINT, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SRFNMS(1) = 'AAAbbb'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )



      METHOD = 'guided/dsk/unprioritized/surfaces = AAAbbb'


      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'POINTS', POINTS, '=', XPOINT, 3, 0.D0, OK )

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



      METHOD = 'guided/dsk/unprioritized/surfaces = low-res'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   XPOINT, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Unload the high-res DSK; set METHOD to remove
!     surface specification.
!
      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'guided/dsk/unprioritized'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'POINTS', POINTS, '=', XPOINT, 3, 0.D0, OK )


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

      METHOD = 'guided/dsk/unprioritized'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure the result matches that obtained with the
!     high-res DSK specified.
!
      METHOD = 'guided/dsk/unprioritized/ SURFACES = "HIGH-RES" '

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   XPOINT, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'POINTS', POINTS, '=', XPOINT, 3, 0.D0, OK )






!***********************************************************************
!
!     Error handling tests follow.
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid method.' )

      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'



      METHOD = 'ELLIPSOID TANGENT'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )

      METHOD = 'DSK/GUIDED'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )

      METHOD = 'DSK/UNPRIORITIZED'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDLIMBTYPE)', OK )

      METHOD = 'UNPRIORITIZED /DSK/guided/INTERCEPT'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)', OK )


      METHOD = 'UNPRIORITIZED/umbral /DSK/guided/'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)', OK )

!
!     Restore a valid method. We'll use the tangent limb
!     type because it requires the search step and
!     tolerance.
!
      METHOD = 'TANGENT/DSK/UNPRIORITIZED'

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid target name.' )

      CALL LIMBPT ( METHOD, 'marr', ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid observer name.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, 'sn',   REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer is target.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, TARGET, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid reference frame center' )

      CALL LIMBPT ( METHOD, TARGET, ET,     'IAU_MOON', &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reference frame not found' )

      CALL LIMBPT ( METHOD, TARGET, ET,     'IAU_M', &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid aberration correction' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    'L',    CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
!     Test SAVE logic by repeating the call.
!
      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    'L',    CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Relativistic aberration correction' )
      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    'RL',   CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Stellar aberration correction w/o light time' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    'S',    CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid aberration correction locus' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, 'dsk',  OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDLOCUS)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid reference vector.' )

      CALL CLEARD ( 6, STATE )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, STATE, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid cut count.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, 0,      SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, -1,     SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, MAXN+1, SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid maximum point count.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    0,      NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    -1,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSIZE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid search tolerance.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, -1.D0, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDTOLERANCE)', OK )


      CALL LIMBPT ( 'ellipsoid/tangent', &
                    TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  -1.D0,  SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid search step.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  -1.D0,  SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSEARCHSTEP)', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  0.D0,   SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSEARCHSTEP)', OK )


      CALL LIMBPT ( 'ellipsoid/tangent', &
                    TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  -1.D0,  SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid roll step.' )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    0.D0,   2,      SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDROLLSTEP)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No loaded SPK files' )

      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDFILES)', OK )


      CALL SPKLEF ( SPK1, HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKLEF ( SPK2, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKLEF ( SPK3, HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No ephemeris data for observer' )



      CALL LIMBPT ( METHOD, TARGET, ET,       FIXREF, &
                    ABCORR, CORLOC, 'GASPRA', REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP,   SOLTOL, &
                    MAXN,   NPTS,   POINTS,   EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No ephemeris data for target' )

      CALL FURNSH ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LIMBPT ( METHOD, 'ALPHA',  ET,     'ALPHAFIXED', &
                    ABCORR, CORLOC,   OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,    SCHSTP, SOLTOL, &
                    MAXN,   NPTS,     POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No orientation data for target' )


!
!     Fetch target radii.
!
      CALL BODVCD (  499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Clear the kernel pool.
!
      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore target radii so we can get to the error
!     condition we're looking for.
!
      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No radius data for target' )

!
!     We need a method that uses an ellipsoid for this one.
!
      METHOD = 'DSK / UNPRIORITIZED / GUIDED'

      CALL DVPOOL ( 'BODY499_RADII' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )


      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad radius data for target' )


!
!     Fetch original radii.
!
      CALL BODVCD (  499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Overwrite good radii with bad in the kernel pool.
!
      CALL VPACK  ( -1.D0, 0.D0,  3.D0, BADRAD )

      CALL PDPOOL ( 'BODY499_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDAXISLENGTH)', OK )

!
!     Replace original radii.
!
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad radius count for target' )


!
!     Fetch original radii.
!
      CALL BODVCD (  499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Overwrite good radii with short array
!     in the kernel pool.
!
      CALL PDPOOL ( 'BODY499_RADII', 2, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(BADRADIUSCOUNT)', OK )

!
!     Replace original radii.
!
      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Not enough room in output arrays.' )

!
!     The checks on NCUTS and MAXN prevent this situation from
!     occurring in the usual case, in which there is one limb
!     point per cutting half-plane. The torus case can present
!     problems.
!
!     Prepare LIMBPT call.
!
      OBSRVR = 'SUN'
      TARGET = 'ALPHA'
      FIXREF = 'ALPHAFIXED'
      CORLOC = 'CENTER'
      METHOD = 'DSK/TANGENT/UNPRIORITIZED'
      ET     = 0.D0
      NCUTS  = 2
      MAXN   = 2

      CALL FURNSH ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKLEF ( SPK3, HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(OUTOFROOM)', OK )

      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'GUIDED limb definition w/ ELLIPSOID LIMB ' &
      //           'correction locus.'                        )

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'
      CORLOC = 'ELLIPSOID LIMB'

      METHOD = 'GUIDED/DSK/UNPRIORITIZED'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(BADLIMBLOCUSMIX)', OK )

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

      ET     = 8 * JYEAR()

      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      OBSRVR = 'EARTH'
      ABCORR = 'NONE'


      METHOD = 'guided/dsk/unprioritized'

      CALL LIMBPT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, CORLOC, OBSRVR, REFVEC, &
                    ROLSTP, NCUTS,  SCHSTP, SOLTOL, &
                    MAXN,   NPTS,   POINTS, EPOCHS, &
                    TANGTS                          )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )


!
! --- Case: ------------------------------------------------------
!

!
!     Clean up.
!
      CALL KCLEAR


      CALL DELFIL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK1      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK2      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(3) )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK3      )
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

      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK4      )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END



!***********************************************************************
!
!     S U P P O R T   U T I L I T I E S
!
!***********************************************************************

!
!     None so far....
!
