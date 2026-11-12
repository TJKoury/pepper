!$Procedure      F_ILUMIN ( ILUMIN family tests )

      SUBROUTINE F_ILUMIN ( OK )
      use pepper_legacy
      use pepper_inc_zzdsk
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Exercise the higher-level SPICELIB geometry routine ILUMIN.
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
!     geometry routine ILUMIN.
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
!-    TSPICE Version 3.0.0 15-AUG-2016 (NJB)
!
!        Updated to test DSK usage. Also more thoroughly
!        tests ellipsoid target cases.
!
!-    TSPICE Version 2.1.0, 27-AUG-2013 (NJB)
!
!        Loosened TIGHT tolerance 1.D-11.
!
!-    TSPICE Version 2.0.0, 22-FEB-2011 (NJB)
!
!        Loosened MED tolerance parameter to 1.D-8. This
!        parameter is used for tests involving 'LT'
!        light time corrections.
!
!-    TSPICE Version 1.0.0, 27-FEB-2008 (NJB)
!
!-&


!
!     SPICELIB functions
!


!
!     Local parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0  = 'ilumin_dsk0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1  = 'ilumin_dsk1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2  = 'ilumin_dsk2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3  = 'ilumin_dsk3.bds' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK   = 'test_0008.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'ilumin_spk.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2  = 'orbiter.bsp' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT  = 1.D-14 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT   = 1.D-12 )

      DOUBLE PRECISION      MTIGHT
      PARAMETER           ( MTIGHT  = 1.D-10 )

      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 5.D-6 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 240 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NREF
      PARAMETER           ( NREF   = 1 )

      INTEGER               NABC
      PARAMETER           ( NABC   = 5 )

      INTEGER               ABCLEN
      PARAMETER           ( ABCLEN = 10 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               NOBS
      PARAMETER           ( NOBS   = 2 )

      INTEGER               NTARG
      PARAMETER           ( NTARG  = 2 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499 )

      INTEGER               MAXPTS
      PARAMETER           ( MAXPTS = 100 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 4 )

      INTEGER               NMETH
      PARAMETER           ( NMETH  = 4 )

!
!     Local variables
!
      CHARACTER*(ABCLEN)    ABCS   ( NABC )
      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(NAMLEN)    KVNAME
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

      DOUBLE PRECISION      ALTRAD ( 3 )
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      DLAT
      DOUBLE PRECISION      DLON
      DOUBLE PRECISION      ELTS   ( 8 )
      DOUBLE PRECISION      EMISSN
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      INCDNC
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LONLAT ( 2, MAXPTS )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      SUNLT
      DOUBLE PRECISION      SUNSTA ( 6 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      XEMISN
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XINCDC
      DOUBLE PRECISION      XPHASE
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               BODYID
      INTEGER               HANDLE ( 2 )
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               MIX
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NPTS
      INTEGER               NSFLAT
      INTEGER               NSFLON
      INTEGER               OBSCDE
      INTEGER               OBSIDX
      INTEGER               PTIDX
      INTEGER               REFIDX
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )
      INTEGER               SURFID
      INTEGER               TRGCDE
      INTEGER               TRGIDX

      LOGICAL               FOUND
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
                                      'Lt', &
                                      'Lt+s', &
                                      'XCn', &
                                      'Cn+s'   /

!
!     REFS is a two-dimensional array. There's a set of
!     ray reference  frames for each target. Currently
!     there are only two targets: Mars and Phobos.
!
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
            'ELLIPSOID', &
            'dsk/unprioritized/surfaces="high-res"', &
            'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"', &
            'UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"' &
                                   /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ILUMIN' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create SPK, PCK file.' )

      CALL TSTSPK ( SPK1, .TRUE., HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create the PCK file, and load it. Do not delete it.
!
      CALL T_PCK08 ( PCK, .TRUE., .TRUE. )
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
                    ELTS(8),      1,            STATE0,    [ET]        )

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
      CALL PIPOOL ( 'NAIF_BODY_CODE', 1,  [SCID]      )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Add an incomplete frame definition to the kernel pool;
!     we'll need this later.
!
      CALL PIPOOL ( 'FRAME_BAD_NAME', 1, [-666] )


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
      NLON   =  200
      NLAT   =  100

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
!     Surface 2 for Mars is very low-res. We also use a
!     different scale for the Mars radii used to create
!     the tessellated shape model.
!
      BODYID = TRGCDE
      SURFID = 2
      NLON   = 40
      NLAT   = 20

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
      END IF

      KVNAME = 'BODY499_RADII'
      CALL GDPOOL ( KVNAME, 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '499 radii FOUND', FOUND, .TRUE., OK )

      CALL VSCL ( 2.D0, RADII, ALTRAD )

      CALL PDPOOL ( KVNAME, 3, ALTRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create and load the second DSK.
!
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore normal Mars radii.
!
      CALL PDPOOL ( KVNAME, 3, RADII )
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

      KVNAME = 'BODY401_RADII'
      CALL GDPOOL ( KVNAME, 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( '401 radii FOUND', FOUND, .TRUE., OK )

      CALL VSCL ( 3.D0, RADII, ALTRAD )

      CALL PDPOOL ( KVNAME, 3, ALTRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


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
!     Restore normal Phobos radii.
!
      CALL PDPOOL ( KVNAME, 3, RADII )
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
!     Generate a grid of planetocentric longitude/latitude
!     coordinate pairs. These, combined with surface models,
!     will yield a grid of surface points at which to
!     compute illumination angles.
!
      NSFLON = 4
      NSFLAT = 5
      NPTS   = NSFLON * NSFLAT

      DLON   = 2*PI() /   NSFLON
      DLAT   =   PI() / ( NSFLAT - 1 )

      K = 0

      DO I = 1, NSFLON
!
!        We shift the coordinates away from possible plate
!        edges because we can't expect the normal vectors
!        to match our computed values at those locations.
!
         LON = (I-1)*DLON + 1.D-3

         DO J = 1, NSFLAT

            LAT = BRCKTD(  HALFPI() - (J-1)*DLAT,  -HALFPI(), HALFPI() )

            IF ( J .EQ. 1 ) THEN

               LAT = LAT - 1.D-3

            ELSE IF ( J .EQ. NSFLAT ) THEN

               LAT = LAT + 1.D-3

            END IF


            K   = K + 1

            LONLAT(1,K) = LON
            LONLAT(2,K) = LAT

         END DO

      END DO


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
!           Get target radii.
!
            CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Loop over the surface point sequence.
!
            DO PTIDX = 1, NPTS

               LON = LONLAT(1,PTIDX)
               LAT = LONLAT(2,PTIDX)

!
!              Loop over every aberration correction choice.
!
               DO ABCIDX = 1, NABC

                  ABCORR = ABCS ( ABCIDX )
!
!                 Set up some logical variables describing the
!                 attributes of the selected correction.
!
                  USELT  = ABCORR      .NE. 'None'

                  USECN  = ABCORR(1:2) .EQ. 'Cn'

!
!                 Loop over every target body-fixed frame choice.
!
                  DO REFIDX = 1, NREF

                     TRGFRM = REFS ( REFIDX, TRGIDX )

!
!                    Loop over all method choices.
!

                     DO MIX = 1, NMETH

                        METHOD = METHDS( MIX )

!
! --- Case: ------------------------------------------------------
!
                        TITLE = 'Observer = #; Target = #; ' &
                        //      'ABCORR = #; TRGFRM = #; ' &
                        //      'METHOD = #; Longitude (deg) = #; ' &
                        //      'Latitude (deg) = #; ET = #.'
                        CALL REPMC ( TITLE, '#', OBSRVR,        TITLE )
                        CALL REPMC ( TITLE, '#', TARGET,        TITLE )
                        CALL REPMC ( TITLE, '#', ABCORR,        TITLE )
                        CALL REPMC ( TITLE, '#', TRGFRM,        TITLE )
                        CALL REPMC ( TITLE, '#', METHOD,        TITLE )
                        CALL REPMD ( TITLE, '#', LON*DPR(), 14, TITLE )
                        CALL REPMD ( TITLE, '#', LAT*DPR(), 14, TITLE )
                        CALL REPMD ( TITLE, '#', ET,        14, TITLE )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        CALL TCASE ( TITLE )

!
!                       Generate the surface point we're going to work
!                       with. We do this here because we need the
!                       current method in order to generate a point on
!                       the surface.
!
                        CALL LATSRF ( METHOD, TARGET, ET, &
                                      TRGFRM, 1,      LONLAT(1,PTIDX), &
                                      SPOINT                           )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!                       Get the outward surface normal vector at
!                       SPOINT.
!
                        CALL SRFNRM ( METHOD, TARGET, ET,    TRGFRM, &
                                      1,      SPOINT, NORMAL        )
                        CALL CHCKXC   ( .FALSE., ' ', OK )

!
                        IF ( .NOT. OK ) THEN
                           STOP
                        END IF

!
!                       Start off by computing the illumination angles.
!                       We'll then check the results.
!
                        CALL ILUMIN ( METHOD, TARGET, ET, &
                                      TRGFRM, ABCORR, OBSRVR, &
                                      SPOINT, TRGEPC, SRFVEC, &
                                      PHASE,  INCDNC, EMISSN  )
                        CALL CHCKXC ( .FALSE., ' ', OK )

                        IF ( .NOT. OK ) THEN
                           STOP
                        END IF


!
!                       We'll treat the input surface point an
!                       ephemeris object and find its position relative
!                       to the observer.
!
                        CALL SPKCPT ( SPOINT, TARGET,   TRGFRM, ET, &
                                      TRGFRM, 'TARGET', ABCORR, OBSRVR, &
                                      STATE,  LT                     )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       If SRFVEC is correct, then the position of
!                       SPOINT relative to the observer should be equal
!                       to SRFVEC. The light time obtained from SPKCPT
!                       should match that implied by TRGEPC.
!
                        TOL = TIGHT

                        CALL ZZCOREPC ( ABCORR, ET, LT, XTE )
                        CALL CHCKXC   ( .FALSE., ' ', OK )

                        CALL CHCKSD ( 'TRGEPC', TRGEPC, '~/', &
                                      XTE,      TOL,    OK    )

                        IF ( USELT ) THEN

                           IF ( USECN ) THEN
                              TOL = MTIGHT
                           ELSE
                              TOL = LOOSE
                           END IF

                        ELSE
                           TOL = VTIGHT
                        END IF

                        CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', &
                                      STATE,    3,      TOL,  OK )

!
!                       We've checked the consistency of SPOINT,
!                       SRFVEC, and TRGEPC, but we haven't done
!                       anything to test the illumination angles.
!                       Do that now.
!
!                       We need the vectors used to define the
!                       illumination angles.
!

!                       Negate the observer-surface point vector.
!
                        CALL VMINUS ( SRFVEC, OBSPOS )

!
!                       Get the apparent position of the sun as seen
!                       from the surface point at TRGEPC.
!
                        CALL SPKCPO ( 'SUN',      TRGEPC, TRGFRM, &
                                      'OBSERVER', ABCORR, SPOINT, &
                                      TARGET,     TRGFRM, SUNSTA, &
                                      SUNLT                       )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       Compute the expected illumination angles.
!
                        XPHASE = VSEP( SUNSTA, OBSPOS )
                        XINCDC = VSEP( SUNSTA, NORMAL )
                        XEMISN = VSEP( OBSPOS, NORMAL )

!
!                       Since we're doing a consistency check, we
!                       expect to get very close agreement with
!                       ILUMIN.
!
                        TOL = VTIGHT

                        CALL CHCKSD ( 'PHASE', PHASE, '~/', &
                                      XPHASE,  TOL,   OK   )

                        CALL CHCKSD ( 'INCDNC', INCDNC, '~/', &
                                      XINCDC,   TOL,   OK   )

                        CALL CHCKSD ( 'EMISSN', EMISSN, '~/', &
                                      XEMISN,   TOL,   OK   )

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
!           End of the surface point loop.
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
!     Set target and target-fixed frame.
!
      OBSRVR = 'EARTH'
      TARGET = 'MARS'
      FIXREF = 'IAU_MARS'
      METHOD = 'ellipsoid'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, XEPOCH, &
                    XSRFVC, XPHASE, XINCDC, XEMISN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILUMIN ( METHOD, '499',  ET,     FIXREF, &
                    ABCORR, '399',  SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE, '=', &
                     XPHASE,  TOL,   OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=', &
                     XINCDC,  TOL,   OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=', &
                     XEMISN,  TOL,   OK   )




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


      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, XEPOCH, &
                    XSRFVC, XPHASE, XINCDC, XEMISN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODDEF ( 'JUPITER', 499 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact matches here.
!
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=', &
                     XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=', &
                     XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=', &
                     XEMISN,  TOL,    OK   )

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


      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We expect exact matches here.
!
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=', &
                     XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=', &
                     XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=', &
                     XEMISN,  TOL,    OK   )
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



      METHOD = 'dsk/unprioritized/surfaces = 1'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, XEPOCH, &
                    XSRFVC, XPHASE, XINCDC, XEMISN )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SRFNMS(1) = 'AAAbbb'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )



      METHOD = 'dsk/unprioritized/surfaces = AAAbbb'

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     We expect exact matches here.
!
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=', &
                     XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=', &
                     XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=', &
                     XEMISN,  TOL,    OK   )
!
!     Restore original mapping.
!
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK )

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



      METHOD = 'dsk/unprioritized/surfaces = low-res'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, XEPOCH, &
                    XSRFVC, XPHASE, XINCDC, XEMISN )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Unload the high-res DSK; set METHOD to remove
!     surface specification.
!
      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'dsk/unprioritized'

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact matches here.
!
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=', &
                     XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=', &
                     XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=', &
                     XEMISN,  TOL,    OK   )

!
!     Restore original mapping.
!
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK )


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

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, XEPOCH, &
                    XSRFVC, XPHASE, XINCDC, XEMISN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure the result matches that obtained with the
!     high-res DSK specified.
!
      METHOD = 'dsk/unprioritized/ SURFACES = "HIGH-RES" '

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect exact matches here.
!
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


      CALL CHCKSD ( 'PHASE',  PHASE,  '=', &
                     XPHASE,  TOL,    OK   )

      CALL CHCKSD ( 'INCDNC', INCDNC, '=', &
                     XINCDC,  TOL,    OK   )

      CALL CHCKSD ( 'EMISSN', EMISSN, '=', &
                     XEMISN,  TOL,    OK   )
!
!     Restore original mapping.
!
      CALL BODDEF ( 'SUN', 10 )
      CALL CHCKXC ( .FALSE., ' ', OK )



!***********************************************************************
!
!     Error handling tests follow.
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid method.' )


      TARGET = 'EARTH'
      FIXREF = 'IAU_EARTH'
      OBSRVR = 'SUN'
      ABCORR = 'NONE'


      METHOD = 'ELLIPSOID'

      CALL LATSRF ( METHOD, TARGET, ET, &
                    FIXREF, 1,      LONLAT(1,NPTS/2), SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILUMIN ( 'ELLIPSID', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC, PHASE, INCDNC, EMISSN )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )



      CALL ILUMIN ( 'INTERCEPT ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC, PHASE, INCDNC, EMISSN )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )



      CALL ILUMIN ( '/DSK', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC, PHASE, INCDNC, EMISSN )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


      CALL ILUMIN ( '/DSK/UPRIORTIZED', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC, PHASE, INCDNC, EMISSN )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )



      CALL ILUMIN ( 'DSK', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC, PHASE, INCDNC, EMISSN )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid observer name.' )

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, 'erth', SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid target name.' )

      CALL ILUMIN ( METHOD, 'su',   ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer is target.' )

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, TARGET, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid reference frame center' )

      CALL ILUMIN ( METHOD, TARGET, ET,     'IAU_MOON', &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid aberration correction' )

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    'LTT',  OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
!     Test SAVE logic by repeating the call.
!

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    'LTT',  OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Relativistic aberration correction' )

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    'RL',   OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Stellar aberration correction w/o light time' )

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    'S',   OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No loaded SPK files' )

      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )

      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDFILES)', OK )


      CALL SPKLEF ( SPK1, HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKLEF ( SPK2, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No ephemeris data for observer' )

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, '1000', SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No ephemeris data for target' )

      CALL ILUMIN ( METHOD, 'gaspra', ET,     'IAU_GASPRA', &
                    ABCORR, OBSRVR,   SPOINT, TRGEPC, &
                    SRFVEC, PHASE,    INCDNC, EMISSN     )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No orientation data for target' )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LDPOOL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No radius data for target' )

      CALL DVPOOL ( 'BODY399_RADII' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILUMIN ( 'ELLIPSOID', TARGET, ET,     FIXREF, &
                    ABCORR,      OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC,      PHASE,  INCDNC, EMISSN )


      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL LDPOOL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad radius data for target' )


!
!     Fetch original radii.
!
      CALL BODVCD (  399, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Overwrite good radii with bad in the kernel pool.
!
      CALL VPACK  ( -1.D0, 0.D0,  3.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )

!
!     Replace original radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

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


      METHOD = 'dsk/unprioritized'

      CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, &
                    SRFVEC, PHASE,  INCDNC, EMISSN )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )

!
! --- Case: ------------------------------------------------------
!

!
!     Clean up.
!
      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

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
