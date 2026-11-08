!$Procedure      F_SUBPNT ( SUBPNT family tests )

      SUBROUTINE F_SUBPNT ( OK )

! $ Abstract
!
!     Exercise the higher-level SPICELIB geometry routine SUBPNT.
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

      INCLUDE 'dsk.inc'
      INCLUDE 'dskdsc.inc'
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
!     geometry routine SUBPNT.
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
!-    TSPICE Version 4.0.0 04-AUG-2020 (NJB)
!
!        Updated to include a test using a DSK with time range not
!        including the J2000 epoch.
!
!-    TSPICE Version 3.0.0 06-APR-2016 (NJB)
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
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      RPD

      LOGICAL               EXISTS
      LOGICAL               MATCHI
!
!     Local parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0  = 'subpnt_dsk0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1  = 'subpnt_dsk1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2  = 'subpnt_dsk2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3  = 'subpnt_dsk3.bds' )

      CHARACTER*(*)         DSK4
      PARAMETER           ( DSK4  = 'subpnt_dsk4.bds' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK   = 'test_0008.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1  = 'subpnt_spk.bsp' )

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
      PARAMETER           ( NREF = 1 )

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

      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 3 )

      INTEGER               SCID
      PARAMETER           ( SCID   = -499 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 4 )

      INTEGER               NMETH
      PARAMETER           ( NMETH  = 8 )

!
!     Local variables
!
      CHARACTER*(ABCLEN)    ABCS   ( NABC )
      CHARACTER*(ABCLEN)    ABCORR
      CHARACTER*(NAMLEN)    FIXREF
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

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      BOUNDS ( 2, 3 )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      ELTS   ( 8 )
      DOUBLE PRECISION      ESUBPT ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      OBSPOS ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      STATE0 ( 6 )
      DOUBLE PRECISION      SUBDIR ( 3 )
      DOUBLE PRECISION      TDELTA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XSUBPT ( 3 )
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      XTE

      INTEGER               ABCIDX
      INTEGER               BODYID
      INTEGER               HANDLE ( 2 )
      INTEGER               MIX
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               OBSCDE
      INTEGER               OBSIDX
      INTEGER               REFIDX
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )
      INTEGER               SURFID
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               TIMIDX
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
            'ELLIPSOID / intercept', &
            'intercept: ellipsoid', &
            'near point: ellipsoid', &
            'near point /ellipsoid', &
            'nadir/dsk/unprioritized/surfaces="high-res"', &
            'dsk/ nadir /unprioritized/surfaces="high-res"', &
            'intercept/ UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"', &
            'intercept/UNPRIORITIZED/ dsk /SURFACES ="LOW-RES"' &
                                   /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_SUBPNT' )

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
      TDELTA = JYEAR()

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
!     Create DSK for Mars with coverage shifted from the normal
!     range.
!
!     Use surface 3.
!
      TARGET = 'MARS'
      TRGCDE = 499
      FIXREF = 'IAU_MARS'

      BODYID = TRGCDE
      SURFID = 3
      NLON   = 40
      NLAT   = 20

      FIRST  = JYEAR()
      LAST   = JYEAR() * 2

      IF ( EXISTS(DSK4) ) THEN
         CALL DELFIL( DSK4 )
      END IF

      CALL CLEARD( NSYPAR, CORPAR )

      CALL BODVCD( TRGCDE, 'RADII', 3, N, RADII )

      BOUNDS(1,1) = 0.D0
      BOUNDS(2,1) = 2 * PI()
      BOUNDS(1,2) = -PI()/2
      BOUNDS(2,2) =  PI()/2
      BOUNDS(1,3) = 0.D0
      BOUNDS(2,3) = RADII(1)

      CALL T_ELDSK2 ( BODYID,   SURFID, FIXREF,  FIRST,    LAST, &
                      LATSYS,   CORPAR, BOUNDS,  RADII(1), RADII(2), &
                      RADII(3), NLON,   NLAT,   .FALSE.,   DSK4     )

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
!           Get target radii.
!
            CALL BODVAR ( TRGCDE, 'RADII', N, RADII )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           Loop over the time sequence.
!
            DO TIMIDX = 1, NTIMES

               ET = ET0 + (TIMIDX-1)*TDELTA

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
                        //      'METHOD = #; ET = #.'
                        CALL REPMC ( TITLE, '#', OBSRVR,     TITLE )
                        CALL REPMC ( TITLE, '#', TARGET,     TITLE )
                        CALL REPMC ( TITLE, '#', ABCORR,     TITLE )
                        CALL REPMC ( TITLE, '#', TRGFRM,     TITLE )
                        CALL REPMC ( TITLE, '#', METHOD,     TITLE )
                        CALL REPMD ( TITLE, '#', ET,     14, TITLE )

                        CALL TCASE ( TITLE )

!
!                       Start off by computing the sub-observer point.
!                       We'll then check the results.
!
                        CALL SUBPNT ( METHOD, TARGET, ET, &
                                      TRGFRM, ABCORR, OBSRVR, &
                                      SPOINT, TRGEPC, SRFVEC )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       We'll treat the computed sub-observer point as
!                       an ephemeris object and find its position
!                       relative to the observer.
!
                        CALL SPKCPT ( SPOINT, TARGET,   TRGFRM, ET, &
                                      TRGFRM, 'TARGET', ABCORR, OBSRVR, &
                                      STATE,  LT                     )
                        CALL CHCKXC ( .FALSE., ' ', OK )

!
!                       If SPOINT is correct, then the position of
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
!                       anything to show that SPOINT is a sub-observer
!                       point. Do that now.
!

!                       Compute the position of the observer relative to
!                       the target center.
!
                        CALL VSUB ( SPOINT, STATE, OBSPOS )


                        FOUND = .FALSE.

                        IF (  MATCHI( METHOD, '*INTERCEPT*', &
                                      '*',    '?'            ) ) THEN

                           IF (  MATCHI( METHOD, '*ELLIPSOID*', &
                                         '*',    '?'            ) ) THEN

                              CALL SURFPT ( OBSPOS,   STATE, &
                                            RADII(1), RADII(2), &
                                            RADII(3), XSUBPT,  FOUND )
                              CALL CHCKXC   ( .FALSE., ' ', OK )


                           ELSE
!
!                             This is the DSK case. Note that we're
!                             working with the target epoch (although
!                             in this case, it's only used for DSK
!                             segment selection).
!
!                             Set the surface list according to the
!                             value of the surface list in METHOD.
!
                              IF ( MATCHI( METHOD, '*HIGH-RES*', &
                                           '*',    '?'          ) ) THEN

                                 SRFLST(1) = 1
                              ELSE
                                 SRFLST(1) = 2
                              END IF


                              CALL DSKXV ( .FALSE., TARGET, 1, SRFLST, &
                                           TRGEPC,  TRGFRM, 1, OBSPOS, &
                                           STATE,   XSUBPT,    FOUND  )
                              CALL CHCKXC   ( .FALSE., ' ', OK )

                           END IF

                           CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!                          Use the tolerance we set for the
!                          SRFVEC test.
!
!                          To simplify the comparison, we'll convert
!                          the expected sub-observer point to an
!                          expected observer-surface vector. This will
!                          give us a vector having the scale on which
!                          we based our tolerance magnitude.
!
                           CALL VSUB ( XSUBPT, OBSPOS, XSRFVC )

                           CALL CHCKAD( 'SRFVEC (2)', SRFVEC, '~~/', &
                                        XSRFVC,    3, TOL,    OK   )

                        ELSE
!
!                          This is the "near point" or "nadir" case.
!
!                          We need the ellipsoid near point for both
!                          the ellipsoid and DSK target shape cases.
!
                           CALL NEARPT ( OBSPOS,   RADII(1), &
                                         RADII(2), RADII(3), &
                                         ESUBPT,   ALT       )
                           CALL CHCKXC   ( .FALSE., ' ', OK )


                           IF (  MATCHI( METHOD, '*ELLIPSOID*', &
                                         '*',    '?'            ) ) THEN
!
!                             This is the ellipsoid case.
!
                              CALL VEQU ( ESUBPT, XSUBPT )

                           ELSE
!
!                             This is the DSK case.
!
!                             Compute the direction vector of a ray
!                             pointing from the observer to the
!                             ellipsoid near point.
!
                              CALL VSUB ( ESUBPT, OBSPOS, SUBDIR )
!
!                             Set the surface list according to the
!                             value of the surface list in METHOD.
!
                              IF ( MATCHI( METHOD, '*HIGH-RES*', &
                                           '*',    '?'          ) ) THEN

                                 SRFLST(1) = 1
                              ELSE
                                 SRFLST(1) = 2
                              END IF

!
!                             Update XSUBPT to be the DSK surface
!                             intercept of the ray from the observer to
!                             the ellipsoid near point.
!
                              CALL DSKXV ( .FALSE., TARGET, 1, SRFLST, &
                                           TRGEPC,  TRGFRM, 1, OBSPOS, &
                                           SUBDIR,  XSUBPT,    FOUND  )
                              CALL CHCKXC   ( .FALSE., ' ', OK )

                              CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

                           END IF
!
!                          Update XSRFVC based on the expected near
!                          point or nadir point.
!
                           CALL VSUB ( XSUBPT, OBSPOS, XSRFVC )

                           CALL CHCKAD( 'SRFVEC (2)', SRFVEC, '~~/', &
                                        XSRFVC,    3, TOL,    OK   )


                        END IF
!
!                       We're finished with the consistency checks for
!                       the intercept and near point/nadir cases.
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
!           End of the time loop.
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
      METHOD = 'ellipsoid/intercept'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, XSUBPT, XEPOCH, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPNT ( METHOD, '499',  ET,     FIXREF, &
                    ABCORR, '399',  SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '=', XSRFVC, 3, 0.D0, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '=', XEPOCH,    0.D0, OK )


!***********************************************************************
!
!     Normal case: DSK time bounds don't include J2000.
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE( 'Use DSK that doesn''t include J2000 epoch in ' &
      //          'its coverage.' )


      CALL UNLOAD ( DSK0 )
      CALL UNLOAD ( DSK1 )
      CALL UNLOAD ( DSK2 )
      CALL UNLOAD ( DSK3 )
      CALL UNLOAD ( DSK4 )

!
!     Verify that no DSK data are available.
!
      ET     = 1.5D0 * JYEAR()

      METHOD = 'DSK/UNPRIORITIZED/INTERCEPT'
      FIXREF = 'IAU_MARS'
      ABCORR = 'NONE'

      CALL SUBPNT ( METHOD, '499',  ET,     FIXREF, &
                    ABCORR, '399',  SPOINT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )

!
!     Make a call that requires data available at a TDB time not equal
!     to J2000.
!
      CALL FURNSH ( DSK4 )

      CALL SUBPNT ( METHOD, '499',  ET,     FIXREF, &
                    ABCORR, '399',  SPOINT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Unload DSK4 so as to not interfere with later tests.
!
      CALL UNLOAD ( DSK4 )

!
!     Restore previously loaded DSKs.
!
      CALL FURNSH( DSK0 )
      CALL FURNSH( DSK1 )
      CALL FURNSH( DSK2 )
      CALL FURNSH( DSK3 )

!***********************************************************************
!
!     Normal case: state change detection
!
!***********************************************************************

!
!     Certain subsystem state changes must be detected and responded to
!     by SUBPNT. The subsystems (or structures) having states that must
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



      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, XSUBPT, XEPOCH, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODDEF ( 'JUPITER', 499 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )


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


      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )

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



      METHOD = 'intercept/dsk/unprioritized/surfaces = 1'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, XSUBPT, XEPOCH, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )


      SRFNMS(1) = 'AAAbbb'

      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )



      METHOD = 'intercept/dsk/unprioritized/surfaces = AAAbbb'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )

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



      METHOD = 'nadir/dsk/unprioritized/surfaces = low-res'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, XSUBPT, XEPOCH, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Unload the high-res DSK; set METHOD to remove
!     surface specification.
!
      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'nadir/dsk/unprioritized'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )


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

      METHOD = 'nadir/dsk/unprioritized'

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, SPOINT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure the result matches that obtained with the
!     high-res DSK specified.
!
      METHOD = 'nadir/dsk/unprioritized/ SURFACES = "HIGH-RES" '

      CALL SUBPNT ( METHOD, TARGET, ET,     FIXREF, &
                    ABCORR, OBSRVR, XSUBPT, XEPOCH, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     We expect an exact match here.
!
      CALL CHCKAD ( 'SPOINT', SPOINT, '=', XSUBPT, 3, 0.D0, OK )



!***********************************************************************
!
!     Error handling tests follow.
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid method.' )

      CALL SUBPNT ( 'INTERCPT/ ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


      CALL SUBPNT ( 'INTERCEPT: ELLIPSOIDD', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )

      CALL SUBPNT ( 'ellipsoid / INTERCEPT /', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


      CALL SUBPNT ( 'ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSUBTYPE)', OK )


      CALL SUBPNT ( 'INTERCEPT ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid target name.' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'ERTH', ET,  'IAU_EARTH', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid observer name.' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'SN', &
                    SPOINT,  TRGEPC, SRFVEC             )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer is target.' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'NONE', 'earth', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid reference frame center' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'EARTH', ET,  'IAU_MOON', 'NONE', 'SUN', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reference frame not found' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'EARTH', ET,  'IAU_EART', 'NONE', 'sun', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid aberration correction' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'LTT', 'sun', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
!     Test SAVE logic by repeating the call.
!
      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'LTT', 'sun', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Relativistic aberration correction' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'RL', 'sun', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Stellar aberration correction w/o light time' )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'S', 'sun', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No loaded SPK files' )

      CALL SPKUEF ( HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'LT+S', 'sun', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDFILES)', OK )


      CALL SPKLEF ( SPK1, HANDLE(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKLEF ( SPK2, HANDLE(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No ephemeris data for observer' )


      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'EARTH', ET,  'IAU_EARTH', 'LT+S', '1000', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No ephemeris data for target' )


      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'gaspra', ET,  'IAU_GASPRA', 'LT+S', '10', &
                    SPOINT,  TRGEPC, SRFVEC             )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No orientation data for target' )

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'earth', ET,  'IAU_EARTH', 'LT+S', '10', &
                    SPOINT,  TRGEPC, SRFVEC             )
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

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'earth', ET,  'IAU_EARTH', 'LT+S', '10', &
                    SPOINT,  TRGEPC, SRFVEC             )
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

      CALL SUBPNT ( 'INTERCEPT: ELLIPSOID', &
                    'earth', ET,  'IAU_EARTH', 'LT+S', '10', &
                    SPOINT,  TRGEPC, SRFVEC             )
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


      METHOD = 'nadir/dsk/unprioritized'

      CALL SUBPNT ( METHOD, '499',  ET,     FIXREF, &
                    ABCORR, '399',  SPOINT, TRGEPC, SRFVEC )
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

      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK4      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
