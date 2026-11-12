!$Procedure F_GFSNTC ( GFSNTC family tests )

      SUBROUTINE F_GFSNTC ( OK )
      use pepper_legacy
      use pepper_inc_gf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        GFSNTC
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
!     geometry routine GFSNTC.
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
!     E.D. Wright     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.2.0, 28-JAN-2017 (EDW)
!
!        Modified GFSTOL test case to correspond to change
!        in ZZGFSOLVX.
!
!-    TSPICE Version 1.1.0, 27-NOV-2012 (EDW)
!
!        Added test on GFSTOL use.
!
!-    TSPICE Version 1.0.0, 12-FEB-2009 (EDW)
!
!-&

!
!     SPICELIB functions
!



!
!     Local parameters
!
      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'gfsntc.bsp' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'gfsntc.pck' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.pck' )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 20000 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 9 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(LNSIZE)    CORR   ( NCORR )

      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    CRDSYS
      CHARACTER*(BDNMLN)    COORD
      CHARACTER*(BDNMLN)    RELATE
      CHARACTER*(BDNMLN)    METHOD
      CHARACTER*(BDNMLN)    FIXREF
      CHARACTER*(BDNMLN)    DREF
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(LNSIZE)    TIME0
      CHARACTER*(LNSIZE)    TIME1


      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      END
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      REFVAL
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      ADJUST
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      WORK   ( LBCELL : MAXWIN, NWMAX )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      Y      ( 3 )
      DOUBLE PRECISION      Z      ( 3 )
      DOUBLE PRECISION      RAD    ( 3 )
      DOUBLE PRECISION      TIMBEG (2)
      DOUBLE PRECISION      TIMEND (2)

      INTEGER               COUNT
      INTEGER               HANDLE
      INTEGER               HAN1
      INTEGER               I
      INTEGER               J
      INTEGER               DIM

      INTEGER               NDESC
      PARAMETER           ( NDESC = 22 )

      CHARACTER*(LNSIZE)    MDESC  ( NDESC )
      DOUBLE PRECISION      MREFS  ( NDESC )
      CHARACTER*(BDNMLN)    ITEMS  ( 3 )


!
!     Saved all.
!
      SAVE


!
!     Initial values
!
      DATA                  CORR / 'NONE', &
                                   'lt',  ' lt+s',  ' cn',  ' cn + s', &
                                   'XLT', 'XLT + S', 'XCN', 'XCN+S'   /

      DATA                  X      / 1.D0, 0.D0, 0.D0 /
      DATA                  Y      / 0.D0, 1.D0, 0.D0 /
      DATA                  Z      / 0.D0, 0.D0, 1.D0 /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFSNTC' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create a PCK, load using FURNSH.
!
      CALL T_PCK08 ( PCK, .FALSE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH( PCK )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL NATPCK ( PCK1,  .FALSE., .TRUE.  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( PCK1 )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create an SPK, load using FURNSH.
!
      CALL TSTSPK( SPK, .TRUE., HANDLE )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL FURNSH( SPK )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL NATSPK ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!     Create a confinement window from ET0 and ET1.
!
      TIME0 = '2000 JAN 1  00:00:00 TDB'
      TIME1 = '2000 JAN 2  00:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!    Error cases
!

!
!   Case 1
!
      CALL TCASE ( 'Non-positive step size' )

      TARGET = 'EARTH'
      FIXREF = 'IAU_EARTH'
      METHOD = 'Ellipsoid'
      ABCORR = CORR(1)
      DREF   = 'IAU_EARTH'
      OBSRVR = 'SUN'
      CRDSYS = 'LATITUDINAL'
      COORD  = 'LATITUDE'
      RELATE = '='
      REFVAL = 0.D0
      ADJUST = 0.D0
      STEP   = 0.D0

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


!
!   Case 2
!
      CALL TCASE ( 'Non unique body IDs.' )

      TARGET = 'EARTH'
      OBSRVR = 'EARTH'
      STEP   = 1.D0

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


!
!   Case 3
!
      CALL TCASE ( 'Invalid aberration correction specifier' )

      TARGET = 'EARTH'
      ABCORR = 'X'
      OBSRVR = 'SUN'

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


!
!   Case 4
!
      CALL TCASE ( 'Invalid relations operator' )

      ABCORR = CORR(1)
      RELATE = '=='

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTRECOGNIZED)',   OK )

!
!   Case 5
!
      CALL TCASE ( 'Invalid body names' )

      RELATE = '='
      TARGET = 'X'
      OBSRVR = 'SUN'

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      TARGET = 'EARTH'
      OBSRVR = 'X'

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


!
!     Case 6
!
      CALL TCASE ( 'Negative adjustment value' )

      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      ADJUST = -1.0D0

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)',   OK )


!
!     Case 7
!
      CALL TCASE ( 'Ephemeris data unavailable' )

      ADJUST = 0.0D0
      TARGET = 'EARTH'
      OBSRVR = 'DAWN'

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


!
!     Case 8
!
      CALL TCASE ( 'Unknown coordinate system' )

      ADJUST = 0.0D0
      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      CRDSYS = 'X'

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


!
!     Case 9
!
      CALL TCASE ( 'Unknown coordinate.' )

      ADJUST = 0.0D0
      TARGET = 'EARTH'
      CRDSYS = 'LATITUDINAL'
      COORD  = 'X'

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


!
!     Case 10
!

      DO I= 1, 2

         IF ( I .EQ. 1 ) THEN
            CRDSYS = 'PLANETOGRAPHIC'
         ELSE
            CRDSYS = 'GEODETIC'
         END IF

         TITLE = 'Unknown FIXREF frame for #'
         CALL REPMC( TITLE, '#', CRDSYS, TITLE )
         CALL TCASE ( TITLE )

         COORD  = 'ALTITUDE'
         FIXREF = 'X'

         CALL GFSNTC (  TARGET,  FIXREF, &
                        METHOD,  ABCORR, OBSRVR, &
                        DREF,    X, &
                        CRDSYS,  COORD, &
                        RELATE,  REFVAL, &
                        ADJUST,  STEP, CNFINE, &
                        MAXWIN, NWMAX, WORK, RESULT )
         CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )

      END DO


!
!     Case 11
!
      CALL TCASE ( 'Invalid target frame' )

      TARGET = 'EARTH'
      OBSRVR = 'SUN'
      CRDSYS = 'LATITUDINAL'
      COORD  = 'LATITUDE'
      FIXREF = 'IAU_MARS'

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN, NWMAX, WORK, RESULT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )



!
!     Define the coordinate test conditions.
!
      CALL BODVRD ( 'ALPHA', 'RADII', 3, DIM, RAD )

!
!     Conditions: all conditions should occur once per
!     GAMMA orbit (delta_t = 24 hours).
!
      MDESC(1)  = 'RECTANGULAR   : X           : <'
      MDESC(2)  = 'RECTANGULAR   : Y           : <'
      MDESC(3)  = 'RECTANGULAR   : Z           : <'
      MDESC(4)  = 'RECTANGULAR   : X           : >'
      MDESC(5)  = 'RECTANGULAR   : Y           : >'
      MDESC(6)  = 'RECTANGULAR   : Z           : >'
      MDESC(7)  = 'LATITUDINAL   : RADIUS      : >'
      MDESC(8)  = 'LATITUDINAL   : LATITUDE    : >'
      MDESC(9)  = 'LATITUDINAL   : LATITUDE    : <'
      MDESC(10) = 'RA/DEC        : RANGE       : >'
      MDESC(11) = 'RA/DEC        : DECLINATION : >'
      MDESC(12) = 'RA/DEC        : DECLINATION : <'
      MDESC(13) = 'SPHERICAL     : RADIUS      : >'
      MDESC(14) = 'SPHERICAL     : COLATITUDE  : >'
      MDESC(15) = 'SPHERICAL     : COLATITUDE  : <'
      MDESC(16) = 'CYLINDRICAL   : RADIUS      : >'
      MDESC(17) = 'CYLINDRICAL   : Z           : <'
      MDESC(18) = 'CYLINDRICAL   : Z           : >'
      MDESC(19) = 'CYLINDRICAL   : LONGITUDE   : >'
      MDESC(20) = 'SPHERICAL     : LONGITUDE   : <'
      MDESC(21) = 'LATITUDINAL   : LONGITUDE   : <'
      MDESC(22) = 'RA/DEC        : RIGHT ASCENSION : >'



!
!     Test conditions reference values:
!
      MREFS(1)  =  RAD(1)
      MREFS(2)  =  RAD(2)
      MREFS(3)  =  RAD(3)
      MREFS(4)  = -RAD(1)
      MREFS(5)  = -RAD(2)
      MREFS(6)  = -RAD(3)
      MREFS(7)  =  0.D0
      MREFS(8)  = -90.D0*RPD()
      MREFS(9)  =  90.D0*RPD()
      MREFS(10) =  0.D0
      MREFS(11) = -90.D0*RPD()
      MREFS(12) =  90.D0*RPD()
      MREFS(13) =  0.D0
      MREFS(14) =  0.D0
      MREFS(15) =  180.D0*RPD()
      MREFS(16) =  0.D0
      MREFS(17) =  RAD(3)
      MREFS(18) = -RAD(3)
      MREFS(19) =  0.D0
      MREFS(20) =  0.D0
      MREFS(21) =  0.D0
      MREFS(22) =  0.D0

!
!     Time interval thirty days.
!
      TIME0 = '2000 JAN 1   18:00:00 TDB'
      TIME1 = '2000 JAN 31  18:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ADJUST = 0.D0
      TARGET = 'ALPHA'
      FIXREF = 'ALPHAFIXED'
      METHOD = 'ELLIPSOID'
      OBSRVR = 'GAMMA'
      DREF   = 'J2000'
      ABCORR = 'NONE'


!
!     Case 12, Sweeps of the GAMMA centered J2000 Z axis on ALPHA.
!

      STEP   =  SPD()*(1.25D0/24.D0)

      DO I=1, NDESC

         CALL SCARDD ( 0, CNFINE )

         CALL WNINSD ( ET0, ET1, CNFINE )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         TITLE = 'MDESC(#) ALPHA sweeps from GAMMA, Z, '//MDESC(I)
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

!
!        Parse from the MDESC(I) string the coordinate system,
!        coordinate, and relation operator.
!
         CALL LPARSE ( MDESC(I), ':', 3, DIM, ITEMS )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CRDSYS = ITEMS(1)
         COORD  = ITEMS(2)
         RELATE = ITEMS(3)
         REFVAL = MREFS(I)

         CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    Z, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN,   NWMAX, WORK, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSI ( 'WNCARD(RESULT)', WNCARD(RESULT), &
                                         '=', 30, 0, OK )

         TIMBEG(1) = 0.D0
         TIMEND(1) = 0.D0
         TIMBEG(2) = 0.D0
         TIMEND(2) = 0.D0

         IF ( OK ) THEN

            CALL WNFETD ( RESULT, 1, TIMBEG(1), TIMEND(1) )

            DO J = 2, WNCARD(RESULT)

               CALL WNFETD ( RESULT, J, TIMBEG(2), TIMEND(2)  )

               CALL CHCKSD ( 'Z SWEEP BEG', TIMBEG(2) - TIMBEG(1), &
                                         '~', &
                                         SPD(), CNVTOL, OK )

               CALL CHCKSD ( 'Z SWEEP END', TIMEND(2) - TIMEND(1), &
                                         '~', &
                                         SPD(), CNVTOL, OK )

               TIMBEG(1) = TIMBEG(2)
               TIMEND(1) = TIMEND(2)

            END DO

         END IF

      END DO




!
!     Case 13,  Sweeps of the GAMMA centered J2000 Y axis on ALPHA.
!
      STEP   =  SPD()*(3.D0/24.D0)

      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I=1, NDESC

         TITLE = 'MDESC(#) ALPHA sweeps from GAMMA, Y, '//MDESC(I)
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

!
!        Parse from the MDESC(I) string the coordinate system,
!        coordinate, and relation operator.
!
         CALL LPARSE ( MDESC(I), ':', 3, DIM, ITEMS )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CRDSYS = ITEMS(1)
         COORD  = ITEMS(2)
         RELATE = ITEMS(3)
         REFVAL = MREFS(I)

         CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    Y, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN,   NWMAX, WORK, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CALL CHCKSI ( 'WNCARD(RESULT)', WNCARD(RESULT), &
                                         '=', 30, 0, OK )

         TIMBEG(1) = 0.D0
         TIMEND(1) = 0.D0
         TIMBEG(2) = 0.D0
         TIMEND(2) = 0.D0

         IF ( OK ) THEN

            CALL WNFETD ( RESULT, 1, TIMBEG(1), TIMEND(1) )

            DO J = 2, WNCARD(RESULT)

               CALL WNFETD ( RESULT, J, TIMBEG(2), TIMEND(2)  )

               CALL CHCKSD ( 'Y SWEEP BEG', TIMBEG(2) - TIMBEG(1), &
                                         '~', &
                                         SPD(), CNVTOL, OK )

               CALL CHCKSD ( 'Y SWEEP END', TIMEND(2) - TIMEND(1), &
                                         '~', &
                                         SPD(), CNVTOL, OK )

               TIMBEG(1) = TIMBEG(2)
               TIMEND(1) = TIMEND(2)

            END DO

         END IF

      END DO




!
!     Case 14, Sweeps of the GAMMA centered J2000 X axis on ALPHA.
!
      STEP   =  SPD()*(1.25D0/24.D0)

      CALL SCARDD ( 0, CNFINE )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      DO I=1, NDESC

         TITLE = 'MDESC(#) ALPHA sweeps from GAMMA, X, '//MDESC(I)
         CALL REPMI ( TITLE, '#', I, TITLE )

         CALL TCASE ( TITLE )

!
!        Parse from the MDESC(I) string the coordinate system,
!        coordinate, and relation operator.
!
         CALL LPARSE ( MDESC(I), ':', 3, DIM, ITEMS )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         CRDSYS = ITEMS(1)
         COORD  = ITEMS(2)
         RELATE = ITEMS(3)
         REFVAL = MREFS(I)

         CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    X, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN,   NWMAX, WORK, RESULT )
         CALL CHCKXC ( .FALSE., ' ',   OK )

!
!        The J2000 X unit with origin at GAMMA's center should never
!        intersect ALPHA.
!
         CALL CHCKSI ( 'WNCARD(RESULT)', WNCARD(RESULT), '=', 0, 0, OK )

      END DO




!
!     Case 15
!
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )

!
!     Re-run a valid search after using GFSTOL.
!
      I = 1

!
!     Parse from the MDESC(I) string the coordinate system,
!     coordinate, and relation operator.
!

      TIME0 = '2000 JAN 1   18:00:00 TDB'
      TIME1 = '2000 JAN 31  18:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( TIME1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      ADJUST = 0.D0
      TARGET = 'ALPHA'
      FIXREF = 'ALPHAFIXED'
      METHOD = 'ELLIPSOID'
      OBSRVR = 'GAMMA'
      DREF   = 'J2000'
      ABCORR = 'NONE'
      STEP   =  SPD()*(1.25D0/24.D0)

      I = 1

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Parse from the MDESC(I) string the coordinate system,
!     coordinate, and relation operator.
!
      CALL LPARSE ( MDESC(I), ':', 3, DIM, ITEMS )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CRDSYS = ITEMS(1)
      COORD  = ITEMS(2)
      RELATE = ITEMS(3)
      REFVAL = MREFS(I)

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    Z, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN,   NWMAX, WORK, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Reset tol.
!

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFSNTC (  TARGET,  FIXREF, &
                     METHOD,  ABCORR, OBSRVR, &
                     DREF,    Z, &
                     CRDSYS,  COORD, &
                     RELATE,  REFVAL, &
                     ADJUST,  STEP, CNFINE, &
                     MAXWIN,   NWMAX, WORK, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      COUNT = 0
      COUNT = WNCARD(RESULT)
      CALL CHCKSI ( 'COUNT', COUNT, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     The values in the time window should not match
!     as the search used different tolerances. Check
!     the first value in the first interval.
!
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )


!
!     Reset the convergence tolerance.
!
      CALL GFSTOL ( CNVTOL )



!
! Case n
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL KCLEAR

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END

!
!23456789012345678901234567890123456789012345678901234567890123456789012
!
