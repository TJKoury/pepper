!$Procedure F_GE01 ( SPICE higher-level geometry routine tests )

      SUBROUTINE F_GE01 ( OK )

! $ Abstract
!
!     Exercise higher-level SPICELIB geometry routines.
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
! $ Files
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
! $ Particulars
!
!     This routine tests a subset of the higher-level SPICELIB
!     geometry routines:
!
!        ILLUM
!        SUBPT
!        SUBSOL
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman     (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    TSPICE Version 1.0.0, 05-OCT-2005 (NJB)
!
!
!-&

!
!     SPICELIB functions
!

!
!     Local Parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'test.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'test.bsp' )

      CHARACTER*(*)         UTC
      PARAMETER           ( UTC    = '1999 JAN 1' )


      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-7 )

      DOUBLE PRECISION      MED
      PARAMETER           ( MED    = 1.D-11 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )




!
!     Local Variables
!
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      EMISSN
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      EXPPNT ( 3 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      RAD
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      SOLAR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SUNALT
      DOUBLE PRECISION      SUNLAT
      DOUBLE PRECISION      SUNLON
      DOUBLE PRECISION      SUNRAD
      DOUBLE PRECISION      SUNSTA ( 6 )

      INTEGER               DIM
      INTEGER               HANDLE




!
!     Initial values
!


!
!     Open the test family.
!
      CALL TOPEN ( 'F_GE01' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  load kernels.' )

!
!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create and load a PCK file. Delete the file afterwards.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Test SUBPT.  Find the sub-solar point of the sun '// &
              'on the Earth using the INTERCEPT definition.'

      CALL TCASE  ( TITLE )

      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPT  ( 'INTERCEPT', 'EARTH', ET, 'NONE', 'SUN', &
                    SPOINT,      ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECLAT ( SPOINT, RAD, LON, LAT )

!
!     Get the state of the sun in Earth bodyfixed coordinates at ET.
!
      CALL SPKGEO ( 10, ET, 'IAU_EARTH', 399, SUNSTA, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECLAT ( SUNSTA, SUNRAD, SUNLON, SUNLAT )

!
!     Make sure the directional coordinates match up.
!
      CALL CHCKSD ( 'Sub point lon', LON, '~', SUNLON, TIGHT, OK )
      CALL CHCKSD ( 'Sub point lat', LAT, '~', SUNLAT, TIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Test SUBPT.  Find the sub-solar point of the sun '// &
              'on the Earth using the NEAR POINT definition.'

      CALL TCASE  ( TITLE )

      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure that strings representing integers are parsed correctly.
!
      CALL SUBPT  ( 'NEAR POINT', '399', ET, 'NONE', '10', &
                    SPOINT,       ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We'll need the radii of the earth.
!
      CALL BODVRD ( 'EARTH', 'RADII', 3, DIM, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RE = RADII(1)
      RP = RADII(3)

      F  = ( RE - RP ) / RE

      CALL RECGEO ( SPOINT, RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Get the state of the sun in Earth bodyfixed coordinates at ET.
!
      CALL SPKGEO ( 10, ET, 'IAU_EARTH', 399, SUNSTA, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECGEO ( SUNSTA, RE, F, SUNLON, SUNLAT, SUNALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Make sure the directional coordinates match up.
!
      CALL CHCKSD ( 'Sub point lon', LON, '~', SUNLON, TIGHT, OK )
      CALL CHCKSD ( 'Sub point lat', LAT, '~', SUNLAT, TIGHT, OK )


!
!     SUBPT error cases:
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid method.' )

      CALL SUBPT  ( 'NARPOINT', '399', ET, 'NONE', '10', &
                    SPOINT,       ALT                         )
      CALL CHCKXC ( .TRUE., 'SPICE(DUBIOUSMETHOD)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid observer name.' )

      CALL SUBPT  ( 'Nearpoint', 'earth', ET, 'NONE', 'sn', &
                    SPOINT,       ALT                         )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid target name.' )

      CALL SUBPT  ( 'Nearpoint', 'erth', ET, 'NONE', 'sun', &
                    SPOINT,       ALT                         )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer is target.' )

      CALL SUBPT  ( 'Nearpoint', 'earth', ET, 'NONE', 'earth', &
                    SPOINT,       ALT                         )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


!
!     ILLUM tests follow.
!
!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Test ILLUM.  Find the illumination angles on the '// &
               'earth as seen from the moon, evaluated at the '     // &
               'sub-moon point (NEARPOINT method).'

      CALL TCASE  ( TITLE )

      CALL SUBPT  ( 'Nearpoint', 'earth', ET, 'NONE', 'moon', &
                    SPOINT,       ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUM ( 'EARTH',  ET,     'NONE',  'MOON', &
                   SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We should have an emission angle of zero.
!
      CALL CHCKSD ( 'Emission angle', EMISSN, '~', 0.D0, TIGHT, OK )

!
!     The phase angle should match the solar incidence angle.
!
      CALL CHCKSD ( 'Phase angle', PHASE, '~', SOLAR, TIGHT, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE  ( 'Repeat tests with integer codes.' )

      CALL SUBPT  ( 'Nearpoint', '399', ET, 'NONE', '301', &
                    SPOINT,       ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUM ( '399',   ET,     'NONE',  '301', &
                   SPOINT,  PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We should have an emission angle of zero.
!
      CALL CHCKSD ( 'Emission angle', EMISSN, '~', 0.D0, TIGHT, OK )

!
!     The phase angle should match the solar incidence angle.
!
      CALL CHCKSD ( 'Phase angle', PHASE, '~', SOLAR, TIGHT, OK )


!
!     Now make the sun the observer:  test the solar incidence
!     angle.
!
!
! --- Case: ------------------------------------------------------
!
      TITLE =  'Test ILLUM.  Find the illumination angles on the ' // &
               'earth as seen from the sun, evaluated at the '     // &
               'sub-sun point (NEARPOINT method).'

      CALL TCASE  ( TITLE )

      CALL SUBPT  ( 'Nearpoint', 'earth', ET, 'NONE', 'sun', &
                    SPOINT,       ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUM ( 'EARTH',  ET,     'NONE',  'sun', &
                   SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     We should have an solar incidence angle of zero.
!
      CALL CHCKSD ( 'Solar inc. angle', SOLAR, '~', 0.D0, TIGHT, OK )

!
!     The phase angle should match the solar incidence angle.
!
      CALL CHCKSD ( 'Phase angle', PHASE, '~', SOLAR, TIGHT, OK )

!
!     ILLUM error cases:
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid observer name.' )

      CALL ILLUM ( 'earth',    ET,     'NONE',  'son', &
                   SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid target name.' )

      CALL ILLUM ( 'erth',    ET,     'NONE',  'sun', &
                   SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer is target.' )

      CALL ILLUM ( 'SUN',    ET,     'NONE',  'sun', &
                   SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No body-fixed frame associated with target.' )

      CALL ILLUM ( 'mars express',    ET,     'NONE',  'sun', &
                   SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


!
!     SUBSOL tests follow.
!
!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Test SUBSOL.  Find the sub-solar point of the sun '// &
              'on the Earth using the NEARPOINT definition.'

      CALL TCASE  ( TITLE )

      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBSOL ( 'NEARPOINT', 'EARTH', ET, 'NONE', 'SUN',  SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPT  ( 'NEARPOINT', 'EARTH', ET, 'NONE', 'SUN', &
                    EXPPNT,      ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECLAT ( SPOINT, RAD, LON, LAT )

!
!     Make sure the surface points match up.
!
      CALL CHCKAD ( 'Geometric sub solar point', &
                    SPOINT, &
                    '~~/', &
                    EXPPNT, &
                    3, &
                    TIGHT, &
                    OK                            )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Test SUBSOL.  Repeat test using integer codes.'

      CALL TCASE  ( TITLE )

      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBSOL ( 'NEARPOINT', '399', ET, 'NONE', '10',  SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPT  ( 'NEARPOINT', 'EARTH', ET, 'NONE', 'SUN', &
                    EXPPNT,      ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECLAT ( SPOINT, RAD, LON, LAT )

!
!     Make sure the surface points match up.
!
      CALL CHCKAD ( 'Geometric sub solar point', &
                    SPOINT, &
                    '~~/', &
                    EXPPNT, &
                    3, &
                    TIGHT, &
                    OK                            )

!
! --- Case: ------------------------------------------------------
!
      TITLE = 'Test SUBSOL.  Find the sub-solar point of the sun '// &
              'on the Earth using the INTERCEPT definition.'

      CALL TCASE  ( TITLE )

      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBSOL ( 'INTERCEPT', 'EARTH', ET, 'NONE', 'SUN',  SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPT  ( 'INTERCEPT', 'EARTH', ET, 'NONE', 'SUN', &
                    EXPPNT,      ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECLAT ( SPOINT, RAD, LON, LAT )

!
!     Make sure the surface points match up.
!
      CALL CHCKAD ( 'Geometric sub solar point', &
                    SPOINT, &
                    '~~/', &
                    EXPPNT, &
                    3, &
                    TIGHT, &
                    OK                           )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid method.' )

      CALL SUBSOL ( 'NARPOINT', '399', ET, 'NONE', '10', SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(DUBIOUSMETHOD)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid observer name.' )

      CALL SUBSOL  ( 'Nearpoint', 'earth', ET, 'NONE', 'sn', SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Invalid target name.' )

      CALL SUBSOL  ( 'Nearpoint', 'erth', ET, 'NONE', 'sun', SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer is target.' )

      CALL SUBSOL  ( 'Nearpoint', 'earth', ET, 'NONE', 'earth', &
                    SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

!
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )



!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


