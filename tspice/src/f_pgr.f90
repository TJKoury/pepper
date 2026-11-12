!$Procedure      F_PGR ( Planetographic coordinate tests )

      SUBROUTINE F_PGR ( OK )
      use pepper_legacy

! $ Abstract
!
!     This routine tests the SPICELIB planetographic coordinate
!     conversion and Jacobian routines.
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
!     This routine tests the SPICELIB planetographic coordinate
!     and coordinate derivative conversion routines
!
!        DPGRDR
!        DRDPGR
!        PGRREC
!        RECPGR
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
!-    TSPICE Version 1.1.0, 16-MAY-2014 (NJB)
!
!        Bug fix: this routine no longer modifies XLON.
!        Added full header.
!
!-    TSPICE Version 1.0.0, 05-JAN-2005 (NJB)
!
!-&

!
!     SPICELIB functions
!


!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK = 'f_recpgr.tpc' )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-10 )


      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      INTEGER               NBODS
      PARAMETER           ( NBODS  = 75 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               MAXCOF
      PARAMETER           ( MAXCOF = 3 )

      INTEGER               NCASE
      PARAMETER           ( NCASE  = 18 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NSPEC
      PARAMETER           ( NSPEC  = 3 )

!
!     Local variables
!
      CHARACTER*(NAMLEN)    BODY   ( NCASE )
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(NAMLEN)    SPCIAL ( NSPEC )

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      COEFFS ( MAXCOF )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      GALT
      DOUBLE PRECISION      GLAT
      DOUBLE PRECISION      GLON
      DOUBLE PRECISION      JAC2   ( 3, 3 )
      DOUBLE PRECISION      JACOBI ( 3, 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      REC    ( 3 )
      DOUBLE PRECISION      RECTAN ( 3, NCASE )
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      XALT   ( NCASE )
      DOUBLE PRECISION      XLAT   ( NCASE )
      DOUBLE PRECISION      XLON   ( NCASE )
      DOUBLE PRECISION      XREC   ( 3 )
      DOUBLE PRECISION      XTEMP

      INTEGER               I
      INTEGER               J
      INTEGER               N

!
!     Saved variables
!
      SAVE                  BODY
      SAVE                  RECTAN
      SAVE                  SPCIAL
      SAVE                  XALT
      SAVE                  XLAT
      SAVE                  XLON

!
!     Initial values
!
      DATA                ( BODY(I), I = 1, 9 )    / &
                                      'EARTH', &
                                      'EARTH', &
                                      'EARTH', &
                                      'EARTH', &
                                      'EARTH', &
                                      'EARTH', &
                                      'EARTH', &
                                      'EARTH', &
                                      'EARTH' /

      DATA                ( BODY(I), I = 10, NCASE )    / &
                                      'MARS', &
                                      'MARS', &
                                      'MARS', &
                                      'MARS', &
                                      'MARS', &
                                      'MARS', &
                                      'MARS', &
                                      'MARS', &
                                      'MARS' /

      DATA                ( ( RECTAN(J,I), J = 1, 3 ), I = 1, 9 )  / &
                                  6378.140D0,     0.000D0,     0.000D0, &
                                 -6378.140D0,     0.000D0,     0.000D0, &
                                 -6388.140D0,     0.000D0,     0.000D0, &
                                 -6368.140D0,     0.000D0,     0.000D0, &
                                     0.000D0, -6378.140D0,     0.000D0, &
                                     0.000D0,  6378.140D0,     0.000D0, &
                                     0.000D0,     0.000D0,  6356.750D0, &
                                     0.000D0,     0.000D0, -6356.750D0, &
                                     0.000D0,     0.000D0,     0.000D0 &
                                    /

      DATA                ( ( RECTAN(J,I), J = 1, 3 ), I = 10, NCASE ) / &
                                  3397.000D0,     0.000D0,     0.000D0, &
                                 -3397.000D0,     0.000D0,     0.000D0, &
                                 -3407.000D0,     0.000D0,     0.000D0, &
                                 -3387.000D0,     0.000D0,     0.000D0, &
                                     0.000D0, -3397.000D0,     0.000D0, &
                                     0.000D0,  3397.000D0,     0.000D0, &
                                     0.000D0,     0.000D0,  3375.000D0, &
                                     0.000D0,     0.000D0, -3375.000D0, &
                                     0.000D0,     0.000D0,     0.000D0 &
                                    /

      DATA  ( XLON(I), XLAT(I), XALT(I), I = 1, 9 ) / &
                                     0.000D0,     0.000D0,      0.000D0, &
                                   180.000D0,     0.000D0,      0.000D0, &
                                   180.000D0,     0.000D0,     10.000D0, &
                                   180.000D0,     0.000D0,    -10.000D0, &
                                   270.000D0,     0.000D0,      0.000D0, &
                                    90.000D0,     0.000D0,      0.000D0, &
                                     0.000D0,    90.000D0,      0.000D0, &
                                     0.000D0,   -90.000D0,      0.000D0, &
                                     0.000D0,    90.000D0,  -6356.750D0 &
                                                        /

      DATA  ( XLON(I), XLAT(I), XALT(I), I = 10, NCASE ) / &
                                     0.000D0,     0.000D0,      0.000D0, &
                                   180.000D0,     0.000D0,      0.000D0, &
                                   180.000D0,     0.000D0,     10.000D0, &
                                   180.000D0,     0.000D0,    -10.000D0, &
                                    90.000D0,     0.000D0,      0.000D0, &
                                   270.000D0,     0.000D0,      0.000D0, &
                                     0.000D0,    90.000D0,      0.000D0, &
                                     0.000D0,   -90.000D0,      0.000D0, &
                                     0.000D0,    90.000D0,  -3375.000D0 &
                                                        /


      DATA  SPCIAL / 'EARTH', 'MOON', 'SUN' /


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_PGR' )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup:  create full text PCK file.' )
!
!     Create, load, and delete a PCK kernel.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Loop through the canned cases.
!
      DO I = 1, NCASE

         TITLE = 'Canned case #; BODY = #.'
         CALL REPMI ( TITLE, '#', I,       TITLE )
         CALL REPMC ( TITLE, '#', BODY(I), TITLE )

!
! --- Case: ------------------------------------------------------
!
         CALL TCASE ( TITLE )

         CALL BODVRD ( BODY(I), 'RADII', 3, N, RADII )

         RE = RADII(1)
         RP = RADII(3)

         F  =  ( RE - RP ) / RE

!
!        Test RECPGR.
!
         CALL RECPGR ( BODY(I), RECTAN(1,I), RE, F,  LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         LON = DPR() * LON
         LAT = DPR() * LAT

         CALL CHCKSD ( 'LON',  LON, '~', XLON(I), VTIGHT, OK )
         CALL CHCKSD ( 'LAT',  LAT, '~', XLAT(I), VTIGHT, OK )
         CALL CHCKSD ( 'ALT',  ALT, '~', XALT(I),  TIGHT, OK )

!
!        Test PGRREC.
!
         CALL PGRREC ( BODY(I), RPD()*XLON(I), RPD()*XLAT(I), XALT(I), &
                       RE,      F,             REC                     )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Test the vector relative error if the test point is not
!        at the origin.
!
         IF (  .NOT.  VZERO( RECTAN(1,I) )  ) THEN

            CALL CHCKAD( 'REC', REC, '~~/', RECTAN(1,I), 3, VTIGHT, OK )

         ELSE
!
!           Check component-wise differences.
!
            CALL CHCKAD( 'REC', REC, '~', RECTAN(1,I), 3, VTIGHT, OK )

         END IF

      END DO

!
!     Run a non-axis test case.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Test non-axis case against RECGEO for oblate ' // &
                   'body.' )

      CALL VPACK ( 1.D5, 1.D5, 1.D5, XREC )


      CALL BODVRD ( 'Mars', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RE = RADII(1)
      RP = RADII(3)

      F = ( RE - RP ) / RE

!
!     Test RECPGR.
!
      CALL RECPGR ( 'MARS', XREC, RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECGEO ( XREC, RE, F, GLON, GLAT, GALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Mars is prograde, so flip the sign of the longitude.
!
      GLON = - GLON

      IF ( GLON .LT. 0.D0 ) THEN
         GLON = GLON + TWOPI()
      END IF

      CALL CHCKSD ( 'LON',  LON, '~', GLON, VTIGHT, OK )
      CALL CHCKSD ( 'LAT',  LAT, '~', GLAT, VTIGHT, OK )
      CALL CHCKSD ( 'ALT',  ALT, '~', GALT,  TIGHT, OK )

!
!     Now test PGRREC.
!
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( 'REC', REC, '~~/', XREC, 3, VTIGHT, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Repeat previous test with Mars longitude ' // &
                   'set to positive east.'                     )


      CALL PCPOOL ( 'BODY499_PGR_POSITIVE_LON', 1, 'EAST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test RECPGR.
!
      CALL RECPGR ( 'MARS', XREC, RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECGEO ( XREC, RE, F, GLON, GLAT, GALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      IF ( GLON .LT. 0.D0 ) THEN
         GLON = GLON + TWOPI()
      END IF

      CALL CHCKSD ( 'LON',  LON, '~', GLON, VTIGHT, OK )
      CALL CHCKSD ( 'LAT',  LAT, '~', GLAT, VTIGHT, OK )
      CALL CHCKSD ( 'ALT',  ALT, '~', GALT,  TIGHT, OK )

!
!     Now test PGRREC.
!
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( 'REC', REC, '~~/', XREC, 3, VTIGHT, OK )

!
!     Clear the override value from the pool.
!
      CALL DVPOOL ( 'BODY499_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Repeat previous test with Mars PM values ' // &
                   'absent from the kernel pool.'             )

!
!     Capture the correct values before starting.
!
      CALL BODVRD ( 'Mars', 'PM', 3, N, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY499_PM' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCPOOL ( 'BODY499_PGR_POSITIVE_LON', 1, 'EAST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test RECPGR.
!
      CALL RECPGR ( 'MARS', XREC, RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECGEO ( XREC, RE, F, GLON, GLAT, GALT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      IF ( GLON .LT. 0.D0 ) THEN
         GLON = GLON + TWOPI()
      END IF

      CALL CHCKSD ( 'LON',  LON, '~', GLON, VTIGHT, OK )
      CALL CHCKSD ( 'LAT',  LAT, '~', GLAT, VTIGHT, OK )
      CALL CHCKSD ( 'ALT',  ALT, '~', GALT,  TIGHT, OK )

!
!     Now test PGRREC.
!
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKAD( 'REC', REC, '~~/', XREC, 3, VTIGHT, OK )

!
!     Clear the override value from the pool.
!
      CALL DVPOOL ( 'BODY499_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore the Mars PM coefficients.
!
      CALL PDPOOL ( 'BODY499_PM', 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check longitude sense for earth, moon, and sun.' )


      DO I = 1, NSPEC
!
!        Test RECPGR.
!
         CALL RECPGR ( SPCIAL(I), XREC, RE, F, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECGEO ( XREC, RE, F, GLON, GLAT, GALT )
         CALL CHCKXC ( .FALSE., ' ', OK )


         IF ( GLON .LT. 0.D0 ) THEN
            GLON = GLON + TWOPI()
         END IF

         CALL CHCKSD ( 'LON',  LON, '~', GLON, VTIGHT, OK )
         CALL CHCKSD ( 'LAT',  LAT, '~', GLAT, VTIGHT, OK )
         CALL CHCKSD ( 'ALT',  ALT, '~', GALT,  TIGHT, OK )

!
!        Now test PGRREC.
!
         CALL PGRREC ( SPCIAL(I), LON, LAT, ALT, RE, F, REC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( 'REC', REC, '~~/', XREC, 3, VTIGHT, OK )

      END DO



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Repeat tests with longitudes set to positive ' // &
                   'west.' )

      CALL PCPOOL ( 'BODY399_PGR_POSITIVE_LON', 1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCPOOL ( 'BODY301_PGR_POSITIVE_LON', 1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCPOOL ( 'BODY10_PGR_POSITIVE_LON',  1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NSPEC
!
!        Test RECPGR.
!
         CALL RECPGR ( SPCIAL(I), XREC, RE, F, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL RECGEO ( XREC, RE, F, GLON, GLAT, GALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         GLON = -GLON

         IF ( GLON .LT. 0.D0 ) THEN
            GLON = GLON + TWOPI()
         END IF

         CALL CHCKSD ( 'LON',  LON, '~', GLON, VTIGHT, OK )
         CALL CHCKSD ( 'LAT',  LAT, '~', GLAT, VTIGHT, OK )
         CALL CHCKSD ( 'ALT',  ALT, '~', GALT,  TIGHT, OK )

!
!        Now test PGRREC.
!
         CALL PGRREC ( SPCIAL(I), LON, LAT, ALT, RE, F, REC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD( 'REC', REC, '~~/', XREC, 3, VTIGHT, OK )

      END DO


      CALL DVPOOL ( 'BODY399_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY301_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY10_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Error cases.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: equatorial radius is zero.' )

!
!     Capture the correct values before starting.
!
      CALL BODVRD ( 'Earth', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RE = RADII(1)
      RP = RADII(3)

      F = ( RE - RP ) / RE


      CALL RECPGR ( 'EARTH', RECTAN(1,1), 0.D0, F, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'EARTH', LON, LAT, ALT, 0.D0, F, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: flattening factor is 1.'   )

      CALL RECPGR ( 'EARTH', RECTAN(1,1), RE, 1.D0, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'EARTH', LON, LAT, ALT, RE, 1.D0, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: flattening factor is > 1.'   )

      CALL RECPGR ( 'EARTH', RECTAN(1,1), RE, 2.D0, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'EARTH', LON, LAT, ALT, RE, 2.D0, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: no Mars PM terms in pool.'  )
!
!     Capture the correct values before starting.
!
      CALL BODVRD ( 'Mars', 'PM', 3, N, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Delete Mars' PM coefficients from the kernel pool.
!
      CALL DVPOOL ( 'BODY499_PM' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECPGR ( 'MARS', RECTAN(1,1), RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: no rate term for Mars PM.'  )
!
!     Overwrite Mars' PM coefficients in the kernel pool.
!
      CALL PDPOOL ( 'BODY499_PM', 1, [0.D0] )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECPGR ( 'MARS', RECTAN(1,1), RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

!
!     Restore the good PM coefficients.
!
      CALL PDPOOL ( 'BODY499_PM', 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: invalid override spec.'  )


      CALL PCPOOL ( 'BODY499_PGR_POSITIVE_LON', 1, 'EST' )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL RECPGR ( 'MARS', RECTAN(1,1), RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL PGRREC ( 'MARS', LON, LAT, ALT, RE, F, RECTAN(1,1) )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
!     Clean up.
!
      CALL DVPOOL ('BODY499_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )




! *****************************************************************
! *****************************************************************
! *****************************************************************
!     Jacobian matrix routine tests
! *****************************************************************
! *****************************************************************
! *****************************************************************



!
!     Loop through the canned cases.
!
      DO I = 1, NCASE

         TITLE = 'Canned DPGRDR case #; BODY = #.'
         CALL REPMI ( TITLE, '#', I,       TITLE )
         CALL REPMC ( TITLE, '#', BODY(I), TITLE )

!
! --- Case: ------------------------------------------------------
!
         CALL TCASE ( TITLE )

         CALL BODVRD ( BODY(I), 'RADII', 3, N, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )

         RE = RADII(1)
         RP = RADII(3)

         F  =  ( RE - RP ) / RE

         CALL DRDPGR ( BODY(I), &
                       XLON(I), XLAT(I), XALT(I), &
                       RE,      F,       JACOBI       )


         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( EQSTR( BODY(I), 'Mars' )  ) THEN
            XTEMP = -XLON(I)
         ELSE
            XTEMP =  XLON(I)
         END IF

         CALL DRDGEO ( XTEMP,   XLAT(I), XALT(I), &
                       RE,      F,       JAC2      )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        For comparison, negate the partials w.r.t. geodetic longitude
!        for Mars.
!
         IF ( EQSTR( BODY(I), 'Mars' )  ) THEN

            DO J = 1, 3
               JAC2(J,1) =  -JAC2(J,1)
            END DO

         END IF

         CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )

      END DO


!
!     Loop through the canned cases for DRDPGR.
!
      DO I = 1, NCASE

         TITLE = 'Canned DRDPGR case #; BODY = #.'
         CALL REPMI ( TITLE, '#', I,       TITLE )
         CALL REPMC ( TITLE, '#', BODY(I), TITLE )

!
! --- Case: ------------------------------------------------------
!
         CALL TCASE ( TITLE )

         CALL BODVRD ( BODY(I), 'RADII', 3, N, RADII )
         CALL CHCKXC ( .FALSE., ' ', OK )

         RE = RADII(1)
         RP = RADII(3)

         F  =  ( RE - RP ) / RE

         CALL DPGRDR ( BODY(I), &
                       RECTAN(1,I), RECTAN(2,I), RECTAN(3,I), &
                       RE,          F,           JACOBI       )

         IF (   VZEROG ( RECTAN(1,I), 2 )  ) THEN

            CALL CHCKXC ( .TRUE., 'SPICE(POINTONZAXIS)', OK )

         ELSE

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL DGEODR ( RECTAN(1,I), RECTAN(2,I), RECTAN(3,I), &
                          RE,          F,           JAC2         )
            CALL CHCKXC ( .FALSE., ' ', OK )

!
!           For comparison, negate the gradient of geodetic longitude
!           for Mars.

            IF ( EQSTR( BODY(I), 'Mars' )  ) THEN

               DO J = 1, 3
                  JAC2(1,J) =  -JAC2(1,J)
               END DO

            END IF

            CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )

         END IF

      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check longitude sense for earth, moon, and sun.' )


      CALL VPACK ( 1.D5, 1.D5, 1.D5, XREC )

      DO I = 1, NSPEC
!
!        Test DPGRDR.
!
         CALL DPGRDR ( SPCIAL(I), XREC(1), XREC(2), XREC(3), &
                       RE,        F,       JACOBI           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DGEODR (            XREC(1), XREC(2), XREC(3), &
                       RE,        F,       JAC2             )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )
!
!        Now test DRDPGR.
!
         CALL DRDPGR ( SPCIAL(I), LON, LAT, ALT, RE, F, JACOBI )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DRDGEO (            LON, LAT, ALT, RE, F, JAC2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )


      END DO


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Repeat tests with longitudes set to positive ' // &
                   'west.' )

      CALL PCPOOL ( 'BODY399_PGR_POSITIVE_LON', 1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCPOOL ( 'BODY301_PGR_POSITIVE_LON', 1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PCPOOL ( 'BODY10_PGR_POSITIVE_LON',  1, 'WEST' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      DO I = 1, NSPEC
!
!        Test DPGRDR.
!
         CALL DPGRDR ( SPCIAL(I), XREC(1), XREC(2), XREC(3), &
                       RE,        F,       JACOBI           )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DGEODR (            XREC(1), XREC(2), XREC(3), &
                       RE,        F,       JAC2             )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Negate the gradient of geodetic longitude.
!
         DO J = 1, 3
            JAC2(1,J) = -JAC2(1,J)
         END DO

         CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )

!
!        Now test DRDPGR.
!
         CALL DRDPGR ( SPCIAL(I), LON, LAT, ALT, RE, F, JACOBI )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL DRDGEO (            LON, LAT, ALT, RE, F, JAC2 )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Negate all partials with respect to geodetic longitude.
!
         DO J = 1, 3
            JAC2(J,1) = -JAC2(J,1)
         END DO

         CALL CHCKAD ( 'JACOBI', JACOBI, '~', JAC2, 9, VTIGHT, OK )

      END DO


!
!     Error cases.
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: equatorial radius is zero.' )

!
!     Capture the correct values before starting.
!
      CALL BODVRD ( 'Earth', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RE = RADII(1)
      RP = RADII(3)

      F = ( RE - RP ) / RE


      CALL DPGRDR ( 'EARTH', &
                     RECTAN(1,1), RECTAN(2,1), RECTAN(3,1), &
                     0.D0,        F,           JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'EARTH', LON, LAT, ALT, 0.D0, F, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: flattening factor is 1.'   )

      CALL DPGRDR ( 'EARTH', &
                     RECTAN(1,1), RECTAN(2,1), RECTAN(3,1), &
                     RE,          1.D0,        JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'EARTH', LON, LAT, ALT, RE, 1.D0, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: flattening factor is > 1.'   )

      CALL DPGRDR ( 'EARTH', &
                     RECTAN(1,1), RECTAN(2,1), RECTAN(3,1), &
                     RE,        2.D0,        JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'EARTH', LON, LAT, ALT, RE, 2.D0, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: no Mars PM terms in pool.'  )
!
!     Capture the correct values before starting.
!
      CALL BODVRD ( 'Mars', 'PM', 3, N, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Delete Mars' PM coefficients from the kernel pool.
!
      CALL DVPOOL ( 'BODY499_PM' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DPGRDR ( 'MARS', &
                     RECTAN(1,1), RECTAN(2,1), RECTAN(3,1), &
                     RE,          F,           JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'MARS', LON, LAT, ALT, RE, F, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: no rate term for Mars PM.'  )
!
!     Overwrite Mars' PM coefficients in the kernel pool.
!
      CALL PDPOOL ( 'BODY499_PM', 1, [0.D0] )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DPGRDR ( 'MARS', &
                     RECTAN(1,1), RECTAN(2,1), RECTAN(3,1), &
                     RE,          F,           JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'MARS', LON, LAT, ALT, RE, F, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(MISSINGDATA)', OK )

!
!     Restore the good PM coefficients.
!
      CALL PDPOOL ( 'BODY499_PM', 3, COEFFS )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error case: invalid override spec.'  )


      CALL PCPOOL ( 'BODY499_PGR_POSITIVE_LON', 1, 'EST' )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL DPGRDR ( 'MARS', &
                     RECTAN(1,1), RECTAN(2,1), RECTAN(3,1), &
                     RE,          F,           JACOBI       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

      LON = 0.D0
      LAT = 0.D0
      ALT = 1.D5
      CALL DRDPGR ( 'MARS', LON, LAT, ALT, RE, F, JACOBI )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )



!
!     Clean up.
!
      CALL DVPOOL ('BODY499_PGR_POSITIVE_LON' )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL T_SUCCESS ( OK )
      RETURN
      END
