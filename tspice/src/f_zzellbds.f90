!$Procedure      F_ZZELLBDS ( Test bounding ellipsoid routine )

      SUBROUTINE F_ZZELLBDS ( OK )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Test the SPICELIB bounding ellipsoid routine ZZELLBDS.
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
!     This routine tests the routine ZZELLBDS.
!
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
!-    TSPICE Version 2.0.0, 13-FEB-2017 (NJB)
!
!        Previous version 15-JAN-2016 (NJB)
!
!        Reduced sample counts from 100001 to 1001.
!
!-    TSPICE Version 1.0.0, 27-NOV-2012 (NJB)
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB functions
!

!
!     Local parameters
!
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-15 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    CASNAM

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      AMAX
      DOUBLE PRECISION      AMIN
      DOUBLE PRECISION      B
      DOUBLE PRECISION      BMAX
      DOUBLE PRECISION      BMIN
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      HMAX
      DOUBLE PRECISION      HMIN
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      XAMAX
      DOUBLE PRECISION      XAMIN
      DOUBLE PRECISION      XBMAX
      DOUBLE PRECISION      XBMIN
      DOUBLE PRECISION      XLAT
      DOUBLE PRECISION      XLON
      DOUBLE PRECISION      P     ( 3 )

      INTEGER               I
      INTEGER               N

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZELLBDS' )

!
!     ZZELLBDS error cases:
!

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error case: B is non-positive.' )

      A      =   5.D0
      B      =   0.D0
      HMAX   =   1.D0
      HMIN   =  -1.D0

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVERADIUS)', OK )


      B      = -1.D0

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVERADIUS)', OK )

!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error case: B > A.' )

      A      =   5.D0
      B      =   5.1D0
      HMAX   =   1.D0
      HMIN   =  -1.D0

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .TRUE., 'SPICE(RADIIOUTOFORDER)', OK )


      B      = -1.D0

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .TRUE., 'SPICE(NONPOSITIVERADIUS)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error case: HMIN > HMAX.' )

      A      =   5.D0
      B      =   4.D0
      HMAX   =   1.D0
      HMIN   =   1.1D0

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error case: HMIN <= -B.' )

      A      =   5.D0
      B      =   4.D0
      HMAX   =   1.D0
      HMIN   =  -B

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .TRUE., 'SPICE(LOWERBOUNDTOOLOW)', OK )


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Error case: B + (A/B)*HMIN <= 0' )

      A      =   5.D0
      B      =   4.D0
      HMAX   =   1.D0
      HMIN   =  -0.9D0 * B

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .TRUE., 'SPICE(LOWERBOUNDTOOLOW)', OK )


!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Normal case: HMAX > 0, HMIN < 0' )

      A      =   25.D0
      B      =   20.D0
      HMAX   =    1.D0
      HMIN   =   -1.D0

      XAMAX  =   A + HMAX
      XBMAX  =   B + HMAX*(A/B)

      XAMIN  =   A + HMIN
      XBMIN  =   B + HMIN*(A/B)

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'AMAX', AMAX, '~', XAMAX, VTIGHT, OK )
      CALL CHCKSD ( 'BMAX', BMAX, '~', XBMAX, VTIGHT, OK )

      CALL CHCKSD ( 'AMIN', AMIN, '~', XAMIN, VTIGHT, OK )
      CALL CHCKSD ( 'BMIN', BMIN, '~', XBMIN, VTIGHT, OK )

!
!     Sample heights from the bounding ellipsoids. Verify that
!     the heights satisfy the expected conditions.
!
      N     = 10001
      DELTA = 2*PI() / (N-1)

      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
!
!        Generate the cartesian coordinates of a point P on
!        the outer bounding ellipsoid at geodetic latitude LAT.
!
         CALL GEOREC ( XLON, XLAT, 0.D0, AMAX, (AMAX-BMAX)/AMAX, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Find the altitude of P relative to the spheroid with
!        radii A, B. The altitude should be no smaller than HMAX.
!
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .LT. HMAX ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '>=', HMAX-TIGHT, 0.D0, OK )

         END IF

      END DO


      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
!
!        Generate the cartesian coordinates of a point P on
!        the inner bounding ellipsoid at geodetic latitude LAT.
!
         CALL GEOREC ( XLON, XLAT, 0.D0, AMIN, (AMIN-BMIN)/AMIN, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Find the altitude of P relative to the spheroid with
!        radii A, B. The altitude should be no greater than HMIN.
!
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .GT. HMIN ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '<=', HMIN+TIGHT, 0.D0, OK )

         END IF

      END DO




!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Normal case: HMAX = 0, HMIN = 0' )

      A      =   25.D0
      B      =   20.D0
      HMAX   =    0.D0
      HMIN   =    0.D0

      XAMAX  =   A + HMAX
      XBMAX  =   B + HMAX*(A/B)

      XAMIN  =   A + HMIN
      XBMIN  =   B + HMIN*(A/B)

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'AMAX', AMAX, '~', XAMAX, VTIGHT, OK )
      CALL CHCKSD ( 'BMAX', BMAX, '~', XBMAX, VTIGHT, OK )

      CALL CHCKSD ( 'AMIN', AMIN, '~', XAMIN, VTIGHT, OK )
      CALL CHCKSD ( 'BMIN', BMIN, '~', XBMIN, VTIGHT, OK )


!
!     Sample heights from the bounding ellipsoids. Verify that
!     the heights satisfy the expected conditions.
!
      N     = 10001
      DELTA = 2*PI() / (N-1)

      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
!
!        Generate the cartesian coordinates of a point P on
!        the outer bounding ellipsoid at geodetic latitude LAT.
!
         CALL GEOREC ( XLON, XLAT, 0.D0, AMAX, (AMAX-BMAX)/AMAX, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Find the altitude of P relative to the spheroid with
!        radii A, B. The altitude should be no smaller than HMAX.
!
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .LT. HMAX ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '>=', HMAX-TIGHT, 0.D0, OK )

         END IF

      END DO


      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
!
!        Generate the cartesian coordinates of a point P on
!        the inner bounding ellipsoid at geodetic latitude LAT.
!
         CALL GEOREC ( XLON, XLAT, 0.D0, AMIN, (AMIN-BMIN)/AMIN, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Find the altitude of P relative to the spheroid with
!        radii A, B. The altitude should be no greater than HMIN.
!
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .GT. HMIN ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '<=', HMIN+TIGHT, 0.D0, OK )

         END IF

      END DO



!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Normal case: HMAX > 0, HMIN > 0' )

      A      =   25.D0
      B      =   20.D0
      HMAX   =    5.D0
      HMIN   =    1.D0

      XAMAX  =   A + HMAX
      XBMAX  =   B + HMAX*(A/B)

      XAMIN  =   A + HMIN*(B/A)
      XBMIN  =   B + HMIN

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'AMAX', AMAX, '~', XAMAX, VTIGHT, OK )
      CALL CHCKSD ( 'BMAX', BMAX, '~', XBMAX, VTIGHT, OK )

      CALL CHCKSD ( 'AMIN', AMIN, '~', XAMIN, VTIGHT, OK )
      CALL CHCKSD ( 'BMIN', BMIN, '~', XBMIN, VTIGHT, OK )

!
!     Sample heights from the bounding ellipsoids. Verify that
!     the heights satisfy the expected conditions.
!
      N     = 10001
      DELTA = 2*PI() / (N-1)

      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
!
!        Generate the cartesian coordinates of a point P on
!        the outer bounding ellipsoid at geodetic latitude LAT.
!
         CALL GEOREC ( XLON, XLAT, 0.D0, AMAX, (AMAX-BMAX)/AMAX, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Find the altitude of P relative to the spheroid with
!        radii A, B. The altitude should be no smaller than HMAX.
!
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .LT. HMAX ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '>=', HMAX-TIGHT, 0.D0, OK )

         END IF

      END DO


      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
!
!        Generate the cartesian coordinates of a point P on
!        the inner bounding ellipsoid at geodetic latitude LAT.
!
         CALL GEOREC ( XLON, XLAT, 0.D0, AMIN, (AMIN-BMIN)/AMIN, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Find the altitude of P relative to the spheroid with
!        radii A, B. The altitude should be no greater than HMIN.
!
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .GT. HMIN ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '<=', HMIN+TIGHT, 0.D0, OK )

         END IF

      END DO




!
! --- Case --------------------------------------------------------
!
      CALL TCASE ( 'Normal case: HMAX < 0, HMIN < 0' )

      A      =   25.D0
      B      =   20.D0
      HMAX   =   -1.D0
      HMIN   =   -2.D0

      XAMAX  =   A + HMAX*(B/A)
      XBMAX  =   B + HMAX

      XAMIN  =   A + HMIN
      XBMIN  =   B + HMIN*(A/B)


      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'AMAX', AMAX, '~', XAMAX, VTIGHT, OK )
      CALL CHCKSD ( 'BMAX', BMAX, '~', XBMAX, VTIGHT, OK )

      CALL CHCKSD ( 'AMIN', AMIN, '~', XAMIN, VTIGHT, OK )
      CALL CHCKSD ( 'BMIN', BMIN, '~', XBMIN, VTIGHT, OK )

!
!     Sample heights from the bounding ellipsoids. Verify that
!     the heights satisfy the expected conditions.
!
      N     = 10001
      DELTA = 2*PI() / (N-1)

      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
!
!        Generate the cartesian coordinates of a point P on
!        the outer bounding ellipsoid at geodetic latitude LAT.
!
         CALL GEOREC ( XLON, XLAT, 0.D0, AMAX, (AMAX-BMAX)/AMAX, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Find the altitude of P relative to the spheroid with
!        radii A, B. The altitude should be no smaller than HMAX.
!
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .LT. HMAX ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '>=', HMAX-TIGHT, 0.D0, OK )

         END IF

      END DO


      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
!
!        Generate the cartesian coordinates of a point P on
!        the inner bounding ellipsoid at geodetic latitude LAT.
!
         CALL GEOREC ( XLON, XLAT, 0.D0, AMIN, (AMIN-BMIN)/AMIN, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
!
!        Find the altitude of P relative to the spheroid with
!        radii A, B. The altitude should be no greater than HMIN.
!
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .GT. HMIN ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '<=', HMIN+TIGHT, 0.D0, OK )

         END IF

      END DO





      CALL T_SUCCESS ( OK )
      RETURN
      END
