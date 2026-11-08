C$Procedure      F_ZZELLBDS ( Test bounding ellipsoid routine )
 
      SUBROUTINE F_ZZELLBDS ( OK )
      IMPLICIT NONE

C$ Abstract
C
C     Test the SPICELIB bounding ellipsoid routine ZZELLBDS.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the routine ZZELLBDS.
C        
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 2.0.0, 13-FEB-2017 (NJB)
C
C        Previous version 15-JAN-2016 (NJB)
C
C        Reduced sample counts from 100001 to 1001.
C
C-    TSPICE Version 1.0.0, 27-NOV-2012 (NJB)
C
C-&

 
C
C     Test Utility Functions
C
 

C
C     SPICELIB functions
C
      DOUBLE PRECISION      PI

C
C     Local parameters
C
      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-15 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-13 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Local Variables
C       
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

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZELLBDS' )

C
C     ZZELLBDS error cases:
C           

C
C --- Case --------------------------------------------------------
C
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

C
C --- Case --------------------------------------------------------
C
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


C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error case: HMIN > HMAX.' )

      A      =   5.D0
      B      =   4.D0
      HMAX   =   1.D0
      HMIN   =   1.1D0

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )       
      CALL CHCKXC ( .TRUE., 'SPICE(BOUNDSOUTOFORDER)', OK )
 
 

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error case: HMIN <= -B.' )

      A      =   5.D0
      B      =   4.D0
      HMAX   =   1.D0
      HMIN   =  -B

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )       
      CALL CHCKXC ( .TRUE., 'SPICE(LOWERBOUNDTOOLOW)', OK )
 

C
C --- Case --------------------------------------------------------
C
      CALL TCASE ( 'Error case: B + (A/B)*HMIN <= 0' )

      A      =   5.D0
      B      =   4.D0
      HMAX   =   1.D0
      HMIN   =  -0.9D0 * B

      CALL ZZELLBDS( A, B, HMAX, HMIN, AMAX, BMAX, AMIN, BMIN )       
      CALL CHCKXC ( .TRUE., 'SPICE(LOWERBOUNDTOOLOW)', OK )
 

C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C --- Case --------------------------------------------------------
C
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

C
C     Sample heights from the bounding ellipsoids. Verify that
C     the heights satisfy the expected conditions.
C     
      N     = 10001
      DELTA = 2*PI() / (N-1)

      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
C
C        Generate the cartesian coordinates of a point P on
C        the outer bounding ellipsoid at geodetic latitude LAT.
C
         CALL GEOREC ( XLON, XLAT, 0.D0, AMAX, (AMAX-BMAX)/AMAX, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Find the altitude of P relative to the spheroid with 
C        radii A, B. The altitude should be no smaller than HMAX.
C
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
C
C        Generate the cartesian coordinates of a point P on
C        the inner bounding ellipsoid at geodetic latitude LAT.
C
         CALL GEOREC ( XLON, XLAT, 0.D0, AMIN, (AMIN-BMIN)/AMIN, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Find the altitude of P relative to the spheroid with 
C        radii A, B. The altitude should be no greater than HMIN.
C
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .GT. HMIN ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '<=', HMIN+TIGHT, 0.D0, OK )

         END IF

      END DO

  


C
C --- Case --------------------------------------------------------
C
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


C
C     Sample heights from the bounding ellipsoids. Verify that
C     the heights satisfy the expected conditions.
C     
      N     = 10001
      DELTA = 2*PI() / (N-1)

      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
C
C        Generate the cartesian coordinates of a point P on
C        the outer bounding ellipsoid at geodetic latitude LAT.
C
         CALL GEOREC ( XLON, XLAT, 0.D0, AMAX, (AMAX-BMAX)/AMAX, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Find the altitude of P relative to the spheroid with 
C        radii A, B. The altitude should be no smaller than HMAX.
C
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
C
C        Generate the cartesian coordinates of a point P on
C        the inner bounding ellipsoid at geodetic latitude LAT.
C
         CALL GEOREC ( XLON, XLAT, 0.D0, AMIN, (AMIN-BMIN)/AMIN, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Find the altitude of P relative to the spheroid with 
C        radii A, B. The altitude should be no greater than HMIN.
C
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .GT. HMIN ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '<=', HMIN+TIGHT, 0.D0, OK )

         END IF

      END DO

  

C
C --- Case --------------------------------------------------------
C
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

C
C     Sample heights from the bounding ellipsoids. Verify that
C     the heights satisfy the expected conditions.
C     
      N     = 10001
      DELTA = 2*PI() / (N-1)

      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
C
C        Generate the cartesian coordinates of a point P on
C        the outer bounding ellipsoid at geodetic latitude LAT.
C
         CALL GEOREC ( XLON, XLAT, 0.D0, AMAX, (AMAX-BMAX)/AMAX, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Find the altitude of P relative to the spheroid with 
C        radii A, B. The altitude should be no smaller than HMAX.
C
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
C
C        Generate the cartesian coordinates of a point P on
C        the inner bounding ellipsoid at geodetic latitude LAT.
C
         CALL GEOREC ( XLON, XLAT, 0.D0, AMIN, (AMIN-BMIN)/AMIN, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Find the altitude of P relative to the spheroid with 
C        radii A, B. The altitude should be no greater than HMIN.
C
         CALL RECGEO ( P, A, (A-B)/A, LON, LAT, ALT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         IF ( ALT .GT. HMIN ) THEN

            CASNAM = 'ALT for I = #'

            CALL REPMI  ( CASNAM, '#', I, CASNAM )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( CASNAM, ALT, '<=', HMIN+TIGHT, 0.D0, OK )

         END IF

      END DO

  


C
C --- Case --------------------------------------------------------
C
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

C
C     Sample heights from the bounding ellipsoids. Verify that
C     the heights satisfy the expected conditions.
C     
      N     = 10001
      DELTA = 2*PI() / (N-1)

      DO I = 1, N

         XLAT = (I-1)*DELTA

         XLON = PI()/3
C
C        Generate the cartesian coordinates of a point P on
C        the outer bounding ellipsoid at geodetic latitude LAT.
C
         CALL GEOREC ( XLON, XLAT, 0.D0, AMAX, (AMAX-BMAX)/AMAX, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Find the altitude of P relative to the spheroid with 
C        radii A, B. The altitude should be no smaller than HMAX.
C
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
C
C        Generate the cartesian coordinates of a point P on
C        the inner bounding ellipsoid at geodetic latitude LAT.
C
         CALL GEOREC ( XLON, XLAT, 0.D0, AMIN, (AMIN-BMIN)/AMIN, P )
         CALL CHCKXC ( .FALSE., ' ', OK )
C
C        Find the altitude of P relative to the spheroid with 
C        radii A, B. The altitude should be no greater than HMIN.
C
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
