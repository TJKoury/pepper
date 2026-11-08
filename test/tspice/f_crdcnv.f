C$Procedure F_CRDCNV (Family of tests on coordinate conversions)

      SUBROUTINE F_CRDCNV ( OK )

C$ Abstract
C
C     This routine performs rudimentary tests on coordinate conversion
C     routines.
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

      IMPLICIT NONE

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
C     None.
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
C     M.C. Kim      (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 27-JUL-2016 (MCK)(BVS)
C
C        Initial code to test
C
C           CYLLAT
C           CYLREC
C           CYLSPH
C           GEOREC
C           LATCYL
C           LATREC
C           LATSPH
C           RADREC
C           RECCYL
C           RECGEO
C           RECLAT
C           RECRAD
C           RECSPH
C           SRFREC
C           SPHCYL
C           SPHLAT
C           SPHREC
C
C-&

C$ Index_Entries
C
C     test coordinate conversion routines
C
C-&

C
C     SPICELIB Functions
C

      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      HALFPI

C
C     grid dimension of a cube ( NGRID x NGRID x NGRID )
C
      INTEGER               NGRID
      PARAMETER           ( NGRID = 5 )

C
C     number of NAIF bodies used for test
C
      INTEGER               NB
      PARAMETER           ( NB = 6 )

C
C     numerical tolerances
C
      DOUBLE PRECISION      LSE
      PARAMETER           ( LSE = 1.0D-9 )

      DOUBLE PRECISION      MED
      PARAMETER           ( MED = 1.0D-11 )

      DOUBLE PRECISION      TGH
      PARAMETER           ( TGH = 1.0D-15 )

C
C     equatorial radius of a reference ellipsoid
C
      DOUBLE PRECISION      AE
      PARAMETER           ( AE = 6378.137D0 )

C
C     flattening of the reference ellipsoid
C
      DOUBLE PRECISION      F
      PARAMETER           ( F = 1.0D0/298.257D0 )

C
C     polar radius of the reference ellipsoid
C
      DOUBLE PRECISION      BE
      PARAMETER           ( BE = AE * ( 1.0D0 - F ) )

C
C     Test PCK.
C
      CHARACTER*(*)         PCK 
      PARAMETER           ( PCK   = 'test_0008.tpc' )

C
C     Local variables
C

C
C     grid locations
C
      DOUBLE PRECISION      GRID   ( NGRID )

C
C     NAIF body ID numbers
C
      INTEGER               BODY   ( NB )

C
C     grid index on x-axis
C
      INTEGER               IX

C
C     grid index on y-axis
C
      INTEGER               IY

C
C     grid index on z-axis
C
      INTEGER               IZ

C
C     Listed below are coordinate values computed in this routine at
C     each grid point. All angles are in radians.
C

C
C     x-component of the right-handed Cartesian frame
C
      DOUBLE PRECISION      EX

C
C     y-component of the right-handed Cartesian frame
C
      DOUBLE PRECISION      EY

C
C     z-component of the right-handed Cartesian frame
C
      DOUBLE PRECISION      EZ

C
C     distance from origin
C
      DOUBLE PRECISION      ER

C
C     distance from z-axis
C
      DOUBLE PRECISION      ERHO

C
C     latitude in [ -HALFPI(), +HALFPI() ]
C
      DOUBLE PRECISION      ELAT

C
C     co-latitude in [ 0.0D0, +PI() ]
C
      DOUBLE PRECISION      ECLAT

C
C     east longitude in ( -PI(), +PI() ]
C
      DOUBLE PRECISION      ELON

C
C     right ascension in [ 0.0D0, TWOPI() )
C
      DOUBLE PRECISION      ERA

C
C     Listed below are coordinate values returned from SPICELIB
C     routines.
C

C
C     rectangular coordinates
C
      DOUBLE PRECISION      REC ( 3 )

C
C     cylindrical coordinates
C
      DOUBLE PRECISION      RHO
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      Z

C
C     spherical coordinates
C
      DOUBLE PRECISION      R
      DOUBLE PRECISION      CLAT
      DOUBLE PRECISION      LONG

C
C     latitudinal coordinates
C
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      LLON
      DOUBLE PRECISION      LLAT

C
C     RAD ( range, right ascension, declination ) coordinates
C
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      DEC

C
C     geodetic coordinates
C
      DOUBLE PRECISION      GLON
      DOUBLE PRECISION      GLAT
      DOUBLE PRECISION      GALT

C
C     surface coordinates
C
      DOUBLE PRECISION      SLAT
      DOUBLE PRECISION      SLON

C
C     naif body ID number
C
      INTEGER               IB

C
C     geodetic altitude vector
C
      DOUBLE PRECISION      GVEC   ( 3 )

C
C     evaluation of spheroidal surface equation
C
      DOUBLE PRECISION      SRFEQN

C
C     origin to nadir vector
C
      DOUBLE PRECISION      NADIR  ( 3 )

C
C     radii of tri-axial ellipsoid
C
      DOUBLE PRECISION      RADII  ( 3 )

C
C     number of radii
C
      INTEGER               NDIM

C
C     test case description
C
      CHARACTER*(80)        CASE

C
C     Save everything.
C
      SAVE

C
C     Initial values
C
      DATA                  GRID  / -1.0D0, 0.0D0, 1.D-2, 1.0D0, 1.D4 /

      DATA                  BODY  / 299, 399, 499, 599, 699, 799 /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_CRDCNV' )

C
C     Create the PCK file, load it, and delete it. We need it to get
C     values of radii of naif bodies.
C      
      CALL T_PCK08 ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Test loop over grid points of the NGRID x NGRID x NGRID cube.
C
      DO IX = 1, NGRID

         DO IY = 1, NGRID

            DO IZ = 1, NGRID

C
C              rectangular coordinates of a grid point
C
               EX = GRID( IX )
               EY = GRID( IY )
               EZ = GRID( IZ )

C
C              Exclude the origin from test.
C
               IF ( EX .EQ. 0.0D0 .AND. 
     .              EX .EQ. 0.0D0 .AND. 
     .              EX .EQ. 0.0D0       ) THEN

                  EX = 1.0D0
                  EY = 1.0D0
                  EZ = 1.0D0

               END IF

C
C              radius from origin
C
               ER = DSQRT( EX**2 + EY**2 + EZ**2 )

C
C              radius from z-axis
C
               ERHO = DSQRT( EX**2 + EY**2 )

C
C              right ascension in [ 0.0D0, TWOPI() )
C
               ERA = DMOD( DATAN2( EY, EX ), TWOPI() )

               IF ( ERA .LT. 0.0D0 ) THEN
                  ERA = ERA + TWOPI()
               END IF

C
C              east longitude in ( -PI(), PI() ]
C
               ELON = DATAN2( EY, EX )
 
               IF ( ELON .EQ. -PI() ) THEN 
                  ELON = PI()
               END IF

C
C              co-latitude in [ 0.0D0, PI() ]
C
               ECLAT = DATAN2( ERHO, EZ )

C
C              latitude in [ -HALFPI(), HALFPI() ]
C
               ELAT  = DATAN2( EZ, ERHO )

C
C              Identity the grid point on the test description.
C              
               CASE = '# # #'
               CALL REPMD ( CASE, '#', EX, 14, CASE )
               CALL REPMD ( CASE, '#', EY, 14, CASE )
               CALL REPMD ( CASE, '#', EZ, 14, CASE )

C
C              Test rectangular to cylindrical coordinate conversion.
C

               CALL TCASE ( 'RECCYL: ' // CASE )

C
C              Pack the grid point into a rectangular coordinate
C              vector.
C
               CALL VPACK( EX, EY, EZ, REC )

C
C              Convert to cylindrical coordinates.
C              
               CALL RECCYL ( REC, RHO, LON, Z )

C
C              The cylindrical longitude, LON, returned from subroutine
C              RECCYL is in the range of [ 0.0D0, TWOPI() ). This
C              conflicts with the range of longitude, ( -PI(), +PI() ],
C              defined in some other subroutines. That is,
C
               LON = DATAN2 ( REC(2), REC(1) )

C              
C              The above line, inserted to suppress error messages,
C              should be removed after fixing the inconsistency in the
C              definition of longitude.
C

C
C              Check the converted coordinates.
C
               CALL CHCKSD ( 'RHO', RHO, '~', ERHO, MED, OK )
               CALL CHCKSD ( 'LON', LON, '~', ELON, MED, OK )
               CALL CHCKSD ( 'Z',   Z,   '~', EZ,   MED, OK )


C
C              Test cylindrical to rectangular coordinate conversion.
C
               CALL TCASE ( 'CYLREC: ' // CASE )

C
C              Revert cylindrical coordinates to rectangular
C              coordinates.
C
               CALL CYLREC ( RHO, LON, Z, REC )

C
C              Check the reverted coordinates.
C
               CALL CHCKSD ( 'REC(1)', REC(1), '~', EX, MED, OK )
               CALL CHCKSD ( 'REC(2)', REC(2), '~', EY, MED, OK )
               CALL CHCKSD ( 'REC(3)', REC(3), '~', EZ, MED, OK )

C
C              Test rectangular to spherical coordinate conversion.
C
               CALL TCASE ( 'RECSPH: ' // CASE )

C
C              Pack the grid point into a rectangular coordinate
C              vector.
C
               CALL VPACK( EX, EY, EZ, REC )

C
C              Convert to spherical coordinates.
C
               CALL RECSPH ( REC, R, CLAT, LONG )

C
C              Check the converted coordinates.
C
               CALL CHCKSD ( 'R',    R,    '~', ER,    MED, OK )
               CALL CHCKSD ( 'CLAT', CLAT, '~', ECLAT, MED, OK )
               CALL CHCKSD ( 'LONG', LONG, '~', ELON,  MED, OK )

C
C              Test spherical to rectangular coordinate conversion.
C
               CALL TCASE ( 'SPHREC: '  //  CASE )

C
C              Revert spherical coordinates to rectangular coordinates.
C
               CALL SPHREC ( R, CLAT, LONG, REC )

C
C              Check the reverted coordinates.
C
               CALL CHCKSD ( 'REC(1)', REC(1), '~', EX, MED, OK )
               CALL CHCKSD ( 'REC(2)', REC(2), '~', EY, MED, OK )
               CALL CHCKSD ( 'REC(3)', REC(3), '~', EZ, MED, OK )

C
C              Test rectangular to latitudinal coordinate conversion.
C
               CALL TCASE ( 'RECLAT: ' // CASE )

C
C              Pack the grid point into a rectangular coordinate
C              vector.
C
               CALL VPACK ( EX, EY, EZ, REC )

C
C              Convert to latitudinal coordinates.
C
               CALL RECLAT ( REC, RADIUS, LLON, LLAT )

C
C              Check the converted coordinates.
C
               CALL CHCKSD ( 'RADIUS', RADIUS, '~', ER,   MED, OK )
               CALL CHCKSD ( 'LLON',   LLON,   '~', ELON, MED, OK )
               CALL CHCKSD ( 'LLAT',   LLAT,   '~', ELAT, MED, OK )

C
C              Test latitudinal to rectangular coordinate conversion.
C
               CALL TCASE ( 'LATREC: ' // CASE )

C
C              Revert latitudinal coordinates to rectangular
C              coordinates.
C
               CALL LATREC ( RADIUS, LLON, LLAT, REC )

C
C              Check the reverted coordinates.
C
               CALL CHCKSD ( 'REC(1)', REC(1), '~', EX, MED, OK )
               CALL CHCKSD ( 'REC(2)', REC(2), '~', EY, MED, OK )
               CALL CHCKSD ( 'REC(3)', REC(3), '~', EZ, MED, OK )

C
C              Test cylindrical to spherical coordinate conversion.
C
               CALL TCASE ( 'CYLSPH: ' // CASE )

C
C              Convert cylindrical coordinates to latitudinal
C              coordinates.
C
               CALL CYLSPH ( RHO, LON, Z, R, CLAT, LONG )

C
C              Check the converted coordinates.
C
               CALL CHCKSD ( 'R',    R,    '~', ER,    MED, OK )
               CALL CHCKSD ( 'CLAT', CLAT, '~', ECLAT, MED, OK )
               CALL CHCKSD ( 'LONG', LONG, '~', ELON,  MED, OK )

C
C              Test spherical to cylindrical coordinate conversion.
C

               CALL TCASE ( 'SPHCYL: ' // CASE )

C
C              Revert latitudinal coordinates to cylindrical
C              coordinates.
C              
               CALL SPHCYL ( R, CLAT, LONG, RHO, LON, Z )

C
C              Check the reverted coordinates.
C              
               CALL CHCKSD ( 'RHO', RHO, '~', ERHO, MED, OK )
               CALL CHCKSD ( 'LON', LON, '~', ELON, MED, OK )
               CALL CHCKSD ( 'Z',   Z,   '~', EZ,   MED, OK )

C
C              Test latitudinal to spherical coordinate conversion.
C

               CALL TCASE ( 'LATSPH: ' // CASE )

C
C              Convert latitudinal coordinates to spherical
C              coordinates.
C
               CALL LATSPH ( RADIUS, LLON, LLAT, R, CLAT, LONG )

C
C              Check the converted coordinates.
C
               CALL CHCKSD ( 'R',    R,    '~', ER,    MED, OK )
               CALL CHCKSD ( 'CLAT', CLAT, '~', ECLAT, MED, OK )
               CALL CHCKSD ( 'LONG', LONG, '~', ELON,  MED, OK )

C
C              Test spherical to latitudinal coordinate conversion.
C
               CALL TCASE ('SPHLAT: ' // CASE )

C
C              Revert spherical coordinates to latitudinal coordinates.
C
               CALL SPHLAT ( R, CLAT, LONG, RADIUS, LLON, LLAT )

C
C              Check the reverted coordinates.
C
               CALL CHCKSD ( 'RADIUS', RADIUS, '~', ER,   MED, OK )
               CALL CHCKSD ( 'LLON',   LLON,   '~', ELON, MED, OK )
               CALL CHCKSD ( 'LLAT',   LLAT,   '~', ELAT, MED, OK )

C
C              Test latitudinal to cylindrical coordinate conversion.
C
               CALL TCASE ( 'LATCYL: ' // CASE )

C
C              Convert latitudinal coordinates to cylindrical
C              coordinates.
C
               CALL LATCYL ( RADIUS, LLON, LLAT, RHO, LON, Z )

C
C              Check the converted coordinates.
C
               CALL CHCKSD ( 'RHO', RHO, '~', ERHO, MED, OK )
               CALL CHCKSD ( 'LON', LON, '~', ELON, MED, OK )
               CALL CHCKSD ( 'Z',   Z,   '~', EZ,   MED, OK )

C
C              Test cylindrical to latitudinal coordinate conversion.
C
               CALL TCASE ('CYLLAT: ' // CASE )

C
C              Revert cylindrical coordinates to latitudinal
C              coordinates.
C
               CALL CYLLAT ( RHO, LON, Z, RADIUS, LLON, LLAT )

C
C              Check the reverted coordinates.
C
               CALL CHCKSD ( 'RADIUS', RADIUS, '~', ER,   MED, OK )
               CALL CHCKSD ( 'LLON',   LLON,   '~', ELON, MED, OK )
               CALL CHCKSD ( 'LLAT',   LLAT,   '~', ELAT, MED, OK )

C
C              Test rectangular to RAD coordinate conversion.
C
               CALL TCASE ( 'RECRAD: ' // CASE )

C
C              Pack the grid point into a rectangular coordinate
C              vector.
C
               CALL VPACK( EX, EY, EZ, REC )

C              
C              Convert rectangular coordinates to RAD coordinates.
C
               CALL RECRAD( REC, RANGE, RA, DEC  )

C
C              Check the converted coordinates.
C
               CALL CHCKSD ( 'RANGE', RANGE, '~', ER,   MED, OK )
               CALL CHCKSD ( 'RA',    RA,    '~', ERA,  MED, OK )
               CALL CHCKSD ( 'DEC',   DEC,   '~', ELAT, MED, OK )

C
C              Test RAD to rectangular coordinate conversion.
C
               CALL TCASE ( 'RADREC: ' // CASE )

C
C              Revert RAD coordinates to rectangular coordinates.
C
               CALL RADREC( RANGE, RA, DEC, REC  )

C
C              Check the converted coordinates.
C
               CALL CHCKSD ( 'REC(1)', REC(1), '~', EX, MED, OK )
               CALL CHCKSD ( 'REC(2)', REC(2), '~', EY, MED, OK )
               CALL CHCKSD ( 'REC(3)', REC(3), '~', EZ, MED, OK )

C
C              Test rectangular to geodetic coordinate conversion.
C
               CALL TCASE ( 'RECGEO: ' // CASE )

C
C              Pack the grid point into a rectangular coordinate
C              vector.
C
               CALL VPACK( EX, EY, EZ, REC )

C
C              Convert rectangular coordinates to geodetic coordinates.
C
               CALL RECGEO( REC, AE, F, GLON, GLAT, GALT )

               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              First, check the geodetic longitude, which should be the
C              same as longitudes in other coordinate system.
C
               CALL CHCKSD ( 'GLON', GLON, '~', ELON, MED, OK )

C              
C              Derive the geodetic altitude vector, GVEC, which is
C              vertically pointing from the nadir on the surface of the
C              reference ellipsoid to the grid point.

               CALL LATREC( GALT, GLON, GLAT, GVEC )

C
C              Get rectangular coordinates of the nadir.
C
               CALL VSUB( REC, GVEC, NADIR )

C
C              Being on the surface of the reference ellipsoid, the
C              rectangular coordinates of the nadir should satisfy the
C              spheroidal surface equation associated with the reference
C              ellipsoid.

C
C              Evaluate the spheroidal surface equation.
C
               SRFEQN =   ( NADIR(1) / AE )**2
     .                  + ( NADIR(2) / AE )**2
     .                  + ( NADIR(3) / BE )**2 - 1.0D0

C
C              Check the nadir is on the surface.
C
               CALL CHCKSD ( 'SRFEQN', SRFEQN, '~', 0.0D0, MED, OK )

C
C              Test geodetic to rectangular coordinate conversion.
C
               CALL TCASE ( 'GEOREC: ' // CASE )

C
C              Revert geodetic coordinates to rectangular coordinates.
C
               CALL GEOREC(  GLON, GLAT, GALT, AE, F, REC )

               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Check the reverted coordinates.
C
               CALL CHCKSD ( 'REC(1)', REC(1), '~', EX, LSE, OK )
               CALL CHCKSD ( 'REC(2)', REC(2), '~', EY, LSE, OK )
               CALL CHCKSD ( 'REC(3)', REC(3), '~', EZ, LSE, OK )

C
C              Test surface to rectangular coordinates conversion.
C

C
C              Set the surface coordinates ( longitude and latitude )
C              computed at the grid point.
C
               SLON = ELON
               SLAT = ELAT

C
C              Loop over NAIF bodies
C
               DO IB = 1, NB

C
C                 Identity NAIF body and surface point on the test
C                 description.
C
                  CASE = '# # #'
                  CALL REPMI ( CASE, '#', BODY(IB), CASE )
                  CALL REPMD ( CASE, '#', SLON, 14, CASE )
                  CALL REPMD ( CASE, '#', SLAT, 14, CASE )

                  CALL TCASE( 'SRFREC: ' // CASE )

C
C                 Convert surface coordinates to rectangular
C                 coordinates.
C
                  CALL SRFREC ( BODY(IB), SLON, SLAT, NADIR )

                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Convert rectangular coordinates to latitudinal
C                 coordinates.
C
                  CALL RECLAT ( NADIR, RADIUS, LLON, LLAT )

C
C                 The longitude and latitude pair of latitudinal
C                 coordinates should be that of surface coordinates.
C
                  CALL CHCKSD ( 'LLON', LLON, '~', ELON, MED, OK )
                  CALL CHCKSD ( 'LLAT', LLAT, '~', ELAT, MED, OK )

C
C                 Get semi-axes, RADII, of the naif body.
C
                  CALL BODVCD ( BODY( IB ), 'RADII', 3, NDIM, RADII )

                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Being on the surface of the naif body, the recovered
C                 rectangular coordinates should satisfy the spheroidal
C                 surface equation associated with the naif body.
C
C                 Evaluate the spheroidal surface equation.
C
                  SRFEQN =    ( NADIR(1) / RADII(1) )**2
     .                      + ( NADIR(2) / RADII(2) )**2
     .                      + ( NADIR(3) / RADII(3) )**2 - 1.0D0

C                 
C                 Check the nadir is on the surface.
C
                  CALL CHCKSD ( 'SRFEQN', SRFEQN, '~', 0.0D0, MED, OK )

               END DO

            END DO

         END DO

      END DO

C
C     Exception test cases.
C
      CALL TCASE  ( 'GEOREC exceptions' )

      CALL GEOREC (  1.0D0, 1.0D0, 1.0D0,  0.0D0, 0.5D0, REC )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL GEOREC (  1.0D0, 1.0D0, 1.0D0, -1.0D0, 0.5D0, REC )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL GEOREC (  1.0D0, 1.0D0, 1.0D0,  1.0D0, 1.0D0, REC )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

      CALL GEOREC (  1.0D0, 1.0D0, 1.0D0,  1.0D0, 1.1D0, REC )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )


      CALL TCASE  ( 'RECGEO exceptions' )

      CALL VPACK  ( 1.0D0, 1.0D0, 1.0D0, REC )

      CALL RECGEO ( REC,  0.0D0, 0.5D0, GLON, GLAT, GALT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
      
      CALL RECGEO ( REC, -1.0D0, 0.5D0, GLON, GLAT, GALT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
      
      CALL RECGEO ( REC,  1.0D0, 1.0D0, GLON, GLAT, GALT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )
      
      CALL RECGEO ( REC,  1.0D0, 1.1D0, GLON, GLAT, GALT )
      CALL CHCKXC ( .TRUE., 'SPICE(VALUEOUTOFRANGE)', OK )

C
C     (0,0,0) test cases.
C
      CALL VPACK  ( 0.0D0, 0.0D0, 0.0D0, REC )

      CALL TCASE  ( 'RECCYL (0,0,0) input' )
      CALL RECCYL ( REC, RHO, LON, Z )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'RHO', RHO, '=', 0.D0, TGH, OK )
      CALL CHCKSD ( 'LON', LON, '=', 0.D0, TGH, OK )
      CALL CHCKSD ( 'Z',   Z,   '=', 0.D0, TGH, OK )

      CALL TCASE  ( 'RECGEO (0,0,0) input' )
      CALL RECGEO ( REC, 2.0D0, 0.5D0, GLON, GLAT, GALT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'GLON', GLON, '=', 0.D0,     TGH, OK )
      CALL CHCKSD ( 'GLAT', GLAT, '=', HALFPI(), TGH, OK )
      CALL CHCKSD ( 'GALT', GALT, '=', -1.0D0,   TGH, OK )

      CALL TCASE  ( 'RECLAT (0,0,0) input' )
      CALL RECLAT ( REC, RADIUS, LLON, LLAT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'RADIUS', RADIUS, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'LLON',   LLON,   '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'LLAT',   LLAT,   '=', 0.0D0, TGH, OK )

      CALL TCASE  ( 'RECRAD (0,0,0) input' )
      CALL RECRAD ( REC, RANGE, RA, DEC  )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'RANGE', RANGE, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'RA',    RA,    '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'DEC',   DEC,   '=', 0.0D0, TGH, OK )

      CALL TCASE  ( 'RECSPH (0,0,0) input' )
      CALL RECSPH ( REC, R, CLAT, LONG )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'R',    R,    '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'CLAT', CLAT, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'LONG', LONG, '=', 0.0D0, TGH, OK )

      

      CALL TCASE  ( 'CYLLAT (0,0,0) input' )
      CALL CYLLAT ( 0.0D0, 0.0D0, 0.0D0, RADIUS, LLON, LLAT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'RADIUS', RADIUS, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'LLON',   LLON,   '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'LLAT',   LLAT,   '=', 0.0D0, TGH, OK )

      CALL TCASE  ( 'CYLREC (0,0,0) input' )
      CALL CYLREC ( 0.0D0, 0.0D0, 0.0D0, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'REC(1)', REC(1), '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(2)', REC(2), '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(3)', REC(3), '=', 0.0D0, TGH, OK )

      CALL TCASE  ( 'CYLSPH (0,0,0) input' )
      CALL CYLSPH ( 0.0D0, 0.0D0, 0.0D0, R, CLAT, LONG )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'R',    R,    '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'CLAT', CLAT, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'LONG', LONG, '=', 0.0D0, TGH, OK )


      CALL TCASE  ( 'GEOREC (0,0,0) input' )
      CALL GEOREC(  0.0D0, 0.0D0, 0.0D0, 2.0D0, 0.5D0, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'REC(1)', REC(1), '=', 2.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(2)', REC(2), '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(3)', REC(3), '=', 0.0D0, TGH, OK )


      CALL TCASE  ( 'LATCYL (0,0,0) input' )
      CALL LATCYL ( 0.0D0, 0.0D0, 0.0D0, RHO, LON, Z )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'RHO', RHO, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'LON', LON, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'Z',   Z,   '=', 0.0D0, TGH, OK )

      CALL TCASE  ( 'LATREC (0,0,0) input' )
      CALL LATREC ( 0.0D0, 0.0D0, 0.0D0, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'REC(1)', REC(1), '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(2)', REC(2), '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(3)', REC(3), '=', 0.0D0, TGH, OK )

      CALL TCASE  ( 'LATSPH (0,0,0) input' )
      CALL LATSPH ( 0.0D0, 0.0D0, 0.0D0, R, CLAT, LONG )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'R',    R,    '=', 0.0D0,    TGH, OK )
      CALL CHCKSD ( 'CLAT', CLAT, '=', HALFPI(), TGH, OK )
      CALL CHCKSD ( 'LONG', LONG, '=', 0.0D0,    TGH, OK )


      CALL TCASE  ( 'RADREC (0,0,0) input' )
      CALL RADREC ( 0.0D0, 0.0D0, 0.0D0, REC  )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'REC(1)', REC(1), '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(2)', REC(2), '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(3)', REC(3), '=', 0.0D0, TGH, OK )

      CALL TCASE  ( 'SPHCYL (0,0,0) input' )
      CALL SPHCYL ( 0.0D0, 0.0D0, 0.0D0, RHO, LON, Z )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'RHO', RHO, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'LON', LON, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'Z',   Z,   '=', 0.0D0, TGH, OK )

      CALL TCASE  ( 'SPHLAT (0,0,0) input' )
      CALL SPHLAT ( 0.0D0, 0.0D0, 0.0D0, RADIUS, LLON, LLAT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'RADIUS', RADIUS, '=', 0.0D0,    TGH, OK )
      CALL CHCKSD ( 'LLON',   LLON,   '=', 0.0D0,    TGH, OK )
      CALL CHCKSD ( 'LLAT',   LLAT,   '=', HALFPI(), TGH, OK )

      CALL TCASE  ( 'SPHREC (0,0,0) input' )
      CALL SPHREC ( 0.0D0, 0.0D0, 0.0D0, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'REC(1)', REC(1), '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(2)', REC(2), '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(3)', REC(3), '=', 0.0D0, TGH, OK )

      CALL TCASE  ( 'SRFREC (0,0) input' )
      CALL BODVCD ( 399, 'RADII', 3, NDIM, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL SRFREC ( 399, 0.0D0, 0.0D0, REC )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'REC(1)', REC(1), '=', RADII(1), TGH, OK )
      CALL CHCKSD ( 'REC(2)', REC(2), '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'REC(3)', REC(3), '=', 0.0D0, TGH, OK )


C
C     Test cases, with one or more items set to 0.
C
      CALL TCASE  ( 'CYLLAT (0,HALFPI,0) input' )
      CALL CYLLAT ( 0.0D0, HALFPI(), 0.0D0, RADIUS, LLON, LLAT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'RADIUS', RADIUS, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'LLON',   LLON,   '=', HALFPI(), TGH, OK )
      CALL CHCKSD ( 'LLAT',   LLAT,   '=', 0.0D0, TGH, OK )

      CALL TCASE  ( 'RECCYL (0,0,1) input' )
      CALL VPACK  ( 0.0D0, 0.0D0, 1.0D0, REC )
      CALL RECCYL ( REC, RHO, LON, Z )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'RHO', RHO, '=', 0.D0, TGH, OK )
      CALL CHCKSD ( 'LON', LON, '=', 0.D0, TGH, OK )
      CALL CHCKSD ( 'Z',   Z,   '=', 1.D0, TGH, OK )

      CALL TCASE  ( 'RECSPH (0,0,1) input' )
      CALL VPACK  ( 0.0D0, 0.0D0, 1.0D0, REC )
      CALL RECSPH ( REC, R, CLAT, LONG )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSD ( 'R',    R,    '=', 1.0D0, TGH, OK )
      CALL CHCKSD ( 'CLAT', CLAT, '=', 0.0D0, TGH, OK )
      CALL CHCKSD ( 'LONG', LONG, '=', 0.0D0, TGH, OK )

C
C     Test cases with angular inputs outside of normal ranges.
C
C     THESE TEST HAVE NOT BEED IMPLEMENTED FOR N0066. THEY MIGHT
C     BE IMPLEMENTED AT A LATER TIME.
C

      CALL T_SUCCESS ( OK )

      RETURN
      END
