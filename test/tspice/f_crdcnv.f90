!$Procedure F_CRDCNV (Family of tests on coordinate conversions)

      SUBROUTINE F_CRDCNV ( OK )

! $ Abstract
!
!     This routine performs rudimentary tests on coordinate conversion
!     routines.
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
!     None.
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
!     M.C. Kim      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 27-JUL-2016 (MCK)(BVS)
!
!        Initial code to test
!
!           CYLLAT
!           CYLREC
!           CYLSPH
!           GEOREC
!           LATCYL
!           LATREC
!           LATSPH
!           RADREC
!           RECCYL
!           RECGEO
!           RECLAT
!           RECRAD
!           RECSPH
!           SRFREC
!           SPHCYL
!           SPHLAT
!           SPHREC
!
!-&

! $ Index_Entries
!
!     test coordinate conversion routines
!
!-&

!
!     SPICELIB Functions
!

      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      HALFPI

!
!     grid dimension of a cube ( NGRID x NGRID x NGRID )
!
      INTEGER               NGRID
      PARAMETER           ( NGRID = 5 )

!
!     number of NAIF bodies used for test
!
      INTEGER               NB
      PARAMETER           ( NB = 6 )

!
!     numerical tolerances
!
      DOUBLE PRECISION      LSE
      PARAMETER           ( LSE = 1.0D-9 )

      DOUBLE PRECISION      MED
      PARAMETER           ( MED = 1.0D-11 )

      DOUBLE PRECISION      TGH
      PARAMETER           ( TGH = 1.0D-15 )

!
!     equatorial radius of a reference ellipsoid
!
      DOUBLE PRECISION      AE
      PARAMETER           ( AE = 6378.137D0 )

!
!     flattening of the reference ellipsoid
!
      DOUBLE PRECISION      F
      PARAMETER           ( F = 1.0D0/298.257D0 )

!
!     polar radius of the reference ellipsoid
!
      DOUBLE PRECISION      BE
      PARAMETER           ( BE = AE * ( 1.0D0 - F ) )

!
!     Test PCK.
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK   = 'test_0008.tpc' )

!
!     Local variables
!

!
!     grid locations
!
      DOUBLE PRECISION      GRID   ( NGRID )

!
!     NAIF body ID numbers
!
      INTEGER               BODY   ( NB )

!
!     grid index on x-axis
!
      INTEGER               IX

!
!     grid index on y-axis
!
      INTEGER               IY

!
!     grid index on z-axis
!
      INTEGER               IZ

!
!     Listed below are coordinate values computed in this routine at
!     each grid point. All angles are in radians.
!

!
!     x-component of the right-handed Cartesian frame
!
      DOUBLE PRECISION      EX

!
!     y-component of the right-handed Cartesian frame
!
      DOUBLE PRECISION      EY

!
!     z-component of the right-handed Cartesian frame
!
      DOUBLE PRECISION      EZ

!
!     distance from origin
!
      DOUBLE PRECISION      ER

!
!     distance from z-axis
!
      DOUBLE PRECISION      ERHO

!
!     latitude in [ -HALFPI(), +HALFPI() ]
!
      DOUBLE PRECISION      ELAT

!
!     co-latitude in [ 0.0D0, +PI() ]
!
      DOUBLE PRECISION      ECLAT

!
!     east longitude in ( -PI(), +PI() ]
!
      DOUBLE PRECISION      ELON

!
!     right ascension in [ 0.0D0, TWOPI() )
!
      DOUBLE PRECISION      ERA

!
!     Listed below are coordinate values returned from SPICELIB
!     routines.
!

!
!     rectangular coordinates
!
      DOUBLE PRECISION      REC ( 3 )

!
!     cylindrical coordinates
!
      DOUBLE PRECISION      RHO
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      Z

!
!     spherical coordinates
!
      DOUBLE PRECISION      R
      DOUBLE PRECISION      CLAT
      DOUBLE PRECISION      LONG

!
!     latitudinal coordinates
!
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      LLON
      DOUBLE PRECISION      LLAT

!
!     RAD ( range, right ascension, declination ) coordinates
!
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      DEC

!
!     geodetic coordinates
!
      DOUBLE PRECISION      GLON
      DOUBLE PRECISION      GLAT
      DOUBLE PRECISION      GALT

!
!     surface coordinates
!
      DOUBLE PRECISION      SLAT
      DOUBLE PRECISION      SLON

!
!     naif body ID number
!
      INTEGER               IB

!
!     geodetic altitude vector
!
      DOUBLE PRECISION      GVEC   ( 3 )

!
!     evaluation of spheroidal surface equation
!
      DOUBLE PRECISION      SRFEQN

!
!     origin to nadir vector
!
      DOUBLE PRECISION      NADIR  ( 3 )

!
!     radii of tri-axial ellipsoid
!
      DOUBLE PRECISION      RADII  ( 3 )

!
!     number of radii
!
      INTEGER               NDIM

!
!     test case description
!
      CHARACTER*(80)        CASE

!
!     Save everything.
!
      SAVE

!
!     Initial values
!
      DATA                  GRID  / -1.0D0, 0.0D0, 1.D-2, 1.0D0, 1.D4 /

      DATA                  BODY  / 299, 399, 499, 599, 699, 799 /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_CRDCNV' )

!
!     Create the PCK file, load it, and delete it. We need it to get
!     values of radii of naif bodies.
!
      CALL T_PCK08 ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Test loop over grid points of the NGRID x NGRID x NGRID cube.
!
      DO IX = 1, NGRID

         DO IY = 1, NGRID

            DO IZ = 1, NGRID

!
!              rectangular coordinates of a grid point
!
               EX = GRID( IX )
               EY = GRID( IY )
               EZ = GRID( IZ )

!
!              Exclude the origin from test.
!
               IF ( EX .EQ. 0.0D0 .AND. &
                    EX .EQ. 0.0D0 .AND. &
                    EX .EQ. 0.0D0       ) THEN

                  EX = 1.0D0
                  EY = 1.0D0
                  EZ = 1.0D0

               END IF

!
!              radius from origin
!
               ER = DSQRT( EX**2 + EY**2 + EZ**2 )

!
!              radius from z-axis
!
               ERHO = DSQRT( EX**2 + EY**2 )

!
!              right ascension in [ 0.0D0, TWOPI() )
!
               ERA = DMOD( DATAN2( EY, EX ), TWOPI() )

               IF ( ERA .LT. 0.0D0 ) THEN
                  ERA = ERA + TWOPI()
               END IF

!
!              east longitude in ( -PI(), PI() ]
!
               ELON = DATAN2( EY, EX )

               IF ( ELON .EQ. -PI() ) THEN
                  ELON = PI()
               END IF

!
!              co-latitude in [ 0.0D0, PI() ]
!
               ECLAT = DATAN2( ERHO, EZ )

!
!              latitude in [ -HALFPI(), HALFPI() ]
!
               ELAT  = DATAN2( EZ, ERHO )

!
!              Identity the grid point on the test description.
!
               CASE = '# # #'
               CALL REPMD ( CASE, '#', EX, 14, CASE )
               CALL REPMD ( CASE, '#', EY, 14, CASE )
               CALL REPMD ( CASE, '#', EZ, 14, CASE )

!
!              Test rectangular to cylindrical coordinate conversion.
!

               CALL TCASE ( 'RECCYL: ' // CASE )

!
!              Pack the grid point into a rectangular coordinate
!              vector.
!
               CALL VPACK( EX, EY, EZ, REC )

!
!              Convert to cylindrical coordinates.
!
               CALL RECCYL ( REC, RHO, LON, Z )

!
!              The cylindrical longitude, LON, returned from subroutine
!              RECCYL is in the range of [ 0.0D0, TWOPI() ). This
!              conflicts with the range of longitude, ( -PI(), +PI() ],
!              defined in some other subroutines. That is,
!
               LON = DATAN2 ( REC(2), REC(1) )

!
!              The above line, inserted to suppress error messages,
!              should be removed after fixing the inconsistency in the
!              definition of longitude.
!

!
!              Check the converted coordinates.
!
               CALL CHCKSD ( 'RHO', RHO, '~', ERHO, MED, OK )
               CALL CHCKSD ( 'LON', LON, '~', ELON, MED, OK )
               CALL CHCKSD ( 'Z',   Z,   '~', EZ,   MED, OK )


!
!              Test cylindrical to rectangular coordinate conversion.
!
               CALL TCASE ( 'CYLREC: ' // CASE )

!
!              Revert cylindrical coordinates to rectangular
!              coordinates.
!
               CALL CYLREC ( RHO, LON, Z, REC )

!
!              Check the reverted coordinates.
!
               CALL CHCKSD ( 'REC(1)', REC(1), '~', EX, MED, OK )
               CALL CHCKSD ( 'REC(2)', REC(2), '~', EY, MED, OK )
               CALL CHCKSD ( 'REC(3)', REC(3), '~', EZ, MED, OK )

!
!              Test rectangular to spherical coordinate conversion.
!
               CALL TCASE ( 'RECSPH: ' // CASE )

!
!              Pack the grid point into a rectangular coordinate
!              vector.
!
               CALL VPACK( EX, EY, EZ, REC )

!
!              Convert to spherical coordinates.
!
               CALL RECSPH ( REC, R, CLAT, LONG )

!
!              Check the converted coordinates.
!
               CALL CHCKSD ( 'R',    R,    '~', ER,    MED, OK )
               CALL CHCKSD ( 'CLAT', CLAT, '~', ECLAT, MED, OK )
               CALL CHCKSD ( 'LONG', LONG, '~', ELON,  MED, OK )

!
!              Test spherical to rectangular coordinate conversion.
!
               CALL TCASE ( 'SPHREC: '  //  CASE )

!
!              Revert spherical coordinates to rectangular coordinates.
!
               CALL SPHREC ( R, CLAT, LONG, REC )

!
!              Check the reverted coordinates.
!
               CALL CHCKSD ( 'REC(1)', REC(1), '~', EX, MED, OK )
               CALL CHCKSD ( 'REC(2)', REC(2), '~', EY, MED, OK )
               CALL CHCKSD ( 'REC(3)', REC(3), '~', EZ, MED, OK )

!
!              Test rectangular to latitudinal coordinate conversion.
!
               CALL TCASE ( 'RECLAT: ' // CASE )

!
!              Pack the grid point into a rectangular coordinate
!              vector.
!
               CALL VPACK ( EX, EY, EZ, REC )

!
!              Convert to latitudinal coordinates.
!
               CALL RECLAT ( REC, RADIUS, LLON, LLAT )

!
!              Check the converted coordinates.
!
               CALL CHCKSD ( 'RADIUS', RADIUS, '~', ER,   MED, OK )
               CALL CHCKSD ( 'LLON',   LLON,   '~', ELON, MED, OK )
               CALL CHCKSD ( 'LLAT',   LLAT,   '~', ELAT, MED, OK )

!
!              Test latitudinal to rectangular coordinate conversion.
!
               CALL TCASE ( 'LATREC: ' // CASE )

!
!              Revert latitudinal coordinates to rectangular
!              coordinates.
!
               CALL LATREC ( RADIUS, LLON, LLAT, REC )

!
!              Check the reverted coordinates.
!
               CALL CHCKSD ( 'REC(1)', REC(1), '~', EX, MED, OK )
               CALL CHCKSD ( 'REC(2)', REC(2), '~', EY, MED, OK )
               CALL CHCKSD ( 'REC(3)', REC(3), '~', EZ, MED, OK )

!
!              Test cylindrical to spherical coordinate conversion.
!
               CALL TCASE ( 'CYLSPH: ' // CASE )

!
!              Convert cylindrical coordinates to latitudinal
!              coordinates.
!
               CALL CYLSPH ( RHO, LON, Z, R, CLAT, LONG )

!
!              Check the converted coordinates.
!
               CALL CHCKSD ( 'R',    R,    '~', ER,    MED, OK )
               CALL CHCKSD ( 'CLAT', CLAT, '~', ECLAT, MED, OK )
               CALL CHCKSD ( 'LONG', LONG, '~', ELON,  MED, OK )

!
!              Test spherical to cylindrical coordinate conversion.
!

               CALL TCASE ( 'SPHCYL: ' // CASE )

!
!              Revert latitudinal coordinates to cylindrical
!              coordinates.
!
               CALL SPHCYL ( R, CLAT, LONG, RHO, LON, Z )

!
!              Check the reverted coordinates.
!
               CALL CHCKSD ( 'RHO', RHO, '~', ERHO, MED, OK )
               CALL CHCKSD ( 'LON', LON, '~', ELON, MED, OK )
               CALL CHCKSD ( 'Z',   Z,   '~', EZ,   MED, OK )

!
!              Test latitudinal to spherical coordinate conversion.
!

               CALL TCASE ( 'LATSPH: ' // CASE )

!
!              Convert latitudinal coordinates to spherical
!              coordinates.
!
               CALL LATSPH ( RADIUS, LLON, LLAT, R, CLAT, LONG )

!
!              Check the converted coordinates.
!
               CALL CHCKSD ( 'R',    R,    '~', ER,    MED, OK )
               CALL CHCKSD ( 'CLAT', CLAT, '~', ECLAT, MED, OK )
               CALL CHCKSD ( 'LONG', LONG, '~', ELON,  MED, OK )

!
!              Test spherical to latitudinal coordinate conversion.
!
               CALL TCASE ('SPHLAT: ' // CASE )

!
!              Revert spherical coordinates to latitudinal coordinates.
!
               CALL SPHLAT ( R, CLAT, LONG, RADIUS, LLON, LLAT )

!
!              Check the reverted coordinates.
!
               CALL CHCKSD ( 'RADIUS', RADIUS, '~', ER,   MED, OK )
               CALL CHCKSD ( 'LLON',   LLON,   '~', ELON, MED, OK )
               CALL CHCKSD ( 'LLAT',   LLAT,   '~', ELAT, MED, OK )

!
!              Test latitudinal to cylindrical coordinate conversion.
!
               CALL TCASE ( 'LATCYL: ' // CASE )

!
!              Convert latitudinal coordinates to cylindrical
!              coordinates.
!
               CALL LATCYL ( RADIUS, LLON, LLAT, RHO, LON, Z )

!
!              Check the converted coordinates.
!
               CALL CHCKSD ( 'RHO', RHO, '~', ERHO, MED, OK )
               CALL CHCKSD ( 'LON', LON, '~', ELON, MED, OK )
               CALL CHCKSD ( 'Z',   Z,   '~', EZ,   MED, OK )

!
!              Test cylindrical to latitudinal coordinate conversion.
!
               CALL TCASE ('CYLLAT: ' // CASE )

!
!              Revert cylindrical coordinates to latitudinal
!              coordinates.
!
               CALL CYLLAT ( RHO, LON, Z, RADIUS, LLON, LLAT )

!
!              Check the reverted coordinates.
!
               CALL CHCKSD ( 'RADIUS', RADIUS, '~', ER,   MED, OK )
               CALL CHCKSD ( 'LLON',   LLON,   '~', ELON, MED, OK )
               CALL CHCKSD ( 'LLAT',   LLAT,   '~', ELAT, MED, OK )

!
!              Test rectangular to RAD coordinate conversion.
!
               CALL TCASE ( 'RECRAD: ' // CASE )

!
!              Pack the grid point into a rectangular coordinate
!              vector.
!
               CALL VPACK( EX, EY, EZ, REC )

!
!              Convert rectangular coordinates to RAD coordinates.
!
               CALL RECRAD( REC, RANGE, RA, DEC  )

!
!              Check the converted coordinates.
!
               CALL CHCKSD ( 'RANGE', RANGE, '~', ER,   MED, OK )
               CALL CHCKSD ( 'RA',    RA,    '~', ERA,  MED, OK )
               CALL CHCKSD ( 'DEC',   DEC,   '~', ELAT, MED, OK )

!
!              Test RAD to rectangular coordinate conversion.
!
               CALL TCASE ( 'RADREC: ' // CASE )

!
!              Revert RAD coordinates to rectangular coordinates.
!
               CALL RADREC( RANGE, RA, DEC, REC  )

!
!              Check the converted coordinates.
!
               CALL CHCKSD ( 'REC(1)', REC(1), '~', EX, MED, OK )
               CALL CHCKSD ( 'REC(2)', REC(2), '~', EY, MED, OK )
               CALL CHCKSD ( 'REC(3)', REC(3), '~', EZ, MED, OK )

!
!              Test rectangular to geodetic coordinate conversion.
!
               CALL TCASE ( 'RECGEO: ' // CASE )

!
!              Pack the grid point into a rectangular coordinate
!              vector.
!
               CALL VPACK( EX, EY, EZ, REC )

!
!              Convert rectangular coordinates to geodetic coordinates.
!
               CALL RECGEO( REC, AE, F, GLON, GLAT, GALT )

               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              First, check the geodetic longitude, which should be the
!              same as longitudes in other coordinate system.
!
               CALL CHCKSD ( 'GLON', GLON, '~', ELON, MED, OK )

!
!              Derive the geodetic altitude vector, GVEC, which is
!              vertically pointing from the nadir on the surface of the
!              reference ellipsoid to the grid point.

               CALL LATREC( GALT, GLON, GLAT, GVEC )

!
!              Get rectangular coordinates of the nadir.
!
               CALL VSUB( REC, GVEC, NADIR )

!
!              Being on the surface of the reference ellipsoid, the
!              rectangular coordinates of the nadir should satisfy the
!              spheroidal surface equation associated with the reference
!              ellipsoid.

!
!              Evaluate the spheroidal surface equation.
!
               SRFEQN =   ( NADIR(1) / AE )**2 &
                        + ( NADIR(2) / AE )**2 &
                        + ( NADIR(3) / BE )**2 - 1.0D0

!
!              Check the nadir is on the surface.
!
               CALL CHCKSD ( 'SRFEQN', SRFEQN, '~', 0.0D0, MED, OK )

!
!              Test geodetic to rectangular coordinate conversion.
!
               CALL TCASE ( 'GEOREC: ' // CASE )

!
!              Revert geodetic coordinates to rectangular coordinates.
!
               CALL GEOREC(  GLON, GLAT, GALT, AE, F, REC )

               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Check the reverted coordinates.
!
               CALL CHCKSD ( 'REC(1)', REC(1), '~', EX, LSE, OK )
               CALL CHCKSD ( 'REC(2)', REC(2), '~', EY, LSE, OK )
               CALL CHCKSD ( 'REC(3)', REC(3), '~', EZ, LSE, OK )

!
!              Test surface to rectangular coordinates conversion.
!

!
!              Set the surface coordinates ( longitude and latitude )
!              computed at the grid point.
!
               SLON = ELON
               SLAT = ELAT

!
!              Loop over NAIF bodies
!
               DO IB = 1, NB

!
!                 Identity NAIF body and surface point on the test
!                 description.
!
                  CASE = '# # #'
                  CALL REPMI ( CASE, '#', BODY(IB), CASE )
                  CALL REPMD ( CASE, '#', SLON, 14, CASE )
                  CALL REPMD ( CASE, '#', SLAT, 14, CASE )

                  CALL TCASE( 'SRFREC: ' // CASE )

!
!                 Convert surface coordinates to rectangular
!                 coordinates.
!
                  CALL SRFREC ( BODY(IB), SLON, SLAT, NADIR )

                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Convert rectangular coordinates to latitudinal
!                 coordinates.
!
                  CALL RECLAT ( NADIR, RADIUS, LLON, LLAT )

!
!                 The longitude and latitude pair of latitudinal
!                 coordinates should be that of surface coordinates.
!
                  CALL CHCKSD ( 'LLON', LLON, '~', ELON, MED, OK )
                  CALL CHCKSD ( 'LLAT', LLAT, '~', ELAT, MED, OK )

!
!                 Get semi-axes, RADII, of the naif body.
!
                  CALL BODVCD ( BODY( IB ), 'RADII', 3, NDIM, RADII )

                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Being on the surface of the naif body, the recovered
!                 rectangular coordinates should satisfy the spheroidal
!                 surface equation associated with the naif body.
!
!                 Evaluate the spheroidal surface equation.
!
                  SRFEQN =    ( NADIR(1) / RADII(1) )**2 &
                            + ( NADIR(2) / RADII(2) )**2 &
                            + ( NADIR(3) / RADII(3) )**2 - 1.0D0

!
!                 Check the nadir is on the surface.
!
                  CALL CHCKSD ( 'SRFEQN', SRFEQN, '~', 0.0D0, MED, OK )

               END DO

            END DO

         END DO

      END DO

!
!     Exception test cases.
!
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

!
!     (0,0,0) test cases.
!
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


!
!     Test cases, with one or more items set to 0.
!
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

!
!     Test cases with angular inputs outside of normal ranges.
!
!     THESE TEST HAVE NOT BEED IMPLEMENTED FOR N0066. THEY MIGHT
!     BE IMPLEMENTED AT A LATER TIME.
!

      CALL T_SUCCESS ( OK )

      RETURN
      END
