!$Procedure DGEODR ( Derivative of geodetic w.r.t. rectangular )

      subroutine dgeodr ( x, y, z, re, f, jacobi )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Compute the Jacobian matrix of the transformation from
!     rectangular to geodetic coordinates.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     COORDINATES
!     DERIVATIVES
!     MATRIX
!
! $ Declarations

      implicit none

      real(wp)              x
      real(wp)              y
      real(wp)              z
      real(wp)              re
      real(wp)              f
      real(wp)              jacobi ( 3, 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     X          I   X-coordinate of point.
!     Y          I   Y-coordinate of point.
!     Z          I   Z-coordinate of point.
!     RE         I   Equatorial radius of the reference spheroid.
!     F          I   Flattening coefficient.
!     JACOBI     O   Matrix of partial derivatives.
!
! $ Detailed_Input
!
!     X,
!     Y,
!     Z        are the rectangular coordinates of the point at
!              which the Jacobian of the map from rectangular
!              to geodetic coordinates is desired.
!
!     RE       is the equatorial radius of the reference spheroid.
!
!     F        is the flattening coefficient = (RE-RP) / RE,  where RP
!              is the polar radius of the spheroid. (More importantly
!              RP = RE*(1-F).)
!
! $ Detailed_Output
!
!     JACOBI   is the matrix of partial derivatives of the conversion
!              between rectangular and geodetic coordinates. It
!              has the form
!
!                  .-                               -.
!                  |  DLONG/DX   DLONG/DY  DLONG/DZ  |
!                  |  DLAT/DX    DLAT/DY   DLAT/DZ   |
!                  |  DALT/DX    DALT/DY   DALT/DZ   |
!                  `-                               -'
!
!               evaluated at the input values of X, Y, and Z.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input point is on the z-axis (X = 0 and Y = 0), the
!         Jacobian is undefined, the error SPICE(POINTONZAXIS) is
!         signaled.
!
!     2)  If the flattening coefficient is greater than or equal to
!         one, the error SPICE(VALUEOUTOFRANGE) is signaled.
!
!     3)  If the equatorial radius is not positive, the error
!         SPICE(BADRADIUS) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     When performing vector calculations with velocities it is
!     usually most convenient to work in rectangular coordinates.
!     However, once the vector manipulations have been performed,
!     it is often desirable to convert the rectangular representations
!     into geodetic coordinates to gain insights about phenomena
!     in this coordinate frame.
!
!     To transform rectangular velocities to derivatives of coordinates
!     in a geodetic system, one uses the Jacobian of the transformation
!     between the two systems.
!
!     Given a state in rectangular coordinates
!
!        ( x, y, z, dx, dy, dz )
!
!     the velocity in geodetic coordinates is given by the matrix
!     equation:
!                          t          |                     t
!        (dlon, dlat, dalt)   = JACOBI|       * (dx, dy, dz)
!                                     |(x,y,z)
!
!     This routine computes the matrix
!
!              |
!        JACOBI|
!              |(x, y, z)
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Find the geodetic state of the earth as seen from
!        Mars in the IAU_MARS reference frame at January 1, 2005 TDB.
!        Map this state back to rectangular coordinates as a check.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: dgeodr_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                     Contents
!              ---------                     --------
!              de421.bsp                     Planetary ephemeris
!              pck00010.tpc                  Planet orientation and
!                                            radii
!              naif0009.tls                  Leapseconds
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'pck00010.tpc',
!                                  'naif0009.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM DGEODR_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      RPD
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         FMT1
!              PARAMETER           ( FMT1 = '(A,E18.8)' )
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      ALT
!              DOUBLE PRECISION      DRECTN ( 3 )
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      F
!              DOUBLE PRECISION      JACOBI ( 3, 3 )
!              DOUBLE PRECISION      LAT
!              DOUBLE PRECISION      LON
!              DOUBLE PRECISION      LT
!              DOUBLE PRECISION      GEOVEL ( 3 )
!              DOUBLE PRECISION      RADII  ( 3 )
!              DOUBLE PRECISION      RE
!              DOUBLE PRECISION      RECTAN ( 3 )
!              DOUBLE PRECISION      RP
!              DOUBLE PRECISION      STATE  ( 6 )
!
!              INTEGER               N
!
!        C
!        C     Load SPK, PCK, and LSK kernels, use a meta kernel for
!        C     convenience.
!        C
!              CALL FURNSH ( 'dgeodr_ex1.tm' )
!
!        C
!        C     Look up the radii for Mars.  Although we
!        C     omit it here, we could first call BADKPV
!        C     to make sure the variable BODY499_RADII
!        C     has three elements and numeric data type.
!        C     If the variable is not present in the kernel
!        C     pool, BODVRD will signal an error.
!        C
!              CALL BODVRD ( 'MARS', 'RADII', 3, N, RADII )
!
!        C
!        C     Compute flattening coefficient.
!        C
!              RE  =  RADII(1)
!              RP  =  RADII(3)
!              F   =  ( RE - RP ) / RE
!
!        C
!        C     Look up the apparent state of earth as seen from Mars at
!        C     January 1, 2005 TDB, relative to the IAU_MARS reference
!        C     frame.
!        C
!              CALL STR2ET ( 'January 1, 2005 TDB', ET )
!
!              CALL SPKEZR ( 'Earth', ET,    'IAU_MARS', 'LT+S',
!             .              'Mars',  STATE, LT                )
!
!        C
!        C     Convert position to geodetic coordinates.
!        C
!              CALL RECGEO ( STATE, RE, F, LON, LAT, ALT )
!
!        C
!        C     Convert velocity to geodetic coordinates.
!        C
!
!              CALL DGEODR (  STATE(1), STATE(2), STATE(3),
!             .               RE,       F,        JACOBI   )
!
!              CALL MXV ( JACOBI, STATE(4), GEOVEL )
!
!        C
!        C     As a check, convert the geodetic state back to
!        C     rectangular coordinates.
!        C
!              CALL GEOREC ( LON, LAT, ALT, RE, F, RECTAN )
!
!              CALL DRDGEO ( LON, LAT, ALT, RE, F, JACOBI )
!
!              CALL MXV ( JACOBI, GEOVEL, DRECTN )
!
!
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Rectangular coordinates:'
!              WRITE(*,*) ' '
!              WRITE(*,FMT1) '  X (km)                 = ', STATE(1)
!              WRITE(*,FMT1) '  Y (km)                 = ', STATE(2)
!              WRITE(*,FMT1) '  Z (km)                 = ', STATE(3)
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Rectangular velocity:'
!              WRITE(*,*) ' '
!              WRITE(*,FMT1) '  dX/dt (km/s)           = ', STATE(4)
!              WRITE(*,FMT1) '  dY/dt (km/s)           = ', STATE(5)
!              WRITE(*,FMT1) '  dZ/dt (km/s)           = ', STATE(6)
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Ellipsoid shape parameters: '
!              WRITE(*,*) ' '
!              WRITE(*,FMT1) '  Equatorial radius (km) = ', RE
!              WRITE(*,FMT1) '  Polar radius      (km) = ', RP
!              WRITE(*,FMT1) '  Flattening coefficient = ', F
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Geodetic coordinates:'
!              WRITE(*,*) ' '
!              WRITE(*,FMT1) '  Longitude (deg)        = ', LON / RPD()
!              WRITE(*,FMT1) '  Latitude  (deg)        = ', LAT / RPD()
!              WRITE(*,FMT1) '  Altitude  (km)         = ', ALT
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Geodetic velocity:'
!              WRITE(*,*) ' '
!              WRITE(*,FMT1) '  d Longitude/dt (deg/s) = ',
!             .                                         GEOVEL(1)/RPD()
!              WRITE(*,FMT1) '  d Latitude/dt  (deg/s) = ',
!             .                                         GEOVEL(2)/RPD()
!              WRITE(*,FMT1) '  d Altitude/dt  (km/s)  = ', GEOVEL(3)
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Rectangular coordinates from inverse ' //
!             .           'mapping:'
!              WRITE(*,*) ' '
!              WRITE(*,FMT1) '  X (km)                 = ', RECTAN(1)
!              WRITE(*,FMT1) '  Y (km)                 = ', RECTAN(2)
!              WRITE(*,FMT1) '  Z (km)                 = ', RECTAN(3)
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Rectangular velocity from inverse mapping:'
!              WRITE(*,*) ' '
!              WRITE(*,FMT1) '  dX/dt (km/s)           = ', DRECTN(1)
!              WRITE(*,FMT1) '  dY/dt (km/s)           = ', DRECTN(2)
!              WRITE(*,FMT1) '  dZ/dt (km/s)           = ', DRECTN(3)
!              WRITE(*,*) ' '
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Rectangular coordinates:
!
!          X (km)                 =    -0.76096183E+08
!          Y (km)                 =     0.32436380E+09
!          Z (km)                 =     0.47470484E+08
!
!         Rectangular velocity:
!
!          dX/dt (km/s)           =     0.22952075E+05
!          dY/dt (km/s)           =     0.53760111E+04
!          dZ/dt (km/s)           =    -0.20881149E+02
!
!         Ellipsoid shape parameters:
!
!          Equatorial radius (km) =     0.33961900E+04
!          Polar radius      (km) =     0.33762000E+04
!          Flattening coefficient =     0.58860076E-02
!
!         Geodetic coordinates:
!
!          Longitude (deg)        =     0.10320290E+03
!          Latitude  (deg)        =     0.81089876E+01
!          Altitude  (km)         =     0.33653182E+09
!
!         Geodetic velocity:
!
!          d Longitude/dt (deg/s) =    -0.40539288E-02
!          d Latitude/dt  (deg/s) =    -0.33189934E-05
!          d Altitude/dt  (km/s)  =    -0.11211601E+02
!
!         Rectangular coordinates from inverse mapping:
!
!          X (km)                 =    -0.76096183E+08
!          Y (km)                 =     0.32436380E+09
!          Z (km)                 =     0.47470484E+08
!
!         Rectangular velocity from inverse mapping:
!
!          dX/dt (km/s)           =     0.22952075E+05
!          dY/dt (km/s)           =     0.53760111E+04
!          dZ/dt (km/s)           =    -0.20881149E+02
!
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
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 26-OCT-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example.
!
!-    SPICELIB Version 1.0.0, 20-JUL-2001 (WLT)
!
!-&


! $ Index_Entries
!
!     Jacobian of geodetic  w.r.t. rectangular coordinates
!
!-&


!
!     SPICELIB functions
!
      logical               return

!
!     Local variables
!
      real(wp)              rectan  ( 3 )
      real(wp)              long
      real(wp)              lat
      real(wp)              alt
      real(wp)              injacb  ( 3, 3 )


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     If the flattening coefficient is greater than one, the polar
!     radius computed below is negative. If it's equal to one, the
!     polar radius is zero. Either case is a problem, so signal an
!     error and check out.
!
      if ( f >= 1.0_wp ) then

          call setmsg ( 'Flattening coefficient was *.'  )
          call errdp  ( '*', f                           )
          call sigerr ( 'SPICE(VALUEOUTOFRANGE)'         )
          return

      end if

      if ( re <= 0.0_wp ) then

          call setmsg ( 'Equatorial Radius <= 0.0D0. RE = *' )
          call errdp  ( '*', re                              )
          call sigerr ( 'SPICE(BADRADIUS)'                   )
          return

      end if

!
!     There is a singularity of the Jacobian for points on the z-axis.
!
      if ( ( x == 0 ) .and. ( y == 0 ) ) then

         call setmsg ( 'The Jacobian of the transformation from '     // &
                       'rectangular to geodetic coordinates '         // &
                       'is not defined for points on the z-axis.'      )
         call sigerr ( 'SPICE(POINTONZAXIS)'                           )
         return

      end if

!
!     We will get the Jacobian of rectangular to geodetic by
!     implicit differentiation.
!
!     First move the X,Y and Z coordinates into a vector.
!
      call vpack  ( x, y, z, rectan )

!
!     Convert from rectangular to geodetic coordinates.
!
      call recgeo ( rectan, re, f, long, lat, alt  )

!
!     Get the Jacobian of the transformation from geodetic to
!     rectangular coordinates at LONG, LAT, ALT.
!
      call drdgeo ( long,  lat, alt, re, f, injacb )

!
!     Now invert INJACB to get the Jacobian of the transformation
!     from rectangular to geodetic coordinates.
!
      call invort ( injacb, jacobi )

      return
      end
