!$Procedure RECPGR ( Rectangular to planetographic )

      subroutine recpgr ( body, rectan, re, f, lon, lat, alt )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Convert rectangular coordinates to planetographic coordinates.
!
! $ Required_Reading
!
!     KERNEL
!     NAIF_IDS
!     PCK
!
! $ Keywords
!
!     CONVERSION
!     COORDINATES
!     GEOMETRY
!     MATH
!
! $ Declarations

      implicit none

      include               'zzctr.inc'

      character(*)          body
      real(wp)              rectan ( 3 )
      real(wp)              re
      real(wp)              f
      real(wp)              lon
      real(wp)              lat
      real(wp)              alt

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   Body with which coordinate system is associated.
!     RECTAN     I   Rectangular coordinates of a point.
!     RE         I   Equatorial radius of the reference spheroid.
!     F          I   Flattening coefficient.
!     LON        O   Planetographic longitude of the point (radians).
!     LAT        O   Planetographic latitude of the point (radians).
!     ALT        O   Altitude of the point above reference spheroid.
!
! $ Detailed_Input
!
!     BODY     is the name of the body with which the planetographic
!              coordinate system is associated.
!
!              BODY is used by this routine to look up from the kernel
!              pool the prime meridian rate coefficient giving the
!              body's spin sense. See the $Files and $Particulars header
!              sections below for details.
!
!     RECTAN   are the rectangular coordinates of a point. Units are
!              arbitrary, except that the input RE must be expressed in
!              the same units.
!
!     RE       is the equatorial radius of a reference spheroid. This
!              spheroid is a volume of revolution: its horizontal cross
!              sections are circular. The shape of the spheroid is
!              defined by an equatorial radius RE and a polar radius RP.
!              Units of RE must match those of RECTAN.
!
!     F        is the flattening coefficient =
!
!                 (RE-RP) / RE
!
!              where RP is the polar radius of the spheroid, and the
!              units of RP match those of RE.
!
! $ Detailed_Output
!
!     LON      is the planetographic longitude of the input point. This
!              is the angle between the prime meridian and the meridian
!              containing RECTAN. For bodies having prograde (aka
!              direct) rotation, the direction of increasing longitude
!              is positive west: from the +X axis of the rectangular
!              coordinate system toward the -Y axis. For bodies having
!              retrograde rotation, the direction of increasing
!              longitude is positive east: from the +X axis toward the
!              +Y axis.
!
!              The earth, moon, and sun are exceptions: planetographic
!              longitude is measured positive east for these bodies.
!
!              The default interpretation of longitude by this and the
!              other planetographic coordinate conversion routines can
!              be overridden; see the discussion in $Particulars below
!              for details.
!
!              LON is output in radians. The nominal range of LON is
!              given by:
!
!                 0  <  LON  <  2*pi
!                    -
!
!              However, round-off error could cause LON to equal 2*pi.
!
!     LAT      is the planetographic latitude of the input point. For a
!              point P on the reference spheroid, this is the angle
!              between the XY plane and the outward normal vector at P.
!              For a point P not on the reference spheroid, the
!              planetographic latitude is that of the closest point to P
!              on the spheroid.
!
!              LAT is output in radians. The range of LAT is given by:
!
!                 -pi/2  <  LAT  <  pi/2
!                        -       -
!
!     ALT      is the altitude of point above the reference spheroid.
!
!              The units associated with ALT are those associated with
!              the input RECTAN and RE.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the body name BODY cannot be mapped to a NAIF ID code,
!         and if BODY is not a string representation of an integer,
!         the error SPICE(IDCODENOTFOUND) is signaled.
!
!     2)  If the kernel variable
!
!            BODY<ID code>_PGR_POSITIVE_LON
!
!         is present in the kernel pool but has a value other than one
!         of
!
!             'EAST'
!             'WEST'
!
!         the error SPICE(INVALIDOPTION) is signaled. Case
!         and blanks are ignored when these values are interpreted.
!
!     3)  If polynomial coefficients for the prime meridian of BODY
!         are not available in the kernel pool, and if the kernel
!         variable BODY<ID code>_PGR_POSITIVE_LON is not present in
!         the kernel pool, the error SPICE(MISSINGDATA) is signaled.
!
!     4)  If the equatorial radius is non-positive, the error
!         SPICE(VALUEOUTOFRANGE) is signaled.
!
!     5)  If the flattening coefficient is greater than or equal to one,
!         the error SPICE(VALUEOUTOFRANGE) is signaled.
!
!     6)  For points inside the reference ellipsoid, the nearest point
!         on the ellipsoid to RECTAN may not be unique, so latitude may
!         not be well-defined.
!
! $ Files
!
!     This routine expects a kernel variable giving BODY's prime
!     meridian angle as a function of time to be available in the
!     kernel pool. Normally this item is provided by loading a PCK
!     file. The required kernel variable is named
!
!        BODY<body ID>_PM
!
!     where <body ID> represents a string containing the NAIF integer
!     ID code for BODY. For example, if BODY is 'JUPITER', then
!     the name of the kernel variable containing the prime meridian
!     angle coefficients is
!
!        BODY599_PM
!
!     The optional kernel variable
!
!        BODY<body ID>_PGR_POSITIVE_LON
!
!     also is normally defined via loading a text kernel. When this
!     variable is present in the kernel pool, the prime meridian
!     coefficients for BODY are not required by this routine. See the
!     $Particulars section for details.
!
! $ Particulars
!
!     Given the body-fixed rectangular coordinates of a point, this
!     routine returns the planetographic coordinates of the point. The
!     body-fixed rectangular frame is that having the X-axis pass
!     through the 0 degree latitude 0 degree longitude direction, the
!     Z-axis pass through the 90 degree latitude direction, and the
!     Y-axis equal to the cross product of the unit Z-axis and X-axis
!     vectors.
!
!     The planetographic definition of latitude is identical to the
!     planetodetic (also called "geodetic" in SPICE documentation)
!     definition. In the planetographic coordinate system, latitude is
!     defined using a reference spheroid. The spheroid is
!     characterized by an equatorial radius and a polar radius. For a
!     point P on the spheroid, latitude is defined as the angle between
!     the X-Y plane and the outward surface normal at P. For a point P
!     off the spheroid, latitude is defined as the latitude of the
!     nearest point to P on the spheroid. Note if P is an interior
!     point, for example, if P is at the center of the spheroid, there
!     may not be a unique nearest point to P.
!
!     In the planetographic coordinate system, longitude is defined
!     using the spin sense of the body. Longitude is positive to the
!     west if the spin is prograde and positive to the east if the spin
!     is retrograde. The spin sense is given by the sign of the first
!     degree term of the time-dependent polynomial for the body's prime
!     meridian Euler angle "W":  the spin is retrograde if this term is
!     negative and prograde otherwise. For the sun, planets, most
!     natural satellites, and selected asteroids, the polynomial
!     expression for W may be found in a SPICE PCK kernel.
!
!     The earth, moon, and sun are exceptions: planetographic longitude
!     is measured positive east for these bodies.
!
!     If you wish to override the default sense of positive longitude
!     for a particular body, you can do so by defining the kernel
!     variable
!
!        BODY<body ID>_PGR_POSITIVE_LON
!
!     where <body ID> represents the NAIF ID code of the body. This
!     variable may be assigned either of the values
!
!        'WEST'
!        'EAST'
!
!     For example, you can have this routine treat the longitude
!     of the earth as increasing to the west using the kernel
!     variable assignment
!
!        BODY399_PGR_POSITIVE_LON = 'WEST'
!
!     Normally such assignments are made by placing them in a text
!     kernel and loading that kernel via FURNSH.
!
!     The definition of this kernel variable controls the behavior of
!     the SPICELIB planetographic routines
!
!        PGRREC
!        RECPGR
!        DPGRDR
!        DRDPGR
!
!     It does not affect the other SPICELIB coordinate conversion
!     routines.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Find the planetographic coordinates of the point having Mars
!        rectangular coordinates:
!
!           X (km) =      0.0
!           Y (km) =  -2620.678914818178
!           Z (km) =   2592.408908856967
!
!        (These input values have been chosen to create "simple" output
!        values.)
!
!        Use the PCK kernel below to load the required triaxial
!        ellipsoidal shape model and orientation data for Mars.
!
!           pck00008.tpc
!
!        Example code begins here.
!
!              PROGRAM RECPGR_EX1
!              IMPLICIT NONE
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      RPD
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      ALT
!              DOUBLE PRECISION      F
!              DOUBLE PRECISION      LAT
!              DOUBLE PRECISION      LON
!              DOUBLE PRECISION      RADII  ( 3 )
!              DOUBLE PRECISION      RE
!              DOUBLE PRECISION      RECTAN ( 3 )
!              DOUBLE PRECISION      RP
!
!              INTEGER               N
!        C
!        C     Load a PCK file containing a triaxial
!        C     ellipsoidal shape model and orientation
!        C     data for Mars.
!        C
!              CALL FURNSH ( 'pck00008.tpc' )
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
!        C     Do the conversion.
!        C
!              RECTAN(1) =      0.D0
!              RECTAN(2) =  -2620.678914818178D0
!              RECTAN(3) =   2592.408908856967D0
!
!              CALL RECPGR ( 'MARS', RECTAN, RE, F, LON, LAT, ALT )
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Rectangular coordinates:'
!              WRITE (*,*) ' '
!              WRITE (*,*) '  X (km)                 = ', RECTAN(1)
!              WRITE (*,*) '  Y (km)                 = ', RECTAN(2)
!              WRITE (*,*) '  Z (km)                 = ', RECTAN(3)
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Ellipsoid shape parameters: '
!              WRITE (*,*) ' '
!              WRITE (*,*) '  Equatorial radius (km) = ', RE
!              WRITE (*,*) '  Polar radius      (km) = ', RP
!              WRITE (*,*) '  Flattening coefficient = ', F
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Planetographic coordinates:'
!              WRITE (*,*) ' '
!              WRITE (*,*) '  Longitude (deg)        = ', LON / RPD()
!              WRITE (*,*) '  Latitude  (deg)        = ', LAT / RPD()
!              WRITE (*,*) '  Altitude  (km)         = ', ALT
!              WRITE (*,*) ' '
!
!              END
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!         Rectangular coordinates:
!
!           X (km)                 =    0.0000000000000000
!           Y (km)                 =   -2620.6789148181779
!           Z (km)                 =    2592.4089088569672
!
!         Ellipsoid shape parameters:
!
!           Equatorial radius (km) =    3396.1900000000001
!           Polar radius      (km) =    3376.1999999999998
!           Flattening coefficient =    5.8860075555255261E-003
!
!         Planetographic coordinates:
!
!           Longitude (deg)        =    90.000000000000000
!           Latitude  (deg)        =    45.000000000000014
!           Altitude  (km)         =    300.00000000000057
!
!     2) Below is a table showing a variety of rectangular coordinates
!        and the corresponding Mars planetographic coordinates. The
!        values are computed using the reference spheroid having radii
!
!           Equatorial radius:    3396.190
!           Polar radius:         3376.200
!
!        Note:  the values shown above may not be current or suitable
!               for your application.
!
!        Corresponding rectangular and planetographic coordinates are
!        listed to three decimal places.
!
!        RECTAN(1)  RECTAN(2)  RECTAN(3)       LON       LAT        ALT
!        --------------------------------------------------------------
!         3396.190      0.000      0.000     0.000     0.000      0.000
!        -3396.190      0.000      0.000   180.000     0.000      0.000
!        -3406.190      0.000      0.000   180.000     0.000     10.000
!        -3386.190      0.000      0.000   180.000     0.000    -10.000
!            0.000  -3396.190      0.000    90.000     0.000      0.000
!            0.000   3396.190      0.000   270.000     0.000      0.000
!            0.000      0.000   3376.200     0.000    90.000      0.000
!            0.000      0.000  -3376.200     0.000   -90.000      0.000
!            0.000      0.000      0.000     0.000    90.000  -3376.200
!
!     3) Below we show the analogous relationships for the earth,
!        using the reference ellipsoid radii
!
!           Equatorial radius:    6378.140
!           Polar radius:         6356.750
!
!        Note the change in longitudes for points on the +/- Y axis
!        for the earth vs the Mars values.
!
!        RECTAN(1)  RECTAN(2)  RECTAN(3)     LON       LAT        ALT
!        --------------------------------------------------------------
!         6378.140      0.000      0.000     0.000     0.000      0.000
!        -6378.140      0.000      0.000   180.000     0.000      0.000
!        -6388.140      0.000      0.000   180.000     0.000     10.000
!        -6368.140      0.000      0.000   180.000     0.000    -10.000
!            0.000  -6378.140      0.000   270.000     0.000      0.000
!            0.000   6378.140      0.000    90.000     0.000      0.000
!            0.000      0.000   6356.750     0.000    90.000      0.000
!            0.000      0.000  -6356.750     0.000   -90.000      0.000
!            0.000      0.000      0.000     0.000    90.000  -6356.750
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
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 06-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 21-SEP-2013 (BVS)
!
!        Updated to save the input body name and ZZBODTRN state
!        counter and to do name-ID conversion only if the counter
!        has changed.
!
!        Updated to call LJUCRS instead of CMPRSS/UCASE.
!
!-    SPICELIB Version 1.0.1, 23-JAN-2008 (EDW)
!
!        Corrected typo in LAT range description, from:
!
!                   -pi/2  <  LAT  <  pi
!                          -       -
!
!        to:
!
!                   -pi/2  <  LAT  <  pi/2
!                          -       -
!
!-    SPICELIB Version 1.0.0, 26-DEC-2004 (CHA) (NJB) (HAN) (BVS) (WLT)
!
!-&

! $ Index_Entries
!
!     convert rectangular to planetographic coordinates
!
!-&

!
!     SPICELIB functions
!
      real(wp)              brcktd
      real(wp)              twopi

      integer(ip)           plnsns

      logical               return

!
!     Local parameters
!
      character(*), parameter :: pmtmp = 'BODY#_PM'

      character(*), parameter :: ovrtmp = 'BODY#_PGR_POSITIVE_LON'

      integer(ip), parameter :: earth = 399

      integer(ip), parameter :: kvnmln = 32

      integer(ip), parameter :: lnsize = 80

      integer(ip), parameter :: moon = 301

      integer(ip), parameter :: sensln = 4

      integer(ip), parameter :: sun = 10

!
!     Saved body name length.
!
      integer(ip), parameter :: maxl = 36

!
!     Local variables
!
      character(lnsize)     kvalue
      character(kvnmln)     pmkvar
      character(sensln)     pgrlon

      integer(ip)           bodyid
      integer(ip)           n
      integer(ip)           sense

      logical               found

!
!     Saved name/ID item declarations.
!
      integer(ip)           svctr1 ( ctrsiz )
      character(maxl)       svbody
      integer(ip)           svbdid
      logical               svfnd1

      logical               first

!
!     Saved name/ID items.
!
      save                  svctr1
      save                  svbody
      save                  svbdid
      save                  svfnd1

      save                  first

!
!     Initial values.
!
      data                  first   / .true. /

!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counter.
!
         call zzctruin( svctr1 )

         first = .false.

      end if

!
!     Convert the body name to an ID code.
!
      call zzbods2c ( svctr1, svbody, svbdid, svfnd1, &
                      body, bodyid, found    )

      if ( .not. found ) then

         call setmsg ( 'The value of the input argument BODY is #, ' &
         //            'this is not a recognized name of an ' &
         //            'ephemeris object. The cause of this ' &
         //            'problem may be that you need an updated ' &
         //            'version of the SPICE Toolkit. '           )
         call errch  ( '#', body                                  )
         call sigerr ( 'SPICE(IDCODENOTFOUND)'                    )
         return

      end if

!
!     The equatorial radius must be positive. If not, signal an error.
!
      if ( re <= 0.0_wp ) then

         call setmsg ( 'Equatorial radius was #.' )
         call errdp  ( '#', re                    )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'   )
         return

      end if

!
!     If the flattening coefficient is greater than 1, the polar radius
!     is negative. If F is equal to 1, the polar radius is zero. Either
!     case is a problem, so signal an error.
!
      if ( f >= 1.0_wp ) then

         call setmsg ( 'Flattening coefficient was #.'  )
         call errdp  ( '#', f                           )
         call sigerr ( 'SPICE(VALUEOUTOFRANGE)'         )
         return

      end if

!
!     Look up the longitude sense override variable from the
!     kernel pool.
!
      call repmi  ( ovrtmp, '#',     bodyid, pmkvar )
      call gcpool ( pmkvar, 1, 1, n, kvalue, found  )

      if ( found ) then
!
!        Make sure we recognize the value of PGRLON.
!
         call ljucrs ( 0, kvalue, pgrlon )

         if ( pgrlon == 'EAST' ) then

            sense =  1

         else if ( pgrlon == 'WEST' ) then

            sense = -1

         else

            call setmsg ( 'Kernel variable # may have the values ' // &
                          'EAST or WEST.  Actual value was #.'    )
            call errch  ( '#', pmkvar                             )
            call errch  ( '#', kvalue                             )
            call sigerr ( 'SPICE(INVALIDOPTION)'                  )
            return

         end if

      else
!
!        Look up the spin sense of the body's prime meridian.
!
         sense = plnsns ( bodyid )

!
!        If the required prime meridian rate was not available,
!        PLNSNS returns the code 0.  Here we consider this situation
!        to be an error.
!
         if ( sense == 0 ) then

            call repmi  ( pmtmp, '#', bodyid, pmkvar )

            call setmsg ( 'Prime meridian rate coefficient defined ' // &
                          'by kernel variable # is required but '    // &
                          'not available for body #. '               )
            call errch  ( '#', pmkvar                                )
            call errch  ( '#', body                                  )
            call sigerr ( 'SPICE(MISSINGDATA)'                       )
            return

         end if

!
!        Handle the special cases:  earth, moon, and sun.
!
         if (      ( bodyid == earth ) &
              .or. ( bodyid == moon  ) &
              .or. ( bodyid == sun   )  ) then

            sense = 1

         end if

      end if

!
!     At this point, SENSE is set to +/- 1.
!
!     Convert the input coordinates first to geodetic coordinates.
!
      call recgeo ( rectan, re, f, lon, lat, alt )

!     Adjust the longitude according to the sense of the body's
!     spin, or according to the override value if one is provided.
!
      lon = sense * lon

!
!     Convert the longitude from the range (-pi, pi] to [0, 2*pi),
!     the latter being the range of planetographic longitude.
!
      if ( lon < 0.0_wp ) then

         lon = lon + twopi()

      end if

!
!     Make sure round-off error doesn't take LON out of range.
!
      lon = brcktd (  lon,   0.0_wp,  twopi()  )

      return
      end
