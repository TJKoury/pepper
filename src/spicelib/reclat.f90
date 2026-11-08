!$Procedure RECLAT ( Rectangular to latitudinal coordinates )

      subroutine reclat ( rectan, radius, lon, lat )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Convert from rectangular coordinates to latitudinal coordinates.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONVERSION
!     COORDINATES
!
! $ Declarations

      implicit none

      real(wp)           rectan ( 3 )
      real(wp)           radius
      real(wp)           lon
      real(wp)           lat

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     RECTAN     I   Rectangular coordinates of the point.
!     RADIUS     O   Distance of a point from the origin.
!     LON        O   Longitude of point in radians.
!     LAT        O   Latitude of point in radians.
!
! $ Detailed_Input
!
!     RECTAN   are the rectangular coordinates of a point.
!
! $ Detailed_Output
!
!     RADIUS   is the distance of a point from the origin.
!
!              The units associated with RADIUS are those
!              associated with the input RECTAN.
!
!     LON      is the longitude of the input point. This is the
!              angle between the prime meridian and the meridian
!              containing the point. The direction of increasing
!              longitude is from the +X axis towards the +Y axis.
!
!              LON is output in radians. The range of LON is
!              [ -pi, pi].
!
!     LAT      is the latitude of the input point. This is the angle
!              from the XY plane of the ray from the origin through
!              the point.
!
!              LAT is output in radians. The range of LAT is
!              [-pi/2, pi/2].
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the X and Y components of RECTAN are both zero, the
!         longitude is set to zero.
!
!     2)  If RECTAN is the zero vector, longitude and latitude are
!         both set to zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine returns the latitudinal coordinates of a point
!     whose position is input in rectangular coordinates.
!
!     Latitudinal coordinates are defined by a distance from a central
!     reference point, an angle from a reference meridian, and an angle
!     above the equator of a sphere centered at the central reference
!     point.
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Compute the latitudinal coordinates of the position of the
!        Moon as seen from the Earth, and convert them to rectangular
!        coordinates.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: reclat_ex1.tm
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
!              naif0012.tls                  Leapseconds
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'naif0012.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM RECLAT_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      DPR
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         FMT1
!              PARAMETER           ( FMT1 = '(A,F20.8)' )
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      LAT
!              DOUBLE PRECISION      LON
!              DOUBLE PRECISION      LT
!              DOUBLE PRECISION      POS    ( 3 )
!              DOUBLE PRECISION      RADIUS
!              DOUBLE PRECISION      RECTAN ( 3 )
!
!        C
!        C     Load SPK and LSK kernels, use a meta kernel for
!        C     convenience.
!        C
!              CALL FURNSH ( 'reclat_ex1.tm' )
!
!        C
!        C     Look up the geometric state of the Moon as seen from
!        C     the Earth at 2017 Mar 20, relative to the J2000
!        C     reference frame.
!        C
!              CALL STR2ET ( '2017 Mar 20', ET )
!
!              CALL SPKPOS ( 'Moon',  ET,  'J2000', 'NONE',
!             .              'Earth', POS, LT               )
!
!        C
!        C     Convert the position vector POS to latitudinal
!        C     coordinates.
!        C
!              CALL RECLAT ( POS, RADIUS, LON, LAT )
!
!        C
!        C     Convert the latitudinal to rectangular coordinates.
!        C
!
!              CALL LATREC ( RADIUS, LON, LAT, RECTAN )
!
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Original rectangular coordinates:'
!              WRITE(*,*) ' '
!              WRITE(*,FMT1) '  X          (km): ', POS(1)
!              WRITE(*,FMT1) '  Y          (km): ', POS(2)
!              WRITE(*,FMT1) '  Z          (km): ', POS(3)
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Latitudinal coordinates:'
!              WRITE(*,*) ' '
!              WRITE(*,FMT1) '  Radius     (km): ', RADIUS
!              WRITE(*,FMT1) '  Longitude (deg): ', LON*DPR()
!              WRITE(*,FMT1) '  Latitude  (deg): ', LAT*DPR()
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Rectangular coordinates from LATREC:'
!              WRITE(*,*) ' '
!              WRITE(*,FMT1) '  X          (km): ', RECTAN(1)
!              WRITE(*,FMT1) '  Y          (km): ', RECTAN(2)
!              WRITE(*,FMT1) '  Z          (km): ', RECTAN(3)
!              WRITE(*,*) ' '
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Original rectangular coordinates:
!
!          X          (km):      -55658.44323296
!          Y          (km):     -379226.32931475
!          Z          (km):     -126505.93063865
!
!         Latitudinal coordinates:
!
!          Radius     (km):      403626.33912495
!          Longitude (deg):         -98.34959789
!          Latitude  (deg):         -18.26566077
!
!         Rectangular coordinates from LATREC:
!
!          X          (km):      -55658.44323296
!          Y          (km):     -379226.32931475
!          Z          (km):     -126505.93063865
!
!
!     2) Create a table showing a variety of rectangular coordinates
!        and the corresponding latitudinal coordinates.
!
!        Corresponding rectangular and latitudinal coordinates are
!        listed to three decimal places. Output angles are in degrees.
!
!
!        Example code begins here.
!
!
!              PROGRAM RECLAT_EX2
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      DPR
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               NREC
!              PARAMETER           ( NREC = 11 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      LAT
!              DOUBLE PRECISION      LON
!              DOUBLE PRECISION      RADIUS
!              DOUBLE PRECISION      RECTAN ( 3, NREC )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define the input rectangular coordinates.
!        C
!              DATA                 RECTAN /
!             .                  0.D0,         0.D0,         0.D0,
!             .                  1.D0,         0.D0,         0.D0,
!             .                  0.D0,         1.D0,         0.D0,
!             .                  0.D0,         0.D0,         1.D0,
!             .                 -1.D0,         0.D0,         0.D0,
!             .                  0.D0,        -1.D0,         0.D0,
!             .                  0.D0,         0.D0,        -1.D0,
!             .                  1.D0,         1.D0,         0.D0,
!             .                  1.D0,         0.D0,         1.D0,
!             .                  0.D0,         1.D0,         1.D0,
!             .                  1.D0,         1.D0,         1.D0  /
!
!        C
!        C     Print the banner.
!        C
!              WRITE(*,*) ' RECT(1)  RECT(2)  RECT(3) '
!             . //        '  RADIUS    LON      LAT   '
!              WRITE(*,*) ' -------  -------  ------- '
!             . //        ' -------  -------  ------- '
!
!        C
!        C     Do the conversion. Output angles in degrees.
!        C
!              DO I = 1, NREC
!
!                 CALL RECLAT( RECTAN(1,I), RADIUS, LON, LAT )
!
!                 WRITE (*,'(6F9.3)') ( RECTAN(J,I), J=1,3 ),
!             .              RADIUS, LON * DPR(), LAT * DPR()
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!          RECT(1)  RECT(2)  RECT(3)   RADIUS    LON      LAT
!          -------  -------  -------  -------  -------  -------
!            0.000    0.000    0.000    0.000    0.000    0.000
!            1.000    0.000    0.000    1.000    0.000    0.000
!            0.000    1.000    0.000    1.000   90.000    0.000
!            0.000    0.000    1.000    1.000    0.000   90.000
!           -1.000    0.000    0.000    1.000  180.000    0.000
!            0.000   -1.000    0.000    1.000  -90.000    0.000
!            0.000    0.000   -1.000    1.000    0.000  -90.000
!            1.000    1.000    0.000    1.414   45.000    0.000
!            1.000    0.000    1.000    1.414    0.000   45.000
!            0.000    1.000    1.000    1.414   90.000   45.000
!            1.000    1.000    1.000    1.732   45.000   35.264
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
!     C.H. Acton         (JPL)
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
!
!        Changed the output argument name LONG to LON for consistency
!        with other routines.
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Added complete code examples.
!
!-    SPICELIB Version 1.0.2, 30-JUL-2003 (NJB) (CHA)
!
!        Various header changes were made to improve clarity. Some
!        minor header corrections were made.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     rectangular to latitudinal coordinates
!
!-&


!
!     Local variables.
!
      real(wp) x
      real(wp) y
      real(wp) z
      real(wp) big

!
!     Store rectangular coordinates in temporary variables
!
      big = max ( dabs(rectan(1)), dabs(rectan(2)), dabs(rectan(3)) )

      if ( big .gt. 0 ) then

         x      = rectan(1) / big
         y      = rectan(2) / big
         z      = rectan(3) / big

         radius = big * dsqrt (x*x + y*y + z*z)

         lat    = datan2 ( z, dsqrt(x*x + y*y) )

         x      = rectan(1)
         y      = rectan(2)

         if (x.eq.0.0_wp .and. y.eq.0.0_wp) then
            lon = 0.0_wp
         else
            lon = datan2 (y,x)
         end if

      else
         radius = 0.0_wp
         lat    = 0.0_wp
         lon   = 0.0_wp
      end if

      return
      end
