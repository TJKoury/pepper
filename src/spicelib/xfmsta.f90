!$Procedure XFMSTA ( Transform state between coordinate systems )

      subroutine xfmsta ( istate, icosys, ocosys, body, ostate )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Transform a state between coordinate systems.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CONVERSION
!     COORDINATE
!     EPHEMERIS
!     STATE
!
! $ Declarations

      implicit none

      include               'zzctr.inc'

      real(wp)              istate(6)
      character*(*)         icosys
      character*(*)         ocosys
      character*(*)         body
      real(wp)              ostate(6)

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     ISTATE     I   Input state.
!     ICOSYS     I   Current (input) coordinate system.
!     OCOSYS     I   Desired (output) coordinate system.
!     BODY       I   Name or NAIF ID of body with which
!                    coordinates are associated (if applicable).
!     OSTATE     O   Converted output state.
!
! $ Detailed_Input
!
!     ISTATE   is a state vector in the input ICOSYS coordinate
!              system representing position and velocity.
!
!              All angular measurements must be in radians.
!
!              Note: body radii values taken from the kernel
!              pool are used when converting to or from geodetic or
!              planetographic coordinates. It is the user's
!              responsibility to verify the distance inputs are in
!              the same units as the radii in the kernel pool,
!              typically kilometers.
!
!     ICOSYS   is the name of the coordinate system that the input
!              state vector ISTATE is currently in.
!
!              ICOSYS may be any of the following:
!
!                 'RECTANGULAR'
!                 'CYLINDRICAL'
!                 'LATITUDINAL'
!                 'SPHERICAL'
!                 'GEODETIC'
!                 'PLANETOGRAPHIC'
!
!              Leading spaces, trailing spaces, and letter case
!              are ignored. For example, ' cyLindRical  ' would be
!              accepted.
!
!     OCOSYS   is the name of the coordinate system that the state
!              should be converted to.
!
!              Please see the description of ICOSYS for details.
!
!     BODY     is the name or NAIF ID of the body associated with the
!              planetographic or geodetic coordinate system.
!
!              If neither of the coordinate system choices are
!              geodetic or planetographic, BODY is ignored. It may
!              be a blank string.
!
!              Examples of accepted body names or IDs are:
!
!                 'Earth'
!                 '399'
!
!              Leading spaces, trailing spaces, and letter case are
!              ignored.
!
! $ Detailed_Output
!
!     OSTATE   is the state vector that has been converted to the
!              output coordinate system OCOSYS.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If either the input or output coordinate system is not
!         recognized, the error SPICE(COORDSYSNOTREC) is signaled.
!
!     2)  If the input body name cannot be converted to a NAIF ID
!         (applies to geodetic and planetographic coordinate
!         systems), the error SPICE(IDCODENOTFOUND) is signaled.
!
!     3)  If the input state ISTATE is not valid, meaning the position
!         but not the velocity is along the z-axis, the error
!         SPICE(INVALIDSTATE) is signaled.
!
!         Note: If both the input position and velocity are along
!         the z-axis and the output coordinate system is not
!         rectangular, the velocity can still be calculated even
!         though the Jacobian is undefined. This case will not
!         signal an error. An example of the input position and
!         velocity along the z-axis is below.
!
!                       Term    Value
!                       -----   ------
!                         x       0
!                         y       0
!                         z       z
!                       dx/dt     0
!                       dy/dt     0
!                       dz/dt   dz_dt
!
!     4)  If either the input or output coordinate system is
!         geodetic or planetographic and at least one of the body's
!         radii is less than or equal to zero, the error
!         SPICE(INVALIDRADIUS) is signaled.
!
!     5)  If either the input or output coordinate system is
!         geodetic or planetographic and the difference of the
!         equatorial and polar radii divided by the equatorial radius
!         would produce numeric overflow, the error
!         SPICE(INVALIDRADIUS) is signaled.
!
!     6)  If the product of the Jacobian and velocity components
!         may lead to numeric overflow, the error
!         SPICE(NUMERICOVERFLOW) is signaled.
!
!     7)  If radii for BODY are not found in the kernel pool, an error
!         is signaled by a routine in the call tree of this routine.
!
!     8)  If the size of the BODY radii kernel variable is not three,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     9)  If any of the three BODY radii is less-than or equal to zero,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     10) If body's equatorial radii are not equal and either the
!         input or output coordinate system is geodetic or
!         planetographic, the error SPICE(NOTSUPPORTED) is signaled.
!
! $ Files
!
!     SPK, PCK, CK, and FK kernels may be required.
!
!     If the input or output coordinate systems are either geodetic or
!     planetographic, a PCK providing the radii of the body
!     name BODY must be loaded via FURNSH.
!
!     Kernel data are normally loaded once per program run, NOT every
!     time this routine is called.
!
! $ Particulars
!
!     Input Order
!     -------------------------------------------
!
!     The input and output states will be structured by the
!     following descriptions.
!
!     For rectangular coordinates, the state vector is the following
!     in which X, Y, and Z are the rectangular position components and
!     DX, DY, and DZ are the time derivatives of each position
!     component.
!
!             ISTATE = ( X, Y, Z, DX, DY, DZ )
!
!     For cylindrical coordinates, the state vector is the following
!     in which R is the radius, LONG is the longitudes, Z is the
!     height, and DR, DLONG, and DZ are the time derivatives of each
!     position component.
!
!             ISTATE = ( R, LONG, Z, DR, DLONG, DZ )
!
!     For latitudinal coordinates, the state vector is the following
!     in which R is the radius, LONG is the longitude, LAT is the
!     latitude, and DR, DLONG, and DLAT are the time derivatives of
!     each position component.
!
!             ISTATE = ( R, LONG, LAT, DR, DLONG, DLAT )
!
!     For spherical coordinates, the state vector is the following in
!     which R is the radius, COLAT is the colatitude, LONG is the
!     longitude, and DR, DCOLAT, and DLONG are the time derivatives of
!     each position component.
!
!             ISTATE = ( R, COLAT, LONG, DR, DCOLAT, DLONG )
!
!     For geodetic coordinates, the state vector is the following in
!     which LONG is the longitude, LAT is the latitude, ALT is the
!     altitude, and DLONG, DLAT, and DALT are the time derivatives of
!     each position component.
!
!             ISTATE = ( LONG, LAT, ALT, DLONG, DLAT, DALT )
!
!     For planetographic coordinates, the state vector is the
!     following in which LONG is the longitude, LAT is the latitude,
!     ALT is the altitude, and DLONG, DLAT, and DALT are the time
!     derivatives of each position component.
!
!             ISTATE = ( LONG, LAT, ALT, DLONG, DLAT, DALT )
!
!
!     Input Boundaries
!     -------------------------------------------
!
!     There are intervals the input angles must fall within if
!     the input coordinate system is not rectangular. These
!     intervals are provided below.
!
!        Input variable    Input meaning   Input interval [rad]
!        --------------    -------------   ------------------------
!            LONG           Longitude        0     <= LONG  <  2*pi
!            LAT            Latitude        -pi/2  <= LAT   <= pi/2
!            COLAT          Colatitude       0     <= COLAT <= pi
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Find the apparent state of Phoebe as seen by CASSINI in the
!        J2000 frame at 2004 Jun 11 19:32:00. Transform the state
!        from rectangular to latitudinal coordinates. For verification,
!        transform the state back from latitudinal to rectangular
!        coordinates.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: xfmsta_ex1.tm
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
!              File name                        Contents
!              ---------                        --------
!              pck00010.tpc                     Planet orientation and
!                                               radii
!              naif0012.tls                     Leapseconds
!              041014R_SCPSE_01066_04199.bsp    CASSINI, planetary and
!                                               Saturn Satellite
!                                               ephemeris
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'naif0012.tls',
!                                  '041014R_SCPSE_01066_04199.bsp',
!                                  'pck00010.tpc'                 )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM XFMSTA_EX1
!              IMPLICIT NONE
!        C
!        C     Local parameters
!        C
!        C     METAKR is the meta-kernel's filename.
!        C
!              CHARACTER*(*)         METAKR
!              PARAMETER           ( METAKR = 'xfmsta_ex1.tm' )
!
!              CHARACTER*(*)         FORM
!              PARAMETER           ( FORM = '(F16.6, F16.6, F16.6)' )
!
!        C
!        C     Local variables
!        C
!        C     STAREC is the state of Phoebe with respect to CASSINI in
!        C     rectangular coordinates. STALAT is the state rotated into
!        C     latitudinal coordinates. STREC2 is the state transformed
!        C     back into rectangular coordinates from latitudinal.
!        C
!              DOUBLE PRECISION      STAREC (6)
!              DOUBLE PRECISION      STALAT (6)
!              DOUBLE PRECISION      STREC2 (6)
!
!        C
!        C     ET is the ephemeris time (TDB) corresponding to the
!        C     observation.
!        C
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      LT
!
!              INTEGER               I
!
!        C
!        C     The required kernels must be loaded.
!        C
!              CALL FURNSH ( METAKR )
!
!        C
!        C     Calculate the state at 2004 Jun 11 19:32:00 UTC.
!        C
!              CALL STR2ET ( '2004-JUN-11-19:32:00', ET )
!
!        C
!        C     Calculate the apparent state of Phoebe as seen by
!        C     CASSINI in the J2000 frame.
!        C
!              CALL SPKEZR ( 'PHOEBE',  ET, 'IAU_PHOEBE', 'LT+S',
!             .              'CASSINI', STAREC, LT              )
!
!        C
!        C     Transform the state from rectangular to latitudinal.
!        C     Notice that since neither the input nor output
!        C     coordinate frames are 'geodetic' or 'planetographic',
!        C     the input for the body name is a blank string.
!        C
!              CALL XFMSTA ( STAREC, 'RECTANGULAR', 'LATITUDINAL', ' ',
!             .              STALAT )
!
!        C
!        C     Transform the state back to rectangular from latitudinal
!        C     for verification. This result should be very similar to
!        C     STAREC.
!        C
!              CALL XFMSTA ( STALAT, 'LATITUDINAL', 'RECTANGULAR',' ',
!             .              STREC2 )
!
!        C
!        C     Report the results.
!        C
!              WRITE (*,*)    ' '
!              WRITE (*,*)    'Phoebe as seen by CASSINI - rectangular'
!              WRITE (*,*)    '  Position [km]:'
!              WRITE (*,FORM) (STAREC(I), I = 1, 3)
!              WRITE (*,*)    '  Velocity [km/s]:'
!              WRITE (*,FORM) (STAREC(I), I = 4, 6)
!              WRITE (*,*)    ' '
!              WRITE (*,*)    'Phoebe as seen by CASSINI - latitudinal'
!              WRITE (*,*)    '  Position [km, rad, rad]:'
!              WRITE (*,FORM) (STALAT(I), I = 1, 3)
!              WRITE (*,*)    '  Velocity [km/s, rad/s, rad/s]:'
!              WRITE (*,FORM) (STALAT(I), I = 4, 6)
!              WRITE (*,*)    ' '
!              WRITE (*,*)    'Verification: '
!              WRITE (*,*)    'Phoebe as seen by CASSINI - rectangular'
!              WRITE (*,*)    '  Position [km]:'
!              WRITE (*,FORM) (STREC2(I), I = 1, 3)
!              WRITE (*,*)    '  Velocity [km/s]:'
!              WRITE (*,FORM) (STREC2(I), I = 4, 6)
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Phoebe as seen by CASSINI - rectangular
!           Position [km]:
!            -2059.271283     -942.128329      -95.837672
!           Velocity [km/s]:
!                3.910113       -4.228139       -1.526561
!
!         Phoebe as seen by CASSINI - latitudinal
!           Position [km, rad, rad]:
!             2266.580876       -2.712515       -0.042296
!           Velocity [km/s, rad/s, rad/s]:
!               -1.730462        0.002416       -0.000706
!
!         Verification:
!         Phoebe as seen by CASSINI - rectangular
!           Position [km]:
!            -2059.271283     -942.128329      -95.837672
!           Velocity [km/s]:
!                3.910113       -4.228139       -1.526561
!
!
!     2) Transform a given state from cylindrical to planetographic
!        coordinates with respect to Earth.
!
!        Use the PCK kernel below to load the required triaxial
!        ellipsoidal shape model and orientation data for the Earth.
!
!           pck00010.tpc
!
!
!        Example code begins here.
!
!
!              PROGRAM XFMSTA_EX2
!              IMPLICIT NONE
!
!        C
!        C     Local parameters
!        C
!              CHARACTER*(*)         FORM
!              PARAMETER           ( FORM = '(F16.6, F16.6, F16.6)' )
!
!        C
!        C     Local variables
!        C
!        C     STACYL is the state in cylindrical coordinates.
!        C
!              DOUBLE PRECISION      STACYL (6)
!        C
!        C     STAPLN is the state transformed into planetographic
!        C     coordinates.
!        C
!              DOUBLE PRECISION      STAPLN (6)
!        C
!        C     STCYL2 is the state transformed back into
!        C     cylindrical coordinates from planetographic.
!        C
!              DOUBLE PRECISION      STCYL2 (6)
!
!              INTEGER               I
!
!              DATA STACYL / 1.0D0, 0.5D0, 0.5D0, 0.2D0, 0.1D0, -0.2D0 /
!
!        C
!        C     The required kernels must be loaded.
!        C
!              CALL FURNSH ( 'pck00010.tpc' )
!
!        C
!        C     Transform the state from cylindrical to planetographic.
!        C     Note that since one of the coordinate systems is
!        C     planetographic, the body name must be input.
!        C
!              CALL XFMSTA ( STACYL, 'CYLINDRICAL', 'PLANETOGRAPHIC',
!             .              'EARTH', STAPLN )
!
!        C
!        C     Transform the state back to cylindrical from
!        C     planetographic for verification. The result should be
!        C     very close to STACYL.
!        C
!              CALL XFMSTA ( STAPLN, 'PLANETOGRAPHIC', 'CYLINDRICAL',
!             .              'EARTH', STCYL2 )
!
!        C
!        C     Report the results.
!        C
!              WRITE (*,*)    'Cylindrical state'
!              WRITE (*,*)    '  Position [km, rad, km]:'
!              WRITE (*,FORM) (STACYL(I), I = 1, 3)
!              WRITE (*,*)    '  Velocity [km/s, rad/s, km/s]:'
!              WRITE (*,FORM) (STACYL(I), I = 4, 6)
!              WRITE (*,*)    ' '
!              WRITE (*,*) 'Planetographic state'
!              WRITE (*,*)    '  Position [rad, rad, km]:'
!              WRITE (*,FORM) (STAPLN(I), I = 1, 3)
!              WRITE (*,*)    '  Velocity [rad/s, rad/s, km/s]:'
!              WRITE (*,FORM) (STAPLN(I), I = 4, 6)
!              WRITE (*,*)    ' '
!              WRITE (*,*)    'Verification:  Cylindrical state'
!              WRITE (*,*)    '  Position [km, rad, km]:'
!              WRITE (*,FORM) (STCYL2(I), I = 1, 3)
!              WRITE (*,*)    '  Velocity [km/s, rad/s, km/s]:'
!              WRITE (*,FORM) (STCYL2(I), I = 4, 6)
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!         Cylindrical state
!           Position [km, rad, km]:
!                1.000000        0.500000        0.500000
!           Velocity [km/s, rad/s, km/s]:
!                0.200000        0.100000       -0.200000
!
!         Planetographic state
!           Position [rad, rad, km]:
!                0.500000        1.547722    -6356.240364
!           Velocity [rad/s, rad/s, km/s]:
!                0.100000       -0.004722       -0.195332
!
!         Verification:  Cylindrical state
!           Position [km, rad, km]:
!                1.000000        0.500000        0.500000
!           Velocity [km/s, rad/s, km/s]:
!                0.200000        0.100000       -0.200000
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
!     S.C. Krening       (JPL)
!     B.V. Semenov       (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 01-NOV-2021 (EDW) (JDR)
!
!        Body radii accessed from kernel pool using ZZGFTREB.
!
!        Edited he header to comply with NAIF standard.
!        Added missing begintext tag to the meta-kernel of example #1.
!        Modified example #2 to furnish a single kernel.
!
!        Updated Examples' kernels set to use PDS archived data.
!
!-    SPICELIB Version 1.1.0, 09-FEB-2017 (BVS)
!
!        BUG FIX: the routine no longer allows converting to and from
!        geodetic and planetographic coordinates for bodies with
!        unequal equatorial radii. Previously it arbitrarily picked the
!        first and the third radii to compute body's flattening
!        coefficient.
!
!-    SPICELIB Version 1.0.0, 22-APR-2014 (SCK) (BVS)
!
!-&


! $ Index_Entries
!
!     state transformation between coordinate systems
!     convert state
!
!-&


!
!     SPICELIB functions
!
      real(wp)              dpmax
      integer(ip)           isrchc
      logical               failed
      logical               return

!
!     Local parameters
!
!     Potentially large numbers produced by transforming the
!     velocity using the Jacobian must not exceed DPMAX()/MARGIN:
!
      real(wp)              margin
      parameter           ( margin = 1.0e2_wp )

!
!     The size of each coordinate system name must not exceed
!     CHSIZ characters.
!
      integer(ip)           chsiz
      parameter           ( chsiz  = 40 )

!
!     NCOSYS is the number of coordinate systems supported by
!     this routine.
!
      integer(ip)           ncosys
      parameter           ( ncosys = 6 )

!
!     The following integer parameters represent the coordinate
!     systems supported by this routine.
!
      integer(ip)           rectan
      parameter           ( rectan = 1 )

      integer(ip)           cyl
      parameter           ( cyl    = 2 )

      integer(ip)           latnl
      parameter           ( latnl  = 3 )

      integer(ip)           sphcl
      parameter           ( sphcl  = 4 )

      integer(ip)           geodet
      parameter           ( geodet = 5 )

      integer(ip)           plntgr
      parameter           ( plntgr = 6 )

!
!     Saved body name length.
!
      integer(ip)           maxl
      parameter           ( maxl   = 36 )


!
!     Local variables
!
!     COSYS is the array of supported coordinate system names.
!     ISYSU and OSYSU are the input and output coordinate systems
!     from the user that are made insensitive to case or leading and
!     trailing spaces.
!
      character*(chsiz)     cosys  (ncosys)
      character*(chsiz)     isysu
      character*(chsiz)     osysu

!
!     IPOS and IVEL are the input position and velocity translated
!     into rectangular.
!
      real(wp)              ipos   ( 3 )
      real(wp)              ivel   ( 3 )

!
!     For transformations including either geodetic or planetographic
!     coordinate systems, RADII is an array of the radii values
!     associated with the input body. These values will be loaded
!     from the kernel pool.
!
      real(wp)              radii  ( 3 )

!
!     JACOBI is the Jacobian matrix that converts the velocity
!     coordinates between systems.
!
      real(wp)              jacobi ( 3, 3 )

!
!     The flattening coefficient, F, is calculated when either
!     geodetic or planetographic coordinate systems are included
!     in the transformation.
!
      real(wp)              f

!
!     SQTMP and TOOBIG are used to check for possible numeric
!     overflow situations.
!
      real(wp)              sqtmp
      real(wp)              toobig

!
!     BODYID is only used when the input or output coordinate
!     systems are geodetic or planetographic. The BODYID is the NAID ID
!     associated with the input body name.
!
      integer(ip)           bodyid

!
!     ISYS and OSYS are the integer codes corresponding to the
!     input and output coordinate systems. I and J are iterators.
!
      integer(ip)           i
      integer(ip)           isys
      integer(ip)           j
      integer(ip)           osys

      logical               found

!
!     Saved name/ID item declarations.
!
      integer(ip)           svctr1 ( ctrsiz )
      character*(maxl)      svbody
      integer(ip)           svbdid
      logical               svfnd1

      logical               first
!
!     Saved variables
!
      save                  cosys

!
!     Saved name/ID items.
!
      save                  svctr1
      save                  svbody
      save                  svbdid
      save                  svfnd1

      save                  first

!
!     Assign the names of the coordinate systems to a character
!     array in which each coordinate system name is located at
!     the index of the integer ID of the coordinate system.
!
      data cosys ( rectan ) / 'RECTANGULAR'    /
      data cosys ( cyl    ) / 'CYLINDRICAL'    /
      data cosys ( latnl  ) / 'LATITUDINAL'    /
      data cosys ( sphcl  ) / 'SPHERICAL'      /
      data cosys ( geodet ) / 'GEODETIC'       /
      data cosys ( plntgr ) / 'PLANETOGRAPHIC' /

!
!     Initial values.
!
      data                  first   / .true. /


!
!     There are three main sections of this routine:
!
!       1)  Error handling and initialization.
!       2)  Conversion of the input to rectangular coordinates.
!       3)  Conversion from rectangular to the output coordinates.
!
!     Error handling and initialization
!     ----------------------------------------------------------------
!
!     Standard SPICE error handling.
!
      if ( return() ) then
         return
      end if

      call chkin ( 'XFMSTA' )

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
!     Remove initial and trailing spaces.
!     Convert the input coordinate systems to upper case.
!
      call ljucrs ( 0, icosys, isysu )
      call ljucrs ( 0, ocosys, osysu )

!
!     Check to see if the input and output coordinate systems
!     provided by the user are acceptable. Store the integer
!     code of the input and output coordinate systems into
!     ISYS and OSYS.
!
      isys = isrchc ( isysu, ncosys, cosys )
      osys = isrchc ( osysu, ncosys, cosys )

!
!     If the coordinate systems are not acceptable, an error is
!     signaled.
!
      if ( ( isys .eq. 0 )  .or.  ( osys .eq. 0 ) ) then

         if    ( ( isys .eq. 0 )  .and. ( osys .eq. 0 ) ) then

!
!           Both the input and the output coordinate systems were not
!           recognized.
!
            call setmsg ( 'Input coordinate system # ' &
                      //  'and output coordinate system # ' &
                      //  'are not recognized.' )
            call errch  ( '#', icosys )
            call errch  ( '#', ocosys )
            call sigerr ( 'SPICE(COORDSYSNOTREC)' )
            call chkout ( 'XFMSTA' )
            return

         else if ( isys .eq. 0 ) then

!
!           The input coordinate system was not recognized.
!
            call setmsg ( 'Input coordinate system # ' &
                     //   'was not recognized'  )
            call errch  ( '#', icosys )
            call sigerr ( 'SPICE(COORDSYSNOTREC)' )
            call chkout ( 'XFMSTA' )
            return

         else

!
!           The output coordinate system was not recognized.
!
            call setmsg ( 'Output coordinate system # ' &
                     //   'was not recognized'    )
            call errch  ( '#', ocosys )
            call sigerr ( 'SPICE(COORDSYSNOTREC)' )
            call chkout ( 'XFMSTA' )
            return

         end if

      end if

!
!     If the input and output coordinate systems are equal, set the
!     output equal to the input since no conversion needs to take
!     place.
!
      if ( isys .eq. osys ) then

         call vequg  ( istate, 6, ostate )
         call chkout ( 'XFMSTA' )
         return

      end if

!
!     If converting to or from either geodetic or planetographic, the
!     NAIF ID must be found from the input body name BODY. If the
!     body name does not have a valid NAIF ID code, an error is
!     signaled. If the NAIF ID is valid, the radii of the body are
!     located and the flattening coefficient is calculated.
!
      if ( ( osys .eq. geodet ) .or. ( osys .eq. plntgr )   .or. &
           ( isys .eq. geodet ) .or. ( isys .eq. plntgr ) ) then

!
!        Find the NAIF ID code
!
         call zzbods2c ( svctr1, svbody, svbdid, svfnd1, &
                         body, bodyid, found    )
!
!        If the body's name was found, find the body's radii and
!        compute flattening coefficient. Otherwise, signal an error.
!
         if ( found ) then

            call zzgftreb ( bodyid, radii )

            if ( failed() ) then
               call chkout ( 'XFMSTA' )
               return
            end if

!
!           If the difference of the equatorial and polar radii
!           divided by the equatorial radius is greater than DPMAX,
!           a numeric overflow may occur, so an error is signaled.
!
            if ( sqrt(abs( radii(1) - radii(3) )) / &
                 sqrt(abs( radii(1) )) .ge. sqrt(dpmax()) ) then

               call setmsg ( 'The equatorial radius for # ' &
                        //   'has a value of # and a polar radius of ' &
                        //   '#. The flattening coefficient cannot be ' &
                        //   'calculated due to numeric overflow.' )
               call errch  ( '#', body     )
               call errdp  ( '#', radii(1) )
               call errdp  ( '#', radii(3) )
               call sigerr ( 'SPICE(INVALIDRADIUS)' )
               call chkout ( 'XFMSTA'      )
               return

            end if

!
!           At this point, we also check for unequal equatorial radii,
!           which are not allowed with geodetic or planetographic
!           coordinates.
!
            if ( radii(1) .ne. radii(2) ) then

               call setmsg ( 'The body # has radii (#, #, #). ' &
               //            'Unequal equatorial ellipsoid radii are ' &
               //            'not supported for # and # coordinates.')
               call errch  ( '#', body                               )
               call errdp  ( '#', radii(1)                           )
               call errdp  ( '#', radii(2)                           )
               call errdp  ( '#', radii(3)                           )
               call errch  ( '#', cosys ( geodet )                   )
               call errch  ( '#', cosys ( plntgr )                   )
               call sigerr ( 'SPICE(NOTSUPPORTED)'                   )
               call chkout ( 'XFMSTA'                                )
               return

            end if

!
!           Calculate the flattening coefficient, F.
!
            f = ( radii(1) - radii(3) ) / radii(1)

         else

            call setmsg ( 'The input body name # does not ' &
                     //   'have a valid NAIF ID code.' )
            call errch  ( '#', body )
            call sigerr ( 'SPICE(IDCODENOTFOUND)'     )
            call chkout ( 'XFMSTA'  )
            return

         end if

      end if

!
!     Conversion of the input to rectangular coordinates
!     ----------------------------------------------------------------
!
!     First, the position and velocity coordinates will be converted
!     into rectangular coordinates. If the input system is not
!     rectangular, then the velocity coordinates must be translated
!     into rectangular using the Jacobian. If the input system is
!     rectangular, then the input state must simply be saved into IPOS
!     and IVEL.
!
!     TOOBIG is used for preventing numerical overflow. The square
!     roots of values are used to safely check if overflow will occur.
!
      toobig = sqrt ( dpmax() / margin )

      if  ( isys .ne. rectan ) then
!
!        To rectangular...
!
         if       ( isys .eq. cyl )    then

!
!                  ... from cylindrical
!
            call cylrec ( istate(1), istate(2), istate(3), ipos   )

            call drdcyl ( istate(1), istate(2), istate(3), jacobi )


         else if  ( isys .eq. latnl )  then

!
!                  ... from latitudinal
!
            call latrec ( istate(1), istate(2), istate(3), ipos   )

            call drdlat ( istate(1), istate(2), istate(3), jacobi )


         else if  ( isys .eq. sphcl )  then

!
!                  ... from spherical
!
            call sphrec ( istate(1), istate(2), istate(3), ipos   )

            call drdsph ( istate(1), istate(2), istate(3), jacobi )


         else if  ( isys .eq. geodet ) then

!
!                  ... from geodetic
!
            call georec ( istate(1), istate(2), istate(3), radii(1), &
                          f, ipos )

            if ( failed() ) then
               call chkout ( 'XFMSTA' )
               return
            end if

            call drdgeo ( istate(1), istate(2), istate(3), radii(1), &
                          f, jacobi )


         else if  ( isys .eq. plntgr ) then

!
!                  ... from planetographic
!
            call pgrrec ( body, istate(1), istate(2), istate(3), &
                          radii(1), f, ipos   )

            if ( failed() ) then
               call chkout ( 'XFMSTA' )
               return
            end if

            call drdpgr ( body, istate(1), istate(2), istate(3), &
                          radii(1), f, jacobi )

         else

            call setmsg ( 'This error should never occur. ' &
                     //   'This is an intermediate step in which a ' &
                     //   'non-rectangular input state should be ' &
                     //   'transferred to rectangular.  The input ' &
                     //   'coordinate system is not recognized, yet ' &
                     //   'was not caught by an earlier check.' )
            call sigerr ( 'SPICE(BUG1)')
            call chkout ( 'XFMSTA' )
            return

         end if

!
!        Some DRD* routines are not error free. Be safe and check
!        FAILED to not use un-initialized JACOBI.
!
         if ( failed() ) then
            call chkout ( 'XFMSTA' )
            return
         end if

!
!        If the multiplication of the Jacobian and velocity can cause
!        overflow, signal an error.
!
         do  i = 1, 3

            do  j = 1, 3

               sqtmp = sqrt ( abs ( jacobi(i,j) ) ) * &
                       sqrt ( abs ( istate(j+3) ) )

               if ( sqtmp .gt. toobig ) then

                  call setmsg ( 'The product of the Jacobian ' &
                           //   'and velocity may cause numeric ' &
                           //   'overflow.' )
                  call sigerr ( 'SPICE(NUMERICOVERFLOW)' )
                  call chkout ( 'XFMSTA'    )
                  return

               end if

            end do

         end do

!
!        Transform the velocity into rectangular coordinates.
!
         call mxv ( jacobi, istate(4), ivel )


      else if  ( isys .eq. rectan ) then

!
!        If the input coordinate system is rectangular, the input
!        position does not need to be translated into rectangular.
!
         call vequ ( istate(1), ipos )
         call vequ ( istate(4), ivel )

      else

         call setmsg ( 'This error should never occur. ' &
                  //   'This is an ELSE statement. If the input ' &
                  //   'coordinate system is not rectangular, the ' &
                  //   'IF should be executed. If the input ' &
                  //   'coordinate system is rectangular, the ELSE IF ' &
                  //   'should be executed.' )
         call sigerr ( 'SPICE(BUG2)' )
         call chkout ( 'XFMSTA'    )
         return

      end if

!
!     Conversion from rectangular into the output coordinates
!     ----------------------------------------------------------------
!
!     Convert to the output coordinate system. If the output
!     coordinate system is not rectangular, four calculations must
!     be made:
!
!       1)  Verify the position and velocity are not along the z-axis.
!           If the position and velocity are along the z-axis, the
!           velocity can still be converted even though the
!           Jacobian is not defined. If the position is along the
!           z-axis but the velocity is not, the velocity cannot be
!           converted to the output coordinate system.
!
!       2)  Calculate the Jacobian from rectangular to the output
!           coordinate system and verify the product of the Jacobian
!           and velocity will not cause numeric overflow.
!
!       3)  Transform the position to the output coordinate system.
!
!       4)  Transform the velocity to the output coordinates using
!           the Jacobian and the rectangular velocity IVEL.
!
      if ( osys .ne. rectan ) then

!
!        From rectangular for the case when the input position is along
!        the z-axis ...
!
         if ( ( abs(ipos(1)) + abs(ipos(2)) ) .eq. 0.0_wp) then

            if ( ( abs(ivel(1)) + abs(ivel(2)) ) .eq. 0.0_wp ) then
!
!              If the velocity is along the z-axis, then the velocity
!              can be computed in the output coordinate frame even
!              though the Jacobian is not defined.
!
               if       ( osys .eq. cyl )    then

!
!                  ... to cylindrical
!
                  call vpack  ( 0.0_wp, 0.0_wp, ivel(3), ostate(4) )

                  call reccyl ( ipos(1),  ostate(1), ostate(2), &
                                ostate(3) )


               else if  ( osys .eq. latnl )  then

!
!                  ... to latitudinal
!
                  call vpack  ( ivel(3), 0.0_wp, 0.0_wp, ostate(4) )

                  call reclat ( ipos(1), ostate(1),  ostate(2), &
                                ostate(3) )


               else if  ( osys .eq. sphcl )  then

!
!                  ... to spherical
!
                  call vpack  ( ivel(3), 0.0_wp, 0.0_wp, ostate(4) )

                  call recsph ( ipos(1), ostate(1),  ostate(2), &
                                ostate(3) )


               else if  ( osys .eq. geodet ) then

!
!                  ... to geodetic
!
                  call vpack  ( 0.0_wp, 0.0_wp, ivel(3),  ostate(4) )

                  call recgeo ( ipos(1), radii(1), f, ostate(1), &
                                ostate(2), ostate(3) )


               else if  ( osys .eq. plntgr ) then

!
!                  ... to planetographic
!
                  call vpack  ( 0.0_wp, 0.0_wp, ivel(3), ostate(4) )

                  call recpgr ( body, ipos(1), radii(1), f, ostate(1), &
                                ostate(2), ostate(3) )

               else

                  call setmsg ( 'This error should never ' &
                       //   'occur. This is an intermediate step in ' &
                       //   'which a position and velocity along ' &
                       //   'the z-axis are converted to a ' &
                       //   'non-rectangular coordinate system from ' &
                       //   'rectangular. The output coordinate ' &
                       //   'system is not recognized, yet was not ' &
                       //   'caught by an earlier check.' )
                  call sigerr ( 'SPICE(BUG3)' )
                  call chkout ( 'XFMSTA'    )
                  return

               end if
!
!              The output state has been calculated for the special
!              case of the position and velocity existing along the
!              z-axis.
!
               call chkout ( 'XFMSTA' )
               return

            else
!
!              The Jacobian is undefined and the velocity cannot be
!              converted since it is not along the z-axis.
!              Signal an error.
!
               call setmsg ( 'Invalid input state: z axis.' )
               call sigerr ( 'SPICE(INVALIDSTATE)' )
               call chkout ( 'XFMSTA' )
               return

            end if

         end if

!
!        From rectangular for cases when the input position is not along
!        the z-axis ...
!
         if       ( osys .eq. cyl )    then
!
!                  ... to cylindrical
!
            call dcyldr ( ipos(1), ipos(2),   ipos(3),   jacobi    )

            call reccyl ( ipos(1), ostate(1), ostate(2), ostate(3) )

         else if  ( osys .eq. latnl )  then

!
!                  ... to latitudinal
!
            call dlatdr ( ipos(1), ipos(2),   ipos(3),   jacobi    )

            call reclat ( ipos(1), ostate(1), ostate(2), ostate(3) )

         else if  ( osys .eq. sphcl )  then

!
!                  ... to spherical
!
            call dsphdr ( ipos(1), ipos(2),   ipos(3),   jacobi    )

            call recsph ( ipos(1), ostate(1), ostate(2), ostate(3) )

         else if  ( osys .eq. geodet ) then

!
!                  ... to geodetic
!
            call dgeodr ( ipos(1), ipos(2),  ipos(3), radii(1), f, &
                          jacobi )

            call recgeo ( ipos(1), radii(1), f, ostate(1), ostate(2), &
                          ostate(3) )

         else if  ( osys .eq. plntgr ) then

!
!                  ... to planetographic
!
            call dpgrdr ( body, ipos(1), ipos(2), ipos(3), radii(1), &
                          f,    jacobi )

            call recpgr ( body, ipos(1), radii(1), f, ostate(1), &
                          ostate(2), ostate(3) )

         else

            call setmsg ( 'This error should never occur. ' &
                     //   'This is an intermediate step in which a ' &
                     //   'state is converted to a non-rectangular ' &
                     //   'coordinate system from rectangular. The ' &
                     //   'output coordinate system is not recognized, ' &
                     //   'yet was not caught by an earlier check.' )
            call sigerr ( 'SPICE(BUG4)' )
            call chkout ( 'XFMSTA'    )
            return

         end if

!
!        Many D*DR and REC* routines are not error free. Be safe and
!        check FAILED to not use un-initialized JACOBI.
!
         if ( failed() ) then
            call chkout ( 'XFMSTA' )
            return
         end if

!
!        If the multiplication of the Jacobian and velocity can cause
!        overflow, signal an error.
!
         do i = 1, 3

            do  j = 1, 3

               sqtmp = sqrt ( abs ( jacobi(i,j) ) ) * &
                       sqrt ( abs ( ivel(j)     ) )

               if ( sqtmp .gt. toobig ) then

                  call setmsg ( 'The product of the Jacobian ' &
                           //   'and velocity may cause numeric ' &
                           //   'overflow.' )
                  call sigerr ( 'SPICE(NUMERICOVERFLOW)' )
                  call chkout ( 'XFMSTA'    )
                  return

               end if

            end do

         end do

!
!        Calculate the velocity in the output coordinate system.
!
         call mxv ( jacobi, ivel, ostate(4) )


      else if ( osys .eq. rectan ) then

!
!        If the output coordinate system is rectangular, the position
!        and velocity components of the output state are set equal to
!        the rectangular IPOS and IVEL, respectively, because the
!        components have already been converted to rectangular.
!
         call vequ ( ipos, ostate(1) )
         call vequ ( ivel, ostate(4) )

      else

         call setmsg ( 'This error should never occur. ' &
                  //   'This is an ELSE statement. If the output ' &
                  //   'coordinate system is not rectangular, the ' &
                  //   'IF should be executed. If the output ' &
                  //   'coordinate system is rectangular, the ELSE IF ' &
                  //   'should be executed.' )
         call sigerr ( 'SPICE(BUG5)' )
         call chkout ( 'XFMSTA'    )
         return

      end if

      call chkout ( 'XFMSTA' )
      return
      end
