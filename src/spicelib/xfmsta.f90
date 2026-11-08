!$Procedure XFMSTA ( Transform state between coordinate systems )

      SUBROUTINE XFMSTA ( ISTATE, ICOSYS, OCOSYS, BODY, OSTATE )

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

      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      DOUBLE PRECISION      ISTATE(6)
      CHARACTER*(*)         ICOSYS
      CHARACTER*(*)         OCOSYS
      CHARACTER*(*)         BODY
      DOUBLE PRECISION      OSTATE(6)

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
      DOUBLE PRECISION      DPMAX
      INTEGER               ISRCHC
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
!     Potentially large numbers produced by transforming the
!     velocity using the Jacobian must not exceed DPMAX()/MARGIN:
!
      DOUBLE PRECISION      MARGIN
      PARAMETER           ( MARGIN = 1.D2 )

!
!     The size of each coordinate system name must not exceed
!     CHSIZ characters.
!
      INTEGER               CHSIZ
      PARAMETER           ( CHSIZ  = 40 )

!
!     NCOSYS is the number of coordinate systems supported by
!     this routine.
!
      INTEGER               NCOSYS
      PARAMETER           ( NCOSYS = 6 )

!
!     The following integer parameters represent the coordinate
!     systems supported by this routine.
!
      INTEGER               RECTAN
      PARAMETER           ( RECTAN = 1 )

      INTEGER               CYL
      PARAMETER           ( CYL    = 2 )

      INTEGER               LATNL
      PARAMETER           ( LATNL  = 3 )

      INTEGER               SPHCL
      PARAMETER           ( SPHCL  = 4 )

      INTEGER               GEODET
      PARAMETER           ( GEODET = 5 )

      INTEGER               PLNTGR
      PARAMETER           ( PLNTGR = 6 )

!
!     Saved body name length.
!
      INTEGER               MAXL
      PARAMETER           ( MAXL   = 36 )


!
!     Local variables
!
!     COSYS is the array of supported coordinate system names.
!     ISYSU and OSYSU are the input and output coordinate systems
!     from the user that are made insensitive to case or leading and
!     trailing spaces.
!
      CHARACTER*(CHSIZ)     COSYS  (NCOSYS)
      CHARACTER*(CHSIZ)     ISYSU
      CHARACTER*(CHSIZ)     OSYSU

!
!     IPOS and IVEL are the input position and velocity translated
!     into rectangular.
!
      DOUBLE PRECISION      IPOS   ( 3 )
      DOUBLE PRECISION      IVEL   ( 3 )

!
!     For transformations including either geodetic or planetographic
!     coordinate systems, RADII is an array of the radii values
!     associated with the input body. These values will be loaded
!     from the kernel pool.
!
      DOUBLE PRECISION      RADII  ( 3 )

!
!     JACOBI is the Jacobian matrix that converts the velocity
!     coordinates between systems.
!
      DOUBLE PRECISION      JACOBI ( 3, 3 )

!
!     The flattening coefficient, F, is calculated when either
!     geodetic or planetographic coordinate systems are included
!     in the transformation.
!
      DOUBLE PRECISION      F

!
!     SQTMP and TOOBIG are used to check for possible numeric
!     overflow situations.
!
      DOUBLE PRECISION      SQTMP
      DOUBLE PRECISION      TOOBIG

!
!     BODYID is only used when the input or output coordinate
!     systems are geodetic or planetographic. The BODYID is the NAID ID
!     associated with the input body name.
!
      INTEGER               BODYID

!
!     ISYS and OSYS are the integer codes corresponding to the
!     input and output coordinate systems. I and J are iterators.
!
      INTEGER               I
      INTEGER               ISYS
      INTEGER               J
      INTEGER               OSYS

      LOGICAL               FOUND

!
!     Saved name/ID item declarations.
!
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVBODY
      INTEGER               SVBDID
      LOGICAL               SVFND1

      LOGICAL               FIRST
!
!     Saved variables
!
      SAVE                  COSYS

!
!     Saved name/ID items.
!
      SAVE                  SVCTR1
      SAVE                  SVBODY
      SAVE                  SVBDID
      SAVE                  SVFND1

      SAVE                  FIRST

!
!     Assign the names of the coordinate systems to a character
!     array in which each coordinate system name is located at
!     the index of the integer ID of the coordinate system.
!
      DATA COSYS ( RECTAN ) / 'RECTANGULAR'    /
      DATA COSYS ( CYL    ) / 'CYLINDRICAL'    /
      DATA COSYS ( LATNL  ) / 'LATITUDINAL'    /
      DATA COSYS ( SPHCL  ) / 'SPHERICAL'      /
      DATA COSYS ( GEODET ) / 'GEODETIC'       /
      DATA COSYS ( PLNTGR ) / 'PLANETOGRAPHIC' /

!
!     Initial values.
!
      DATA                  FIRST   / .TRUE. /


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
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'XFMSTA' )

!
!     Initialization.
!
      IF ( FIRST ) THEN

!
!        Initialize counter.
!
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF

!
!     Remove initial and trailing spaces.
!     Convert the input coordinate systems to upper case.
!
      CALL LJUCRS ( 0, ICOSYS, ISYSU )
      CALL LJUCRS ( 0, OCOSYS, OSYSU )

!
!     Check to see if the input and output coordinate systems
!     provided by the user are acceptable. Store the integer
!     code of the input and output coordinate systems into
!     ISYS and OSYS.
!
      ISYS = ISRCHC ( ISYSU, NCOSYS, COSYS )
      OSYS = ISRCHC ( OSYSU, NCOSYS, COSYS )

!
!     If the coordinate systems are not acceptable, an error is
!     signaled.
!
      IF ( ( ISYS .EQ. 0 )  .OR.  ( OSYS .EQ. 0 ) ) THEN

         IF    ( ( ISYS .EQ. 0 )  .AND. ( OSYS .EQ. 0 ) ) THEN

!
!           Both the input and the output coordinate systems were not
!           recognized.
!
            CALL SETMSG ( 'Input coordinate system # ' &
                      //  'and output coordinate system # ' &
                      //  'are not recognized.' )
            CALL ERRCH  ( '#', ICOSYS )
            CALL ERRCH  ( '#', OCOSYS )
            CALL SIGERR ( 'SPICE(COORDSYSNOTREC)' )
            CALL CHKOUT ( 'XFMSTA' )
            RETURN

         ELSE IF ( ISYS .EQ. 0 ) THEN

!
!           The input coordinate system was not recognized.
!
            CALL SETMSG ( 'Input coordinate system # ' &
                     //   'was not recognized'  )
            CALL ERRCH  ( '#', ICOSYS )
            CALL SIGERR ( 'SPICE(COORDSYSNOTREC)' )
            CALL CHKOUT ( 'XFMSTA' )
            RETURN

         ELSE

!
!           The output coordinate system was not recognized.
!
            CALL SETMSG ( 'Output coordinate system # ' &
                     //   'was not recognized'    )
            CALL ERRCH  ( '#', OCOSYS )
            CALL SIGERR ( 'SPICE(COORDSYSNOTREC)' )
            CALL CHKOUT ( 'XFMSTA' )
            RETURN

         END IF

      END IF

!
!     If the input and output coordinate systems are equal, set the
!     output equal to the input since no conversion needs to take
!     place.
!
      IF ( ISYS .EQ. OSYS ) THEN

         CALL VEQUG  ( ISTATE, 6, OSTATE )
         CALL CHKOUT ( 'XFMSTA' )
         RETURN

      END IF

!
!     If converting to or from either geodetic or planetographic, the
!     NAIF ID must be found from the input body name BODY. If the
!     body name does not have a valid NAIF ID code, an error is
!     signaled. If the NAIF ID is valid, the radii of the body are
!     located and the flattening coefficient is calculated.
!
      IF ( ( OSYS .EQ. GEODET ) .OR. ( OSYS .EQ. PLNTGR )   .OR. &
           ( ISYS .EQ. GEODET ) .OR. ( ISYS .EQ. PLNTGR ) ) THEN

!
!        Find the NAIF ID code
!
         CALL ZZBODS2C ( SVCTR1, SVBODY, SVBDID, SVFND1, &
                         BODY, BODYID, FOUND    )
!
!        If the body's name was found, find the body's radii and
!        compute flattening coefficient. Otherwise, signal an error.
!
         IF ( FOUND ) THEN

            CALL ZZGFTREB ( BODYID, RADII )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'XFMSTA' )
               RETURN
            END IF

!
!           If the difference of the equatorial and polar radii
!           divided by the equatorial radius is greater than DPMAX,
!           a numeric overflow may occur, so an error is signaled.
!
            IF ( SQRT(ABS( RADII(1) - RADII(3) )) / &
                 SQRT(ABS( RADII(1) )) .GE. SQRT(DPMAX()) ) THEN

               CALL SETMSG ( 'The equatorial radius for # ' &
                        //   'has a value of # and a polar radius of ' &
                        //   '#. The flattening coefficient cannot be ' &
                        //   'calculated due to numeric overflow.' )
               CALL ERRCH  ( '#', BODY     )
               CALL ERRDP  ( '#', RADII(1) )
               CALL ERRDP  ( '#', RADII(3) )
               CALL SIGERR ( 'SPICE(INVALIDRADIUS)' )
               CALL CHKOUT ( 'XFMSTA'      )
               RETURN

            END IF

!
!           At this point, we also check for unequal equatorial radii,
!           which are not allowed with geodetic or planetographic
!           coordinates.
!
            IF ( RADII(1) .NE. RADII(2) ) THEN

               CALL SETMSG ( 'The body # has radii (#, #, #). ' &
               //            'Unequal equatorial ellipsoid radii are ' &
               //            'not supported for # and # coordinates.')
               CALL ERRCH  ( '#', BODY                               )
               CALL ERRDP  ( '#', RADII(1)                           )
               CALL ERRDP  ( '#', RADII(2)                           )
               CALL ERRDP  ( '#', RADII(3)                           )
               CALL ERRCH  ( '#', COSYS ( GEODET )                   )
               CALL ERRCH  ( '#', COSYS ( PLNTGR )                   )
               CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                   )
               CALL CHKOUT ( 'XFMSTA'                                )
               RETURN

            END IF

!
!           Calculate the flattening coefficient, F.
!
            F = ( RADII(1) - RADII(3) ) / RADII(1)

         ELSE

            CALL SETMSG ( 'The input body name # does not ' &
                     //   'have a valid NAIF ID code.' )
            CALL ERRCH  ( '#', BODY )
            CALL SIGERR ( 'SPICE(IDCODENOTFOUND)'     )
            CALL CHKOUT ( 'XFMSTA'  )
            RETURN

         END IF

      END IF

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
      TOOBIG = SQRT ( DPMAX() / MARGIN )

      IF  ( ISYS .NE. RECTAN ) THEN
!
!        To rectangular...
!
         IF       ( ISYS .EQ. CYL )    THEN

!
!                  ... from cylindrical
!
            CALL CYLREC ( ISTATE(1), ISTATE(2), ISTATE(3), IPOS   )

            CALL DRDCYL ( ISTATE(1), ISTATE(2), ISTATE(3), JACOBI )


         ELSE IF  ( ISYS .EQ. LATNL )  THEN

!
!                  ... from latitudinal
!
            CALL LATREC ( ISTATE(1), ISTATE(2), ISTATE(3), IPOS   )

            CALL DRDLAT ( ISTATE(1), ISTATE(2), ISTATE(3), JACOBI )


         ELSE IF  ( ISYS .EQ. SPHCL )  THEN

!
!                  ... from spherical
!
            CALL SPHREC ( ISTATE(1), ISTATE(2), ISTATE(3), IPOS   )

            CALL DRDSPH ( ISTATE(1), ISTATE(2), ISTATE(3), JACOBI )


         ELSE IF  ( ISYS .EQ. GEODET ) THEN

!
!                  ... from geodetic
!
            CALL GEOREC ( ISTATE(1), ISTATE(2), ISTATE(3), RADII(1), &
                          F, IPOS )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'XFMSTA' )
               RETURN
            END IF

            CALL DRDGEO ( ISTATE(1), ISTATE(2), ISTATE(3), RADII(1), &
                          F, JACOBI )


         ELSE IF  ( ISYS .EQ. PLNTGR ) THEN

!
!                  ... from planetographic
!
            CALL PGRREC ( BODY, ISTATE(1), ISTATE(2), ISTATE(3), &
                          RADII(1), F, IPOS   )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'XFMSTA' )
               RETURN
            END IF

            CALL DRDPGR ( BODY, ISTATE(1), ISTATE(2), ISTATE(3), &
                          RADII(1), F, JACOBI )

         ELSE

            CALL SETMSG ( 'This error should never occur. ' &
                     //   'This is an intermediate step in which a ' &
                     //   'non-rectangular input state should be ' &
                     //   'transferred to rectangular.  The input ' &
                     //   'coordinate system is not recognized, yet ' &
                     //   'was not caught by an earlier check.' )
            CALL SIGERR ( 'SPICE(BUG1)')
            CALL CHKOUT ( 'XFMSTA' )
            RETURN

         END IF

!
!        Some DRD* routines are not error free. Be safe and check
!        FAILED to not use un-initialized JACOBI.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'XFMSTA' )
            RETURN
         END IF

!
!        If the multiplication of the Jacobian and velocity can cause
!        overflow, signal an error.
!
         DO  I = 1, 3

            DO  J = 1, 3

               SQTMP = SQRT ( ABS ( JACOBI(I,J) ) ) * &
                       SQRT ( ABS ( ISTATE(J+3) ) )

               IF ( SQTMP .GT. TOOBIG ) THEN

                  CALL SETMSG ( 'The product of the Jacobian ' &
                           //   'and velocity may cause numeric ' &
                           //   'overflow.' )
                  CALL SIGERR ( 'SPICE(NUMERICOVERFLOW)' )
                  CALL CHKOUT ( 'XFMSTA'    )
                  RETURN

               END IF

            END DO

         END DO

!
!        Transform the velocity into rectangular coordinates.
!
         CALL MXV ( JACOBI, ISTATE(4), IVEL )


      ELSE IF  ( ISYS .EQ. RECTAN ) THEN

!
!        If the input coordinate system is rectangular, the input
!        position does not need to be translated into rectangular.
!
         CALL VEQU ( ISTATE(1), IPOS )
         CALL VEQU ( ISTATE(4), IVEL )

      ELSE

         CALL SETMSG ( 'This error should never occur. ' &
                  //   'This is an ELSE statement. If the input ' &
                  //   'coordinate system is not rectangular, the ' &
                  //   'IF should be executed. If the input ' &
                  //   'coordinate system is rectangular, the ELSE IF ' &
                  //   'should be executed.' )
         CALL SIGERR ( 'SPICE(BUG2)' )
         CALL CHKOUT ( 'XFMSTA'    )
         RETURN

      END IF

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
      IF ( OSYS .NE. RECTAN ) THEN

!
!        From rectangular for the case when the input position is along
!        the z-axis ...
!
         IF ( ( ABS(IPOS(1)) + ABS(IPOS(2)) ) .EQ. 0.D0) THEN

            IF ( ( ABS(IVEL(1)) + ABS(IVEL(2)) ) .EQ. 0.D0 ) THEN
!
!              If the velocity is along the z-axis, then the velocity
!              can be computed in the output coordinate frame even
!              though the Jacobian is not defined.
!
               IF       ( OSYS .EQ. CYL )    THEN

!
!                  ... to cylindrical
!
                  CALL VPACK  ( 0.D0, 0.D0, IVEL(3), OSTATE(4) )

                  CALL RECCYL ( IPOS(1),  OSTATE(1), OSTATE(2), &
                                OSTATE(3) )


               ELSE IF  ( OSYS .EQ. LATNL )  THEN

!
!                  ... to latitudinal
!
                  CALL VPACK  ( IVEL(3), 0.D0, 0.D0, OSTATE(4) )

                  CALL RECLAT ( IPOS(1), OSTATE(1),  OSTATE(2), &
                                OSTATE(3) )


               ELSE IF  ( OSYS .EQ. SPHCL )  THEN

!
!                  ... to spherical
!
                  CALL VPACK  ( IVEL(3), 0.D0, 0.D0, OSTATE(4) )

                  CALL RECSPH ( IPOS(1), OSTATE(1),  OSTATE(2), &
                                OSTATE(3) )


               ELSE IF  ( OSYS .EQ. GEODET ) THEN

!
!                  ... to geodetic
!
                  CALL VPACK  ( 0.D0, 0.D0, IVEL(3),  OSTATE(4) )

                  CALL RECGEO ( IPOS(1), RADII(1), F, OSTATE(1), &
                                OSTATE(2), OSTATE(3) )


               ELSE IF  ( OSYS .EQ. PLNTGR ) THEN

!
!                  ... to planetographic
!
                  CALL VPACK  ( 0.D0, 0.D0, IVEL(3), OSTATE(4) )

                  CALL RECPGR ( BODY, IPOS(1), RADII(1), F, OSTATE(1), &
                                OSTATE(2), OSTATE(3) )

               ELSE

                  CALL SETMSG ( 'This error should never ' &
                       //   'occur. This is an intermediate step in ' &
                       //   'which a position and velocity along ' &
                       //   'the z-axis are converted to a ' &
                       //   'non-rectangular coordinate system from ' &
                       //   'rectangular. The output coordinate ' &
                       //   'system is not recognized, yet was not ' &
                       //   'caught by an earlier check.' )
                  CALL SIGERR ( 'SPICE(BUG3)' )
                  CALL CHKOUT ( 'XFMSTA'    )
                  RETURN

               END IF
!
!              The output state has been calculated for the special
!              case of the position and velocity existing along the
!              z-axis.
!
               CALL CHKOUT ( 'XFMSTA' )
               RETURN

            ELSE
!
!              The Jacobian is undefined and the velocity cannot be
!              converted since it is not along the z-axis.
!              Signal an error.
!
               CALL SETMSG ( 'Invalid input state: z axis.' )
               CALL SIGERR ( 'SPICE(INVALIDSTATE)' )
               CALL CHKOUT ( 'XFMSTA' )
               RETURN

            END IF

         END IF

!
!        From rectangular for cases when the input position is not along
!        the z-axis ...
!
         IF       ( OSYS .EQ. CYL )    THEN
!
!                  ... to cylindrical
!
            CALL DCYLDR ( IPOS(1), IPOS(2),   IPOS(3),   JACOBI    )

            CALL RECCYL ( IPOS(1), OSTATE(1), OSTATE(2), OSTATE(3) )

         ELSE IF  ( OSYS .EQ. LATNL )  THEN

!
!                  ... to latitudinal
!
            CALL DLATDR ( IPOS(1), IPOS(2),   IPOS(3),   JACOBI    )

            CALL RECLAT ( IPOS(1), OSTATE(1), OSTATE(2), OSTATE(3) )

         ELSE IF  ( OSYS .EQ. SPHCL )  THEN

!
!                  ... to spherical
!
            CALL DSPHDR ( IPOS(1), IPOS(2),   IPOS(3),   JACOBI    )

            CALL RECSPH ( IPOS(1), OSTATE(1), OSTATE(2), OSTATE(3) )

         ELSE IF  ( OSYS .EQ. GEODET ) THEN

!
!                  ... to geodetic
!
            CALL DGEODR ( IPOS(1), IPOS(2),  IPOS(3), RADII(1), F, &
                          JACOBI )

            CALL RECGEO ( IPOS(1), RADII(1), F, OSTATE(1), OSTATE(2), &
                          OSTATE(3) )

         ELSE IF  ( OSYS .EQ. PLNTGR ) THEN

!
!                  ... to planetographic
!
            CALL DPGRDR ( BODY, IPOS(1), IPOS(2), IPOS(3), RADII(1), &
                          F,    JACOBI )

            CALL RECPGR ( BODY, IPOS(1), RADII(1), F, OSTATE(1), &
                          OSTATE(2), OSTATE(3) )

         ELSE

            CALL SETMSG ( 'This error should never occur. ' &
                     //   'This is an intermediate step in which a ' &
                     //   'state is converted to a non-rectangular ' &
                     //   'coordinate system from rectangular. The ' &
                     //   'output coordinate system is not recognized, ' &
                     //   'yet was not caught by an earlier check.' )
            CALL SIGERR ( 'SPICE(BUG4)' )
            CALL CHKOUT ( 'XFMSTA'    )
            RETURN

         END IF

!
!        Many D*DR and REC* routines are not error free. Be safe and
!        check FAILED to not use un-initialized JACOBI.
!
         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'XFMSTA' )
            RETURN
         END IF

!
!        If the multiplication of the Jacobian and velocity can cause
!        overflow, signal an error.
!
         DO I = 1, 3

            DO  J = 1, 3

               SQTMP = SQRT ( ABS ( JACOBI(I,J) ) ) * &
                       SQRT ( ABS ( IVEL(J)     ) )

               IF ( SQTMP .GT. TOOBIG ) THEN

                  CALL SETMSG ( 'The product of the Jacobian ' &
                           //   'and velocity may cause numeric ' &
                           //   'overflow.' )
                  CALL SIGERR ( 'SPICE(NUMERICOVERFLOW)' )
                  CALL CHKOUT ( 'XFMSTA'    )
                  RETURN

               END IF

            END DO

         END DO

!
!        Calculate the velocity in the output coordinate system.
!
         CALL MXV ( JACOBI, IVEL, OSTATE(4) )


      ELSE IF ( OSYS .EQ. RECTAN ) THEN

!
!        If the output coordinate system is rectangular, the position
!        and velocity components of the output state are set equal to
!        the rectangular IPOS and IVEL, respectively, because the
!        components have already been converted to rectangular.
!
         CALL VEQU ( IPOS, OSTATE(1) )
         CALL VEQU ( IVEL, OSTATE(4) )

      ELSE

         CALL SETMSG ( 'This error should never occur. ' &
                  //   'This is an ELSE statement. If the output ' &
                  //   'coordinate system is not rectangular, the ' &
                  //   'IF should be executed. If the output ' &
                  //   'coordinate system is rectangular, the ELSE IF ' &
                  //   'should be executed.' )
         CALL SIGERR ( 'SPICE(BUG5)' )
         CALL CHKOUT ( 'XFMSTA'    )
         RETURN

      END IF

      CALL CHKOUT ( 'XFMSTA' )
      RETURN
      END
