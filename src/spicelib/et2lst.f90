!$Procedure ET2LST ( ET to Local Solar Time )

      SUBROUTINE ET2LST ( ET, BODY, LON, TYPE, HR, MN, SC, TIME, AMPM )

! $ Abstract
!
!     Compute the local solar time for a given ephemeris epoch ET
!     for an object on the surface of a body at a specified longitude.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
!
! $ Required_Reading
!
!     TIME
!
! $ Keywords
!
!     TIME
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      INTEGER               BODY
      DOUBLE PRECISION      LON
      CHARACTER*(*)         TYPE
      INTEGER               HR
      INTEGER               MN
      INTEGER               SC
      CHARACTER*(*)         TIME
      CHARACTER*(*)         AMPM

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Epoch in seconds past J2000 epoch
!     BODY       I   ID-code of the body of interest
!     LON        I   Longitude of surface point (RADIANS)
!     TYPE       I   Type of longitude 'PLANETOCENTRIC', etc.
!     HR         O   Local hour on a "24 hour" clock
!     MN         O   Minutes past the hour
!     SC         O   Seconds past the minute
!     TIME       O   String giving local time on 24 hour clock
!     AMPM       O   String giving time on A.M./ P.M. scale
!
! $ Detailed_Input
!
!     ET       is the epoch expressed in TDB seconds past the J2000
!              epoch at which a local time is desired.
!
!     BODY     is the NAIF ID-code of a body on which local time is to
!              be measured.
!
!     LON      is the longitude (either planetocentric or
!              planetographic) in radians of the site on the surface of
!              body for which local time should be computed.
!
!     TYPE     is the form of longitude supplied by the variable LON.
!              Allowed values are:
!
!                 'PLANETOCENTRIC'
!                 'PLANETOGRAPHIC'
!
!              Note the case of the letters in TYPE is insignificant.
!              Both 'PLANETOCENTRIC' and 'planetocentric' are
!              recognized.
!
! $ Detailed_Output
!
!     HR       is the local "hour" of the site specified at the epoch
!              ET. Note that an "hour" of local time does not have the
!              same duration as an hour measured by conventional clocks.
!              It is simply a representation of an angle. See the
!              $Particulars section for a more complete discussion of
!              the meaning of local time.
!
!     MN       is the number of "minutes" past the hour of the local
!              time of the site at the epoch ET. Again note that a
!              "local minute" is not the same as a minute you would
!              measure with conventional clocks.
!
!     SC       is the number of "seconds" past the minute of the local
!              time of the site at the epoch ET. Again note that a
!              "local second" is not the same as a second you would
!              measure with conventional clocks.
!
!     TIME     is a string expressing the local time on a "24 hour"
!              local clock.
!
!     AMPM     is a string expressing the local time on a "12 hour"
!              local clock together with the traditional AM/PM label to
!              indicate whether the sun has crossed the local zenith
!              meridian.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine defines local solar time for any point on the
!         surface of the Sun to be 12:00:00 noon.
!
!     2)  If the TYPE of the coordinates is not recognized, the
!         error SPICE(UNKNOWNSYSTEM) is signaled.
!
!     3)  If the body-fixed frame to associate with BODY cannot be
!         determined, the error SPICE(CANTFINDFRAME) is signaled.
!
!     4)  If insufficient data are available to compute the location of
!         the sun in body-fixed coordinates, an error is signaled by a
!         routine in the call tree of this routine.
!
!     5)  If the BODY#_PM keyword required to determine the body
!         rotation sense is not found in the POOL or if it is found but
!         is not a numeric keyword with at least two elements, the error
!         SPICE(CANTGETROTATIONTYPE) is signaled.
!
! $ Files
!
!     Suitable SPK and PCK files must be loaded prior to calling this
!     routine so that the body-fixed position of the sun relative to
!     BODY can be computed. The PCK files must contain the standard
!     BODY#_PM keyword need by this routine to determine the body
!     rotation sense.
!
!     When the input longitude is planetographic, the default
!     interpretation of this value can be overridden using the optional
!     kernel variable
!
!        BODY<body ID>_PGR_POSITIVE_LON
!
!     which is normally defined via loading a text kernel.
!
! $ Particulars
!
!     This routine returns the local solar time at a user
!     specified location on a user specified body.
!
!     Let SUNLNG be the planetocentric longitude (in degrees) of
!     the sun as viewed from the center of the body of interest.
!
!     Let SITLNG be the planetocentric longitude (in degrees) of
!     the site for which local time is desired.
!
!     We define local time to be 12 + (SITLNG - SUNLNG)/15
!
!     (where appropriate care is taken to map ( SITLNG - SUNLNG )
!     into the range from -180 to 180).
!
!     Using this definition, we see that from the point of view
!     of this routine, local solar time is simply a measure of angles
!     between meridians on the surface of a body. Consequently,
!     this routine is not appropriate for computing "local times"
!     in the sense of Pacific Standard Time. For computing times
!     relative to standard time zones on earth, see the routines
!     TIMOUT and STR2ET.
!
!
!     Regarding planetographic longitude
!     ----------------------------------
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
!     If you wish to override the default sense of positive
!     planetographic longitude for a particular body, you can do so by
!     defining the kernel variable
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
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) The following code example illustrates how to compute the
!        local time at a site on Mars with planetographic longitude
!        +326.17 deg at epoch ET.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: et2lst_ex1.tm
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
!              naif0012.tls                  Leapseconds
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'de421.bsp',
!                                  'pck00010.tpc',
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
!              PROGRAM ET2LST_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions
!        C
!              DOUBLE PRECISION      RPD
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         FMT
!              PARAMETER           ( FMT    = '(A,F7.2,A)'    )
!
!              CHARACTER*(*)         META
!              PARAMETER           ( META   = 'et2lst_ex1.tm' )
!
!              CHARACTER*(*)         TYPE
!              PARAMETER           ( TYPE   = 'PLANETOGRAPHIC' )
!
!              INTEGER               AMPMLEN
!              PARAMETER           ( AMPMLEN = 51 )
!
!              INTEGER               MARS
!              PARAMETER           ( MARS   = 499 )
!
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN = 51 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(AMPMLEN)   AMPM
!              CHARACTER*(TIMLEN)    TIME
!              CHARACTER*(20)        UTCSTR
!
!              DOUBLE PRECISION      DLON
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      RLON
!
!              INTEGER               HR
!              INTEGER               MN
!              INTEGER               SC
!
!        C
!        C     Load the kernels.
!        C
!              CALL FURNSH ( META )
!
!              DLON   =  326.17D0
!              RLON   =  DLON * RPD( )
!              UTCSTR = '2002 SEP 02 00:00:00'
!
!              CALL STR2ET ( UTCSTR, ET )
!
!              CALL ET2LST ( ET, MARS, RLON, TYPE,
!             .              HR, MN,   SC,   TIME, AMPM )
!
!              WRITE(*,FMT) 'Local time at Mars', DLON,
!             .             ' degrees planetographic longitude:'
!              WRITE(*,*)   '   at UTC ', UTCSTR, ', LST = ', AMPM
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Local time at Mars 326.17 degrees planetographic longitude:
!            at UTC 2002 SEP 02 00:00:00, LST = 03:25:35 A.M.
!
!
! $ Restrictions
!
!     1)  This routine relies on being able to determine the name
!         of the body-fixed frame associated with BODY through the
!         frames subsystem. If the BODY specified is NOT one of the
!         nine planets or their satellites, you will need to load
!         an appropriate frame definition kernel that contains
!         the relationship between the body id and the body-fixed frame
!         name. See frames.req required reading for more details
!         on specifying this relationship.
!
!     2)  The routine determines the body rotation sense using the PCK
!         keyword BODY#_PM. Therefore, you will need to a text PCK file
!         defining the complete set of the standard PCK body rotation
!         keywords for the body of interest. The text PCK file must be
!         loaded independently of whether a binary PCK file providing
!         rotation data for the same body is loaded or not.
!
!     3)  Although it is not currently the case for any of the Solar
!         System bodies, it is possible that the retrograde rotation
!         rate of a body would be slower than the orbital rate of the
!         body rotation around the Sun. The routine does not account for
!         such cases; for them it will compute incorrect the local time
!         progressing backwards.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.1.0, 26-OCT-2021 (JDR)
!
!        Changed the input argument name LONG to LON for consistency
!        with other routines.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Added complete code example
!        from existing fragment.
!
!-    SPICELIB Version 3.0.2, 18-APR-2014 (BVS)
!
!        Minor edits to long error messages.
!
!-    SPICELIB Version 3.0.1, 09-SEP-2009 (EDW)
!
!        Header edits: deleted a spurious C$ marker from the
!        "Detailed_Output" section. The existence of the marker
!        caused a failure in the HTML documentation creation script.
!
!        Deleted the "Revisions" section as it contained several
!        identical entries from the "Version" section.
!
!        Corrected order of header sections.
!
!-    SPICELIB Version 3.0.0, 28-OCT-2006 (BVS)
!
!        Bug fix: incorrect computation of the local time for the
!        bodies with the retrograde rotation causing the local time to
!        flow backwards has been fixed. The local time for all types of
!        bodies now progresses as expected -- midnight, increasing AM
!        hours, noon, increasing PM hours, next midnight, and so on.
!
!-    SPICELIB Version 2.0.0, 03-NOV-2005 (NJB)
!
!        Bug fix: treatment of planetographic longitude has been
!        updated to be consistent with the SPICE planetographic/
!        rectangular coordinate conversion routines. The effect of
!        this change is that the default sense of positive longitude
!        for the moon is now east; also, the default sense of positive
!        planetographic longitude now may be overridden for any body
!        (see $Particulars above).
!
!        Updated to remove non-standard use of duplicate arguments
!        in RMAIND calls.
!
!-    SPICELIB Version 1.1.0, 24-MAR-1998 (WLT)
!
!        The integer variable SUN was never initialized in the
!        previous version of the routine. Now it is set to
!        the proper value of 10.
!
!-    SPICELIB Version 1.0.0, 09-JUL-1997 (WLT)
!
!-&


! $ Index_Entries
!
!     Compute the local time for a point on a body.
!
!-&


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      BRCKTD
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      TWOPI

      LOGICAL               RETURN

!
!     Local parameters
!
!
      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               SUN
      PARAMETER           ( SUN    = 10 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )


!
!     Local Variables
!
      CHARACTER*(4)         AMORPM
      CHARACTER*(BDNMLN)    BODNAM
      CHARACTER*(WDSIZE)    FRAME
      CHARACTER*(2)         H
      CHARACTER*(WDSIZE)    BPMKWD
      CHARACTER*(2)         M
      CHARACTER*(WDSIZE)    MYTYPE
      CHARACTER*(2)         S
      CHARACTER*(1)         KWTYPE

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      HOURS
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      MINS
      DOUBLE PRECISION      MYLONG
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      Q
      DOUBLE PRECISION      SECNDS
      DOUBLE PRECISION      SLAT
      DOUBLE PRECISION      SLONG
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      STATE  ( 6 )
      DOUBLE PRECISION      TMPANG
      DOUBLE PRECISION      TMPSEC

      INTEGER               FRCODE
      INTEGER               HRAMPM
      INTEGER               N

      LOGICAL               FOUND

!
!     Standard SPICE error handling.
!
      IF ( RETURN() ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ET2LST')


      CALL LJUST ( TYPE,   MYTYPE )
      CALL UCASE ( MYTYPE, MYTYPE )

      IF ( MYTYPE .EQ. 'PLANETOGRAPHIC' ) THEN
!
!        Find planetocentric longitude corresponding to the input
!        longitude.  We first represent in rectangular coordinates
!        a surface point having zero latitude, zero altitude, and
!        the input planetographic longitude. We then find the
!        planetocentric longitude of this point.
!
!        Since PGRREC accepts a body name, map the input code to
!        a name, if possible.  Otherwise, just convert the input code
!        to a string.
!
         CALL BODC2N ( BODY, BODNAM, FOUND )

         IF ( .NOT. FOUND ) THEN
            CALL INTSTR ( BODY, BODNAM )
         END IF

!
!        Convert planetographic coordinates to rectangular coordinates.
!        All we care about here is longitude.  Set the other inputs
!        as follows:
!
!            Latitude          = 0
!            Altitude          = 0
!            Equatorial radius = 1
!            Flattening factor = 0
!
         CALL PGRREC ( BODNAM, LON, 0.D0, 0.D0, 1.D0, 0.D0, SPOINT )

!
!        The output MYLONG is planetocentric longitude.  The other
!        outputs are not used.  Note that the variable RANGE appears
!        later in another RECLAT call; it's not used after that.
!
         CALL RECLAT ( SPOINT, RANGE, MYLONG, LAT )


      ELSE IF ( MYTYPE .EQ. 'PLANETOCENTRIC' ) THEN

         MYLONG = LON

      ELSE

         CALL SETMSG ( 'The coordinate system ''#'' is not a ' &
         //            'recognized system of longitude.  The ' &
         //            'recognized systems are ' &
         //            '''PLANETOCENTRIC'' and ' &
         //            '''PLANETOGRAPHIC''. ' )

         CALL ERRCH  ( '#', TYPE )
         CALL SIGERR ( 'SPICE(UNKNOWNSYSTEM)'  )
         CALL CHKOUT ( 'ET2LST' )
         RETURN

      END IF
!
!     It's always noon on the surface of the sun.
!
      IF ( BODY .EQ. 10 ) THEN

         HR = 12
         MN = 0
         SC = 0

         TIME = '12:00:00'
         AMPM = '12:00:00 P.M.'

         CALL CHKOUT ( 'ET2LST' )
         RETURN

      END IF

!
!     Get the body-fixed position of the sun.
!
      CALL CIDFRM ( BODY, FRCODE, FRAME, FOUND )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The body-fixed frame associated with ' &
         //            'body # could not be determined.  This ' &
         //            'information needs to be "loaded" via a ' &
         //            'frames definition kernel.  See ' &
         //            'frames.req for more details. '         )
         CALL ERRINT ( '#', BODY                               )
         CALL SIGERR ( 'SPICE(CANTFINDFRAME)'                  )
         CALL CHKOUT ( 'ET2LST'                                )
         RETURN

      END IF



      CALL SPKEZ  (  SUN,   ET,    FRAME, 'LT+S', BODY, STATE, LT )
      CALL RECLAT (  STATE, RANGE, SLONG,  SLAT )

      ANGLE = MYLONG - SLONG

!
!     Force the angle into the region from -PI to PI
!
      CALL RMAIND ( ANGLE, TWOPI(), Q, TMPANG )
      ANGLE = TMPANG

      IF ( ANGLE .GT. PI() ) THEN
         ANGLE = ANGLE - TWOPI()
      END IF

!
!     Get the rotation sense of the body and invert the angle if the
!     rotation sense is retrograde. Use the BODY#_PM PCK keyword to
!     determine the sense of the body rotation.
!
      BPMKWD = 'BODY#_PM'
      CALL REPMI ( BPMKWD, '#', BODY, BPMKWD )

      CALL DTPOOL ( BPMKWD, FOUND, N, KWTYPE )

      IF (      .NOT. FOUND &
            .OR. KWTYPE .NE. 'N' &
            .OR. N      .LT.  2  ) THEN

         CALL SETMSG ( 'The rotation type for the body # could ' &
         //            'not be determined because the # keyword ' &
         //            'was either not found in the POOL or ' &
         //            'or it was not of the expected type and/or ' &
         //            'dimension. This keyword is usually ' &
         //            'provided via a planetary constants ' &
         //            'kernel. See pck.req for more details. ')
         CALL ERRINT ( '#', BODY                               )
         CALL ERRCH  ( '#', BPMKWD                             )
         CALL SIGERR ( 'SPICE(CANTGETROTATIONTYPE)'            )
         CALL CHKOUT ( 'ET2LST'                                )
         RETURN

      ELSE

!
!        If the rotation rate is negative, invert the angle.
!
         CALL GDPOOL ( BPMKWD, 2, 1, N, RATE, FOUND )

         IF ( RATE .LT. 0.D0 ) THEN
            ANGLE = - ANGLE
         END IF

      END IF

!
!     Convert the angle to "angle seconds" before or after local noon.
!
      SECNDS = 86400.0D0 * ANGLE / TWOPI()
      SECNDS = BRCKTD ( SECNDS, -43200.0D0, 43200.0D0 )

!
!     Get the hour, and minutes components of the local time.
!
      CALL RMAIND ( SECNDS, 3600.0D0, HOURS, TMPSEC )
      CALL RMAIND ( TMPSEC, 60.0D0,   MINS,  SECNDS )
!
!     Construct the integer components of the local time.
!
      HR = 12 + INT(HOURS)
      MN =      INT(MINS)
      SC =      INT(SECNDS)
!
!     Set the A.M./P.M. components of local time.
!
      IF      ( HR .EQ. 24 ) THEN

         HR     = 0
         HRAMPM = 12
         AMORPM = 'A.M.'

      ELSE IF ( HR .GT. 12 ) THEN

         HRAMPM = HR - 12
         AMORPM = 'P.M.'

      ELSE IF ( HR .EQ. 12 ) THEN

         HRAMPM = 12
         AMORPM = 'P.M.'

      ELSE IF ( HR .EQ. 0 ) THEN

         HRAMPM = 12
         AMORPM = 'A.M.'

      ELSE

         HRAMPM = HR
         AMORPM = 'A.M.'

      END IF

!
!     Now construct the two strings we need.
!
      HOURS  = DBLE(HR)
      MINS   = DBLE(MN)
      SECNDS = DBLE(SC)

      CALL DPFMT ( HOURS,  '0x', H )
      CALL DPFMT ( MINS,   '0x', M )
      CALL DPFMT ( SECNDS, '0x', S )

      TIME = H // ':' // M // ':' // S


      HOURS = DBLE(HRAMPM)

      CALL DPFMT ( HOURS,  '0x', H )

      AMPM = H // ':' // M // ':' // S //  ' ' // AMORPM

      CALL CHKOUT ( 'ET2LST' )
      RETURN
      END
