!$Procedure LSPCN  ( Longitude of the sun, planetocentric )

      DOUBLE PRECISION FUNCTION LSPCN ( BODY, ET, ABCORR )

! $ Abstract
!
!     Compute L_s, the planetocentric longitude of the sun, as seen
!     from a specified body.
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
!     NAIF_IDS
!     PCK
!     TIME
!     SPK
!
! $ Keywords
!
!     GEOMETRY
!     TIME
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         BODY
      DOUBLE PRECISION      ET
      CHARACTER*(*)         ABCORR

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     BODY       I   Name of central body.
!     ET         I   Epoch in seconds past J2000 TDB.
!     ABCORR     I   Aberration correction.
!
!     The function returns the value of L_s for the specified body
!     at the specified time.
!
! $ Detailed_Input
!
!     BODY     is the name of the central body, typically a planet.
!
!     ET       is the epoch at which the longitude of the sun (L_s)
!              is to be computed. ET is expressed as seconds past
!              J2000 TDB (Barycentric Dynamical Time).
!
!     ABCORR   indicates the aberration corrections to be applied
!              when computing the longitude of the sun. ABCORR may
!              be any of the following.
!
!                 'NONE'     Apply no correction.
!
!                 'LT'       Correct the position of the sun,
!                            relative to the central body, for
!                            planetary (light time) aberration.
!
!                 'LT+S'     Correct the position of the sun,
!                            relative to the central body, for
!                            planetary and stellar aberrations.
!
! $ Detailed_Output
!
!     The function returns the planetocentric longitude of the Sun,
!     often called "L_s," for the specified body at the specified time.
!     This is the longitude of the body-Sun vector in a right-handed
!     frame whose basis vectors are defined as follows:
!
!     -  The positive Z direction is given by the instantaneous
!        angular velocity vector of the orbit of the body about
!        the Sun.
!
!     -  The positive X direction is that of the cross product of the
!        instantaneous north spin axis of the body with the positive
!        Z direction.
!
!     -  The positive Y direction is Z x X.
!
!     Units are radians; the range is 0 to 2*pi. Longitudes are
!     positive to the east.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input body name cannot be translated to an ID code,
!         and if the name is not a string representation of an integer
!         (for example, '399'), the error SPICE(NOTRANSLATION) is
!         signaled.
!
!     2)  If no SPK (ephemeris) file has been loaded prior to calling
!         this routine, or if the SPK data has insufficient coverage, an
!         error is signaled by a routine in the call
!         tree of this routine.
!
!     3)  If a PCK file containing rotational elements for the central
!         body has not been loaded prior to calling this routine, an
!         error is signaled by a routine in the call tree of this
!         routine.
!
!     4)  If the instantaneous angular velocity and spin axis of BODY
!         are parallel, an error is signaled by a
!         routine in the call tree of this routine.
!
! $ Files
!
!     Appropriate SPICE kernels must be loaded by the calling program
!     before this routine is called.
!
!     The following data are required:
!
!     -  An SPK file (or files) containing ephemeris data sufficient to
!        compute the geometric state of the central body relative to
!        the sun at ET must be loaded before this routine is called. If
!        light time correction is used, data must be available that
!        enable computation of the state the sun relative to the solar
!        system barycenter at the light-time corrected epoch. If
!        stellar aberration correction is used, data must be available
!        that enable computation of the state the central body relative
!        to the solar system barycenter at ET.
!
!     -  A PCK file containing rotational elements for the central body
!        must be loaded before this routine is called.
!
! $ Particulars
!
!     The direction of the vernal equinox for the central body is
!     determined from the instantaneous equatorial and orbital planes
!     of the central body. This equinox definition is specified in
!     reference [1]. The "instantaneous orbital plane" is interpreted
!     in this routine as the plane normal to the cross product of the
!     position and velocity of the central body relative to the sun.
!     The geometric state of the central body relative to the sun is
!     used for this normal vector computation. The "instantaneous
!     equatorial plane" is normal to the central body's north pole
!     at the requested epoch. The pole direction is determined from
!     rotational elements loaded via a PCK file.
!
!     The result returned by this routine will depend on the
!     ephemeris data and rotational elements used. The result may
!     differ from that given in any particular version of the
!     Astronomical Almanac, due to differences in these input data,
!     and due to differences in precision of the computations.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) A simple program that computes L_s for a body and time
!        supplied interactively. The geometric state of the Sun is
!        used.
!
!        Example code begins here.
!
!
!              PROGRAM LSPCN_EX1
!              IMPLICIT NONE
!
!              DOUBLE PRECISION      DPR
!              DOUBLE PRECISION      LSPCN
!
!              CHARACTER*(*)         ABCORR
!              PARAMETER           ( ABCORR = 'NONE' )
!
!              INTEGER               FILSIZ
!              PARAMETER           ( FILSIZ = 255 )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 36 )
!
!              INTEGER               TIMLEN
!              PARAMETER           ( TIMLEN = 40 )
!
!              CHARACTER*(NAMLEN)    BODY
!              CHARACTER*(FILSIZ)    LSK
!              CHARACTER*(FILSIZ)    PCK
!              CHARACTER*(FILSIZ)    SPK
!              CHARACTER*(TIMLEN)    TIMSTR
!
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      LON
!
!
!              CALL PROMPT ( 'Enter name of LSK file > ', LSK )
!              CALL PROMPT ( 'Enter name of PCK file > ', PCK )
!              CALL PROMPT ( 'Enter name of SPK file > ', SPK )
!
!              CALL FURNSH ( LSK )
!              CALL FURNSH ( PCK )
!              CALL FURNSH ( SPK )
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Kernels have been loaded.'
!              WRITE (*,*) ' '
!
!              CALL PROMPT ( 'Enter name of central body       > ',
!             .               BODY                                  )
!              CALL PROMPT ( 'Enter calendar, JD, or DOY time  > ',
!             .               TIMSTR                                )
!
!              CALL STR2ET ( TIMSTR, ET )
!
!        C
!        C     Convert longitude to degrees.
!        C
!              LON = DPR() * LSPCN ( BODY, ET, ABCORR )
!
!              WRITE (*,*) ' '
!              WRITE (*,*) 'Central body              = ',  BODY
!              WRITE (*,*) 'Time                      = ',  TIMSTR
!              WRITE (*,*) 'Planetocentric L_s (deg.) = ',  LON
!              WRITE (*,*) ' '
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, using the LSK file named naif0012.tls, the PCK file
!        named pck00010.tpc, the SPK file named de421.bsp; the 'EARTH'
!        as central body and the time string '2018 Mar 8  17:59 UTC',
!        the output was:
!
!
!        Enter name of LSK file > naif0012.tls
!        Enter name of PCK file > pck00010.tpc
!        Enter name of SPK file > de421.bsp
!
!         Kernels have been loaded.
!
!        Enter name of central body       > EARTH
!        Enter calendar, JD, or DOY time  > 2018 Mar 8  17:59 UTC
!
!         Central body              = EARTH
!         Time                      = 2018 Mar 8  17:59 UTC
!         Planetocentric L_s (deg.) =    348.11593978317080
!
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  "The Astronomical Almanac for the Year 2005," page L9,
!          United States Naval Observatory, U.S. Government Printing
!          Office, Washington, D.C., 2004.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.1, 25-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Removed WHILE
!        loop from example code, and added solution.
!
!-    SPICELIB Version 1.1.0, 19-SEP-2013 (BVS)
!
!        Updated to save the input body name and ZZBODTRN state
!        counter and to do name-ID conversion only if the counter
!        has changed.
!
!-    SPICELIB Version 1.0.0, 07-JAN-2005 (NJB)
!
!-&


! $ Index_Entries
!
!     planetocentric longitude of Sun
!     compute L_s
!     compute Ls
!     compute L_sub_s
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.1.0, 19-SEP-2013 (BVS)
!
!        Updated to save the input body name and ZZBODTRN state
!        counter and to do name-ID conversion only if the counter
!        has changed.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      CHARACTER*(*)         REF
      PARAMETER           ( REF     = 'J2000' )

      INTEGER               SUN
      PARAMETER           ( SUN     = 10 )

!
!     Saved body name length.
!
      INTEGER               MAXL
      PARAMETER           ( MAXL  = 36 )


!
!     Local variables
!
      DOUBLE PRECISION      UAVEL  ( 3 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NPOLE  ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      RADIUS
      DOUBLE PRECISION      BSTATE ( 6 )
      DOUBLE PRECISION      SSTATE ( 6 )
      DOUBLE PRECISION      TIPM   ( 3, 3 )
      DOUBLE PRECISION      TRANS  ( 3, 3 )

      INTEGER               IDCODE
      INTEGER               I

      LOGICAL               FOUND

!
!     Saved name/ID item declarations.
!
      INTEGER               SVCTR1 ( CTRSIZ )
      CHARACTER*(MAXL)      SVBODY
      INTEGER               SVIDCD
      LOGICAL               SVFND1

      LOGICAL               FIRST

!
!     Saved name/ID items.
!
      SAVE                  SVCTR1
      SAVE                  SVBODY
      SAVE                  SVIDCD
      SAVE                  SVFND1

      SAVE                  FIRST

!
!     Initial values.
!
      DATA                  FIRST   / .TRUE. /


!
!     Give the function an initial value.
!
      LSPCN = 0.D0

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'LSPCN' )

!
!     Initialization.
!
      IF ( FIRST ) THEN

!
!        Initialize counters
!
         CALL ZZCTRUIN( SVCTR1 )

         FIRST = .FALSE.

      END IF

!
!     Map the body name to an ID code.
!
      CALL ZZBODS2C ( SVCTR1, SVBODY, SVIDCD, SVFND1, &
                      BODY, IDCODE, FOUND    )

      IF ( .NOT. FOUND ) THEN

         CALL SETMSG ( 'The body name # could not be translated ' // &
                       'to a NAIF ID code.  The cause of this '   // &
                       'problem may be that you need an updated ' // &
                       'version of the SPICE Toolkit.'            )
         CALL ERRCH  ( '#', BODY                                  )
         CALL SIGERR ( 'SPICE(NOTRANSLATION)'                     )
         CALL CHKOUT ( 'LSPCN'                                    )
         RETURN

      END IF

!
!     Look up the direction of the North pole of the central body.
!     Note that TIPBOD does make use of binary PCK data if available.
!
      CALL TIPBOD ( REF, IDCODE, ET, TIPM )

      DO I = 1, 3
         NPOLE(I) = TIPM(3,I)
      END DO

!
!     Get the geometric state of the body relative to the sun.
!
      CALL SPKGEO ( IDCODE, ET, REF, SUN, BSTATE, LT )

!
!     Get the unit direction vector parallel to the angular velocity
!     vector of the orbit.  This is just the unitized cross product of
!     position and velocity.
!
      CALL UCRSS ( BSTATE, BSTATE(4), UAVEL )

!
!     We want to create a transformation matrix that maps vectors from
!     basis REF to the following frame:

!        Z  =  UAVEL
!
!        X  =  NPOLE x UAVEL
!
!        Y  =  Z x X
!
!     This is a "two-vector" frame with the unit orbital
!     angular velocity vector UAVEL as the primary vector and the
!     spin axis NPOLE as the secondary vector.  The primary
!     vector is associated with the +Z axis; the secondary vector
!     is associated with the +Y axis.
!
      CALL TWOVEC ( UAVEL, 3, NPOLE, 2, TRANS )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'LSPCN' )
         RETURN
      END IF

!
!     We'll find the position of the Sun relative to this frame.
!
!     Get the state of the sun in frame REF.  Since we may be using
!     aberration corrections, this is not necessarily the negative of
!     the state we've just found.
!
      CALL SPKEZR ( 'SUN', ET, REF, ABCORR, BODY, SSTATE, LT )

!
!     Now transform the position of the Sun into the "orbit plane
!     and equinox" frame.
!
      CALL MXV ( TRANS, SSTATE, POS )

!
!     Let RECRAD find the longitude LS for us.  RECRAD performs
!     the same coordinate transformation as the more commonly used
!     RECLAT, but the range of right ascension is 0:2*pi, which is
!     what we want for Ls.
!
      CALL RECRAD ( POS, RADIUS, LSPCN, LAT )

      CALL CHKOUT ( 'LSPCN' )
      RETURN
      END
