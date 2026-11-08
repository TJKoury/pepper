!$Procedure LSPCN  ( Longitude of the sun, planetocentric )

      double precision function lspcn ( body, et, abcorr )

! $ Abstract
!
!     Compute L_s, the planetocentric longitude of the sun, as seen
!     from a specified body.
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

      implicit none

      include               'zzctr.inc'

      character*(*)         body
      double precision      et
      character*(*)         abcorr

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
      logical               failed
      logical               return

!
!     Local parameters
!
      character*(*)         ref
      parameter           ( ref     = 'J2000' )

      integer               sun
      parameter           ( sun     = 10 )

!
!     Saved body name length.
!
      integer               maxl
      parameter           ( maxl  = 36 )


!
!     Local variables
!
      double precision      uavel  ( 3 )
      double precision      lat
      double precision      lt
      double precision      npole  ( 3 )
      double precision      pos    ( 3 )
      double precision      radius
      double precision      bstate ( 6 )
      double precision      sstate ( 6 )
      double precision      tipm   ( 3, 3 )
      double precision      trans  ( 3, 3 )

      integer               idcode
      integer               i

      logical               found

!
!     Saved name/ID item declarations.
!
      integer               svctr1 ( ctrsiz )
      character*(maxl)      svbody
      integer               svidcd
      logical               svfnd1

      logical               first

!
!     Saved name/ID items.
!
      save                  svctr1
      save                  svbody
      save                  svidcd
      save                  svfnd1

      save                  first

!
!     Initial values.
!
      data                  first   / .true. /


!
!     Give the function an initial value.
!
      lspcn = 0.d0

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      end if

      call chkin ( 'LSPCN' )

!
!     Initialization.
!
      if ( first ) then

!
!        Initialize counters
!
         call zzctruin( svctr1 )

         first = .false.

      end if

!
!     Map the body name to an ID code.
!
      call zzbods2c ( svctr1, svbody, svidcd, svfnd1, &
                      body, idcode, found    )

      if ( .not. found ) then

         call setmsg ( 'The body name # could not be translated ' // &
                       'to a NAIF ID code.  The cause of this '   // &
                       'problem may be that you need an updated ' // &
                       'version of the SPICE Toolkit.'            )
         call errch  ( '#', body                                  )
         call sigerr ( 'SPICE(NOTRANSLATION)'                     )
         call chkout ( 'LSPCN'                                    )
         return

      end if

!
!     Look up the direction of the North pole of the central body.
!     Note that TIPBOD does make use of binary PCK data if available.
!
      call tipbod ( ref, idcode, et, tipm )

      do i = 1, 3
         npole(i) = tipm(3,i)
      end do

!
!     Get the geometric state of the body relative to the sun.
!
      call spkgeo ( idcode, et, ref, sun, bstate, lt )

!
!     Get the unit direction vector parallel to the angular velocity
!     vector of the orbit.  This is just the unitized cross product of
!     position and velocity.
!
      call ucrss ( bstate, bstate(4), uavel )

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
      call twovec ( uavel, 3, npole, 2, trans )

      if ( failed() ) then
         call chkout ( 'LSPCN' )
         return
      end if

!
!     We'll find the position of the Sun relative to this frame.
!
!     Get the state of the sun in frame REF.  Since we may be using
!     aberration corrections, this is not necessarily the negative of
!     the state we've just found.
!
      call spkezr ( 'SUN', et, ref, abcorr, body, sstate, lt )

!
!     Now transform the position of the Sun into the "orbit plane
!     and equinox" frame.
!
      call mxv ( trans, sstate, pos )

!
!     Let RECRAD find the longitude LS for us.  RECRAD performs
!     the same coordinate transformation as the more commonly used
!     RECLAT, but the range of right ascension is 0:2*pi, which is
!     what we want for Ls.
!
      call recrad ( pos, radius, lspcn, lat )

      call chkout ( 'LSPCN' )
      return
      end
