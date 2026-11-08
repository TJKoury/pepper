!$Procedure      F_ZZILUSTA ( ZZILUSTA tests )

      SUBROUTINE F_ZZILUSTA ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        ZZILUSTA
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

      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'

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
!     This routine tests the private SPICELIB
!     geometry routine ZZILUSTA.
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 02-APR-2014 (NJB)
!
!        Added test case for zero normal vector.
!
!     Last update was 27-AUG-2013
!
!        Tolerance ANGTOL was loosened from 1.D-14
!        to 1.D-11.
!
!-    TSPICE Version 1.0.0, 02-MAY-2012 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      DVSEP
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD

!
!     Local parameters
!
      CHARACTER*(*)         PCK2
      PARAMETER           ( PCK2   = 'generic.tpc' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2   = 'generic.bsp' )

      CHARACTER*(*)         OCTSPK
      PARAMETER           ( OCTSPK = 'octl_test.bsp' )

      CHARACTER*(*)         OCTL
      PARAMETER           ( OCTL   = 'OCTL' )

      CHARACTER*(*)         IREF
      PARAMETER           ( IREF   = 'J2000' )

!
!
!
      DOUBLE PRECISION      ANGTOL
      PARAMETER           ( ANGTOL = 1.D-11 )

      DOUBLE PRECISION      SINGLE
      PARAMETER           ( SINGLE = 5.D-6 )

!
!     Tolerances for tests involving discrete derivatives
!     of illumination angle rate. These are applicable
!     when aberration corrections are turned off, or when
!     a "smooth" planetary ephemeris such as DE421.bsp is
!     used.
!
      DOUBLE PRECISION      DPHTOL
      PARAMETER           ( DPHTOL = 1.0D-6 )

      DOUBLE PRECISION      DINTOL
      PARAMETER           ( DINTOL = 5.0D-7 )

      DOUBLE PRECISION      DEMTOL
      PARAMETER           ( DEMTOL = 5.0D-7 )

!
!     Tolerance for illumination angle tests using
!     alternate computation methods:
!
      DOUBLE PRECISION      ALTTOL
      PARAMETER           ( ALTTOL = 1.0D-10 )

      DOUBLE PRECISION      MEDTOL
      PARAMETER           ( MEDTOL = 1.D-7 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

!
!     Number of test epochs:
!
      INTEGER               NTIMES
      PARAMETER           ( NTIMES = 100 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               NCORR
      PARAMETER           ( NCORR = 5 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 25 )

      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 800 )


!
!     Local variables
!
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(CORLEN)    CORRS  ( NCORR )
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(BDNMLN)    ILLUM
      CHARACTER*(LNSIZE)    METHOD
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(FRNMLN)    SRFREF
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(MSGLEN)    TITLE
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(TIMLEN)    TIME0
      CHARACTER*(LNSIZE)    TXT    ( TXTSIZ )
      CHARACTER*(CORLEN)    XMTCOR

      DOUBLE PRECISION      DEMSSN
      DOUBLE PRECISION      DFDT   ( 3 )
      DOUBLE PRECISION      DINCDN
      DOUBLE PRECISION      DLT
      DOUBLE PRECISION      DPHASE
      DOUBLE PRECISION      EMISSN
      DOUBLE PRECISION      EMISTA ( 2 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      ETSURF
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LT2
      DOUBLE PRECISION      LTSRC
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      NRMSTA ( 6 )
      DOUBLE PRECISION      OBSST2 ( 6 )
      DOUBLE PRECISION      OBSSTA ( 6 )
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      PHSSTA ( 2 )
      DOUBLE PRECISION      R      ( 3, 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SOLAR
      DOUBLE PRECISION      SOLSTA ( 2 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRCST2 ( 6 )
      DOUBLE PRECISION      SRCSTA ( 6 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SSTATE ( 6 )
      DOUBLE PRECISION      STATES ( 6, 2 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TMPSTA ( 6 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      XDEMIT
      DOUBLE PRECISION      XDSOLR
      DOUBLE PRECISION      XDPHAS
!
!     The workspace array has to handle the largest workspace
!     we'll use, which is that required by GFPOSC.
!
      INTEGER               CORIDX
      INTEGER               EPIDX
      INTEGER               HANDLE
      INTEGER               HAN2
      INTEGER               N
      INTEGER               OBSID
      INTEGER               OCTID

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND
      LOGICAL               GEOM
      LOGICAL               USECN
      LOGICAL               USELT
      LOGICAL               USESTL


!
!     Saved everything.
!
      SAVE

!
!     Initial values
!
      DATA                  CORRS  / 'NONE', 'CN', 'CN+S', 'LT', 'LT+S'/

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZILUSTA' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

      CALL TSTSPK ( SPK2, .TRUE., HAN2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!      Use a JPL DE for smoother planetary ephemerides. This
!      option is not available for a deliverable version of
!      this test family.
!
!      CALL FURNSH ( 'de421.bsp' )
!      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL TSTPCK ( PCK2, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!     Set up a confinement window. Initialize this and
!     the result window.
!
      TIME0 = '2000 JAN 1  00:00:00 TDB'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )






!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Invalid METHOD.' )

      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'Elipsoid'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'

      LON    =  100.D0 * RPD()
      LAT    =   30.D0 * RPD()

      CALL SRFREC ( 399, LON, LAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, NORMAL )

      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                      FIXREF, ABCORR, OBSRVR, SPOINT, &
                      NORMAL, PHSSTA, SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Zero normal vector.' )

      CALL CHCKXC ( .FALSE., ' ', OK )

      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'

      LON    =  100.D0 * RPD()
      LAT    =   30.D0 * RPD()

      CALL SRFREC ( 399, LON, LAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 0.D0, NORMAL )

      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                      FIXREF, ABCORR, OBSRVR, SPOINT, &
                      NORMAL, PHSSTA, SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)',   OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, NORMAL )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Unrecognized target, observer, or illumination ' &
      //           'source.'                                        )


      METHOD = 'ELLIPSOID'

      CALL ZZILUSTA ( METHOD, 'X',    ILLUM,  ET, &
                      FIXREF, ABCORR, OBSRVR, SPOINT, &
                      NORMAL, PHSSTA, SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL ZZILUSTA ( METHOD, TARGET, 'X',    ET, &
                      FIXREF, ABCORR, OBSRVR, SPOINT, &
                      NORMAL, PHSSTA, SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )



      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                      FIXREF, ABCORR, 'X',    SPOINT, &
                      NORMAL, PHSSTA, SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No SPK data for observer' )

      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,    ET, &
                      FIXREF, ABCORR, 'GASPRA', SPOINT, &
                      NORMAL, PHSSTA, SOLSTA,   EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No SPK data for target' )

      CALL ZZILUSTA ( METHOD, 'GASPRA', ILLUM,  ET, &
                      FIXREF, ABCORR,   OBSRVR, SPOINT, &
                      NORMAL, PHSSTA,   SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No SPK data for illumination source' )


      CALL ZZILUSTA ( METHOD, TARGET, 'GASPRA', ET, &
                      FIXREF, ABCORR, OBSRVR,   SPOINT, &
                      NORMAL, PHSSTA, SOLSTA,   EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No PCK orientation data for target.'  )

      FIXREF = 'ITRF93'

      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                      FIXREF, ABCORR, OBSRVR, SPOINT, &
                      NORMAL, PHSSTA, SOLSTA, EMISTA  )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Aberration correction is for transmission.'  )

      FIXREF = 'IAU_EARTH'

      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                      FIXREF, 'XCN',  OBSRVR, SPOINT, &
                      NORMAL, PHSSTA, SOLSTA, EMISTA  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************





!*********************************************************************
!*
!*    Comprehensive cases using comparisons against alternate
!*    computations
!*
!*********************************************************************


!*********************************************************************
!*
!*    PHASE angle tests
!*
!*********************************************************************

!
!     We'll use the earth, moon, and sun as the three participating
!     bodies. The surface point will be the OCTL telescope; we'll
!     create an SPK file for this point. We also need an FK for a
!     topocentric frame centered at the point.
!
!     Though it's not strictly necessary, we'll use real data for
!     these kernels, with one exception: we'll use the terrestrial
!     reference frame IAU_EARTH rather than ITRF93.
!
!     The original reference frame specifications follow:
!
!
!        Topocentric frame OCTL_TOPO
!
!           The Z axis of this frame points toward the zenith.
!           The X axis of this frame points North.
!
!           Topocentric frame OCTL_TOPO is centered at the site OCTL
!           which has Cartesian coordinates
!
!              X (km):                 -0.2448937761729E+04
!              Y (km):                 -0.4667935793438E+04
!              Z (km):                  0.3582748499430E+04
!
!           and planetodetic coordinates
!
!              Longitude (deg):      -117.6828380000000
!              Latitude  (deg):        34.3817491000000
!              Altitude   (km):         0.2259489999999E+01
!
!           These planetodetic coordinates are expressed relative to
!           a reference spheroid having the dimensions
!
!              Equatorial radius (km):  6.3781400000000E+03
!              Polar radius      (km):  6.3567523100000E+03
!
!           All of the above coordinates are relative to the frame
!           ITRF93.
!

!
!---- Case -------------------------------------------------------------
!

!
!     This isn't a test, but we'll call it that so we'll have
!     a meaningful label in any error messages that arise.
!
      CALL TCASE ( 'Create OCTL kernels.' )

!
!     As mentioned, we go with a frame that's more convenient than
!     ITRF93:
!
      FIXREF = 'IAU_EARTH'

!
!     Prepare a frame kernel in a string array.
!
      TXT( 1) = 'FRAME_OCTL_TOPO            =  1398962'
      TXT( 2) = 'FRAME_1398962_NAME         =  ''OCTL_TOPO'' '
      TXT( 3) = 'FRAME_1398962_CLASS        =  4'
      TXT( 4) = 'FRAME_1398962_CLASS_ID     =  1398962'
      TXT( 5) = 'FRAME_1398962_CENTER       =  398962'

      TXT( 6) = 'OBJECT_398962_FRAME        =  ''OCTL_TOPO'' '

      TXT( 7) = 'TKFRAME_1398962_RELATIVE   =  ''IAU_EARTH'' '
      TXT( 8) = 'TKFRAME_1398962_SPEC       =  ''ANGLES'' '
      TXT( 9) = 'TKFRAME_1398962_UNITS      =  ''DEGREES'' '
      TXT(10) = 'TKFRAME_1398962_AXES       =  ( 3, 2, 3 )'
      TXT(11) = 'TKFRAME_1398962_ANGLES     =  ( -242.3171620000000,'
      TXT(12) = '                                 -55.6182509000000,'
      TXT(13) = '                                 180.0000000000000  )'
      TXT(14) = 'NAIF_BODY_NAME            +=  ''OCTL'' '
      TXT(15) = 'NAIF_BODY_CODE            +=  398962'

!
!     It will be convenient to have a version of this frame that
!     has the +Z axis pointed down instead of up.
!
      TXT(16) = 'FRAME_OCTL_FLIP            =  2398962'
      TXT(17) = 'FRAME_2398962_NAME         =  ''OCTL_FLIP'' '
      TXT(18) = 'FRAME_2398962_CLASS        =  4'
      TXT(19) = 'FRAME_2398962_CLASS_ID     =  2398962'
      TXT(20) = 'FRAME_2398962_CENTER       =  398962'

      TXT(21) = 'TKFRAME_2398962_RELATIVE   =  ''OCTL_TOPO'' '
      TXT(22) = 'TKFRAME_2398962_SPEC       =  ''ANGLES'' '
      TXT(23) = 'TKFRAME_2398962_UNITS      =  ''DEGREES'' '
      TXT(24) = 'TKFRAME_2398962_AXES       =  ( 3, 2, 3 )'
      TXT(25) = 'TKFRAME_2398962_ANGLES     =  ( 0, 180.0, 0 ) '

      CALL LMPOOL ( TXT, TXTSIZ )
      CALL CHCKXC ( .FALSE., ' ',   OK )



!
!     Now create an SPK file containing a type 8 segment for OCTL.
!
      CALL SPKOPN ( OCTSPK, OCTSPK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Initialize both states to zero.
!
      CALL CLEARD ( 12, STATES )

!
!     The first position component:
!
      SPOINT(1) =   -0.2448937761729D4
      SPOINT(2) =   -0.4667935793438D4
      SPOINT(3) =    0.3582748499430D4

      CALL VEQU ( SPOINT, STATES(1,1) )

!
!     The second position matches the first: we don't model
!     plate motion.
!
      CALL VEQU ( SPOINT, STATES(1,2) )

!
!     Time bounds for the segment:
!

      FIRST = ( -50 ) * SPD() * 365.25D0
      STEP  = ( 100 ) * SPD() * 365.25D0

      LAST  = FIRST + STEP - 1.D-6

!
!     Get the OCTL ID code from the kernel we just
!     loaded.
!
      CALL BODN2C ( OCTL, OCTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )

!
!     Write the segment.
!
      CALL SPKW08 ( HANDLE, OCTID,  399,     FIXREF, &
                    FIRST,  LAST,   'octl',  1, &
                    2,      STATES, FIRST,   STEP   )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Now load the OCTL SPK file.
!
      CALL FURNSH ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Create an outward normal vector at SPOINT.
!
      CALL BODVRD ( 'EARTH', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SURFNM ( RADII(1),  RADII(2),  RADII(3), SPOINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Phase angle tests: we'll compare results from GFILUM to those
!     obtained using GFPA. Note that the surface point must be
!     an ephemeris object in order to carry out these tests.
!
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      OBSRVR = 'MOON'


!
!     Set up the sequence of test times.
!
      TIME0 = '2011 JAN 1'

      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Search over approximately two months. Take NTIMES samples.
!
      ET1  = ET0 + ( 60 * SPD() )

      STEP = ( ET1 - ET0 ) / ( NTIMES - 1 )

!
!     Loop over all aberration corrections and all
!     observation epochs.
!
      DO CORIDX = 1, NCORR

         ABCORR = CORRS( CORIDX )

!
!        Parse the aberration correction specifier.
!
         CALL ZZPRSCOR ( ABCORR, ATTBLK )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         USELT  = ATTBLK( LTIDX  )
         USESTL = ATTBLK( STLIDX )
         USECN  = ATTBLK( CNVIDX )
         GEOM   = ATTBLK( GEOIDX )

         DO EPIDX = 1, NTIMES

!
!---- Case -------------------------------------------------------------
!
            ET = ET0 + (EPIDX-1)*STEP

            CALL TIMOUT ( ET, &
                          'YYYY MON DD HR:MN:SC.######::TDB TDB', &
                          TIMSTR                                 )

            TITLE = 'ZZILUSTA test: ABCORR = #; ' &
            //      'observer = #; target = #; illum source = #; ' &
            //      'FIXREF = #; SPOINT = ( # # # ); ET = # (#); ' &
            //      'EPIDX = #.'

            CALL REPMC ( TITLE, '#', ABCORR,          TITLE )
            CALL REPMC ( TITLE, '#', OBSRVR,          TITLE )
            CALL REPMC ( TITLE, '#', TARGET,          TITLE )
            CALL REPMC ( TITLE, '#', ILLUM,           TITLE )
            CALL REPMC ( TITLE, '#', FIXREF,          TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(1),   14, TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(2),   14, TITLE )
            CALL REPMD ( TITLE, '#', SPOINT(3),   14, TITLE )
            CALL REPMC ( TITLE, '#', TIMSTR,          TITLE )
            CALL REPMD ( TITLE, '#', ET,          14, TITLE )
            CALL REPMI ( TITLE, '#', EPIDX,           TITLE )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL TCASE ( TITLE )

!            CALL TOSTDO ( TITLE )

!
!           Look up illumination angle states.
!
            CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                            FIXREF, ABCORR, OBSRVR, SPOINT, &
                            NORMAL, PHSSTA, SOLSTA, EMISTA  )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           First compare angles against those from ILUMIN.
!           The algorithms used should be quite similar,
!           so we can use a tight tolerance.
!
            CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                          ABCORR, OBSRVR, SPOINT, TRGEPC, &
                          SRFVEC, PHASE,  SOLAR,  EMISSN  )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           Check the phase angle.
!
            CALL CHCKSD ( 'PHASE', PHSSTA,  '~/', PHASE,  ANGTOL, OK )

!
!           Check the solar incidence angle.
!
            CALL CHCKSD ( 'SOLAR', SOLSTA,  '~/', SOLAR,  ANGTOL, OK )

!
!           Check the emission angle.
!
            CALL CHCKSD ( 'EMISTA', EMISTA, '~/', EMISSN, ANGTOL, OK )

!
!           Now compute illumination angles via an independent
!           algorithm.
!
!           This algorithm computes aberration corrections for
!           the illumination source less accurately, so adjust
!           the tolerance when aberration corrections are used.
!
            IF (  ATTBLK( GEOIDX )  ) THEN
               TOL = ANGTOL
            ELSE
               TOL = SINGLE
            END IF

            CALL T_ILUMIN ( METHOD, TARGET, ET,     FIXREF, &
                            ABCORR, OBSRVR, SPOINT, TRGEPC, &
                            SRFVEC, PHASE,  SOLAR,  EMISSN  )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           Check the phase angle.
!
            CALL CHCKSD ( 'PHASE (2)', PHSSTA, '~', PHASE, TOL, OK )

!
!           Check the solar incidence angle.
!
            CALL CHCKSD ( 'SOLAR (2)', SOLSTA, '~', SOLAR,  TOL, OK )

!
!           Check the emission angle.
!
            CALL CHCKSD ( 'EMISTA (2)', EMISTA, '~', EMISSN, TOL, OK )

!
!           Now for the interesting part: check the angular rates.
!           We'll compute discrete derivatives of each one and
!           compare these to the rates produced by ZZILUSTA.
!
!           Fit Cheby expansions to the illumination angles
!           and find derivatives of the expansion wrt time at ET.
!
            CALL T_DZZILU ( METHOD, TARGET, ILLUM,  ET, &
                            FIXREF, ABCORR, OBSRVR, SPOINT, &
                            NORMAL, DPHASE, DINCDN, DEMSSN  )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DFDT(1) = DPHASE
            DFDT(2) = DINCDN
            DFDT(3) = DEMSSN

            CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET, &
                            FIXREF, ABCORR, OBSRVR, SPOINT, &
                            NORMAL, PHSSTA, SOLSTA, EMISTA  )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!            WRITE (*,*) 'Cheby Phase rate:          ', DPHASE
!            WRITE (*,*) 'ZZILUSTA Phase rate:  ', PHSSTA(2)
!            WRITE (*,*) 'Difference:           ', DPHASE-PHSSTA(2)
!            WRITE (*,*) 'Phase rate rel error:      ',
!     .                                  (DPHASE-PHSSTA(2))/DPHASE
!            WRITE (*,*) 'Cheby Incidence rate:      ', DINCDN
!            WRITE (*,*) 'Incidence rate rel error:  ',
!     .                                  (DINCDN-SOLSTA(2))/DINCDN
!            WRITE (*,*) 'Cheby Emission rate:       ', DEMSSN
!            WRITE (*,*) 'Emission rate rel error:   ',
!     .                                  (DEMSSN-EMISTA(2))/DEMSSN
!
!            WRITE (*,*) ' '


!
!           Check the phase angle rate.
!
            IF ( GEOM ) THEN
               TOL = DPHTOL
            ELSE
               TOL = DPHTOL * 1.5D0
            END IF

            CALL CHCKSD ( 'd PHASE/dt',  PHSSTA(2),  '~/', &
                                         DFDT(1),    TOL, OK )

!
!           Check the solar incidence angle rate.
!
            TOL = DINTOL

            CALL CHCKSD ( 'd SOLAR/dt',  SOLSTA(2),  '~/', &
                                         DFDT(2),    TOL, OK )
!
!           Check the emission angle rate.
!
            TOL = DEMTOL


            CALL CHCKSD ( 'd EMISSN/dt', EMISTA(2),  '~/', &
                                         DFDT(3),   TOL, OK )

!
!           Now check the angular rates using an alternate
!           analytic method. We'll do the computation inline.
!
!           We'll use a body-fixed frame to compute the
!           angles and rates. Life will be easier if the
!           frame is centered at the surface point.
!
            SRFREF = 'OCTL_TOPO'

!
!           Get the observer-surface point state at ET in the surface
!           point centered, body-fixed frame. Negate this state to
!           obtain the surface point-observer state.
!
            CALL SPKEZR ( 'OCTL', ET,     SRFREF, &
                          ABCORR, OBSRVR, SSTATE, LT )

            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL VMINUG ( SSTATE, 6, OBSSTA )

!
!           Compute the epoch associated with the surface point.
!
!           Also compute the rate of change of light time.
!
            CALL BODN2C ( OBSRVR, OBSID, FOUND )
            CALL CHCKXC ( .FALSE., ' ',   OK )
            CALL CHCKSL ( 'OBSID FOUND', FOUND, .TRUE., OK )


            CALL SPKACS ( OCTID,  ET,  IREF, ABCORR, OBSID, &
                          TMPSTA, LT2, DLT                 )

            IF (  ATTBLK( GEOIDX )  ) THEN

               ETSURF = ET
               DLT    = 0.D0

            ELSE

               ETSURF = ET - LT

            END IF

!
!           Compute the surface point-illumination source state
!           at ETSURF in the surface point body-fixed frame.
!
            CALL SPKEZR ( ILLUM,  ETSURF, SRFREF, ABCORR, &
                          'OCTL', SRCSTA, LTSRC           )
            CALL CHCKXC ( .FALSE., ' ',   OK )

!
!           Adjust the surface point-illumination source velocity
!           for the rate of change of ETSURF with respect to ET.
!
            CALL VSCLIP ( 1.D0 - DLT, SRCSTA(4) )

!
!           Create a state for the normal vector. Transform
!           the normal vector to frame SRFREF.
!
            CALL PXFORM ( FIXREF, SRFREF, ETSURF, R )
            CALL MXV    ( R,      NORMAL, NRMSTA    )
            CALL CLEARD ( 3,              NRMSTA(4) )

!
!           Compute and check expected rates.
!
!           Phase angle rate:
!
            XDPHAS = DVSEP ( OBSSTA, SRCSTA )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd PHASE/dt (2)', PHSSTA(2),  '~/', &
                                            XDPHAS,    ALTTOL, OK )
!
!           Emission angle rate:
!
            XDEMIT = DVSEP ( OBSSTA, NRMSTA )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd EMISSN/dt (2)', EMISTA(2),  '~/', &
                                             XDEMIT,    ALTTOL, OK )

!
!           Solar incidence angle rate:
!
            XDSOLR = DVSEP ( SRCSTA, NRMSTA )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd SOLAR/dt (2)',  SOLSTA(2),  '~/', &
                                             XDSOLR,    ALTTOL, OK )

!
!           Now compute the rates using a second, alternate,
!           analytic method.
!
!           This time we compute rates using ETSURF as the
!           observation epoch. After computing the rates,
!           we adjust them so they express the rates of change
!           of the illumination angles with respect to ET.
!
!           Start out by creating a transmission aberration
!           correction specifier, if corrections are being
!           used.
!
            IF ( USELT ) THEN

               CALL LJUST  ( ABCORR, XMTCOR )
               CALL PREFIX ( 'X', 0, XMTCOR )
            ELSE
               DLT    = 0.D0
               XMTCOR = 'NONE'
            END IF

!
!           Compute the surface point-observer state at ETSURF.
!
            IF ( USESTL ) THEN
!
!              Due to the asymmetry of stellar aberration
!              corrections, it's easiest to compute the
!              surface point-observer vector in the usual
!              way.
!
               CALL SPKEZR ( 'OCTL', ET,     SRFREF, ABCORR, &
                             OBSRVR, SSTATE, LT             )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL VMINUG ( SSTATE, 6, OBSST2 )

!
!              Adjust the velocity portion of the state so
!              that it is the derivative of position with
!              respect to ETSURF rather than ET.
!
               CALL VSCLIP ( 1.D0/( 1.D0 - DLT ), OBSST2(4) )

            ELSE
!
!              We can compute the surface point-observer state
!              directly using transmission corrections and
!              an event time of ETSURF.
!
               CALL SPKEZR ( OBSRVR, ETSURF, SRFREF, &
                             XMTCOR, 'OCTL', OBSST2, LT )
               CALL CHCKXC ( .FALSE., ' ',   OK )

            END IF

!
!           Compute the surface point-illumination source
!           state at ETSURF.
!
            CALL SPKEZR ( ILLUM,  ETSURF, SRFREF, &
                          ABCORR, 'OCTL', SRCST2, LT )

            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL MOVED ( OBSST2, 6, TMPSTA )

            CALL VSCLIP ( 1.D0 - DLT,  TMPSTA(4) )

            CALL MOVED ( SRCST2, 6, TMPSTA )

            CALL VSCLIP ( 1.D0 - DLT,  TMPSTA(4) )
!
!           We already have the state of the normal vector.
!
!
!           Compute the phase angle rates; scale each one
!           by the rate of change of ETSURF with respect to
!           ET.
!
            IF ( GEOM .OR. USECN ) THEN

               TOL = ALTTOL
            ELSE
               TOL = MEDTOL
            END IF
!
!           Phase angle rate:
!
            XDPHAS = DVSEP ( OBSST2, SRCST2 ) * ( 1.D0 - DLT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd PHASE/dt (3)',  PHSSTA(2),  '~/', &
                                             XDPHAS,     TOL, OK )
!
!           Emission angle rate:
!
            XDEMIT = DVSEP ( OBSST2, NRMSTA )  * ( 1.D0 - DLT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd EMISSN/dt (3)', EMISTA(2),  '~/', &
                                             XDEMIT,     TOL, OK )

!
!           Solar incidence angle rate:
!
            XDSOLR = DVSEP ( SRCST2, NRMSTA )  * ( 1.D0 - DLT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd SOLAR/dt (3)',  SOLSTA(2),  '~/', &
                                             XDSOLR,     TOL, OK )

         END DO
!
!        End of epoch loop.
!

      END DO
!
!     End of aberration correction loop.
!

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

!
!     Clean up SPK files.
!
      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL UNLOAD ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL T_SUCCESS ( OK )

      RETURN
      END
