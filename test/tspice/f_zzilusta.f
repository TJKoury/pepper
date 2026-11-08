C$Procedure      F_ZZILUSTA ( ZZILUSTA tests )

      SUBROUTINE F_ZZILUSTA ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        ZZILUSTA
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations
 
      IMPLICIT NONE
      
      INCLUDE 'gf.inc'
      INCLUDE 'zzabcorr.inc'

      LOGICAL               OK
 
C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK         is a logical that indicates the test status to the
C                caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     This routine does not generate any errors. Routines in its
C     call tree may generate errors that are either intentional and
C     trapped or unintentional and need reporting.  The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the private SPICELIB 
C     geometry routine ZZILUSTA.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 02-APR-2014 (NJB)
C
C        Added test case for zero normal vector.
C
C     Last update was 27-AUG-2013 
C
C        Tolerance ANGTOL was loosened from 1.D-14
C        to 1.D-11.
C
C-    TSPICE Version 1.0.0, 02-MAY-2012 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DVSEP
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD

C     
C     Local parameters
C
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

C
C
C
      DOUBLE PRECISION      ANGTOL
      PARAMETER           ( ANGTOL = 1.D-11 )

      DOUBLE PRECISION      SINGLE
      PARAMETER           ( SINGLE = 5.D-6 )

C
C     Tolerances for tests involving discrete derivatives
C     of illumination angle rate. These are applicable 
C     when aberration corrections are turned off, or when
C     a "smooth" planetary ephemeris such as DE421.bsp is
C     used.
C     
      DOUBLE PRECISION      DPHTOL
      PARAMETER           ( DPHTOL = 1.0D-6 )

      DOUBLE PRECISION      DINTOL
      PARAMETER           ( DINTOL = 5.0D-7 )

      DOUBLE PRECISION      DEMTOL
      PARAMETER           ( DEMTOL = 5.0D-7 )

C
C     Tolerance for illumination angle tests using
C     alternate computation methods:
C
      DOUBLE PRECISION      ALTTOL
      PARAMETER           ( ALTTOL = 1.0D-10 )

      DOUBLE PRECISION      MEDTOL
      PARAMETER           ( MEDTOL = 1.D-7 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

C
C     Number of test epochs:
C
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


C
C     Local variables
C
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
C
C     The workspace array has to handle the largest workspace
C     we'll use, which is that required by GFPOSC.
C
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
      

C
C     Saved everything.
C
      SAVE
      
C
C     Initial values
C
      DATA                  CORRS  / 'NONE', 'CN', 'CN+S', 'LT', 'LT+S'/
     .                             
C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZILUSTA' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

      CALL TSTSPK ( SPK2, .TRUE., HAN2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C      Use a JPL DE for smoother planetary ephemerides. This
C      option is not available for a deliverable version of
C      this test family.
C
C      CALL FURNSH ( 'de421.bsp' )
C      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL TSTPCK ( PCK2, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )
      
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C     Set up a confinement window. Initialize this and 
C     the result window.
C
      TIME0 = '2000 JAN 1  00:00:00 TDB'
      
      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

 




C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C---- Case -------------------------------------------------------------
C
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

      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,
     .                FIXREF, ABCORR, OBSRVR, SPOINT,
     .                NORMAL, PHSSTA, SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)',   OK )


C
C---- Case -------------------------------------------------------------
C
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

      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,
     .                FIXREF, ABCORR, OBSRVR, SPOINT,
     .                NORMAL, PHSSTA, SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)',   OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, NORMAL )

 
 
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized target, observer, or illumination '
     .//           'source.'                                        )

  
      METHOD = 'ELLIPSOID'

      CALL ZZILUSTA ( METHOD, 'X',    ILLUM,  ET,
     .                FIXREF, ABCORR, OBSRVR, SPOINT,
     .                NORMAL, PHSSTA, SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL ZZILUSTA ( METHOD, TARGET, 'X',    ET,
     .                FIXREF, ABCORR, OBSRVR, SPOINT,
     .                NORMAL, PHSSTA, SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


  
      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,
     .                FIXREF, ABCORR, 'X',    SPOINT,
     .                NORMAL, PHSSTA, SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )
                                   


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for observer' )

      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,    ET,
     .                FIXREF, ABCORR, 'GASPRA', SPOINT,
     .                NORMAL, PHSSTA, SOLSTA,   EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for target' )

      CALL ZZILUSTA ( METHOD, 'GASPRA', ILLUM,  ET,
     .                FIXREF, ABCORR,   OBSRVR, SPOINT,
     .                NORMAL, PHSSTA,   SOLSTA, EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for illumination source' )


      CALL ZZILUSTA ( METHOD, TARGET, 'GASPRA', ET,
     .                FIXREF, ABCORR, OBSRVR,   SPOINT,
     .                NORMAL, PHSSTA, SOLSTA,   EMISTA  )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No PCK orientation data for target.'  )

      FIXREF = 'ITRF93'

      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,
     .                FIXREF, ABCORR, OBSRVR, SPOINT,
     .                NORMAL, PHSSTA, SOLSTA, EMISTA  )
  
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )

 
   
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Aberration correction is for transmission.'  )
      
      FIXREF = 'IAU_EARTH'

      CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,
     .                FIXREF, 'XCN',  OBSRVR, SPOINT,
     .                NORMAL, PHSSTA, SOLSTA, EMISTA  )
  
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************



      

C*********************************************************************
C*
C*    Comprehensive cases using comparisons against alternate
C*    computations
C*
C*********************************************************************


C*********************************************************************
C*
C*    PHASE angle tests
C*
C*********************************************************************

C
C     We'll use the earth, moon, and sun as the three participating
C     bodies. The surface point will be the OCTL telescope; we'll
C     create an SPK file for this point. We also need an FK for a
C     topocentric frame centered at the point.
C
C     Though it's not strictly necessary, we'll use real data for
C     these kernels, with one exception: we'll use the terrestrial
C     reference frame IAU_EARTH rather than ITRF93.
C
C     The original reference frame specifications follow:
C
C
C        Topocentric frame OCTL_TOPO
C
C           The Z axis of this frame points toward the zenith.
C           The X axis of this frame points North.
C
C           Topocentric frame OCTL_TOPO is centered at the site OCTL
C           which has Cartesian coordinates
C
C              X (km):                 -0.2448937761729E+04
C              Y (km):                 -0.4667935793438E+04
C              Z (km):                  0.3582748499430E+04
C
C           and planetodetic coordinates
C
C              Longitude (deg):      -117.6828380000000
C              Latitude  (deg):        34.3817491000000
C              Altitude   (km):         0.2259489999999E+01
C
C           These planetodetic coordinates are expressed relative to
C           a reference spheroid having the dimensions
C
C              Equatorial radius (km):  6.3781400000000E+03
C              Polar radius      (km):  6.3567523100000E+03
C
C           All of the above coordinates are relative to the frame
C           ITRF93.
C

C
C---- Case -------------------------------------------------------------
C

C
C     This isn't a test, but we'll call it that so we'll have
C     a meaningful label in any error messages that arise.
C
      CALL TCASE ( 'Create OCTL kernels.' )
      
C
C     As mentioned, we go with a frame that's more convenient than
C     ITRF93:
C
      FIXREF = 'IAU_EARTH'

C
C     Prepare a frame kernel in a string array.
C
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

C
C     It will be convenient to have a version of this frame that
C     has the +Z axis pointed down instead of up.
C
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

     

C
C     Now create an SPK file containing a type 8 segment for OCTL.
C
      CALL SPKOPN ( OCTSPK, OCTSPK, 0, HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Initialize both states to zero.
C
      CALL CLEARD ( 12, STATES )

C
C     The first position component:
C
      SPOINT(1) =   -0.2448937761729D4
      SPOINT(2) =   -0.4667935793438D4
      SPOINT(3) =    0.3582748499430D4

      CALL VEQU ( SPOINT, STATES(1,1) )

C
C     The second position matches the first: we don't model
C     plate motion.
C
      CALL VEQU ( SPOINT, STATES(1,2) )
      
C
C     Time bounds for the segment:
C
     
      FIRST = ( -50 ) * SPD() * 365.25D0
      STEP  = ( 100 ) * SPD() * 365.25D0

      LAST  = FIRST + STEP - 1.D-6

C
C     Get the OCTL ID code from the kernel we just
C     loaded.
C
      CALL BODN2C ( OCTL, OCTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE.,  OK )
      
C
C     Write the segment.
C
      CALL SPKW08 ( HANDLE, OCTID,  399,     FIXREF, 
     .              FIRST,  LAST,   'octl',  1, 
     .              2,      STATES, FIRST,   STEP   )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SPKCLS ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Now load the OCTL SPK file.
C
      CALL FURNSH ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Create an outward normal vector at SPOINT.
C
      CALL BODVRD ( 'EARTH', 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SURFNM ( RADII(1),  RADII(2),  RADII(3), SPOINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Phase angle tests: we'll compare results from GFILUM to those
C     obtained using GFPA. Note that the surface point must be
C     an ephemeris object in order to carry out these tests.
C     
      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      OBSRVR = 'MOON'


C
C     Set up the sequence of test times.
C
      TIME0 = '2011 JAN 1'
      
      CALL STR2ET ( TIME0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Search over approximately two months. Take NTIMES samples.
C
      ET1  = ET0 + ( 60 * SPD() )
 
      STEP = ( ET1 - ET0 ) / ( NTIMES - 1 )

C
C     Loop over all aberration corrections and all 
C     observation epochs.
C
      DO CORIDX = 1, NCORR

         ABCORR = CORRS( CORIDX )

C
C        Parse the aberration correction specifier.
C        
         CALL ZZPRSCOR ( ABCORR, ATTBLK )
         CALL CHCKXC ( .FALSE., ' ',   OK )

         USELT  = ATTBLK( LTIDX  )
         USESTL = ATTBLK( STLIDX )
         USECN  = ATTBLK( CNVIDX )
         GEOM   = ATTBLK( GEOIDX )

         DO EPIDX = 1, NTIMES

C
C---- Case -------------------------------------------------------------
C
            ET = ET0 + (EPIDX-1)*STEP

            CALL TIMOUT ( ET, 
     .                    'YYYY MON DD HR:MN:SC.######::TDB TDB',
     .                    TIMSTR                                 )

            TITLE = 'ZZILUSTA test: ABCORR = #; '
     .      //      'observer = #; target = #; illum source = #; '
     .      //      'FIXREF = #; SPOINT = ( # # # ); ET = # (#); '
     .      //      'EPIDX = #.'

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

C            CALL TOSTDO ( TITLE )

C
C           Look up illumination angle states.
C
            CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,
     .                      FIXREF, ABCORR, OBSRVR, SPOINT,
     .                      NORMAL, PHSSTA, SOLSTA, EMISTA  )
            CALL CHCKXC ( .FALSE., ' ',   OK )
          
C
C           First compare angles against those from ILUMIN.
C           The algorithms used should be quite similar,
C           so we can use a tight tolerance.
C
            CALL ILUMIN ( METHOD, TARGET, ET,     FIXREF,
     .                    ABCORR, OBSRVR, SPOINT, TRGEPC,
     .                    SRFVEC, PHASE,  SOLAR,  EMISSN  )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           Check the phase angle.
C
            CALL CHCKSD ( 'PHASE', PHSSTA,  '~/', PHASE,  ANGTOL, OK )

C
C           Check the solar incidence angle.
C
            CALL CHCKSD ( 'SOLAR', SOLSTA,  '~/', SOLAR,  ANGTOL, OK )

C
C           Check the emission angle.
C
            CALL CHCKSD ( 'EMISTA', EMISTA, '~/', EMISSN, ANGTOL, OK )

C
C           Now compute illumination angles via an independent
C           algorithm.
C
C           This algorithm computes aberration corrections for
C           the illumination source less accurately, so adjust
C           the tolerance when aberration corrections are used.
C
            IF (  ATTBLK( GEOIDX )  ) THEN
               TOL = ANGTOL
            ELSE
               TOL = SINGLE
            END IF

            CALL T_ILUMIN ( METHOD, TARGET, ET,     FIXREF,
     .                      ABCORR, OBSRVR, SPOINT, TRGEPC,
     .                      SRFVEC, PHASE,  SOLAR,  EMISSN  )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           Check the phase angle.
C
            CALL CHCKSD ( 'PHASE (2)', PHSSTA, '~', PHASE, TOL, OK )

C
C           Check the solar incidence angle.
C
            CALL CHCKSD ( 'SOLAR (2)', SOLSTA, '~', SOLAR,  TOL, OK )

C
C           Check the emission angle.
C
            CALL CHCKSD ( 'EMISTA (2)', EMISTA, '~', EMISSN, TOL, OK )

C
C           Now for the interesting part: check the angular rates.
C           We'll compute discrete derivatives of each one and
C           compare these to the rates produced by ZZILUSTA. 
C
C           Fit Cheby expansions to the illumination angles
C           and find derivatives of the expansion wrt time at ET.
C
            CALL T_DZZILU ( METHOD, TARGET, ILLUM,  ET,  
     .                      FIXREF, ABCORR, OBSRVR, SPOINT,
     .                      NORMAL, DPHASE, DINCDN, DEMSSN  )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            DFDT(1) = DPHASE
            DFDT(2) = DINCDN
            DFDT(3) = DEMSSN
            
            CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,
     .                      FIXREF, ABCORR, OBSRVR, SPOINT,
     .                      NORMAL, PHSSTA, SOLSTA, EMISTA  )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C            WRITE (*,*) 'Cheby Phase rate:          ', DPHASE
C            WRITE (*,*) 'ZZILUSTA Phase rate:  ', PHSSTA(2)
C            WRITE (*,*) 'Difference:           ', DPHASE-PHSSTA(2)
C            WRITE (*,*) 'Phase rate rel error:      ', 
C     .                                  (DPHASE-PHSSTA(2))/DPHASE
C            WRITE (*,*) 'Cheby Incidence rate:      ', DINCDN
C            WRITE (*,*) 'Incidence rate rel error:  ', 
C     .                                  (DINCDN-SOLSTA(2))/DINCDN
C            WRITE (*,*) 'Cheby Emission rate:       ', DEMSSN
C            WRITE (*,*) 'Emission rate rel error:   ', 
C     .                                  (DEMSSN-EMISTA(2))/DEMSSN
C
C            WRITE (*,*) ' '


C
C           Check the phase angle rate.
C
            IF ( GEOM ) THEN
               TOL = DPHTOL
            ELSE
               TOL = DPHTOL * 1.5D0
            END IF

            CALL CHCKSD ( 'd PHASE/dt',  PHSSTA(2),  '~/', 
     .                                   DFDT(1),    TOL, OK )

C
C           Check the solar incidence angle rate.
C
            TOL = DINTOL

            CALL CHCKSD ( 'd SOLAR/dt',  SOLSTA(2),  '~/', 
     .                                   DFDT(2),    TOL, OK )
C
C           Check the emission angle rate.
C
            TOL = DEMTOL
             

            CALL CHCKSD ( 'd EMISSN/dt', EMISTA(2),  '~/', 
     .                                   DFDT(3),   TOL, OK )

C
C           Now check the angular rates using an alternate
C           analytic method. We'll do the computation inline.
C
C           We'll use a body-fixed frame to compute the
C           angles and rates. Life will be easier if the
C           frame is centered at the surface point.
C
            SRFREF = 'OCTL_TOPO'

C
C           Get the observer-surface point state at ET in the surface
C           point centered, body-fixed frame. Negate this state to
C           obtain the surface point-observer state.
C
            CALL SPKEZR ( 'OCTL', ET,     SRFREF, 
     .                    ABCORR, OBSRVR, SSTATE, LT ) 

            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL VMINUG ( SSTATE, 6, OBSSTA )

C
C           Compute the epoch associated with the surface point.
C           
C           Also compute the rate of change of light time.
C
            CALL BODN2C ( OBSRVR, OBSID, FOUND )
            CALL CHCKXC ( .FALSE., ' ',   OK )
            CALL CHCKSL ( 'OBSID FOUND', FOUND, .TRUE., OK )


            CALL SPKACS ( OCTID,  ET,  IREF, ABCORR, OBSID, 
     .                    TMPSTA, LT2, DLT                 )

            IF (  ATTBLK( GEOIDX )  ) THEN

               ETSURF = ET               
               DLT    = 0.D0

            ELSE

               ETSURF = ET - LT

            END IF
 
C
C           Compute the surface point-illumination source state
C           at ETSURF in the surface point body-fixed frame.
C
            CALL SPKEZR ( ILLUM,  ETSURF, SRFREF, ABCORR,
     .                    'OCTL', SRCSTA, LTSRC           )
            CALL CHCKXC ( .FALSE., ' ',   OK )

C
C           Adjust the surface point-illumination source velocity
C           for the rate of change of ETSURF with respect to ET.
C
            CALL VSCLIP ( 1.D0 - DLT, SRCSTA(4) )

C
C           Create a state for the normal vector. Transform
C           the normal vector to frame SRFREF.
C           
            CALL PXFORM ( FIXREF, SRFREF, ETSURF, R )
            CALL MXV    ( R,      NORMAL, NRMSTA    )
            CALL CLEARD ( 3,              NRMSTA(4) )

C
C           Compute and check expected rates.
C
C           Phase angle rate:
C
            XDPHAS = DVSEP ( OBSSTA, SRCSTA )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd PHASE/dt (2)', PHSSTA(2),  '~/', 
     .                                      XDPHAS,    ALTTOL, OK )
C
C           Emission angle rate:
C
            XDEMIT = DVSEP ( OBSSTA, NRMSTA )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd EMISSN/dt (2)', EMISTA(2),  '~/', 
     .                                       XDEMIT,    ALTTOL, OK )

C
C           Solar incidence angle rate:
C
            XDSOLR = DVSEP ( SRCSTA, NRMSTA )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd SOLAR/dt (2)',  SOLSTA(2),  '~/', 
     .                                       XDSOLR,    ALTTOL, OK )

C
C           Now compute the rates using a second, alternate,
C           analytic method. 
C
C           This time we compute rates using ETSURF as the
C           observation epoch. After computing the rates,
C           we adjust them so they express the rates of change
C           of the illumination angles with respect to ET.
C
C           Start out by creating a transmission aberration 
C           correction specifier, if corrections are being
C           used.
C          
            IF ( USELT ) THEN

               CALL LJUST  ( ABCORR, XMTCOR )
               CALL PREFIX ( 'X', 0, XMTCOR )
            ELSE
               DLT    = 0.D0
               XMTCOR = 'NONE'
            END IF
            
C
C           Compute the surface point-observer state at ETSURF.
C
            IF ( USESTL ) THEN
C
C              Due to the asymmetry of stellar aberration
C              corrections, it's easiest to compute the
C              surface point-observer vector in the usual
C              way.
C
               CALL SPKEZR ( 'OCTL', ET,     SRFREF, ABCORR, 
     .                       OBSRVR, SSTATE, LT             )
               CALL CHCKXC ( .FALSE., ' ',   OK )

               CALL VMINUG ( SSTATE, 6, OBSST2 )

C
C              Adjust the velocity portion of the state so
C              that it is the derivative of position with
C              respect to ETSURF rather than ET.
C
               CALL VSCLIP ( 1.D0/( 1.D0 - DLT ), OBSST2(4) )
               
            ELSE
C
C              We can compute the surface point-observer state
C              directly using transmission corrections and
C              an event time of ETSURF.
C
               CALL SPKEZR ( OBSRVR, ETSURF, SRFREF, 
     .                       XMTCOR, 'OCTL', OBSST2, LT )
               CALL CHCKXC ( .FALSE., ' ',   OK )

            END IF

C
C           Compute the surface point-illumination source 
C           state at ETSURF.
C
            CALL SPKEZR ( ILLUM,  ETSURF, SRFREF, 
     .                    ABCORR, 'OCTL', SRCST2, LT )

            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL MOVED ( OBSST2, 6, TMPSTA )

            CALL VSCLIP ( 1.D0 - DLT,  TMPSTA(4) )

            CALL MOVED ( SRCST2, 6, TMPSTA )

            CALL VSCLIP ( 1.D0 - DLT,  TMPSTA(4) )
C
C           We already have the state of the normal vector.
C
C
C           Compute the phase angle rates; scale each one 
C           by the rate of change of ETSURF with respect to
C           ET.
C
            IF ( GEOM .OR. USECN ) THEN

               TOL = ALTTOL
            ELSE
               TOL = MEDTOL
            END IF
C
C           Phase angle rate:
C
            XDPHAS = DVSEP ( OBSST2, SRCST2 ) * ( 1.D0 - DLT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd PHASE/dt (3)',  PHSSTA(2),  '~/', 
     .                                       XDPHAS,     TOL, OK )
C
C           Emission angle rate:
C
            XDEMIT = DVSEP ( OBSST2, NRMSTA )  * ( 1.D0 - DLT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd EMISSN/dt (3)', EMISTA(2),  '~/', 
     .                                       XDEMIT,     TOL, OK )

C
C           Solar incidence angle rate:
C
            XDSOLR = DVSEP ( SRCST2, NRMSTA )  * ( 1.D0 - DLT )
            CALL CHCKXC ( .FALSE., ' ',   OK )

            CALL CHCKSD ( 'd SOLAR/dt (3)',  SOLSTA(2),  '~/', 
     .                                       XDSOLR,     TOL, OK )

         END DO
C
C        End of epoch loop.
C

      END DO
C
C     End of aberration correction loop.
C

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

C
C     Clean up SPK files.
C      
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
