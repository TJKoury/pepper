C$Procedure F_GE01 ( SPICE higher-level geometry routine tests )
 
      SUBROUTINE F_GE01 ( OK )
 
C$ Abstract
C
C     Exercise higher-level SPICELIB geometry routines.
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
C$ Files
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
C$ Particulars
C
C     This routine tests a subset of the higher-level SPICELIB 
C     geometry routines:
C
C        ILLUM
C        SUBPT
C        SUBSOL
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman     (JPL)
C
C$ Literature_References
C
C     None.
C
C$ Version
C
C-    TSPICE Version 1.0.0, 05-OCT-2005 (NJB)
C
C
C-&
 
C
C     SPICELIB functions
C

C
C     Local Parameters
C
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'test.tpc' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'test.bsp' )

      CHARACTER*(*)         UTC
      PARAMETER           ( UTC    = '1999 JAN 1' )


      DOUBLE PRECISION      LOOSE
      PARAMETER           ( LOOSE  = 1.D-7 )
      
      DOUBLE PRECISION      MED
      PARAMETER           ( MED    = 1.D-11 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )


      INTEGER               MSGLEN
      PARAMETER           ( MSGLEN = 240 )




C
C     Local Variables
C
      CHARACTER*(MSGLEN)    TITLE

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      EMISSN
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      EXPPNT ( 3 )
      DOUBLE PRECISION      F
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      RAD
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RE
      DOUBLE PRECISION      RP
      DOUBLE PRECISION      SOLAR
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SUNALT
      DOUBLE PRECISION      SUNLAT
      DOUBLE PRECISION      SUNLON
      DOUBLE PRECISION      SUNRAD
      DOUBLE PRECISION      SUNSTA ( 6 )

      INTEGER               DIM
      INTEGER               HANDLE
           



C
C     Initial values
C
 

C
C     Open the test family.
C
      CALL TOPEN ( 'F_GE01' )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup:  load kernels.' )

C
C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create and load a PCK file. Delete the file afterwards.
C
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load an SPK file as well.
C     
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Test SUBPT.  Find the sub-solar point of the sun '//
     .        'on the Earth using the INTERCEPT definition.'

      CALL TCASE  ( TITLE )

      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPT  ( 'INTERCEPT', 'EARTH', ET, 'NONE', 'SUN', 
     .              SPOINT,      ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECLAT ( SPOINT, RAD, LON, LAT )

C
C     Get the state of the sun in Earth bodyfixed coordinates at ET.
C
      CALL SPKGEO ( 10, ET, 'IAU_EARTH', 399, SUNSTA, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECLAT ( SUNSTA, SUNRAD, SUNLON, SUNLAT )

C
C     Make sure the directional coordinates match up.
C
      CALL CHCKSD ( 'Sub point lon', LON, '~', SUNLON, TIGHT, OK )
      CALL CHCKSD ( 'Sub point lat', LAT, '~', SUNLAT, TIGHT, OK )

         
C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Test SUBPT.  Find the sub-solar point of the sun '//
     .        'on the Earth using the NEAR POINT definition.'

      CALL TCASE  ( TITLE )

      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure that strings representing integers are parsed correctly.
C
      CALL SUBPT  ( 'NEAR POINT', '399', ET, 'NONE', '10', 
     .              SPOINT,       ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We'll need the radii of the earth.
C
      CALL BODVRD ( 'EARTH', 'RADII', 3, DIM, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RE = RADII(1)
      RP = RADII(3)

      F  = ( RE - RP ) / RE 

      CALL RECGEO ( SPOINT, RE, F, LON, LAT, ALT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

C
C     Get the state of the sun in Earth bodyfixed coordinates at ET.
C
      CALL SPKGEO ( 10, ET, 'IAU_EARTH', 399, SUNSTA, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECGEO ( SUNSTA, RE, F, SUNLON, SUNLAT, SUNALT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Make sure the directional coordinates match up.
C
      CALL CHCKSD ( 'Sub point lon', LON, '~', SUNLON, TIGHT, OK )
      CALL CHCKSD ( 'Sub point lat', LAT, '~', SUNLAT, TIGHT, OK )


C
C     SUBPT error cases:
C
  
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid method.' )

      CALL SUBPT  ( 'NARPOINT', '399', ET, 'NONE', '10', 
     .              SPOINT,       ALT                         )
      CALL CHCKXC ( .TRUE., 'SPICE(DUBIOUSMETHOD)', OK )
     
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid observer name.' )

      CALL SUBPT  ( 'Nearpoint', 'earth', ET, 'NONE', 'sn', 
     .              SPOINT,       ALT                         )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid target name.' )

      CALL SUBPT  ( 'Nearpoint', 'erth', ET, 'NONE', 'sun', 
     .              SPOINT,       ALT                         )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer is target.' )

      CALL SUBPT  ( 'Nearpoint', 'earth', ET, 'NONE', 'earth', 
     .              SPOINT,       ALT                         )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


C
C     ILLUM tests follow.
C
C
C --- Case: ------------------------------------------------------
C
      TITLE =  'Test ILLUM.  Find the illumination angles on the '//
     .         'earth as seen from the moon, evaluated at the '     //
     .         'sub-moon point (NEARPOINT method).' 

      CALL TCASE  ( TITLE )

      CALL SUBPT  ( 'Nearpoint', 'earth', ET, 'NONE', 'moon', 
     .              SPOINT,       ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUM ( 'EARTH',  ET,     'NONE',  'MOON',
     .             SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should have an emission angle of zero.
C
      CALL CHCKSD ( 'Emission angle', EMISSN, '~', 0.D0, TIGHT, OK )

C
C     The phase angle should match the solar incidence angle.
C     
      CALL CHCKSD ( 'Phase angle', PHASE, '~', SOLAR, TIGHT, OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE  ( 'Repeat tests with integer codes.' )

      CALL SUBPT  ( 'Nearpoint', '399', ET, 'NONE', '301', 
     .              SPOINT,       ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUM ( '399',   ET,     'NONE',  '301',
     .             SPOINT,  PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should have an emission angle of zero.
C
      CALL CHCKSD ( 'Emission angle', EMISSN, '~', 0.D0, TIGHT, OK )

C
C     The phase angle should match the solar incidence angle.
C     
      CALL CHCKSD ( 'Phase angle', PHASE, '~', SOLAR, TIGHT, OK )


C
C     Now make the sun the observer:  test the solar incidence
C     angle.
C
C
C --- Case: ------------------------------------------------------
C
      TITLE =  'Test ILLUM.  Find the illumination angles on the ' //
     .         'earth as seen from the sun, evaluated at the '     //
     .         'sub-sun point (NEARPOINT method).' 

      CALL TCASE  ( TITLE )

      CALL SUBPT  ( 'Nearpoint', 'earth', ET, 'NONE', 'sun', 
     .              SPOINT,       ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ILLUM ( 'EARTH',  ET,     'NONE',  'sun',
     .             SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     We should have an solar incidence angle of zero.
C
      CALL CHCKSD ( 'Solar inc. angle', SOLAR, '~', 0.D0, TIGHT, OK )

C
C     The phase angle should match the solar incidence angle.
C     
      CALL CHCKSD ( 'Phase angle', PHASE, '~', SOLAR, TIGHT, OK )

C
C     ILLUM error cases:
C
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid observer name.' )

      CALL ILLUM ( 'earth',    ET,     'NONE',  'son',
     .             SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid target name.' )

      CALL ILLUM ( 'erth',    ET,     'NONE',  'sun',
     .             SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer is target.' )

      CALL ILLUM ( 'SUN',    ET,     'NONE',  'sun',
     .             SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No body-fixed frame associated with target.' )

      CALL ILLUM ( 'mars express',    ET,     'NONE',  'sun',
     .             SPOINT,   PHASE,  SOLAR,   EMISSN  )
      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


C
C     SUBSOL tests follow.
C
C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Test SUBSOL.  Find the sub-solar point of the sun '//
     .        'on the Earth using the NEARPOINT definition.'

      CALL TCASE  ( TITLE )

      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBSOL ( 'NEARPOINT', 'EARTH', ET, 'NONE', 'SUN',  SPOINT ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPT  ( 'NEARPOINT', 'EARTH', ET, 'NONE', 'SUN', 
     .              EXPPNT,      ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECLAT ( SPOINT, RAD, LON, LAT )

C
C     Make sure the surface points match up.
C
      CALL CHCKAD ( 'Geometric sub solar point',
     .              SPOINT,
     .              '~~/',
     .              EXPPNT,
     .              3,
     .              TIGHT, 
     .              OK                            )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Test SUBSOL.  Repeat test using integer codes.'

      CALL TCASE  ( TITLE )

      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBSOL ( 'NEARPOINT', '399', ET, 'NONE', '10',  SPOINT ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPT  ( 'NEARPOINT', 'EARTH', ET, 'NONE', 'SUN', 
     .              EXPPNT,      ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECLAT ( SPOINT, RAD, LON, LAT )

C
C     Make sure the surface points match up.
C
      CALL CHCKAD ( 'Geometric sub solar point',
     .              SPOINT,
     .              '~~/',
     .              EXPPNT,
     .              3,
     .              TIGHT, 
     .              OK                            )

C
C --- Case: ------------------------------------------------------
C
      TITLE = 'Test SUBSOL.  Find the sub-solar point of the sun '//
     .        'on the Earth using the INTERCEPT definition.'

      CALL TCASE  ( TITLE )

      CALL STR2ET ( UTC, ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBSOL ( 'INTERCEPT', 'EARTH', ET, 'NONE', 'SUN',  SPOINT ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SUBPT  ( 'INTERCEPT', 'EARTH', ET, 'NONE', 'SUN', 
     .              EXPPNT,      ALT                         )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL RECLAT ( SPOINT, RAD, LON, LAT )

C
C     Make sure the surface points match up.
C
      CALL CHCKAD ( 'Geometric sub solar point',
     .              SPOINT,
     .              '~~/',
     .              EXPPNT,
     .              3,
     .              TIGHT, 
     .              OK                           )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid method.' )

      CALL SUBSOL ( 'NARPOINT', '399', ET, 'NONE', '10', SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(DUBIOUSMETHOD)', OK )
     
 
C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid observer name.' )

      CALL SUBSOL  ( 'Nearpoint', 'earth', ET, 'NONE', 'sn', SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Invalid target name.' )

      CALL SUBSOL  ( 'Nearpoint', 'erth', ET, 'NONE', 'sun', SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer is target.' )

      CALL SUBSOL  ( 'Nearpoint', 'earth', ET, 'NONE', 'earth', 
     .              SPOINT )
      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

C
C


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )


 
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


