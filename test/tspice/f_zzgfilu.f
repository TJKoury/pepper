C$Procedure      F_ZZGFILU ( ZZGFILU family tests )

      SUBROUTINE F_ZZGFILU ( OK )

C$ Abstract
C
C     This routine tests entry points of the SPICELIB routine
C     
C        ZZGFILU
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
C     This routine tests the higher-level SPICELIB 
C     geometry routine GFILUM.
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
C-    TSPICE Version 1.0.0, 23-MAY-2012 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD

      EXTERNAL              UDDC

C     
C     Local parameters
C
      CHARACTER*(*)         PCK 
      PARAMETER           ( PCK    = 'nat.tpc' )

      CHARACTER*(*)         PCK2 
      PARAMETER           ( PCK2   = 'generic.tpc' )

      CHARACTER*(*)         OCTSPK
      PARAMETER           ( OCTSPK = 'octl_test.bsp' )

      CHARACTER*(*)         OCTL
      PARAMETER           ( OCTL   = 'OCTL' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'generic.bsp' )

      DOUBLE PRECISION      ANGTOL
      PARAMETER           ( ANGTOL = 1.D-13 )


      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      INTEGER               NCASE
      PARAMETER           ( NCASE  = 100 )

      INTEGER               TYPLEN
      PARAMETER           ( TYPLEN = 25 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 25 )

C
C     Local variables
C
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(TYPLEN)    ANGTYP
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(BDNMLN)    ILLUM
      CHARACTER*(LNSIZE)    METHOD
      CHARACTER*(LNSIZE)    NAME
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(TIMLEN)    TIME0
      CHARACTER*(LNSIZE)    TXT    ( TXTSIZ )

      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      EMISSN
      DOUBLE PRECISION      EMISTA ( 2 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      INCDNC
      DOUBLE PRECISION      INCSTA ( 2 )
      DOUBLE PRECISION      LAT
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LON
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      PHASE
      DOUBLE PRECISION      PHSSTA ( 2 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      STATES ( 6, 2 )
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TRGEPC
 
      INTEGER               HANDLE
      INTEGER               HAN1
      INTEGER               I
      INTEGER               N
      INTEGER               OCTID

      LOGICAL               FOUND
      LOGICAL               ISDEC
      LOGICAL               XDEC

C
C---- Case -------------------------------------------------------------
C


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFILU' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

 
      CALL TSTSPK ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )
 
      CALL NATPCK ( PCK,  .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )

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


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C*********************************************************************
C*
C*    Error cases for ZZGFILIN
C*
C*********************************************************************



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized value of FIXREF.' )



 

      METHOD = 'Ellipsoid'
      ANGTYP = 'INCIDENCE'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'
 
      LON    =  100.D0 * RPD()
      LAT    =   30.D0 * RPD()

      CALL SRFREC ( 399, LON, LAT, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                'XYZ',  ABCORR, OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(UNKNOWNFRAME)', OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'FIXREF is not centered on the target body.' )


      CALL ZZGFILIN ( METHOD,     ANGTYP, TARGET, ILLUM,  
     .                'IAU_MARS', ABCORR, OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad computation method.' )

      ANGTYP = 'EMISSION'

      METHOD = 'DSK'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, ABCORR, OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDMETHOD)',   OK )

      METHOD = 'ELLIPSOID'

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad illumination angle type' )

      ANGTYP = 'XYZ'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .              FIXREF, ABCORR, OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction values' )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .              FIXREF, 'S',    OBSRVR, SPOINT )
       
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .              FIXREF, 'XS',   OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .              FIXREF, 'RLT',  OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .              FIXREF, 'XRLT', OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .              FIXREF, 'Z',    OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Transmission aberration correction values.' )

      
      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, 'XLT',  OBSRVR, SPOINT )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, 'XCN',  OBSRVR, SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


      CALL ZZGFILIN ( METHOD, ANGTYP,  TARGET, ILLUM,  
     .                FIXREF, 'XLT+S', OBSRVR, SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP,  TARGET, ILLUM,  
     .                FIXREF, 'XCN+S', OBSRVR, SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized target, observer, or illumination '
     .//           'source.'                                        )


      CALL ZZGFILIN ( METHOD, ANGTYP, 'X',    ILLUM,  
     .                FIXREF, ABCORR, OBSRVR, SPOINT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, 'X',  
     .                FIXREF, ABCORR, OBSRVR, SPOINT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, ABCORR, 'X',    SPOINT  )
 
      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Target and observer are identical.' )


      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, ABCORR, TARGET, SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )

 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Target and illumination source are identical.' )


      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, TARGET,  
     .                FIXREF, ABCORR, OBSRVR, SPOINT  )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )



C*********************************************************************
C*
C*    Error cases for ZZGFILDC
C*
C*********************************************************************


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILDC: No SPK data for observer' )

      ANGTYP = 'PHASE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET,   ILLUM,  
     .                FIXREF, ABCORR, 'GASPRA', SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILDC ( UDDC, 0.D0, ISDEC )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILDC: No SPK data for target' )

      FIXREF = 'IAU_GASPRA'

      CALL ZZGFILIN ( METHOD, ANGTYP, 'GASPRA', ILLUM,  
     .                FIXREF, ABCORR, OBSRVR,   SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILDC ( UDDC, 0.D0, ISDEC )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILDC: No SPK data for illumination source' )

      FIXREF = 'IAU_EARTH'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, 'GASPRA',  
     .                FIXREF, ABCORR, OBSRVR, SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILDC ( UDDC, 0.D0, ISDEC )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )
 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGILDC: No PCK orientation data for target.'  )

      CALL ZZGFILIN ( METHOD,       ANGTYP, TARGET,  ILLUM,
     .                'ITRF93',     ABCORR, OBSRVR,  SPOINT )
      CALL CHCKXC ( .FALSE., ' ',   OK )
 
      CALL ZZGFILDC ( UDDC, 0.D0, ISDEC )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )

 


C*********************************************************************
C*
C*    Error cases for ZZGFILGQ
C*
C*********************************************************************


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILGQ: No SPK data for observer' )

      ANGTYP = 'PHASE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET,   ILLUM,  
     .                FIXREF, ABCORR, 'GASPRA', SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILGQ ( 0.D0, ANGLE )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILGQ: No SPK data for target' )

      FIXREF = 'IAU_GASPRA'

      CALL ZZGFILIN ( METHOD, ANGTYP, 'GASPRA', ILLUM,  
     .                FIXREF, ABCORR, OBSRVR,   SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILGQ ( 0.D0, ANGLE )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILGQ: No SPK data for illumination source' )

      FIXREF = 'IAU_EARTH'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, 'GASPRA',  
     .                FIXREF, ABCORR, OBSRVR, SPOINT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL ZZGFILGQ ( 0.D0, ANGLE )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )
 

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGILGQ: No PCK orientation data for target.'  )

      CALL ZZGFILIN ( METHOD,       ANGTYP, TARGET,  ILLUM,
     .                'ITRF93',     ABCORR, OBSRVR,  SPOINT )
      CALL CHCKXC ( .FALSE., ' ',   OK )
 
      CALL ZZGFILGQ ( 0.D0, ANGLE )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )

 
   




C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************
 



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILDC: check slope of phase angle '
     .//           'angles for a list of times.'          )


      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'
 
      CALL SPKPOS ( 'OCTL', 0.D0, FIXREF, 'NONE', TARGET, SPOINT, LT )

      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Phase angle:
C
      ANGTYP = 'PHASE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Get the normal vector required by ZZILUSTA:
C
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILDC ( UDDC, ET, ISDEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,
     .                   FIXREF, ABCORR, OBSRVR, SPOINT,
     .                   NORMAL, PHSSTA, INCSTA, EMISTA )
         CALL CHCKXC ( .FALSE., ' ', OK )


         XDEC = PHSSTA(2) .LT. 0.D0
C
C        Maintenance note: verify that both true and false
C        values of XDEC are generated by this test case.
C
C        WRITE (*,*) XDEC

         NAME = 'Phase angle decreasing #*'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSL ( NAME, ISDEC, XDEC, OK ) 
         
      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILDC: check slope of emission angle '
     .//           'angles for a list of times.'             )


C
C     Emission angle:
C
      ANGTYP = 'EMISSION'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Get the normal vector required by ZZILUSTA:
C
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILDC ( UDDC, ET, ISDEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,
     .                   FIXREF, ABCORR, OBSRVR, SPOINT,
     .                   NORMAL, PHSSTA, INCSTA, EMISTA )
         CALL CHCKXC ( .FALSE., ' ', OK )


         XDEC = EMISTA(2) .LT. 0.D0
C
C        Maintenance note: verify that both true and false
C        values of XDEC are generated by this test case.
C
C        WRITE (*,*) XDEC

         NAME = 'Emission angle decreasing #*'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSL ( NAME, ISDEC, XDEC, OK ) 
         
      END DO




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILDC: check slope of incidence angle '
     .//           'angles for a list of times.'             )


C
C     Emission angle:
C
      ANGTYP = 'INCIDENCE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Get the normal vector required by ZZILUSTA:
C
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SPOINT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILDC ( UDDC, ET, ISDEC )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ZZILUSTA ( METHOD, TARGET, ILLUM,  ET,
     .                   FIXREF, ABCORR, OBSRVR, SPOINT,
     .                   NORMAL, PHSSTA, INCSTA, EMISTA )
         CALL CHCKXC ( .FALSE., ' ', OK )


         XDEC = INCSTA(2) .LT. 0.D0
C
C        Maintenance note: verify that both true and false
C        values of XDEC are generated by this test case.
C
C        WRITE (*,*) XDEC

         NAME = 'Incidence angle decreasing #*'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSL ( NAME, ISDEC, XDEC, OK ) 
         
      END DO




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILGQ: get illumination incidence '
     .//           'angles for a list of times.'          )


      METHOD = 'Ellipsoid'
      TARGET = 'EARTH'
      ILLUM  = 'SUN'
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      OBSRVR = 'MOON'
 
      CALL SPKPOS ( 'OCTL', 0.D0, FIXREF, 'NONE', TARGET, SPOINT, LT )

      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Illumination incidence angle:
C
      ANGTYP = 'INCIDENCE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILGQ ( ET, ANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ILLUMG ( METHOD, TARGET, ILLUM,  ET,
     .                 FIXREF, ABCORR, OBSRVR, SPOINT,
     .                 TRGEPC, SRFVEC, PHASE,  INCDNC, EMISSN )
         CALL CHCKXC ( .FALSE., ' ', OK )


         NAME = 'Illum incidence (*)'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSD ( NAME, ANGLE, '~/', INCDNC, ANGTOL, OK ) 
         
      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILGQ: get emission '
     .//           'angles for a list of times.'          )


C
C     Illumination incidence angle:
C
      ANGTYP = 'EMISSION'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILGQ ( ET, ANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ILLUMG ( METHOD, TARGET, ILLUM,  ET,
     .                 FIXREF, ABCORR, OBSRVR, SPOINT,
     .                 TRGEPC, SRFVEC, PHASE,  INCDNC, EMISSN )
         CALL CHCKXC ( .FALSE., ' ', OK )


         NAME = 'Emission angle #*'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSD ( NAME, ANGLE, '~/', EMISSN, ANGTOL, OK ) 
         
      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'ZZGFILGQ: get phase '
     .//           'angles for a list of times.'          )


C
C     Phase angle:
C
      ANGTYP = 'PHASE'

      CALL ZZGFILIN ( METHOD, ANGTYP, TARGET, ILLUM,  
     .                FIXREF, ABCORR, OBSRVR, SPOINT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET0 = 0.D0

      DO I = 1, NCASE

         ET = ET0 + (I-1)*SPD()

         CALL ZZGFILGQ ( ET, ANGLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL ILLUMG ( METHOD, TARGET, ILLUM,  ET,
     .                 FIXREF, ABCORR, OBSRVR, SPOINT,
     .                 TRGEPC, SRFVEC, PHASE,  INCDNC, EMISSN )
         CALL CHCKXC ( .FALSE., ' ', OK )


         NAME = 'Phase angle #*'
         CALL REPMI ( NAME, '*', I, NAME )

         CALL CHCKSD ( NAME, ANGLE, '~/', PHASE, ANGTOL, OK ) 
         
      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

C
C     Clean up SPK files.
C      
      CALL SPKUEF ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL UNLOAD ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( OCTSPK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
