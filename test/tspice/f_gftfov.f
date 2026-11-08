C$Procedure      F_GFTFOV ( Test GFTFOV )
  
      SUBROUTINE F_GFTFOV ( OK )

C$ Abstract
C
C     Test the GF FOV search API GFTFOV. 
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
C     GF
C
C$ Keywords
C
C     GF
C     TEST
C     SEARCH
C
C$ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

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
C     This routine does not generate any exceptions. Routines in its
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
C     This family tests the GF AP routine GFTFOV. The underlying
C     GF FOV search utility routines
C
C        ZZGFFVIN
C        ZZGFFVST
C
C     are implicitly tested by this family together with
C
C        F_GFRFOV.
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
C     N.J. Bachman   (JPL)
C
C$ Version
C
C-    TSPICE Version 1.3.0, 25-AUG-2021 (EDW)
C
C        Edited all error strings on radii values, and number of values
C        returned from kernel pool, to correspond to us-of/changes-in 
C        ZZGFTREB.
C
C-    TSPICE Version 1.2.0, 31-JAN-2017 (EDW)
C
C        Modified GFSTOL test case to correspond to change
C        in ZZGFSOLV.
C
C-    TSPICE Version 1.1.0, 20-NOV-2012 (EDW)
C
C        Added test on GFSTOL use.
C
C-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB)
C
C-&
 
  
C
C     SPICELIB functions
C
      DOUBLE PRECISION      SPD

      INTEGER               WNCARD

C
C     EXTERNAL declarations
C
      

C     
C     Local parameters
C
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'zzgffvu.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         NIK
      PARAMETER           ( NIK    = 'nat.ti' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgffvu.bsp' )
  
      
      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = 2*CNVTOL )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               KVARLN
      PARAMETER           ( KVARLN = 32 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 20000 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NINST
      PARAMETER           ( NINST = 4 )

      INTEGER               LBLSIZ
      PARAMETER           ( LBLSIZ = 40 )

      INTEGER               NCORS
      PARAMETER           ( NCORS  = 5 )

C
C     Local variables
C
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(CORLEN)    ABCORS ( NCORS )
      CHARACTER*(BDNMLN)    INST
      CHARACTER*(BDNMLN)    INSTRS ( NINST )
      CHARACTER*(KVARLN)    KVNAME
      CHARACTER*(FRNMLN)    NEWFRM
      CHARACTER*(FRNMLN)    OLDFRM
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(LBLSIZ)    QNAME
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(FRNMLN)    TFRAME
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(SHPLEN)    TSHAPE
      
      DOUBLE PRECISION      BADBND ( 3, MAXVRT )
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      BSITE  ( 3 )
      DOUBLE PRECISION      CNFINE ( LBCELL : MAXWIN )
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      END
      DOUBLE PRECISION      ENDPT  ( 2 )
      DOUBLE PRECISION      FOVBND ( 3, MAXVRT )
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      SVRADI ( 3 )
      DOUBLE PRECISION      XTIME

      INTEGER               HAN2
      INTEGER               HANDLE      
      INTEGER               I
      INTEGER               INS
      INTEGER               INSTID
      INTEGER               N
      INTEGER               NC

      LOGICAL               ATTBLK ( ABATSZ )
      LOGICAL               FOUND
      LOGICAL               USELT
      LOGICAL               XMIT

C
C     Saved Variables
C
      SAVE

C
C     Initial values 
C
      DATA                  ABCORS / 'NONE', 
     .                               'CN', 'XCN', 'CN+S', 'XCN+S' /

      DATA                  INSTRS / 'ALPHA_CIRCLE_NONE',
     .                               'ALPHA_ELLIPSE_NONE',
     .                               'ALPHA_RECTANGLE_NONE',
     .                               'ALPHA_DIAMOND_NONE'    /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFTFOV' )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


C     Leapseconds:  Note that the LSK is deleted after loading, so we
C     don't have to clean it up later.
C
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Load a PCK.
C     
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

C
C     Load an SPK file as well.
C     
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create and load Nat's solar system SPK, PCK/FK, and IK
C     files.
C     
      CALL NATPCK ( NPCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATSPK ( NSPK, .TRUE., HAN2    )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATIK  ( NIK,  NSPK,   NPCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Actual DE-based ephemerides yield better comparisons,
C     since these ephemerides have less noise than do those
C     produced by TSTSPK.
C     
C      CALL FURNSH ( 'de421.bsp' )
C      CALL FURNSH ( 'jup230.bsp' )
C 


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C     The following error cases involve invalid initialization
C     values or missing data discovered at search time.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad step size' )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BET'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      STEP   = 0.D0

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Result window too small' )


      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( 0,      RESULT )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'
      STEP   = 300.D0

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWTOOSMALL)', OK )

C
C     Restore window sizes.
C
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad observer or target name.' )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BET'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'SUN'
      OBSRVR = 'SUN'
 
      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction for ellipsoid '
     .//           'target.'                                 )
 
      ABCORR = 'XS'

      TSHAPE = 'ELLIPSOID'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
 
      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction for point '
     .//           'target.'                                 )
 
      ABCORR = 'S'

      TSHAPE = 'POINT'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
 
      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction for ray '
     .//           'target.'                                 )
 
      ABCORR = 'LT+S'

      TSHAPE = 'Ray'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
 
      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad shape specification.' )
 
      ABCORR = 'LT+S'

      TSHAPE = 'Line'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
 
      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSHAPE)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad reference frame TFRAME' )


      TSHAPE = 'ELLIPSOID'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      ABCORR = 'XCN+S'
      TFRAME = 'EME2000'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Blank reference frame TFRAME for '
     .//           'ellipsoidal target'               )


      TSHAPE = 'ELLIPSOID'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      ABCORR = 'XCN+S'
      TFRAME = ' '

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad shape: ray.' )


      TSHAPE = 'RAY'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      ABCORR = 'XS'
      TFRAME = ' '

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Reference frame TFRAME not centered on target '
     .//           'when TSHAPE requires it.'                   )

      TSHAPE = 'ELLIPSOID'
      TARGET = 'Mars'
      TFRAME = 'IAU_EARTH'
      OBSRVR = 'SUN'
 
      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Non-existent reference frame.' )


      TARGET = 'beta'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'
      TFRAME = 'EME2000'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target radii in kernel pool' )


      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      TFRAME = 'IAU_EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, SVRADI, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DVPOOL ( 'BODY399_RADII' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore all three radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, SVRADI )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad target radii count' )

      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      TFRAME = 'IAU_EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, SVRADI, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( 'BODY399_RADII', 2, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C     Restore all three radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, SVRADI )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad target radii values' )

      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      TFRAME = 'IAU_EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, SVRADI, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VPACK  ( 0.D0, 1.D0, 2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )



      CALL VPACK  (  1.D0, -1.D0, 2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )



      CALL VPACK  (  1.D0, 1.D0, -2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


C
C     Restore all three radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, SVRADI )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No ID code for instrument.' )

      INST   = 'ALPHA_ELLIPSE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Instrument parameters missing from kernel pool.' )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL BODS2C ( INST, INSTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      KVNAME = 'INS#_FOV_SHAPE'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
C
C     Delete the instrument shape from the kernel pool.
C
      CALL DVPOOL ( KVNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'      

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SHAPEMISSING)', OK )

C
C     Restore the instrument shape value.
C
      CALL PCPOOL ( KVNAME, 1, 'ELLIPSE' )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Degenerate FOV ellipse.' )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL BODS2C ( INST, INSTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     Fetch the instrument boresight vector from the kernel pool.
C
      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      
      CALL GDPOOL ( KVNAME, 1, 3, N, BSITE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Fetch the instrument boundary vectors from the kernel pool.
C
      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )

      CALL GDPOOL ( KVNAME, 1, 3*MAXVRT, N, FOVBND, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the first boundary vector equal to the boresight vector.
C
      CALL MOVED ( FOVBND, N, BADBND      )
      CALL MOVED ( BSITE,  3, BADBND(1,1) )
      
      CALL PDPOOL ( KVNAME, N, BADBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'      

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


C
C     Restore the first boundary vector and set the second
C     equal to the boresight vector.
C
      CALL MOVED ( FOVBND(1,1), 3, BADBND(1,1) )
      CALL MOVED ( BSITE,       3, BADBND(1,2) )
      
      CALL PDPOOL ( KVNAME, N, BADBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'      

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

C
C     Restore the instrument FOV boundary vectors.
C
      CALL PDPOOL ( KVNAME, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'FOV boundary vector has excessive angular '
     .//           'separation from boresight.'                 )

      INST   = 'ALPHA_RECTANGLE_NONE'

      CALL BODS2C ( INST, INSTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

C
C     Fetch the instrument boresight vector from the kernel pool.
C
      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      
      CALL GDPOOL ( KVNAME, 1, 3, N, BSITE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Fetch the instrument boundary vectors from the kernel pool.
C
      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )

      CALL GDPOOL ( KVNAME, 1, 3*MAXVRT, N, FOVBND, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the third boundary vector equal to the *negative*
C     of the boresight vector.
C
      CALL MOVED  ( FOVBND, N, BADBND      )
      CALL VMINUS ( BSITE,     BADBND(1,3) )
      
      CALL PDPOOL ( KVNAME, N, BADBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'      

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(FACENOTFOUND)', OK )

C
C     Restore the instrument FOV boundary vectors.
C
      CALL PDPOOL ( KVNAME, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target orientation data available' )

C
C     This error is detected post-initialization.
C
C     At this point, we need an actual, non-empty confinement
C     window.
C
      CALL WNINSD ( 0.D0, SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      TFRAME = 'ITRF93'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
 
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target ephemeris data available' )
C
C     This error is detected post-initialization.
C
      CALL WNINSD ( 0.D0, SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TSHAPE = 'ellipsoid'
      TARGET = 'GASPRA'
      TFRAME = 'IAU_GASPRA'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
  
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No observer ephemeris data available' )
C
C     This error is detected post-initialization.
C
      CALL WNINSD ( 0.D0, SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TSHAPE = 'ellipsoid'
      TARGET = 'SUN'
      TFRAME = 'IAU_SUN'
      OBSRVR = 'GASPRA'
      ABCORR = 'None'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
  
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No TFRAME orientation data available' )
C
C     This error is detected post-initialization.
C
      CALL WNINSD ( 0.D0, SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      TFRAME = 'ITRF93'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
  
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C*********************************************************************
C*
C*    Non-error exceptional cases
C*
C*********************************************************************



C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************

C
C     We'll start out with cases using an ellipsoidal target and
C     geometric states. We'll use the four instruments defined
C     in nat.ti:
C
C        ALPHA_CIRCLE_NONE
C        ALPHA_ELLIPSE_NONE
C        ALPHA_RECTANGLE_NONE
C        ALPHA_DIAMOND_NONE
C
C     These have the FOV shapes 
C
C        ELLIPSE
C        CIRCLE
C        RECTANGLE
C        POLYGON
C
C     and track body Alpha so body Beta's FOV entry and
C     exit times match the start and stop times of Beta's
C     transit across Alpha.
C
C     Initialize inputs for the search.
C
      CALL SCARDD ( 0, RESULT )
      CALL SCARDD ( 0, CNFINE )

C
C     Use a 3-day span that covers 3 events.
C
      CALL WNINSD ( -0.5D0*SPD(), 2.5D0*SPD(), CNFINE )

      TARGET = 'BETA'
      TSHAPE = 'ELLIPSOID'
      TFRAME = 'BETAFIXED'
      OBSRVR = 'SUN'
      STEP   = 300.D0

      DO INS = 1, NINST        

         INST = INSTRS(INS)

         DO NC = 1, NCORS
C
C --- Case: ------------------------------------------------------
C
C
            ABCORR = ABCORS(NC)

            CALL ZZPRSCOR ( ABCORR, ATTBLK )
            CALL CHCKXC ( .FALSE., ' ', OK )

            USELT = ATTBLK( LTIDX  )
            XMIT  = ATTBLK( XMTIDX )

            IF ( .NOT. ATTBLK( GEOIDX ) ) THEN
C
C              We have some type of aberration correction.
C
C              We're going to change the instrument frame dynamically
C              so as to keep the frame definition compatible with the
C              aberration correction we're using. We'll fetch the
C              instrument ID and use the aberration correction to
C              construct the name of the appropriate
C              aberration-corrected frame.

               CALL BODN2C ( INST, INSTID, FOUND ) 
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'INSTID found', FOUND, .TRUE., OK )

               KVNAME = 'INS#_FOV_FRAME'

               CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL GCPOOL ( KVNAME, 1, 1, N, OLDFRM, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'frame found', FOUND, .TRUE., OK )

               NEWFRM = OLDFRM

               CALL SUFFIX ( '_#', 0, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Substitute the aberration correction flag into
C              the frame name.
C
               CALL REPMC ( NEWFRM, '#', ABCORR, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              At this point we have a frame name such as
C
C                 ALPHA_VIEW_XY_CN+S
C
C              We must remove the '+' symbol from the name.
C
               CALL CMPRSS ( '+', 0, NEWFRM, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Insert the new instrument frame association into
C              the kernel pool.
C
               CALL PCPOOL ( KVNAME, 1, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

            ELSE
C
C              Set NEWFRM, since we reference it in the test case
C              title.
C
               NEWFRM = OLDFRM

            END IF

C
C           Set up the TCASE call.
C
            TITLE = 'Target #; target shape #; Inst. #; #; '
     .      //      'inst frame #'

            CALL REPMC ( TITLE, '#', TARGET, TITLE )
            CALL REPMC ( TITLE, '#', TSHAPE, TITLE )
            CALL REPMC ( TITLE, '#', INST,   TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
            CALL REPMC ( TITLE, '#', NEWFRM, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )


C
C           Perform the search.
C
            CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )

C
C           We're expecting 3 events.
C
            CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

            IF ( N .GT. 0 ) THEN

               DO I = 1, N

                  CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check the interval endpoints. The event should
C                 start at one noon+delta and stop 10 minutes later,
C                 where delta depends on the aberration correction:
C
C                    CN             delta = +1
C                    CN+S           delta = +1
C                    XCN            delta = -1
C                    XCN+S          delta = -1
C
                  IF ( USELT ) THEN

                     IF ( XMIT ) THEN
                        DELTA = -1.D0
                     ELSE
                        DELTA =  1.D0
                     END IF

                  ELSE
                     DELTA = 0.D0
                  END IF
               
                  QNAME = 'Start time @'
                  CALL REPMI ( QNAME, '@', I, QNAME )

                  XTIME = (I-1)*SPD() + DELTA

                  CALL CHCKSD ( QNAME, ENDPT(1), '~', 
     .                          XTIME, TIMTOL,    OK )

               
                  QNAME = 'Stop time @'
                  CALL REPMI ( QNAME, '@', I, QNAME )

                  XTIME = (I-1)*SPD() + 600.D0 + DELTA

                  CALL CHCKSD ( QNAME, ENDPT(2), '~', 
     .                          XTIME, TIMTOL,    OK )

               END DO
C
C              End of result interval check loop.
C
            END IF
C
C           End of non-empty result window block.
C

            IF ( .NOT. ATTBLK( GEOIDX ) ) THEN
C
C              Restore the old instrument frame association in
C              the kernel pool.
C   
               CALL PCPOOL ( KVNAME, 1, OLDFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

         END DO
C
C        End of aberration correction loop.
C
      END DO
C
C     End of instrument loop.
C


C
C     For the next set of cases, we keep the inputs as before, except
C     for the target shape: we treat the target as a single point.
C     This should cause each FOV entry to start 1 minute late and end
C     1 minute early.
C
      TARGET = 'BETA'
      TSHAPE = 'POINT'
      TFRAME = 'BETAFIXED'
      OBSRVR = 'SUN'
      STEP   = 300.D0


      DO INS = 1, NINST        

         INST = INSTRS(INS)

         DO NC = 1, NCORS
C
C --- Case: ------------------------------------------------------
C
C
            ABCORR = ABCORS(NC)

            CALL ZZPRSCOR ( ABCORR, ATTBLK )
            CALL CHCKXC ( .FALSE., ' ', OK )

            USELT = ATTBLK( LTIDX  )
            XMIT  = ATTBLK( XMTIDX )

            IF ( .NOT. ATTBLK( GEOIDX ) ) THEN
C
C              We have some type of aberration correction.
C
C              We're going to change the instrument frame dynamically
C              so as to keep the frame definition compatible with the
C              aberration correction we're using. We'll fetch the
C              instrument ID and use the aberration correction to
C              construct the name of the appropriate
C              aberration-corrected frame.

               CALL BODN2C ( INST, INSTID, FOUND ) 
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'INSTID found', FOUND, .TRUE., OK )

               KVNAME = 'INS#_FOV_FRAME'

               CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL GCPOOL ( KVNAME, 1, 1, N, OLDFRM, FOUND )
               CALL CHCKXC ( .FALSE., ' ', OK )

               CALL CHCKSL ( 'frame found', FOUND, .TRUE., OK )

               NEWFRM = OLDFRM

               CALL SUFFIX ( '_#', 0, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Substitute the aberration correction flag into
C              the frame name.
C
               CALL REPMC ( NEWFRM, '#', ABCORR, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              At this point we have a frame name such as
C
C                 ALPHA_VIEW_XY_CN+S
C
C              We must remove the '+' symbol from the name.
C
               CALL CMPRSS ( '+', 0, NEWFRM, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

C
C              Insert the new instrument frame association into
C              the kernel pool.
C
               CALL PCPOOL ( KVNAME, 1, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

            ELSE
C
C              Set NEWFRM, since we reference it in the test case
C              title.
C
               NEWFRM = OLDFRM

            END IF

C
C           Set up the TCASE call.
C
            TITLE = 'Target #; target shape #; Inst. #; #; '
     .      //      'inst frame #'

            CALL REPMC ( TITLE, '#', TARGET, TITLE )
            CALL REPMC ( TITLE, '#', TSHAPE, TITLE )
            CALL REPMC ( TITLE, '#', INST,   TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
            CALL REPMC ( TITLE, '#', NEWFRM, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

C
C           Perform the search.
C
            CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )

C
C           We're expecting 3 events.
C
            CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

            IF ( N .GT. 0 ) THEN

               DO I = 1, N

                  CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

C
C                 Check the interval endpoints. The event should
C                 start at one noon+delta and stop 10 minutes later,
C                 where delta depends on the aberration correction:
C
C                    CN             delta = +1
C                    CN+S           delta = +1
C                    XCN            delta = -1
C                    XCN+S          delta = -1
C
                  IF ( USELT ) THEN

                     IF ( XMIT ) THEN
                        DELTA = -1.D0
                     ELSE
                        DELTA =  1.D0
                     END IF

                  ELSE
                     DELTA = 0.D0
                  END IF
               
                  QNAME = 'Start time @'
                  CALL REPMI ( QNAME, '@', I, QNAME )

                  XTIME = (I-1)*SPD() + DELTA + 60.D0

                  CALL CHCKSD ( QNAME, ENDPT(1), '~', 
     .                          XTIME, TIMTOL,    OK )

               
                  QNAME = 'Stop time @'
                  CALL REPMI ( QNAME, '@', I, QNAME )

                  XTIME = (I-1)*SPD() + 600.D0 + DELTA - 60.D0

                  CALL CHCKSD ( QNAME, ENDPT(2), '~', 
     .                          XTIME, TIMTOL,    OK )

               END DO
C
C              End of result interval check loop.
C
            END IF
C
C           End of non-empty result window block.
C

            IF ( .NOT. ATTBLK( GEOIDX ) ) THEN
C
C              Restore the old instrument frame association in
C              the kernel pool.
C   
               CALL PCPOOL ( KVNAME, 1, OLDFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

         END DO
C
C        End of aberration correction loop.
C
      END DO
C
C     End of instrument loop.
C




C
C     Case
C
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


C
C     Re-run a valid search after using GFSTOL to set the convergence
C     tolerance to a value that should cause a numerical error signal.
C

      I = 1
      TARGET = 'BETA'
      TSHAPE = 'POINT'
      TFRAME = 'BETAFIXED'
      OBSRVR = 'SUN'
      STEP   = 1.D0
      INST   = INSTRS(I)
      ABCORR = ABCORS(I)

      CALL SCARDD ( 0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Use a 3-day span that covers 3 events.
C
      CALL WNINSD ( -0.5D0*SPD(), 2.5D0*SPD(), CNFINE )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      N = 0
      N = WNCARD( RESULT )
      CALL CHCKSI ( 'COUNT', N, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Reset tol.
C

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME,
     .              ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      N = 0
      N = WNCARD( RESULT )
      CALL CHCKSI ( 'COUNT', N, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     The values in the time window should not match
C     as the search used different tolerances. Check
C     the first value in the first interval.
C
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )



C
C     Reset the convergence tolerance.
C
      CALL GFSTOL ( CNVTOL )
        
            

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Clean up:  delete kernels.' )

      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( SPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( NSPK )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END



