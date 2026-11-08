!$Procedure      F_GFTFOV ( Test GFTFOV )

      SUBROUTINE F_GFTFOV ( OK )

! $ Abstract
!
!     Test the GF FOV search API GFTFOV.
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
!     GF
!
! $ Keywords
!
!     GF
!     TEST
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'gf.inc'
      INCLUDE               'zzgf.inc'
      INCLUDE               'zzabcorr.inc'

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
!     This routine does not generate any exceptions. Routines in its
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
!     This family tests the GF AP routine GFTFOV. The underlying
!     GF FOV search utility routines
!
!        ZZGFFVIN
!        ZZGFFVST
!
!     are implicitly tested by this family together with
!
!        F_GFRFOV.
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
!     N.J. Bachman   (JPL)
!
! $ Version
!
!-    TSPICE Version 1.3.0, 25-AUG-2021 (EDW)
!
!        Edited all error strings on radii values, and number of values
!        returned from kernel pool, to correspond to us-of/changes-in
!        ZZGFTREB.
!
!-    TSPICE Version 1.2.0, 31-JAN-2017 (EDW)
!
!        Modified GFSTOL test case to correspond to change
!        in ZZGFSOLV.
!
!-    TSPICE Version 1.1.0, 20-NOV-2012 (EDW)
!
!        Added test on GFSTOL use.
!
!-    SPICELIB Version 1.0.0, 17-FEB-2009 (NJB)
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      SPD

      INTEGER               WNCARD

!
!     EXTERNAL declarations
!


!
!     Local parameters
!
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

!
!     Local variables
!
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

!
!     Saved Variables
!
      SAVE

!
!     Initial values
!
      DATA                  ABCORS / 'NONE', &
                                     'CN', 'XCN', 'CN+S', 'XCN+S' /

      DATA                  INSTRS / 'ALPHA_CIRCLE_NONE', &
                                     'ALPHA_ELLIPSE_NONE', &
                                     'ALPHA_RECTANGLE_NONE', &
                                     'ALPHA_DIAMOND_NONE'    /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_GFTFOV' )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )


!     Leapseconds:  Note that the LSK is deleted after loading, so we
!     don't have to clean it up later.
!
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load a PCK.
!
      CALL TSTPCK ( PCK, .TRUE., .FALSE. )

!
!     Load an SPK file as well.
!
      CALL TSTSPK ( SPK, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create and load Nat's solar system SPK, PCK/FK, and IK
!     files.
!
      CALL NATPCK ( NPCK, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATSPK ( NSPK, .TRUE., HAN2    )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL NATIK  ( NIK,  NSPK,   NPCK, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Actual DE-based ephemerides yield better comparisons,
!     since these ephemerides have less noise than do those
!     produced by TSTSPK.
!
!      CALL FURNSH ( 'de421.bsp' )
!      CALL FURNSH ( 'jup230.bsp' )
!


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!     The following error cases involve invalid initialization
!     values or missing data discovered at search time.
!

!
! --- Case: ------------------------------------------------------
!
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

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)', OK )

!
! --- Case: ------------------------------------------------------
!
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

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWTOOSMALL)', OK )

!
!     Restore window sizes.
!
      CALL SSIZED ( MAXWIN, CNFINE )
      CALL SSIZED ( MAXWIN, RESULT )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad observer or target name.' )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BET'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'SUN'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction for ellipsoid ' &
      //           'target.'                                 )

      ABCORR = 'XS'

      TSHAPE = 'ELLIPSOID'
      TARGET = 'BETA'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction for point ' &
      //           'target.'                                 )

      ABCORR = 'S'

      TSHAPE = 'POINT'
      TARGET = 'BETA'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction for ray ' &
      //           'target.'                                 )

      ABCORR = 'LT+S'

      TSHAPE = 'Ray'
      TARGET = 'BETA'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad shape specification.' )

      ABCORR = 'LT+S'

      TSHAPE = 'Line'
      TARGET = 'BETA'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSHAPE)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad reference frame TFRAME' )


      TSHAPE = 'ELLIPSOID'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      ABCORR = 'XCN+S'
      TFRAME = 'EME2000'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Blank reference frame TFRAME for ' &
      //           'ellipsoidal target'               )


      TSHAPE = 'ELLIPSOID'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      ABCORR = 'XCN+S'
      TFRAME = ' '

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad shape: ray.' )


      TSHAPE = 'RAY'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      ABCORR = 'XS'
      TFRAME = ' '

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reference frame TFRAME not centered on target ' &
      //           'when TSHAPE requires it.'                   )

      TSHAPE = 'ELLIPSOID'
      TARGET = 'Mars'
      TFRAME = 'IAU_EARTH'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-existent reference frame.' )


      TARGET = 'beta'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'
      TFRAME = 'EME2000'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
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

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

!
!     Restore all three radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, SVRADI )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
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

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

!
!     Restore all three radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, SVRADI )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
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

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )



      CALL VPACK  (  1.D0, -1.D0, 2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )



      CALL VPACK  (  1.D0, 1.D0, -2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


!
!     Restore all three radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, SVRADI )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No ID code for instrument.' )

      INST   = 'ALPHA_ELLIPSE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Instrument parameters missing from kernel pool.' )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL BODS2C ( INST, INSTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      KVNAME = 'INS#_FOV_SHAPE'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
!
!     Delete the instrument shape from the kernel pool.
!
      CALL DVPOOL ( KVNAME )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SHAPEMISSING)', OK )

!
!     Restore the instrument shape value.
!
      CALL PCPOOL ( KVNAME, 1, 'ELLIPSE' )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Degenerate FOV ellipse.' )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL BODS2C ( INST, INSTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Fetch the instrument boresight vector from the kernel pool.
!
      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )

      CALL GDPOOL ( KVNAME, 1, 3, N, BSITE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Fetch the instrument boundary vectors from the kernel pool.
!
      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )

      CALL GDPOOL ( KVNAME, 1, 3*MAXVRT, N, FOVBND, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the first boundary vector equal to the boresight vector.
!
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

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


!
!     Restore the first boundary vector and set the second
!     equal to the boresight vector.
!
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

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )

!
!     Restore the instrument FOV boundary vectors.
!
      CALL PDPOOL ( KVNAME, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'FOV boundary vector has excessive angular ' &
      //           'separation from boresight.'                 )

      INST   = 'ALPHA_RECTANGLE_NONE'

      CALL BODS2C ( INST, INSTID, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

!
!     Fetch the instrument boresight vector from the kernel pool.
!
      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )

      CALL GDPOOL ( KVNAME, 1, 3, N, BSITE, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Fetch the instrument boundary vectors from the kernel pool.
!
      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )

      CALL GDPOOL ( KVNAME, 1, 3*MAXVRT, N, FOVBND, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the third boundary vector equal to the *negative*
!     of the boresight vector.
!
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

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(FACENOTFOUND)', OK )

!
!     Restore the instrument FOV boundary vectors.
!
      CALL PDPOOL ( KVNAME, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target orientation data available' )

!
!     This error is detected post-initialization.
!
!     At this point, we need an actual, non-empty confinement
!     window.
!
      CALL WNINSD ( 0.D0, SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )


      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      TFRAME = 'ITRF93'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target ephemeris data available' )
!
!     This error is detected post-initialization.
!
      CALL WNINSD ( 0.D0, SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TSHAPE = 'ellipsoid'
      TARGET = 'GASPRA'
      TFRAME = 'IAU_GASPRA'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No observer ephemeris data available' )
!
!     This error is detected post-initialization.
!
      CALL WNINSD ( 0.D0, SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TSHAPE = 'ellipsoid'
      TARGET = 'SUN'
      TFRAME = 'IAU_SUN'
      OBSRVR = 'GASPRA'
      ABCORR = 'None'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No TFRAME orientation data available' )
!
!     This error is detected post-initialization.
!
      CALL WNINSD ( 0.D0, SPD(), CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      TFRAME = 'ITRF93'

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


!*********************************************************************
!*
!*    Non-error exceptional cases
!*
!*********************************************************************



!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************

!
!     We'll start out with cases using an ellipsoidal target and
!     geometric states. We'll use the four instruments defined
!     in nat.ti:
!
!        ALPHA_CIRCLE_NONE
!        ALPHA_ELLIPSE_NONE
!        ALPHA_RECTANGLE_NONE
!        ALPHA_DIAMOND_NONE
!
!     These have the FOV shapes
!
!        ELLIPSE
!        CIRCLE
!        RECTANGLE
!        POLYGON
!
!     and track body Alpha so body Beta's FOV entry and
!     exit times match the start and stop times of Beta's
!     transit across Alpha.
!
!     Initialize inputs for the search.
!
      CALL SCARDD ( 0, RESULT )
      CALL SCARDD ( 0, CNFINE )

!
!     Use a 3-day span that covers 3 events.
!
      CALL WNINSD ( -0.5D0*SPD(), 2.5D0*SPD(), CNFINE )

      TARGET = 'BETA'
      TSHAPE = 'ELLIPSOID'
      TFRAME = 'BETAFIXED'
      OBSRVR = 'SUN'
      STEP   = 300.D0

      DO INS = 1, NINST

         INST = INSTRS(INS)

         DO NC = 1, NCORS
!
! --- Case: ------------------------------------------------------
!
!
            ABCORR = ABCORS(NC)

            CALL ZZPRSCOR ( ABCORR, ATTBLK )
            CALL CHCKXC ( .FALSE., ' ', OK )

            USELT = ATTBLK( LTIDX  )
            XMIT  = ATTBLK( XMTIDX )

            IF ( .NOT. ATTBLK( GEOIDX ) ) THEN
!
!              We have some type of aberration correction.
!
!              We're going to change the instrument frame dynamically
!              so as to keep the frame definition compatible with the
!              aberration correction we're using. We'll fetch the
!              instrument ID and use the aberration correction to
!              construct the name of the appropriate
!              aberration-corrected frame.

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

!
!              Substitute the aberration correction flag into
!              the frame name.
!
               CALL REPMC ( NEWFRM, '#', ABCORR, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              At this point we have a frame name such as
!
!                 ALPHA_VIEW_XY_CN+S
!
!              We must remove the '+' symbol from the name.
!
               CALL CMPRSS ( '+', 0, NEWFRM, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Insert the new instrument frame association into
!              the kernel pool.
!
               CALL PCPOOL ( KVNAME, 1, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

            ELSE
!
!              Set NEWFRM, since we reference it in the test case
!              title.
!
               NEWFRM = OLDFRM

            END IF

!
!           Set up the TCASE call.
!
            TITLE = 'Target #; target shape #; Inst. #; #; ' &
            //      'inst frame #'

            CALL REPMC ( TITLE, '#', TARGET, TITLE )
            CALL REPMC ( TITLE, '#', TSHAPE, TITLE )
            CALL REPMC ( TITLE, '#', INST,   TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
            CALL REPMC ( TITLE, '#', NEWFRM, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )


!
!           Perform the search.
!
            CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                          ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )

!
!           We're expecting 3 events.
!
            CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

            IF ( N .GT. 0 ) THEN

               DO I = 1, N

                  CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check the interval endpoints. The event should
!                 start at one noon+delta and stop 10 minutes later,
!                 where delta depends on the aberration correction:
!
!                    CN             delta = +1
!                    CN+S           delta = +1
!                    XCN            delta = -1
!                    XCN+S          delta = -1
!
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

                  CALL CHCKSD ( QNAME, ENDPT(1), '~', &
                                XTIME, TIMTOL,    OK )


                  QNAME = 'Stop time @'
                  CALL REPMI ( QNAME, '@', I, QNAME )

                  XTIME = (I-1)*SPD() + 600.D0 + DELTA

                  CALL CHCKSD ( QNAME, ENDPT(2), '~', &
                                XTIME, TIMTOL,    OK )

               END DO
!
!              End of result interval check loop.
!
            END IF
!
!           End of non-empty result window block.
!

            IF ( .NOT. ATTBLK( GEOIDX ) ) THEN
!
!              Restore the old instrument frame association in
!              the kernel pool.
!
               CALL PCPOOL ( KVNAME, 1, OLDFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

         END DO
!
!        End of aberration correction loop.
!
      END DO
!
!     End of instrument loop.
!


!
!     For the next set of cases, we keep the inputs as before, except
!     for the target shape: we treat the target as a single point.
!     This should cause each FOV entry to start 1 minute late and end
!     1 minute early.
!
      TARGET = 'BETA'
      TSHAPE = 'POINT'
      TFRAME = 'BETAFIXED'
      OBSRVR = 'SUN'
      STEP   = 300.D0


      DO INS = 1, NINST

         INST = INSTRS(INS)

         DO NC = 1, NCORS
!
! --- Case: ------------------------------------------------------
!
!
            ABCORR = ABCORS(NC)

            CALL ZZPRSCOR ( ABCORR, ATTBLK )
            CALL CHCKXC ( .FALSE., ' ', OK )

            USELT = ATTBLK( LTIDX  )
            XMIT  = ATTBLK( XMTIDX )

            IF ( .NOT. ATTBLK( GEOIDX ) ) THEN
!
!              We have some type of aberration correction.
!
!              We're going to change the instrument frame dynamically
!              so as to keep the frame definition compatible with the
!              aberration correction we're using. We'll fetch the
!              instrument ID and use the aberration correction to
!              construct the name of the appropriate
!              aberration-corrected frame.

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

!
!              Substitute the aberration correction flag into
!              the frame name.
!
               CALL REPMC ( NEWFRM, '#', ABCORR, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              At this point we have a frame name such as
!
!                 ALPHA_VIEW_XY_CN+S
!
!              We must remove the '+' symbol from the name.
!
               CALL CMPRSS ( '+', 0, NEWFRM, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

!
!              Insert the new instrument frame association into
!              the kernel pool.
!
               CALL PCPOOL ( KVNAME, 1, NEWFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

            ELSE
!
!              Set NEWFRM, since we reference it in the test case
!              title.
!
               NEWFRM = OLDFRM

            END IF

!
!           Set up the TCASE call.
!
            TITLE = 'Target #; target shape #; Inst. #; #; ' &
            //      'inst frame #'

            CALL REPMC ( TITLE, '#', TARGET, TITLE )
            CALL REPMC ( TITLE, '#', TSHAPE, TITLE )
            CALL REPMC ( TITLE, '#', INST,   TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
            CALL REPMC ( TITLE, '#', NEWFRM, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL TCASE ( TITLE )

!
!           Perform the search.
!
            CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                          ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            N = WNCARD( RESULT )

!
!           We're expecting 3 events.
!
            CALL CHCKSI ( 'N', N, '=', 3, 0, OK )

            IF ( N .GT. 0 ) THEN

               DO I = 1, N

                  CALL WNFETD ( RESULT, I, ENDPT(1), ENDPT(2) )
                  CALL CHCKXC ( .FALSE., ' ', OK )

!
!                 Check the interval endpoints. The event should
!                 start at one noon+delta and stop 10 minutes later,
!                 where delta depends on the aberration correction:
!
!                    CN             delta = +1
!                    CN+S           delta = +1
!                    XCN            delta = -1
!                    XCN+S          delta = -1
!
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

                  CALL CHCKSD ( QNAME, ENDPT(1), '~', &
                                XTIME, TIMTOL,    OK )


                  QNAME = 'Stop time @'
                  CALL REPMI ( QNAME, '@', I, QNAME )

                  XTIME = (I-1)*SPD() + 600.D0 + DELTA - 60.D0

                  CALL CHCKSD ( QNAME, ENDPT(2), '~', &
                                XTIME, TIMTOL,    OK )

               END DO
!
!              End of result interval check loop.
!
            END IF
!
!           End of non-empty result window block.
!

            IF ( .NOT. ATTBLK( GEOIDX ) ) THEN
!
!              Restore the old instrument frame association in
!              the kernel pool.
!
               CALL PCPOOL ( KVNAME, 1, OLDFRM )
               CALL CHCKXC ( .FALSE., ' ', OK )

            END IF

         END DO
!
!        End of aberration correction loop.
!
      END DO
!
!     End of instrument loop.
!




!
!     Case
!
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


!
!     Re-run a valid search after using GFSTOL to set the convergence
!     tolerance to a value that should cause a numerical error signal.
!

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

!
!     Use a 3-day span that covers 3 events.
!
      CALL WNINSD ( -0.5D0*SPD(), 2.5D0*SPD(), CNFINE )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      N = 0
      N = WNCARD( RESULT )
      CALL CHCKSI ( 'COUNT', N, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Reset tol.
!

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFTFOV ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, STEP,   CNFINE, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      N = 0
      N = WNCARD( RESULT )
      CALL CHCKSI ( 'COUNT', N, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     The values in the time window should not match
!     as the search used different tolerances. Check
!     the first value in the first interval.
!
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )



!
!     Reset the convergence tolerance.
!
      CALL GFSTOL ( CNVTOL )



!
! --- Case: ------------------------------------------------------
!
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



