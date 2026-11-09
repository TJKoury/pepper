!$Procedure      F_FOVTRG ( FOVTRG family tests )

      SUBROUTINE F_FOVTRG ( OK )
      use pepper_inc_gf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        FOVTRG
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
!     This routine tests the higher-level SPICELIB
!     geometry routine FOVTRG.
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
!     S.C. Krening     (JPL)
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 25-AUG-2021 (EDW)
!
!        Edited all error strings on radii values, and number of values
!        returned from kernel pool, to correspond to us-of/changes-in
!        ZZGFTREB.
!
!-    TSPICE Version 1.0.0, 13-FEB-2012 (SCK) (NJB)
!
!-&

!
!     SPICELIB functions
!
!      DOUBLE PRECISION      SPD

!
!     Local parameters
!
      CHARACTER*(*)         PCK
      PARAMETER           ( PCK    = 'fovtrg.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         NIK
      PARAMETER           ( NIK    = 'nat.ti' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'zzgffvu.bsp' )


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
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(FRNMLN)    TFRAME
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(SHPLEN)    TSHAPE

      DOUBLE PRECISION      BADBND ( 3, MAXVRT )
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      BSITE  ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FOVBND ( 3, MAXVRT )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      SVRADI ( 3 )
      DOUBLE PRECISION      TIMES  ( 2, 5, 2 )

      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               INS
      INTEGER               INSTID
      INTEGER               N
      INTEGER               NC
      INTEGER               SHPITR
      INTEGER               TMP

      LOGICAL               FOUND
      LOGICAL               RESULT ( 2 )
      LOGICAL               VISIBL

!
!     Saved Variables
!
      SAVE

!
!     Initial values
!
      DATA  ABCORS / 'NONE', 'CN', 'XCN', 'CN+S', 'XCN+S' /

      DATA                  INSTRS / 'ALPHA_CIRCLE_NONE', &
                                     'ALPHA_ELLIPSE_NONE', &
                                     'ALPHA_RECTANGLE_NONE', &
                                     'ALPHA_DIAMOND_NONE'    /

!
!     Begin every test family with an open call.
!

      CALL TOPEN ( 'F_FOVTRG' )

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

      ET = 0.D0


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
!     Instrument not recognized.

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad observer or target name.' )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'
      TARGET = 'BET'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'SUN'
      OBSRVR = 'SUN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad shape specification.' )

      ABCORR = 'LT+S'

      TSHAPE = 'Line'
      TARGET = 'BETA'
      OBSRVR = 'SUN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSHAPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad shape: ray.' )


      TSHAPE = 'RAY'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      ABCORR = 'XS'
      TFRAME = ' '

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad reference frame TFRAME' )


      TSHAPE = 'ELLIPSOID'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      ABCORR = 'XCN+S'
      TFRAME = 'EME2000'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

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

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Reference frame TFRAME not centered on target ' &
      //           'when TSHAPE requires it.'                   )

      TSHAPE = 'ELLIPSOID'
      TARGET = 'Mars'
      TFRAME = 'IAU_EARTH'
      OBSRVR = 'SUN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-existent reference frame.' )


      TARGET = 'beta'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'
      TFRAME = 'EME2000'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction for ellipsoid ' &
      //           'target.'                                 )

      ABCORR = 'XS'

      TSHAPE = 'ELLIPSOID'
      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      OBSRVR = 'SUN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

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

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

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

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
!---- Case -------------------------------------------------------------
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

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

!
!     Restore all three radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, SVRADI )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!---- Case -------------------------------------------------------------
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

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

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

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


      CALL VPACK  (  1.D0, -1.D0, 2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


      CALL VPACK  (  1.D0, 1.D0, -2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GDPOOL ( 'BODY399_RADII', 1, 3, N, RADII, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

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
      TARGET = 'BETA'
      TSHAPE = 'ELLIPSOID'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

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
      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

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
      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

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
      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

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
      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

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
      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      TFRAME = 'ITRF93'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No target ephemeris data available' )
!
!     This error is detected post-initialization.
!

      TSHAPE = 'ellipsoid'
      TARGET = 'GASPRA'
      TFRAME = 'IAU_GASPRA'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No observer ephemeris data available' )
!
!     This error is detected post-initialization.
!

      TSHAPE = 'ellipsoid'
      TARGET = 'SUN'
      TFRAME = 'IAU_SUN'
      OBSRVR = 'GASPRA'
      ABCORR = 'None'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No TFRAME orientation data available' )
!
!     This error is detected post-initialization.
!

      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      TFRAME = 'ITRF93'

      CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                    ABCORR, OBSRVR, ET,     VISIBL )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

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
      RESULT(1) = .TRUE.
      RESULT(2) = .FALSE.

!
!     The TIMES variable is indexed according to the following in which
!     case is an expected result of visible or not visible, abcorr are
!     ordered as shown below, and the target shape is either ellipsoid
!     or point. (case, abcorr, target shape)
!
!     1: None
!     2: CN
!     3: XCN
!     4: CN+S
!     5: XCN+S
!
!     The times in the TIMES variable below are from GFTFOV.
!
      TIMES(1,1,1) = 1.D0
      TIMES(2,1,1) = 602.D0
      TIMES(1,2,1) = 3.D0
      TIMES(2,2,1) = 603.D0
      TIMES(1,3,1) = -1.D0
      TIMES(2,3,1) = 599.D0
      TIMES(1,4,1) = 3.D0
      TIMES(2,4,1) = 603.D0
      TIMES(1,5,1) = -1.D0
      TIMES(2,5,1) = 599.D0

      TIMES(1,1,2) = 61.D0
      TIMES(2,1,2) = 542.D0
      TIMES(1,2,2) = 63.D0
      TIMES(2,2,2) = 543.D0
      TIMES(1,3,2) = 59.D0
      TIMES(2,3,2) = 539.D0
      TIMES(1,4,2) = 63.D0
      TIMES(2,4,2) = 543.D0
      TIMES(1,5,2) = 59.D0
      TIMES(2,5,2) = 539.D0

      TARGET = 'BETA'
      OBSRVR = 'SUN'
!
!     Loop over two target shape cases: ellipsoid and point.
!     For the ellipsoid cases, the start times should be one
!     minute early and the stop times should be one minute late.
!
      DO SHPITR = 1, 2

         IF ( SHPITR .EQ. 1 ) THEN

            TSHAPE = 'ELLIPSOID'
            TFRAME = 'BETAFIXED'

         ELSE

            TSHAPE = 'POINT'

         END IF

!
!     Loop through the instruments.
!
         DO INS = 1, NINST

            INST = INSTRS(INS)
!
!           Loop through the ABCORR values (5 of them)
!
            DO NC = 1, NCORS
!
! --- Case: ------------------------------------------------------
!
!
               ABCORR = ABCORS(NC)

!
!              Set up the TCASE call.
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

               DO TMP = 1, 2

!
!                 Is it visible?
!
                  CALL FOVTRG ( INST,   TARGET, TSHAPE, TFRAME, &
                                ABCORR, OBSRVR, TIMES(TMP,NC,SHPITR), &
                                VISIBL )
                  CALL CHCKXC ( .FALSE., ' ', OK )
                  CALL CHCKSL ( 'VISIBL', VISIBL, RESULT(TMP), OK )
               END DO

            END DO

         END DO

      END DO
!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

!
!     Clean up SPK files.
!
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
