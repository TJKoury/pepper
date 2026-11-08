!$Procedure F_FOVRAY ( FOVRAY family tests )

      SUBROUTINE F_FOVRAY ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        FOVRAY
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

      INCLUDE               'gf.inc'
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the higher-level SPICELIB
!     geometry routine FOVRAY.
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     S.C. Krening       (JPL)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    TSPICE Version 1.1.0, 29-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.0.0, 07-FEB-2012 (SCK) (NJB)
!
!-&


! $ Index_Entries
!
!     None.
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
      PARAMETER           ( PCK    = 'fovray.tpc' )

      CHARACTER*(*)         NPCK
      PARAMETER           ( NPCK   = 'nat.tpc' )

      CHARACTER*(*)         NSPK
      PARAMETER           ( NSPK   = 'nat.bsp' )

      CHARACTER*(*)         NIK
      PARAMETER           ( NIK    = 'nat.ti' )

      CHARACTER*(*)         SPK
      PARAMETER           ( SPK    = 'fovray.bsp' )


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
      PARAMETER           ( NCORS  = 3 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(CORLEN)    ABCORS ( NCORS )
      CHARACTER*(BDNMLN)    INST
      CHARACTER*(BDNMLN)    INSTRS ( NINST )
      CHARACTER*(KVARLN)    KVNAME
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(FRNMLN)    RFRAME
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      BADBND ( 3, MAXVRT )
      DOUBLE PRECISION      BSITE  ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FOVBND ( 3, MAXVRT )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      TIMES  ( 2 )

      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               INS
      INTEGER               INSTID
      INTEGER               N
      INTEGER               NC
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
      DATA  ABCORS / 'NONE', 'S', 'XS' /

      DATA                  INSTRS / 'ALPHA_CIRCLE_NONE', &
                                     'ALPHA_ELLIPSE_NONE', &
                                     'ALPHA_RECTANGLE_NONE', &
                                     'ALPHA_DIAMOND_NONE'    /

!
!     Begin every test family with an open call.
!

      CALL TOPEN ( 'F_FOVRAY' )

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
      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad observer name.' )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'BET'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Bad reference frame RFRAME' )


      OBSRVR = 'SUN'
      ABCORR = 'XCN+S'
      RFRAME = 'EME2000'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-existent reference frame.' )

      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'
      RFRAME = 'EME2000'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad aberration correction for ray ' &
      //           'target.'                                 )

      ABCORR = 'LT+S'
      OBSRVR = 'SUN'
      RFRAME = 'BETAFIXED'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No ID code for instrument.' )

      INST   = 'ALPHA_ELLIPSE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL        )

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

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL        )

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

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL         )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


!
!     Restore the first boundary vector and set the second
!     equal to the boresight vector.
!
      CALL MOVED ( FOVBND(1,1), 3, BADBND(1,1) )
      CALL MOVED ( BSITE,       3, BADBND(1,2) )

      CALL PDPOOL ( KVNAME, N, BADBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL        )

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

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(FACENOTFOUND)', OK )

!
!     Restore the instrument FOV boundary vectors.
!
      CALL PDPOOL ( KVNAME, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No observer ephemeris data available' )
!
!     This error is detected post-initialization.
!

      RFRAME = 'IAU_SUN'
      OBSRVR = 'GASPRA'
      ABCORR = 'S'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'No RFRAME orientation data available' )
!
!     This error is detected post-initialization.
!

      OBSRVR = 'MOON'
      RFRAME = 'ITRF93'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                    OBSRVR, ET,     VISIBL        )

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
!     The TIMES variable is indexed according to the expected RESULT,
!     so the first index in TIMES corresponds to a .TRUE. visibility
!     case, while the second corresponds to a .FALSE. visibility case.
!
      TIMES(1) = 61.5D0
      TIMES(2) = 540.5D0


      OBSRVR = 'SUN'
      RFRAME = 'BETA_VIEW_XY'

!
!           Set the ray's direction vector: BETA_VIEW_XY +X.
!
            CALL VPACK ( 1.D0, 0.D0, 0.D0, RAYDIR )

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
!           Set up the TCASE call.
!
            TITLE = 'Inst. #; #; inst frame #'

            CALL REPMC ( TITLE, '#', INST,   TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
            CALL REPMC ( TITLE, '#', RFRAME, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO TMP = 1, 2

               CALL TCASE ( TITLE )
!
!                 Is it visible?
!
               CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR, &
                             OBSRVR, TIMES(TMP),     VISIBL        )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'VISIBL', VISIBL, RESULT(TMP), OK )
            END DO

         END DO

      END DO

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

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
