C$Procedure F_FOVRAY ( FOVRAY family tests )

      SUBROUTINE F_FOVRAY ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        FOVRAY
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

      INCLUDE               'gf.inc'
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the higher-level SPICELIB
C     geometry routine FOVRAY.
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
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     S.C. Krening       (JPL)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    TSPICE Version 1.1.0, 29-MAY-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 1.0.0, 07-FEB-2012 (SCK) (NJB)
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     SPICELIB functions
C
c      DOUBLE PRECISION      SPD

C
C     Local parameters
C
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

C
C     Local variables
C
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

C
C     Saved Variables
C
      SAVE

C
C     Initial values
C
      DATA  ABCORS / 'NONE', 'S', 'XS' /

      DATA                  INSTRS / 'ALPHA_CIRCLE_NONE',
     .                               'ALPHA_ELLIPSE_NONE',
     .                               'ALPHA_RECTANGLE_NONE',
     .                               'ALPHA_DIAMOND_NONE'    /

C
C     Begin every test family with an open call.
C

      CALL TOPEN ( 'F_FOVRAY' )

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

      ET = 0.D0
      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad observer name.' )

      INST   = 'ALPHA_ELLIPSE_NONE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'BET'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad reference frame RFRAME' )


      OBSRVR = 'SUN'
      ABCORR = 'XCN+S'
      RFRAME = 'EME2000'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Non-existent reference frame.' )

      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'
      RFRAME = 'EME2000'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction for ray '
     .//           'target.'                                 )

      ABCORR = 'LT+S'
      OBSRVR = 'SUN'
      RFRAME = 'BETAFIXED'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No ID code for instrument.' )

      INST   = 'ALPHA_ELLIPSE'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL        )

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

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL        )

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

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL         )

      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


C
C     Restore the first boundary vector and set the second
C     equal to the boresight vector.
C
      CALL MOVED ( FOVBND(1,1), 3, BADBND(1,1) )
      CALL MOVED ( BSITE,       3, BADBND(1,2) )

      CALL PDPOOL ( KVNAME, N, BADBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL        )

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

      RFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(FACENOTFOUND)', OK )

C
C     Restore the instrument FOV boundary vectors.
C
      CALL PDPOOL ( KVNAME, N, FOVBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No observer ephemeris data available' )
C
C     This error is detected post-initialization.
C

      RFRAME = 'IAU_SUN'
      OBSRVR = 'GASPRA'
      ABCORR = 'S'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No RFRAME orientation data available' )
C
C     This error is detected post-initialization.
C

      OBSRVR = 'MOON'
      RFRAME = 'ITRF93'

      CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .              OBSRVR, ET,     VISIBL        )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )

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
      RESULT(1) = .TRUE.
      RESULT(2) = .FALSE.

C
C     The TIMES variable is indexed according to the expected RESULT,
C     so the first index in TIMES corresponds to a .TRUE. visibility
C     case, while the second corresponds to a .FALSE. visibility case.
C
      TIMES(1) = 61.5D0
      TIMES(2) = 540.5D0


      OBSRVR = 'SUN'
      RFRAME = 'BETA_VIEW_XY'

C
C           Set the ray's direction vector: BETA_VIEW_XY +X.
C
            CALL VPACK ( 1.D0, 0.D0, 0.D0, RAYDIR )

C
C     Loop through the instruments.
C
      DO INS = 1, NINST

         INST = INSTRS(INS)
C
C           Loop through the ABCORR values (5 of them)
C
         DO NC = 1, NCORS
C
C --- Case: ------------------------------------------------------
C
C
            ABCORR = ABCORS(NC)

C
C           Set up the TCASE call.
C
            TITLE = 'Inst. #; #; inst frame #'

            CALL REPMC ( TITLE, '#', INST,   TITLE )
            CALL REPMC ( TITLE, '#', ABCORR, TITLE )
            CALL REPMC ( TITLE, '#', RFRAME, TITLE )
            CALL CHCKXC ( .FALSE., ' ', OK )

            DO TMP = 1, 2

               CALL TCASE ( TITLE )
C
C                 Is it visible?
C
               CALL FOVRAY ( INST,   RAYDIR, RFRAME, ABCORR,
     .                       OBSRVR, TIMES(TMP),     VISIBL        )
               CALL CHCKXC ( .FALSE., ' ', OK )
               CALL CHCKSL ( 'VISIBL', VISIBL, RESULT(TMP), OK )
            END DO

         END DO

      END DO

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

      CALL CLPOOL
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
