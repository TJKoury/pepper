!$Procedure F_ZZGFFVU ( Test GF FOV utilities )

      SUBROUTINE F_ZZGFFVU ( OK )
      use pepper_legacy
      use pepper_inc_gf
      use pepper_inc_zzgf
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Test the GF private FOV utility package ZZGFFVU. Tests
!     are restricted to error handling.
!
! $ Required_Reading
!
!     GF
!
! $ Keywords
!
!     GF
!     SEARCH
!     TEST
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
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any exceptions. Routines in its
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
!     This routine tests exception handling performed by the private
!     SPICELIB GF FOV utility package ZZGFFVU. Computational
!     capabilities of ZZGFFVU are exercised by the test families
!
!        F_GFTFOV
!        F_GFRFOV
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
!
! $ Version
!
!-    TSPICE Version 4.1.0, 25-AUG-2021 (EDW)
!
!        Edited all error strings on radii values, and number of values
!        returned from kernel pool, to correspond to us-of/changes-in
!        ZZGFTREB.
!
!-    TSPICE Version 4.0.0, 30-MAY-2021 (NJB) (JDR)
!
!        Test cases were added to exercise bug fixes for large FOV
!        and non-unit boresight cases.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 3.1.0, 23-SEP-2008 (NJB)
!
!        Bug fix: initialized second vector of BADBND
!        for first degenerate ellipse test case.
!
!-    TSPICE Version 3.0.0, 20-AUG-2008 (NJB)
!
!        Updated to reflect changes in SPICE errors signaled
!        as a result of the ZZHULLAX dot product sign bug
!        fix.
!
!-    TSPICE Version 2.0.0, 30-JUL-2008 (NJB)
!
!        Updated to handle new FOV axis determination
!        scheme.
!
!-    TSPICE Version 1.0.0, 17-JUL-2008 (NJB)
!
!-&


! $ Index_Entries
!
!     Test the GF private FOV utility package ZZGFFVU
!
!-&


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


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 200 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               KVARLN
      PARAMETER           ( KVARLN = 32 )

      INTEGER               TXTSIZ
      PARAMETER           ( TXTSIZ = 2 )

!
!     Local variables
!
      CHARACTER*(LNSIZE)    ABCORR
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(BDNMLN)    INST
      CHARACTER*(KVARLN)    KVNAME
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET
      CHARACTER*(FRNMLN)    TFRAME
      CHARACTER*(SHPLEN)    TSHAPE
      CHARACTER*(LNSIZE)    TXTBUF ( TXTSIZ )

      DOUBLE PRECISION      ANGRAD
      DOUBLE PRECISION      BADBND ( 3, MAXVRT )
      DOUBLE PRECISION      BADRAD ( 3 )
      DOUBLE PRECISION      BIGRAD
      DOUBLE PRECISION      BOUNDS ( 3, 2 )
      DOUBLE PRECISION      BSITE  ( 3 )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      EPS
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FOVBND ( 3, MAXVRT )
      DOUBLE PRECISION      H
      DOUBLE PRECISION      LMBRAD
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NEGBOR ( 3 )
      DOUBLE PRECISION      POLE   ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      REFVEC ( 3 )
      DOUBLE PRECISION      ROTANG
      DOUBLE PRECISION      ROTBDS ( 3, 2 )
      DOUBLE PRECISION      ROTBOR ( 3 )
      DOUBLE PRECISION      SMLRAD
      DOUBLE PRECISION      SVRADI ( 3 )
      DOUBLE PRECISION      TRGPOS ( 3 )
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XFORMI ( 3, 3 )
      DOUBLE PRECISION      X      ( 3 )
      DOUBLE PRECISION      Y      ( 3 )

      INTEGER               HAN2
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               INSTID
      INTEGER               N
      INTEGER               OBSID
      INTEGER OBSID_A1 ( 1 )
      EQUIVALENCE ( OBSID, OBSID_A1 )
      INTEGER               TFRMID
      INTEGER TFRMID_A1 ( 1 )
      EQUIVALENCE ( TFRMID, TFRMID_A1 )

      LOGICAL               FOUND
      LOGICAL               VISTAT

!
!     Save everything.
!
      SAVE

!
!     Initial values
!


!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_ZZGFFVU' )

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
      CALL TCASE ( 'Bad observer or target name.' )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BET'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SN'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'SUN'
      OBSRVR = 'SUN'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad shape specification.' )

      ABCORR = 'LT+S'

      TSHAPE = 'Line'
      TARGET = 'BETA'
      OBSRVR = 'SUN'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )


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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Blank reference frame TFRAME for ' &
      //           'ray target'                      )


      TSHAPE = 'RAY'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      ABCORR = 'XS'
      TFRAME = ' '

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Non-existent reference frame.' )


      TARGET = 'beta'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'
      TFRAME = 'EME2000'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )



      CALL VPACK  (  1.D0, -1.D0, 2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )



      CALL VPACK  (  1.D0, 1.D0, -2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


!
!     Restore all three radii.
!
      CALL PDPOOL ( 'BODY399_RADII', 3, SVRADI )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Bad ray direction vector RAYDIR' )

      TSHAPE = 'RAY'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XS'

      TFRAME = 'J2000'

      CALL CLEARD ( 3, RAYDIR )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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
!     Keep the second boundary vector.
!
      CALL MOVED ( BSITE,       3, BADBND(1,1) )
      CALL MOVED ( FOVBND(1,2), 3, BADBND(1,2) )

      CALL PDPOOL ( KVNAME, N, BADBND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )

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
      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      TFRAME = 'ITRF93'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET = 0.D0
      CALL ZZGFFVST ( ET, VISTAT )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET = 0.D0
      CALL ZZGFFVST ( ET, VISTAT )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET = 0.D0
      CALL ZZGFFVST ( ET, VISTAT )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, TFRAME, ABCORR, OBSRVR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET = 0.D0
      CALL ZZGFFVST ( ET, VISTAT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


!*********************************************************************
!*
!*    Non-error exceptional cases
!*
!*********************************************************************

!*********************************************************************
!*
!*    Non-error cases
!*
!*********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create instrument with near-180 degree ' &
      //           'circular FOV, centered 2 m above the Moon, ' &
      //           'aimed at the Moon. Verify that ZZGFFVIN ' &
      //           'doesn''t create an ellipsoid that intersects ' &
      //           'the Moon.' )

!
!     We'll use as the observer the "Tranquility base" object from the
!     SPK created by TSTSPK. This object is located at radius 1737.402
!     km from the center of the Moon. The PCK created by TSTPCK sets
!     all three Moon radii to 1737.400 km. This test case relies on the
!     2m difference.
!
      OBSRVR = '301001'
      TARGET = 'Moon'
      TSHAPE = 'ELLIPSOID'
      FIXREF = 'IAU_MOON'
      ET     = 0.D0
      ABCORR = 'NONE'

!
!     Get the position of the center of the Moon relative to the
!     observer.
!
      CALL SPKPOS ( TARGET, ET, FIXREF, ABCORR, OBSRVR, TRGPOS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create an instrument frame that's centered at Tranquility base.
!     The axis directions of the frame are:
!
!         X:  North
!         Y:  East
!         Z:  Inward normal
!
      CALL VPACK  ( 0.D0,   0.D0, 1.D0,    POLE  )
      CALL TWOVEC ( TRGPOS, 3,    POLE, 1, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      OBSID  =  301001
      TFRMID = 1301001
      TFRAME = 'TRANQUILITY_INST'

!
!     Set the first instrument name and ID. Add the name-ID mapping
!     to the kernel pool.
!
      INSTID = 301001000
      INST   = 'INST_#'
      CALL REPMI( INST, '#', INSTID, INST )

      TXTBUF(1) = 'NAIF_BODY_NAME += ''#'''
      CALL REPMC ( TXTBUF(1), '#', INST, TXTBUF(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TXTBUF(2) = 'NAIF_BODY_CODE += #'
      CALL REPMI ( TXTBUF(2), '#', INSTID, TXTBUF(2) )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL LMPOOL ( TXTBUF, 2 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      KVNAME = 'FRAME_#'
      CALL REPMC ( KVNAME, '#', TFRAME, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PIPOOL( KVNAME, 1, TFRMID_A1 )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'FRAME_#_NAME'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PCPOOL( KVNAME, 1, TFRAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'FRAME_#_CLASS'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PIPOOL( KVNAME, 1, [4] )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'FRAME_#_CLASS_ID'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PIPOOL( KVNAME, 1, TFRMID_A1 )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'FRAME_#_CENTER'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PIPOOL( KVNAME, 1, OBSID_A1 )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'TKFRAME_#_RELATIVE'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PCPOOL( KVNAME, 1, FIXREF )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'TKFRAME_#_SPEC'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PCPOOL( KVNAME, 1, 'MATRIX' )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'TKFRAME_#_MATRIX'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL XPOSE ( XFORM,     XFORMI )
      CALL PDPOOL( KVNAME, 9, XFORMI )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create the specification of an instrument having a circular FOV.
!     The instrument boresight points in the +Z direction of the
!     instrument frame (towards the center of the Moon).
!
      KVNAME = 'INS#_FOV_SHAPE'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PCPOOL( KVNAME, 1, 'CIRCLE' )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'INS#_FOV_FRAME'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PCPOOL( KVNAME, 1, TFRAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, BSITE )
      CALL PDPOOL( KVNAME, 3,        BSITE )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create a circular FOV with angular radius equal the angular
!     radius of the limb as seen from the observer.
!
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC( .FALSE., ' ', OK )

      R = RADII(1)
      H = 2.0D-3
      D = R + H

      LMBRAD = ASIN( R / D )
      ANGRAD = LMBRAD
!
!     Create a reference vector by rotating the boresight by ANGRAD
!     radians about the +Y axis of the instrument frame.
!
      CALL VPACK ( 0.D0, 1.D0, 0.D0, Y )

      CALL VROTV ( BSITE, Y, ANGRAD, REFVEC )

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 3, REFVEC )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Initialize the GF FOV subsystem with parameters for the
!     instrument.
!
      CALL CLEARD ( 3, RAYDIR )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Now test whether the target is in the FOV.
!
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create instrument with near-180 degree ' &
      //           'circular FOV, centered 2 m above the Moon, ' &
      //           'aimed away from the Moon. Verify that the target ' &
      //           'is outside of the FOV.' )

      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL VMINUS( BSITE,     NEGBOR )
      CALL PDPOOL( KVNAME, 3, NEGBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create a new reference vector.
!
      CALL VROTV ( NEGBOR, Y, ANGRAD, REFVEC )

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 3, REFVEC )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Now test whether the target is in the FOV.
!
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create instrument with near-180 degree ' &
      //           'circular FOV, centered 2 m above the Moon. ' &
      //           'Rotate the instrument boresight from the nadir ' &
      //           'direction by slightly less than twice the ' &
      //           'angular radius of the FOV and verify the target ' &
      //           'intersects the FOV cone.' )

      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      EPS    = 1.D-12
      ROTANG = 2*ANGRAD - EPS

      CALL VROTV ( BSITE, Y, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create a new reference vector.
!
      CALL VROTV ( ROTBOR, Y, ANGRAD, REFVEC )

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 3, REFVEC )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Now test whether the target is in the FOV.
!
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create instrument with near-180 degree ' &
      //           'circular FOV, centered 2 m above the Moon. ' &
      //           'Rotate the instrument boresight from the nadir ' &
      //           'direction by slightly more than twice the ' &
      //           'angular radius of the FOV and verify the target ' &
      //           'does not intersect the FOV cone.' )


      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      EPS    = 1.D-12
      ROTANG = 2*ANGRAD + EPS

      CALL VROTV ( BSITE, Y, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Create a new reference vector.
!
      CALL VROTV ( ROTBOR, Y, ANGRAD, REFVEC )

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 3, REFVEC )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Now test whether the target is in the FOV.
!
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .FALSE., OK )

!
!     The following cases use an elliptical FOV.
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create instrument elliptical FOV. Make the ' &
      //           'angular size corresponding to the semi-major ' &
      //           'axis nearly 90 degrees and make the angular size ' &
      //           'corresponding to semi-minor axis half that. Point ' &
      //           'the instrument at the target.' )


      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL VPACK ( 0.D0, 0.D0, 1.D0, BSITE )
      CALL PDPOOL( KVNAME, 3,        BSITE )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'INS#_FOV_SHAPE'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PCPOOL( KVNAME, 1, 'ELLIPSE' )
      CALL CHCKXC( .FALSE., ' ', OK )

      BIGRAD = ANGRAD
      SMLRAD = ANGRAD / 2

      CALL VPACK ( 1.D0, 0.D0, 0.D0, X )


      CALL VROTV ( BSITE, Y, BIGRAD, BOUNDS(1,1) )
      CALL VROTV ( BSITE, X, SMLRAD, BOUNDS(1,2) )


      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 6, BOUNDS )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Now test whether the target is in the FOV.
!
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create instrument elliptical FOV. Make the ' &
      //           'angular size corresponding to the semi-major ' &
      //           'axis nearly 90 degrees and make the angular size ' &
      //           'corresponding to semi-minor axis half that. ' &
      //           'Rotate the FOV from the nadir direction, about ' &
      //           'the X axis, in the direction of the semi-minor ' &
      //           'axis, by slightly more than the sum of the FOV ' &
      //           'angular radius in that direction and the limb ' &
      //           'angular radius. Verify the target is not in the ' &
      //           'FOV cone.' )


      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     We use a looser tolerance for this test because the FOV
!     intersection computation relies on an ellipse-ray angular
!     separation computation, which relies on a root-finding process
!     that cannot achieve high precision. The value 1.D-10 radians
!     was found to work on a Mac Intel/gfortran/64-bit platform. The
!     value 1.D-8 radians was selected for portability.
!
      EPS    = 1.D-8
      ROTANG = SMLRAD + LMBRAD + EPS

      CALL VROTV ( BSITE, X, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     We must rotate the boundary vectors as well.
!
      DO I = 1, 2
         CALL VROTV( BOUNDS(1,I), X, ROTANG, ROTBDS(1,I) )
      END DO

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 6, ROTBDS )
      CALL CHCKXC( .FALSE., ' ', OK )


      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Now test whether the target is in the FOV.
!
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create instrument elliptical FOV. Make the ' &
      //           'angular size corresponding to the semi-major ' &
      //           'axis nearly 90 degrees and make the angular size ' &
      //           'corresponding to semi-minor axis half that. ' &
      //           'Rotate the FOV from the nadir direction, about ' &
      //           'the X axis, in the direction of the semi-minor ' &
      //           'axis, by slightly *less* than the sum of the FOV ' &
      //           'angular radius in that direction and the limb ' &
      //           'angular radius. Verify the target intersects ' &
      //           'FOV cone.' )


      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     We use a looser tolerance for this test because the FOV
!     intersection computation relies on an ellipse-ray angular
!     separation computation, which relies on a root-finding process
!     that cannot achieve high precision. The value 1.D-10 radians
!     was found to work on a Mac Intel/gfortran/64-bit platform. The
!     value 1.D-8 radians was selected for portability.
!
      EPS    = 1.D-8
      ROTANG = SMLRAD + LMBRAD - EPS

      CALL VROTV ( BSITE, X, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     We must rotate the boundary vectors as well.
!
      DO I = 1, 2
         CALL VROTV( BOUNDS(1,I), X, ROTANG, ROTBDS(1,I) )
      END DO

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 6, ROTBDS )
      CALL CHCKXC( .FALSE., ' ', OK )


      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Now test whether the target is in the FOV.
!
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .TRUE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create instrument elliptical FOV. Make the ' &
      //           'angular size corresponding to the semi-major ' &
      //           'axis nearly 90 degrees and make the angular size ' &
      //           'corresponding to semi-minor axis half that. ' &
      //           'Rotate the FOV from the nadir direction, about ' &
      //           'the Y axis, in the direction of the semi-major ' &
      //           'axis, by slightly more than the sum of the FOV ' &
      //           'angular radius in that direction and the limb ' &
      //           'angular radius. Verify the target does not ' &
      //           'intersect the FOV cone.' )

      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     We use a looser tolerance for this test because the FOV
!     intersection computation relies on an ellipse-ray angular
!     separation computation, which relies on a root-finding process
!     that cannot achieve high precision. The value 1.D-10 radians
!     was found to work on a Mac Intel/gfortran/64-bit platform. The
!     value 1.D-8 radians was selected for portability.
!
      EPS    = 1.D-8
      ROTANG = BIGRAD + LMBRAD + EPS

      CALL VROTV ( BSITE, Y, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     We must rotate the boundary vectors as well.
!
      DO I = 1, 2
         CALL VROTV( BOUNDS(1,I), Y, ROTANG, ROTBDS(1,I) )
      END DO

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 6, ROTBDS )
      CALL CHCKXC( .FALSE., ' ', OK )


      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Now test whether the target is in the FOV.
!
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Create instrument elliptical FOV. Make the ' &
      //           'angular size corresponding to the semi-major ' &
      //           'axis nearly 90 degrees and make the angular size ' &
      //           'corresponding to semi-minor axis half that. ' &
      //           'Rotate the FOV from the nadir direction, about ' &
      //           'the Y axis, in the direction of the semi-major ' &
      //           'axis, by slightly *less* than the sum of the FOV ' &
      //           'angular radius in that direction and the limb ' &
      //           'angular radius. Verify the target intersects ' &
      //           'the FOV cone.' )

      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     We use a looser tolerance for this test because the FOV
!     intersection computation relies on an ellipse-ray angular
!     separation computation, which relies on a root-finding process
!     that cannot achieve high precision. The value 1.D-10 radians
!     was found to work on a Mac Intel/gfortran/64-bit platform. The
!     value 1.D-8 radians was selected for portability.
!
      EPS    = 1.D-8
      ROTANG = BIGRAD + LMBRAD - EPS

      CALL VROTV ( BSITE, Y, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     We must rotate the boundary vectors as well.
!
      DO I = 1, 2
         CALL VROTV( BOUNDS(1,I), Y, ROTANG, ROTBDS(1,I) )
      END DO

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 6, ROTBDS )
      CALL CHCKXC( .FALSE., ' ', OK )


      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR, &
                      TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

!
!     Now test whether the target is in the FOV.
!
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .TRUE., OK )

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
