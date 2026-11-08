C$Procedure F_ZZGFFVU ( Test GF FOV utilities )

      SUBROUTINE F_ZZGFFVU ( OK )

C$ Abstract
C
C     Test the GF private FOV utility package ZZGFFVU. Tests
C     are restricted to error handling.
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
C     SEARCH
C     TEST
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
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any exceptions. Routines in its
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
C     This routine tests exception handling performed by the private
C     SPICELIB GF FOV utility package ZZGFFVU. Computational
C     capabilities of ZZGFFVU are exercised by the test families
C
C        F_GFTFOV
C        F_GFRFOV
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
C
C$ Version
C
C-    TSPICE Version 4.1.0, 25-AUG-2021 (EDW)
C
C        Edited all error strings on radii values, and number of values
C        returned from kernel pool, to correspond to us-of/changes-in 
C        ZZGFTREB.
C
C-    TSPICE Version 4.0.0, 30-MAY-2021 (NJB) (JDR)
C
C        Test cases were added to exercise bug fixes for large FOV
C        and non-unit boresight cases.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 3.1.0, 23-SEP-2008 (NJB)
C
C        Bug fix: initialized second vector of BADBND
C        for first degenerate ellipse test case.
C
C-    TSPICE Version 3.0.0, 20-AUG-2008 (NJB)
C
C        Updated to reflect changes in SPICE errors signaled
C        as a result of the ZZHULLAX dot product sign bug
C        fix.
C
C-    TSPICE Version 2.0.0, 30-JUL-2008 (NJB)
C
C        Updated to handle new FOV axis determination
C        scheme.
C
C-    TSPICE Version 1.0.0, 17-JUL-2008 (NJB)
C
C-&


C$ Index_Entries
C
C     Test the GF private FOV utility package ZZGFFVU
C
C-&


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

C
C     Local variables
C
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
      INTEGER               TFRMID

      LOGICAL               FOUND
      LOGICAL               VISTAT

C
C     Save everything.
C
      SAVE

C
C     Initial values
C


C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_ZZGFFVU' )

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
      CALL TCASE ( 'Bad observer or target name.' )

      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ELLIPSOID'

      CALL VPACK ( 0.D0, 0.D0, 1.D0, RAYDIR )

      TARGET = 'BET'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SUN'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


      TARGET = 'BETA'
      TFRAME = 'BETAFIXED'
      ABCORR = 'NONE'
      OBSRVR = 'SN'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Observer equals target' )

      TARGET = 'SUN'
      OBSRVR = 'SUN'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )


      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad shape specification.' )

      ABCORR = 'LT+S'

      TSHAPE = 'Line'
      TARGET = 'BETA'
      OBSRVR = 'SUN'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )


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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Blank reference frame TFRAME for '
     .//           'ray target'                      )


      TSHAPE = 'RAY'
      TARGET = 'BETA'
      OBSRVR = 'SUN'
      ABCORR = 'XS'
      TFRAME = ' '

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Reference frame TFRAME not centered on target '
     .//           'when TSHAPE requires it.'                   )

      TSHAPE = 'ELLIPSOID'
      TARGET = 'Mars'
      TFRAME = 'IAU_EARTH'
      OBSRVR = 'SUN'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Non-existent reference frame.' )


      TARGET = 'beta'
      OBSRVR = 'MOON'
      ABCORR = 'XCN+S'
      TFRAME = 'EME2000'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )



      CALL VPACK  (  1.D0, -1.D0, 2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )



      CALL VPACK  (  1.D0, 1.D0, -2.D0, BADRAD )

      CALL PDPOOL ( 'BODY399_RADII', 3, BADRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(BADAXISLENGTH)', OK )


C
C     Restore all three radii.
C
      CALL PDPOOL ( 'BODY399_RADII', 3, SVRADI )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Bad ray direction vector RAYDIR' )

      TSHAPE = 'RAY'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      ABCORR = 'XS'

      TFRAME = 'J2000'

      CALL CLEARD ( 3, RAYDIR )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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
C     Keep the second boundary vector.
C
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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )

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
      INST   = 'ALPHA_ELLIPSE_NONE'
      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      TFRAME = 'ITRF93'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET = 0.D0
      CALL ZZGFFVST ( ET, VISTAT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No target ephemeris data available' )
C
C     This error is detected post-initialization.
C

      TSHAPE = 'ellipsoid'
      TARGET = 'GASPRA'
      TFRAME = 'IAU_GASPRA'
      OBSRVR = 'MOON'
      ABCORR = 'None'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET = 0.D0
      CALL ZZGFFVST ( ET, VISTAT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No observer ephemeris data available' )
C
C     This error is detected post-initialization.
C
      TSHAPE = 'ellipsoid'
      TARGET = 'SUN'
      TFRAME = 'IAU_SUN'
      OBSRVR = 'GASPRA'
      ABCORR = 'None'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET = 0.D0
      CALL ZZGFFVST ( ET, VISTAT )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'No TFRAME orientation data available' )
C
C     This error is detected post-initialization.
C
      TSHAPE = 'ellipsoid'
      TARGET = 'EARTH'
      OBSRVR = 'MOON'
      TFRAME = 'ITRF93'

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, TFRAME, ABCORR, OBSRVR )
      CALL CHCKXC ( .FALSE., ' ', OK )


      ET = 0.D0
      CALL ZZGFFVST ( ET, VISTAT )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C*********************************************************************
C*
C*    Non-error exceptional cases
C*
C*********************************************************************

C*********************************************************************
C*
C*    Non-error cases
C*
C*********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create instrument with near-180 degree '
     .//           'circular FOV, centered 2 m above the Moon, '
     .//           'aimed at the Moon. Verify that ZZGFFVIN '
     .//           'doesn''t create an ellipsoid that intersects '
     .//           'the Moon.' )

C
C     We'll use as the observer the "Tranquility base" object from the
C     SPK created by TSTSPK. This object is located at radius 1737.402
C     km from the center of the Moon. The PCK created by TSTPCK sets
C     all three Moon radii to 1737.400 km. This test case relies on the
C     2m difference.
C
      OBSRVR = '301001'
      TARGET = 'Moon'
      TSHAPE = 'ELLIPSOID'
      FIXREF = 'IAU_MOON'
      ET     = 0.D0
      ABCORR = 'NONE'

C
C     Get the position of the center of the Moon relative to the
C     observer.
C
      CALL SPKPOS ( TARGET, ET, FIXREF, ABCORR, OBSRVR, TRGPOS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create an instrument frame that's centered at Tranquility base.
C     The axis directions of the frame are:
C
C         X:  North
C         Y:  East
C         Z:  Inward normal
C
      CALL VPACK  ( 0.D0,   0.D0, 1.D0,    POLE  )
      CALL TWOVEC ( TRGPOS, 3,    POLE, 1, XFORM )
      CALL CHCKXC ( .FALSE., ' ', OK )

      OBSID  =  301001
      TFRMID = 1301001
      TFRAME = 'TRANQUILITY_INST'

C
C     Set the first instrument name and ID. Add the name-ID mapping
C     to the kernel pool.
C
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
      CALL PIPOOL( KVNAME, 1, TFRMID )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'FRAME_#_NAME'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PCPOOL( KVNAME, 1, TFRAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'FRAME_#_CLASS'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PIPOOL( KVNAME, 1, 4 )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'FRAME_#_CLASS_ID'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PIPOOL( KVNAME, 1, TFRMID )
      CALL CHCKXC( .FALSE., ' ', OK )

      KVNAME = 'FRAME_#_CENTER'
      CALL REPMI ( KVNAME, '#', TFRMID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )
      CALL PIPOOL( KVNAME, 1, OBSID )
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

C
C     Create the specification of an instrument having a circular FOV.
C     The instrument boresight points in the +Z direction of the
C     instrument frame (towards the center of the Moon).
C
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

C
C     Create a circular FOV with angular radius equal the angular
C     radius of the limb as seen from the observer.
C
      CALL BODVRD ( TARGET, 'RADII', 3, N, RADII )
      CALL CHCKXC( .FALSE., ' ', OK )

      R = RADII(1)
      H = 2.0D-3
      D = R + H

      LMBRAD = ASIN( R / D )
      ANGRAD = LMBRAD
C
C     Create a reference vector by rotating the boresight by ANGRAD
C     radians about the +Y axis of the instrument frame.
C
      CALL VPACK ( 0.D0, 1.D0, 0.D0, Y )

      CALL VROTV ( BSITE, Y, ANGRAD, REFVEC )

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 3, REFVEC )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Initialize the GF FOV subsystem with parameters for the
C     instrument.
C
      CALL CLEARD ( 3, RAYDIR )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Now test whether the target is in the FOV.
C
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .TRUE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create instrument with near-180 degree '
     .//           'circular FOV, centered 2 m above the Moon, '
     .//           'aimed away from the Moon. Verify that the target '
     .//           'is outside of the FOV.' )

      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL VMINUS( BSITE,     NEGBOR )
      CALL PDPOOL( KVNAME, 3, NEGBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create a new reference vector.
C
      CALL VROTV ( NEGBOR, Y, ANGRAD, REFVEC )

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 3, REFVEC )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Now test whether the target is in the FOV.
C
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create instrument with near-180 degree '
     .//           'circular FOV, centered 2 m above the Moon. '
     .//           'Rotate the instrument boresight from the nadir '
     .//           'direction by slightly less than twice the '
     .//           'angular radius of the FOV and verify the target '
     .//           'intersects the FOV cone.' )

      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      EPS    = 1.D-12
      ROTANG = 2*ANGRAD - EPS

      CALL VROTV ( BSITE, Y, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create a new reference vector.
C
      CALL VROTV ( ROTBOR, Y, ANGRAD, REFVEC )

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 3, REFVEC )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Now test whether the target is in the FOV.
C
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .TRUE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create instrument with near-180 degree '
     .//           'circular FOV, centered 2 m above the Moon. '
     .//           'Rotate the instrument boresight from the nadir '
     .//           'direction by slightly more than twice the '
     .//           'angular radius of the FOV and verify the target '
     .//           'does not intersect the FOV cone.' )


      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      EPS    = 1.D-12
      ROTANG = 2*ANGRAD + EPS

      CALL VROTV ( BSITE, Y, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Create a new reference vector.
C
      CALL VROTV ( ROTBOR, Y, ANGRAD, REFVEC )

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 3, REFVEC )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Now test whether the target is in the FOV.
C
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .FALSE., OK )

C
C     The following cases use an elliptical FOV.
C

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create instrument elliptical FOV. Make the '
     .//           'angular size corresponding to the semi-major '
     .//           'axis nearly 90 degrees and make the angular size '
     .//           'corresponding to semi-minor axis half that. Point '
     .//           'the instrument at the target.' )


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

      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Now test whether the target is in the FOV.
C
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .TRUE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create instrument elliptical FOV. Make the '
     .//           'angular size corresponding to the semi-major '
     .//           'axis nearly 90 degrees and make the angular size '
     .//           'corresponding to semi-minor axis half that. '
     .//           'Rotate the FOV from the nadir direction, about '
     .//           'the X axis, in the direction of the semi-minor '
     .//           'axis, by slightly more than the sum of the FOV '
     .//           'angular radius in that direction and the limb '
     .//           'angular radius. Verify the target is not in the '
     .//           'FOV cone.' )


      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     We use a looser tolerance for this test because the FOV
C     intersection computation relies on an ellipse-ray angular
C     separation computation, which relies on a root-finding process
C     that cannot achieve high precision. The value 1.D-10 radians
C     was found to work on a Mac Intel/gfortran/64-bit platform. The
C     value 1.D-8 radians was selected for portability.
C
      EPS    = 1.D-8
      ROTANG = SMLRAD + LMBRAD + EPS

      CALL VROTV ( BSITE, X, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     We must rotate the boundary vectors as well.
C
      DO I = 1, 2
         CALL VROTV( BOUNDS(1,I), X, ROTANG, ROTBDS(1,I) )
      END DO

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 6, ROTBDS )
      CALL CHCKXC( .FALSE., ' ', OK )


      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Now test whether the target is in the FOV.
C
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .FALSE., OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create instrument elliptical FOV. Make the '
     .//           'angular size corresponding to the semi-major '
     .//           'axis nearly 90 degrees and make the angular size '
     .//           'corresponding to semi-minor axis half that. '
     .//           'Rotate the FOV from the nadir direction, about '
     .//           'the X axis, in the direction of the semi-minor '
     .//           'axis, by slightly *less* than the sum of the FOV '
     .//           'angular radius in that direction and the limb '
     .//           'angular radius. Verify the target intersects '
     .//           'FOV cone.' )


      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     We use a looser tolerance for this test because the FOV
C     intersection computation relies on an ellipse-ray angular
C     separation computation, which relies on a root-finding process
C     that cannot achieve high precision. The value 1.D-10 radians
C     was found to work on a Mac Intel/gfortran/64-bit platform. The
C     value 1.D-8 radians was selected for portability.
C
      EPS    = 1.D-8
      ROTANG = SMLRAD + LMBRAD - EPS

      CALL VROTV ( BSITE, X, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     We must rotate the boundary vectors as well.
C
      DO I = 1, 2
         CALL VROTV( BOUNDS(1,I), X, ROTANG, ROTBDS(1,I) )
      END DO

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 6, ROTBDS )
      CALL CHCKXC( .FALSE., ' ', OK )


      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Now test whether the target is in the FOV.
C
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .TRUE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create instrument elliptical FOV. Make the '
     .//           'angular size corresponding to the semi-major '
     .//           'axis nearly 90 degrees and make the angular size '
     .//           'corresponding to semi-minor axis half that. '
     .//           'Rotate the FOV from the nadir direction, about '
     .//           'the Y axis, in the direction of the semi-major '
     .//           'axis, by slightly more than the sum of the FOV '
     .//           'angular radius in that direction and the limb '
     .//           'angular radius. Verify the target does not '
     .//           'intersect the FOV cone.' )

      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     We use a looser tolerance for this test because the FOV
C     intersection computation relies on an ellipse-ray angular
C     separation computation, which relies on a root-finding process
C     that cannot achieve high precision. The value 1.D-10 radians
C     was found to work on a Mac Intel/gfortran/64-bit platform. The
C     value 1.D-8 radians was selected for portability.
C
      EPS    = 1.D-8
      ROTANG = BIGRAD + LMBRAD + EPS

      CALL VROTV ( BSITE, Y, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     We must rotate the boundary vectors as well.
C
      DO I = 1, 2
         CALL VROTV( BOUNDS(1,I), Y, ROTANG, ROTBDS(1,I) )
      END DO

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 6, ROTBDS )
      CALL CHCKXC( .FALSE., ' ', OK )


      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Now test whether the target is in the FOV.
C
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Create instrument elliptical FOV. Make the '
     .//           'angular size corresponding to the semi-major '
     .//           'axis nearly 90 degrees and make the angular size '
     .//           'corresponding to semi-minor axis half that. '
     .//           'Rotate the FOV from the nadir direction, about '
     .//           'the Y axis, in the direction of the semi-major '
     .//           'axis, by slightly *less* than the sum of the FOV '
     .//           'angular radius in that direction and the limb '
     .//           'angular radius. Verify the target intersects '
     .//           'the FOV cone.' )

      KVNAME = 'INS#_BORESIGHT'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     We use a looser tolerance for this test because the FOV
C     intersection computation relies on an ellipse-ray angular
C     separation computation, which relies on a root-finding process
C     that cannot achieve high precision. The value 1.D-10 radians
C     was found to work on a Mac Intel/gfortran/64-bit platform. The
C     value 1.D-8 radians was selected for portability.
C
      EPS    = 1.D-8
      ROTANG = BIGRAD + LMBRAD - EPS

      CALL VROTV ( BSITE, Y, ROTANG, ROTBOR )

      CALL PDPOOL( KVNAME, 3, ROTBOR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     We must rotate the boundary vectors as well.
C
      DO I = 1, 2
         CALL VROTV( BOUNDS(1,I), Y, ROTANG, ROTBDS(1,I) )
      END DO

      KVNAME = 'INS#_FOV_BOUNDARY'
      CALL REPMI ( KVNAME, '#', INSTID, KVNAME )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL PDPOOL( KVNAME, 6, ROTBDS )
      CALL CHCKXC( .FALSE., ' ', OK )


      CALL ZZGFFVIN ( INST,   TSHAPE, RAYDIR,
     .                TARGET, FIXREF, ABCORR, OBSRVR )
      CALL CHCKXC( .FALSE., ' ', OK )

C
C     Now test whether the target is in the FOV.
C
      CALL ZZGFFVST ( ET, VISTAT )
      CALL CHCKXC( .FALSE., ' ', OK )

      CALL CHCKSL ( 'VISTAT', VISTAT, .TRUE., OK )

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
