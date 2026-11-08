C$Procedure F_ZZSINUTL ( ZZSINUTL tests )
 
      SUBROUTINE F_ZZSINUTL ( OK )
 
C$ Abstract
C
C     Exercise the entry points of routine ZZSINUTL.
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
      
      INCLUDE 'dsk.inc'
      
      
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
C     This routine tests the entry points of the SPICELIB routine 
C     ZZSINUTL:
C
C
C        ZZSUELIN:   Initialize ray intercept and near point utilities 
C                    for ellipsoidal target surface
C
C
C        ZZSUDSKI:   Initialize ray intercept and near point utilities 
C                    for DSK target surface
C
C        ZZMINRAD:   Return minimum spherical bounding radius for
C                    surface
C
C        ZZMAXRAD:   Return maximum spherical bounding radius for
C                    surface
C
C        ZZRAYSFX:   Compute ray-surface intercept given inputs
C                    set by ZZSUELIN or ZZSUDSKI
C     
C
C        ZZRAYNP:    Compute nearest point on surface to ray given
C                    inputs set by ZZSUELIN or ZZSUDSKI
C
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
C-    TSPICE Version 1.0.0, 01-JUN-2016 (NJB)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      JYEAR
      LOGICAL               EXISTS

C
C     Local Parameters 
C
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'zzsinutl_0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1   = 'zzsinutl_1.bds' )


      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zzsinutl.tpc' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )
      
C     DOUBLE PRECISION      TIGHT
C     PARAMETER           ( TIGHT  = 1.D-11 )
 
      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

C
C     Local Variables 
C  
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(NAMLEN)    KVNAME
      CHARACTER*(NAMLEN)    TARGET
C      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      DIST 
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      MAXRAD
      DOUBLE PRECISION      MINRAD
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      SPOINT ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      XDIST
      DOUBLE PRECISION      XMAX
      DOUBLE PRECISION      XMIN
      DOUBLE PRECISION      XRADII ( 3 )
      DOUBLE PRECISION      XPNEAR ( 3 )
      DOUBLE PRECISION      XPT    ( 3 )
      DOUBLE PRECISION      XXPT   ( 3 )

      INTEGER               BODYID
      INTEGER               FIXFID
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NSURF
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               SURFID
      
      LOGICAL               FOUND

C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZSINUTL' )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Set-up: create PCK.' )

      IF ( EXISTS(PCK0) ) THEN
         
         CALL DELFIL ( PCK0 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


      CALL T_PCK08 ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Set-up: create Mars DSK.' )

      IF ( EXISTS(DSK0) ) THEN
         
         CALL DELFIL ( DSK0 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


      BODYID = 499
      SURFID = 1
      FIXREF = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      NLON   = 40
      NLAT   = 20


      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Set-up: create scaled-down Mars DSK.' )

      IF ( EXISTS(DSK1) ) THEN
         
         CALL DELFIL ( DSK1 ) 
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


C
C     Get target radii from PCK file.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Temporarily replace radii in pool with scaled
C     radii.
C
      CALL VSCL ( 0.5D0, XRADII, RADII )

      KVNAME = 'BODY499_RADII'

      CALL PDPOOL ( KVNAME, 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )



C
C     This file contains "surface 2" for Mars.
C
      SURFID = 2
      BODYID = 499
      FIXREF = 'IAU_MARS'

      NLON   = 40
      NLAT   = 20

      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Restore original Mars radii.
C
      CALL PDPOOL ( KVNAME, 3, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )



C**********************************************************************
C
C     ZZSINUTL error case
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZSINUTL error: direct call to ZZSINUTL.' )

      
      CALL ZZSINUTL ( BODYID, NSURF,  SRFLST, ET,    FIXFID,
     .                VERTEX, RAYDIR, SPOINT, FOUND, MINRAD,
     .                MAXRAD, PNEAR,  DIST                  )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )



C**********************************************************************
C
C     ZZMAXRAD / ZZSUDSKI / ZZSUELIN tests
C
C**********************************************************************

C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZMAXRAD: ellipsoid shape.' )

C
C     Set target parameters.
C
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get target radii from PCK file.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMAX = MAX ( XRADII(1), XRADII(2), XRADII(3) )

C
C     Get the max target radius.
C     
      CALL ZZMAXRAD ( MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      
      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~', XMAX, TOL, OK )
      

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZMAXRAD: DSK shape; use two surfaces.' )

C
C     Load DSKs.
C
      CALL FURNSH ( DSK0 )
      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set target parameters.
C
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get target radii from PCK file.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMAX = MAX ( XRADII(1), XRADII(2), XRADII(3) )

C
C     Get the max target radius. We expect surface 2 not to
C     contribute.
C     
C     In principle, the maximum radius should match that of the
C     ellipsoid, since the tessellation, which uses an even number of
C     latitude bands, places some vertices on the target body's
C     equator. However, round-off error should be expected.
C
      CALL ZZMAXRAD ( MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      
      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAX, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZMAXRAD: DSK shape; use empty surface list.' )

C
C     Set target parameters.
C
      BODYID    = 499
      NSURF     = 0
      SRFLST(1) = 1
      SRFLST(2) = 2

      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get target radii from PCK file.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMAX = MAX ( XRADII(1), XRADII(2), XRADII(3) )

C
C     Get the max target radius. We expect surface 2 not to
C     contribute.
C     
      CALL ZZMAXRAD ( MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      
      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAX, TOL, OK )
      

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZMAXRAD: DSK shape; use surface 2 only.' )

C
C     Set target parameters.
C
      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2


      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get target radii from PCK file.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMAX = 0.5D0 * MAX ( XRADII(1), XRADII(2), XRADII(3) )

C
C     Get the max target radius. We expect surface 2 not to
C     contribute.
C     
      CALL ZZMAXRAD ( MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      
      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAX, TOL, OK )
      

C**********************************************************************
C
C     ZZSUELIN error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZSUELIN: ellipsoid radii are not available.' )

C
C     Radii for the target are not loaded.
C
      BODYID = -777

      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )



C**********************************************************************
C
C     ZZSUDSKI error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZSUDSKI: surface count too large.' )

      NSURF = MAXSRF + 1

      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZSUDSKI: surface count too small.' )

      NSURF = -1

      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )




C**********************************************************************
C
C     ZZMINRAD / ZZSUDSKI / ZZSUELIN tests
C
C**********************************************************************

C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZMINRAD: ellipsoid shape.' )

C
C     Set target parameters.
C
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get target radii from PCK file.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMIN = MIN ( XRADII(1), XRADII(2), XRADII(3) )

C
C     Get the min target radius.
C     
      CALL ZZMINRAD ( MINRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      
      CALL CHCKSD ( 'MINRAD', MINRAD, '~', XMIN, TOL, OK )
      

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZMINRAD: DSK shape; use two surfaces.' )

C
C     The DSKs are already loaded.
C

C
C     Set target parameters.
C
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

C
C     Get the expected minimum radius via a brute force
C     computation which considers each plate.
C
      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, XMIN, MAXRAD ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set the target parameters.
C
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      CALL ZZMINRAD ( MINRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMIN, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZMINRAD: DSK shape; use empty surface list.' )

C
C     Set target parameters.
C
      BODYID    = 499
      NSURF     = 0
      SRFLST(1) = 1
      SRFLST(2) = 2

      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get the expected minimum radius via a brute force
C     computation which considers each plate.
C
      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, XMIN, MAXRAD ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
 
 
C
C     Get the min target radius. We expect surface 2 to
C     contribute this value.
C     
      CALL ZZMINRAD ( MINRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMIN, TOL, OK )
      

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZMINRAD: DSK shape; use surface 2 only.' )

C
C     Set target parameters.
C
      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2


      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Get the expected minimum radius via a brute force
C     computation which considers each plate.
C
      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, XMIN, MAXRAD ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

C 
C     Get the min target radius. We expect surface 2 to
C     contribute this value.

      CALL ZZMINRAD ( MINRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT
      
      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMIN, TOL, OK )
      


C**********************************************************************
C
C     ZZRAYSFX tests
C
C**********************************************************************

C**********************************************************************
C
C     Normal cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYSFX: Ellipsoid intercept.' )

C
C     Set target parameters.
C
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set ray vertex and direction.
C
      CALL VPACK ( -5000.D0, 6000.D0, 10000.D0, VERTEX )
      
      CALL VMINUS ( VERTEX, RAYDIR )

C
C     Get target radii from PCK file.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMIN = MIN ( XRADII(1), XRADII(2), XRADII(3) )

C
C     Get expected intercept.
C
      CALL SURFPT ( VERTEX,    RAYDIR,    XRADII(1), 
     .              XRADII(2), XRADII(3), XXPT,       FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'SURFPT found', FOUND, .TRUE., OK )

C
C     The epoch is unused for the ellipsoid case.
C     
      ET = 0.D0

C
C     Now compute the intercept using the generalized callback.
C
      
      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .TRUE., OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYSFX: Ellipsoid non-intercept.' )

C
C     Set target parameters.
C
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set ray vertex and direction.
C
      CALL VPACK ( -5000.D0, 6000.D0, 10000.D0, VERTEX )
      
      CALL VEQU ( VERTEX, RAYDIR )

C
C     Get target radii from PCK file.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMIN = MIN ( XRADII(1), XRADII(2), XRADII(3) )


C
C     Now try to compute the intercept using the generalized callback.
C
      
      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .FALSE., OK )






C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYSFX: DSK intercept; use 2 surfaces.' )
 
C
C     Set target parameters.
C
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

C
C     Set ray vertex and direction.
C
      CALL VPACK ( -5000.D0, 6000.D0, 10000.D0, VERTEX )
      
      CALL VMINUS ( VERTEX, RAYDIR )


C
C     Set the target parameters.
C
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The epoch IS used for the DSK case; it's used to
C     select applicable segments.
C     
      ET = 0.D0

C
C     Compute the expected intercept.
C
      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  RAYDIR, XXPT,   FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DSKXV found', FOUND, .TRUE., OK )


C
C     Now compute the intercept using the generalized callback.
C
      
      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .TRUE., OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYSFX: DSK intercept; use an empty surface '
     .//           'list.'                                        )
 
C
C     Set target parameters.
C
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 0
      SRFLST(1) = 1
      SRFLST(2) = 2

C
C     Set the target parameters.
C
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The epoch IS used for the DSK case; it's used to
C     select applicable segments.
C     
      ET = 0.D0

C
C     Compute the expected intercept.
C
      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  RAYDIR, XXPT,   FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DSKXV found', FOUND, .TRUE., OK )


C
C     Now compute the intercept using the generalized callback.
C
      
      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .TRUE., OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )




C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYSFX: DSK intercept; use the second '
     .//           'surface only.'                            )
 
C
C     Set target parameters.
C
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2

C
C     Set the target parameters.
C
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The epoch IS used for the DSK case; it's used to
C     select applicable segments.
C     
      ET = 0.D0

C
C     Compute the expected intercept.
C
      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  RAYDIR, XXPT,   FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DSKXV found', FOUND, .TRUE., OK )


C
C     Now compute the intercept using the generalized callback.
C
      
      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .TRUE., OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYSFX: DSK non-intercept due to ray '
     .//           'direction; use 2 surfaces.' )
 
C
C     Set target parameters.
C
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

C
C     Set ray vertex and direction.
C
      CALL VPACK ( -5000.D0, 6000.D0, 10000.D0, VERTEX )
      
      CALL VEQU ( VERTEX, RAYDIR )


C
C     Set the target parameters.
C
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The epoch IS used for the DSK case; it's used to
C     select applicable segments.
C     
      ET = 0.D0
 
C
C     Now try to compute the intercept using the generalized callback.
C      
      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .FALSE., OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYSFX: DSK non-intercept due to '
     .//           'epoch; use 2 surfaces.' )
 
C
C     Set target parameters.
C
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

C
C     Set ray vertex and direction so that an intercept
C     will occur if the required surface data are 
C     available.
C
      CALL VPACK ( -5000.D0, 6000.D0, 10000.D0, VERTEX )
      
      CALL VMINUS ( VERTEX, RAYDIR )


C
C     Set the target parameters.
C
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The epoch IS used for the DSK case; it's used to
C     select applicable segments.
C     
      ET = -500 * JYEAR()
 
C
C     Now try to compute the intercept using the generalized callback.
C      
      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .FALSE., OK )




C**********************************************************************
C
C     ZZRAYNP tests
C
C**********************************************************************

C**********************************************************************
C
C     Normal cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYNP: find near point to ray on ellipsoid. '
     .//           'Non-intercept case.'                           )

C
C     Set target parameters.
C
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set ray vertex and direction.
C
      CALL VPACK ( -5000.D0,  6000.D0, 10000.D0, VERTEX )
      
      CALL VPACK (  5000.D0, -6000.D0,     0.D0, RAYDIR )

C
C     Get target radii from PCK file.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get expected near point.
C
      CALL NPEDLN ( XRADII(1), XRADII(2), XRADII(3),
     .              VERTEX,    RAYDIR,    XPNEAR,   XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     The epoch is unused for the ellipsoid case.
C     
      ET = 0.D0

C
C     Now compute the near point using the generalized callback.
C
      
      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYNP: find near point to ray on ellipsoid. '
     .//           'Intercept case.'                              )

C
C     Set target parameters.
C
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set ray vertex and direction.
C
      CALL VPACK ( -5000.D0,  6000.D0, 0.D0, VERTEX )
      
      CALL VPACK (  5000.D0, -6000.D0, 0.D0, RAYDIR )

C
C     Get target radii from PCK file.
C
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get expected near point.
C
      CALL NPEDLN ( XRADII(1), XRADII(2), XRADII(3),
     .              VERTEX,    RAYDIR,    XPNEAR,   XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     The epoch is unused for the ellipsoid case.
C     
      ET = 0.D0

C
C     Now compute the near point using the generalized callback.
C
      
      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

C
C     For the intercept cases, use an absolute error test for
C     distance, rather than a relative error test.
C
      CALL CHCKSD ( 'DIST',  DIST,  '~' ,  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


      
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYNP: DSK case: find near point to ray '
     .//           'on bounding ellipsoid. Use two surfaces. '
     .//           'Non-intercept case.'                      )

C
C     Set target parameters.
C
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2
 
C
C     Set the target parameters.
C
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set ray vertex and direction.
C
      CALL VPACK ( -5000.D0,  6000.D0,  0.D0, VERTEX )
      
      CALL VPACK (  5000.D0, -6000.D0,  0.D0, RAYDIR )


C
C     Get maximum bounding radius. 
C 
      CALL ZZMAXRAD ( XRADII(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get expected near point. Note that we have a spherical
C     bounding surface.
C
      CALL NPEDLN ( XRADII(1), XRADII(1), XRADII(1),
     .              VERTEX,    RAYDIR,    XPNEAR,   XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     The epoch is unused for the ellipsoid case.
C     
      ET = 0.D0

C
C     Now compute the near point using the generalized callback.
C
      
      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

 
      TOL = VTIGHT

      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYNP: DSK case: find near point to ray '
     .//           'on bounding ellipsoid. Use two surfaces, this ' 
     .//           'time with an empty surface list. Non-intercept '
     .//           'case.'                                          )

C
C     Set target parameters.
C
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 0
 
C
C     Set the target parameters.
C
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set ray vertex and direction.
C
      CALL VPACK ( -5000.D0,  6000.D0, 10000.D0, VERTEX )
      
      CALL VPACK (  5000.D0, -6000.D0,     0.D0, RAYDIR )


C
C     Get maximum bounding radius. 
C 
      CALL ZZMAXRAD ( XRADII(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get expected near point. Note that we have a spherical
C     bounding surface.
C
      CALL NPEDLN ( XRADII(1), XRADII(1), XRADII(1),
     .              VERTEX,    RAYDIR,    XPNEAR,    XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     The epoch is unused for the ellipsoid case.
C     
      ET = 0.D0

C
C     Now compute the near point using the generalized callback.
C
      
      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = VTIGHT

      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYNP: DSK case: find near point to ray '
     .//           'on bounding ellipsoid. Use the second '
     .//           'surface only. Non-intercept case.'          )

C
C     Set target parameters.
C
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2
 
C
C     Set the target parameters.
C
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set ray vertex and direction.
C
      CALL VPACK ( -5000.D0,  6000.D0, 10000.D0, VERTEX )
      
      CALL VPACK (  5000.D0, -6000.D0,     0.D0, RAYDIR )


C
C     Get maximum bounding radius. 
C 
      CALL ZZMAXRAD ( XRADII(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get expected near point. Note that we have a spherical
C     bounding surface.
C
      CALL NPEDLN ( XRADII(1), XRADII(1), XRADII(1),
     .              VERTEX,    RAYDIR,    XPNEAR,    XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The epoch is unused for the ellipsoid case.
C     
      ET = 0.D0

C
C     Now compute the near point using the generalized callback.
C
      
      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZRAYNP: DSK case: find near point to ray '
     .//           'on bounding ellipsoid. Use the second '
     .//           'surface only. Intercept case.'            )

C
C     Set target parameters.
C
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2
 
C
C     Set the target parameters.
C
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Set ray vertex and direction.
C
      CALL VPACK ( -5000.D0,  6000.D0, 0.D0, VERTEX )
      
      CALL VPACK (  5000.D0, -6000.D0, 0.D0, RAYDIR )


C
C     Get maximum bounding radius. 
C 
      CALL ZZMAXRAD ( XRADII(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Get expected near point. Note that we have a spherical
C     bounding surface.
C
      CALL NPEDLN ( XRADII(1), XRADII(1), XRADII(1),
     .              VERTEX,    RAYDIR,    XPNEAR,    XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The epoch is unused for the ellipsoid case.
C     
      ET = 0.D0

C
C     Now compute the near point using the generalized callback.
C
      
      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )

 


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Clean up.' )

      CALL DELFIL ( PCK0 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK0 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( DSK1 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


