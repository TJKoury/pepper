!$Procedure F_ZZSINUTL ( ZZSINUTL tests )

      SUBROUTINE F_ZZSINUTL ( OK )
      use pepper_inc_dsk

! $ Abstract
!
!     Exercise the entry points of routine ZZSINUTL.
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
!     This routine tests the entry points of the SPICELIB routine
!     ZZSINUTL:
!
!
!        ZZSUELIN:   Initialize ray intercept and near point utilities
!                    for ellipsoidal target surface
!
!
!        ZZSUDSKI:   Initialize ray intercept and near point utilities
!                    for DSK target surface
!
!        ZZMINRAD:   Return minimum spherical bounding radius for
!                    surface
!
!        ZZMAXRAD:   Return maximum spherical bounding radius for
!                    surface
!
!        ZZRAYSFX:   Compute ray-surface intercept given inputs
!                    set by ZZSUELIN or ZZSUDSKI
!
!
!        ZZRAYNP:    Compute nearest point on surface to ray given
!                    inputs set by ZZSUELIN or ZZSUDSKI
!
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
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 01-JUN-2016 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      JYEAR
      LOGICAL               EXISTS

!
!     Local Parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'zzsinutl_0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1   = 'zzsinutl_1.bds' )


      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zzsinutl.tpc' )


      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

!     DOUBLE PRECISION      TIGHT
!     PARAMETER           ( TIGHT  = 1.D-11 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

!
!     Local Variables
!
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(NAMLEN)    KVNAME
      CHARACTER*(NAMLEN)    TARGET
!      CHARACTER*(LNSIZE)    TITLE

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

!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZSINUTL' )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Set-up: create PCK.' )

      IF ( EXISTS(PCK0) ) THEN

         CALL DELFIL ( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


      CALL T_PCK08 ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
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


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Set-up: create scaled-down Mars DSK.' )

      IF ( EXISTS(DSK1) ) THEN

         CALL DELFIL ( DSK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )

      END IF


!
!     Get target radii from PCK file.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Temporarily replace radii in pool with scaled
!     radii.
!
      CALL VSCL ( 0.5D0, XRADII, RADII )

      KVNAME = 'BODY499_RADII'

      CALL PDPOOL ( KVNAME, 3, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
!     This file contains "surface 2" for Mars.
!
      SURFID = 2
      BODYID = 499
      FIXREF = 'IAU_MARS'

      NLON   = 40
      NLAT   = 20

      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Restore original Mars radii.
!
      CALL PDPOOL ( KVNAME, 3, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )



!**********************************************************************
!
!     ZZSINUTL error case
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZSINUTL error: direct call to ZZSINUTL.' )


      CALL ZZSINUTL ( BODYID, NSURF,  SRFLST, ET,    FIXFID, &
                      VERTEX, RAYDIR, SPOINT, FOUND, MINRAD, &
                      MAXRAD, PNEAR,  DIST                  )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )



!**********************************************************************
!
!     ZZMAXRAD / ZZSUDSKI / ZZSUELIN tests
!
!**********************************************************************

!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZMAXRAD: ellipsoid shape.' )

!
!     Set target parameters.
!
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get target radii from PCK file.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMAX = MAX ( XRADII(1), XRADII(2), XRADII(3) )

!
!     Get the max target radius.
!
      CALL ZZMAXRAD ( MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~', XMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZMAXRAD: DSK shape; use two surfaces.' )

!
!     Load DSKs.
!
      CALL FURNSH ( DSK0 )
      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set target parameters.
!
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get target radii from PCK file.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMAX = MAX ( XRADII(1), XRADII(2), XRADII(3) )

!
!     Get the max target radius. We expect surface 2 not to
!     contribute.
!
!     In principle, the maximum radius should match that of the
!     ellipsoid, since the tessellation, which uses an even number of
!     latitude bands, places some vertices on the target body's
!     equator. However, round-off error should be expected.
!
      CALL ZZMAXRAD ( MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAX, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZMAXRAD: DSK shape; use empty surface list.' )

!
!     Set target parameters.
!
      BODYID    = 499
      NSURF     = 0
      SRFLST(1) = 1
      SRFLST(2) = 2

      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get target radii from PCK file.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMAX = MAX ( XRADII(1), XRADII(2), XRADII(3) )

!
!     Get the max target radius. We expect surface 2 not to
!     contribute.
!
      CALL ZZMAXRAD ( MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAX, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZMAXRAD: DSK shape; use surface 2 only.' )

!
!     Set target parameters.
!
      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2


      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get target radii from PCK file.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMAX = 0.5D0 * MAX ( XRADII(1), XRADII(2), XRADII(3) )

!
!     Get the max target radius. We expect surface 2 not to
!     contribute.
!
      CALL ZZMAXRAD ( MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'MAXRAD', MAXRAD, '~/', XMAX, TOL, OK )


!**********************************************************************
!
!     ZZSUELIN error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZSUELIN: ellipsoid radii are not available.' )

!
!     Radii for the target are not loaded.
!
      BODYID = -777

      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )



!**********************************************************************
!
!     ZZSUDSKI error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZSUDSKI: surface count too large.' )

      NSURF = MAXSRF + 1

      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZSUDSKI: surface count too small.' )

      NSURF = -1

      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )




!**********************************************************************
!
!     ZZMINRAD / ZZSUDSKI / ZZSUELIN tests
!
!**********************************************************************

!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZMINRAD: ellipsoid shape.' )

!
!     Set target parameters.
!
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get target radii from PCK file.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMIN = MIN ( XRADII(1), XRADII(2), XRADII(3) )

!
!     Get the min target radius.
!
      CALL ZZMINRAD ( MINRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~', XMIN, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZMINRAD: DSK shape; use two surfaces.' )

!
!     The DSKs are already loaded.
!

!
!     Set target parameters.
!
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

!
!     Get the expected minimum radius via a brute force
!     computation which considers each plate.
!
      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, XMIN, MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set the target parameters.
!
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZMINRAD ( MINRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMIN, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZMINRAD: DSK shape; use empty surface list.' )

!
!     Set target parameters.
!
      BODYID    = 499
      NSURF     = 0
      SRFLST(1) = 1
      SRFLST(2) = 2

      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the expected minimum radius via a brute force
!     computation which considers each plate.
!
      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, XMIN, MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Get the min target radius. We expect surface 2 to
!     contribute this value.
!
      CALL ZZMINRAD ( MINRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMIN, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZMINRAD: DSK shape; use surface 2 only.' )

!
!     Set target parameters.
!
      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2


      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Get the expected minimum radius via a brute force
!     computation which considers each plate.
!
      CALL ZZDSKSPH ( BODYID, NSURF, SRFLST, XMIN, MAXRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get the min target radius. We expect surface 2 to
!     contribute this value.

      CALL ZZMINRAD ( MINRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'MINRAD', MINRAD, '~/', XMIN, TOL, OK )



!**********************************************************************
!
!     ZZRAYSFX tests
!
!**********************************************************************

!**********************************************************************
!
!     Normal cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYSFX: Ellipsoid intercept.' )

!
!     Set target parameters.
!
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set ray vertex and direction.
!
      CALL VPACK ( -5000.D0, 6000.D0, 10000.D0, VERTEX )

      CALL VMINUS ( VERTEX, RAYDIR )

!
!     Get target radii from PCK file.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMIN = MIN ( XRADII(1), XRADII(2), XRADII(3) )

!
!     Get expected intercept.
!
      CALL SURFPT ( VERTEX,    RAYDIR,    XRADII(1), &
                    XRADII(2), XRADII(3), XXPT,       FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'SURFPT found', FOUND, .TRUE., OK )

!
!     The epoch is unused for the ellipsoid case.
!
      ET = 0.D0

!
!     Now compute the intercept using the generalized callback.
!

      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .TRUE., OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYSFX: Ellipsoid non-intercept.' )

!
!     Set target parameters.
!
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set ray vertex and direction.
!
      CALL VPACK ( -5000.D0, 6000.D0, 10000.D0, VERTEX )

      CALL VEQU ( VERTEX, RAYDIR )

!
!     Get target radii from PCK file.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      XMIN = MIN ( XRADII(1), XRADII(2), XRADII(3) )


!
!     Now try to compute the intercept using the generalized callback.
!

      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .FALSE., OK )






!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYSFX: DSK intercept; use 2 surfaces.' )

!
!     Set target parameters.
!
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

!
!     Set ray vertex and direction.
!
      CALL VPACK ( -5000.D0, 6000.D0, 10000.D0, VERTEX )

      CALL VMINUS ( VERTEX, RAYDIR )


!
!     Set the target parameters.
!
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The epoch IS used for the DSK case; it's used to
!     select applicable segments.
!
      ET = 0.D0

!
!     Compute the expected intercept.
!
      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  RAYDIR, XXPT,   FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DSKXV found', FOUND, .TRUE., OK )


!
!     Now compute the intercept using the generalized callback.
!

      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .TRUE., OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYSFX: DSK intercept; use an empty surface ' &
      //           'list.'                                        )

!
!     Set target parameters.
!
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 0
      SRFLST(1) = 1
      SRFLST(2) = 2

!
!     Set the target parameters.
!
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The epoch IS used for the DSK case; it's used to
!     select applicable segments.
!
      ET = 0.D0

!
!     Compute the expected intercept.
!
      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  RAYDIR, XXPT,   FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DSKXV found', FOUND, .TRUE., OK )


!
!     Now compute the intercept using the generalized callback.
!

      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .TRUE., OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYSFX: DSK intercept; use the second ' &
      //           'surface only.'                            )

!
!     Set target parameters.
!
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2

!
!     Set the target parameters.
!
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The epoch IS used for the DSK case; it's used to
!     select applicable segments.
!
      ET = 0.D0

!
!     Compute the expected intercept.
!
      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  RAYDIR, XXPT,   FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'DSKXV found', FOUND, .TRUE., OK )


!
!     Now compute the intercept using the generalized callback.
!

      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .TRUE., OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'XPT', XPT, '~~/', XXPT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYSFX: DSK non-intercept due to ray ' &
      //           'direction; use 2 surfaces.' )

!
!     Set target parameters.
!
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

!
!     Set ray vertex and direction.
!
      CALL VPACK ( -5000.D0, 6000.D0, 10000.D0, VERTEX )

      CALL VEQU ( VERTEX, RAYDIR )


!
!     Set the target parameters.
!
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The epoch IS used for the DSK case; it's used to
!     select applicable segments.
!
      ET = 0.D0

!
!     Now try to compute the intercept using the generalized callback.
!
      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .FALSE., OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYSFX: DSK non-intercept due to ' &
      //           'epoch; use 2 surfaces.' )

!
!     Set target parameters.
!
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

!
!     Set ray vertex and direction so that an intercept
!     will occur if the required surface data are
!     available.
!
      CALL VPACK ( -5000.D0, 6000.D0, 10000.D0, VERTEX )

      CALL VMINUS ( VERTEX, RAYDIR )


!
!     Set the target parameters.
!
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The epoch IS used for the DSK case; it's used to
!     select applicable segments.
!
      ET = -500 * JYEAR()

!
!     Now try to compute the intercept using the generalized callback.
!
      CALL ZZRAYSFX ( VERTEX, RAYDIR, ET, XPT, FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'ZZRAYSFX found', FOUND, .FALSE., OK )




!**********************************************************************
!
!     ZZRAYNP tests
!
!**********************************************************************

!**********************************************************************
!
!     Normal cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYNP: find near point to ray on ellipsoid. ' &
      //           'Non-intercept case.'                           )

!
!     Set target parameters.
!
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set ray vertex and direction.
!
      CALL VPACK ( -5000.D0,  6000.D0, 10000.D0, VERTEX )

      CALL VPACK (  5000.D0, -6000.D0,     0.D0, RAYDIR )

!
!     Get target radii from PCK file.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get expected near point.
!
      CALL NPEDLN ( XRADII(1), XRADII(2), XRADII(3), &
                    VERTEX,    RAYDIR,    XPNEAR,   XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The epoch is unused for the ellipsoid case.
!
      ET = 0.D0

!
!     Now compute the near point using the generalized callback.
!

      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYNP: find near point to ray on ellipsoid. ' &
      //           'Intercept case.'                              )

!
!     Set target parameters.
!
      BODYID = 499
      CALL ZZSUELIN ( BODYID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set ray vertex and direction.
!
      CALL VPACK ( -5000.D0,  6000.D0, 0.D0, VERTEX )

      CALL VPACK (  5000.D0, -6000.D0, 0.D0, RAYDIR )

!
!     Get target radii from PCK file.
!
      CALL BODVCD ( BODYID, 'RADII', 3, N, XRADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get expected near point.
!
      CALL NPEDLN ( XRADII(1), XRADII(2), XRADII(3), &
                    VERTEX,    RAYDIR,    XPNEAR,   XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The epoch is unused for the ellipsoid case.
!
      ET = 0.D0

!
!     Now compute the near point using the generalized callback.
!

      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

!
!     For the intercept cases, use an absolute error test for
!     distance, rather than a relative error test.
!
      CALL CHCKSD ( 'DIST',  DIST,  '~' ,  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYNP: DSK case: find near point to ray ' &
      //           'on bounding ellipsoid. Use two surfaces. ' &
      //           'Non-intercept case.'                      )

!
!     Set target parameters.
!
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

!
!     Set the target parameters.
!
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set ray vertex and direction.
!
      CALL VPACK ( -5000.D0,  6000.D0,  0.D0, VERTEX )

      CALL VPACK (  5000.D0, -6000.D0,  0.D0, RAYDIR )


!
!     Get maximum bounding radius.
!
      CALL ZZMAXRAD ( XRADII(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get expected near point. Note that we have a spherical
!     bounding surface.
!
      CALL NPEDLN ( XRADII(1), XRADII(1), XRADII(1), &
                    VERTEX,    RAYDIR,    XPNEAR,   XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The epoch is unused for the ellipsoid case.
!
      ET = 0.D0

!
!     Now compute the near point using the generalized callback.
!

      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = VTIGHT

      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYNP: DSK case: find near point to ray ' &
      //           'on bounding ellipsoid. Use two surfaces, this ' &
      //           'time with an empty surface list. Non-intercept ' &
      //           'case.'                                          )

!
!     Set target parameters.
!
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 0

!
!     Set the target parameters.
!
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set ray vertex and direction.
!
      CALL VPACK ( -5000.D0,  6000.D0, 10000.D0, VERTEX )

      CALL VPACK (  5000.D0, -6000.D0,     0.D0, RAYDIR )


!
!     Get maximum bounding radius.
!
      CALL ZZMAXRAD ( XRADII(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get expected near point. Note that we have a spherical
!     bounding surface.
!
      CALL NPEDLN ( XRADII(1), XRADII(1), XRADII(1), &
                    VERTEX,    RAYDIR,    XPNEAR,    XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     The epoch is unused for the ellipsoid case.
!
      ET = 0.D0

!
!     Now compute the near point using the generalized callback.
!

      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )


      TOL = VTIGHT

      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYNP: DSK case: find near point to ray ' &
      //           'on bounding ellipsoid. Use the second ' &
      //           'surface only. Non-intercept case.'          )

!
!     Set target parameters.
!
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2

!
!     Set the target parameters.
!
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set ray vertex and direction.
!
      CALL VPACK ( -5000.D0,  6000.D0, 10000.D0, VERTEX )

      CALL VPACK (  5000.D0, -6000.D0,     0.D0, RAYDIR )


!
!     Get maximum bounding radius.
!
      CALL ZZMAXRAD ( XRADII(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get expected near point. Note that we have a spherical
!     bounding surface.
!
      CALL NPEDLN ( XRADII(1), XRADII(1), XRADII(1), &
                    VERTEX,    RAYDIR,    XPNEAR,    XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The epoch is unused for the ellipsoid case.
!
      ET = 0.D0

!
!     Now compute the near point using the generalized callback.
!

      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'DIST',  DIST,  '~/',  XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZRAYNP: DSK case: find near point to ray ' &
      //           'on bounding ellipsoid. Use the second ' &
      //           'surface only. Intercept case.'            )

!
!     Set target parameters.
!
      TARGET    = 'MARS'
      FIXREF    = 'IAU_MARS'
      BODYID    = 499
      NSURF     = 1
      SRFLST(1) = 2

!
!     Set the target parameters.
!
      CALL ZZSUDSKI ( BODYID, NSURF, SRFLST, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Set ray vertex and direction.
!
      CALL VPACK ( -5000.D0,  6000.D0, 0.D0, VERTEX )

      CALL VPACK (  5000.D0, -6000.D0, 0.D0, RAYDIR )


!
!     Get maximum bounding radius.
!
      CALL ZZMAXRAD ( XRADII(1) )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Get expected near point. Note that we have a spherical
!     bounding surface.
!
      CALL NPEDLN ( XRADII(1), XRADII(1), XRADII(1), &
                    VERTEX,    RAYDIR,    XPNEAR,    XDIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The epoch is unused for the ellipsoid case.
!
      ET = 0.D0

!
!     Now compute the near point using the generalized callback.
!

      CALL ZZRAYNP ( VERTEX, RAYDIR, ET, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'DIST',  DIST,  '~',   XDIST,     TOL, OK )
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', XPNEAR, 3, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
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


!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


