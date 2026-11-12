!$Procedure F_ZZTANUTL ( ZZTANUTL/ZZTANGNT tests )

      SUBROUTINE F_ZZTANUTL ( OK )
      use pepper_legacy
      use pepper_inc_zzdsk

! $ Abstract
!
!     Exercise the entry points of routine ZZTANUTL and the
!     separate routine ZZTANGNT.
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
!
!     PEPPER: the size of this test's surface lists (SPICELIB's MAXSRF of
!     dsk.inc; the library's surface lists have no limit).
!
      INTEGER               MAXSRF
      PARAMETER           ( MAXSRF = 100 )


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
!     ZZTANUTL:
!
!
!        ZZTANINI:   Initialize tangent ray utilities for limb or
!                    terminator computation. Terminators may be
!                    umbral or penumbral.
!
!        ZZTANSTA:   Compute state of ray for a given input angle.
!                    State is "occulted" or "not occulted." When
!                    the state is occulted, return the ray-surface
!                    intercept point.
!
!     This routine also tests the related routine
!
!        ZZTANGNT
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
!     E.D. Wright      (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 23-AUG-2016 (NJB) (EDW)
!
!-&

!
!     SPICELIB functions
!



!
!     Local Parameters
!
      CHARACTER*(*)         DSK0
      PARAMETER           ( DSK0   = 'zztanutl_0.bds' )

      CHARACTER*(*)         DSK1
      PARAMETER           ( DSK1   = 'zztanutl_1.bds' )

      CHARACTER*(*)         DSK2
      PARAMETER           ( DSK2   = 'zztanutl_2.bds' )

      CHARACTER*(*)         DSK3
      PARAMETER           ( DSK3   = 'zztanutl_3.bds' )


      CHARACTER*(*)         DSK4
      PARAMETER           ( DSK4  = 'zztanutl_4.bds' )


      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0   = 'zztanutl.tpc' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1   = 'nat.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1   = 'nat.bsp' )


      DOUBLE PRECISION      MED
      PARAMETER           ( MED    = 1.D-7 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-10 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-11 )


      INTEGER               ALPHA
      PARAMETER           ( ALPHA  = 1000 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               NMAP
      PARAMETER           ( NMAP   = 4 )

      INTEGER               UBEL
      PARAMETER           ( UBEL   = 9 )

      INTEGER               UBPL
      PARAMETER           ( UBPL   = 4 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 2000 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               NCROSS
      PARAMETER           ( NCROSS = 400 )

      INTEGER               NPOLYV
      PARAMETER           ( NPOLYV = 100 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 240 )

!
!     Local Variables
!
      CHARACTER*(NAMLEN)    FIXREF
      CHARACTER*(NAMLEN)    LABEL
      CHARACTER*(NAMLEN)    SRFNMS ( NMAP )
      CHARACTER*(NAMLEN)    TARGET
      CHARACTER*(LNSIZE)    TITLE

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ANGLE
      DOUBLE PRECISION      AXIS   ( 3 )
      DOUBLE PRECISION      B
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CDIR
      DOUBLE PRECISION      CENTER ( 3 )
      DOUBLE PRECISION      CUT    ( UBPL )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DP
      DOUBLE PRECISION      DIR    ( 3 )
      DOUBLE PRECISION      DIST
      DOUBLE PRECISION      DTHETA
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      LIMB   ( UBEL )
      DOUBLE PRECISION      LPOINT ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      NPT    ( 3 )
      DOUBLE PRECISION      OFFSET ( 3 )
      DOUBLE PRECISION      ORIGIN ( 3 )
      DOUBLE PRECISION      POINT  ( 3 )
      DOUBLE PRECISION      POINTS ( 3, MAXWIN )
      DOUBLE PRECISION      PLNVEC ( 3 )
      DOUBLE PRECISION      PNEAR  ( 3 )
      DOUBLE PRECISION      PROJ   ( 3 )
      DOUBLE PRECISION      R
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RAYDIR ( 3 )
      DOUBLE PRECISION      REFVEC ( 3 )
      DOUBLE PRECISION      RCROSS
      DOUBLE PRECISION      RESULT ( LBCELL : MAXWIN )
      DOUBLE PRECISION      RPOINT ( 3 )
      DOUBLE PRECISION      SCHSTP
      DOUBLE PRECISION      SDIR
      DOUBLE PRECISION      SOLTOL
      DOUBLE PRECISION      SRCRAD
      DOUBLE PRECISION      SUNLPT ( 3 )
      DOUBLE PRECISION      SUNRAD ( 3 )
      DOUBLE PRECISION      TANPNT ( 3 )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      VERTEX ( 3 )
      DOUBLE PRECISION      VTEMP  ( 3 )
      DOUBLE PRECISION      XVEC   ( 3 )
      DOUBLE PRECISION      XPT1   ( 3 )
      DOUBLE PRECISION      XPT2   ( 3 )
      DOUBLE PRECISION      YVEC   ( 3 )
      DOUBLE PRECISION      ZVEC   ( 3 )

      INTEGER               BODYID
      INTEGER               CURVE
      INTEGER               FIXFID
      INTEGER               HANDLE
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               NCUTS
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               NSURF
      INTEGER               NXPTS
      INTEGER               SHAPE
      INTEGER               SRFBOD ( NMAP )
      INTEGER               SRFIDS ( NMAP )
      INTEGER               SRFLST ( MAXSRF )
      INTEGER               SURFID
      INTEGER               TRGCDE

      LOGICAL               FOUND
      LOGICAL FOUND_A1 ( 1 )
      EQUIVALENCE ( FOUND, FOUND_A1 )
      LOGICAL               OCULTD


!
!     Saved values
!
!     Save variables in order to avoid stack room problems.
!
      SAVE

!
!     Initial values
!
      DATA                  ORIGIN / 3 * 0.D0 /

!
!     Open the test family.
!
      CALL TOPEN ( 'F_ZZTANUTL' )




!**********************************************************************
!
!     Setup for ZZTANINI/ZZTANSTA tests
!
!**********************************************************************

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
      CALL TCASE ( 'Setup: create DSK files.' )

!
!     For Mars, surface 1 is the "main" surface.
!
      TARGET = 'MARS'
      TRGCDE = 499
      FIXREF = 'IAU_MARS'

      BODYID = TRGCDE
      SURFID = 1
      NLON   =  220
      NLAT   =  110

      IF ( EXISTS(DSK0) ) THEN
         CALL DELFIL( DSK0 )
      END IF

      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Load main Mars DSK.
!
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Surface 2 for Mars has slightly lower resolution.
!
      BODYID = TRGCDE
      SURFID = 2
      NLON   = 190
      NLAT   =  95

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
      END IF

!
!     Create and load the second DSK.
!
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Surface 1 for Phobos is high-res.
!
      BODYID = 401
      SURFID = 1
      FIXREF = 'IAU_PHOBOS'

      NLON   = 200
      NLAT   = 100

      IF ( EXISTS(DSK2) ) THEN
         CALL DELFIL( DSK2 )
      END IF

!
!     Create and load the first Phobos DSK.
!
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Surface 2 for Phobos is lower-res.
!
      BODYID = 401
      SURFID = 2
      FIXREF = 'IAU_PHOBOS'

      NLON   =  80
      NLAT   =  40

      IF ( EXISTS(DSK3) ) THEN
         CALL DELFIL( DSK3 )
      END IF

!
!     Create and load the second Phobos DSK.
!
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Set up a surface name-ID map.
!
      SRFBOD(1) = 499
      SRFIDS(1) = 1
      SRFNMS(1) = 'high-res'

      SRFBOD(2) = 499
      SRFIDS(2) = 2
      SRFNMS(2) = 'low-res'

      SRFBOD(3) = 401
      SRFIDS(3) = 1
      SRFNMS(3) = 'high-res'

      SRFBOD(4) = 401
      SRFIDS(4) = 2
      SRFNMS(4) = 'low-res'



      CALL PCPOOL ( 'NAIF_SURFACE_NAME', NMAP, SRFNMS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_SURFACE_CODE', NMAP, SRFIDS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_SURFACE_BODY', NMAP, SRFBOD )
      CALL CHCKXC ( .FALSE., ' ', OK )


!**********************************************************************
!
!     ZZTANUTL Error cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Call ZZTANUTL directly.' )

      CALL ZZTANUTL ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS, &
                      ANGLE,  OCULTD, POINT                )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )



!**********************************************************************
!
!     ZZTANINI Error cases
!
!**********************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANINI: Zero axis vector.' )


      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL CLEARD ( 3, AXIS )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANINI: Zero half-plane reference vector.' )


      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL CLEARD ( 3, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANINI: invalid curve type.' )


      CURVE     = -1
      SRCRAD    = 0.D0
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(BADCURVETYPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANINI: invalid source radius' )


      CURVE     = UMBRAL
      SRCRAD    = 0.D0
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSOURCERADIUS)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANINI: axis and reference vector ' &
      //           'are linearly dependent.'             )


      CURVE     = UMBRAL
      SRCRAD    = 696000.D0
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )

      CALL VSCL ( 2.D0, AXIS, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANINI: invalid target shape.' )


      CURVE     = UMBRAL
      SRCRAD    = 696000.D0
      SHAPE     = -1
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSHAPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANINI: missing PCK data.' )



      CURVE     = UMBRAL
      SRCRAD    = 696000.D0
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL CLPOOL

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!**********************************************************************
!
!     ZZTANSTA Error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANSTA: no DSKs loaded.' )


      CURVE     = UMBRAL
      SRCRAD    = 696000.D0
      SHAPE     = DSKSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL UNLOAD ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL ZZTANSTA ( 0.D0, OCULTD, POINT )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANSTA: no DSK data for target' )


      CURVE     = UMBRAL
      SRCRAD    = 696000.D0
      SHAPE     = DSKSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )



      CALL UNLOAD ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )





!**********************************************************************
!
!     ZZTANGNT Error cases
!
!**********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT: Zero axis vector.' )


      CURVE     = UMBRAL
      SRCRAD    = 696000.D0
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL CLEARD ( 3, AXIS )

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT: Zero plane reference vector.' )

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL CLEARD ( 3, PLNVEC )

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT: Bad curve type.' )

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15


      CURVE = -1

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADCURVETYPE)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT: Bad source radius.' )

      CURVE     = UMBRAL
      SRCRAD    = 0.D0
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15


      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSOURCERADIUS)', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT: Axis and plane reference vector ' &
      //           'are linearly dependent.'                   )

      CURVE     = UMBRAL
      SRCRAD    = 696000.D0
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )

      CALL VSCL ( 2.D0, AXIS, PLNVEC )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15


      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT: Bad target shape.' )

      CURVE     = UMBRAL
      SRCRAD    = 0.D0
      SHAPE     = -1
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15


      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSOURCERADIUS)', OK )



!**********************************************************************
!
!     ZZTANINI/ZZTANSTA normal cases
!
!**********************************************************************

!
!     These cases are combined because the two entry points
!     work in a cooperative fashion, and because the results of
!     initialization alone are not observable.
!


!**********************************************************************
!
!     ELLIPSOID target shape
!
!**********************************************************************

!
!     LIMB cases
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize limb computation using ' &
      //           'ellipsoidal target shape. (Mars)'    )


      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near limb point in cutting ' &
      //           'half-plane of previous case. Non-occultation ' &
      //           'case. (Mars)'                                 )

!
!     Find limb point in cutting half-plane of previous case.
!
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


      A = RADII(1)
      B = RADII(2)
      C = RADII(3)

      CALL EDLIMB ( A, B, C, AXIS, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )


      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, LPOINT )
      ELSE
         CALL VEQU ( XPT2, LPOINT )
      END IF

!
!     Generate off-ellipsoid point RPOINT. RPOINT is 1 mm above the
!     surface.
!
      CALL VLCOM ( 1.D0, LPOINT, 1.D-6, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near limb point in cutting ' &
      //           'half-plane of previous case. Occultation ' &
      //           'case. (Mars)'                                 )
!
!     Generate off-ellipsoid point RPOINT. RPOINT is 1 mm below the
!     surface.
!
      CALL VLCOM ( 1.D0, LPOINT, -1.D-6, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be on the ellipsoid. Map the
!     point to a point that is on the ellipsoid and check the
!     distance between the two.
!
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT test: create limb point matching that ' &
      //           'from the previous case. (Mars)'                 )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Set absolute tolerance to 1 m. (Can use 10 cm on PC/Linux).
!
      CALL VEQU ( POINTS, TANPNT )

      TOL = 1.D-3
      CALL CHCKAD ( 'TANPNT', TANPNT, '~~', LPOINT, 3, TOL, OK )

!
!     The returned point should be on the ellipsoid. Map the
!     point to a point that is on the ellipsoid and check the
!     distance between the two.
!
      CALL EDPNT ( TANPNT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'TANPNT', TANPNT, '~~', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( TANPNT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( AXIS, RAYDIR, TANPNT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', TANPNT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( TANPNT, AXIS, OFFSET )

      DP =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Repeat the test with a different ellipsoidal ' &
      //           'target. Initialize the system for Phobos.'   )


      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = ELLSHP
      TRGCDE    = 401
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near limb point in cutting ' &
      //           'half-plane of previous case. Non-occultation ' &
      //           'case. (Phobos)'                               )

!
!     Find limb point in cutting half-plane of previous case.
!
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


      A = RADII(1)
      B = RADII(2)
      C = RADII(3)

      CALL EDLIMB ( A, B, C, AXIS, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( VDOT(XPT1, PLNVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, LPOINT )
      ELSE
         CALL VEQU ( XPT2, LPOINT )
      END IF

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-ellipsoid point.
!
      CALL VLCOM ( 1.D0, LPOINT, 1.D-6, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near limb point in cutting ' &
      //           'half-plane of previous case. Occultation ' &
      //           'case. (Phobos)'                              )

      CALL VLCOM ( 1.D0, LPOINT, -1.D-6, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be on the ellipsoid. Map the
!     point to a point that is on the ellipsoid and check the
!     distance between the two.
!
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT test: create limb point matching that ' &
      //           'from the previous case. (Phobos)'              )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

!
!     Set absolute tolerance to 1 m. (Can use 10 cm on PC/Linux).
!
      CALL VEQU ( POINTS, TANPNT )

      TOL = 1.D-3
      CALL CHCKAD ( 'TANPNT', TANPNT, '~~', LPOINT, 3, TOL, OK )

!
!     The returned point should be on the ellipsoid. Map the
!     point to a point that is on the ellipsoid and check the
!     distance between the two.
!
      CALL EDPNT ( TANPNT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'TANPNT', TANPNT, '~~', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( TANPNT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( AXIS, RAYDIR, TANPNT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-6
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', TANPNT, 3, TOL, OK )


!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( TANPNT, AXIS, OFFSET )

      DP =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )


!
!     UMBRAL TERMINATOR cases
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize umbral terminator computation using ' &
      //           'ellipsoidal target shape. (Mars)'              )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = UMBRAL
      SRCRAD    = SUNRAD(1)
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near umbral terminator point ' &
      //           'in cutting half-plane of previous case. ' &
      //           'Non-occultation case. (Mars)'               )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution
!     point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-ellipsoid point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, LPOINT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )


      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near umbral terminator point ' &
      //           'in cutting half-plane of previous case. ' &
      //           'Occultation case. (Mars)' )

!
!     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, LPOINT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )


      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be on the ellipsoid. Map the
!     point to a point that is on the ellipsoid and check the
!     distance between the two.
!
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )


!
!
!     Repeat previous tests, this time using Phobos as the target.
!
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize umbral terminator computation using ' &
      //           'ellipsoidal target shape. (Phobos)'             )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = UMBRAL
      SRCRAD    = SUNRAD(1)
      SHAPE     = ELLSHP
      TRGCDE    = 401
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near umbral terminator point ' &
      //           'in cutting half-plane of previous case. ' &
      //           'Non-occultation case. (Phobos)'               )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution
!     point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-ellipsoid point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, LPOINT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )


      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near umbral terminator point ' &
      //           'in cutting half-plane of previous case. ' &
      //           'Occultation case. (Phobos)' )

!
!     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, LPOINT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )


      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be on the ellipsoid. Map the
!     point to a point that is on the ellipsoid and check the
!     distance between the two.
!
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )




!
!     PENUMBRAL TERMINATOR cases
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize penumbral terminator computation using ' &
      //           'ellipsoidal target shape. (Mars)'              )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = PNMBRL
      SRCRAD    = SUNRAD(1)
      SHAPE     = ELLSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near penumbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Non-occultation case. (Mars)'               )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-ellipsoid point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, LPOINT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )

!
!     For the penumbral case, the sun limb point should be
!     on the -Y side of the axis.
!
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the axis bounding the
!        half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near penumbral terminator  ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Occultation case. (Mars)' )

!
!     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, LPOINT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )


      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
!     For the penumbral case, the sun limb point should be
!     on the -Y side of the axis.
!
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the axis bounding the
!        half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be on the ellipsoid. Map the
!     point to a point that is on the ellipsoid and check the
!     distance between the two.
!
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )



!
!
!     Repeat previous tests, this time using Phobos as the target.
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize penumbral terminator computation using ' &
      //           'ellipsoidal target shape. (Phobos)'              )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = PNMBRL
      SRCRAD    = SUNRAD(1)
      SHAPE     = ELLSHP
      TRGCDE    = 401
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near penumbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Non-occultation case. (Phobos)'               )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-ellipsoid point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, LPOINT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )

!
!     For the penumbral case, the sun limb point should be
!     on the -Y side of the axis.
!
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the axis bounding the
!        half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near penumbral terminator  ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Occultation case. (Phobos)' )

!
!     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, LPOINT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )


      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
!     For the penumbral case, the sun limb point should be
!     on the -Y side of the axis.
!
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the axis bounding the
!        half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the ellipsoid.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be on the ellipsoid. Map the
!     point to a point that is on the ellipsoid and check the
!     distance between the two.
!
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )







!**********************************************************************
!
!     DSK target shape
!
!**********************************************************************




!
!     LIMB cases
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize limb computation using ' &
      //           'DSK target shape. Empty surface list. (Mars)' )

!
!     Load DSKs.
!
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!     Use all surfaces (empty surface list).
!

      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = DSKSHP
      TRGCDE    = 499
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting ' &
      //           'half-plane of previous case. Non-occultation ' &
      //           'case. (Mars)'                                 )

!
!     Find limb point in cutting half-plane of previous case.
!


      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm above the
!     surface.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting ' &
      //           'half-plane of previous case. Occultation ' &
      //           'case. (Mars)'                                 )
!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
!     surface.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize limb computation using ' &
      //           'DSK target shape. Specify both surfaces ' &
      //           'in surface list. (Mars)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = DSKSHP
      TRGCDE    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT test: compare DSK limb point to that ' &
      //           'found in previous case. (Mars)'                   )

!
!     Find limb point in cutting half-plane of previous case.
!


      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )
!
!     We expect an exact match.
!
      TOL = 0.D0

      CALL CHCKAD ( 'LPOINT', LPOINT, '~~/', TANPNT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize limb computation using ' &
      //           'DSK target shape. Use surface 2. (Mars)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = DSKSHP
      TRGCDE    = 499
      NSURF     = 1
      SRFLST(1) = 2

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting ' &
      //           'half-plane of previous case. Non-occultation ' &
      //           'case. Use surface 2. (Mars)'                    )

!
!     Find limb point in cutting half-plane of previous case.
!


      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm above the
!     surface.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting ' &
      //           'half-plane of previous case. Occultation ' &
      //           'case.  Use surface 2. (Mars)'                      )
!
!     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
!     surface.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )


!
!
!     Use Phobos as the target.
!
!
!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize limb computation using ' &
      //           'DSK target shape. Empty surface list. (Phobos)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = DSKSHP
      TRGCDE    = 401
      TARGET    = 'PHOBOS'
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting ' &
      //           'half-plane of previous case. Non-occultation ' &
      //           'case. (Phobos)'                                 )

!
!     Find limb point in cutting half-plane of previous case.
!


      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm above the
!     surface.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting ' &
      //           'half-plane of previous case. Occultation ' &
      //           'case. (Phobos)'                                 )
!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
!     surface.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize limb computation using ' &
      //           'DSK target shape. Specify both surfaces ' &
      //           'in surface list. (Phobos)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = DSKSHP
      TRGCDE    = 401
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT test: compare DSK limb point to that ' &
      //           'found in previous case. (Phobos)'             )

!
!     Find limb point in cutting half-plane of previous case.
!


      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )
!
!     We expect an exact match.
!
      TOL = 0.D0

      CALL CHCKAD ( 'LPOINT', LPOINT, '~~/', TANPNT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize limb computation using ' &
      //           'DSK target shape. Use surface 2. (Phobos)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = DSKSHP
      TRGCDE    = 401
      TARGET    = 'PHOBOS'
      NSURF     = 1
      SRFLST(1) = 2

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D4

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting ' &
      //           'half-plane of previous case. Non-occultation ' &
      //           'case. Use surface 2. (Phobos)'                    )

!
!     Find limb point in cutting half-plane of previous case.
!


      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm above the
!     surface.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting ' &
      //           'half-plane of previous case. Occultation ' &
      //           'case.  Use surface 2. (Phobos)'                   )
!
!     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
!     surface.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )






!
!     DSK UMBRAL TERMINATOR cases
!

!
!
!     Mars is the target.
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize umbral terminator computation using ' &
      //           'DSK target shape. Use empty surface list. (Mars)' )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = UMBRAL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TRGCDE    = 499
      TARGET    = 'MARS'
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK umbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Non-occultation case. Empty surface list. (Mars)' )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution
!     point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-DSK surface point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )


      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK umbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Occultation case. Empty surface list. (Mars)' )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

!
!     Find limb on the source (sun) as seen from the tangent point.
!
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize umbral terminator computation using ' &
      //           'DSK target shape. Specify both surfaces ' &
      //           'in surface list. (Mars)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = UMBRAL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TARGET    = 'MARS'
      TRGCDE    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT test: compare DSK umbral terminator ' &
      //           'point to that found in previous case. ' &
      //           '(Mars)' )

!
!     Find limb point in cutting half-plane of previous case.
!


      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, POINT )
!
!     We expect an exact match.
!
      TOL = 0.D0

      CALL CHCKAD ( 'POINT', POINT, '~~/', TANPNT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize umbral terminator computation using ' &
      //           'DSK target shape. Use surface 2. (Mars)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = UMBRAL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TRGCDE    = 499
      NSURF     = 1
      SRFLST(1) = 2

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK umbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Non-occultation case. Use surface 2. (Mars)' )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution
!     point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-DSK surface point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )


      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK umbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Occultation case. Use surface 2. (Mars)' )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

!
!     Find limb on the source (sun) as seen from the tangent point.
!
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )





!
!
!     Phobos is the target.
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize umbral terminator computation using ' &
      //           'DSK target shape. Use empty surface list. ' &
      //           '(Phobos)' )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = UMBRAL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TRGCDE    = 401
      TARGET    = 'PHOBOS'
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK umbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Non-occultation case. Empty surface list. ' &
      //           '(Phobos)' )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution
!     point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-DSK surface point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )


      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK umbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Occultation case. Empty surface list. (Phobos)' )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

!
!     Find limb on the source (sun) as seen from the tangent point.
!
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize umbral terminator computation using ' &
      //           'DSK target shape. Specify both surfaces ' &
      //           'in surface list. (Phobos)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = UMBRAL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TARGET    = 'PHOBOS'
      TRGCDE    = 401
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT test: compare DSK umbral terminator ' &
      //           'point to that found in previous case. ' &
      //           '(Phobos)' )

!
!     Find limb point in cutting half-plane of previous case.
!


      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, POINT )
!
!     We expect an exact match.
!
      TOL = 0.D0

      CALL CHCKAD ( 'POINT', POINT, '~~/', TANPNT, 3, TOL, OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize umbral terminator computation using ' &
      //           'DSK target shape. Use surface 2. (Phobos)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = UMBRAL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TRGCDE    = 401
      NSURF     = 1
      SRFLST(1) = 2

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK umbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Non-occultation case. Use surface 2. (Phobos)' )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution
!     point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-DSK surface point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )


      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK umbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Occultation case. Use surface 2. (Phobos)' )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

!
!     Find limb on the source (sun) as seen from the tangent point.
!
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!        XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )





!
!
!     DSK PENUMBRAL TERMINATOR cases
!

!
!
!     Mars is the target.
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize penumbral terminator computation using ' &
      //           'DSK target shape. Use empty surface list. (Mars)' )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = PNMBRL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TRGCDE    = 499
      TARGET    = 'MARS'
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Non-occultation case. Empty surface list. (Mars)' )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution
!     point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-DSK surface point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )


      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the half-plane defined by AXIS
!        and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Occultation case. Empty surface list. (Mars)' )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

!
!     Find limb on the source (sun) as seen from the tangent point.
!
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the half-plane defined by AXIS
!        and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize penumbral terminator computation using ' &
      //           'DSK target shape. Specify both surfaces ' &
      //           'in surface list. (Mars)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = PNMBRL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TARGET    = 'MARS'
      TRGCDE    = 499
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT test: compare DSK penumbral terminator ' &
      //           'point to that found in previous case. ' &
      //           '(Mars)' )

!
!     Find limb point in cutting half-plane of previous case.
!


      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, POINT )
!
!     We expect an exact match.
!
      TOL = 0.D0

      CALL CHCKAD ( 'POINT', POINT, '~~/', TANPNT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize penumbral terminator computation using ' &
      //           'DSK target shape. Use surface 2. (Mars)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = PNMBRL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TRGCDE    = 499
      NSURF     = 1
      SRFLST(1) = 2

      FIXREF    = 'IAU_MARS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Non-occultation case. Use surface 2. (Mars)' )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution
!     point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-DSK surface point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )

      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the half-plane defined by AXIS
!        and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Occultation case. Use surface 2. (Mars)' )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

!
!     Find limb on the source (sun) as seen from the tangent point.
!
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the half-plane defined by AXIS
!        and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )


!
!
!     Phobos is the target.
!
!

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize penumbral terminator computation using ' &
      //           'DSK target shape. Use empty surface list. ' &
      //           '(Phobos)' )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = PNMBRL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TRGCDE    = 401
      TARGET    = 'PHOBOS'
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )



!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Non-occultation case. Empty surface list. ' &
      //           '(Phobos)' )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution
!     point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-DSK surface point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )


      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the half-plane defined by AXIS
!        and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )




!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Occultation case. Empty surface list. (Phobos)' )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

!
!     Find limb on the source (sun) as seen from the tangent point.
!
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the half-plane defined by AXIS
!        and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize penumbral terminator computation using ' &
      //           'DSK target shape. Specify both surfaces ' &
      //           'in surface list. (Phobos)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = PNMBRL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TARGET    = 'PHOBOS'
      TRGCDE    = 401
      NSURF     = 2
      SRFLST(1) = 1
      SRFLST(2) = 2

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'ZZTANGNT test: compare DSK penumbral terminator ' &
      //           'point to that found in previous case. ' &
      //           '(Phobos)' )

!
!     Find limb point in cutting half-plane of previous case.
!


      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, POINT )
!
!     We expect an exact match.
!
      TOL = 0.D0

      CALL CHCKAD ( 'POINT', POINT, '~~/', TANPNT, 3, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Initialize penumbral terminator computation using ' &
      //           'DSK target shape. Use surface 2. (Phobos)' )

!
!     Use all surfaces (empty surface list).
!

      CURVE     = PNMBRL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TRGCDE    = 401
      NSURF     = 1
      SRFLST(1) = 2

      FIXREF    = 'IAU_PHOBOS'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      D         = 1.D8

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Non-occultation case. Use surface 2. (Phobos)' )

!
!     Find umbral point in cutting half-plane of previous case.
!
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
!
!     We can use ZZTANGNT to "suggest" a solution
!     point.
!
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                      SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                      SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Note that RESULT is a cell, not a window.
!
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

!
!     Get unit direction vector parallel to PLNVEC. Generate
!     off-DSK surface point RPOINT. RPOINT is 1 cm above the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

!
!     To generate a near-tangent ray passing through RPOINT,
!     we need to find the ray's vertex on the source. We can
!     do this by finding the limb on the source as seen from
!     RPOINT.
!
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Generate basis vectors defined by AXIS (primary)
!     and PLNVEC (secondary).
!
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )

      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the half-plane defined by AXIS
!        and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should miss the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator ' &
      //           'point in cutting half-plane of previous case. ' &
      //           'Occultation case. Use surface 2. (Phobos)' )

!
!     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
!     terminator point found by ZZTANGNT.
!
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

!
!     Find limb on the source (sun) as seen from the tangent point.
!
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Caution: LIMB and CUT are expressed with respect to different
!     origins. Shift LIMB so its center is an offset from the
!     target center.
!
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

!
!     The basis from the previous case still applies.
!
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!        XPT1 is on the opposite side of the half-plane defined by AXIS
!        and PLNVEC.
!
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

!
!     Now generate the ray's direction vector.
!
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

!
!     We can't just use VSEP to find the rotation angle of the
!     tangent ray with respect to AXIS, because the angle may
!     be larger than pi. Compute the angle from the components
!     of RAYDIR.
!
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )

      ANGLE = ATAN2 ( SDIR, CDIR )

!
!     Find intersection state, given the angle between AXIS and RAYDIR.
!
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     The ray should hit the DSK surface.
!
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

!
!     The returned point should be very close to the DSK surface. Map
!     the point to a point that is on the surface and check the
!     distance between the two.
!
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF, &
                   SRFLST,  ET,     FIXREF, 1, &
                   VERTEX,  DIR,    NPT,    FOUND_A1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

!
!     The returned point should be in the plane containing
!     the cutting half-plane.
!
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be the correct half-plane.
!
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

!
!     The returned point should be on the line containing the ray.
!
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

!
!     The returned point should be on the correct side of the vertex of
!     the line containing the ray.
!
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )





!***********************************************************************
!
!     TORUS tests
!
!***********************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Torus test setup' )

!
!     Create and load Nat's SPK and PCK.
!
      IF ( EXISTS(SPK1) ) THEN
         CALL DELFIL ( SPK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL NATSPK ( SPK1, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL ( PCK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL NATPCK ( PCK1, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create DSK containing three nested tori. The object is
!     centered at body Alpha.
!
      IF ( EXISTS(DSK4) ) THEN
         CALL DELFIL( DSK4 )
      END IF


      FIXREF = 'ALPHA_VIEW_XY'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'FIXFID', FIXFID, '>', 0, 0, OK )


      R      = 11.0D4
      RCROSS = 1.0D4

      CALL VPACK ( 0.D0, 0.D0, 0.D0, CENTER )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, NORMAL )

      SURFID = 1
      CALL T_TORUS( ALPHA, SURFID, FIXREF, NPOLYV, NCROSS, &
                    R,     RCROSS, CENTER, NORMAL, DSK4   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R      = 8.0D4

      CALL VPACK ( 0.D0, 0.D0, 0.D0, CENTER )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, NORMAL )

      SURFID = 2
      CALL T_TORUS( ALPHA, SURFID, FIXREF, NPOLYV, NCROSS, &
                    R,     RCROSS, CENTER, NORMAL, DSK4   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R      = 5.0D4

      CALL VPACK ( 0.D0, 0.D0, 0.D0, CENTER )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, NORMAL )

      SURFID = 3
      CALL T_TORUS( ALPHA, SURFID, FIXREF, NPOLYV, NCROSS, &
                    R,     RCROSS, CENTER, NORMAL, DSK4   )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL FURNSH ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
!
!     NESTED TORUS LIMB CASES
!
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Limb computation using nested' &
      //           'torus target shape. Use empty surface list. ' &
      //           '(ALPHA)' )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = LMBCRV
      SRCRAD    = 0.D0
      SHAPE     = DSKSHP
      TRGCDE    = 1000
      TARGET    = 'ALPHA'
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'ALPHA_VIEW_XY'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      CALL SPKPOS ( 'SUN', ET, FIXREF, 'NONE', TARGET, AXIS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODVCD ( TRGCDE, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL VHAT  ( AXIS, ZVEC )
      CALL FRAME ( ZVEC, REFVEC, VTEMP )

!
!     The ZZTANINI call requires a plane reference vector,
!     so this call must be made for each cutting half-plane.
!

      NCUTS  = 10
      DTHETA = 2*PI() / NCUTS

      DO I = 1, NCUTS

         THETA = (I-1)*DTHETA

!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Nested torus limb test: THETA (deg) = #.'
         CALL REPMD ( TITLE, '#', THETA*DPR(), 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


         CALL VROTV ( REFVEC, AXIS, THETA, PLNVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                         SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL SSIZED ( MAXWIN, RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCHSTP = 1.D-4
         SOLTOL = 1.D-15

         CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                         SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                         SCHSTP, SOLTOL, RESULT, POINTS        )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Note that RESULT is a cell, not a window.
!
         N = CARDD( RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect to find six limb points in each cutting half-plane.
!
         CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

!
!        Check the points and the angles in the RESULT array.
!
         DO J = 1, N

            CALL VEQU ( POINTS(1,J), TANPNT )

!
!           The returned point should be very close to the DSK surface.
!           Map the point to a point that is on the surface and check
!           the distance between the two.
!
            IF ( ODD(J) ) THEN

               CALL VLCOM  ( 1.D0,   TANPNT,  1.D2, PLNVEC, VERTEX )
               CALL VMINUS ( PLNVEC, DIR )

            ELSE

               CALL VLCOM  ( 1.D0,   TANPNT, -1.D2, PLNVEC, VERTEX )
               CALL VEQU   ( PLNVEC, DIR )

            END IF



            CALL DSKXV ( .FALSE., TARGET, NSURF, &
                         SRFLST,  ET,     FIXREF, 1, &
                         VERTEX,  DIR,    NPT,    FOUND_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


            TOL = TIGHT

            LABEL = 'TANPNT #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, TANPNT, '~~/', NPT, 3, TOL, OK )

!
!           The returned point should be in the plane containing
!           the cutting half-plane.
!
            CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VPRJP ( TANPNT, CUT, PROJ )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = MED

            LABEL = 'PROJ #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PROJ, '~~', TANPNT, 3, TOL, OK )

!
!           The returned point should be in the correct half-plane.
!
            DP  = VDOT( TANPNT, PLNVEC )

            TOL = TIGHT

            LABEL = 'YVEC dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

!
!           Compute the direction vector of the current tangent ray.
!
            CALL VSUB ( TANPNT, AXIS, RAYDIR )

!
!           The returned point should be on the line containing the
!           ray.
!
            CALL NPLNPT ( AXIS, RAYDIR, TANPNT, PNEAR, DIST )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            LABEL = 'PNEAR #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PNEAR, '~~/', TANPNT, 3, TOL, OK )

!
!           The returned point should be on the correct side of the
!           vertex of the line containing the ray.
!
            CALL VSUB ( TANPNT, AXIS, OFFSET )

            DP  =  VDOT( OFFSET, RAYDIR )

            LABEL = 'AXIS dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

!
!           The angle between RAYDIR and AXIS should match the
!           Jth element of RESULT.
!
            ANGLE = VSEP( AXIS, RAYDIR )

!             WRITE (*,*) '-------'
!             WRITE (*,*) 'ANGLE  = ', ANGLE
!             WRITE (*,*) 'RESULT = ', RESULT(J)

            LABEL = 'RESULT(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            CALL CHCKSD ( LABEL, RESULT(J), '~', ANGLE, TOL, OK )

         END DO

      END DO





!
!
!     NESTED TORUS UMBRAL TERMINATOR CASES
!
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Umbral terminator computation using nested' &
      //           'torus target shape. Use empty surface list. ' &
      //           '(ALPHA)' )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = UMBRAL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TRGCDE    = 1000
      TARGET    = 'ALPHA'
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'ALPHA_VIEW_XY'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      CALL SPKPOS ( 'SUN', ET, FIXREF, 'NONE', TARGET, AXIS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODVCD ( TRGCDE, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL VHAT  ( AXIS, ZVEC )
      CALL FRAME ( ZVEC, REFVEC, VTEMP )

!
!     The ZZTANINI call requires a plane reference vector,
!     so this call must be made for each cutting half-plane.
!

      NCUTS  = 10
      DTHETA = 2*PI() / NCUTS

      DO I = 1, NCUTS

         THETA = (I-1)*DTHETA

!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Nested torus umbral terminator test: ' &
         //      'THETA (deg) = #.'
         CALL REPMD ( TITLE, '#', THETA*DPR(), 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


         CALL VROTV ( REFVEC, AXIS, THETA, PLNVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                         SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL SSIZED ( MAXWIN, RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCHSTP = 1.D-4
         SOLTOL = 1.D-15

         CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                         SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                         SCHSTP, SOLTOL, RESULT, POINTS        )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Note that RESULT is a cell, not a window.
!
         N = CARDD( RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect to find six umbral terminator points in each cutting
!        half-plane.
!
         CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

!
!        Check the points and the angles in the RESULT array.
!
         DO J = 1, N

            CALL VEQU ( POINTS(1,J), TANPNT )

!
!           The returned point should be very close to the DSK surface.
!           Map the point to a point that is on the surface and check
!           the distance between the two.
!
            IF ( ODD(J) ) THEN

               CALL VLCOM  ( 1.D0,   TANPNT,  1.D2, PLNVEC, VERTEX )
               CALL VMINUS ( PLNVEC, DIR )

            ELSE

               CALL VLCOM  ( 1.D0,   TANPNT, -1.D2, PLNVEC, VERTEX )
               CALL VEQU   ( PLNVEC, DIR )

            END IF



            CALL DSKXV ( .FALSE., TARGET, NSURF, &
                         SRFLST,  ET,     FIXREF, 1, &
                         VERTEX,  DIR,    NPT,    FOUND_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


            TOL = TIGHT

            LABEL = 'TANPNT #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, TANPNT, '~~/', NPT, 3, TOL, OK )

!
!           The returned point should be in the plane containing
!           the cutting half-plane.
!
            CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VPRJP ( TANPNT, CUT, PROJ )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = MED

            LABEL = 'PROJ #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PROJ, '~~', TANPNT, 3, TOL, OK )

!
!           The returned point should be in the correct half-plane.
!
            DP  = VDOT( TANPNT, PLNVEC )

            TOL = TIGHT

            LABEL = 'YVEC dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

!
!           Compute the direction vector of the current tangent ray.
!
!           To generate a near-tangent ray passing through TANPNT, we
!           need to find the ray's vertex on the source. We can do this
!           by finding the limb on the source as seen from TANPNT.
!
            CALL VSUB ( TANPNT, AXIS, OFFSET )

            CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), &
                          OFFSET,    LIMB                 )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Caution: LIMB and CUT are expressed with respect to
!           different origins. Shift LIMB so its center is an offset
!           from the target center.
!
            CALL VADD ( AXIS,  LIMB(1), VTEMP   )
            CALL VEQU ( VTEMP,          LIMB(1) )

            CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Generate basis vectors defined by AXIS (primary)
!           and PLNVEC (secondary).
!
            CALL UCRSS ( AXIS, PLNVEC, ZVEC )
            CALL VHAT  ( AXIS,         XVEC )
            CALL UCRSS ( ZVEC, XVEC,   YVEC )


            IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
!
!              XPT1 is in the half-plane defined by AXIS and PLNVEC.
!
               CALL VEQU ( XPT1, SUNLPT )
            ELSE
               CALL VEQU ( XPT2, SUNLPT )
            END IF

!
!           Now generate the ray's direction vector.
!
            CALL VSUB ( TANPNT, SUNLPT, RAYDIR )

!
!           The returned point should be on the line containing the
!           ray.
!
            CALL NPLNPT ( SUNLPT, RAYDIR, TANPNT, PNEAR, DIST )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            LABEL = 'PNEAR #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PNEAR, '~~/', TANPNT, 3, TOL, OK )

!
!           The returned point should be on the correct side of the
!           vertex of the line containing the ray.
!
            CALL VSUB ( TANPNT, AXIS, OFFSET )

            DP  =  VDOT( OFFSET, RAYDIR )

            LABEL = 'AXIS dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

!
!           The angle between RAYDIR and AXIS should match the
!           Jth element of RESULT.
!
!           We can't just use VSEP to find the rotation angle of the
!           tangent ray with respect to AXIS, because the angle may be
!           larger than pi. Compute the angle from the components of
!           RAYDIR.
!
            CDIR  = VDOT ( RAYDIR, XVEC )
            SDIR  = VDOT ( RAYDIR, YVEC )

            ANGLE = ATAN2 ( SDIR, CDIR )
!             WRITE (*,*) '-------'
!             WRITE (*,*) 'ANGLE  = ', ANGLE
!             WRITE (*,*) 'RESULT = ', RESULT(J)

            LABEL = 'RESULT(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            CALL CHCKSD ( LABEL, RESULT(J), '~', ANGLE, TOL, OK )

         END DO

      END DO



!
!
!     NESTED TORUS PENUMBRAL TERMINATOR CASES
!
!


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Penumbral terminator computation using nested' &
      //           'torus target shape. Use empty surface list. ' &
      //           '(ALPHA)' )


!
!     Get solar radius.
!
      CALL BODVCD ( 10, 'RADII', 3, N, SUNRAD )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CURVE     = PNMBRL
      SRCRAD    = SUNRAD(1)
      SHAPE     = DSKSHP
      TRGCDE    = 1000
      TARGET    = 'ALPHA'
      NSURF     = 0
      SRFLST(1) = 0

      FIXREF    = 'ALPHA_VIEW_XY'
      CALL NAMFRM ( FIXREF, FIXFID )
      CALL CHCKXC ( .FALSE., ' ', OK )

      ET        = 0.D0

      CALL SPKPOS ( 'SUN', ET, FIXREF, 'NONE', TARGET, AXIS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL BODVCD ( TRGCDE, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL VHAT  ( AXIS, ZVEC )
      CALL FRAME ( ZVEC, REFVEC, VTEMP )

!
!     The ZZTANINI call requires a plane reference vector,
!     so this call must be made for each cutting half-plane.
!

      NCUTS  = 10
      DTHETA = 2*PI() / NCUTS

      DO I = 1, NCUTS

         THETA = (I-1)*DTHETA

!
! --- Case: ------------------------------------------------------
!
         TITLE = 'Nested torus penumbral terminator test: ' &
         //      'THETA (deg) = #.'
         CALL REPMD ( TITLE, '#', THETA*DPR(), 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


         CALL VROTV ( REFVEC, AXIS, THETA, PLNVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, &
                         SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL SSIZED ( MAXWIN, RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCHSTP = 1.D-4
         SOLTOL = 1.D-15

         CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF, &
                         SRFLST, FIXFID, ET,     PLNVEC, AXIS, &
                         SCHSTP, SOLTOL, RESULT, POINTS        )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        Note that RESULT is a cell, not a window.
!
         N = CARDD( RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

!
!        We expect to find six penumbral terminator points in each
!        cutting half-plane.
!
         CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

!
!        Check the points and the angles in the RESULT array.
!
         DO J = 1, N

            CALL VEQU ( POINTS(1,J), TANPNT )

!
!           The returned point should be very close to the DSK surface.
!           Map the point to a point that is on the surface and check
!           the distance between the two.
!
            IF ( ODD(J) ) THEN

               CALL VLCOM  ( 1.D0,   TANPNT,  1.D2, PLNVEC, VERTEX )
               CALL VMINUS ( PLNVEC, DIR )

            ELSE

               CALL VLCOM  ( 1.D0,   TANPNT, -1.D2, PLNVEC, VERTEX )
               CALL VEQU   ( PLNVEC, DIR )

            END IF



            CALL DSKXV ( .FALSE., TARGET, NSURF, &
                         SRFLST,  ET,     FIXREF, 1, &
                         VERTEX,  DIR,    NPT,    FOUND_A1 )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )


            TOL = TIGHT

            LABEL = 'TANPNT #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, TANPNT, '~~/', NPT, 3, TOL, OK )

!
!           The returned point should be in the plane containing
!           the cutting half-plane.
!
            CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VPRJP ( TANPNT, CUT, PROJ )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = MED

            LABEL = 'PROJ #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PROJ, '~~', TANPNT, 3, TOL, OK )

!
!           The returned point should be in the correct half-plane.
!
            DP  = VDOT( TANPNT, PLNVEC )

            TOL = TIGHT

            LABEL = 'YVEC dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

!
!           Compute the direction vector of the current tangent ray.
!
!           To generate a near-tangent ray passing through TANPNT, we
!           need to find the ray's vertex on the source. We can do this
!           by finding the limb on the source as seen from TANPNT.
!
            CALL VSUB ( TANPNT, AXIS, OFFSET )

            CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), &
                          OFFSET,    LIMB                 )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Caution: LIMB and CUT are expressed with respect to
!           different origins. Shift LIMB so its center is an offset
!           from the target center.
!
            CALL VADD ( AXIS,  LIMB(1), VTEMP   )
            CALL VEQU ( VTEMP,          LIMB(1) )

            CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 )
            CALL CHCKXC ( .FALSE., ' ', OK )
!
!           Generate basis vectors defined by AXIS (primary)
!           and PLNVEC (secondary).
!
            CALL UCRSS ( AXIS, PLNVEC, ZVEC )
            CALL VHAT  ( AXIS,         XVEC )
            CALL UCRSS ( ZVEC, XVEC,   YVEC )


            IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
!
!              XPT1 is in the half-plane on the opposite side
!              of AXI from the one defined by AXIS and PLNVEC.
!
               CALL VEQU ( XPT1, SUNLPT )
            ELSE
               CALL VEQU ( XPT2, SUNLPT )
            END IF

!
!           Now generate the ray's direction vector.
!
            CALL VSUB ( TANPNT, SUNLPT, RAYDIR )

!
!           The returned point should be on the line containing the
!           ray.
!
            CALL NPLNPT ( SUNLPT, RAYDIR, TANPNT, PNEAR, DIST )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            LABEL = 'PNEAR #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PNEAR, '~~/', TANPNT, 3, TOL, OK )

!
!           The returned point should be on the correct side of the
!           vertex of the line containing the ray.
!
            CALL VSUB ( TANPNT, AXIS, OFFSET )

            DP  =  VDOT( OFFSET, RAYDIR )

            LABEL = 'AXIS dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

!
!           The angle between RAYDIR and AXIS should match the
!           Jth element of RESULT.
!
!           We can't just use VSEP to find the rotation angle of the
!           tangent ray with respect to AXIS, because the angle may be
!           larger than pi. Compute the angle from the components of
!           RAYDIR.
!
            CDIR  = VDOT ( RAYDIR, XVEC )
            SDIR  = VDOT ( RAYDIR, YVEC )

            ANGLE = ATAN2 ( SDIR, CDIR )

            LABEL = 'RESULT(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            CALL CHCKSD ( LABEL, RESULT(J), '~', ANGLE, TOL, OK )

         END DO

      END DO





!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Clean up.' )


!
!     Clean up.
!
      CALL DELFIL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL DELFIL ( PCK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL SPKUEF ( HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( SPK1    )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL UNLOAD ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK0      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK1      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK2      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK3      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL UNLOAD ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL DELFIL ( DSK4      )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL KCLEAR

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END


