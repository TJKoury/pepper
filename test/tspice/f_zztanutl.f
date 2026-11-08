C$Procedure F_ZZTANUTL ( ZZTANUTL/ZZTANGNT tests )
 
      SUBROUTINE F_ZZTANUTL ( OK )
 
C$ Abstract
C
C     Exercise the entry points of routine ZZTANUTL and the
C     separate routine ZZTANGNT.
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
      INCLUDE 'zzdsk.inc'
      
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
C     ZZTANUTL:
C
C
C        ZZTANINI:   Initialize tangent ray utilities for limb or
C                    terminator computation. Terminators may be
C                    umbral or penumbral.
C
C        ZZTANSTA:   Compute state of ray for a given input angle.
C                    State is "occulted" or "not occulted." When
C                    the state is occulted, return the ray-surface 
C                    intercept point.
C
C     This routine also tests the related routine 
C
C        ZZTANGNT
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
C     E.D. Wright      (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 23-AUG-2016 (NJB) (EDW)
C
C-&
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      DPR
      DOUBLE PRECISION      PI
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VSEP

      INTEGER               CARDD

      LOGICAL               EXISTS
      LOGICAL               ODD

C
C     Local Parameters 
C
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

C
C     Local Variables 
C  
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
      LOGICAL               OCULTD


C
C     Saved values
C
C     Save variables in order to avoid stack room problems.
C
      SAVE

C
C     Initial values
C
      DATA                  ORIGIN / 3 * 0.D0 /

C
C     Open the test family.
C
      CALL TOPEN ( 'F_ZZTANUTL' )




C**********************************************************************
C
C     Setup for ZZTANINI/ZZTANSTA tests
C
C**********************************************************************
 
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
      CALL TCASE ( 'Setup: create DSK files.' )

C
C     For Mars, surface 1 is the "main" surface.
C
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

C
C     Load main Mars DSK.
C
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Surface 2 for Mars has slightly lower resolution.
C
      BODYID = TRGCDE
      SURFID = 2
      NLON   = 190
      NLAT   =  95

      IF ( EXISTS(DSK1) ) THEN
         CALL DELFIL( DSK1 )
      END IF

C
C     Create and load the second DSK.
C
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Surface 1 for Phobos is high-res.
C
      BODYID = 401
      SURFID = 1
      FIXREF = 'IAU_PHOBOS'

      NLON   = 200
      NLAT   = 100

      IF ( EXISTS(DSK2) ) THEN
         CALL DELFIL( DSK2 )
      END IF

C
C     Create and load the first Phobos DSK.
C
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Surface 2 for Phobos is lower-res.
C
      BODYID = 401
      SURFID = 2
      FIXREF = 'IAU_PHOBOS'

      NLON   =  80
      NLAT   =  40

      IF ( EXISTS(DSK3) ) THEN
         CALL DELFIL( DSK3 )
      END IF

C
C     Create and load the second Phobos DSK.
C
      CALL T_ELDS2Z ( BODYID, SURFID, FIXREF, NLON, NLAT, DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Set up a surface name-ID map.
C
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


C**********************************************************************
C
C     ZZTANUTL Error cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Call ZZTANUTL directly.' )

      CALL ZZTANUTL ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF,  
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS,
     .                ANGLE,  OCULTD, POINT                )

      CALL CHCKXC ( .TRUE., 'SPICE(BOGUSENTRY)', OK )



C**********************************************************************
C
C     ZZTANINI Error cases
C
C**********************************************************************

C
C --- Case: ------------------------------------------------------
C 
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C --- Case: ------------------------------------------------------
C 
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C --- Case: ------------------------------------------------------
C 
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(BADCURVETYPE)', OK )


C
C --- Case: ------------------------------------------------------
C 
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSOURCERADIUS)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANINI: axis and reference vector ' 
     .//           'are linearly dependent.'             )


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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )


C
C --- Case: ------------------------------------------------------
C 
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSHAPE)', OK )


C
C --- Case: ------------------------------------------------------
C 
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

      CALL FURNSH ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C**********************************************************************
C
C     ZZTANSTA Error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
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



C
C --- Case: ------------------------------------------------------
C 
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .TRUE., 'SPICE(NOLOADEDDSKFILES)', OK )



      CALL UNLOAD ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )





C**********************************************************************
C
C     ZZTANGNT Error cases
C
C**********************************************************************


C
C --- Case: ------------------------------------------------------
C 
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
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT: Zero plane reference vector.' )

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

      CALL CLEARD ( 3, PLNVEC )
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT: Bad curve type.' )

      CALL VPACK ( D,    2*D,  3*D,  AXIS   )
      CALL VPACK ( 0.D0, 1.D0, 1.D0, PLNVEC )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15

 
      CURVE = -1

      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADCURVETYPE)', OK )


C
C --- Case: ------------------------------------------------------
C 
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

 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSOURCERADIUS)', OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT: Axis and plane reference vector '
     .//           'are linearly dependent.'                   )

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

 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(DEGENERATECASE)', OK )




C
C --- Case: ------------------------------------------------------
C 
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

 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .TRUE., 'SPICE(BADSOURCERADIUS)', OK )



C**********************************************************************
C
C     ZZTANINI/ZZTANSTA normal cases
C
C**********************************************************************

C
C     These cases are combined because the two entry points
C     work in a cooperative fashion, and because the results of
C     initialization alone are not observable.
C  


C**********************************************************************
C
C     ELLIPSOID target shape
C
C**********************************************************************
   
C
C     LIMB cases
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize limb computation using '
     .//           'ellipsoidal target shape. (Mars)'    )


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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near limb point in cutting '
     .//           'half-plane of previous case. Non-occultation '
     .//           'case. (Mars)'                                 )

C
C     Find limb point in cutting half-plane of previous case.
C
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

C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, LPOINT )
      ELSE
         CALL VEQU ( XPT2, LPOINT )
      END IF

C
C     Generate off-ellipsoid point RPOINT. RPOINT is 1 mm above the
C     surface.
C
      CALL VLCOM ( 1.D0, LPOINT, 1.D-6, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near limb point in cutting '
     .//           'half-plane of previous case. Occultation '
     .//           'case. (Mars)'                                 )
C
C     Generate off-ellipsoid point RPOINT. RPOINT is 1 mm below the
C     surface.
C
      CALL VLCOM ( 1.D0, LPOINT, -1.D-6, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should hit the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

C
C     The returned point should be on the ellipsoid. Map the
C     point to a point that is on the ellipsoid and check the
C     distance between the two.
C
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~', NPT, 3, TOL, OK )

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~', POINT, 3, TOL, OK )

C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT test: create limb point matching that '
     .//           'from the previous case. (Mars)'                 )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Set absolute tolerance to 1 m. (Can use 10 cm on PC/Linux).
C
      CALL VEQU ( POINTS, TANPNT )

      TOL = 1.D-3
      CALL CHCKAD ( 'TANPNT', TANPNT, '~~', LPOINT, 3, TOL, OK )

C
C     The returned point should be on the ellipsoid. Map the
C     point to a point that is on the ellipsoid and check the
C     distance between the two.
C
      CALL EDPNT ( TANPNT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'TANPNT', TANPNT, '~~', NPT, 3, TOL, OK )

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( TANPNT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( AXIS, RAYDIR, TANPNT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', TANPNT, 3, TOL, OK )

C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( TANPNT, AXIS, OFFSET )

      DP =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )




C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Repeat the test with a different ellipsoidal '
     .//           'target. Initialize the system for Phobos.'   )


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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near limb point in cutting '
     .//           'half-plane of previous case. Non-occultation '
     .//           'case. (Phobos)'                               )

C
C     Find limb point in cutting half-plane of previous case.
C
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
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, LPOINT )
      ELSE
         CALL VEQU ( XPT2, LPOINT )
      END IF

C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-ellipsoid point.
C
      CALL VLCOM ( 1.D0, LPOINT, 1.D-6, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near limb point in cutting '
     .//           'half-plane of previous case. Occultation '
     .//           'case. (Phobos)'                              )

      CALL VLCOM ( 1.D0, LPOINT, -1.D-6, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should hit the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

C
C     The returned point should be on the ellipsoid. Map the
C     point to a point that is on the ellipsoid and check the
C     distance between the two.
C
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~', NPT, 3, TOL, OK )

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT test: create limb point matching that '
     .//           'from the previous case. (Phobos)'              )

      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

C
C     Set absolute tolerance to 1 m. (Can use 10 cm on PC/Linux).
C
      CALL VEQU ( POINTS, TANPNT )

      TOL = 1.D-3
      CALL CHCKAD ( 'TANPNT', TANPNT, '~~', LPOINT, 3, TOL, OK )

C
C     The returned point should be on the ellipsoid. Map the
C     point to a point that is on the ellipsoid and check the
C     distance between the two.
C
      CALL EDPNT ( TANPNT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'TANPNT', TANPNT, '~~', NPT, 3, TOL, OK )

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )

C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( TANPNT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( AXIS, RAYDIR, TANPNT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-6
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', TANPNT, 3, TOL, OK )
 

C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( TANPNT, AXIS, OFFSET )

      DP =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )
       

C
C     UMBRAL TERMINATOR cases
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize umbral terminator computation using '
     .//           'ellipsoidal target shape. (Mars)'              )


C
C     Get solar radius.
C
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )
 


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near umbral terminator point '
     .//           'in cutting half-plane of previous case. '
     .//           'Non-occultation case. (Mars)'               )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution
C     point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-ellipsoid point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, LPOINT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

              
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near umbral terminator point '
     .//           'in cutting half-plane of previous case. '
     .//           'Occultation case. (Mars)' )

C
C     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, LPOINT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

 
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be on the ellipsoid. Map the
C     point to a point that is on the ellipsoid and check the
C     distance between the two.
C
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )


C
C
C     Repeat previous tests, this time using Phobos as the target.
C     
C


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize umbral terminator computation using '
     .//           'ellipsoidal target shape. (Phobos)'             )


C
C     Get solar radius.
C
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )
 


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near umbral terminator point '
     .//           'in cutting half-plane of previous case. '
     .//           'Non-occultation case. (Phobos)'               )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution
C     point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-ellipsoid point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, LPOINT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

              
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near umbral terminator point '
     .//           'in cutting half-plane of previous case. '
     .//           'Occultation case. (Phobos)' )

C
C     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, LPOINT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

 
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be on the ellipsoid. Map the
C     point to a point that is on the ellipsoid and check the
C     distance between the two.
C
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )




C
C     PENUMBRAL TERMINATOR cases
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize penumbral terminator computation using '
     .//           'ellipsoidal target shape. (Mars)'              )


C
C     Get solar radius.
C
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near penumbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Non-occultation case. (Mars)'               )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-ellipsoid point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, LPOINT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 
C
C     For the penumbral case, the sun limb point should be 
C     on the -Y side of the axis.
C
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the axis bounding the
C        half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near penumbral terminator  '
     .//           'point in cutting half-plane of previous case. '
     .//           'Occultation case. (Mars)' )

C
C     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, LPOINT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

 
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C
C     For the penumbral case, the sun limb point should be 
C     on the -Y side of the axis.
C
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the axis bounding the
C        half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be on the ellipsoid. Map the
C     point to a point that is on the ellipsoid and check the
C     distance between the two.
C
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )



C
C
C     Repeat previous tests, this time using Phobos as the target.
C     
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize penumbral terminator computation using '
     .//           'ellipsoidal target shape. (Phobos)'              )


C
C     Get solar radius.
C
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near penumbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Non-occultation case. (Phobos)'               )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-ellipsoid point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, LPOINT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 
C
C     For the penumbral case, the sun limb point should be 
C     on the -Y side of the axis.
C
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the axis bounding the
C        half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near penumbral terminator  '
     .//           'point in cutting half-plane of previous case. '
     .//           'Occultation case. (Phobos)' )

C
C     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, LPOINT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

 
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C
C     For the penumbral case, the sun limb point should be 
C     on the -Y side of the axis.
C
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the axis bounding the
C        half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the ellipsoid.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be on the ellipsoid. Map the
C     point to a point that is on the ellipsoid and check the
C     distance between the two.
C
      CALL EDPNT ( POINT, A, B, C, NPT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )





       

C**********************************************************************
C
C     DSK target shape
C
C**********************************************************************




C
C     LIMB cases
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize limb computation using '
     .//           'DSK target shape. Empty surface list. (Mars)' )

C
C     Load DSKs.
C
      CALL FURNSH ( DSK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL FURNSH ( DSK3 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting '
     .//           'half-plane of previous case. Non-occultation '
     .//           'case. (Mars)'                                 )

C
C     Find limb point in cutting half-plane of previous case.
C


      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm above the
C     surface.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

       
       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting '
     .//           'half-plane of previous case. Occultation '
     .//           'case. (Mars)'                                 )
C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
C     surface.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~', POINT, 3, TOL, OK )

C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize limb computation using '
     .//           'DSK target shape. Specify both surfaces '
     .//           'in surface list. (Mars)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT test: compare DSK limb point to that '
     .//           'found in previous case. (Mars)'                   )

C
C     Find limb point in cutting half-plane of previous case.
C


      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )
C
C     We expect an exact match.
C
      TOL = 0.D0

      CALL CHCKAD ( 'LPOINT', LPOINT, '~~/', TANPNT, 3, TOL, OK )       


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize limb computation using '
     .//           'DSK target shape. Use surface 2. (Mars)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting '
     .//           'half-plane of previous case. Non-occultation '
     .//           'case. Use surface 2. (Mars)'                    )

C
C     Find limb point in cutting half-plane of previous case.
C


      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm above the
C     surface.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

      
       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting '
     .//           'half-plane of previous case. Occultation '
     .//           'case.  Use surface 2. (Mars)'                      )
C
C     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
C     surface.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~', POINT, 3, TOL, OK )

C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )


C
C
C     Use Phobos as the target.
C     
C
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize limb computation using '
     .//           'DSK target shape. Empty surface list. (Phobos)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting '
     .//           'half-plane of previous case. Non-occultation '
     .//           'case. (Phobos)'                                 )

C
C     Find limb point in cutting half-plane of previous case.
C


      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm above the
C     surface.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

       
       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting '
     .//           'half-plane of previous case. Occultation '
     .//           'case. (Phobos)'                                 )
C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
C     surface.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~', POINT, 3, TOL, OK )

C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize limb computation using '
     .//           'DSK target shape. Specify both surfaces '
     .//           'in surface list. (Phobos)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT test: compare DSK limb point to that '
     .//           'found in previous case. (Phobos)'             )

C
C     Find limb point in cutting half-plane of previous case.
C


      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, LPOINT )
C
C     We expect an exact match.
C
      TOL = 0.D0

      CALL CHCKAD ( 'LPOINT', LPOINT, '~~/', TANPNT, 3, TOL, OK )       


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize limb computation using '
     .//           'DSK target shape. Use surface 2. (Phobos)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting '
     .//           'half-plane of previous case. Non-occultation '
     .//           'case. Use surface 2. (Phobos)'                    )

C
C     Find limb point in cutting half-plane of previous case.
C


      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm above the
C     surface.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

      
       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK limb point in cutting '
     .//           'half-plane of previous case. Occultation '
     .//           'case.  Use surface 2. (Phobos)'                   )
C
C     Generate off-ellipsoid point RPOINT. RPOINT is 1 cm below the
C     surface.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, RAYDIR )

      ANGLE = VSEP ( AXIS, RAYDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )

C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~', POINT, 3, TOL, OK )

C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )

C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( AXIS, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )

C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, AXIS, OFFSET )

      DP  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', DP, '>', 0.D0, TOL, OK )






C
C     DSK UMBRAL TERMINATOR cases
C

C
C
C     Mars is the target.
C
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize umbral terminator computation using '
     .//           'DSK target shape. Use empty surface list. (Mars)' )


C
C     Get solar radius.
C
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK umbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Non-occultation case. Empty surface list. (Mars)' )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution
C     point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-DSK surface point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

              
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK umbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Occultation case. Empty surface list. (Mars)' )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

C
C     Find limb on the source (sun) as seen from the tangent point.
C
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )




C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize umbral terminator computation using '
     .//           'DSK target shape. Specify both surfaces '
     .//           'in surface list. (Mars)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT test: compare DSK umbral terminator '
     .//           'point to that found in previous case. '
     .//           '(Mars)' )

C
C     Find limb point in cutting half-plane of previous case.
C


      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, POINT )
C
C     We expect an exact match.
C
      TOL = 0.D0

      CALL CHCKAD ( 'POINT', POINT, '~~/', TANPNT, 3, TOL, OK )       



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize umbral terminator computation using '
     .//           'DSK target shape. Use surface 2. (Mars)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK umbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Non-occultation case. Use surface 2. (Mars)' )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution
C     point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-DSK surface point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

              
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK umbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Occultation case. Use surface 2. (Mars)' )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

C
C     Find limb on the source (sun) as seen from the tangent point.
C
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = TIGHT

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )





C
C
C     Phobos is the target.
C
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize umbral terminator computation using '
     .//           'DSK target shape. Use empty surface list. '
     .//           '(Phobos)' )


C
C     Get solar radius.
C
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK umbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Non-occultation case. Empty surface list. '
     .//           '(Phobos)' )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution
C     point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-DSK surface point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

              
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK umbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Occultation case. Empty surface list. (Phobos)' )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

C
C     Find limb on the source (sun) as seen from the tangent point.
C
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )




C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize umbral terminator computation using '
     .//           'DSK target shape. Specify both surfaces '
     .//           'in surface list. (Phobos)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT test: compare DSK umbral terminator '
     .//           'point to that found in previous case. '
     .//           '(Phobos)' )

C
C     Find limb point in cutting half-plane of previous case.
C


      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, POINT )
C
C     We expect an exact match.
C
      TOL = 0.D0

      CALL CHCKAD ( 'POINT', POINT, '~~/', TANPNT, 3, TOL, OK )       



C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize umbral terminator computation using '
     .//           'DSK target shape. Use surface 2. (Phobos)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK umbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Non-occultation case. Use surface 2. (Phobos)' )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution
C     point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-DSK surface point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

              
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK umbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Occultation case. Use surface 2. (Phobos)' )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

C
C     Find limb on the source (sun) as seen from the tangent point.
C
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C
      IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C        XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )





C
C
C     DSK PENUMBRAL TERMINATOR cases
C

C
C
C     Mars is the target.
C
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize penumbral terminator computation using '
     .//           'DSK target shape. Use empty surface list. (Mars)' )


C
C     Get solar radius.
C
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Non-occultation case. Empty surface list. (Mars)' )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution
C     point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-DSK surface point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the half-plane defined by AXIS
C        and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

       

              
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Occultation case. Empty surface list. (Mars)' )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

C
C     Find limb on the source (sun) as seen from the tangent point.
C
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the half-plane defined by AXIS
C        and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize penumbral terminator computation using '
     .//           'DSK target shape. Specify both surfaces '
     .//           'in surface list. (Mars)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT test: compare DSK penumbral terminator '
     .//           'point to that found in previous case. '
     .//           '(Mars)' )

C
C     Find limb point in cutting half-plane of previous case.
C


      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, POINT )
C
C     We expect an exact match.
C
      TOL = 0.D0

      CALL CHCKAD ( 'POINT', POINT, '~~/', TANPNT, 3, TOL, OK )       


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize penumbral terminator computation using '
     .//           'DSK target shape. Use surface 2. (Mars)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Non-occultation case. Use surface 2. (Mars)' )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 499, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution
C     point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-DSK surface point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the half-plane defined by AXIS
C        and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

              
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Occultation case. Use surface 2. (Mars)' )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

C
C     Find limb on the source (sun) as seen from the tangent point.
C
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C 
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the half-plane defined by AXIS
C        and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = MED
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )


C
C
C     Phobos is the target.
C
C

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize penumbral terminator computation using '
     .//           'DSK target shape. Use empty surface list. '
     .//           '(Phobos)' )


C
C     Get solar radius.
C
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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
       

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Non-occultation case. Empty surface list. '
     .//           '(Phobos)' )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution
C     point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-DSK surface point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the half-plane defined by AXIS
C        and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

       

              
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Occultation case. Empty surface list. (Phobos)' )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

C
C     Find limb on the source (sun) as seen from the tangent point.
C
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the half-plane defined by AXIS
C        and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize penumbral terminator computation using '
     .//           'DSK target shape. Specify both surfaces '
     .//           'in surface list. (Phobos)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'ZZTANGNT test: compare DSK penumbral terminator '
     .//           'point to that found in previous case. '
     .//           '(Phobos)' )

C
C     Find limb point in cutting half-plane of previous case.
C


      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, POINT )
C
C     We expect an exact match.
C
      TOL = 0.D0

      CALL CHCKAD ( 'POINT', POINT, '~~/', TANPNT, 3, TOL, OK )       


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Initialize penumbral terminator computation using '
     .//           'DSK target shape. Use surface 2. (Phobos)' )

C
C     Use all surfaces (empty surface list).
C

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

      CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Non-occultation case. Use surface 2. (Phobos)' )

C
C     Find umbral point in cutting half-plane of previous case.
C
      CALL BODVCD ( 401, 'RADII', 3, N, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )
 
      A = RADII(1)
      B = RADII(2)
      C = RADII(3)
C
C     We can use ZZTANGNT to "suggest" a solution
C     point.  
C
      CALL SSIZED ( MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SCHSTP = 4.D0
      SOLTOL = 1.D-15
 
      CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                SCHSTP, SOLTOL, RESULT, POINTS        )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Note that RESULT is a cell, not a window.
C
      N = CARDD( RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      CALL VEQU ( POINTS, TANPNT )
       
C
C     Get unit direction vector parallel to PLNVEC. Generate
C     off-DSK surface point RPOINT. RPOINT is 1 cm above the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, 1.D-5, YVEC, RPOINT )

C
C     To generate a near-tangent ray passing through RPOINT, 
C     we need to find the ray's vertex on the source. We can
C     do this by finding the limb on the source as seen from
C     RPOINT.
C
      CALL VSUB ( RPOINT, AXIS, OFFSET )

      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )
      
C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Generate basis vectors defined by AXIS (primary)
C     and PLNVEC (secondary).
C
      CALL UCRSS ( AXIS, PLNVEC, ZVEC )
      CALL VHAT  ( AXIS,         XVEC )
      CALL UCRSS ( ZVEC, XVEC,   YVEC )
 
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the half-plane defined by AXIS
C        and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C    
      CDIR  = VDOT ( RAYDIR, XVEC )
      SDIR  = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )

C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     The ray should miss the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .FALSE., OK )

              
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Check state of ray near DSK penumbral terminator '
     .//           'point in cutting half-plane of previous case. '
     .//           'Occultation case. Use surface 2. (Phobos)' )

C
C     Generate off-DSK point RPOINT. RPOINT is 1 cm below the
C     terminator point found by ZZTANGNT.
C
      CALL VLCOM ( 1.D0, TANPNT, -1.D-5, YVEC, RPOINT )

      CALL VSUB ( RPOINT, AXIS, OFFSET )

C
C     Find limb on the source (sun) as seen from the tangent point.
C
      CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), OFFSET, LIMB )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Caution: LIMB and CUT are expressed with respect to different
C     origins. Shift LIMB so its center is an offset from the 
C     target center.
C
      CALL VADD ( AXIS,  LIMB(1), VTEMP   )
      CALL VEQU ( VTEMP,          LIMB(1) )

      CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( CUT, NORMAL )

C
C     The basis from the previous case still applies.
C 
      IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C        XPT1 is on the opposite side of the half-plane defined by AXIS
C        and PLNVEC.
C
         CALL VEQU ( XPT1, SUNLPT )
      ELSE
         CALL VEQU ( XPT2, SUNLPT )
      END IF

C
C     Now generate the ray's direction vector.
C
      CALL VSUB ( RPOINT, SUNLPT, RAYDIR )

C
C     We can't just use VSEP to find the rotation angle of the 
C     tangent ray with respect to AXIS, because the angle may
C     be larger than pi. Compute the angle from the components
C     of RAYDIR. 
C      
      CDIR = VDOT ( RAYDIR, XVEC )
      SDIR = VDOT ( RAYDIR, YVEC )
      
      ANGLE = ATAN2 ( SDIR, CDIR )
      
C
C     Find intersection state, given the angle between AXIS and RAYDIR.
C
      CALL ZZTANSTA ( ANGLE, OCULTD, POINT )
      CALL CHCKXC ( .FALSE., ' ', OK )
       
C
C     The ray should hit the DSK surface.
C
      CALL CHCKSL ( 'OCULTD', OCULTD, .TRUE., OK )
         
C
C     The returned point should be very close to the DSK surface. Map
C     the point to a point that is on the surface and check the
C     distance between the two.
C
      CALL VSCL   ( 2.D0,   POINT, VERTEX )
      CALL VMINUS ( VERTEX, DIR )

      CALL DSKXV ( .FALSE., TARGET, NSURF,
     .             SRFLST,  ET,     FIXREF, 1,
     .             VERTEX,  DIR,    NPT,    FOUND )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )
      
      TOL = TIGHT

      CALL CHCKAD ( 'POINT', POINT, '~~/', NPT, 3, TOL, OK )       

C
C     The returned point should be in the plane containing
C     the cutting half-plane.
C
      CALL VPRJP ( POINT, CUT, PROJ )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5

      CALL CHCKAD ( 'PROJ', PROJ, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be the correct half-plane.
C
      DP  = VDOT( POINT, YVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'YVEC dot', DP, '>', 0.D0, TOL, OK )
       
C
C     The returned point should be on the line containing the ray.
C
      CALL NPLNPT ( SUNLPT, RAYDIR, POINT, PNEAR, DIST )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = 1.D-5
      CALL CHCKAD ( 'PNEAR', PNEAR, '~~/', POINT, 3, TOL, OK )
      
C
C     The returned point should be on the correct side of the vertex of
C     the line containing the ray.
C
      CALL VSUB ( POINT, SUNLPT, OFFSET )

      D  =  VDOT( OFFSET, RAYDIR )

      CALL CHCKSD ( 'AXIS dot', D, '>', 0.D0, TOL, OK )





C***********************************************************************
C
C     TORUS tests
C
C***********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Torus test setup' )

C
C     Create and load Nat's SPK and PCK.
C
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

C
C     Create DSK containing three nested tori. The object is 
C     centered at body Alpha.
C
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
      CALL T_TORUS( ALPHA, SURFID, FIXREF, NPOLYV, NCROSS,
     .              R,     RCROSS, CENTER, NORMAL, DSK4   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R      = 8.0D4

      CALL VPACK ( 0.D0, 0.D0, 0.D0, CENTER )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, NORMAL )

      SURFID = 2
      CALL T_TORUS( ALPHA, SURFID, FIXREF, NPOLYV, NCROSS,
     .              R,     RCROSS, CENTER, NORMAL, DSK4   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      R      = 5.0D4

      CALL VPACK ( 0.D0, 0.D0, 0.D0, CENTER )
      CALL VPACK ( 1.D0, 0.D0, 0.D0, NORMAL )

      SURFID = 3
      CALL T_TORUS( ALPHA, SURFID, FIXREF, NPOLYV, NCROSS,
     .              R,     RCROSS, CENTER, NORMAL, DSK4   )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL FURNSH ( DSK4 )
      CALL CHCKXC ( .FALSE., ' ', OK )
      

C
C
C     NESTED TORUS LIMB CASES
C
C


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Limb computation using nested'
     .//           'torus target shape. Use empty surface list. '
     .//           '(ALPHA)' )


C
C     Get solar radius.
C
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

C
C     The ZZTANINI call requires a plane reference vector, 
C     so this call must be made for each cutting half-plane.
C

      NCUTS  = 10
      DTHETA = 2*PI() / NCUTS

      DO I = 1, NCUTS
             
         THETA = (I-1)*DTHETA

C
C --- Case: ------------------------------------------------------
C 
         TITLE = 'Nested torus limb test: THETA (deg) = #.'
         CALL REPMD ( TITLE, '#', THETA*DPR(), 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


         CALL VROTV ( REFVEC, AXIS, THETA, PLNVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                   SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
         CALL CHCKXC ( .FALSE., ' ', OK )
 

         CALL SSIZED ( MAXWIN, RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCHSTP = 1.D-4
         SOLTOL = 1.D-15
 
         CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                   SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                   SCHSTP, SOLTOL, RESULT, POINTS        )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Note that RESULT is a cell, not a window.
C
         N = CARDD( RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect to find six limb points in each cutting half-plane.
C
         CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

C
C        Check the points and the angles in the RESULT array.
C
         DO J = 1, N

            CALL VEQU ( POINTS(1,J), TANPNT )
 
C
C           The returned point should be very close to the DSK surface.
C           Map the point to a point that is on the surface and check
C           the distance between the two.
C
            IF ( ODD(J) ) THEN

               CALL VLCOM  ( 1.D0,   TANPNT,  1.D2, PLNVEC, VERTEX )
               CALL VMINUS ( PLNVEC, DIR )

            ELSE

               CALL VLCOM  ( 1.D0,   TANPNT, -1.D2, PLNVEC, VERTEX )
               CALL VEQU   ( PLNVEC, DIR )

            END IF
            


            CALL DSKXV ( .FALSE., TARGET, NSURF,
     .                   SRFLST,  ET,     FIXREF, 1,
     .                   VERTEX,  DIR,    NPT,    FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      
            TOL = TIGHT

            LABEL = 'TANPNT #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, TANPNT, '~~/', NPT, 3, TOL, OK )

C
C           The returned point should be in the plane containing
C           the cutting half-plane.
C
            CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VPRJP ( TANPNT, CUT, PROJ )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = MED

            LABEL = 'PROJ #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PROJ, '~~', TANPNT, 3, TOL, OK )

C
C           The returned point should be in the correct half-plane.
C
            DP  = VDOT( TANPNT, PLNVEC )

            TOL = TIGHT

            LABEL = 'YVEC dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

C
C           Compute the direction vector of the current tangent ray.
C
            CALL VSUB ( TANPNT, AXIS, RAYDIR )

C
C           The returned point should be on the line containing the
C           ray.
C
            CALL NPLNPT ( AXIS, RAYDIR, TANPNT, PNEAR, DIST )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            LABEL = 'PNEAR #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PNEAR, '~~/', TANPNT, 3, TOL, OK )

C
C           The returned point should be on the correct side of the
C           vertex of the line containing the ray.
C
            CALL VSUB ( TANPNT, AXIS, OFFSET )

            DP  =  VDOT( OFFSET, RAYDIR )

            LABEL = 'AXIS dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

C
C           The angle between RAYDIR and AXIS should match the
C           Jth element of RESULT.
C
            ANGLE = VSEP( AXIS, RAYDIR )

C             WRITE (*,*) '-------'
C             WRITE (*,*) 'ANGLE  = ', ANGLE
C             WRITE (*,*) 'RESULT = ', RESULT(J)
           
            LABEL = 'RESULT(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            CALL CHCKSD ( LABEL, RESULT(J), '~', ANGLE, TOL, OK )

         END DO

      END DO      





C
C
C     NESTED TORUS UMBRAL TERMINATOR CASES
C
C


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Umbral terminator computation using nested'
     .//           'torus target shape. Use empty surface list. '
     .//           '(ALPHA)' )


C
C     Get solar radius.
C
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

C
C     The ZZTANINI call requires a plane reference vector, 
C     so this call must be made for each cutting half-plane.
C

      NCUTS  = 10
      DTHETA = 2*PI() / NCUTS

      DO I = 1, NCUTS
             
         THETA = (I-1)*DTHETA

C
C --- Case: ------------------------------------------------------
C 
         TITLE = 'Nested torus umbral terminator test: '
     .   //      'THETA (deg) = #.'
         CALL REPMD ( TITLE, '#', THETA*DPR(), 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


         CALL VROTV ( REFVEC, AXIS, THETA, PLNVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                   SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
         CALL CHCKXC ( .FALSE., ' ', OK )
 

         CALL SSIZED ( MAXWIN, RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCHSTP = 1.D-4
         SOLTOL = 1.D-15
 
         CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                   SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                   SCHSTP, SOLTOL, RESULT, POINTS        )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Note that RESULT is a cell, not a window.
C
         N = CARDD( RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect to find six umbral terminator points in each cutting
C        half-plane.
C
         CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

C
C        Check the points and the angles in the RESULT array.
C
         DO J = 1, N

            CALL VEQU ( POINTS(1,J), TANPNT )
 
C
C           The returned point should be very close to the DSK surface.
C           Map the point to a point that is on the surface and check
C           the distance between the two.
C
            IF ( ODD(J) ) THEN

               CALL VLCOM  ( 1.D0,   TANPNT,  1.D2, PLNVEC, VERTEX )
               CALL VMINUS ( PLNVEC, DIR )

            ELSE

               CALL VLCOM  ( 1.D0,   TANPNT, -1.D2, PLNVEC, VERTEX )
               CALL VEQU   ( PLNVEC, DIR )

            END IF
            


            CALL DSKXV ( .FALSE., TARGET, NSURF,
     .                   SRFLST,  ET,     FIXREF, 1,
     .                   VERTEX,  DIR,    NPT,    FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      
            TOL = TIGHT

            LABEL = 'TANPNT #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, TANPNT, '~~/', NPT, 3, TOL, OK )

C
C           The returned point should be in the plane containing
C           the cutting half-plane.
C
            CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VPRJP ( TANPNT, CUT, PROJ )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = MED

            LABEL = 'PROJ #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PROJ, '~~', TANPNT, 3, TOL, OK )

C
C           The returned point should be in the correct half-plane.
C
            DP  = VDOT( TANPNT, PLNVEC )

            TOL = TIGHT

            LABEL = 'YVEC dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

C
C           Compute the direction vector of the current tangent ray.
C
C           To generate a near-tangent ray passing through TANPNT, we
C           need to find the ray's vertex on the source. We can do this
C           by finding the limb on the source as seen from TANPNT.
C
            CALL VSUB ( TANPNT, AXIS, OFFSET )

            CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), 
     .                    OFFSET,    LIMB                 )
            CALL CHCKXC ( .FALSE., ' ', OK )      
C
C           Caution: LIMB and CUT are expressed with respect to
C           different origins. Shift LIMB so its center is an offset
C           from the target center.
C
            CALL VADD ( AXIS,  LIMB(1), VTEMP   )
            CALL VEQU ( VTEMP,          LIMB(1) )

            CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Generate basis vectors defined by AXIS (primary)
C           and PLNVEC (secondary).
C
            CALL UCRSS ( AXIS, PLNVEC, ZVEC )
            CALL VHAT  ( AXIS,         XVEC )
            CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

            IF ( VDOT(XPT1, YVEC) .GT. 0 ) THEN
C
C              XPT1 is in the half-plane defined by AXIS and PLNVEC.
C
               CALL VEQU ( XPT1, SUNLPT )
            ELSE
               CALL VEQU ( XPT2, SUNLPT )
            END IF

C
C           Now generate the ray's direction vector.
C
            CALL VSUB ( TANPNT, SUNLPT, RAYDIR )

C
C           The returned point should be on the line containing the
C           ray.
C
            CALL NPLNPT ( SUNLPT, RAYDIR, TANPNT, PNEAR, DIST )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            LABEL = 'PNEAR #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PNEAR, '~~/', TANPNT, 3, TOL, OK )

C
C           The returned point should be on the correct side of the
C           vertex of the line containing the ray.
C
            CALL VSUB ( TANPNT, AXIS, OFFSET )

            DP  =  VDOT( OFFSET, RAYDIR )

            LABEL = 'AXIS dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

C
C           The angle between RAYDIR and AXIS should match the
C           Jth element of RESULT.
C
C           We can't just use VSEP to find the rotation angle of the
C           tangent ray with respect to AXIS, because the angle may be
C           larger than pi. Compute the angle from the components of
C           RAYDIR.
C    
            CDIR  = VDOT ( RAYDIR, XVEC )
            SDIR  = VDOT ( RAYDIR, YVEC )
      
            ANGLE = ATAN2 ( SDIR, CDIR )
C             WRITE (*,*) '-------'
C             WRITE (*,*) 'ANGLE  = ', ANGLE
C             WRITE (*,*) 'RESULT = ', RESULT(J)
           
            LABEL = 'RESULT(#)'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            CALL CHCKSD ( LABEL, RESULT(J), '~', ANGLE, TOL, OK )

         END DO

      END DO      



C
C
C     NESTED TORUS PENUMBRAL TERMINATOR CASES
C
C


C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Penumbral terminator computation using nested'
     .//           'torus target shape. Use empty surface list. '
     .//           '(ALPHA)' )


C
C     Get solar radius.
C
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

C
C     The ZZTANINI call requires a plane reference vector, 
C     so this call must be made for each cutting half-plane.
C

      NCUTS  = 10
      DTHETA = 2*PI() / NCUTS

      DO I = 1, NCUTS
             
         THETA = (I-1)*DTHETA

C
C --- Case: ------------------------------------------------------
C 
         TITLE = 'Nested torus penumbral terminator test: '
     .   //      'THETA (deg) = #.'
         CALL REPMD ( TITLE, '#', THETA*DPR(), 14, TITLE )
         CALL CHCKXC ( .FALSE., ' ', OK )

         CALL TCASE ( TITLE )


         CALL VROTV ( REFVEC, AXIS, THETA, PLNVEC )
         CALL CHCKXC ( .FALSE., ' ', OK )


         CALL ZZTANINI ( CURVE,  SRCRAD, SHAPE, TRGCDE, NSURF, 
     .                   SRFLST, FIXFID, ET,    PLNVEC, AXIS   )
         CALL CHCKXC ( .FALSE., ' ', OK )
 

         CALL SSIZED ( MAXWIN, RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

         SCHSTP = 1.D-4
         SOLTOL = 1.D-15
 
         CALL ZZTANGNT ( CURVE,  SRCRAD, SHAPE,  TRGCDE, NSURF,
     .                   SRFLST, FIXFID, ET,     PLNVEC, AXIS,
     .                   SCHSTP, SOLTOL, RESULT, POINTS        )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        Note that RESULT is a cell, not a window.
C
         N = CARDD( RESULT )
         CALL CHCKXC ( .FALSE., ' ', OK )

C
C        We expect to find six penumbral terminator points in each
C        cutting half-plane.
C
         CALL CHCKSI ( 'N', N, '=', 6, 0, OK )

C
C        Check the points and the angles in the RESULT array.
C
         DO J = 1, N

            CALL VEQU ( POINTS(1,J), TANPNT )
 
C
C           The returned point should be very close to the DSK surface.
C           Map the point to a point that is on the surface and check
C           the distance between the two.
C
            IF ( ODD(J) ) THEN

               CALL VLCOM  ( 1.D0,   TANPNT,  1.D2, PLNVEC, VERTEX )
               CALL VMINUS ( PLNVEC, DIR )

            ELSE

               CALL VLCOM  ( 1.D0,   TANPNT, -1.D2, PLNVEC, VERTEX )
               CALL VEQU   ( PLNVEC, DIR )

            END IF
            


            CALL DSKXV ( .FALSE., TARGET, NSURF,
     .                   SRFLST,  ET,     FIXREF, 1,
     .                   VERTEX,  DIR,    NPT,    FOUND )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      
            TOL = TIGHT

            LABEL = 'TANPNT #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, TANPNT, '~~/', NPT, 3, TOL, OK )

C
C           The returned point should be in the plane containing
C           the cutting half-plane.
C
            CALL PSV2PL ( ORIGIN, AXIS, PLNVEC, CUT )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL VPRJP ( TANPNT, CUT, PROJ )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = MED

            LABEL = 'PROJ #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PROJ, '~~', TANPNT, 3, TOL, OK )

C
C           The returned point should be in the correct half-plane.
C
            DP  = VDOT( TANPNT, PLNVEC )

            TOL = TIGHT

            LABEL = 'YVEC dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

C
C           Compute the direction vector of the current tangent ray.
C
C           To generate a near-tangent ray passing through TANPNT, we
C           need to find the ray's vertex on the source. We can do this
C           by finding the limb on the source as seen from TANPNT.
C
            CALL VSUB ( TANPNT, AXIS, OFFSET )

            CALL EDLIMB ( SUNRAD(1), SUNRAD(2), SUNRAD(3), 
     .                    OFFSET,    LIMB                 )
            CALL CHCKXC ( .FALSE., ' ', OK )      
C
C           Caution: LIMB and CUT are expressed with respect to
C           different origins. Shift LIMB so its center is an offset
C           from the target center.
C
            CALL VADD ( AXIS,  LIMB(1), VTEMP   )
            CALL VEQU ( VTEMP,          LIMB(1) )

            CALL INELPL ( LIMB, CUT, NXPTS, XPT1, XPT2 ) 
            CALL CHCKXC ( .FALSE., ' ', OK )
C
C           Generate basis vectors defined by AXIS (primary)
C           and PLNVEC (secondary).
C
            CALL UCRSS ( AXIS, PLNVEC, ZVEC )
            CALL VHAT  ( AXIS,         XVEC )
            CALL UCRSS ( ZVEC, XVEC,   YVEC )
 

            IF ( VDOT(XPT1, YVEC) .LT. 0 ) THEN
C
C              XPT1 is in the half-plane on the opposite side
C              of AXI from the one defined by AXIS and PLNVEC.
C
               CALL VEQU ( XPT1, SUNLPT )
            ELSE
               CALL VEQU ( XPT2, SUNLPT )
            END IF

C
C           Now generate the ray's direction vector.
C
            CALL VSUB ( TANPNT, SUNLPT, RAYDIR )

C
C           The returned point should be on the line containing the
C           ray.
C
            CALL NPLNPT ( SUNLPT, RAYDIR, TANPNT, PNEAR, DIST )
            CALL CHCKXC ( .FALSE., ' ', OK )

            TOL = TIGHT

            LABEL = 'PNEAR #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKAD ( LABEL, PNEAR, '~~/', TANPNT, 3, TOL, OK )

C
C           The returned point should be on the correct side of the
C           vertex of the line containing the ray.
C
            CALL VSUB ( TANPNT, AXIS, OFFSET )

            DP  =  VDOT( OFFSET, RAYDIR )

            LABEL = 'AXIS dot #'
            CALL REPMI ( LABEL, '#', J, LABEL )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSD ( LABEL, DP, '>', 0.D0, TOL, OK )

C
C           The angle between RAYDIR and AXIS should match the
C           Jth element of RESULT.
C
C           We can't just use VSEP to find the rotation angle of the
C           tangent ray with respect to AXIS, because the angle may be
C           larger than pi. Compute the angle from the components of
C           RAYDIR.
C    
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




 
C
C --- Case: ------------------------------------------------------
C 
      CALL TCASE ( 'Clean up.' )


C
C     Clean up.
C
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

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN 
      END


