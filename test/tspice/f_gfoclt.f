C$Procedure      F_GFOCLT ( GFOCLT family tests )

      SUBROUTINE F_GFOCLT ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C
C        GFOCLT
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

      INCLUDE 'gf.inc'
      INCLUDE 'dskdsc.inc'
      INCLUDE 'zzabcorr.inc'

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
C     This routine tests the higher-level SPICELIB
C     geometry routine GFOCLT.
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
C-    TSPICE Version 3.1.0, 31-JAN-2017 (EDW)
C
C        Modified GFSTOL test case to correspond to change
C        in ZZGFSOLV.
C
C-    TSPICE Version 3.0.0, 23-MAY-2016 (NJB)
C
C        Updated to test support for DSK shapes.
C
C        Updated expected keywords for invalid shape cases.
C
C-    TSPICE Version 2.3.0, 20-NOV-2012 (EDW)
C
C        Added test on GFSTOL use.
C
C-    TSPICE Version 2.2.0, 26-JUL-2008 (NJB)
C
C        Updated to account for change in GFOCLT's argument list.
C        Tests of error handling for small result window were added.
C
C-    TSPICE Version 2.1.0, 14-APR-2008 (NJB)
C
C        Kernel clean-up now consists only of
C        SPK unloading via SPKUEF and SPK file
C        deletion.
C
C-    TSPICE Version 2.0.0, 12-APR-2008 (NJB)
C
C        Added test cases using zero radii and blank frame
C        names for point targets. Added cases for invalid
C        aberration corrections. Added cases to test
C        handling of missing SPK and PCK orientation data.
C
C-    TSPICE Version 1.1.0, 13-MAR-2008 (NJB)
C
C        Corrected one syntax error and numerous expressions
C        that caused FTNCHEK warnings.
C
C-    TSPICE Version 1.0.0, 15-NOV-2007 (NJB)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      JYEAR
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VNORM

      INTEGER               WNCARD

      LOGICAL               EXISTS
      LOGICAL               ODD

C
C     Local parameters
C
      CHARACTER*(*)         DSK
      PARAMETER           ( DSK  = 'gfoclt_nat.bds' )

      CHARACTER*(*)         PCK
      PARAMETER           ( PCK  = 'nat.tpc' )

      CHARACTER*(*)         PCK2
      PARAMETER           ( PCK2 = 'generic.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1 = 'nat.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2 = 'generic.bsp' )

      DOUBLE PRECISION      TIMTOL
      PARAMETER           ( TIMTOL = 3.D-6 )

C
C     Looser time tolerance for point/ellipsoid
C     occultations where the ellipsoid is
C     in front:
C
      DOUBLE PRECISION      TTOL2
      PARAMETER           ( TTOL2  = 35.D-6 )

C
C     Still looser time tolerance for point/ellipsoid
C     occultations where the ellipsoid is
C     in back:
C
      DOUBLE PRECISION      TTOL3
      PARAMETER           ( TTOL3  = 175.D-6 )


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

C      INTEGER               NREF
C      PARAMETER           ( NREF = 3 )


      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 50 )

      INTEGER               LBCELL
      PARAMETER           ( LBCELL = -5 )

      INTEGER               MAXWIN
      PARAMETER           ( MAXWIN = 100 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )
C
C     Local variables
C
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(FRNMLN)    AFRAME
      CHARACTER*(NAMLEN)    BACK
      CHARACTER*(FRNMLN)    BFRAME
      CHARACTER*(NAMLEN)    BSHAPE
      CHARACTER*(FRNMLN)    FFRAME
      CHARACTER*(NAMLEN)    FRONT      
      CHARACTER*(NAMLEN)    FSHAPE      
      CHARACTER*(NAMLEN)    OBSRVR
      CHARACTER*(NAMLEN)    OCCTYP
      CHARACTER*(LNSIZE)    TITLE
      CHARACTER*(TIMLEN)    TIMSTR
      CHARACTER*(TIMLEN)    UTC0
      CHARACTER*(TIMLEN)    UTC1

      DOUBLE PRECISION      A
      DOUBLE PRECISION      ARAD   ( 3 )
      DOUBLE PRECISION      ASIZE
      DOUBLE PRECISION      BEG
      DOUBLE PRECISION      BOUNDS ( 2, 2 )
      DOUBLE PRECISION      BRAD   ( 3 )
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CNFINE ( LBCELL : 2*MAXWIN )
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      END
      DOUBLE PRECISION      ET0
      DOUBLE PRECISION      ET1
      DOUBLE PRECISION      FINISH
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LEFT
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      NEWRAD ( 3 )
      DOUBLE PRECISION      PALPHA ( 3 )
      DOUBLE PRECISION      PMCOEF ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      RESULT ( LBCELL : 2*MAXWIN )
      DOUBLE PRECISION      RIGHT
      DOUBLE PRECISION      RSCALE
      DOUBLE PRECISION      START
      DOUBLE PRECISION      STEP
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      XFINSH
      DOUBLE PRECISION      XSTART
      DOUBLE PRECISION      XTIME

      INTEGER               BODYID
      INTEGER               CORSYS
      INTEGER               HAN1
      INTEGER               HAN2
      INTEGER               I
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               SURFID

      LOGICAL               FOUND
      LOGICAL               MAKVTL
      LOGICAL               USEPAD

C
C     Saved everything.
C
      SAVE

C
C     Initial values
C

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_GFOCLT' )

      CALL TCASE ( 'Setup: create and load SPK, PCK, LSK files.' )

      CALL NATSPK ( SPK1, .TRUE., HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTSPK ( SPK2, .TRUE., HAN2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL NATPCK ( PCK,  .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTPCK ( PCK2, .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C     Set up a confinement window. Initialize this and
C     the result window.
C
      UTC0 = '2000 JAN 1  00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

C
C     Create a confinement window with 4 intervals.
C
      DO I = 1, 4

         LEFT  = ET0  + (I-1)*SPD() + 3600.D0
         RIGHT = LEFT +               3600.D0 * 22

         CALL WNINSD ( LEFT, RIGHT, CNFINE )

      END DO




C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Result window too small (detected before search)' )

      CALL SSIZED ( 1, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL GFOCLT ( 'Any',  'ALPHA',      'point',        'ALPHAFIXED',
     .              'BETA', 'ellipsoid',  'BETAFIXED',  'LT',
     .              'SUN',   STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWTOOSMALL)', OK )

      CALL SSIZED ( 2*MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Result window too small (detected during search)' )

      CALL STR2ET ( '2000 JAN 1 TDB', ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL STR2ET ( '2000 JAN 5 TDB', ET1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ', OK )

      STEP = 300.D0

      CALL SSIZED ( 2, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL GFOCLT ( 'Any',   'beta',       'ELLIPSOID',   'betaFIXED',
     .              'alpha', 'ellipsoid',  'ALPHAFIXED',  'NONE',
     .              'SUN',   STEP,         CNFINE,        RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(WINDOWEXCESS)', OK )

      CALL SSIZED ( 2*MAXWIN, RESULT )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized shape for front target' )


      CALL GFOCLT ( 'Any',  'ALPHA',      'pnt',        'ALPHAFIXED',
     .              'BETA', 'ellipsoid',  'BETAFIXED',  'LT',
     .              'SUN',   STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)',   OK )


C
C     Missing "unprioritized" keyword:
C
      CALL GFOCLT ( 'Any',  'ALPHA',      'DSK',        'ALPHAFIXED',
     .              'BETA', 'ellipsoid',  'BETAFIXED',  'LT',
     .              'SUN',   STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized shape for back target' )


      CALL GFOCLT ( 'Any',  'ALPHA',     'point',      'ALPHAFIXED',
     .              'BETA', 'ellipsod',  'BETAFIXED',  'LT',
     .              'SUN',   STEP,        CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)',   OK )


C
C     Missing "unprioritized" keyword:
C
      CALL GFOCLT ( 'Any',  'ALPHA',      'point',        'ALPHAFIXED',
     .              'BETA', 'DSK',        'BETAFIXED',  'LT',
     .              'SUN',   STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )


C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Two point targets' )


      CALL GFOCLT ( 'Any',  'ALPHA',     'point',      'ALPHAFIXED',
     .              'BETA', 'point',     'BETAFIXED',  'LT',
     .              'SUN',   STEP,        CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSHAPECOMBO)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Targets are identical' )


      CALL GFOCLT ( 'Any',   'ALPHA',      'point',      'ALPHAFIXED',
     .              'ALPHA', 'ellipsoid',  'BETAFIXED',  'LT',
     .              'SUN',    STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Front target and observer are identical' )


      CALL GFOCLT ( 'Any',   'ALPHA',      'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid',  'BETAFIXED',  'LT',
     .              'alpha', STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Back target and observer are identical' )


      CALL GFOCLT ( 'Any',   'ALPHA',      'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid',  'BETAFIXED',  'LT',
     .              'beta',   STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Non-positive step size' )

      STEP = 0.D0
      CALL GFOCLT ( 'Any',   'ALPHA',      'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid',  'BETAFIXED',  'LT',
     .              'sun',    STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )

      STEP = -1.D0
      CALL GFOCLT ( 'Any',   'ALPHA',      'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid',  'BETAFIXED',  'LT',
     .              'sun',    STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSTEP)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad occultation type' )

      STEP = 1.D0
      CALL GFOCLT ( 'Ay',    'ALPHA',      'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid',  'BETAFIXED',  'LT',
     .              'sun',    STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOCCTYPE)',   OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Occultation type unsuitable for point target' )

      STEP = 1.D0
      CALL GFOCLT ( 'Annular', 'ALPHA',      'point',      'ALPHAFIXED',
     .              'BETA',    'ellipsoid',  'BETAFIXED',  'LT',
     .              'sun',      STEP,         CNFINE,       RESULT     )

      CALL CHCKXC ( .TRUE., 'SPICE(BADTYPESHAPECOMBO)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Unrecognized targets or observer' )

      STEP = 1.D0
      CALL GFOCLT ( 'Any',   'ALPH',      'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid', 'BETAFIXED',  'LT',
     .              'sun',    STEP,        CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL GFOCLT ( 'Any',   'ALPHA',     'point',      'ALPHAFIXED',
     .              'BETAA', 'ellipsoid', 'BETAFIXED',  'LT',
     .              'sun',    STEP,        CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


      CALL GFOCLT ( 'Any',  'ALPHA',      'point',      'ALPHAFIXED',
     .              'BETA', 'ellipsoid',  'BETAFIXED',  'LT',
     .              'sunN', STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Bad aberration correction values' )

      STEP = 1.D0

      CALL GFOCLT ( 'Any',   'ALPHA',     'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid', 'BETAFIXED',  'S',
     .              'sun',    STEP,        CNFINE,       RESULT       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL GFOCLT ( 'Any',   'ALPHA',     'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid', 'BETAFIXED',  'XS',
     .              'SUN',    STEP,        CNFINE,       RESULT       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL GFOCLT ( 'Any',   'ALPHA',     'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid', 'BETAFIXED',  'RLT',
     .              'sun',   STEP,        CNFINE,       RESULT       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL GFOCLT ( 'Any',   'ALPHA',     'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid', 'BETAFIXED',  'XRLT',
     .              'sun',  STEP,        CNFINE,       RESULT       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL GFOCLT ( 'Any',   'ALPHA',     'point',      'ALPHAFIXED',
     .              'BETA',  'ellipsoid', 'BETAFIXED',  'z',
     .              'sun',   STEP,        CNFINE,       RESULT       )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for observer' )

      STEP = 1.D0

      CALL GFOCLT ( 'Any',    'ALPHA',     'point',      'ALPHAFIXED',
     .              'BETA',   'ellipsoid', 'BETAFIXED',  'LT',
     .              'GASPRA', STEP,        CNFINE,       RESULT       )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for front target' )

      STEP = 1.D0

      CALL GFOCLT ( 'Any',   'GASPRA',     'point',     'ALPHAFIXED',
     .              'BETA',  'ellipsoid', 'BETAFIXED',  'LT',
     .              'SUN',   STEP,        CNFINE,       RESULT       )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for back target' )

      STEP = 1.D0

      CALL GFOCLT ( 'Any',    'ALPHA',     'ellipsoid',  'ALPHAFIXED',
     .              'gaspra', 'point',     'BETAFIXED',  'LT',
     .              'SUN',     STEP,        CNFINE,       RESULT       )
      CALL CHCKXC ( .TRUE.,  'SPICE(SPKINSUFFDATA)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No PCK orientation data for front target; '
     .//           'both targets are ellipsoids.'               )

      STEP = 1.D3

      CALL GFOCLT ( 'Any',   'EARTH',     'ELLIPSOID',  'ITRF93',
     .              'BETA',  'ellipsoid', 'BETAFIXED',  'LT',
     .              'SUN',    STEP,        CNFINE,       RESULT       )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No PCK orientation data for back target; '
     .//           'both targets are ellipsoids.'               )

      STEP = 1.D3


      CALL GFOCLT ( 'Any',   'ALPHA',     'ELLIPSOID', 'ALPHAFIXED',
     .              'EARTH', 'ellipsoid', 'ITRF93',    'LT',
     .              'SUN',    STEP,        CNFINE,     RESULT       )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No PCK orientation data for front target; '
     .//           'front target is ellipsoid; back is point.' )


C
C     Capture Beta's prime meridian data; delete it from
C     the kernel pool.
C
      CALL GDPOOL ( 'BODY2000_PM', 1, 3, N, PMCOEF, FOUND )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DVPOOL ( 'BODY2000_PM' )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     In order to exercise the SINCPT call, we need to use a
C     start time such that the bounding sphere logic is skipped.
C     The specific times used here are dependent on the geometry
C     of Nat's solar system.
C
      CALL SCARDD( 0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( 240.D0-0.001D0, 260.D0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      STEP = 2.D0

      CALL GFOCLT ( 'Any',   'beta' ,     'ellipsoid',   'betafixed',
     .              'alpha', 'point',     'ALPHAFIXED',  'none',
     .              'SUN',  STEP,        CNFINE,        RESULT     )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',  OK )

C
C     Restore the PM data.
C
      CALL PDPOOL ( 'BODY2000_PM', 3, PMCOEF )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No PCK orientation data for back target; '
     .//           'front target is point; back is ellipsoid.' )


C
C     Because the frame ALPHAFIXED is a TK frame defined
C     relative to the PCK frame BETAAFIXED, we can make
C     the SINCPT call, which accesses the ALPHAFIXED frame,
C     fail by damaging the data for the for BETAAFIXED frame.
C
C     This scheme depends on the Nat's solar system PCK
C     implementation.
C
C     Capture Beta's prime meridian data; delete it from
C     the kernel pool.
C
      CALL GDPOOL ( 'BODY2000_PM', 1, 3, N, PMCOEF, FOUND )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DVPOOL ( 'BODY2000_PM' )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     In order to exercise the SINCPT call, we need to use a
C     start time such that the bounding sphere logic is skipped.
C     The specific times used here are dependent on the geometry
C     of Nat's solar system.
C
      CALL SCARDD( 0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( 60.D0-0.001D0, 80.D0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      STEP = 2.D0

      CALL GFOCLT ( 'Any',   'beta' ,     'point',       'betafixed',
     .              'alpha', 'ellipsoid', 'ALPHAFIXED',  'none',
     .              'Sun',   STEP,        CNFINE,        RESULT     )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',  OK )

C
C     Restore the PM data.
C
      CALL PDPOOL ( 'BODY2000_PM', 3, PMCOEF )
      CALL CHCKXC ( .FALSE., ' ',   OK )





C*********************************************************************
C*
C*    Normal cases
C*
C*********************************************************************



C
C---- Case -------------------------------------------------------------
C

C
C     Unload the generic test SPK, so we don't have interference from
C     the sun ephemeris.
C
      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     All expected event times are based on Nat's solar system.
C
      CALL TCASE ( 'Any occultation of ALPHA by BETA, abcorr = NONE' )

      UTC0 = '2000 JAN 1  00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

C
C     Create a confinement window with 4 intervals.
C
      DO I = 1, 4

         LEFT  = ET0  + (I-1)*SPD() + 3600.D0
         RIGHT = LEFT +               3600.D0 * 22

         CALL WNINSD ( LEFT, RIGHT, CNFINE )

      END DO



      STEP = 60.D0

      CALL GFOCLT ( 'Any',  'BETA',       'ellipsoid',  'BETAFIXED',
     .              'ALPHA', 'ellipsoid', 'ALPHAFIXED', 'NONE',
     .              'SUN',   STEP,        CNFINE,       RESULT     )

      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )


      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD()
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 10*60.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Any occultation of ALPHA by BETA, abcorr = LT+S' )

C
C     Note: the stellar aberration correction spec should be ignored.
C
      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Any',   'BETA',       'ellipsoid',  'BETAFIXED',
     .              'ALPHA', 'ellipsoid',  'ALPHAFIXED', 'LT+S',
     .              'SUN',   STEP,         CNFINE,       RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )

         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )
C
C        Check occultation start time.
C
         XTIME = DBLE(I-1)*SPD() + 1.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 10*60.D0 + 1.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Annular transit of ALPHA by BETA, abcorr = NONE' )

      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'ANNULAR',
     .              'BETA',  'ellipsoid',  'BETAFIXED',
     .              'ALPHA', 'ellipsoid',  'ALPHAFIXED', 'NONE',
     .              'SUN',   STEP,         CNFINE,       RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )

C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 2*60.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 8*60.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO





C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Annular transit of ALPHA by BETA, abcorr = CN' )

      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'ANNULAR',
     .              'BETA',  'ellipsoid',  'BETAFIXED',
     .              'ALPHA', 'ellipsoid',  'ALPHAFIXED', 'CN',
     .              'SUN',    STEP,         CNFINE,       RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)


         CALL WNFETD ( RESULT, I, LEFT, RIGHT )

C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 2*60.D0 + 1.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 8*60.D0 + 1.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO


C
C---- Case -------------------------------------------------------------
C

C
C     All expected events time are based on Nat's solar system.
C
      CALL TCASE ( 'Partial occultation of ALPHA by BETA, '
     .//           'abcorr = NONE' )


      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Partial',
     .              'BETA',  'ellipsoid',  'BETAFIXED',
     .              'ALPHA', 'ellipsoid',  'ALPHAFIXED', 'NONE',
     .              'SUN',   STEP,         CNFINE,       RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 8, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )

         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )
C
C        Check occultation start time.
C
         IF ( ODD(I) ) THEN
            XTIME = DBLE( (I+1)/2 -1)*SPD()
         ELSE
            XTIME = DBLE(  I/2    -1)*SPD() + 8*60.D0
         END IF

         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         IF ( ODD(I) ) THEN
            XTIME = DBLE( (I+1)/2 -1)*SPD() + 2*60.D0
         ELSE
            XTIME = DBLE(  I/2    -1)*SPD() + 10*60.D0
         END IF

         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO



C
C---- Case -------------------------------------------------------------
C

C
C     All expected events time are based on Nat's solar system.
C
      CALL TCASE ( 'Partial occultation of ALPHA by BETA, '
     .//           'abcorr = XLT' )


      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Partial',
     .              'BETA',  'ellipsoid',  'BETAFIXED',
     .              'ALPHA', 'ellipsoid',  'ALPHAFIXED', 'XLT',
     .              'SUN',   STEP,         CNFINE,       RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 8, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )

         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )
C
C        Check occultation start time.
C
         IF ( ODD(I) ) THEN
            XTIME = DBLE( (I+1)/2 -1)*SPD()  -  1.D0
         ELSE
            XTIME = DBLE(  I/2    -1)*SPD() + 8*60.D0  -  1.D0
         END IF

         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         IF ( ODD(I) ) THEN
            XTIME = DBLE( (I+1)/2 -1)*SPD() + 2*60.D0  -  1.D0
         ELSE
            XTIME = DBLE(  I/2    -1)*SPD() + 10*60.D0  -  1.D0
         END IF

         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO






C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Full occultation of ALPHA by BETA, '
     .//           'abcorr = NONE. No events expected.' )


      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Full',
     .              'BETA',  'ellipsoid',  'BETAFIXED',
     .              'ALPHA', 'ellipsoid',  'ALPHAFIXED', 'NONE',
     .              'SUN',   STEP,         CNFINE,       RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     There should be no full occultations.
C
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 0, 0,
     .              OK                                              )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Full occultation, '
     .//           'abcorr = NONE. ALPHA re-sized to 1/2 '
     .//           'angular size of BETA.' )


      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Look up the radii of alpha.
C
      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ARAD )

C
C     Compute the scale factor needed to reduce the
C     angular radius in the x-y plane of ALPHA to 1/8 its original
C     size. Recall that alpha's x-axis is parallel to the J2000 z-axis,
C     the +y-axis points toward the Sun at noon, and Alpha's +z axis
C     points in the J2000 -y direction at noon. So Alpha's
C     angular size in the x-y plane is controlled by the lengths
C     of its y and z axes, which are equal.
C
      CALL SPKPOS ( 'ALPHA', ET0, 'J2000', 'NONE', 'SUN', PALPHA, LT )

      RANGE     = VNORM(PALPHA)

      ASIZE     = ASIN ( ARAD(2)/RANGE )

      NEWRAD(2) = SIN ( ASIZE/8.D0 ) * RANGE

      RSCALE    = NEWRAD(2) / ARAD(2)

C
C     Produce the new radii of ALPHA and store these
C     in the kernel pool.
C
      CALL VSCL   ( RSCALE, ARAD, NEWRAD )

C
C     Because of alpha's large vertical dimension, we must shrink
C     this dimension even farther to make total occultation possible.
C     We shrink Alpha's x-axis dimension (which is aligned with the
C     J2000 z-axis) by a factor of 4, making the total shrinkage
C     factor in this dimension equal to 32.
C
      NEWRAD(1) = NEWRAD(1) / 4

      CALL PDPOOL ( 'BODY1000_RADII', 3, NEWRAD )

C
C     Search for full occultations using the modified
C     version of ALPHA.
C
      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Full',  'BETA',       'ellipsoid',   'BETAFIXED',
     .              'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'NONE',
     .              'SUN',   STEP,         CNFINE,        RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 4.5D0 * 60.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 5.5D0*60.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO

C
C     Restore original radii of ALPHA.
C
      CALL PDPOOL ( 'BODY1000_RADII', 3, ARAD )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Full occultation, '
     .//           'abcorr = LT. ALPHA re-sized to 1/2 '
     .//           'angular size of BETA.' )


      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Look up the radii of alpha.
C
      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ARAD )

C
C     Compute the scale factor needed to reduce the
C     angular radius of ALPHA to 1/8 its original size.
C
      CALL SPKPOS ( 'ALPHA', ET0, 'J2000', 'NONE', 'SUN', PALPHA, LT )

      RANGE     = VNORM(PALPHA)

      ASIZE     = ASIN ( ARAD(2)/RANGE )

      NEWRAD(2) = SIN ( ASIZE/8.D0 ) * RANGE

      RSCALE    = NEWRAD(2) / ARAD(2)

C
C     Produce the new radii of ALPHA and store these
C     in the kernel pool.
C
      CALL VSCL   ( RSCALE, ARAD, NEWRAD )

C
C     Because of alpha's large vertical dimension, we must shrink
C     this dimension even farther to make total occultation possible.
C     We shrink Alpha's x-axis dimension (which is aligned with the
C     J2000 z-axis) by a factor of 4, making the total shrinkage
C     factor in this dimension equal to 32.
C
      NEWRAD(1) = NEWRAD(1) / 4

      CALL PDPOOL ( 'BODY1000_RADII', 3, NEWRAD )

C
C     Search for full occultations using the modified
C     version of ALPHA.
C
      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Full',  'BETA',       'ellipsoid',   'BETAFIXED',
     .              'ALPHA', 'ellipsoid',  'ALPHAFIXED',  'LT',
     .              'SUN',   STEP,         CNFINE,        RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 4.5D0 * 60.D0  + 1.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 5.5D0*60.D0 + 1.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO

C
C     Restore original radii of ALPHA.
C
      CALL PDPOOL ( 'BODY1000_RADII', 3, ARAD )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Occultation of ALPHA center by BETA, '
     .//           'abcorr = NONE.' )


      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Any',   'BETA',  'ellipsoid',  'BETAFIXED',
     .              'ALPHA', 'point', ' ',          'NONE',
     .              'SUN',   STEP,    CNFINE,       RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 4*60.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 6*60.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO


C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Occultation of ALPHA center by BETA, '
     .//           'abcorr = NONE, radii of ALPHA set to 0' )


      CALL BODVRD ( 'ALPHA', 'RADII', 3, N, ARAD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CLEARD ( 3, NEWRAD )

      CALL PDPOOL ( 'BODY1000_RADII', 3, NEWRAD )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Any',   'BETA',  'ellipsoid',  'BETAFIXED',
     .              'ALPHA', 'point', ' ',          'NONE',
     .              'SUN',   STEP,    CNFINE,       RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 4*60.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 6*60.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO

C
C     Restore the original radii.
C
      CALL PDPOOL ( 'BODY1000_RADII', 3, ARAD )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Occultation of ALPHA center by BETA, '
     .//           'abcorr = LT.' )

C
C     Note that we use a looser time tolerance for these events,
C     since the light time correction for the ellipsoid is not
C     based on distance from the observer to the ellipsoid's center,
C     but rather to the intercept point, which is closer to
C     the observer.
C
      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Any',   'BETA',    'ellipsoid',   'BETAFIXED',
     .              'ALPHA', 'point',   ' ',           'LT',
     .              'SUN',   STEP,      CNFINE,        RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 4*60.D0 + 1.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TTOL2, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 6*60.D0 + 1.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TTOL2, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Transit of BETA center across ALPHA, '
     .//           'abcorr = NONE.' )


      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Any',   'BETA',      'POINT',      ' ',
     .              'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', 'NONE',
     .              'SUN',   STEP,        CNFINE,        RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 1*60.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 9*60.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Transit of BETA center across ALPHA, '
     .//           'abcorr = NONE, BETA raii set to 0' )


      CALL BODVRD ( 'BETA', 'RADII', 3, N, BRAD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CLEARD ( 3, NEWRAD )

      CALL PDPOOL ( 'BODY2000_RADII', 3, NEWRAD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Any',   'BETA',      'POINT',      ' ',
     .              'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', 'NONE',
     .              'SUN',   STEP,        CNFINE,        RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 1*60.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TIMTOL, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 9*60.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TIMTOL, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO


      CALL PDPOOL ( 'BODY2000_RADII', 3, BRAD )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Transit of BETA center across ALPHA, '
     .//           'abcorr = LT.' )


      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Any',   'BETA',      'POINT',      'BETAFIXED',
     .              'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', 'LT',
     .              'SUN',    STEP,        CNFINE,       RESULT   )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 4, 0,
     .              OK                                              )

      DO I = 1, WNCARD(RESULT)

         CALL WNFETD ( RESULT, I, LEFT, RIGHT )
C
C        Check occultation start time.
C
         TITLE = 'Occ # beg time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 1*60.D0 + 1.D0
         CALL CHCKSD ( TITLE, LEFT, '~', XTIME, TTOL3, OK )

C
C        Check occultation end time.
C
         TITLE = 'Occ # end time'
         CALL REPMI ( TITLE, '#', I, TITLE )

         XTIME = DBLE(I-1)*SPD() + 9*60.D0  + 1.D0
         CALL CHCKSD ( TITLE, RIGHT, '~', XTIME, TTOL3, OK )

         CALL TIMOUT ( LEFT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )

C         WRITE (*,*) I, ' ',  TIMSTR
         CALL TIMOUT ( RIGHT,
     .                 'YYYY MON DD HR:MN:SC.###### ::TDB (TDB)',
     .                 TIMSTR )
C         WRITE (*,*) '  ', TIMSTR

      END DO




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Any occultation of BETA by ALPHA, abcorr = LT+S' )
C
C     Since Alpha can't occult Beta as seen from the Sun, we
C     should produce an empty result window.
C

C
C     Note: the stellar aberration correction spec should be ignored.
C
      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Any',  'ALPHA',      'ellipsoid',  'ALPHAFIXED',
     .              'BETA', 'ellipsoid',  'BETAFIXED',  'LT+S',
     .              'SUN',  STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 0, 0,
     .              OK                                              )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Occultation of BETA center by ALPHA, abcorr = LT' )
C
C     Since Alpha can't occult Beta as seen from the Sun, we
C     should produce an empty result window.
C

C
C     Note: the stellar aberration correction spec should be ignored.
C
      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Any',  'ALPHA',      'ellipsoid',  'ALPHAFIXED',
     .              'BETA', 'point',      'BETAFIXED',  'LT',
     .              'SUN',  STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 0, 0,
     .              OK                                              )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Occultation of BETA by ALPHA center, abcorr = LT' )
C
C     Since Alpha can't occult Beta as seen from the Sun, we
C     should produce an empty result window.
C

C
C     Note: the stellar aberration correction spec should be ignored.
C
      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SSIZED ( 2*MAXWIN, CNFINE )
      CALL SSIZED ( 2*MAXWIN, RESULT )

      CALL WNINSD ( ET0, ET1, CNFINE )


      STEP = 60.D0

      CALL GFOCLT ( 'Any',  'ALPHA',      'point',      'ALPHAFIXED',
     .              'BETA', 'ellipsoid',  'BETAFIXED',  'LT',
     .              'SUN',  STEP,         CNFINE,       RESULT       )

      CALL CHCKXC ( .FALSE., ' ',   OK )
      CALL CHCKSI ( 'RESULT cardinality', WNCARD(RESULT), '=', 0, 0,
     .              OK                                              )



 
C
C     Case
C
      TITLE = 'Check the GF call uses the GFSTOL value'
      CALL TCASE( TITLE )


C
C     Re-run a valid search after using GFSTOL to set the convergence
C     tolerance to a value that should cause a numerical error signal.
C

      UTC0 = '2000 JAN 1  00:00:00 TDB'
      UTC1 = '2000 JAN 05 00:00:00 TDB'

      CALL STR2ET ( UTC0, ET0 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL STR2ET ( UTC1, ET1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      STEP = 60.D0

      CALL GFOCLT ( 'Any',   'BETA',      'POINT',      'BETAFIXED',
     .              'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', 'LT',
     .              'SUN',    STEP,        CNFINE,       RESULT   )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      N = 0
      N = WNCARD(RESULT)
      CALL CHCKSI ( 'N', N, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, BEG, END  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SCARDD ( 0, RESULT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Reset tol.
C

      CALL GFSTOL ( 1.D-4 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL GFOCLT ( 'Any',   'BETA',      'POINT',      'BETAFIXED',
     .              'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', 'LT',
     .              'SUN',    STEP,        CNFINE,       RESULT   )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      N = 0
      N = WNCARD(RESULT)
      CALL CHCKSI ( 'N', N, '!=', 0, 0, OK )

      CALL WNFETD ( RESULT, 1, LEFT, RIGHT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     The values in the time window should not match
C     as the search used different tolerances. Check
C     the first value in the first interval.
C
      CALL CHCKSD ( TITLE, BEG, '!=', LEFT, 0.D0, OK )




C
C     Reset the convergence tolerance.
C
      CALL GFSTOL ( CNVTOL )






C***********************************************************************
C
C     DSK tests
C
C***********************************************************************


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Create a DSK containing shape models for '
     .//           'bodies Alpha and Beta.'                    )


C
C     Make sure the generic SPK is unloaded! We need the sun ephemeris
C     from Nat's SPK.
C
      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C
C     This block is suitable for use in F_GFOCLT. We don't actually
C     have to have the high-resolution DSK patches to test OCCULT.
C
C     We'll enhance the generic DSK models for Alpha and Beta by
C     appending segments containing small patches of high-resolution
C     data for the surface regions that participate in computation of
C     accurate occultation ingress and egress times. We use small
C     patches because the size and run time needed to create full
C     models at this resolution would be prohibitive.
C
C     Start by creating the basic DSK.
C
      IF ( EXISTS(DSK) ) THEN

         CALL UNLOAD ( DSK )
         CALL DELFIL ( DSK )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END IF

C
C     Use low resolution tessellations for the main models.
C
      NLON   = 20
      NLAT   = 10

      AFRAME = 'ALPHAFIXED'
      BFRAME = 'BETAFIXED'

      CALL NATDSK ( DSK, AFRAME, NLON, NLAT,
     .                   BFRAME, NLON, NLAT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Create high-resolution patches for Alpha. At the J2000 epoch,
C     the ALPHAFIXED +X axis is aligned with the J2000 +Z axis. The
C     ALPHAFIXED +Y axis is aligned with the J2000 +Y axis.
C
C
C     Create the -Y patch for body Alpha. The patch covers
C     the lon/lat rectangle
C
C        -92 deg. <= lon <= -88 deg.
C          0 deg. <= lat <=   4 deg.
C
C     Note that Alpha' body-fixed Z axis lies in Alpha's orbital
C     plane.
C
      BODYID = 1000
      SURFID = 2
      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()

      CALL BODVCD ( BODYID, 'RADII', 3, N, ARAD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Make the patch spherical, using the ellipsoid's Z
C     semi-axis length.
C
      C = ARAD(3) 
     
      CORSYS = LATSYS
      CALL CLEARD ( NSYPAR, CORPAR )

      MAKVTL = .FALSE.

      BOUNDS(1,1) =  RPD() * (-92.D0)
      BOUNDS(2,1) =  RPD() * (-88.D0)
      BOUNDS(1,2) =  RPD() * (  0.D0)
      BOUNDS(2,2) =  RPD() * (  4.D0)

      NLAT   = 200
      NLON   = 200
      
      USEPAD = .TRUE.

C
C     Append the patch segment to the existing DSK.
C
      CALL T_SECDS2 ( BODYID, SURFID, AFRAME, FIRST,  LAST,
     .                CORSYS, CORPAR, BOUNDS, C,      C,
     .                C,      NLON,   NLAT,   MAKVTL, USEPAD,
     .                DSK                                    )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Create the +Y patch for body Alpha. The patch covers
C     the lon/lat rectangle
C
C         88 deg. <= lon <=  92 deg.
C          0 deg. <= lat <=   4 deg.
C

      SURFID = 3
 
      BOUNDS(1,1) =  RPD() * ( 88.D0)
      BOUNDS(2,1) =  RPD() * ( 92.D0)
      BOUNDS(1,2) =  RPD() * (  0.D0)
      BOUNDS(2,2) =  RPD() * (  4.D0)

      NLAT   = 200
      NLON   = 200
      
      USEPAD = .TRUE.

C
C     Append the patch segment to the existing DSK.
C
      CALL T_SECDS2 ( BODYID, SURFID, AFRAME,  FIRST,  LAST,
     .                CORSYS, CORPAR, BOUNDS, C,      C,
     .                C,      NLON,   NLAT,   MAKVTL, USEPAD,
     .                DSK                                    )
      CALL CHCKXC ( .FALSE., ' ',   OK )
       

C
C     Create high-resolution patches for Beta. At the J2000 epoch,
C     the BETAFIXED +X axis is aligned with the J2000 +Y axis. The
C     BETAFIXED +Y axis is aligned with the J2000 -X axis.
C
      BODYID = 2000
      SURFID = 2
      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()

      CALL BODVCD ( BODYID, 'RADII', 3, N, BRAD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Make the patch spherical, using the ellipsoid's X
C     semi-axis length.
C
      A      = BRAD(1) 
     
      CORSYS = LATSYS
      CALL CLEARD ( NSYPAR, CORPAR )

      MAKVTL = .FALSE.

      BOUNDS(1,1) =  RPD() * (  0.D0)
      BOUNDS(2,1) =  RPD() * (  4.D0)
      BOUNDS(1,2) =  RPD() * ( -2.D0)
      BOUNDS(2,2) =  RPD() * (  2.D0)

      NLAT   = 200
      NLON   = 200
      
      USEPAD = .TRUE.

      CALL T_SECDS2 ( BODYID, SURFID, BFRAME, FIRST,  LAST,
     .                CORSYS, CORPAR, BOUNDS, A,      A,
     .                A,      NLON,   NLAT,   MAKVTL, USEPAD,
     .                DSK                                    )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      BOUNDS(1,1) =  RPD() * ( 178.D0)
      BOUNDS(2,1) =  RPD() * (-178.D0)
      BOUNDS(1,2) =  RPD() * (  -2.D0)
      BOUNDS(2,2) =  RPD() * (   2.D0)

      CALL T_SECDS2 ( BODYID, SURFID, BFRAME, FIRST,  LAST,
     .                CORSYS, CORPAR, BOUNDS, A,      A,
     .                A,      NLON,   NLAT,   MAKVTL, USEPAD,
     .                DSK                                    )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C     Load the DSK for subsequent use.
C
      CALL FURNSH ( DSK )
      CALL CHCKXC ( .FALSE., ' ',   OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find transit of point BETA across DSK ALPHA '
     .//           'near the epoch '
     .//           '2000 JAN 1 12:05:00 TDB'                          )



      CALL STR2ET ( '2000 JAN 1 11:00:00 TDB', ET0 )
      CALL STR2ET ( '2000 JAN 1 13:00:00 TDB', ET1 )
      
      CALL SCARDD ( 0, CNFINE )
      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      

      OCCTYP = 'ANY'

      OBSRVR = 'SUN'
      ABCORR = 'NONE'

      FRONT  = 'BETA'
      FSHAPE = 'POINT'
      FFRAME = ' '
      BACK   = 'ALPHA'
      BFRAME = 'ALPHAFIXED'
      BSHAPE = 'DSK/UNPRIORITIZED'


      CALL GFOCLT ( OCCTYP,  FRONT,   FSHAPE,  FFRAME,
     .              BACK,    BSHAPE,  BFRAME,  ABCORR,
     .              OBSRVR,  STEP,    CNFINE,  RESULT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      N = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      START  = RESULT(1)
      FINISH = RESULT(2)

      XSTART =     60.D0
      XFINSH = 9 * 60.D0

C
C     On cayenne (PC/Linux/gfortran 64-bit), this tolerance may be
C     reduced to 1.D-6.
C
      TOL    = 3.D-6

      CALL CHCKSD ( 'START',  START,  '~', XSTART, TOL, OK )
      CALL CHCKSD ( 'FINISH', FINISH, '~', XFINSH, TOL, OK )




C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Find occultation by DSK BETA of point ALPHA '
     .//           'near the epoch '
     .//           '2000 JAN 1 12:05:00 TDB'                          )


      CALL STR2ET ( '2000 JAN 1 11:00:00 TDB', ET0 )
      CALL STR2ET ( '2000 JAN 1 13:00:00 TDB', ET1 )
      
      CALL SCARDD ( 0, CNFINE )
      CALL WNINSD ( ET0, ET1, CNFINE )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      

      OCCTYP = 'ANY'

      OBSRVR = 'SUN'
      ABCORR = 'NONE'

      FRONT  = 'BETA'
      FSHAPE = 'DSK/UNPRIORITIZED'
      FFRAME = 'BETAFIXED'
      BACK   = 'ALPHA'
      BFRAME = ' '
      BSHAPE = 'POINT'


      CALL GFOCLT ( OCCTYP,  FRONT,   FSHAPE,  FFRAME,
     .              BACK,    BSHAPE,  BFRAME,  ABCORR,
     .              OBSRVR,  STEP,    CNFINE,  RESULT  )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      N = WNCARD( RESULT )

      CALL CHCKSI ( 'N', N, '=', 1, 0, OK )

      START  = RESULT(1)
      FINISH = RESULT(2)

      XSTART = 4 * 60.D0
      XFINSH = 6 * 60.D0

C
C     On cayenne (PC/Linux/gfortran 64-bit), this tolerance may be
C     reduced to 1.D-6.
C
      TOL    = 3.D-6

      CALL CHCKSD ( 'START',  START,  '~', XSTART, TOL, OK )
      CALL CHCKSD ( 'FINISH', FINISH, '~', XFINSH, TOL, OK )




C
C---- Case -------------------------------------------------------------
C

      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

C
C     Clean up SPK and DSK files.
C      
      CALL SPKUEF ( HAN1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( SPK1 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( SPK2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )


      CALL UNLOAD ( DSK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL DELFIL ( DSK )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL T_SUCCESS ( OK )

      RETURN
      END
