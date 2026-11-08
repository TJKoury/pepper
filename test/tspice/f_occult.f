C$Procedure      F_OCCULT ( OCCULT family tests )

      SUBROUTINE F_OCCULT ( OK )

C$ Abstract
C
C     This routine tests the SPICELIB routine
C     
C        OCCULT
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

      INCLUDE 'dskdsc.inc'
      INCLUDE 'occult.inc'

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
C     geometry routine OCCULT.
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
C     S.C. Krening     (JPL)
C     N.J. Bachman     (JPL)
C
C$ Version
C
C-    TSPICE Version 2.0.0, 23-MAY-2016 (SCK) (NJB)
C
C        Updated to test support for DSK-based target
C        shape models.
C
C-    TSPICE Version 1.0.0, 17-JAN-2012 (SCK) (NJB)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      JYEAR

      LOGICAL               EXISTS

C     
C     Local parameters
C
      CHARACTER*(*)         DSK
      PARAMETER           ( DSK  = 'occult_nat.bds' )

      CHARACTER*(*)         PCK 
      PARAMETER           ( PCK  = 'nat.tpc' )

      CHARACTER*(*)         PCK2 
      PARAMETER           ( PCK2 = 'generic.tpc' )

      CHARACTER*(*)         SPK1
      PARAMETER           ( SPK1 = 'nat.bsp' )

      CHARACTER*(*)         SPK2
      PARAMETER           ( SPK2 = 'generic.bsp' )

      INTEGER               TIMSIZ
      PARAMETER           ( TIMSIZ = 20 )

      INTEGER               NUMTIM
      PARAMETER           ( NUMTIM = 5 )

C
C     Local variables
C
      CHARACTER*(TIMSIZ)    ABCORR
      CHARACTER*(TIMSIZ)    AFRAME
      CHARACTER*(TIMSIZ)    BFRAME
      CHARACTER*(TIMSIZ)    FRAME
      CHARACTER*(TIMSIZ)    FRAME1
      CHARACTER*(TIMSIZ)    FRAME2
      CHARACTER*(TIMSIZ)    OBSRVR
      CHARACTER*(TIMSIZ)    SHAPE1
      CHARACTER*(TIMSIZ)    SHAPE2
      CHARACTER*(TIMSIZ)    TARG1
      CHARACTER*(TIMSIZ)    TARG2
      CHARACTER*(TIMSIZ)    TIMES ( NUMTIM )

      DOUBLE PRECISION      ARAD   ( 3 )
      DOUBLE PRECISION      C
      DOUBLE PRECISION      CORPAR ( NSYPAR )
      DOUBLE PRECISION      BOUNDS ( 2, 2 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      PMCOEF ( 3 )
      DOUBLE PRECISION      POS    ( 3 )
      DOUBLE PRECISION      LAST
      DOUBLE PRECISION      LT

      INTEGER               BODYID
      INTEGER               CORSYS
      INTEGER               HAN1
      INTEGER               HAN2
      INTEGER               I
      INTEGER               J
      INTEGER               MLTFAC
      INTEGER               N
      INTEGER               NLAT
      INTEGER               NLON
      INTEGER               OCLTID
      INTEGER               RESULT ( NUMTIM )
      INTEGER               SURFID

      LOGICAL               FOUND
      LOGICAL               MAKVTL
      LOGICAL               USEPAD

C
C     Save variables
C
      SAVE                  TIMES
      SAVE                  RESULT

C
C     Initialize
C
      DATA TIMES ( 1 ) / '2011-JAN-02 19:00:00' /
      DATA TIMES ( 2 ) / '2011-JAN-02 21:00:00' /
      DATA TIMES ( 3 ) / '2011-JAN-03 00:00:00' /
      DATA TIMES ( 4 ) / '2011-JAN-03 09:00:00' /
      DATA TIMES ( 5 ) / '2011-JAN-03 11:00:00' /

      DATA RESULT / 0, -1, -3, 1, 2 /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_OCCULT' )

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

      CALL STR2ET (  '2000-JAN-01 00:00:00 (TDB)', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


C*********************************************************************
C*
C*    Error cases
C*
C*********************************************************************


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Invalid first shape' )

      CALL OCCULT ( 'ALPHA', 'PNT',   'ALPHAFIXED',
     .              'BETA',  'POINT', 'BETAFIXED',
     .              'NONE',  'SUN',    ET, OCLTID  )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


C
C     Missing "unprioritized" keyword:
C
      CALL OCCULT ( 'ALPHA', 'DSK',   'ALPHAFIXED',
     .              'BETA',  'POINT', 'BETAFIXED',
     .              'NONE',  'SUN',    ET, OCLTID  )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )



C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Invalid second shape' )

      CALL OCCULT ( 'ALPHA', 'POINT', 'ALPHAFIXED',
     .              'BETA',  'PNT',   'BETAFIXED', 
     .              'NONE',  'SUN',    ET, OCLTID   )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )

C
C     Missing "unprioritized" keyword:
C
      CALL OCCULT ( 'ALPHA', 'POINT', 'ALPHAFIXED',
     .              'BETA',  'DSK',   'BETAFIXED', 
     .              'NONE',  'SUN',    ET, OCLTID   )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Verify both targets cannot be points' )

      CALL OCCULT ( 'ALPHA', 'POINT', 'ALPHAFIXED', 
     .              'BETA',  'POINT', 'BETAFIXED', 
     .              'NONE',  'SUN',    ET, OCLTID  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSHAPECOMBO)', OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Verify both targets cannot be identical' )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', 
     .              'ALPHA', 'ELLIPSOID', 'BETAFIXED', 
     .              'NONE',  'SUN',        ET, OCLTID  )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Verify the first target and observer cannot '
     .        //   'be identical.' )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED',
     .              'BETA',  'ELLIPSOID', 'BETAFIXED', 
     .              'NONE',  'ALPHA',      ET,  OCLTID )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Verify the second target and observer cannot '
     .        //   'be identical.' )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', 
     .              'BETA',  'ELLIPSOID', 'BETAFIXED',
     .              'NONE',  'BETA',       ET,  OCLTID )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

C
C---- Case -------------------------------------------------------------
C
C     Note:  This is a test for three cases:
C     1)  If an unrecognized target is elliptical and the frame
C         cannot be found.
C     2)  If an unrecognized target is a point and the ID code is
C         not found in the call tree of ZZGFOCIN.
C     3)  If an observer is unrecognized.
C
      CALL TCASE ( 'Verify an error if the targets or observer are '
     .        //   'unrecognized' )

      CALL OCCULT ( 'ALPH', 'ellipsoid', 'ALPHAFIXED',
     .              'BETA', 'ELLIPSOID', 'BETAFIXED', 
     .              'NONE', 'SUN', ET,    OCLTID )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED',
     .              'BTA',   'ELLIPSOID', 'BETAFIXED', 
     .              'NONE',  'SUN', ET,    OCLTID )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED',
     .              'BETA',  'ELLIPSOID', 'BETAFIXED',  
     .              'NONE',  'SUNN', ET,   OCLTID )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

C
C
C---- Case -------------------------------------------------------------
C 

      CALL TCASE ( 'Verify aberration correction inputs' )
 
      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED',
     .              'BETA',  'ELLIPSOID', 'BETAFIXED',
     .              'S',     'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )
 
      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED',
     .              'BETA',  'ELLIPSOID', 'BETAFIXED',
     .              'XS',    'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )
 
      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED',
     .              'BETA',  'ELLIPSOID', 'BETAFIXED',
     .              'RLT',   'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )
 
      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED',
     .              'BETA',  'ELLIPSOID', 'BETAFIXED',
     .              'XRLT',  'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )
 
      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED',
     .              'BETA',  'ELLIPSOID', 'BETAFIXED',
     .              'z',     'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

C
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for observer' )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED',
     .              'BETA',  'ELLIPSOID', 'BETAFIXED',
     .              'LT',    'GASPRA', ET, OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for first target' )

      CALL OCCULT ( 'GASPRA', 'POINT',     'IAU_GASPRA',
     .              'BETA',   'ELLIPSOID', 'BETAFIXED',
     .              'LT',     'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No SPK data for second target' )

      CALL OCCULT ( 'ALPHA',  'ELLIPSOID', 'ALPHAFIXED',
     .              'GASPRA', 'POINT',     'IAU_GASPRA',
     .              'LT',     'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No PCK orientation data for first target; '
     .//           'both targets are ellipsoids.'               )

      CALL OCCULT ( 'EARTH', 'ellipsoid', 'ITRF93', 
     .              'ALPHA', 'ELLIPSOID', 'ALPHAFIXED',
     .              'LT',    'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'No PCK orientation data for second target; '
     .//           'both targets are ellipsoids.'               )

      CALL OCCULT ( 'ALPHA', 'ELLIPSOID', 'ALPHAFIXED',
     .              'EARTH', 'ellipsoid', 'ITRF93', 
     .              'LT',    'SUN', ET,    OCLTID )
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

      CALL OCCULT ( 'BETA',  'ELLIPSOID', 'BETAFIXED',
     .              'ALPHA', 'point',     ' ', 
     .              'NONE',  'SUN', 250.D0,    OCLTID )
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
C     fail by damaging the data for the for BETAFIXED frame.
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

      CALL OCCULT ( 'BETA',  'point', ' ',
     .              'ALPHA', 'ellipsoid', 'alphafixed', 
     .              'NONE',  'SUN', 62.D0,    OCLTID )
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
C     When TARG1 is GAMMA (abcorr = none):
C
C 2000-JAN-02 19 GAMMA  not occulted by        ALPHA  as seen by SUN   0
C 2000-JAN-02 21 GAMMA  partially occulted by  ALPHA  as seen by SUN  -1
C 2000-JAN-03 00 GAMMA  totally occulted by    ALPHA  as seen by SUN  -3
C 2000-JAN-03 07 GAMMA  not occulted by        ALPHA  as seen by SUN   0
C 2000-JAN-03 09 ALPHA  partially occulted by  GAMMA  as seen by SUN   1
C 2000-JAN-03 11 ALPHA  transited by           GAMMA  as seen by SUN   2
C 2000-JAN-03 16 GAMMA  not occulted by        ALPHA  as seen by SUN   0
C
C     When TARG1 is ALPHA (abcorr = none):
C
C 2000-JAN-02 19 ALPHA not occulted by         GAMMA  as seen by SUN   0
C 2000-JAN-02 21 GAMMA partially occulted by   ALPHA  as seen by SUN   1
C 2000-JAN-03 00 GAMMA totally occulted by     ALPHA  as seen by SUN   3
C 2000-JAN-03 07 ALPHA not occulted by         GAMMA  as seen by SUN   0
C 2000-JAN-03 09 ALPHA partially occulted by   GAMMA  as seen by SUN  -1
C 2000-JAN-03 11 ALPHA transited by            GAMMA  as seen by SUN  -2
C 2000-JAN-03 16 ALPHA not occulted by         GAMMA  as seen by SUN   0
C
C     From GFOCLT, Front (alpha), back (gamma), abcorr (none), 
C                  occultation type (any)
C
C                  Interval            1
C                     Start time: 2000-JAN-02 20:41:15
C                     Stop time:  2000-JAN-03 03:51:58
C
C     From GFOCLT, Front (alpha), back (gamma), abcorr (none), 
C                  occultation type (partial)
C
C                  Interval            1
C                     Start time: 2000-JAN-02 20:41:15
C                     Stop time:  2000-JAN-02 22:52:53
C                  Interval            2
C                     Start time: 2000-JAN-03 02:28:28
C                     Stop time:  2000-JAN-03 03:51:58
C
C     From GFOCLT, Front (alpha), back (gamma), abcorr (none), 
C                  occultation type (annular)
C
C                  No occultation was found.
C
C     From GFOCLT, Front (alpha), back (gamma), abcorr (none), 
C                  occultation type (full, total)
C
C                  Interval            1
C                     Start time: 2000-JAN-02 22:52:53
C                     Stop time:  2000-JAN-03 02:28:28
C
C     From GFOCLT, Front (gamma), back (alpha), abcorr (none), 
C                  occultation type (any)
C
C                  Interval            1
C                     Start time: 2000-JAN-03 08:24:46
C                     Stop time:  2000-JAN-03 14:57:23
C
C     From GFOCLT, Front (gamma), back (alpha), abcorr (none), 
C                  occultation type (partial)
C
C                  Interval            1
C                     Start time: 2000-JAN-03 08:24:46
C                     Stop time:  2000-JAN-03 09:58:08
C                  Interval            2
C                     Start time: 2000-JAN-03 12:34:35
C                     Stop time:  2000-JAN-03 14:57:23
C
C     From GFOCLT, Front (gamma), back (alpha), abcorr (none), 
C                  occultation type (annular)
C
C                  Interval            1
C                     Start time: 2000-JAN-03 09:58:08
C                     Stop time:  2000-JAN-03 12:34:35
C
C     From GFOCLT, Front (gamma), back (alpha), abcorr (none), 
C                  occultation type (full, total)
C
C                  No occultation was found.
C
C           -   -   -   -   -   -   -   -   -   -   -   -   
C     From GFOCLT, Front (gamma), back (alpha), abcorr (xcn),
C                  occultation type (any)
C
C                  Interval            1
C                     Start time: 2000-JAN-03 08:24:39
C                     Stop time:  2000-JAN-03 14:57:17
C
C     From GFOCLT, Front (gamma), back (alpha), abcorr (xcn),
C                  occultation type (partial)
C
C                  Interval            1
C                     Start time: 2000-JAN-03 08:24:39
C                     Stop time:  2000-JAN-03 09:57:59
C                  Interval            2
C                     Start time: 2000-JAN-03 12:34:32
C                     Stop time:  2000-JAN-03 14:57:17
C
C     From GFOCLT, Front (gamma), back (alpha), abcorr (xcn),
C                  occultation type (annular transit)
C
C                  Interval            1
C                     Start time: 2000-JAN-03 09:57:59
C                     Stop time:  2000-JAN-03 12:34:32
C
C     From GFOCLT, Front (gamma), back (alpha), abcorr (xcn),
C                  occultation type (full)
C
C                  No occultation was found.
C
C     From GFOCLT, Front (alpha), back (gamma), abcorr (xcn), 
C                  occultation type (any)
C
C                  Interval            1
C                     Start time: 2000-JAN-02 20:41:09
C                     Stop time:  2000-JAN-03 03:51:51
C
C     From GFOCLT, Front (alpha), back (gamma), abcorr (xcn), 
C                  occultation type (partial)
C
C                  Interval            1
C                     Start time: 2000-JAN-02 20:41:09
C                     Stop time:  2000-JAN-02 22:52:49
C                  Interval            2
C                     Start time: 2000-JAN-03 02:28:19
C                     Stop time:  2000-JAN-03 03:51:51
C
C     From GFOCLT, Front (alpha), back (gamma), abcorr (xcn), 
C                  occultation type (annular)
C
C                  No occultation was found.
C
C     From GFOCLT, Front (alpha), back (gamma), abcorr (xcn), 
C                  occultation type (full)
C
C                  Interval            1
C                     Start time: 2000-JAN-02 22:52:49
C                     Stop time:  2000-JAN-03 02:28:19
C
C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Verify the occultation types for specific times '
     .        //   'are correct.' )

C
C     In order to test all occultation ID codes, first make GAMMA the
C     first target, and then make ALPHA the first target.  The same
C     configuration will have an occultation ID of -1*result if the
C     target 1 and target 2 bodies are reversed.  This is why MLTFAC
C     exists.
C

      DO I = 1, 2

         IF ( I .EQ. 1 ) THEN

            TARG1  = 'GAMMA'
            SHAPE1 = 'ELLIPSOID'
            FRAME1 = 'GAMMAFIXED'
            TARG2  = 'ALPHA'
            SHAPE2 = 'ELLIPSOID'
            FRAME2 = 'ALPHAFIXED'
            OBSRVR = 'SUN'
            ABCORR = 'NONE'
            MLTFAC = 1

         ELSE

            TARG1  = 'ALPHA'
            FRAME1 = 'ALPHAFIXED'
            TARG2  = 'GAMMA'
            FRAME2 = 'GAMMAFIXED'
            MLTFAC = -1
            

         END IF
C
C        For each test, convert the time to ET, call OCCULT, and verify
C        the OCLTID matches with the desired result.
C
         DO J = 1, NUMTIM

            CALL STR2ET ( TIMES(J), ET )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL OCCULT ( TARG1,  SHAPE1, FRAME1,
     .                    TARG2,  SHAPE2, FRAME2, 
     .                    ABCORR, OBSRVR, ET, OCLTID )

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ('OCLTID', OCLTID, '=', MLTFAC*RESULT(J), 
     .                    0, OK)
            CALL CHCKSI ('OCLTID', OCLTID, '=', MLTFAC*RESULT(J), 
     .                    0, OK)

         END DO

      END DO

C
C---- Case -------------------------------------------------------------
C
C     At TIMES(3) with Gamma as the first target (point), the
C     occultation ID should be -3, representing that Gamma is completely
C     occulted by Alpha.
C
      CALL TCASE ( 'Point case:  Point totally occulted by ellipsoid' )

      CALL STR2ET ( TIMES(3), ET )

      CALL OCCULT ( 'gamma', 'point',     ' ',
     .              'alpha', 'ellipsoid', 'ALPHAFIXED', 
     .              'NONE',   OBSRVR, ET, OCLTID )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', RESULT(3), 0, OK)

      CALL OCCULT ( 'alpha', 'ellipsoid', 'ALPHAFIXED', 
     .              'gamma', 'point',     ' ',
     .              'NONE',   OBSRVR, ET, OCLTID )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', -1*RESULT(3), 0, OK)

C
C---- Case -------------------------------------------------------------
C
C     At TIMES(5) with Gamma as the first target (point), the
C     occultation ID should be 2 (alpha transited by gamma).
C
      CALL TCASE ('Point case: Point transiting ellipsoid')

      CALL STR2ET ( TIMES(5), ET )

      CALL OCCULT ( 'GAMMA', 'POINT', ' ',
     .              'ALPHA', 'ELLIPSOID', 'ALPHAFIXED',
     .              'NONE',  OBSRVR, ET, OCLTID )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', RESULT(5), 0, OK)

      CALL OCCULT ( 'ALPHA', 'ELLIPSOID', 'ALPHAFIXED',
     .              'GAMMA', 'POINT', ' ',
     .               'NONE',  OBSRVR, ET, OCLTID )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', -1*RESULT(5), 0, OK)

C
C---- Case -------------------------------------------------------------
C
C     At the time below, if the abcorr of 'none' is used, a partial
C     occultation is reported.  If 'xcn' is used, an annular transit
C     is reported.  The time and occultation types were calculated
C     using GFOCLT.
C
      CALL TCASE ( 'Verify results with different abcorr' )

      CALL STR2ET (  '2000-JAN-03 09:58:02 (TDB)', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )
C
C     Calculate the occultation type with no aberration correction.
C     Check that the result is 'partial'.
C
      CALL OCCULT ( 'GAMMA', 'ELLIPSOID', 'GAMMAFIXED',
     .              'ALPHA', 'ELLIPSOID', 'ALPHAFIXED',
     .              'NONE',   OBSRVR, ET,  OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', 1, 0, OK)
C
C     Calculate the occultation type with the aberration correction set
C     to 'xcn'.  Check that the result is 'annular'.
C
      CALL OCCULT ( 'GAMMA', 'ELLIPSOID', 'GAMMAFIXED',
     .              'ALPHA', 'ELLIPSOID', 'ALPHAFIXED',
     .              'XCN',    OBSRVR, ET,  OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', 2, 0, OK)



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
      NLON = 20
      NLAT = 10

      AFRAME = 'ALPHAFIXED'
      BFRAME = 'BETAFIXED'

      CALL NATDSK ( DSK, AFRAME, NLON, NLAT,
     .                   BFRAME,  NLON, NLAT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

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
      FRAME  = AFRAME
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
      CALL T_SECDS2 ( BODYID, SURFID, FRAME,  FIRST,  LAST,
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
      CALL T_SECDS2 ( BODYID, SURFID, FRAME,  FIRST,  LAST,
     .                CORSYS, CORPAR, BOUNDS, C,      C,
     .                C,      NLON,   NLAT,   MAKVTL, USEPAD,
     .                DSK                                    )
      CALL CHCKXC ( .FALSE., ' ',   OK )
       

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Check for non-occultation just prior to transit '
     .//           'start, for point BETA and DSK ALPHA. BETA is '
     .//           'body 1.'                                         )
  
      CALL NATPCK ( PCK,  .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )
      
      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL SPKPOS ( 'ALPHA', 0.D0, 'J2000', 'NONE', 'sun', POS, LT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

C
C     Use a time 2 microseconds before the nominal start of occultation.
C
      CALL STR2ET ( '2000 JAN 1 12:00:59.999998 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL OCCULT ( 'BETA',  'POINT', ' ',
     .              'ALPHA', 'DSK/UNPRIORITIZED', FRAME,
     .              'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', NOOCC, 0, OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Check for non-occultation just prior to transit '
     .//           'start, for point BETA and DSK ALPHA. BETA is '
     .//           'body 2.'                                         )

      CALL OCCULT ( 'ALPHA', 'DSK/UNPRIORITIZED', FRAME,
     .              'BETA',  'POINT',             ' ',
     .              'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', NOOCC, 0, OK )


C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Check for annular transit just after transit '
     .//           'start, for point BETA and DSK ALPHA. BETA is '
     .//           'body 1.'                                      )


C
C     Use a time 2 microseconds after the nominal start of occultation.
C
      CALL STR2ET ( '2000 JAN 1 12:01:00.000002 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL OCCULT ( 'BETA',  'POINT', ' ',
     .              'ALPHA', 'DSK/UNPRIORITIZED', FRAME,
     .              'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', ANNLR2, 0, OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Check for annular transit just after transit '
     .//           'start, for point BETA and DSK ALPHA. BETA is '
     .//           'body 2.'                                         )

      CALL OCCULT ( 'ALPHA', 'DSK/UNPRIORITIZED', FRAME,
     .              'BETA',  'POINT',             ' ',
     .              'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', ANNLR1, 0, OK )


C
C     In the tests below, BETA is modeled by a DSK and ALPHA is 
C     treated as a point.
C

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Check for full occultation at the midpoint of '
     .//           'the occultation interval, for DSK BETA and '
     .//           'point ALPHA. BETA is body 1.' )

      CALL STR2ET ( '2000 JAN 1 12:05:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL OCCULT ( 'BETA',  'DSK/UNPRIORITIZED', BFRAME,
     .              'ALPHA', 'POINT',             ' ',
     .              'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', TOTAL2, 0, OK )

C
C---- Case -------------------------------------------------------------
C
      CALL TCASE ( 'Check for full occultation at the midpoint of '
     .//           'the occultation interval, for DSK BETA and '
     .//           'point ALPHA. BETA is body 2.' )

      CALL STR2ET ( '2000 JAN 1 12:05:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL OCCULT ( 'ALPHA', 'POINT',              ' ', 
     .              'BETA',  'DSK/UNPRIORITIZED',  BFRAME,
     .              'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', TOTAL1, 0, OK )





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
