!$Procedure      F_OCCULT ( OCCULT family tests )

      SUBROUTINE F_OCCULT ( OK )

! $ Abstract
!
!     This routine tests the SPICELIB routine
!
!        OCCULT
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      INCLUDE 'dskdsc.inc'
      INCLUDE 'occult.inc'

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
!     This routine tests the higher-level SPICELIB
!     geometry routine OCCULT.
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
!     S.C. Krening     (JPL)
!     N.J. Bachman     (JPL)
!
! $ Version
!
!-    TSPICE Version 2.0.0, 23-MAY-2016 (SCK) (NJB)
!
!        Updated to test support for DSK-based target
!        shape models.
!
!-    TSPICE Version 1.0.0, 17-JAN-2012 (SCK) (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      JYEAR

      LOGICAL               EXISTS

!
!     Local parameters
!
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

!
!     Local variables
!
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

!
!     Save variables
!
      SAVE                  TIMES
      SAVE                  RESULT

!
!     Initialize
!
      DATA TIMES ( 1 ) / '2011-JAN-02 19:00:00' /
      DATA TIMES ( 2 ) / '2011-JAN-02 21:00:00' /
      DATA TIMES ( 3 ) / '2011-JAN-03 00:00:00' /
      DATA TIMES ( 4 ) / '2011-JAN-03 09:00:00' /
      DATA TIMES ( 5 ) / '2011-JAN-03 11:00:00' /

      DATA RESULT / 0, -1, -3, 1, 2 /

!
!     Begin every test family with an open call.
!
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


!*********************************************************************
!*
!*    Error cases
!*
!*********************************************************************


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Invalid first shape' )

      CALL OCCULT ( 'ALPHA', 'PNT',   'ALPHAFIXED', &
                    'BETA',  'POINT', 'BETAFIXED', &
                    'NONE',  'SUN',    ET, OCLTID  )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )


!
!     Missing "unprioritized" keyword:
!
      CALL OCCULT ( 'ALPHA', 'DSK',   'ALPHAFIXED', &
                    'BETA',  'POINT', 'BETAFIXED', &
                    'NONE',  'SUN',    ET, OCLTID  )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )



!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Invalid second shape' )

      CALL OCCULT ( 'ALPHA', 'POINT', 'ALPHAFIXED', &
                    'BETA',  'PNT',   'BETAFIXED', &
                    'NONE',  'SUN',    ET, OCLTID   )

      CALL CHCKXC ( .TRUE., 'SPICE(BADMETHODSYNTAX)', OK )

!
!     Missing "unprioritized" keyword:
!
      CALL OCCULT ( 'ALPHA', 'POINT', 'ALPHAFIXED', &
                    'BETA',  'DSK',   'BETAFIXED', &
                    'NONE',  'SUN',    ET, OCLTID   )

      CALL CHCKXC ( .TRUE., 'SPICE(BADPRIORITYSPEC)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Verify both targets cannot be points' )

      CALL OCCULT ( 'ALPHA', 'POINT', 'ALPHAFIXED', &
                    'BETA',  'POINT', 'BETAFIXED', &
                    'NONE',  'SUN',    ET, OCLTID  )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDSHAPECOMBO)', OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Verify both targets cannot be identical' )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'ALPHA', 'ELLIPSOID', 'BETAFIXED', &
                    'NONE',  'SUN',        ET, OCLTID  )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Verify the first target and observer cannot ' &
              //   'be identical.' )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'BETA',  'ELLIPSOID', 'BETAFIXED', &
                    'NONE',  'ALPHA',      ET,  OCLTID )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Verify the second target and observer cannot ' &
              //   'be identical.' )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'BETA',  'ELLIPSOID', 'BETAFIXED', &
                    'NONE',  'BETA',       ET,  OCLTID )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )

!
!---- Case -------------------------------------------------------------
!
!     Note:  This is a test for three cases:
!     1)  If an unrecognized target is elliptical and the frame
!         cannot be found.
!     2)  If an unrecognized target is a point and the ID code is
!         not found in the call tree of ZZGFOCIN.
!     3)  If an observer is unrecognized.
!
      CALL TCASE ( 'Verify an error if the targets or observer are ' &
              //   'unrecognized' )

      CALL OCCULT ( 'ALPH', 'ellipsoid', 'ALPHAFIXED', &
                    'BETA', 'ELLIPSOID', 'BETAFIXED', &
                    'NONE', 'SUN', ET,    OCLTID )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'BTA',   'ELLIPSOID', 'BETAFIXED', &
                    'NONE',  'SUN', ET,    OCLTID )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'BETA',  'ELLIPSOID', 'BETAFIXED', &
                    'NONE',  'SUNN', ET,   OCLTID )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)',   OK )

!
!
!---- Case -------------------------------------------------------------
!

      CALL TCASE ( 'Verify aberration correction inputs' )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'BETA',  'ELLIPSOID', 'BETAFIXED', &
                    'S',     'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'BETA',  'ELLIPSOID', 'BETAFIXED', &
                    'XS',    'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'BETA',  'ELLIPSOID', 'BETAFIXED', &
                    'RLT',   'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'BETA',  'ELLIPSOID', 'BETAFIXED', &
                    'XRLT',  'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'BETA',  'ELLIPSOID', 'BETAFIXED', &
                    'z',     'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)',   OK )

!
!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No SPK data for observer' )

      CALL OCCULT ( 'ALPHA', 'POINT',     'ALPHAFIXED', &
                    'BETA',  'ELLIPSOID', 'BETAFIXED', &
                    'LT',    'GASPRA', ET, OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No SPK data for first target' )

      CALL OCCULT ( 'GASPRA', 'POINT',     'IAU_GASPRA', &
                    'BETA',   'ELLIPSOID', 'BETAFIXED', &
                    'LT',     'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No SPK data for second target' )

      CALL OCCULT ( 'ALPHA',  'ELLIPSOID', 'ALPHAFIXED', &
                    'GASPRA', 'POINT',     'IAU_GASPRA', &
                    'LT',     'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No PCK orientation data for first target; ' &
      //           'both targets are ellipsoids.'               )

      CALL OCCULT ( 'EARTH', 'ellipsoid', 'ITRF93', &
                    'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', &
                    'LT',    'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No PCK orientation data for second target; ' &
      //           'both targets are ellipsoids.'               )

      CALL OCCULT ( 'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', &
                    'EARTH', 'ellipsoid', 'ITRF93', &
                    'LT',    'SUN', ET,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No PCK orientation data for front target; ' &
      //           'front target is ellipsoid; back is point.' )


!
!     Capture Beta's prime meridian data; delete it from
!     the kernel pool.
!
      CALL GDPOOL ( 'BODY2000_PM', 1, 3, N, PMCOEF, FOUND )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DVPOOL ( 'BODY2000_PM' )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL OCCULT ( 'BETA',  'ELLIPSOID', 'BETAFIXED', &
                    'ALPHA', 'point',     ' ', &
                    'NONE',  'SUN', 250.D0,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',  OK )

!
!     Restore the PM data.
!
      CALL PDPOOL ( 'BODY2000_PM', 3, PMCOEF )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'No PCK orientation data for back target; ' &
      //           'front target is point; back is ellipsoid.' )


!
!     Because the frame ALPHAFIXED is a TK frame defined
!     relative to the PCK frame BETAAFIXED, we can make
!     the SINCPT call, which accesses the ALPHAFIXED frame,
!     fail by damaging the data for the for BETAFIXED frame.
!
!     This scheme depends on the Nat's solar system PCK
!     implementation.
!
!     Capture Beta's prime meridian data; delete it from
!     the kernel pool.
!
      CALL GDPOOL ( 'BODY2000_PM', 1, 3, N, PMCOEF, FOUND )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL CHCKSL ( 'FOUND', FOUND, .TRUE., OK )

      CALL DVPOOL ( 'BODY2000_PM' )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL OCCULT ( 'BETA',  'point', ' ', &
                    'ALPHA', 'ellipsoid', 'alphafixed', &
                    'NONE',  'SUN', 62.D0,    OCLTID )
      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)',  OK )

!
!     Restore the PM data.
!
      CALL PDPOOL ( 'BODY2000_PM', 3, PMCOEF )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!*********************************************************************
!*
!*    Normal cases
!*
!*********************************************************************
!
!     When TARG1 is GAMMA (abcorr = none):
!
! 2000-JAN-02 19 GAMMA  not occulted by        ALPHA  as seen by SUN   0
! 2000-JAN-02 21 GAMMA  partially occulted by  ALPHA  as seen by SUN  -1
! 2000-JAN-03 00 GAMMA  totally occulted by    ALPHA  as seen by SUN  -3
! 2000-JAN-03 07 GAMMA  not occulted by        ALPHA  as seen by SUN   0
! 2000-JAN-03 09 ALPHA  partially occulted by  GAMMA  as seen by SUN   1
! 2000-JAN-03 11 ALPHA  transited by           GAMMA  as seen by SUN   2
! 2000-JAN-03 16 GAMMA  not occulted by        ALPHA  as seen by SUN   0
!
!     When TARG1 is ALPHA (abcorr = none):
!
! 2000-JAN-02 19 ALPHA not occulted by         GAMMA  as seen by SUN   0
! 2000-JAN-02 21 GAMMA partially occulted by   ALPHA  as seen by SUN   1
! 2000-JAN-03 00 GAMMA totally occulted by     ALPHA  as seen by SUN   3
! 2000-JAN-03 07 ALPHA not occulted by         GAMMA  as seen by SUN   0
! 2000-JAN-03 09 ALPHA partially occulted by   GAMMA  as seen by SUN  -1
! 2000-JAN-03 11 ALPHA transited by            GAMMA  as seen by SUN  -2
! 2000-JAN-03 16 ALPHA not occulted by         GAMMA  as seen by SUN   0
!
!     From GFOCLT, Front (alpha), back (gamma), abcorr (none),
!                  occultation type (any)
!
!                  Interval            1
!                     Start time: 2000-JAN-02 20:41:15
!                     Stop time:  2000-JAN-03 03:51:58
!
!     From GFOCLT, Front (alpha), back (gamma), abcorr (none),
!                  occultation type (partial)
!
!                  Interval            1
!                     Start time: 2000-JAN-02 20:41:15
!                     Stop time:  2000-JAN-02 22:52:53
!                  Interval            2
!                     Start time: 2000-JAN-03 02:28:28
!                     Stop time:  2000-JAN-03 03:51:58
!
!     From GFOCLT, Front (alpha), back (gamma), abcorr (none),
!                  occultation type (annular)
!
!                  No occultation was found.
!
!     From GFOCLT, Front (alpha), back (gamma), abcorr (none),
!                  occultation type (full, total)
!
!                  Interval            1
!                     Start time: 2000-JAN-02 22:52:53
!                     Stop time:  2000-JAN-03 02:28:28
!
!     From GFOCLT, Front (gamma), back (alpha), abcorr (none),
!                  occultation type (any)
!
!                  Interval            1
!                     Start time: 2000-JAN-03 08:24:46
!                     Stop time:  2000-JAN-03 14:57:23
!
!     From GFOCLT, Front (gamma), back (alpha), abcorr (none),
!                  occultation type (partial)
!
!                  Interval            1
!                     Start time: 2000-JAN-03 08:24:46
!                     Stop time:  2000-JAN-03 09:58:08
!                  Interval            2
!                     Start time: 2000-JAN-03 12:34:35
!                     Stop time:  2000-JAN-03 14:57:23
!
!     From GFOCLT, Front (gamma), back (alpha), abcorr (none),
!                  occultation type (annular)
!
!                  Interval            1
!                     Start time: 2000-JAN-03 09:58:08
!                     Stop time:  2000-JAN-03 12:34:35
!
!     From GFOCLT, Front (gamma), back (alpha), abcorr (none),
!                  occultation type (full, total)
!
!                  No occultation was found.
!
!           -   -   -   -   -   -   -   -   -   -   -   -
!     From GFOCLT, Front (gamma), back (alpha), abcorr (xcn),
!                  occultation type (any)
!
!                  Interval            1
!                     Start time: 2000-JAN-03 08:24:39
!                     Stop time:  2000-JAN-03 14:57:17
!
!     From GFOCLT, Front (gamma), back (alpha), abcorr (xcn),
!                  occultation type (partial)
!
!                  Interval            1
!                     Start time: 2000-JAN-03 08:24:39
!                     Stop time:  2000-JAN-03 09:57:59
!                  Interval            2
!                     Start time: 2000-JAN-03 12:34:32
!                     Stop time:  2000-JAN-03 14:57:17
!
!     From GFOCLT, Front (gamma), back (alpha), abcorr (xcn),
!                  occultation type (annular transit)
!
!                  Interval            1
!                     Start time: 2000-JAN-03 09:57:59
!                     Stop time:  2000-JAN-03 12:34:32
!
!     From GFOCLT, Front (gamma), back (alpha), abcorr (xcn),
!                  occultation type (full)
!
!                  No occultation was found.
!
!     From GFOCLT, Front (alpha), back (gamma), abcorr (xcn),
!                  occultation type (any)
!
!                  Interval            1
!                     Start time: 2000-JAN-02 20:41:09
!                     Stop time:  2000-JAN-03 03:51:51
!
!     From GFOCLT, Front (alpha), back (gamma), abcorr (xcn),
!                  occultation type (partial)
!
!                  Interval            1
!                     Start time: 2000-JAN-02 20:41:09
!                     Stop time:  2000-JAN-02 22:52:49
!                  Interval            2
!                     Start time: 2000-JAN-03 02:28:19
!                     Stop time:  2000-JAN-03 03:51:51
!
!     From GFOCLT, Front (alpha), back (gamma), abcorr (xcn),
!                  occultation type (annular)
!
!                  No occultation was found.
!
!     From GFOCLT, Front (alpha), back (gamma), abcorr (xcn),
!                  occultation type (full)
!
!                  Interval            1
!                     Start time: 2000-JAN-02 22:52:49
!                     Stop time:  2000-JAN-03 02:28:19
!
!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Verify the occultation types for specific times ' &
              //   'are correct.' )

!
!     In order to test all occultation ID codes, first make GAMMA the
!     first target, and then make ALPHA the first target.  The same
!     configuration will have an occultation ID of -1*result if the
!     target 1 and target 2 bodies are reversed.  This is why MLTFAC
!     exists.
!

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
!
!        For each test, convert the time to ET, call OCCULT, and verify
!        the OCLTID matches with the desired result.
!
         DO J = 1, NUMTIM

            CALL STR2ET ( TIMES(J), ET )
            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL OCCULT ( TARG1,  SHAPE1, FRAME1, &
                          TARG2,  SHAPE2, FRAME2, &
                          ABCORR, OBSRVR, ET, OCLTID )

            CALL CHCKXC ( .FALSE., ' ', OK )

            CALL CHCKSI ('OCLTID', OCLTID, '=', MLTFAC*RESULT(J), &
                          0, OK)
            CALL CHCKSI ('OCLTID', OCLTID, '=', MLTFAC*RESULT(J), &
                          0, OK)

         END DO

      END DO

!
!---- Case -------------------------------------------------------------
!
!     At TIMES(3) with Gamma as the first target (point), the
!     occultation ID should be -3, representing that Gamma is completely
!     occulted by Alpha.
!
      CALL TCASE ( 'Point case:  Point totally occulted by ellipsoid' )

      CALL STR2ET ( TIMES(3), ET )

      CALL OCCULT ( 'gamma', 'point',     ' ', &
                    'alpha', 'ellipsoid', 'ALPHAFIXED', &
                    'NONE',   OBSRVR, ET, OCLTID )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', RESULT(3), 0, OK)

      CALL OCCULT ( 'alpha', 'ellipsoid', 'ALPHAFIXED', &
                    'gamma', 'point',     ' ', &
                    'NONE',   OBSRVR, ET, OCLTID )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', -1*RESULT(3), 0, OK)

!
!---- Case -------------------------------------------------------------
!
!     At TIMES(5) with Gamma as the first target (point), the
!     occultation ID should be 2 (alpha transited by gamma).
!
      CALL TCASE ('Point case: Point transiting ellipsoid')

      CALL STR2ET ( TIMES(5), ET )

      CALL OCCULT ( 'GAMMA', 'POINT', ' ', &
                    'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', &
                    'NONE',  OBSRVR, ET, OCLTID )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', RESULT(5), 0, OK)

      CALL OCCULT ( 'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', &
                    'GAMMA', 'POINT', ' ', &
                     'NONE',  OBSRVR, ET, OCLTID )

      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', -1*RESULT(5), 0, OK)

!
!---- Case -------------------------------------------------------------
!
!     At the time below, if the abcorr of 'none' is used, a partial
!     occultation is reported.  If 'xcn' is used, an annular transit
!     is reported.  The time and occultation types were calculated
!     using GFOCLT.
!
      CALL TCASE ( 'Verify results with different abcorr' )

      CALL STR2ET (  '2000-JAN-03 09:58:02 (TDB)', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )
!
!     Calculate the occultation type with no aberration correction.
!     Check that the result is 'partial'.
!
      CALL OCCULT ( 'GAMMA', 'ELLIPSOID', 'GAMMAFIXED', &
                    'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', &
                    'NONE',   OBSRVR, ET,  OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', 1, 0, OK)
!
!     Calculate the occultation type with the aberration correction set
!     to 'xcn'.  Check that the result is 'annular'.
!
      CALL OCCULT ( 'GAMMA', 'ELLIPSOID', 'GAMMAFIXED', &
                    'ALPHA', 'ELLIPSOID', 'ALPHAFIXED', &
                    'XCN',    OBSRVR, ET,  OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )
      CALL CHCKSI ('OCLTID', OCLTID, '=', 2, 0, OK)



!***********************************************************************
!
!     DSK tests
!
!***********************************************************************


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Create a DSK containing shape models for ' &
      //           'bodies Alpha and Beta.'                    )


!
!     Make sure the generic SPK is unloaded! We need the sun ephemeris
!     from Nat's SPK.
!
      CALL SPKUEF ( HAN2 )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!
!     This block is suitable for use in F_GFOCLT. We don't actually
!     have to have the high-resolution DSK patches to test OCCULT.
!
!     We'll enhance the generic DSK models for Alpha and Beta by
!     appending segments containing small patches of high-resolution
!     data for the surface regions that participate in computation of
!     accurate occultation ingress and egress times. We use small
!     patches because the size and run time needed to create full
!     models at this resolution would be prohibitive.
!
!     Start by creating the basic DSK.
!
      IF ( EXISTS(DSK) ) THEN

         CALL UNLOAD ( DSK )
         CALL DELFIL ( DSK )
         CALL CHCKXC ( .FALSE., ' ',   OK )

      END IF

!
!     Use low resolution tessellations for the main models.
!
      NLON = 20
      NLAT = 10

      AFRAME = 'ALPHAFIXED'
      BFRAME = 'BETAFIXED'

      CALL NATDSK ( DSK, AFRAME, NLON, NLAT, &
                         BFRAME,  NLON, NLAT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Create the -Y patch for body Alpha. The patch covers
!     the lon/lat rectangle
!
!        -92 deg. <= lon <= -88 deg.
!          0 deg. <= lat <=   4 deg.
!
!     Note that Alpha' body-fixed Z axis lies in Alpha's orbital
!     plane.
!
      FRAME  = AFRAME
      BODYID = 1000
      SURFID = 2
      FIRST  = -100 * JYEAR()
      LAST   =  100 * JYEAR()

      CALL BODVCD ( BODYID, 'RADII', 3, N, ARAD )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Make the patch spherical, using the ellipsoid's Z
!     semi-axis length.
!
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

!
!     Append the patch segment to the existing DSK.
!
      CALL T_SECDS2 ( BODYID, SURFID, FRAME,  FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, C,      C, &
                      C,      NLON,   NLAT,   MAKVTL, USEPAD, &
                      DSK                                    )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Create the +Y patch for body Alpha. The patch covers
!     the lon/lat rectangle
!
!         88 deg. <= lon <=  92 deg.
!          0 deg. <= lat <=   4 deg.
!

      SURFID = 3

      BOUNDS(1,1) =  RPD() * ( 88.D0)
      BOUNDS(2,1) =  RPD() * ( 92.D0)
      BOUNDS(1,2) =  RPD() * (  0.D0)
      BOUNDS(2,2) =  RPD() * (  4.D0)

      NLAT   = 200
      NLON   = 200

      USEPAD = .TRUE.

!
!     Append the patch segment to the existing DSK.
!
      CALL T_SECDS2 ( BODYID, SURFID, FRAME,  FIRST,  LAST, &
                      CORSYS, CORPAR, BOUNDS, C,      C, &
                      C,      NLON,   NLAT,   MAKVTL, USEPAD, &
                      DSK                                    )
      CALL CHCKXC ( .FALSE., ' ',   OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Check for non-occultation just prior to transit ' &
      //           'start, for point BETA and DSK ALPHA. BETA is ' &
      //           'body 1.'                                         )

      CALL NATPCK ( PCK,  .TRUE., .FALSE. )
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL TSTLSK
      CALL CHCKXC ( .FALSE., ' ',   OK )

      CALL FURNSH ( DSK )
      CALL CHCKXC ( .FALSE., ' ', OK )



      CALL SPKPOS ( 'ALPHA', 0.D0, 'J2000', 'NONE', 'sun', POS, LT )
      CALL CHCKXC ( .FALSE., ' ',   OK )

!
!     Use a time 2 microseconds before the nominal start of occultation.
!
      CALL STR2ET ( '2000 JAN 1 12:00:59.999998 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL OCCULT ( 'BETA',  'POINT', ' ', &
                    'ALPHA', 'DSK/UNPRIORITIZED', FRAME, &
                    'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', NOOCC, 0, OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Check for non-occultation just prior to transit ' &
      //           'start, for point BETA and DSK ALPHA. BETA is ' &
      //           'body 2.'                                         )

      CALL OCCULT ( 'ALPHA', 'DSK/UNPRIORITIZED', FRAME, &
                    'BETA',  'POINT',             ' ', &
                    'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', NOOCC, 0, OK )


!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Check for annular transit just after transit ' &
      //           'start, for point BETA and DSK ALPHA. BETA is ' &
      //           'body 1.'                                      )


!
!     Use a time 2 microseconds after the nominal start of occultation.
!
      CALL STR2ET ( '2000 JAN 1 12:01:00.000002 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL OCCULT ( 'BETA',  'POINT', ' ', &
                    'ALPHA', 'DSK/UNPRIORITIZED', FRAME, &
                    'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', ANNLR2, 0, OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Check for annular transit just after transit ' &
      //           'start, for point BETA and DSK ALPHA. BETA is ' &
      //           'body 2.'                                         )

      CALL OCCULT ( 'ALPHA', 'DSK/UNPRIORITIZED', FRAME, &
                    'BETA',  'POINT',             ' ', &
                    'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', ANNLR1, 0, OK )


!
!     In the tests below, BETA is modeled by a DSK and ALPHA is
!     treated as a point.
!

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Check for full occultation at the midpoint of ' &
      //           'the occultation interval, for DSK BETA and ' &
      //           'point ALPHA. BETA is body 1.' )

      CALL STR2ET ( '2000 JAN 1 12:05:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL OCCULT ( 'BETA',  'DSK/UNPRIORITIZED', BFRAME, &
                    'ALPHA', 'POINT',             ' ', &
                    'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', TOTAL2, 0, OK )

!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Check for full occultation at the midpoint of ' &
      //           'the occultation interval, for DSK BETA and ' &
      //           'point ALPHA. BETA is body 2.' )

      CALL STR2ET ( '2000 JAN 1 12:05:00 TDB', ET )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL OCCULT ( 'ALPHA', 'POINT',              ' ', &
                    'BETA',  'DSK/UNPRIORITIZED',  BFRAME, &
                    'NONE',  OBSRVR, ET, OCLTID )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL CHCKSI ( 'OCLTID', OCLTID, '=', TOTAL1, 0, OK )





!
!---- Case -------------------------------------------------------------
!
      CALL TCASE ( 'Clean up. Unload and delete kernels.' )

!
!     Clean up SPK and DSK files.
!
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
