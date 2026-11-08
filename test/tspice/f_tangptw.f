C$Procedure F_TANGPTW ( Test tangent point routine, wrapper tests )

      SUBROUTINE F_TANGPTW ( OK )

C$ Abstract
C
C     Perform interface tests on the tangent point routine TANGPT.
C     This test family can be converted to C and other languages for
C     regression testing of wrappers.
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

      INCLUDE 'zzdsk.inc'
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
C     trapped or unintentional and need reporting. The test family
C     utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     None.
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
C     M. Costa Sitja   (JPL)
C
C$ Version
C
C-    TSPICE Version 1.0.0, 10-DEC-2021 (NJB) (MCS)
C
C-&

C
C     SPICELIB functions
C
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               EXISTS

C
C     Local Parameters
C
      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0 = 'tangpt_test0.tpc' )

      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0 = 'tangpt_test0.bsp' )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LOCLEN
      PARAMETER           ( LOCLEN = 20 )


      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-11 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-13 )

C
C     Local Variables
C
      CHARACTER*(CORLEN)    ABCORR
      CHARACTER*(LOCLEN)    CORLOC
      CHARACTER*(FRNMLN)    DREF
      CHARACTER*(FRNMLN)    FIXREF
      CHARACTER*(MTHLEN)    METHOD
      CHARACTER*(BDNMLN)    OBSRVR
      CHARACTER*(BDNMLN)    TARGET

      DOUBLE PRECISION      ALT
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      FIXDIR ( 3 )
      DOUBLE PRECISION      FIXOBS ( 3 )
      DOUBLE PRECISION      FIXSTL ( 3 )
      DOUBLE PRECISION      FIXTAN ( 3 )
      DOUBLE PRECISION      FIXTRG ( 3 )
      DOUBLE PRECISION      H
      DOUBLE PRECISION      J2OBS  ( 3 )
      DOUBLE PRECISION      LT
      DOUBLE PRECISION      LTTMP
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SRFPT  ( 3 )
      DOUBLE PRECISION      SRFTAN ( 3 )
      DOUBLE PRECISION      SRFSTA ( 6 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SSBOBS ( 6 )
      DOUBLE PRECISION      SSBTRG ( 6 )
      DOUBLE PRECISION      STLOFF ( 3 )
      DOUBLE PRECISION      TANPT  ( 3 )
      DOUBLE PRECISION      TANSTA ( 6 )
      DOUBLE PRECISION      TANST1 ( 6 )
      DOUBLE PRECISION      TANVEC ( 3 )
      DOUBLE PRECISION      TMPPOS ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGSTA ( 6 )
      DOUBLE PRECISION      XALT
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XRANGE
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      ZVEC   ( 3 )

      INTEGER               HANDLE
      INTEGER               NRAD
      INTEGER               OBSCDE
      INTEGER               TRGCDE

C
C     Saved variables
C
      SAVE

C
C     Initial values
C

C
C     Elevation units are radians.
C
      DATA                  ZVEC   / 0.D0, 0.D0, 1.D0 /
C
C     Open the test family.
C
      CALL TOPEN ( 'F_TANGPTW' )


C
C --- Case: -----------------------------------------------------------
C
      CALL TCASE ( 'Setup' )

C
C     Create default test PCK.
C
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTPCK ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create test SPK.
C
      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTSPK ( SPK0, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


C *********************************************************************
C
C     Normal cases
C
C *********************************************************************

C
C --- Case: -------------------------------------------------------
C
      CALL TCASE ( 'Simple case 1: Earth observes Moon, abcorr = '
     .//           'NONE. Ray points toward a point 100 km above '
     .//           'the Moon''s north pole. Use tangent point locus.' )

      METHOD = 'Ellipsoid'
      TARGET = 'MOON'

C
C     Don't use ET = 0.D0 for testing expected results; that time
C     value could hide uninitialized time errors.
C
      ET     = 31557600.D0

      FIXREF = 'IAU_MOON'
      ABCORR = 'NONE'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'

C
C     Get radii of Moon.
C
      CALL BODVRD ( 'MOON', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create target point FIXTRG in the body-fixed frame.
C
      H = 100.D0

      CALL VPACK( 0.D0, 0.D0,  1.D0,  ZVEC   )
      CALL VSCL ( RADII(3)+H,  ZVEC,  FIXTRG )

C
C     Compute the state of the target point relative to the observer
C     in the target body-fixed frame, evaluated at the target epoch.
C     Return the state expressed in the J2000 frame.
C
      CALL SPKCPT ( FIXTRG,   'MOON', FIXREF, ET,     DREF,
     .              'TARGET', ABCORR, OBSRVR, TRGSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( TRGSTA, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Convert the ray direction from the J2000 frame to the body-fixed
C     frame at the target epoch.
C
      CALL PXFORM ( DREF,  FIXREF, TRGEPC, XFORM )
      CALL MXV    ( XFORM, DVEC,   FIXDIR )

C
C     Compute the vector from the observer to the tangent point in
C     the body-fixed frame at the target epoch.
C
      CALL VLCOM3 ( 1.D0, SRFVEC, -1.D0, SRFPT, 1.D0, TANPT, FIXTAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check angular separation of ray direction and direction
C     to tangent point.
C
      SEP = VSEP( FIXDIR, FIXTAN )

      TOL = VTIGHT

      CALL CHCKSD ( 'RAY-OBS-TO-TANPT SEP', SEP, '~', 0.D0, TOL, OK )

C
C     Check the position of the tangent point above the surface point.
C
      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSUB ( TANPT, SRFPT, SRFTAN )

      SEP = VSEP ( SRFTAN, NORMAL )

C
C     Use looser tolerance here since the relative errors in TANPT
C     and SRFPT are larger compared to the lengths of those vectors
C     than they are relative to the observer-target distance.
C
      TOL = 1.D-11

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

C
C     Verify the zenith direction is normal to the ray direction.
C
      SEP = VSEP ( NORMAL, FIXDIR )

      CALL CHCKSD ( 'RAY ZENITH SEP', SEP, '~', HALFPI(), TOL, OK )

C
C     Check the altitude of the tangent point ALT.
C
      XALT = VDIST( SRFPT, TANPT )

C
C     Use the range from the observer to the tangent point to scale
C     the tolerance for the altitude check.
C
      TOL  = TIGHT * RANGE

      CALL CHCKSD ( 'ALT', ALT, '~', XALT, TOL, OK )

C
C     Get the position of the observer relative to the target center,
C     expressed in the target body-fixed frame. This call works only
C     for the geometric case.
C
      CALL SPKPOS ( OBSRVR, ET, FIXREF, 'NONE', TARGET, FIXOBS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check SRFVEC.
C
      CALL VSUB ( SRFPT, FIXOBS, XSRFVC )

      TOL = 1.D-13

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', XSRFVC, 3, TOL, OK )

C
C     Check range to the tangent point.
C
      XRANGE = VDIST( TANPT, FIXOBS )

      TOL = 1.D-13

      CALL CHCKSD ( 'RANGE', RANGE, '~/', XRANGE, TOL, OK )

C
C     Check the target epoch. In this case, it should match ET.
C
      XEPOCH = ET

      TOL    = VTIGHT

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

C
C  --- Case: ----------------------------------------------------------
C
      CALL TCASE ( 'Simple case 2: Phobos observes Mars, abcorr = '
     .//           'CN+S. Ray points toward a point 350 km above '
     .//           'Mars''s north pole. Use tangent point locus.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'Mars'

C
C     Don't use ET = 0.D0 for testing expected results; that could hide
C     uninitialized time errors.
C
      ET     = 31557600.D0

      FIXREF = 'IAU_MARS'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'Phobos'
      DREF   = 'J2000'

C
C     Get radii of Mars.
C
      CALL BODVRD ( 'MARS', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create target point FIXTRG in the body-fixed frame.
C
      H = 350.D0

      CALL VPACK( 0.D0, 0.D0,  1.D0,  ZVEC   )
      CALL VSCL ( RADII(3)+H,  ZVEC,  FIXTRG )

C
C     Compute the state of the target point relative to the observer
C     in the target body-fixed frame, evaluated at the target epoch.
C     Return the state expressed in the J2000 frame.
C
      CALL SPKCPT ( FIXTRG,   'MARS', FIXREF, ET,     DREF,
     .              'TARGET', ABCORR, OBSRVR, TRGSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( TRGSTA, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Convert the ray direction from the J2000 frame to the body-fixed
C     frame at the target epoch.
C
      CALL PXFORM ( DREF,  FIXREF, TRGEPC, XFORM )
      CALL MXV    ( XFORM, DVEC,   FIXDIR )

C
C     Compute the vector from the observer to the tangent point in
C     the body-fixed frame at the target epoch.
C
      CALL VLCOM3 ( 1.D0, SRFVEC, -1.D0, SRFPT, 1.D0, TANPT, FIXTAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check angular separation of ray direction and direction
C     to tangent point.
C
      SEP = VSEP( FIXDIR, FIXTAN )

      TOL = VTIGHT

      CALL CHCKSD ( 'RAY-OBS-TO-TANPT SEP', SEP, '~', 0.D0, TOL, OK )

C
C     Check the position of the tangent point above the surface point.
C
      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSUB ( TANPT, SRFPT, SRFTAN )

      SEP = VSEP ( SRFTAN, NORMAL )

      TOL = TIGHT

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

C
C     Verify the zenith direction is normal to the ray direction.
C
      SEP = VSEP ( NORMAL, FIXDIR )

      TOL = TIGHT

      CALL CHCKSD ( 'RAY ZENITH SEP', SEP, '~', HALFPI(), TOL, OK )

C
C     Check ALT.
C
      XALT = VDIST( SRFPT, TANPT )

C
C     Use the range from the observer to the tangent point to scale
C     the tolerance for the altitude check.
C
      TOL = TIGHT * RANGE

      CALL CHCKSD ( 'ALT', ALT, '~', XALT, TOL, OK )

C
C     Prepare to perform a consistency check on SRFVEC. We'll
C     use the target epoch associated with the tangent point to
C     compute the target body's position and orientation. We'll also
C     use the stellar aberration correction of the tangent point to
C     shift the target position. The target body's position and
C     orientation, along with the surface point, will be used to
C     compute an expected value of SRFVEC.
C
C     Get the position of the observer relative to the target center,
C     expressed in the target body-fixed frame. We must compute this
C     vector using the position of the observer relative to the solar
C     system barycenter at ET and the position of the target relative
C     to the solar system barycenter at TRGEPC.
C
C     This computation accounts for light time between the observer
C     and the tangent point. It does not account for stellar
C     aberration. That will be done separately.
C
      OBSCDE = 401
      TRGCDE = 499

      CALL SPKSSB ( OBSCDE, ET,     DREF, SSBOBS )
      CALL SPKSSB ( TRGCDE, TRGEPC, DREF, SSBTRG )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSUB ( SSBOBS, SSBTRG, J2OBS  )
      CALL MXV  ( XFORM,  J2OBS,  FIXOBS )

C
C     Now adjust the position of the observer relative to the target
C     to take into account the shift of the target by the stellar
C     aberration offset of the tangent point. We can get this offset
C     by computing the state of the tangent point with and without
C     stellar aberration correction.
C
C     Find the aberration-corrected position of the tangent point
C     relative to the observer, expressed in the J2000 frame.
C
      CALL SPKCPT ( TANPT,   'MARS',  FIXREF, ET,     DREF,
     .              'TARGET', ABCORR, OBSRVR, TANSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Find the light time corrected position of the tangent point
C     relative to the observer, expressed in the J2000 frame.
C
      CALL SPKCPT ( TANPT,    'MARS', FIXREF, ET,     DREF,
     .              'TARGET', 'CN',   OBSRVR, TANST1, LTTMP )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Find the stellar aberration offset of the tangent point,
C     expressed in the J2000 frame. Convert to the body-fixed frame
C     at epoch TRGEPC.
C
      CALL VSUB ( TANSTA, TANST1, STLOFF )

      CALL MXV  ( XFORM, STLOFF, FIXSTL )

C
C     Now compute the expected observer-to-surface vector, accounting
C     for the aberration corrections applicable to the tangent point.
C
      CALL VSUB ( SRFPT,  FIXOBS, TMPPOS )
      CALL VADD ( FIXSTL, TMPPOS, XSRFVC )

C
C     Check SRFVEC.
C
      TOL = TIGHT

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', XSRFVC, 3, TOL, OK )

C
C     Check range to the tangent point using the tangent point state.
C
      XRANGE = VNORM( TANSTA )

      TOL = TIGHT

      CALL CHCKSD ( 'RANGE', RANGE, '~/', XRANGE, TOL, OK )

C
C     Check the target epoch using the tangent point state.
C     It should match ET-LT, where LT is light time between the
C     tangent point and the observer.
C
      XEPOCH = ET - LT

      TOL    = VTIGHT

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

C
C     Compute the angular separation of the observer-tangent point
C     vector with the original ray direction. This differs from the
C     earlier test in that this time, the vector from the observer
C     to the tangent point was computed by SPKCPT.
C
      SEP = VSEP( TANSTA, DVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'TANSTA-DVEC SEP', SEP, '~', 0.D0, TOL, OK )

C
C --- Case: -----------------------------------------------------------
C
      CALL TCASE ( 'Simple case 3: Phobos observes Mars, abcorr = '
     .//           'XCN+S. Ray points toward a point 350 km above '
     .//           'Mars''s north pole. Use surface point locus.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'Mars'

C
C     Don't use ET = 0.D0 for testing expected results; that could hide
C     uninitialized time errors.
C
      ET     = 31557600.D0

      FIXREF = 'IAU_MARS'
      ABCORR = 'XCN+S'
      CORLOC = 'SURFACE POINT'
      OBSRVR = 'Phobos'
      DREF   = 'J2000'

C
C     Get radii of Mars.
C
      CALL BODVRD ( 'MARS', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Create target point FIXTRG in the body-fixed frame.
C
      H = 350.D0

      CALL VPACK( 0.D0, 0.D0,  1.D0,  ZVEC   )
      CALL VSCL ( RADII(3)+H,  ZVEC,  FIXTRG )

C
C     Compute the state of the target point relative to the observer
C     in the target body-fixed frame, evaluated at the target epoch.
C     Return the state expressed in the J2000 frame.
C
      CALL SPKCPT ( FIXTRG,   'MARS', FIXREF, ET,     DREF,
     .              'TARGET', ABCORR, OBSRVR, TRGSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( TRGSTA, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Convert the ray direction from the J2000 frame to the body-fixed
C     frame at the target epoch.
C
      CALL PXFORM ( DREF,  FIXREF, TRGEPC, XFORM )
      CALL MXV    ( XFORM, DVEC,   FIXDIR )

C
C     Compute the vector from the observer to the tangent point in
C     the body-fixed frame at the target epoch.
C
      CALL VLCOM3 ( 1.D0, SRFVEC, -1.D0, SRFPT, 1.D0, TANPT, FIXTAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Check angular separation of ray direction and direction
C     to tangent point.
C
      SEP = VSEP( FIXDIR, FIXTAN )

      TOL = VTIGHT

      CALL CHCKSD ( 'RAY-OBS-TO-TANPT SEP', SEP, '~', 0.D0, TOL, OK )

C
C     Check the position of the tangent point above the surface point.
C
      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSUB ( TANPT, SRFPT, SRFTAN )

      SEP = VSEP ( SRFTAN, NORMAL )

      TOL = TIGHT

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

C
C     Verify the zenith direction is normal to the ray direction.
C
      SEP = VSEP ( NORMAL, FIXDIR )

      TOL = TIGHT

      CALL CHCKSD ( 'RAY ZENITH SEP', SEP, '~', HALFPI(), TOL, OK )

C
C     Check ALT.
C
      XALT = VDIST( SRFPT, TANPT )

      TOL = TIGHT * RANGE

      CALL CHCKSD ( 'ALT', ALT, '~', XALT, TOL, OK )

C
C     Check SRFVEC. In order to compute an expected vector, we can
C     simply treat the surface point as an ephemeris object.
C
C     Find the aberration-corrected position of the surface point
C     relative to the observer, expressed in the target body-fixed
C     frame.
C
      CALL SPKCPT ( SRFPT,   'MARS',  FIXREF, ET,     FIXREF,
     .              'TARGET', ABCORR, OBSRVR, SRFSTA, LT     )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( SRFSTA, XSRFVC )

C
C     This tolerance value was chosen to support the
C     PC/Windows/Intel Fortran platform. The previous value was 1.D-12.
C
      TOL = 2.0D-12

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', XSRFVC, 3, TOL, OK )

C
C     Compute the position of the observer relative to the target
C     center in the body-fixed frame.
C
      CALL VSUB ( SRFPT, SRFVEC, FIXOBS )

C
C     Check range to the tangent point.
C
      XRANGE = VDIST( TANPT, FIXOBS )

      TOL = TIGHT

      CALL CHCKSD ( 'RANGE', RANGE, '~/', XRANGE, TOL, OK )

C
C     Check the target epoch.
C
      XEPOCH = ET + LT

      TOL    = VTIGHT

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

C
C     Compute the angular separation of the observer-tangent point
C     vector with the original ray direction.
C
      CALL VSUB ( TANPT, FIXOBS, TMPPOS )
      CALL MTXV ( XFORM, TMPPOS, TANVEC )

      SEP = VSEP( TANVEC, DVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'TANSTA-DVEC SEP', SEP, '~', 0.D0, TOL, OK )


C *********************************************************************
C
C     Error cases
C
C *********************************************************************

C
C --- Case: -----------------------------------------------------------
C
      CALL TCASE ( 'Invalid aberration correction locus.' )

      CORLOC = 'XYZ'

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case: -----------------------------------------------------------
C
      CALL TCASE ( 'Clean up.' )

      CALL KCLEAR
      CALL CHCKXC ( .FALSE., ' ', OK )

      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      IF ( EXISTS(SPK0) ) THEN
         CALL SPKUEF ( HANDLE )
         CALL DELFIL ( SPK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
