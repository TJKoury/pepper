!$Procedure F_TANGPTW ( Test tangent point routine, wrapper tests )

      SUBROUTINE F_TANGPTW ( OK )

! $ Abstract
!
!     Perform interface tests on the tangent point routine TANGPT.
!     This test family can be converted to C and other languages for
!     regression testing of wrappers.
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

      INCLUDE 'zzdsk.inc'
      INCLUDE 'zzabcorr.inc'

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
!     trapped or unintentional and need reporting. The test family
!     utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
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
!     M. Costa Sitja   (JPL)
!
! $ Version
!
!-    TSPICE Version 1.0.0, 10-DEC-2021 (NJB) (MCS)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      VDIST
      DOUBLE PRECISION      VNORM
      DOUBLE PRECISION      VSEP

      LOGICAL               EXISTS

!
!     Local Parameters
!
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

!
!     Local Variables
!
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

!
!     Saved variables
!
      SAVE

!
!     Initial values
!

!
!     Elevation units are radians.
!
      DATA                  ZVEC   / 0.D0, 0.D0, 1.D0 /
!
!     Open the test family.
!
      CALL TOPEN ( 'F_TANGPTW' )


!
! --- Case: -----------------------------------------------------------
!
      CALL TCASE ( 'Setup' )

!
!     Create default test PCK.
!
      IF ( EXISTS(PCK0) ) THEN
         CALL DELFIL( PCK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTPCK ( PCK0, .TRUE., .TRUE. )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create test SPK.
!
      IF ( EXISTS(SPK0) ) THEN
         CALL DELFIL( SPK0 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL TSTSPK ( SPK0, .TRUE., HANDLE )
      CALL CHCKXC ( .FALSE., ' ', OK )


! *********************************************************************
!
!     Normal cases
!
! *********************************************************************

!
! --- Case: -------------------------------------------------------
!
      CALL TCASE ( 'Simple case 1: Earth observes Moon, abcorr = ' &
      //           'NONE. Ray points toward a point 100 km above ' &
      //           'the Moon''s north pole. Use tangent point locus.' )

      METHOD = 'Ellipsoid'
      TARGET = 'MOON'

!
!     Don't use ET = 0.D0 for testing expected results; that time
!     value could hide uninitialized time errors.
!
      ET     = 31557600.D0

      FIXREF = 'IAU_MOON'
      ABCORR = 'NONE'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'

!
!     Get radii of Moon.
!
      CALL BODVRD ( 'MOON', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create target point FIXTRG in the body-fixed frame.
!
      H = 100.D0

      CALL VPACK( 0.D0, 0.D0,  1.D0,  ZVEC   )
      CALL VSCL ( RADII(3)+H,  ZVEC,  FIXTRG )

!
!     Compute the state of the target point relative to the observer
!     in the target body-fixed frame, evaluated at the target epoch.
!     Return the state expressed in the J2000 frame.
!
      CALL SPKCPT ( FIXTRG,   'MOON', FIXREF, ET,     DREF, &
                    'TARGET', ABCORR, OBSRVR, TRGSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( TRGSTA, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Convert the ray direction from the J2000 frame to the body-fixed
!     frame at the target epoch.
!
      CALL PXFORM ( DREF,  FIXREF, TRGEPC, XFORM )
      CALL MXV    ( XFORM, DVEC,   FIXDIR )

!
!     Compute the vector from the observer to the tangent point in
!     the body-fixed frame at the target epoch.
!
      CALL VLCOM3 ( 1.D0, SRFVEC, -1.D0, SRFPT, 1.D0, TANPT, FIXTAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check angular separation of ray direction and direction
!     to tangent point.
!
      SEP = VSEP( FIXDIR, FIXTAN )

      TOL = VTIGHT

      CALL CHCKSD ( 'RAY-OBS-TO-TANPT SEP', SEP, '~', 0.D0, TOL, OK )

!
!     Check the position of the tangent point above the surface point.
!
      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSUB ( TANPT, SRFPT, SRFTAN )

      SEP = VSEP ( SRFTAN, NORMAL )

!
!     Use looser tolerance here since the relative errors in TANPT
!     and SRFPT are larger compared to the lengths of those vectors
!     than they are relative to the observer-target distance.
!
      TOL = 1.D-11

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

!
!     Verify the zenith direction is normal to the ray direction.
!
      SEP = VSEP ( NORMAL, FIXDIR )

      CALL CHCKSD ( 'RAY ZENITH SEP', SEP, '~', HALFPI(), TOL, OK )

!
!     Check the altitude of the tangent point ALT.
!
      XALT = VDIST( SRFPT, TANPT )

!
!     Use the range from the observer to the tangent point to scale
!     the tolerance for the altitude check.
!
      TOL  = TIGHT * RANGE

      CALL CHCKSD ( 'ALT', ALT, '~', XALT, TOL, OK )

!
!     Get the position of the observer relative to the target center,
!     expressed in the target body-fixed frame. This call works only
!     for the geometric case.
!
      CALL SPKPOS ( OBSRVR, ET, FIXREF, 'NONE', TARGET, FIXOBS, LT )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check SRFVEC.
!
      CALL VSUB ( SRFPT, FIXOBS, XSRFVC )

      TOL = 1.D-13

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', XSRFVC, 3, TOL, OK )

!
!     Check range to the tangent point.
!
      XRANGE = VDIST( TANPT, FIXOBS )

      TOL = 1.D-13

      CALL CHCKSD ( 'RANGE', RANGE, '~/', XRANGE, TOL, OK )

!
!     Check the target epoch. In this case, it should match ET.
!
      XEPOCH = ET

      TOL    = VTIGHT

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

!
!  --- Case: ----------------------------------------------------------
!
      CALL TCASE ( 'Simple case 2: Phobos observes Mars, abcorr = ' &
      //           'CN+S. Ray points toward a point 350 km above ' &
      //           'Mars''s north pole. Use tangent point locus.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'Mars'

!
!     Don't use ET = 0.D0 for testing expected results; that could hide
!     uninitialized time errors.
!
      ET     = 31557600.D0

      FIXREF = 'IAU_MARS'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'Phobos'
      DREF   = 'J2000'

!
!     Get radii of Mars.
!
      CALL BODVRD ( 'MARS', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create target point FIXTRG in the body-fixed frame.
!
      H = 350.D0

      CALL VPACK( 0.D0, 0.D0,  1.D0,  ZVEC   )
      CALL VSCL ( RADII(3)+H,  ZVEC,  FIXTRG )

!
!     Compute the state of the target point relative to the observer
!     in the target body-fixed frame, evaluated at the target epoch.
!     Return the state expressed in the J2000 frame.
!
      CALL SPKCPT ( FIXTRG,   'MARS', FIXREF, ET,     DREF, &
                    'TARGET', ABCORR, OBSRVR, TRGSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( TRGSTA, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Convert the ray direction from the J2000 frame to the body-fixed
!     frame at the target epoch.
!
      CALL PXFORM ( DREF,  FIXREF, TRGEPC, XFORM )
      CALL MXV    ( XFORM, DVEC,   FIXDIR )

!
!     Compute the vector from the observer to the tangent point in
!     the body-fixed frame at the target epoch.
!
      CALL VLCOM3 ( 1.D0, SRFVEC, -1.D0, SRFPT, 1.D0, TANPT, FIXTAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check angular separation of ray direction and direction
!     to tangent point.
!
      SEP = VSEP( FIXDIR, FIXTAN )

      TOL = VTIGHT

      CALL CHCKSD ( 'RAY-OBS-TO-TANPT SEP', SEP, '~', 0.D0, TOL, OK )

!
!     Check the position of the tangent point above the surface point.
!
      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSUB ( TANPT, SRFPT, SRFTAN )

      SEP = VSEP ( SRFTAN, NORMAL )

      TOL = TIGHT

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

!
!     Verify the zenith direction is normal to the ray direction.
!
      SEP = VSEP ( NORMAL, FIXDIR )

      TOL = TIGHT

      CALL CHCKSD ( 'RAY ZENITH SEP', SEP, '~', HALFPI(), TOL, OK )

!
!     Check ALT.
!
      XALT = VDIST( SRFPT, TANPT )

!
!     Use the range from the observer to the tangent point to scale
!     the tolerance for the altitude check.
!
      TOL = TIGHT * RANGE

      CALL CHCKSD ( 'ALT', ALT, '~', XALT, TOL, OK )

!
!     Prepare to perform a consistency check on SRFVEC. We'll
!     use the target epoch associated with the tangent point to
!     compute the target body's position and orientation. We'll also
!     use the stellar aberration correction of the tangent point to
!     shift the target position. The target body's position and
!     orientation, along with the surface point, will be used to
!     compute an expected value of SRFVEC.
!
!     Get the position of the observer relative to the target center,
!     expressed in the target body-fixed frame. We must compute this
!     vector using the position of the observer relative to the solar
!     system barycenter at ET and the position of the target relative
!     to the solar system barycenter at TRGEPC.
!
!     This computation accounts for light time between the observer
!     and the tangent point. It does not account for stellar
!     aberration. That will be done separately.
!
      OBSCDE = 401
      TRGCDE = 499

      CALL SPKSSB ( OBSCDE, ET,     DREF, SSBOBS )
      CALL SPKSSB ( TRGCDE, TRGEPC, DREF, SSBTRG )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSUB ( SSBOBS, SSBTRG, J2OBS  )
      CALL MXV  ( XFORM,  J2OBS,  FIXOBS )

!
!     Now adjust the position of the observer relative to the target
!     to take into account the shift of the target by the stellar
!     aberration offset of the tangent point. We can get this offset
!     by computing the state of the tangent point with and without
!     stellar aberration correction.
!
!     Find the aberration-corrected position of the tangent point
!     relative to the observer, expressed in the J2000 frame.
!
      CALL SPKCPT ( TANPT,   'MARS',  FIXREF, ET,     DREF, &
                    'TARGET', ABCORR, OBSRVR, TANSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the light time corrected position of the tangent point
!     relative to the observer, expressed in the J2000 frame.
!
      CALL SPKCPT ( TANPT,    'MARS', FIXREF, ET,     DREF, &
                    'TARGET', 'CN',   OBSRVR, TANST1, LTTMP )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the stellar aberration offset of the tangent point,
!     expressed in the J2000 frame. Convert to the body-fixed frame
!     at epoch TRGEPC.
!
      CALL VSUB ( TANSTA, TANST1, STLOFF )

      CALL MXV  ( XFORM, STLOFF, FIXSTL )

!
!     Now compute the expected observer-to-surface vector, accounting
!     for the aberration corrections applicable to the tangent point.
!
      CALL VSUB ( SRFPT,  FIXOBS, TMPPOS )
      CALL VADD ( FIXSTL, TMPPOS, XSRFVC )

!
!     Check SRFVEC.
!
      TOL = TIGHT

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', XSRFVC, 3, TOL, OK )

!
!     Check range to the tangent point using the tangent point state.
!
      XRANGE = VNORM( TANSTA )

      TOL = TIGHT

      CALL CHCKSD ( 'RANGE', RANGE, '~/', XRANGE, TOL, OK )

!
!     Check the target epoch using the tangent point state.
!     It should match ET-LT, where LT is light time between the
!     tangent point and the observer.
!
      XEPOCH = ET - LT

      TOL    = VTIGHT

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

!
!     Compute the angular separation of the observer-tangent point
!     vector with the original ray direction. This differs from the
!     earlier test in that this time, the vector from the observer
!     to the tangent point was computed by SPKCPT.
!
      SEP = VSEP( TANSTA, DVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'TANSTA-DVEC SEP', SEP, '~', 0.D0, TOL, OK )

!
! --- Case: -----------------------------------------------------------
!
      CALL TCASE ( 'Simple case 3: Phobos observes Mars, abcorr = ' &
      //           'XCN+S. Ray points toward a point 350 km above ' &
      //           'Mars''s north pole. Use surface point locus.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'Mars'

!
!     Don't use ET = 0.D0 for testing expected results; that could hide
!     uninitialized time errors.
!
      ET     = 31557600.D0

      FIXREF = 'IAU_MARS'
      ABCORR = 'XCN+S'
      CORLOC = 'SURFACE POINT'
      OBSRVR = 'Phobos'
      DREF   = 'J2000'

!
!     Get radii of Mars.
!
      CALL BODVRD ( 'MARS', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Create target point FIXTRG in the body-fixed frame.
!
      H = 350.D0

      CALL VPACK( 0.D0, 0.D0,  1.D0,  ZVEC   )
      CALL VSCL ( RADII(3)+H,  ZVEC,  FIXTRG )

!
!     Compute the state of the target point relative to the observer
!     in the target body-fixed frame, evaluated at the target epoch.
!     Return the state expressed in the J2000 frame.
!
      CALL SPKCPT ( FIXTRG,   'MARS', FIXREF, ET,     DREF, &
                    'TARGET', ABCORR, OBSRVR, TRGSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( TRGSTA, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Convert the ray direction from the J2000 frame to the body-fixed
!     frame at the target epoch.
!
      CALL PXFORM ( DREF,  FIXREF, TRGEPC, XFORM )
      CALL MXV    ( XFORM, DVEC,   FIXDIR )

!
!     Compute the vector from the observer to the tangent point in
!     the body-fixed frame at the target epoch.
!
      CALL VLCOM3 ( 1.D0, SRFVEC, -1.D0, SRFPT, 1.D0, TANPT, FIXTAN )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Check angular separation of ray direction and direction
!     to tangent point.
!
      SEP = VSEP( FIXDIR, FIXTAN )

      TOL = VTIGHT

      CALL CHCKSD ( 'RAY-OBS-TO-TANPT SEP', SEP, '~', 0.D0, TOL, OK )

!
!     Check the position of the tangent point above the surface point.
!
      CALL SURFNM ( RADII(1), RADII(2), RADII(3), SRFPT, NORMAL )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSUB ( TANPT, SRFPT, SRFTAN )

      SEP = VSEP ( SRFTAN, NORMAL )

      TOL = TIGHT

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

!
!     Verify the zenith direction is normal to the ray direction.
!
      SEP = VSEP ( NORMAL, FIXDIR )

      TOL = TIGHT

      CALL CHCKSD ( 'RAY ZENITH SEP', SEP, '~', HALFPI(), TOL, OK )

!
!     Check ALT.
!
      XALT = VDIST( SRFPT, TANPT )

      TOL = TIGHT * RANGE

      CALL CHCKSD ( 'ALT', ALT, '~', XALT, TOL, OK )

!
!     Check SRFVEC. In order to compute an expected vector, we can
!     simply treat the surface point as an ephemeris object.
!
!     Find the aberration-corrected position of the surface point
!     relative to the observer, expressed in the target body-fixed
!     frame.
!
      CALL SPKCPT ( SRFPT,   'MARS',  FIXREF, ET,     FIXREF, &
                    'TARGET', ABCORR, OBSRVR, SRFSTA, LT     )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VEQU ( SRFSTA, XSRFVC )

!
!     This tolerance value was chosen to support the
!     PC/Windows/Intel Fortran platform. The previous value was 1.D-12.
!
      TOL = 2.0D-12

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', XSRFVC, 3, TOL, OK )

!
!     Compute the position of the observer relative to the target
!     center in the body-fixed frame.
!
      CALL VSUB ( SRFPT, SRFVEC, FIXOBS )

!
!     Check range to the tangent point.
!
      XRANGE = VDIST( TANPT, FIXOBS )

      TOL = TIGHT

      CALL CHCKSD ( 'RANGE', RANGE, '~/', XRANGE, TOL, OK )

!
!     Check the target epoch.
!
      XEPOCH = ET + LT

      TOL    = VTIGHT

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

!
!     Compute the angular separation of the observer-tangent point
!     vector with the original ray direction.
!
      CALL VSUB ( TANPT, FIXOBS, TMPPOS )
      CALL MTXV ( XFORM, TMPPOS, TANVEC )

      SEP = VSEP( TANVEC, DVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'TANSTA-DVEC SEP', SEP, '~', 0.D0, TOL, OK )


! *********************************************************************
!
!     Error cases
!
! *********************************************************************

!
! --- Case: -----------------------------------------------------------
!
      CALL TCASE ( 'Invalid aberration correction locus.' )

      CORLOC = 'XYZ'

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
! --- Case: -----------------------------------------------------------
!
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

!
!     Close out the test family.
!
      CALL T_SUCCESS ( OK )
      RETURN
      END
