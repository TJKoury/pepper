!$Procedure F_TANGPT1 ( Test tangent point routine, part 1 )

      SUBROUTINE F_TANGPT1 ( OK )

! $ Abstract
!
!     Perform tests on the tangent point routine TANGPT. This family
!     tests a few basic cases. See the test families f_tangpt* for
!     further tests.
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
!
! $ Version
!
!-    TSPICE Version 1.0.0, 05-OCT-2021 (NJB)
!
!-&

!
!     SPICELIB functions
!
      DOUBLE PRECISION      CLIGHT
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
      PARAMETER           ( TIGHT  = 1.D-12 )

      DOUBLE PRECISION      VTIGHT
      PARAMETER           ( VTIGHT = 1.D-14 )

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
      DOUBLE PRECISION      NORMAL ( 3 )
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      SEP
      DOUBLE PRECISION      SRFPT  ( 3 )
      DOUBLE PRECISION      SRFTAN ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      SSBOBS ( 6 )
      DOUBLE PRECISION      SSBTRG ( 6 )
      DOUBLE PRECISION      STLOFF ( 3 )
      DOUBLE PRECISION      TANPT  ( 3 )
      DOUBLE PRECISION      TANSTA ( 6 )
      DOUBLE PRECISION      TANST1 ( 6 )
      DOUBLE PRECISION      TMPPOS ( 3 )
      DOUBLE PRECISION      TOL
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      TRGSTA ( 6 )
      DOUBLE PRECISION      XALT
      DOUBLE PRECISION      XEPOCH
      DOUBLE PRECISION      XFORM  ( 3, 3 )
      DOUBLE PRECISION      XRANGE
      DOUBLE PRECISION      XSRFPT ( 3 )
      DOUBLE PRECISION      XSRFVC ( 3 )
      DOUBLE PRECISION      XTANPT ( 3 )
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
      CALL TOPEN ( 'F_TANGPT1' )

!*****************************************************************
!
!     Error cases
!
!*****************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Setup' )

!
!     Create and load LSK, then delete LSK.
!
      CALL TSTLSK

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


!*****************************************************************
!
!     Normal cases
!
!*****************************************************************


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple case 1: Earth observes Moon, abcorr = ' &
      //           'NONE. Ray points toward a point 100 km above ' &
      //           'the Moon''s north pole. Use tangent point locus. ' &
      //           'Use modified Moon radii: make the shape a ' &
      //           'true triaxial ellipsoid.' )

      METHOD = 'ELLIPSOID'
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
!     Get radii of Moon. Replace radii with values giving a true
!     triaxial shape: radii are a, a/2, a/3.
!
      CALL BODVRD ( 'MOON', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RADII(2) = RADII(1) / 2
      RADII(3) = RADII(1) / 3

      CALL PDPOOL ( 'BODY301_RADII', 3, RADII )
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
      TOL = TIGHT

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
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple case 2: Earth observes Moon, abcorr = ' &
      //           'NONE. Ray points toward a point 100 km above ' &
      //           'the Moon''s north pole. Use surface point locus.' )

!
!     Because we're using geometric computations, all output quantities
!     should match those from the previous case.
!
      XEPOCH = TRGEPC
      XALT   = ALT
      XRANGE = RANGE

      CALL VEQU( TANPT,  XTANPT )
      CALL VEQU( SRFPT,  XSRFPT )
      CALL VEQU( SRFVEC, XSRFVC )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'

!
!     Don't use ET = 0.D0 for testing expected results; that time
!     value could hide uninitialized time errors.
!
      ET     = 31557600.D0

      FIXREF = 'IAU_MOON'
      ABCORR = 'NONE'
      CORLOC = 'SURFACE POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      TOL = VTIGHT

      CALL CHCKSD ( 'RANGE',  RANGE,  '~/',  XRANGE, TOL, OK )
      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~',   XEPOCH, TOL, OK )
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', XSRFVC, 3, TOL, OK )

      TOL = TIGHT * RANGE

      CALL CHCKSD ( 'ALT',    ALT,    '~',   XALT,   TOL, OK )

      TOL = TIGHT

      CALL CHCKAD ( 'TANPT',  TANPT,  '~~/', XTANPT, 3, TOL, OK )
      CALL CHCKAD ( 'SRFPT',  SRFPT,  '~~/', XSRFPT, 3, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple case 3: Earth observes Moon. ABCORR = ' &
      //           'CN. Ray points toward a point 100 km above ' &
      //           'the Moon''s north pole. Use tangent point locus. ' &
      //           'Use modified Moon radii: make the shape a ' &
      //           'true triaxial ellipsoid. The geometric position ' &
      //           'of the Moon is used to compute the ray''s ' &
      //           'direction vector. Use surface point locus.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'

!
!     Don't use ET = 0.D0 for testing expected results; that could hide
!     uninitialized time errors.
!
      ET     = 31557600.D0

      FIXREF = 'IAU_MOON'
      ABCORR = 'CN'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'

!
!     Get radii of Moon. Replace radii with values giving a true
!     triaxial shape: radii are a, a/2, a/3.
!
      CALL BODVRD ( 'MOON', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RADII(2) = RADII(1) / 2
      RADII(3) = RADII(1) / 3

      CALL PDPOOL ( 'BODY301_RADII', 3, RADII )
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
                    'TARGET', ABCORR, OBSRVR, TRGSTA, LT    )
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
      TOL = TIGHT

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

!
!     Verify the zenith direction is normal to the ray direction.
!
      SEP = VSEP ( NORMAL, FIXDIR )

      TOL = TIGHT

      CALL CHCKSD ( 'RAY ZENITH SEP', SEP, '~', HALFPI(), TOL, OK )

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
      TOL = TIGHT

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

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
!     Get the position of the observer relative to the target center,
!     expressed in the target body-fixed frame. We must compute this
!     vector using the position of the observer relative to the solar
!     system barycenter at ET and the position of the target relative
!     to the solar system barycenter at TRGEPC.
!
      OBSCDE = 399
      TRGCDE = 301

      CALL SPKSSB ( OBSCDE, ET,     DREF, SSBOBS )
      CALL SPKSSB ( TRGCDE, TRGEPC, DREF, SSBTRG )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL VSUB ( SSBOBS, SSBTRG, J2OBS  )
      CALL MXV  ( XFORM,  J2OBS,  FIXOBS )

!
!     Check SRFVEC.
!
      CALL VSUB ( SRFPT, FIXOBS, XSRFVC )

      TOL = VTIGHT

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', XSRFVC, 3, TOL, OK )
      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~', XSRFVC, 3, TOL, OK )

!
!     Check range to the tangent point.
!
      XRANGE = VDIST( TANPT, FIXOBS )

      TOL = VTIGHT

      CALL CHCKSD ( 'RANGE', RANGE, '~/', XRANGE, TOL, OK )

!
!     Check the target epoch.
!
      XEPOCH = ET - ( RANGE / CLIGHT() )

      TOL    = VTIGHT

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

!
!     Find the aberration-corrected position of the tangent point
!     relative to the observer, expressed in the J2000 frame.
!
      CALL SPKCPT ( TANPT,   'MOON',  FIXREF, ET,     DREF, &
                    'TARGET', ABCORR, OBSRVR, TANSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Compute the angular separation of the observer-tangent point
!     vector with the original ray direction.
!
      SEP = VSEP( TANSTA, DVEC )

      TOL    = 1.D-13

      CALL CHCKSD ( 'TANSTA-DVEC SEP', SEP, '~', 0.D0, TOL, OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple case 4: Earth observes Moon, abcorr = ' &
      //           'CN+S. Ray points toward a point 100 km above ' &
      //           'the Moon''s north pole. Use tangent point locus. ' &
      //           'Use modified Moon radii: make the shape a ' &
      //           'true triaxial ellipsoid.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'

!
!     Don't use ET = 0.D0 for testing expected results; that could hide
!     uninitialized time errors.
!
      ET     = 31557600.D0

      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'

!
!     Get radii of Moon. Replace radii with values giving a true
!     triaxial shape: radii are a, a/2, a/3.
!
      CALL BODVRD ( 'MOON', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RADII(2) = RADII(1) / 2
      RADII(3) = RADII(1) / 3

      CALL PDPOOL ( 'BODY301_RADII', 3, RADII )
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
                    'TARGET', ABCORR, OBSRVR, TRGSTA, LT    )
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
      TOL  = TIGHT * RANGE

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
      OBSCDE = 399
      TRGCDE = 301

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
      CALL SPKCPT ( TANPT,   'MOON',  FIXREF, ET,     DREF, &
                    'TARGET', ABCORR, OBSRVR, TANSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Find the light time-corrected position of the tangent point
!     relative to the observer, expressed in the J2000 frame.
!
      CALL SPKCPT ( TANPT,    'MOON', FIXREF, ET,     DREF, &
                    'TARGET', 'CN',   OBSRVR, TANST1, LT   )
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
      TOL = VTIGHT

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', XSRFVC, 3, TOL, OK )

!
!     Check range to the tangent point using the tangent point state.
!
      XRANGE = VDIST( TANPT, FIXOBS )

      TOL = VTIGHT

      CALL CHCKSD ( 'RANGE', RANGE, '~/', XRANGE, TOL, OK )

!
!     Check the target epoch using the tangent point state.
!     It should match ET-LT, where LT is light time between the
!     tangent point and the observer.
!
      XEPOCH = ET - ( RANGE / CLIGHT() )

      TOL    = VTIGHT

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

!
!     Compute the angular separation of the observer-tangent point
!     vector with the original ray direction. This differs from the
!     earlier test in that this time, the vector from the observer
!     to the tangent point was computed by SPKCPT.
!
      SEP = VSEP( TANSTA, DVEC )

      TOL    = 1.D-13

      CALL CHCKSD ( 'TANSTA-DVEC SEP', SEP, '~', 0.D0, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple case 5: Phobos observes Mars, abcorr = ' &
      //           'NONE. Ray points toward a point 100 km above ' &
      //           'Mars''s north pole. Use tangent point locus. ' &
      //           'Use modified Mars radii: make the shape a ' &
      //           'true triaxial ellipsoid.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'Mars'

!
!     Don't use ET = 0.D0 for testing expected results; that could hide
!     uninitialized time errors.
!
      ET     = 31557600.D0

      FIXREF = 'IAU_MARS'
      ABCORR = 'NONE'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'Phobos'
      DREF   = 'J2000'

!
!     Get radii of Mars. Replace radii with values giving a true
!     triaxial shape: radii are a, a/2, a/3.
!
      CALL BODVRD ( 'MARS', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RADII(2) = RADII(1) / 2
      RADII(3) = RADII(1) / 3

      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
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
      CALL SPKCPT ( FIXTRG,   'MARS', FIXREF, ET,     DREF, &
                    'TARGET', ABCORR, OBSRVR, TRGSTA, LT      )
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
      TOL = TIGHT

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

!
!     Verify the zenith direction is normal to the ray direction.
!
      SEP = VSEP ( NORMAL, FIXDIR )

      TOL = TIGHT

      CALL CHCKSD ( 'RAY ZENITH SEP', SEP, '~', HALFPI(), TOL, OK )

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
      TOL = TIGHT

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

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

      TOL = TIGHT

      CALL CHCKAD ( 'SRFVEC', SRFVEC, '~~/', XSRFVC, 3, TOL, OK )

!
!     Check range to the tangent point.
!
      XRANGE = VDIST( TANPT, FIXOBS )

      TOL = TIGHT

      CALL CHCKSD ( 'RANGE', RANGE, '~/', XRANGE, TOL, OK )

!
!     Check the target epoch. In this case, it should match ET.
!
      XEPOCH = ET

      TOL    = VTIGHT

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

!
!     Compute the angular separation of the observer-tangent point
!     vector with the original ray direction.
!
      CALL SPKCPT ( TANPT,    TARGET, FIXREF, ET,     DREF, &
                    'TARGET', ABCORR, OBSRVR, TANSTA, LT   )
      CALL CHCKXC ( .FALSE., ' ', OK )

      SEP = VSEP( TANSTA, DVEC )

      TOL = TIGHT

      CALL CHCKSD ( 'TANSTA-DVEC SEP', SEP, '~', 0.D0, TOL, OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Simple case 6: Phobos observes Mars, abcorr = ' &
      //           'CN+S. Ray points toward a point 100 km above ' &
      //           'Mars''s north pole. Use tangent point locus. ' &
      //           'Use modified Mars radii: make the shape a ' &
      //           'true triaxial ellipsoid.' )

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
!     Get radii of Mars. Replace radii with values giving a true
!     triaxial shape: radii are a, a/2, a/3.
!
      CALL BODVRD ( 'MARS', 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      RADII(2) = RADII(1) / 2
      RADII(3) = RADII(1) / 3

      CALL PDPOOL ( 'BODY499_RADII', 3, RADII )
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
      CALL SPKCPT ( FIXTRG,   'MARS', FIXREF, ET,     DREF, &
                    'TARGET', ABCORR, OBSRVR, TRGSTA, LT      )
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
      TOL = TIGHT

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

!
!     Verify the zenith direction is normal to the ray direction.
!
      SEP = VSEP ( NORMAL, FIXDIR )

      TOL = TIGHT

      CALL CHCKSD ( 'RAY ZENITH SEP', SEP, '~', HALFPI(), TOL, OK )

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
      TOL = TIGHT

      CALL CHCKSD ( 'SRFPT-TANPT ZENITH SEP', SEP, '~', 0.D0, TOL, OK )

!
!     Check ALT.
!
      XALT = VDIST( SRFPT, TANPT )

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
!     Find the light time-corrected position of the tangent point
!     relative to the observer, expressed in the J2000 frame.
!
      CALL SPKCPT ( TANPT,    'MARS', FIXREF, ET,     DREF, &
                    'TARGET', 'CN',   OBSRVR, TANST1, LT   )
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
!     Check the target epoch.
!
      XEPOCH = ET - ( RANGE / CLIGHT() )

      TOL    = VTIGHT

      CALL CHCKSD ( 'TRGEPC', TRGEPC, '~', XEPOCH, TOL, OK )

!
!     Compute the angular separation of the observer-tangent point
!     vector with the original ray direction.
!
      SEP = VSEP( TANSTA, DVEC )

!
!     Use a tolerance approximately equal to the angular extent of
!     0.1 mm at the range of the tangent point.
!
      TOL = 1.D-7 / RANGE

      CALL CHCKSD ( 'TANSTA-DVEC SEP', SEP, '~', 0.D0, TOL, OK )

!
! --- Case: ------------------------------------------------------
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
