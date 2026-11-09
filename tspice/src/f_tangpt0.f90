!$Procedure F_TANGPT0 ( Test tangent point routine, part 0 )

      SUBROUTINE F_TANGPT0 ( OK )
      use pepper_inc_zzdsk
      use pepper_inc_zzabcorr

! $ Abstract
!
!     Perform tests on the tangent point routine TANGPT. This family
!     focuses on exception handling. See the test families f_tangpt*
!     for further tests.
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
      LOGICAL               EXISTS

!
!     Test utility functions
!

!
!     Local Parameters
!
      CHARACTER*(*)         PCK0
      PARAMETER           ( PCK0 = 'tangpt_test0.tpc' )

      CHARACTER*(*)         PCK1
      PARAMETER           ( PCK1 = 'tangpt_test0.tpc' )

      CHARACTER*(*)         SPK0
      PARAMETER           ( SPK0 = 'tangpt_test0.bsp' )


      INTEGER               BDNMLN
      PARAMETER           ( BDNMLN = 36 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               LOCLEN
      PARAMETER           ( LOCLEN = 20 )

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
      DOUBLE PRECISION      DVALS  ( 4 )
      DOUBLE PRECISION      DVEC   ( 3 )
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RADII  ( 3 )
      DOUBLE PRECISION      RANGE
      DOUBLE PRECISION      SRFPT  ( 3 )
      DOUBLE PRECISION      SRFVEC ( 3 )
      DOUBLE PRECISION      TANPT  ( 3 )
      DOUBLE PRECISION      TRGEPC
      DOUBLE PRECISION      XALT
      DOUBLE PRECISION      XRANGE
      DOUBLE PRECISION      XTANPT ( 3 )
      DOUBLE PRECISION      XTRGEP
      DOUBLE PRECISION      XSRFPT ( 3 )
      DOUBLE PRECISION      XSRFVC ( 3 )

      INTEGER               HANDLE
      INTEGER               I
      INTEGER               NRAD

!
!     Saved variables
!
      SAVE

!
!     Open the test family.
!
      CALL TOPEN ( 'F_TANGPT0' )

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

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: no ID code for target.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'XMOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: no ID code for observer.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'XEARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: no ID code for observer.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'XEARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: zero-length normal vector.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL CLEARD ( 3, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: invalid aberration correction.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN-S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: consecutive calls with blank ' &
      //           'aberration correction.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = ' '
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

!
!     Make sure matching but invalid value of ABCORR is not
!     considered a recognized value.
!
      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: invalid aberration correction locus.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGPT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: observer is target.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'MOON'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: target frame not recognized.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOOOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: no orientation data available for target ' &
      //           'frame.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      ET     = 0.D0
      FIXREF = 'ITRF93'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'MOON'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: target frame center is not target.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: target radii unavailable.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_EARTH'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL DVPOOL ( 'BODY301_RADII' )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

!
!     Restore Moon radii.
!
      CALL LDPOOL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: too few target radii; 3 required.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL BODVCD ( 301, 'RADII', 3, NRAD, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PDPOOL ( 'BODY301_RADII', 2, RADII )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

!
!     Restore Moon radii.
!
      CALL LDPOOL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: too many target radii; 3 required.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      DO I = 1, 4
         DVALS(I) = DBLE(I)
      END DO

      CALL PDPOOL ( 'BODY301_RADII', 4, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )


      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )

!
!     Restore Moon radii.
!
      CALL LDPOOL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: ray frame not recognized.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'XYZ'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: no orientation data available for ray ' &
      //           'frame.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'ITRF93'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: no ephemeris data available for observer.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'GASPRA'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: no ephemeris data available for target.' )

!
!     We need to have a target frame available, but no ephemeris data
!     for the target. We need to have ephemeris data for the observer.
!     Pick a target for which we have frame orientation in a generic
!     PCK file.
!
!     Create test PCK containing data from pck00010.tpc. Clear out
!     existing PCK data first.
!
      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL( PCK1 )
         CALL CHCKXC ( .FALSE., ' ', OK )
      END IF

      CALL CLPOOL
      CALL T_PCK10 ( PCK1, .TRUE., .TRUE. )
      CALL CHCKXC  ( .FALSE., ' ', OK )

      METHOD = 'ELLIPSOID'
      TARGET = 'GASPRA'
      ET     = 0.D0
      FIXREF = 'IAU_GASPRA'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

!
!     Restore default text kernels.
!
      CALL TSTLSK
      CALL LDPOOL ( PCK0 )
      CALL CHCKXC  ( .FALSE., ' ', OK )

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Error: observer is inside target ellipsoid.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

!
!     Make the Moon's radii long enough so the Moon envelops the Earth.
!
      CALL FILLD ( 1.D6, 3, DVALS )

      CALL PDPOOL ( 'BODY301_RADII', 3, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDGEOMETRY)', OK )

!
!     Restore target radii.
!
      CALL LDPOOL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!*****************************************************************
!
!     Non-error exception cases
!
!*****************************************************************

!
! --- Case: ------------------------------------------------------
!
      CALL TCASE ( 'Change observer name-ID association.' )

!
!     Perform normal computation with the Earth as observer.
!
      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF,  ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,    XTANPT, &
                    XALT,   XRANGE, XSRFPT, XTRGEP, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Now map the name JUPITER to the ID code 399.
!
      CALL PCPOOL ( 'NAIF_BODY_NAME', 1, 'JUPITER' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_BODY_CODE', 1, 399 )
      CALL CHCKXC ( .FALSE., ' ', OK )

!
!     Repeat the call, using JUPITER as the observer name.
!
      OBSRVR = 'EARTH'

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR, &
                    CORLOC, OBSRVR, DREF,  DVEC,   TANPT, &
                    ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ALT',   ALT,    '=', XALT,      0.D0, OK )
      CALL CHCKSD ( 'RANGE', RANGE,  '=', XRANGE,    0.D0, OK )
      CALL CHCKAD ( 'TANPT', TANPT,  '=', XTANPT, 3, 0.D0, OK )
      CALL CHCKAD ( 'SRFPT', SRFPT,  '=', XSRFPT, 3, 0.D0, OK )

!
!     TODO: Check handling of invalidation of cached ID codes and other
!     saved values.
!


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

      IF ( EXISTS(PCK1) ) THEN
         CALL DELFIL( PCK1 )
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
