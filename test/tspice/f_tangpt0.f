C$Procedure F_TANGPT0 ( Test tangent point routine, part 0 )

      SUBROUTINE F_TANGPT0 ( OK )

C$ Abstract
C
C     Perform tests on the tangent point routine TANGPT. This family
C     focuses on exception handling. See the test families f_tangpt*
C     for further tests.
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
C
C$ Version
C
C-    TSPICE Version 1.0.0, 05-OCT-2021 (NJB)
C
C-&

C
C     SPICELIB functions
C
      LOGICAL               EXISTS

C
C     Test utility functions
C

C
C     Local Parameters
C
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

C
C     Saved variables
C
      SAVE

C
C     Open the test family.
C
      CALL TOPEN ( 'F_TANGPT0' )

C*****************************************************************
C
C     Error cases
C
C*****************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Setup' )

C
C     Create and load LSK, then delete LSK.
C
      CALL TSTLSK

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

C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )

C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(IDCODENOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(ZEROVECTOR)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: consecutive calls with blank '
     .//           'aberration correction.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = ' '
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )

C
C     Make sure matching but invalid value of ABCORR is not
C     considered a recognized value.
C
      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDOPTION)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(NOTSUPPORTED)', OK )

C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(BODIESNOTDISTINCT)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )


C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: no orientation data available for target '
     .//           'frame.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'EARTH'
      ET     = 0.D0
      FIXREF = 'ITRF93'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'MOON'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(KERNELVARNOTFOUND)', OK )

C
C     Restore Moon radii.
C
      CALL LDPOOL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
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


      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDCOUNT)', OK )

C
C     Restore Moon radii.
C
      CALL LDPOOL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
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


      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(ARRAYTOOSMALL)', OK )

C
C     Restore Moon radii.
C
      CALL LDPOOL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(NOFRAME)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: no orientation data available for ray '
     .//           'frame.' )

      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'ITRF93'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(FRAMEDATANOTFOUND)', OK )


C
C --- Case: ------------------------------------------------------
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Error: no ephemeris data available for target.' )

C
C     We need to have a target frame available, but no ephemeris data
C     for the target. We need to have ephemeris data for the observer.
C     Pick a target for which we have frame orientation in a generic
C     PCK file.
C
C     Create test PCK containing data from pck00010.tpc. Clear out
C     existing PCK data first.
C
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

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(SPKINSUFFDATA)', OK )

C
C     Restore default text kernels.
C
      CALL TSTLSK
      CALL LDPOOL ( PCK0 )
      CALL CHCKXC  ( .FALSE., ' ', OK )

C
C --- Case: ------------------------------------------------------
C
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

C
C     Make the Moon's radii long enough so the Moon envelops the Earth.
C
      CALL FILLD ( 1.D6, 3, DVALS )

      CALL PDPOOL ( 'BODY301_RADII', 3, DVALS )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )

      CALL CHCKXC ( .TRUE., 'SPICE(INVALIDGEOMETRY)', OK )

C
C     Restore target radii.
C
      CALL LDPOOL ( PCK0 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C*****************************************************************
C
C     Non-error exception cases
C
C*****************************************************************

C
C --- Case: ------------------------------------------------------
C
      CALL TCASE ( 'Change observer name-ID association.' )

C
C     Perform normal computation with the Earth as observer.
C
      METHOD = 'ELLIPSOID'
      TARGET = 'MOON'
      ET     = 0.D0
      FIXREF = 'IAU_MOON'
      ABCORR = 'CN+S'
      CORLOC = 'TANGENT POINT'
      OBSRVR = 'EARTH'
      DREF   = 'J2000'
      CALL VPACK ( 1.D0, 0.D0, 0.D0, DVEC )

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF,  ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,    XTANPT,
     .              XALT,   XRANGE, XSRFPT, XTRGEP, XSRFVC )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Now map the name JUPITER to the ID code 399.
C
      CALL PCPOOL ( 'NAIF_BODY_NAME', 1, 'JUPITER' )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL PIPOOL ( 'NAIF_BODY_CODE', 1, 399 )
      CALL CHCKXC ( .FALSE., ' ', OK )

C
C     Repeat the call, using JUPITER as the observer name.
C
      OBSRVR = 'EARTH'

      CALL TANGPT ( METHOD, TARGET, ET,    FIXREF, ABCORR,
     .              CORLOC, OBSRVR, DREF,  DVEC,   TANPT,
     .              ALT,    RANGE,  SRFPT, TRGEPC, SRFVEC )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL CHCKSD ( 'ALT',   ALT,    '=', XALT,      0.D0, OK )
      CALL CHCKSD ( 'RANGE', RANGE,  '=', XRANGE,    0.D0, OK )
      CALL CHCKAD ( 'TANPT', TANPT,  '=', XTANPT, 3, 0.D0, OK )
      CALL CHCKAD ( 'SRFPT', SRFPT,  '=', XSRFPT, 3, 0.D0, OK )

C
C     TODO: Check handling of invalidation of cached ID codes and other
C     saved values.
C


C
C --- Case: ------------------------------------------------------
C
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

C
C     Close out the test family.
C
      CALL T_SUCCESS ( OK )
      RETURN
      END
