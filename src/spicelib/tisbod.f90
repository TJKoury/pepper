!$Procedure TISBOD ( Transformation, inertial state to bodyfixed )

      SUBROUTINE TISBOD ( REF, BODY, ET, TSIPM )

! $ Abstract
!
!     Return a 6x6 matrix that transforms states in inertial
!     coordinates to states in body-equator-and-prime-meridian
!     coordinates.
!
! $ Required_Reading
!
!     FRAMES
!     PCK
!     NAIF_IDS
!     ROTATION
!     TIME
!
! $ Keywords
!
!     ROTATION
!     TRANSFORMATION
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE               'errhnd.inc'
      INCLUDE               'frmtyp.inc'
      INCLUDE               'zzctr.inc'

      CHARACTER*(*)         REF
      INTEGER               BODY
      DOUBLE PRECISION      ET
      DOUBLE PRECISION      TSIPM   ( 6,6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     REF        I   ID of inertial reference frame to transform from
!     BODY       I   ID code of body
!     ET         I   Epoch of transformation
!     TSIPM      O   Transformation (state), inertial to prime meridian
!
! $ Detailed_Input
!
!     REF      is the NAIF name for an inertial reference frame.
!              Acceptable names include:
!
!                 Name       Description
!                 --------   --------------------------------
!                 'J2000'    Earth mean equator, dynamical
!                            equinox of J2000
!
!                 'B1950'    Earth mean equator, dynamical
!                            equinox of B1950
!
!                 'FK4'      Fundamental Catalog (4)
!
!                 'DE-118'   JPL Developmental Ephemeris (118)
!
!                 'DE-96'    JPL Developmental Ephemeris ( 96)
!
!                 'DE-102'   JPL Developmental Ephemeris (102)
!
!                 'DE-108'   JPL Developmental Ephemeris (108)
!
!                 'DE-111'   JPL Developmental Ephemeris (111)
!
!                 'DE-114'   JPL Developmental Ephemeris (114)
!
!                 'DE-122'   JPL Developmental Ephemeris (122)
!
!                 'DE-125'   JPL Developmental Ephemeris (125)
!
!                 'DE-130'   JPL Developmental Ephemeris (130)
!
!                 'GALACTIC' Galactic System II
!
!                 'DE-200'   JPL Developmental Ephemeris (200)
!
!                 'DE-202'   JPL Developmental Ephemeris (202)
!
!              See the Frames Required Reading frames.req for a full
!              list of inertial reference frame names built into
!              SPICE.
!
!              The output TSIPM will give the transformation
!              from this frame to the bodyfixed frame specified by
!              BODY at the epoch specified by ET.
!
!     BODY     is the integer ID code of the body for which the
!              state transformation matrix is requested. Bodies
!              are numbered according to the standard NAIF numbering
!              scheme. The numbering scheme is explained in the NAIF
!              IDs Required Reading naif_ids.req.
!
!     ET       is the epoch at which the state transformation
!              matrix is requested. (This is typically the
!              epoch of observation minus the one-way light time
!              from the observer to the body at the epoch of
!              observation.)
!
! $ Detailed_Output
!
!     TSIPM    is a 6x6 transformation matrix. It is used to
!              transform states from inertial coordinates to body
!              fixed (also called equator and prime meridian ---
!              PM) coordinates.
!
!              Given a state S in the inertial reference frame
!              specified by REF, the corresponding bodyfixed state
!              is given by the matrix vector product:
!
!                 TSIPM * S
!
!              The X axis of the PM system is directed  to the
!              intersection of the equator and prime meridian.
!              The Z axis points along  the spin axis and points
!              towards the same side of the invariable plane of
!              the solar system as does earth's north pole.
!
!              NOTE: The inverse of TSIPM is NOT its transpose.
!              The matrix, TSIPM, has a structure as shown below:
!
!                 .-            -.
!                 |       :      |
!                 |   R   :  0   |
!                 | ......:......|
!                 |       :      |
!                 | dR/dt :  R   |
!                 |       :      |
!                 `-            -'
!
!              where R is a time varying rotation matrix and dR/dt is
!              its derivative. The inverse of this matrix is:
!
!                 .-              -.
!                 |     T  :       |
!                 |    R   :  0    |
!                 | .......:.......|
!                 |        :       |
!                 |      T :   T   |
!                 | dR/dt  :  R    |
!                 |        :       |
!                 `-              -'
!
!              The SPICELIB routine INVSTM is available for producing
!              this inverse.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If data required to define the body-fixed frame associated
!         with BODY are not found in the binary PCK system or the kernel
!         pool, the error SPICE(FRAMEDATANOTFOUND) is signaled. In
!         the case of IAU style body-fixed frames, the absence of
!         prime meridian polynomial data (which are required) is used
!         as an indicator of missing data.
!
!     2)  If the test for exception (1) passes, but in fact requested
!         data are not available in the kernel pool, an error is
!         signaled by a routine in the call tree of this routine.
!
!     3)  If the kernel pool does not contain all of the data required
!         to define the number of nutation precession angles
!         corresponding to the available nutation precession
!         coefficients, the error SPICE(INSUFFICIENTANGLES) is
!         signaled.
!
!     4)  If the reference frame REF is not recognized, an error is
!         signaled by a routine in the call tree of this routine.
!
!     5)  If the specified body code BODY is not recognized, an error is
!         signaled by a routine in the call tree of this routine.
!
!     6)  If, for a given body, both forms of the kernel variable names
!
!            BODY<body ID>_CONSTANTS_JED_EPOCH
!            BODY<body ID>_CONSTS_JED_EPOCH
!
!         are found in the kernel pool, the error
!         SPICE(COMPETINGEPOCHSPEC) is signaled. This is done
!         regardless of whether the values assigned to the kernel
!         variable names match.
!
!     7)  If, for a given body, both forms of the kernel variable names
!
!            BODY<body ID>_CONSTANTS_REF_FRAME
!            BODY<body ID>_CONSTS_REF_FRAME
!
!         are found in the kernel pool, the error
!         SPICE(COMPETINGFRAMESPEC) is signaled. This is done
!         regardless of whether the values assigned to the kernel
!         variable names match.
!
!     8)  If the central body associated with the input BODY, whether
!         a system barycenter or BODY itself, has associated phase
!         angles (aka nutation precession angles), and the kernel
!         variable BODY<body ID>_MAX_PHASE_DEGREE for the central
!         body is present but has a value outside the range 1:3,
!         the error SPICE(DEGREEOUTOFRANGE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Note: NAIF recommends the use of SPKEZR with the appropriate
!     frames kernels when possible over TISBOD.
!
!     The matrix for transforming inertial states to bodyfixed
!     states is the 6x6 matrix shown below as a block structured
!     matrix.
!
!        .-            -.
!        |       :      |
!        | TIPM  :  0   |
!        | ......:......|
!        |       :      |
!        | DTIPM : TIPM |
!        |       :      |
!        `-            -'
!
!     This can also be expressed in terms of Euler angles
!     PHI, DELTA and W. The transformation from inertial to
!     bodyfixed coordinates is represented in the SPICE kernel
!     pool as:
!
!        TIPM   = [W]  [DELTA]  [PHI]
!                    3        1      3
!     Thus
!
!        DTIPM  = d[W] /dt [DELTA]  [PHI]
!                     3           1      3
!
!               + [W]  d[DELTA] /dt  [PHI]
!                    3             1      3
!
!               + [W]  [DELTA]  d[PHI] /dt
!                    3        1           3
!
!
!     If a binary PCK file record can be used for the time and
!     body requested, it will be used. The most recently loaded
!     binary PCK file has first priority, followed by previously
!     loaded binary PCK files in backward time order. If no
!     binary PCK file has been loaded, the text P_constants
!     kernel file is used.
!
!     If there is only text PCK kernel information, it is
!     expressed in terms of RA, DEC and W, where
!
!        RA  = PHI - HALFPI
!        DEC = HALFPI - DELTA
!        W   = W
!
!     The angles RA, DEC, and W are defined as follows in the
!     text PCK file:
!
!                                      2    .-----
!                      RA1*t      RA2*t      \
!        RA  = RA0  + -------  + -------   +  )  a(i) * sin( theta(i) )
!                        T          2        /
!                                  T        '-----
!                                              i
!
!                                       2   .-----
!                      DEC1*t     DEC2*t     \
!        DEC = DEC0 + -------- + --------  +  )  d(i) * cos( theta(i) )
!                        T           2       /
!                                   T       '-----
!                                              i
!
!                                     2     .-----
!                       W1*t      W2*t       \
!        W   = W0   +  ------  + -------   +  )  w(i) * sin( theta(i) )
!                        d          2        /
!                                  d        '-----
!                                              i
!
!
!     where `d' is in seconds/day; T in seconds/Julian century;
!     a(i), d(i), and w(i) arrays apply to satellites only; and
!     theta(i), defined as
!
!                                THETA1(i)*t
!        theta(i) = THETA0(i) + -------------
!                                     T
!
!     are specific to each planet.
!
!     These angles ---typically nodal rates--- vary in number and
!     definition from one planetary system to the next.
!
!     Thus
!
!                                   .-----
!                 RA1     2*RA2*t    \   a(i)*THETA1(i)*cos(theta(i))
!     dRA/dt   = ----- + --------- +  ) ------------------------------
!                  T          2      /                 T
!                            T      '-----
!                                      i
!
!                                     .-----
!                 DEC1     2*DEC2*t    \   d(i)*THETA1(i)*sin(theta(i))
!      dDEC/dt = ------ + ---------- -  ) ------------------------------
!                   T          2       /                 T
!                             T       '-----
!                                        i
!
!                                 .-----
!                 W1     2*W2*t    \   w(i)*THETA1(i)*cos(theta(i))
!      dW/dt   = ---- + -------- +  ) ------------------------------
!                 d         2      /                 T
!                          d      '-----
!                                    i
!
! $ Examples
!
!     The numerical results shown for these examples may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Calculate the matrix to transform a state vector from the
!        J2000 frame to the Saturn fixed frame at a specified
!        time, and use it to compute the geometric position and
!        velocity of Titan in Saturn's body-fixed frame.
!
!        Use the meta-kernel shown below to load the required SPICE
!        kernels.
!
!
!           KPL/MK
!
!           File name: tisbod_ex1.tm
!
!           This meta-kernel is intended to support operation of SPICE
!           example programs. The kernels shown here should not be
!           assumed to contain adequate or correct versions of data
!           required by SPICE-based user applications.
!
!           In order for an application to use this meta-kernel, the
!           kernels referenced here must be present in the user's
!           current working directory.
!
!           The names and contents of the kernels referenced
!           by this meta-kernel are as follows:
!
!              File name                     Contents
!              ---------                     --------
!              sat375.bsp                    Saturn satellite ephemeris
!              pck00010.tpc                  Planet orientation and
!                                            radii
!              naif0012.tls                  Leapseconds
!
!
!           \begindata
!
!              KERNELS_TO_LOAD = ( 'sat375.bsp',
!                                  'pck00010.tpc',
!                                  'naif0012.tls'  )
!
!           \begintext
!
!           End of meta-kernel
!
!
!        Example code begins here.
!
!
!              PROGRAM TISBOD_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      LT
!              DOUBLE PRECISION      STATE  ( 6    )
!              DOUBLE PRECISION      SATVEC ( 6    )
!              DOUBLE PRECISION      TSIPM  ( 6, 6 )
!
!              INTEGER               I
!              INTEGER               SATID
!
!        C
!        C     Load the kernels.
!        C
!              CALL FURNSH ( 'tisbod_ex1.tm' )
!
!        C
!        C     The body ID for Saturn.
!        C
!              SATID = 699
!
!        C
!        C     Retrieve the transformation matrix at some time.
!        C
!              CALL STR2ET ( 'Jan 1 2005',   ET        )
!              CALL TISBOD ( 'J2000', SATID, ET, TSIPM )
!
!        C
!        C     Retrieve the state of Titan as seen from Saturn
!        C     in the J2000 frame at ET.
!        C
!              CALL SPKEZR ( 'TITAN',  ET,    'J2000', 'NONE',
!             .              'SATURN', STATE, LT              )
!
!              WRITE(*,'(A)') 'Titan as seen from Saturn '
!             .            // '(J2000 frame):'
!              WRITE(*,'(A,3F13.3)') '   position   (km):',
!             .               ( STATE(I), I=1,3 )
!              WRITE(*,'(A,3F13.3)') '   velocity (km/s):',
!             .               ( STATE(I), I=4,6 )
!
!        C
!        C     Rotate the 6-vector STATE into the
!        C     Saturn body-fixed reference frame.
!        C
!              CALL MXVG ( TSIPM, STATE, 6, 6, SATVEC )
!
!              WRITE(*,'(A)') 'Titan as seen from Saturn '
!             .            // '(IAU_SATURN frame):'
!              WRITE(*,'(A,3F13.3)') '   position   (km):',
!             .               ( SATVEC(I), I=1,3 )
!              WRITE(*,'(A,3F13.3)') '   velocity (km/s):',
!             .               ( SATVEC(I), I=4,6 )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Titan as seen from Saturn (J2000 frame):
!           position   (km):  1071928.661  -505781.970   -60383.976
!           velocity (km/s):        2.404        5.176       -0.560
!        Titan as seen from Saturn (IAU_SATURN frame):
!           position   (km):   401063.338 -1116965.364    -5408.806
!           velocity (km/s):     -177.547      -63.745        0.028
!
!
!        Note that the complete example could be replaced by a single
!        SPKEZR call:
!
!           CALL SPKEZR ( 'TITAN',  ET,    'IAU_SATURN', 'NONE',
!          .              'SATURN', STATE, LT                   )
!
!
!     2) Use TISBOD is used to compute the angular velocity vector (with
!        respect to the J2000 inertial frame) of the specified body at
!        given time.
!
!        Use the meta-kernel from Example 1 above.
!
!
!        Example code begins here.
!
!
!              PROGRAM TISBOD_EX2
!              IMPLICIT NONE
!
!        C
!        C     Local variables
!        C
!              DOUBLE PRECISION      AV     ( 3    )
!              DOUBLE PRECISION      ET
!              DOUBLE PRECISION      DTIPM  ( 3, 3 )
!              DOUBLE PRECISION      OMEGA  ( 3, 3 )
!              DOUBLE PRECISION      ROT    ( 3, 3 )
!              DOUBLE PRECISION      TIPM   ( 3, 3 )
!              DOUBLE PRECISION      TSIPM  ( 6, 6 )
!              DOUBLE PRECISION      V      ( 3    )
!
!              INTEGER               I
!              INTEGER               J
!              INTEGER               SATID
!
!        C
!        C     Load the kernels.
!        C
!              CALL FURNSH ( 'tisbod_ex1.tm' )
!
!        C
!        C     The body ID for Saturn.
!        C
!              SATID = 699
!
!        C
!        C     First get the state transformation matrix.
!        C
!              CALL STR2ET ( 'Jan 1 2005',   ET        )
!              CALL TISBOD ( 'J2000', SATID, ET, TSIPM )
!
!        C
!        C     This matrix has the form:
!        C
!        C          .-            -.
!        C          |       :      |
!        C          | TIPM  :  0   |
!        C          | ......:......|
!        C          |       :      |
!        C          | DTIPM : TIPM |
!        C          |       :      |
!        C          `-            -'
!        C
!        C     We extract TIPM and DTIPM
!        C
!              DO  I = 1,3
!                 DO  J = 1,3
!
!                    TIPM  ( I, J ) = TSIPM ( I,   J )
!                    DTIPM ( I, J ) = TSIPM ( I+3, J )
!
!                 END DO
!              END DO
!
!        C
!        C     The transpose of TIPM and DTIPM, (TPMI and DTPMI), gives
!        C     the transformation from bodyfixed coordinates to inertial
!        C     coordinates.
!        C
!        C     Here is a fact about the relationship between angular
!        C     velocity associated with a time varying rotation matrix
!        C     that gives the orientation of a body with respect to
!        C     an inertial frame.
!        C
!        C        The angular velocity vector can be read from the off
!        C        diagonal components of the matrix product:
!        C
!        C                                t
!        C        OMEGA =     DTPMI * TPMI
!        C
!        C                         t
!        C              =     DTIPM * TIPM
!        C
!        C        the components of the angular velocity V will appear
!        C        in this matrix as:
!        C
!        C            .-                   -.
!        C            |                     |
!        C            |   0    -V(3)  V(2)  |
!        C            |                     |
!        C            |  V(3)    0   -V(1)  |
!        C            |                     |
!        C            | -V(2)   V(1)   0    |
!        C            |                     |
!        C            `-                   -'
!        C
!        C
!              CALL MTXM ( DTIPM, TIPM, OMEGA )
!
!              V(1) = OMEGA (3,2)
!              V(2) = OMEGA (1,3)
!              V(3) = OMEGA (2,1)
!
!        C
!        C     Display the results.
!        C
!              WRITE(*,'(A)') 'Angular velocity (km/s):'
!              WRITE(*,'(3F16.9)') V
!
!        C
!        C     It is possible to compute the angular velocity using
!        C     a single call to XF2RAV.
!        C
!              CALL XF2RAV ( TSIPM, ROT, AV )
!
!              WRITE(*,'(A)') 'Angular velocity using XF2RAV (km/s):'
!              WRITE(*,'(3F16.9)') AV
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Angular velocity (km/s):
!             0.000014001     0.000011995     0.000162744
!        Angular velocity using XF2RAV (km/s):
!             0.000014001     0.000011995     0.000162744
!
!
! $ Restrictions
!
!     1)  The kernel pool must be loaded with the appropriate
!         coefficients (from a text or binary PCK file) prior to calling
!         this routine.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     K.S. Zukor         (JPL)
!
! $ Version
!
!-    SPICELIB Version 4.5.1, 16-DEC-2021 (NJB) (JDR)
!
!        The routine was updated to support user-defined maximum phase
!        angle degrees. The additional text kernel kernel variable name
!        BODYnnn_MAX_PHASE_DEGREE must be used when the phase angle
!        polynomials have degree higher than 1. The maximum allowed
!        degree is 3.
!
!        The kernel variable names
!
!           BODY#_CONSTS_REF_FRAME
!           BODY#_CONSTS_JED_EPOCH
!
!        are now recognized.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Added note to $Particulars section.
!
!-    SPICELIB Version 4.5.0, 26-JUL-2016 (BVS)
!
!        The routine was updated to be more efficient by using a hash
!        and buffers so save text PCK data instead of doing kernel POOL
!        look-ups over an over again. The routine now checks the POOL
!        state counter and dumps all buffered data if it changes.
!
!        BUG FIX: changed available room in the BODVCD call
!        fetching 'NUT_PREC_ANGLES' from MAXANG to MAXANG*2.
!
!-    SPICELIB Version 4.4.0, 01-FEB-2008 (NJB)
!
!        The routine was updated to improve the error messages created
!        when required PCK data are not found. Now in most cases the
!        messages are created locally rather than by the kernel pool
!        access routines. In particular missing binary PCK data will
!        be indicated with a reasonable error message.
!
!-    SPICELIB Version 4.3.0, 13-DEC-2005 (NJB)
!
!        Bug fix: previous update introduced bug in state
!        transformation when REF was unequal to PCK native frame.
!
!-    SPICELIB Version 4.2.0, 23-OCT-2005 (NJB)
!
!        Re-wrote portions of algorithm to simplify source code.
!        Updated to remove non-standard use of duplicate arguments
!        in MXM and VADDG calls.
!
!        Replaced calls to ZZBODVCD with calls to BODVCD.
!
!-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
!
!        Tests of routine FAILED() were added.
!
!-    SPICELIB Version 4.0.0, 12-FEB-2004 (NJB)
!
!        Code has been updated to support satellite ID codes in the
!        range 10000 to 99999 and to allow nutation precession angles
!        to be associated with any object.
!
!        Implementation changes were made to improve robustness
!        of the code.
!
!-    SPICELIB Version 3.3.0, 29-MAR-1995 (WLT)
!
!        Properly initialized the variable NPAIRS.
!
!-    SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ)
!
!        Changed to call PCKMAT rather than PCKEUL.
!
!-    SPICELIB Version 3.1.0, 18-OCT-1994 (KSZ)
!
!        Fixed bug which incorrectly modded DW by two pi.
!
!-    SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
!
!        Changed to look for binary PCK file, and used this
!        to find Euler angles, if such data has been loaded.
!
!-    SPICELIB Version 2.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
!
!        Updated to handle P_constants referenced to different epochs
!        and inertial reference frames.
!
!        $Required_Reading and $Literature_References sections were
!        updated.
!
!-    SPICELIB Version 1.0.0, 05-NOV-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     transformation from inertial state to bodyfixed
!
!-&


! $ Revisions
!
!-    SPICELIB Version 4.5.0, 26-JUL-2016 (BVS)
!
!        The routine was updated to be more efficient by using a hash
!        and buffers so save text PCK data instead of doing kernel POOL
!        look-ups over an over again. The routine now checks the POOL
!        state counter and dumps all buffered data if it changes.
!
!-    SPICELIB Version 4.2.0, 23-OCT-2005 (NJB)
!
!        Re-wrote portions of algorithm to simplify source code.
!        The routine now takes advantage of EUL2XF, which wasn't
!        available when the first version of this routine was written.
!
!        Updated to remove non-standard use of duplicate arguments
!        in MXM and VADDG calls.
!
!        Replaced calls to ZZBODVCD with calls to BODVCD.
!
!-    SPICELIB Version 4.1.0, 05-JAN-2005 (NJB)
!
!        Tests of routine FAILED() were added. The new checks
!        are intended to prevent arithmetic operations from
!        being performed with uninitialized or invalid data.
!
!-     SPICELIB Version 4.0.0, 27-JAN-2004 (NJB)
!
!         Code has been updated to support satellite ID codes in the
!         range 10000 to 99999 and to allow nutation precession angles
!         to be associated with any object.
!
!         Calls to deprecated kernel pool access routine RTPOOL
!         were replaced by calls to GDPOOL.
!
!         Calls to BODVAR have been replaced with calls to
!         ZZBODVCD.
!
!-     SPICELIB Version 3.3.0, 29-MAR-1995 (WLT)
!
!        The variable NPAIRS is now initialized
!        at the same point as NA, NTHETA, ND, and NW to be
!        zero. This prevents the routine from performing
!        needless calculations for planets and avoids possible
!        floating point exceptions.
!
!-     SPICELIB Version 3.2.0, 22-MAR-1995 (KSZ)
!
!        TISBOD now gets the TSIPM matrix from PCKMAT.
!        Reference frame calculation moved to end.
!
!-     SPICELIB Version 3.0.1, 07-OCT-1994 (KSZ)
!
!        TISBOD bug which mistakenly moded DW by 2PI
!        was removed.
!
!-     SPICELIB Version 3.0.0, 10-MAR-1994 (KSZ)
!
!        TISBOD now uses new software to check for the
!        existence of binary PCK files, search the for
!        data corresponding to the requested body and time,
!        and return the appropriate Euler angles. Otherwise
!        the code calculates the Euler angles from the
!        P_constants kernel file.
!
!-     SPICELIB Version 2.0.0, 04-SEP-1991 (NJB)
!
!         Updated to handle P_constants referenced to different epochs
!         and inertial reference frames.
!
!         TISBOD now checks the kernel pool for presence of the
!         variables
!
!            BODY#_CONSTANTS_REF_FRAME
!
!         and
!
!            BODY#_CONSTANTS_JED_EPOCH
!
!         where # is the NAIF integer code of the barycenter of a
!         planetary system or of a body other than a planet or
!         satellite. If either or both of these variables are
!         present, the P_constants for BODY are presumed to be
!         referenced to the specified inertial frame or epoch.
!         If the epoch of the constants is not J2000, the input
!         time ET is converted to seconds past the reference epoch.
!         If the frame of the constants is not the frame specified
!         by REF, the rotation from the P_constants' frame to
!         body-fixed coordinates is transformed to the rotation from
!         the requested frame to body-fixed coordinates. The same
!         transformation is applied to the derivative of this
!         rotation.
!
!         Due to the prescience of the original author, the code
!         was already prepared to handle the possibility of
!         specification of a P_constants inertial reference frame via
!         kernel pool variables.
!
!
!         Also, the $Required_Reading and $Literature_References
!         sections were updated. The SPK required reading has been
!         deleted from the $Literature_References section, and the
!         NAIF_IDS, KERNEL, and TIME Required Reading files have
!         been added in the $Required_Reading section.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      TWOPI
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      J2000
      DOUBLE PRECISION      RPD
      DOUBLE PRECISION      SPD
      DOUBLE PRECISION      VDOTG

      INTEGER               ZZBODBRY

      LOGICAL               BODFND
      LOGICAL               FAILED
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               MAXANG
      PARAMETER           ( MAXANG = 200 )

!
!     Maximum number of coefficients per phase angle polynomial.
!
      INTEGER               MXPHAS
      PARAMETER           ( MXPHAS = 4 )

      INTEGER               MXPOLY
      PARAMETER           ( MXPOLY = 3 )

      INTEGER               NAMLEN
      PARAMETER           ( NAMLEN = 32 )

      INTEGER               FRNMLN
      PARAMETER           ( FRNMLN = 32 )

      INTEGER               TIMLEN
      PARAMETER           ( TIMLEN = 35 )

!
!     Local variables
!
      CHARACTER*(1)         DTYPE
      CHARACTER*(LMSGLN)    ERRMSG
      CHARACTER*(FRNMLN)    FIXFRM
      CHARACTER*(NAMLEN)    ITEM
      CHARACTER*(NAMLEN)    ITEM2
      CHARACTER*(TIMLEN)    TIMSTR

      DOUBLE PRECISION      AC     (    MAXANG )
      DOUBLE PRECISION      COSTH  (    MAXANG )
      DOUBLE PRECISION      D
      DOUBLE PRECISION      DC     (    MAXANG )
      DOUBLE PRECISION      DCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      DCOSTH (    MAXANG )
      DOUBLE PRECISION      DDEC
      DOUBLE PRECISION      DDELTA
      DOUBLE PRECISION      DEC
      DOUBLE PRECISION      DELTA
      DOUBLE PRECISION      DPHI
      DOUBLE PRECISION      DPVAL
      DOUBLE PRECISION      DRA
      DOUBLE PRECISION      DSINTH (    MAXANG )
      DOUBLE PRECISION      DTIPM  (     3,  3 )
      DOUBLE PRECISION      DTHETA
      DOUBLE PRECISION      DW
      DOUBLE PRECISION      EULSTA (     6     )
      DOUBLE PRECISION      EPOCH
      DOUBLE PRECISION      PCKEPC
      DOUBLE PRECISION      PHI
      DOUBLE PRECISION      RA
      DOUBLE PRECISION      RCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      REQ2PC (     3, 3  )
      DOUBLE PRECISION      SINTH  (    MAXANG )
      DOUBLE PRECISION      T
      DOUBLE PRECISION      TC
      DOUBLE PRECISION      TD
      DOUBLE PRECISION      TCOEF  ( MXPHAS * MAXANG )
      DOUBLE PRECISION      TIPM   (     3, 3  )
      DOUBLE PRECISION      THETA
      DOUBLE PRECISION      TMPEPC
      DOUBLE PRECISION      W
      DOUBLE PRECISION      WC     (    MAXANG )
      DOUBLE PRECISION      WCOEF  ( 0:MXPOLY-1 )
      DOUBLE PRECISION      XDTIPM (     3, 3  )
      DOUBLE PRECISION      XTIPM  (     3, 3  )

      INTEGER               CENT
      INTEGER               DEG
      INTEGER               DIM
      INTEGER               FRCODE
      INTEGER               I
      INTEGER               J
      INTEGER               J2CODE
      INTEGER               K
      INTEGER               L
      INTEGER               M
      INTEGER               NA
      INTEGER               ND
      INTEGER               NPHASE
      INTEGER               NPHSCO
      INTEGER               NTHETA
      INTEGER               NW
      INTEGER               PCREF
      INTEGER               REFID
      INTEGER               REQREF

      LOGICAL               FIRST
      LOGICAL               FOUND
      LOGICAL               FOUND2

!
!     POOL state counter.
!
      INTEGER               PULCTR ( CTRSIZ )

      LOGICAL               UPDATE

!
!     ID-based hash for text PCK data. KIDLST, KIDPOL, and KIDIDS
!     provide the index in the body PCK data arrays at which the data
!     of the body with a given ID are stored.
!
      INTEGER               LBPOOL
      PARAMETER           ( LBPOOL = -5 )

      INTEGER               MAXBOD
      PARAMETER           ( MAXBOD = 157 )

      INTEGER               BIDLST (             MAXBOD )
      INTEGER               BIDPOL ( LBPOOL :    MAXBOD )
      INTEGER               BIDIDS (             MAXBOD )

      DOUBLE PRECISION      BAC    (     MAXANG, MAXBOD )
      DOUBLE PRECISION      BDC    (     MAXANG, MAXBOD )
      DOUBLE PRECISION      BDCOEF ( 0:MXPOLY-1, MAXBOD )
      DOUBLE PRECISION      BPCKEP (             MAXBOD )
      DOUBLE PRECISION      BRCOEF ( 0:MXPOLY-1, MAXBOD )
      DOUBLE PRECISION      BTCOEF ( MXPHAS * MAXANG, MAXBOD )
      DOUBLE PRECISION      BWC    (     MAXANG, MAXBOD )
      DOUBLE PRECISION      BWCOEF ( 0:MXPOLY-1, MAXBOD )

      INTEGER               BNA    (             MAXBOD )
      INTEGER               BND    (             MAXBOD )
      INTEGER               BNPHAS (             MAXBOD )
      INTEGER               BNPHCO (             MAXBOD )
      INTEGER               BNW    (             MAXBOD )
      INTEGER               BPCREF (             MAXBOD )

      DOUBLE PRECISION      SINTMP
      DOUBLE PRECISION      COSTMP

      INTEGER               AT
      INTEGER               AVAIL

      LOGICAL               NEW

!
!     Saved variables
!
!     Because we need to save almost everything, we save everything
!     rather than taking a chance and accidentally leaving something
!     off the list.
!
      SAVE

!
!     Initial values
!
      DATA  BPCREF / MAXBOD * 0 /
      DATA  FIRST  / .TRUE.  /
      DATA  FOUND  / .FALSE. /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'TISBOD' )

!
!     Perform any needed first pass initializations.
!
      IF ( FIRST ) THEN

!
!        Initialize POOL state counter to the user value.
!
         CALL ZZCTRUIN ( PULCTR )

!
!        Initialize kernel POOL frame hashes.
!
         CALL ZZHSIINI ( MAXBOD, BIDLST, BIDPOL )

!
!        Get the code for the J2000 frame.
!
         CALL IRFNUM ( 'J2000', J2CODE )

!
!        Set seconds per day and per century.
!
         D = SPD()
         T = D  *  36525.D0

         FIRST = .FALSE.

      END IF

      CALL IRFNUM  ( REF, REQREF )

!
!     Get state transformation matrix from high precision PCK file, if
!     available.
!
      CALL PCKMAT ( BODY, ET, PCREF, TSIPM, FOUND )

      IF ( .NOT. FOUND ) THEN
!
!        The data for the frame of interest are not available in a
!        loaded binary PCK file. This is not an error: the data may be
!        present in the kernel pool.
!
!        Check the POOL counter. If it changed, dump all buffered
!        constants data.
!
         CALL ZZPCTRCK ( PULCTR, UPDATE )

         IF ( UPDATE ) THEN
            CALL ZZHSIINI ( MAXBOD, BIDLST, BIDPOL )
         END IF

!
!        Check if we have data for this body in our buffers.
!
         CALL ZZHSICHK ( BIDLST, BIDPOL, BIDIDS, BODY, AT )

         IF ( AT .NE. 0 ) THEN
!
!           Set PCREF as it is used after the text PCK "IF".
!
            PCREF  = BPCREF ( AT )

         ELSE
!
!           Conduct a non-error-signaling check for the presence of a
!           kernel variable that is required to implement an IAU style
!           body-fixed reference frame. If the data aren't available,
!           we don't want BODVCD to signal a SPICE(KERNELVARNOTFOUND)
!           error; we want to issue the error signal locally, with a
!           better error message.
!
            ITEM = 'BODY#_PM'
            CALL REPMI  ( ITEM, '#',   BODY, ITEM  )
            CALL DTPOOL ( ITEM, FOUND, NW,   DTYPE )

            IF ( .NOT. FOUND ) THEN
!
!              Now we do have an error.
!
!              We don't have the data we'll need to produced the
!              requested state transformation matrix. In order to
!              create an error message understandable to the user,
!              find, if possible, the name of the reference frame
!              associated with the input body. Note that the body is
!              really identified by a PCK frame class ID code, though
!              most of the documentation just calls it a body ID code.
!
               CALL CCIFRM ( PCK, BODY,  FRCODE, FIXFRM, CENT, FOUND )
               CALL ETCAL  ( ET,  TIMSTR )

               ERRMSG = 'PCK data required to compute the orientation ' &
            //       'of the # # for epoch # TDB were not found. ' &
            //       'If these data were to be provided by a binary ' &
            //       'PCK file, then it is possible that the PCK ' &
            //       'file does not have coverage for the specified ' &
            //       'body-fixed frame at the time of interest. If the ' &
            //       'data were to be provided by a text PCK file, ' &
            //       'then possibly the file does not contain data ' &
            //       'for the specified body-fixed frame. In ' &
            //       'either case it is possible that a required ' &
            //       'PCK file was not loaded at all.'

!
!              Fill in the variable data in the error message.
!
               IF ( FOUND ) THEN
!
!                 The frame system knows the name of the body-fixed
!                 frame.
!
                  CALL SETMSG ( ERRMSG                  )
                  CALL ERRCH  ( '#', 'body-fixed frame' )
                  CALL ERRCH  ( '#', FIXFRM             )
                  CALL ERRCH  ( '#', TIMSTR             )

               ELSE
!
!                 The frame system doesn't know the name of the
!                 body-fixed frame, most likely due to a missing
!                 frame kernel.
!
                  CALL SUFFIX ( '#', 1, ERRMSG                      )
                  CALL SETMSG ( ERRMSG                              )
                  CALL ERRCH  ( '#', &
                             'body-fixed frame associated with ' &
               //            'the ID code'                       )
                  CALL ERRINT ( '#', BODY                           )
                  CALL ERRCH  ( '#', TIMSTR                         )
                  CALL ERRCH  ( '#', &
                             'Also, a frame kernel defining the ' &
               //            'body-fixed frame associated with ' &
               //            'body # may need to be loaded.'     )
                  CALL ERRINT ( '#', BODY                           )

               END IF

               CALL SIGERR ( 'SPICE(FRAMEDATANOTFOUND)' )
               CALL CHKOUT ( 'TISBOD'                   )
               RETURN

            END IF

!
!           Find the body code used to label the reference frame and
!           epoch specifiers for the orientation constants for BODY.
!
!           For planetary systems, the reference frame and epoch for
!           the orientation constants is associated with the system
!           barycenter, not with individual bodies in the system. For
!           any other bodies, (the Sun or asteroids, for example) the
!           body's own code is used as the label.
!
            REFID = ZZBODBRY ( BODY )

!
!           Look up the epoch of the constants. The epoch is specified
!           as a Julian ephemeris date. The epoch defaults to J2000.
!
!           Look for both forms of the JED epoch kernel variable. At
!           most one is allowed to be present.
!
            ITEM   =  'BODY#_CONSTANTS_JED_EPOCH'
            CALL REPMI ( ITEM, '#', REFID, ITEM )

            ITEM2  =  'BODY#_CONSTS_JED_EPOCH'
            CALL REPMI ( ITEM2, '#', REFID, ITEM2 )

            CALL GDPOOL ( ITEM, 1, 1, DIM, PCKEPC, FOUND )

            IF ( .NOT. FOUND ) THEN

               CALL GDPOOL ( ITEM2, 1, 1, DIM, PCKEPC, FOUND2 )

               IF ( .NOT. FOUND2 ) THEN
                  PCKEPC = J2000()
               END IF

            ELSE
!
!              Check for presence of both forms of the variable names.
!
               CALL DTPOOL ( ITEM2, FOUND2, DIM, DTYPE )

               IF ( FOUND2 ) THEN

                  CALL SETMSG ( 'Both kernel variables # and # are ' &
                  //            'present in the kernel pool. At most ' &
                  //            'one form of the kernel variable name ' &
                  //            'may be present.'           )
                  CALL ERRCH  ( '#', ITEM                   )
                  CALL ERRCH  ( '#', ITEM2                  )
                  CALL SIGERR ( 'SPICE(COMPETINGEPOCHSPEC)' )
                  CALL CHKOUT ( 'TISBOD'                    )
                  RETURN

               END IF

            END IF

!
!           Look up the reference frame of the constants. The reference
!           frame is specified by a code recognized by CHGIRF. The
!           default frame is J2000, symbolized by the code J2CODE.
!
            ITEM   =  'BODY#_CONSTANTS_REF_FRAME'
            CALL REPMI ( ITEM, '#', REFID, ITEM )

            ITEM2  =  'BODY#_CONSTS_REF_FRAME'
            CALL REPMI ( ITEM2, '#', REFID, ITEM2 )

            CALL REPMI  ( ITEM, '#', REFID, ITEM              )
            CALL GIPOOL ( ITEM,  1,  1,     DIM, PCREF, FOUND )

            IF ( .NOT. FOUND ) THEN

               CALL GIPOOL ( ITEM2, 1, 1, DIM, PCREF, FOUND )

               IF ( .NOT. FOUND ) THEN
                  PCREF  =  J2CODE
               END IF

            ELSE
!
!              Check for presence of both forms of the variable names.
!
               CALL DTPOOL ( ITEM2, FOUND, DIM, DTYPE )

               IF ( FOUND ) THEN

                  CALL SETMSG ( 'Both kernel variables # and # are ' &
                  //            'present in the kernel pool. At most ' &
                  //            'one form of the kernel variable name ' &
                  //            'may be present.'           )
                  CALL ERRCH  ( '#', ITEM                   )
                  CALL ERRCH  ( '#', ITEM2                  )
                  CALL SIGERR ( 'SPICE(COMPETINGFRAMESPEC)' )
                  CALL CHKOUT ( 'TISBOD'                    )
                  RETURN

               END IF

            END IF

!
!           Whatever the body, it has quadratic time polynomials for
!           the RA and Dec of the pole, and for the rotation of the
!           Prime Meridian.
!
            ITEM = 'POLE_RA'
            CALL CLEARD (             MXPOLY,     RCOEF )
            CALL BODVCD ( BODY, ITEM, MXPOLY, NA, RCOEF )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TISBOD' )
               RETURN
            END IF

            ITEM = 'POLE_DEC'
            CALL CLEARD (             MXPOLY,     DCOEF )
            CALL BODVCD ( BODY, ITEM, MXPOLY, ND, DCOEF )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TISBOD' )
               RETURN
            END IF

            ITEM = 'PM'
            CALL CLEARD (             MXPOLY,     WCOEF )
            CALL BODVCD ( BODY, ITEM, MXPOLY, NW, WCOEF )

            IF ( FAILED() ) THEN
               CALL CHKOUT ( 'TISBOD' )
               RETURN
            END IF

!
!           Whether or not the body is a satellite, there may be
!           additional nutation and libration (THETA) terms.
!
            NTHETA = 0
            NPHASE = 0
            NPHSCO = 0
            NA     = 0
            ND     = 0
            NW     = 0

            ITEM = 'NUT_PREC_ANGLES'

            IF ( BODFND ( REFID, ITEM ) ) THEN

!              Find out whether the maximum phase angle degree
!              has been explicitly set.
!
               ITEM2 = 'MAX_PHASE_DEGREE'

               IF ( BODFND(REFID, ITEM2) ) THEN

                  CALL BODVCD ( REFID, ITEM2, 1, DIM, DPVAL )

                  DEG = NINT( DPVAL )

                  IF ( ( DEG .LT. 1 ) .OR. ( DEG .GT. MXPHAS-1 ) ) THEN

                     CALL SETMSG ( 'Maximum phase angle degree for ' &
                     //            'body # must be in the range 1:# ' &
                     //            'but was #.'                      )
                     CALL ERRINT ( '#', REFID                        )
                     CALL ERRINT ( '#', MXPHAS-1                     )
                     CALL ERRINT ( '#', DEG                          )
                     CALL SIGERR ( 'SPICE(DEGREEOUTOFRANGE)'         )
                     CALL CHKOUT ( 'TISBOD'                          )
                     RETURN

                  END IF

                  NPHSCO = DEG + 1

               ELSE
!
!                 The default degree is 1, yielding two coefficients.
!
                  NPHSCO = 2

               END IF
!
!              There is something a bit obscure going on below. BODVCD
!              loads the array TCOEF in the following order
!
!                 TCOEF(1,1), TCOEF(2,1), ... TCOEF(NPHSCO),
!                 TCOEF(1,2), TCOEF(2,2), ...
!
!              The NTHETA that comes back is the total number of items
!              loaded, but we will need the actual limit on the second
!              dimension. That is --- NTHETA / NPHSCO.
!
               CALL BODVCD ( REFID, ITEM, MAXANG*MXPHAS, NTHETA, TCOEF )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'TISBOD' )
                  RETURN
               END IF

!
!              NPHSCO is at least 1; this division is safe.
!
               NPHASE = NTHETA / NPHSCO

            END IF

!
!           Look up the right ascension nutations in the precession of
!           the pole. NA is the number of Ascension coefficients. AC
!           are the Ascension coefficients.
!
            ITEM = 'NUT_PREC_RA'

            IF ( BODFND ( BODY, ITEM ) ) THEN

               CALL BODVCD ( BODY, ITEM, MAXANG, NA, AC )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'TISBOD' )
                  RETURN
               END IF

            END IF

!
!           Look up the declination nutations in the precession of the
!           pole. ND is the number of Declination coefficients. DC are
!           the Declination coefficients.
!
            ITEM = 'NUT_PREC_DEC'

            IF ( BODFND ( BODY, ITEM ) ) THEN

               CALL BODVCD ( BODY, ITEM, MAXANG, ND, DC )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'TISBOD' )
                  RETURN
               END IF

            END IF

!
!           Finally look up the prime meridian nutations. NW is the
!           number of coefficients. WC is the array of coefficients.
!
            ITEM = 'NUT_PREC_PM'

            IF ( BODFND ( BODY, ITEM ) ) THEN

               CALL BODVCD ( BODY, ITEM, MAXANG, NW, WC )

               IF ( FAILED() ) THEN
                  CALL CHKOUT ( 'TISBOD' )
                  RETURN
               END IF

            END IF

!
!           The number of coefficients returned had better not be
!           bigger than the number of angles we are going to compute.
!           If it is we simply signal an error and bag it, for sure.
!
            IF ( MAX( NA, ND, NW ) .GT. NPHASE ) THEN
               CALL SETMSG ( 'Insufficient number of nutation/' &
               //            'precession angles for body * at time #. ' &
               //            'Number of angles is #; number required ' &
               //            'is #.'                )
               CALL ERRINT ( '*', BODY              )
               CALL ERRDP  ( '#', ET                )
               CALL ERRINT ( '#', NPHASE            )
               CALL ERRINT ( '#', MAX( NA, ND, NW ) )
               CALL SIGERR ( 'SPICE(INSUFFICIENTANGLES)' )
               CALL CHKOUT ( 'TISBOD' )
               RETURN
            END IF

!
!           We succeeded to fetch all data for this body. Save it in
!           the buffers. First check if there is room. If not, dump the
!           buffers to make room.
!
            CALL ZZHSIAVL( BIDPOL, AVAIL )
            IF ( AVAIL .LE. 0 ) THEN
               CALL ZZHSIINI ( MAXBOD, BIDLST, BIDPOL )
            END IF

!
!           Add this body to the hash and save its data in the buffers.
!
            CALL ZZHSIADD ( BIDLST, BIDPOL, BIDIDS, BODY, AT, NEW )

            BPCKEP ( AT ) = PCKEPC
            BPCREF ( AT ) = PCREF
            BNPHAS ( AT ) = NPHASE
            BNPHCO ( AT ) = NPHSCO
            BNA    ( AT ) = NA
            BND    ( AT ) = ND
            BNW    ( AT ) = NW

            CALL MOVED( RCOEF( 0 ), MXPOLY,        BRCOEF( 0, AT ) )
            CALL MOVED( DCOEF( 0 ), MXPOLY,        BDCOEF( 0, AT ) )
            CALL MOVED( WCOEF( 0 ), MXPOLY,        BWCOEF( 0, AT ) )
            CALL MOVED( TCOEF( 1 ), MAXANG*NPHSCO, BTCOEF( 1, AT ) )
            CALL MOVED( AC   ( 1 ), MAXANG,        BAC   ( 1, AT ) )
            CALL MOVED( DC   ( 1 ), MAXANG,        BDC   ( 1, AT ) )
            CALL MOVED( WC   ( 1 ), MAXANG,        BWC   ( 1, AT ) )

         END IF

!
!        The reference epoch in the PCK is given as JED. Convert to
!        ephemeris seconds past J2000. Then convert the input ET to
!        seconds past the reference epoch.
!
         TMPEPC  =  SPD() * ( BPCKEP(AT) - J2000() )
         EPOCH   =  ET    -   TMPEPC
         TD      =  EPOCH / D
         TC      =  EPOCH / T

!
!        Evaluate the time polynomials and their derivatives w.r.t.
!        EPOCH at EPOCH.
!
!        Evaluate the time polynomials at EPOCH.
!
         RA  = BRCOEF(0,AT) + TC * ( BRCOEF(1,AT) + TC * BRCOEF(2,AT) )
         DEC = BDCOEF(0,AT) + TC * ( BDCOEF(1,AT) + TC * BDCOEF(2,AT) )
         W   = BWCOEF(0,AT) + TD * ( BWCOEF(1,AT) + TD * BWCOEF(2,AT) )

         DRA  = ( BRCOEF(1,AT)  + 2.0D0*TC*BRCOEF(2,AT) ) / T
         DDEC = ( BDCOEF(1,AT)  + 2.0D0*TC*BDCOEF(2,AT) ) / T
         DW   = ( BWCOEF(1,AT)  + 2.0D0*TD*BWCOEF(2,AT) ) / D

!
!        Compute the nutations and librations (and their derivatives)
!        as appropriate.
!
         DO I = 1, BNPHAS(AT)

            IF ( BNPHCO(AT) .EQ. 2 ) THEN
!
!              This case is so common that we'll deal with it
!              separately. We'll avoid unnecessary arithmetic
!              operations.
!
               K = 1 + 2*(I-1)
               M = 1 + K

               THETA  = ( BTCOEF(K,AT) + TC * BTCOEF(M,AT) ) * RPD()
               DTHETA = ( BTCOEF(M,AT) / T ) * RPD()

            ELSE
!
!              THETA and DTHETA have higher-order terms; there are
!              BNPHCO(AT) coefficients for each angle.
!
               THETA = 0.D0

               DO J = 1, BNPHCO(AT)
!
!                 K is the start index for the coefficients of the
!                 Ith angle.
!
                  K     = J + BNPHCO(AT)*(I-1)

                  THETA = THETA  + ( TC**(J-1) * BTCOEF(K,AT) )

               END DO

               THETA  = THETA * RPD()

               K = 1 + BNPHCO(AT)*(I-1)
               M = 1 + K

               DTHETA = BTCOEF(M,AT) / T

               DO J = 3, BNPHCO(AT)

                  K      = J + BNPHCO(AT)*(I-1)
                  L      = J - 1
!
!                 Differentiate with respect to EPOCH the Jth
!                 term of the expression for the Ith angle; add
!                 the result to the current sum.
!
                  DTHETA = DTHETA + &
                           ( L * TC**(L-1) * BTCOEF(K,AT) ) / T**(L-1)
               END DO

               DTHETA = DTHETA * RPD()

            END IF

            SINTMP    =  SIN ( THETA )
            COSTMP    =  COS ( THETA )

            SINTH(I)  =  SINTMP
            COSTH(I)  =  COSTMP
            DSINTH(I) =  COSTMP * DTHETA
            DCOSTH(I) = -SINTMP * DTHETA

         END DO

!
!        Adjust RA, DEC, W and their derivatives by the librations
!        and nutations.
!
         RA  = RA    + VDOTG ( BAC(1,AT), SINTH,  BNA(AT) )
         DEC = DEC   + VDOTG ( BDC(1,AT), COSTH,  BND(AT) )
         W   = W     + VDOTG ( BWC(1,AT), SINTH,  BNW(AT) )

         DRA  = DRA  + VDOTG ( BAC(1,AT), DSINTH, BNA(AT) )
         DDEC = DDEC + VDOTG ( BDC(1,AT), DCOSTH, BND(AT) )
         DW   = DW   + VDOTG ( BWC(1,AT), DSINTH, BNW(AT) )

!
!        Convert from degrees to radians
!
         RA      = RA   * RPD()
         DEC     = DEC  * RPD()
         W       = W    * RPD()

         DRA     = DRA  * RPD()
         DDEC    = DDEC * RPD()
         DW      = DW   * RPD()
!
!        Convert to Euler angles.
!
         W       = MOD ( W,  TWOPI() )
         PHI     = RA + HALFPI()
         DELTA   = HALFPI() - DEC

         DPHI    =   DRA
         DDELTA  = - DDEC

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TISBOD' )
            RETURN
         END IF

!
!        Pack the Euler angles and their derivatives into
!        a state vector.
!
         CALL VPACK ( W,  DELTA,  PHI,  EULSTA    )
         CALL VPACK ( DW, DDELTA, DPHI, EULSTA(4) )

!
!        Find the state transformation defined by the Euler angle
!        state vector. The transformation matrix TSIPM has the
!        following structure:
!
!            -            -
!           |       :      |
!           | TIPM  :  0   |
!           | ......:......|
!           |       :      |
!           | DTIPM : TIPM |
!           |       :      |
!            -            -
!
         CALL EUL2XF ( EULSTA, 3, 1, 3, TSIPM )

      END IF

!
!     At this point the base frame PCREF has been determined.
!
!     If the requested base frame is not base frame associated with the
!     PCK data, adjust the transformation matrix TSIPM to map from the
!     requested frame to the body-fixed frame.
!
      IF ( REQREF .NE. PCREF ) THEN
!
!        Next get the position transformation from the user specified
!        inertial frame to the native PCK inertial frame.
!
         CALL IRFROT ( REQREF, PCREF, REQ2PC )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'TISBOD' )
            RETURN
         END IF

!
!        Since we're applying an inertial transformation to TSIPM,
!        we can rotate the non-zero blocks of TSIPM. This saves
!        a bunch of double precision multiplications.
!
!        Extract the upper and lower left blocks of TSIPM.
!
         DO I = 1, 3

            DO J = 1, 3

               TIPM (I,J) =  TSIPM( I,   J )
               DTIPM(I,J) =  TSIPM( I+3, J )

            END DO

         END DO

!
!        Rotate the blocks. Note this is a right multiplication.
!
         CALL MXM ( TIPM,  REQ2PC, XTIPM  )
         CALL MXM ( DTIPM, REQ2PC, XDTIPM )

!
!        Replace the non-zero blocks of TSIPM. This gives us the
!        transformation from the requested frame to the
!        bodyfixed frame.
!
         DO I = 1, 3

            DO J = 1, 3

               TSIPM( I,   J   ) = XTIPM (I,J)
               TSIPM( I+3, J+3 ) = XTIPM (I,J)
               TSIPM( I+3, J   ) = XDTIPM(I,J)

            END DO

         END DO

      END IF
!
!     That's all folks. Check out and get out.
!
      CALL CHKOUT ( 'TISBOD' )
      RETURN
      END
