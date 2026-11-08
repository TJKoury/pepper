!$Procedure TISBOD ( Transformation, inertial state to bodyfixed )

      subroutine tisbod ( ref, body, et, tsipm )
      use pepper_kinds, only: ip, wp

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

      implicit none

      include               'errhnd.inc'
      include               'frmtyp.inc'
      include               'zzctr.inc'

      character(*)          ref
      integer(ip)           body
      real(wp)              et
      real(wp)              tsipm   ( 6,6 )

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
      real(wp)              twopi
      real(wp)              halfpi
      real(wp)              j2000
      real(wp)              rpd
      real(wp)              spd
      real(wp)              vdotg

      integer(ip)           zzbodbry

      logical               bodfnd
      logical               failed
      logical               return

!
!     Local parameters
!
      integer(ip)           maxang
      parameter           ( maxang = 200 )

!
!     Maximum number of coefficients per phase angle polynomial.
!
      integer(ip)           mxphas
      parameter           ( mxphas = 4 )

      integer(ip)           mxpoly
      parameter           ( mxpoly = 3 )

      integer(ip)           namlen
      parameter           ( namlen = 32 )

      integer(ip)           frnmln
      parameter           ( frnmln = 32 )

      integer(ip)           timlen
      parameter           ( timlen = 35 )

!
!     Local variables
!
      character(1)          dtype
      character(lmsgln)     errmsg
      character(frnmln)     fixfrm
      character(namlen)     item
      character(namlen)     item2
      character(timlen)     timstr

      real(wp)              ac     (    maxang )
      real(wp)              costh  (    maxang )
      real(wp)              d
      real(wp)              dc     (    maxang )
      real(wp)              dcoef  ( 0:mxpoly-1 )
      real(wp)              dcosth (    maxang )
      real(wp)              ddec
      real(wp)              ddelta
      real(wp)              dec
      real(wp)              delta
      real(wp)              dphi
      real(wp)              dpval
      real(wp)              dra
      real(wp)              dsinth (    maxang )
      real(wp)              dtipm  (     3,  3 )
      real(wp)              dtheta
      real(wp)              dw
      real(wp)              eulsta (     6     )
      real(wp)              epoch
      real(wp)              pckepc
      real(wp)              phi
      real(wp)              ra
      real(wp)              rcoef  ( 0:mxpoly-1 )
      real(wp)              req2pc (     3, 3  )
      real(wp)              sinth  (    maxang )
      real(wp)              t
      real(wp)              tc
      real(wp)              td
      real(wp)              tcoef  ( mxphas * maxang )
      real(wp)              tipm   (     3, 3  )
      real(wp)              theta
      real(wp)              tmpepc
      real(wp)              w
      real(wp)              wc     (    maxang )
      real(wp)              wcoef  ( 0:mxpoly-1 )
      real(wp)              xdtipm (     3, 3  )
      real(wp)              xtipm  (     3, 3  )

      integer(ip)           cent
      integer(ip)           deg
      integer(ip)           dim
      integer(ip)           frcode
      integer(ip)           i
      integer(ip)           j
      integer(ip)           j2code
      integer(ip)           k
      integer(ip)           l
      integer(ip)           m
      integer(ip)           na
      integer(ip)           nd
      integer(ip)           nphase
      integer(ip)           nphsco
      integer(ip)           ntheta
      integer(ip)           nw
      integer(ip)           pcref
      integer(ip)           refid
      integer(ip)           reqref

      logical               first
      logical               found
      logical               found2

!
!     POOL state counter.
!
      integer(ip)           pulctr ( ctrsiz )

      logical               update

!
!     ID-based hash for text PCK data. KIDLST, KIDPOL, and KIDIDS
!     provide the index in the body PCK data arrays at which the data
!     of the body with a given ID are stored.
!
      integer(ip)           lbpool
      parameter           ( lbpool = -5 )

      integer(ip)           maxbod
      parameter           ( maxbod = 157 )

      integer(ip)           bidlst (             maxbod )
      integer(ip)           bidpol ( lbpool :    maxbod )
      integer(ip)           bidids (             maxbod )

      real(wp)              bac    (     maxang, maxbod )
      real(wp)              bdc    (     maxang, maxbod )
      real(wp)              bdcoef ( 0:mxpoly-1, maxbod )
      real(wp)              bpckep (             maxbod )
      real(wp)              brcoef ( 0:mxpoly-1, maxbod )
      real(wp)              btcoef ( mxphas * maxang, maxbod )
      real(wp)              bwc    (     maxang, maxbod )
      real(wp)              bwcoef ( 0:mxpoly-1, maxbod )

      integer(ip)           bna    (             maxbod )
      integer(ip)           bnd    (             maxbod )
      integer(ip)           bnphas (             maxbod )
      integer(ip)           bnphco (             maxbod )
      integer(ip)           bnw    (             maxbod )
      integer(ip)           bpcref (             maxbod )

      real(wp)              sintmp
      real(wp)              costmp

      integer(ip)           at
      integer(ip)           avail

      logical               new

!
!     Saved variables
!
!     Because we need to save almost everything, we save everything
!     rather than taking a chance and accidentally leaving something
!     off the list.
!
      save

!
!     Initial values
!
      data  bpcref / maxbod * 0 /
      data  first  / .true.  /
      data  found  / .false. /

!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Perform any needed first pass initializations.
!
      if ( first ) then

!
!        Initialize POOL state counter to the user value.
!
         call zzctruin ( pulctr )

!
!        Initialize kernel POOL frame hashes.
!
         call zzhsiini ( maxbod, bidlst, bidpol )

!
!        Get the code for the J2000 frame.
!
         call irfnum ( 'J2000', j2code )

!
!        Set seconds per day and per century.
!
         d = spd()
         t = d  *  36525.0_wp

         first = .false.

      end if

      call irfnum  ( ref, reqref )

!
!     Get state transformation matrix from high precision PCK file, if
!     available.
!
      call pckmat ( body, et, pcref, tsipm, found )

      if ( .not. found ) then
!
!        The data for the frame of interest are not available in a
!        loaded binary PCK file. This is not an error: the data may be
!        present in the kernel pool.
!
!        Check the POOL counter. If it changed, dump all buffered
!        constants data.
!
         call zzpctrck ( pulctr, update )

         if ( update ) then
            call zzhsiini ( maxbod, bidlst, bidpol )
         end if

!
!        Check if we have data for this body in our buffers.
!
         call zzhsichk ( bidlst, bidpol, bidids, body, at )

         if ( at /= 0 ) then
!
!           Set PCREF as it is used after the text PCK "IF".
!
            pcref  = bpcref ( at )

         else
!
!           Conduct a non-error-signaling check for the presence of a
!           kernel variable that is required to implement an IAU style
!           body-fixed reference frame. If the data aren't available,
!           we don't want BODVCD to signal a SPICE(KERNELVARNOTFOUND)
!           error; we want to issue the error signal locally, with a
!           better error message.
!
            item = 'BODY#_PM'
            call repmi  ( item, '#',   body, item  )
            call dtpool ( item, found, nw,   dtype )

            if ( .not. found ) then
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
               call ccifrm ( pck, body,  frcode, fixfrm, cent, found )
               call etcal  ( et,  timstr )

               errmsg = 'PCK data required to compute the orientation ' &
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
               if ( found ) then
!
!                 The frame system knows the name of the body-fixed
!                 frame.
!
                  call setmsg ( errmsg                  )
                  call errch  ( '#', 'body-fixed frame' )
                  call errch  ( '#', fixfrm             )
                  call errch  ( '#', timstr             )

               else
!
!                 The frame system doesn't know the name of the
!                 body-fixed frame, most likely due to a missing
!                 frame kernel.
!
                  call suffix ( '#', 1, errmsg                      )
                  call setmsg ( errmsg                              )
                  call errch  ( '#', &
                             'body-fixed frame associated with ' &
               //            'the ID code'                       )
                  call errint ( '#', body                           )
                  call errch  ( '#', timstr                         )
                  call errch  ( '#', &
                             'Also, a frame kernel defining the ' &
               //            'body-fixed frame associated with ' &
               //            'body # may need to be loaded.'     )
                  call errint ( '#', body                           )

               end if

               call sigerr ( 'SPICE(FRAMEDATANOTFOUND)' )
               return

            end if

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
            refid = zzbodbry ( body )

!
!           Look up the epoch of the constants. The epoch is specified
!           as a Julian ephemeris date. The epoch defaults to J2000.
!
!           Look for both forms of the JED epoch kernel variable. At
!           most one is allowed to be present.
!
            item   =  'BODY#_CONSTANTS_JED_EPOCH'
            call repmi ( item, '#', refid, item )

            item2  =  'BODY#_CONSTS_JED_EPOCH'
            call repmi ( item2, '#', refid, item2 )

            call gdpool ( item, 1, 1, dim, pckepc, found )

            if ( .not. found ) then

               call gdpool ( item2, 1, 1, dim, pckepc, found2 )

               if ( .not. found2 ) then
                  pckepc = j2000()
               end if

            else
!
!              Check for presence of both forms of the variable names.
!
               call dtpool ( item2, found2, dim, dtype )

               if ( found2 ) then

                  call setmsg ( 'Both kernel variables # and # are ' &
                  //            'present in the kernel pool. At most ' &
                  //            'one form of the kernel variable name ' &
                  //            'may be present.'           )
                  call errch  ( '#', item                   )
                  call errch  ( '#', item2                  )
                  call sigerr ( 'SPICE(COMPETINGEPOCHSPEC)' )
                  return

               end if

            end if

!
!           Look up the reference frame of the constants. The reference
!           frame is specified by a code recognized by CHGIRF. The
!           default frame is J2000, symbolized by the code J2CODE.
!
            item   =  'BODY#_CONSTANTS_REF_FRAME'
            call repmi ( item, '#', refid, item )

            item2  =  'BODY#_CONSTS_REF_FRAME'
            call repmi ( item2, '#', refid, item2 )

            call repmi  ( item, '#', refid, item              )
            call gipool ( item,  1,  1,     dim, pcref, found )

            if ( .not. found ) then

               call gipool ( item2, 1, 1, dim, pcref, found )

               if ( .not. found ) then
                  pcref  =  j2code
               end if

            else
!
!              Check for presence of both forms of the variable names.
!
               call dtpool ( item2, found, dim, dtype )

               if ( found ) then

                  call setmsg ( 'Both kernel variables # and # are ' &
                  //            'present in the kernel pool. At most ' &
                  //            'one form of the kernel variable name ' &
                  //            'may be present.'           )
                  call errch  ( '#', item                   )
                  call errch  ( '#', item2                  )
                  call sigerr ( 'SPICE(COMPETINGFRAMESPEC)' )
                  return

               end if

            end if

!
!           Whatever the body, it has quadratic time polynomials for
!           the RA and Dec of the pole, and for the rotation of the
!           Prime Meridian.
!
            item = 'POLE_RA'
            call cleard (             mxpoly,     rcoef )
            call bodvcd ( body, item, mxpoly, na, rcoef )

            if ( failed() ) return

            item = 'POLE_DEC'
            call cleard (             mxpoly,     dcoef )
            call bodvcd ( body, item, mxpoly, nd, dcoef )

            if ( failed() ) return

            item = 'PM'
            call cleard (             mxpoly,     wcoef )
            call bodvcd ( body, item, mxpoly, nw, wcoef )

            if ( failed() ) return

!
!           Whether or not the body is a satellite, there may be
!           additional nutation and libration (THETA) terms.
!
            ntheta = 0
            nphase = 0
            nphsco = 0
            na     = 0
            nd     = 0
            nw     = 0

            item = 'NUT_PREC_ANGLES'

            if ( bodfnd ( refid, item ) ) then

!              Find out whether the maximum phase angle degree
!              has been explicitly set.
!
               item2 = 'MAX_PHASE_DEGREE'

               if ( bodfnd(refid, item2) ) then

                  call bodvcd ( refid, item2, 1, dim, dpval )

                  deg = nint( dpval )

                  if ( ( deg < 1 ) .or. ( deg > mxphas-1 ) ) then

                     call setmsg ( 'Maximum phase angle degree for ' &
                     //            'body # must be in the range 1:# ' &
                     //            'but was #.'                      )
                     call errint ( '#', refid                        )
                     call errint ( '#', mxphas-1                     )
                     call errint ( '#', deg                          )
                     call sigerr ( 'SPICE(DEGREEOUTOFRANGE)'         )
                     return

                  end if

                  nphsco = deg + 1

               else
!
!                 The default degree is 1, yielding two coefficients.
!
                  nphsco = 2

               end if
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
               call bodvcd ( refid, item, maxang*mxphas, ntheta, tcoef )

               if ( failed() ) return

!
!              NPHSCO is at least 1; this division is safe.
!
               nphase = ntheta / nphsco

            end if

!
!           Look up the right ascension nutations in the precession of
!           the pole. NA is the number of Ascension coefficients. AC
!           are the Ascension coefficients.
!
            item = 'NUT_PREC_RA'

            if ( bodfnd ( body, item ) ) then

               call bodvcd ( body, item, maxang, na, ac )

               if ( failed() ) return

            end if

!
!           Look up the declination nutations in the precession of the
!           pole. ND is the number of Declination coefficients. DC are
!           the Declination coefficients.
!
            item = 'NUT_PREC_DEC'

            if ( bodfnd ( body, item ) ) then

               call bodvcd ( body, item, maxang, nd, dc )

               if ( failed() ) return

            end if

!
!           Finally look up the prime meridian nutations. NW is the
!           number of coefficients. WC is the array of coefficients.
!
            item = 'NUT_PREC_PM'

            if ( bodfnd ( body, item ) ) then

               call bodvcd ( body, item, maxang, nw, wc )

               if ( failed() ) return

            end if

!
!           The number of coefficients returned had better not be
!           bigger than the number of angles we are going to compute.
!           If it is we simply signal an error and bag it, for sure.
!
            if ( max( na, nd, nw ) > nphase ) then
               call setmsg ( 'Insufficient number of nutation/' &
               //            'precession angles for body * at time #. ' &
               //            'Number of angles is #; number required ' &
               //            'is #.'                )
               call errint ( '*', body              )
               call errdp  ( '#', et                )
               call errint ( '#', nphase            )
               call errint ( '#', max( na, nd, nw ) )
               call sigerr ( 'SPICE(INSUFFICIENTANGLES)' )
               return
            end if

!
!           We succeeded to fetch all data for this body. Save it in
!           the buffers. First check if there is room. If not, dump the
!           buffers to make room.
!
            call zzhsiavl( bidpol, avail )
            if ( avail <= 0 ) then
               call zzhsiini ( maxbod, bidlst, bidpol )
            end if

!
!           Add this body to the hash and save its data in the buffers.
!
            call zzhsiadd ( bidlst, bidpol, bidids, body, at, new )

            bpckep ( at ) = pckepc
            bpcref ( at ) = pcref
            bnphas ( at ) = nphase
            bnphco ( at ) = nphsco
            bna    ( at ) = na
            bnd    ( at ) = nd
            bnw    ( at ) = nw

            call moved( rcoef( 0 ), mxpoly,        brcoef( 0, at ) )
            call moved( dcoef( 0 ), mxpoly,        bdcoef( 0, at ) )
            call moved( wcoef( 0 ), mxpoly,        bwcoef( 0, at ) )
            call moved( tcoef( 1 ), maxang*nphsco, btcoef( 1, at ) )
            call moved( ac   ( 1 ), maxang,        bac   ( 1, at ) )
            call moved( dc   ( 1 ), maxang,        bdc   ( 1, at ) )
            call moved( wc   ( 1 ), maxang,        bwc   ( 1, at ) )

         end if

!
!        The reference epoch in the PCK is given as JED. Convert to
!        ephemeris seconds past J2000. Then convert the input ET to
!        seconds past the reference epoch.
!
         tmpepc  =  spd() * ( bpckep(at) - j2000() )
         epoch   =  et    -   tmpepc
         td      =  epoch / d
         tc      =  epoch / t

!
!        Evaluate the time polynomials and their derivatives w.r.t.
!        EPOCH at EPOCH.
!
!        Evaluate the time polynomials at EPOCH.
!
         ra  = brcoef(0,at) + tc * ( brcoef(1,at) + tc * brcoef(2,at) )
         dec = bdcoef(0,at) + tc * ( bdcoef(1,at) + tc * bdcoef(2,at) )
         w   = bwcoef(0,at) + td * ( bwcoef(1,at) + td * bwcoef(2,at) )

         dra  = ( brcoef(1,at)  + 2.0_wp*tc*brcoef(2,at) ) / t
         ddec = ( bdcoef(1,at)  + 2.0_wp*tc*bdcoef(2,at) ) / t
         dw   = ( bwcoef(1,at)  + 2.0_wp*td*bwcoef(2,at) ) / d

!
!        Compute the nutations and librations (and their derivatives)
!        as appropriate.
!
         do i = 1, bnphas(at)

            if ( bnphco(at) == 2 ) then
!
!              This case is so common that we'll deal with it
!              separately. We'll avoid unnecessary arithmetic
!              operations.
!
               k = 1 + 2*(i-1)
               m = 1 + k

               theta  = ( btcoef(k,at) + tc * btcoef(m,at) ) * rpd()
               dtheta = ( btcoef(m,at) / t ) * rpd()

            else
!
!              THETA and DTHETA have higher-order terms; there are
!              BNPHCO(AT) coefficients for each angle.
!
               theta = 0.0_wp

               do j = 1, bnphco(at)
!
!                 K is the start index for the coefficients of the
!                 Ith angle.
!
                  k     = j + bnphco(at)*(i-1)

                  theta = theta  + ( tc**(j-1) * btcoef(k,at) )

               end do

               theta  = theta * rpd()

               k = 1 + bnphco(at)*(i-1)
               m = 1 + k

               dtheta = btcoef(m,at) / t

               do j = 3, bnphco(at)

                  k      = j + bnphco(at)*(i-1)
                  l      = j - 1
!
!                 Differentiate with respect to EPOCH the Jth
!                 term of the expression for the Ith angle; add
!                 the result to the current sum.
!
                  dtheta = dtheta + &
                           ( l * tc**(l-1) * btcoef(k,at) ) / t**(l-1)
               end do

               dtheta = dtheta * rpd()

            end if

            sintmp    =  sin ( theta )
            costmp    =  cos ( theta )

            sinth(i)  =  sintmp
            costh(i)  =  costmp
            dsinth(i) =  costmp * dtheta
            dcosth(i) = -sintmp * dtheta

         end do

!
!        Adjust RA, DEC, W and their derivatives by the librations
!        and nutations.
!
         ra  = ra    + vdotg ( bac(1,at), sinth,  bna(at) )
         dec = dec   + vdotg ( bdc(1,at), costh,  bnd(at) )
         w   = w     + vdotg ( bwc(1,at), sinth,  bnw(at) )

         dra  = dra  + vdotg ( bac(1,at), dsinth, bna(at) )
         ddec = ddec + vdotg ( bdc(1,at), dcosth, bnd(at) )
         dw   = dw   + vdotg ( bwc(1,at), dsinth, bnw(at) )

!
!        Convert from degrees to radians
!
         ra      = ra   * rpd()
         dec     = dec  * rpd()
         w       = w    * rpd()

         dra     = dra  * rpd()
         ddec    = ddec * rpd()
         dw      = dw   * rpd()
!
!        Convert to Euler angles.
!
         w       = mod ( w,  twopi() )
         phi     = ra + halfpi()
         delta   = halfpi() - dec

         dphi    =   dra
         ddelta  = - ddec

         if ( failed() ) return

!
!        Pack the Euler angles and their derivatives into
!        a state vector.
!
         call vpack ( w,  delta,  phi,  eulsta    )
         call vpack ( dw, ddelta, dphi, eulsta(4) )

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
         call eul2xf ( eulsta, 3, 1, 3, tsipm )

      end if

!
!     At this point the base frame PCREF has been determined.
!
!     If the requested base frame is not base frame associated with the
!     PCK data, adjust the transformation matrix TSIPM to map from the
!     requested frame to the body-fixed frame.
!
      if ( reqref /= pcref ) then
!
!        Next get the position transformation from the user specified
!        inertial frame to the native PCK inertial frame.
!
         call irfrot ( reqref, pcref, req2pc )

         if ( failed() ) return

!
!        Since we're applying an inertial transformation to TSIPM,
!        we can rotate the non-zero blocks of TSIPM. This saves
!        a bunch of double precision multiplications.
!
!        Extract the upper and lower left blocks of TSIPM.
!
         do i = 1, 3

            do j = 1, 3

               tipm (i,j) =  tsipm( i,   j )
               dtipm(i,j) =  tsipm( i+3, j )

            end do

         end do

!
!        Rotate the blocks. Note this is a right multiplication.
!
         call mxm ( tipm,  req2pc, xtipm  )
         call mxm ( dtipm, req2pc, xdtipm )

!
!        Replace the non-zero blocks of TSIPM. This gives us the
!        transformation from the requested frame to the
!        bodyfixed frame.
!
         do i = 1, 3

            do j = 1, 3

               tsipm( i,   j   ) = xtipm (i,j)
               tsipm( i+3, j+3 ) = xtipm (i,j)
               tsipm( i+3, j   ) = xdtipm(i,j)

            end do

         end do

      end if
!
!     That's all folks. Check out and get out.
!
      return
      end
