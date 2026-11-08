!$Procedure      T_CKW06 ( CK, Write segment, type 6 )

      SUBROUTINE T_CKW06 ( HANDLE,  INST,    REF,     AVFLAG, &
                           FIRST,   LAST,    SEGID,   NINTVL, &
                           NPKTS,   SUBTPS,  DEGRES,  PACKTS, &
                           RATES,   SCLKDP,  IVLBDS,  SELLST )

! $ Abstract
!
!     This is a TSPICE test routine.
!
!     Write a type 6 segment to a CK file, without performing
!     the usual error checks on the input data.
!
! $ Required_Reading
!
!     DAF
!     NAIF_IDS
!     SPC
!     CK
!     TIME
!
! $ Keywords
!
!     ATTITUDE
!     FILES
!     POINTING
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ck06.inc'
      INCLUDE 'ckparam.inc'


      INTEGER               HANDLE
      INTEGER               INST
      CHARACTER*(*)         REF
      LOGICAL               AVFLAG
      DOUBLE PRECISION      FIRST
      DOUBLE PRECISION      LAST
      CHARACTER*(*)         SEGID
      INTEGER               NINTVL
      INTEGER               NPKTS  ( * )
      INTEGER               SUBTPS ( * )
      INTEGER               DEGRES ( * )
      DOUBLE PRECISION      PACKTS ( * )
      DOUBLE PRECISION      RATES  ( * )
      DOUBLE PRECISION      SCLKDP ( * )
      DOUBLE PRECISION      IVLBDS ( * )
      LOGICAL               SELLST

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a CK file open for writing.
!     INST       I   NAIF instrument ID code.
!     REF        I   Reference frame name.
!     AVFLAG     I   True if the segment will contain angular velocity.
!     FIRST      I   Start time of interval covered by segment.
!     LAST       I   End time of interval covered by segment.
!     SEGID      I   Segment identifier.
!     NINTVL     I   Number of mini-segments and interpolation
!                    intervals.
!     NPKTS      I   Array of packet counts of mini-segments.
!     SUBTPS     I   Array of segment subtypes of mini-segments.
!     DEGRES     I   Array of polynomial degrees of mini-segments.
!     PACKTS     I   Array of data packets of mini-segments.
!     RATES      I   Nominal SCLK rates in seconds per tick.
!     SCLKDP     I   Array of epochs of mini-segments.
!     IVLBDS     I   Interpolation interval bounds.
!     SELLST     I   Interval selection flag.
!     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
!
! $ Detailed_Input
!
!     HANDLE         is the handle of a CK file that has been opened
!                    for writing.
!
!
!     INST           is a NAIF integer code associated with an
!                    instrument or spacecraft structure whose
!                    orientation is described by the segment to be
!                    created. INST is treated by the SPICE frame
!                    subsystem as a CK frame class ID (see the
!                    Frames Required Reading for details).
!
!
!     AVFLAG         is a logical flag which indicates whether or not
!                    the segment will contain angular velocity.
!
!
!     REF            is the NAIF name for a reference frame relative to
!                    which the pointing (attitude) information for INST
!                    is specified.
!
!     FIRST,
!     LAST           are, respectively, the bounds of the time interval
!                    over which the segment defines the attitude of
!                    INST. FIRST and LAST are encoded SCLK times.
!
!                    FIRST must be greater than or equal to the first
!                    interpolation interval start time; LAST must be
!                    less than or equal to the last interpolation
!                    interval stop time. See the description of IVLBDS
!                    below.
!
!
!     SEGID          is the segment identifier. A CK segment
!                    identifier may contain up to 40 characters.
!
!
!     NINTVL         is the number of interpolation intervals
!                    associated with the input data. The interpolation
!                    intervals are associated with data sets referred
!                    to as "mini-segments."
!
!                    The input data comprising each mini-segment are:
!
!                       - a packet count
!                       - a type 6 subtype
!                       - an interpolating polynomial degree
!                       - a sequence of type 6 data packets
!                       - a sequence of SCLK rates
!                       - a sequence of packet epochs
!
!                    These inputs are described below.
!
!
!     NPKTS          is an array of packet counts. The Ith element of
!                    NPKTS is the packet count of the Ith interpolation
!                    interval/mini-segment.
!
!                    NPKTS has dimension NINTVL.
!
!
!     SUBTPS         is an array of type 6 subtypes. The Ith element
!                    of SUBTPS is the subtype of the packets associated
!                    with the Ith interpolation interval/mini-segment.
!
!                    SUBTPS has dimension NINTVL.
!
!                    *** If an element of SUBTPS is out of range,
!                        that element is written to the corresponding
!                        mini-segment control area. For the purpose
!                        of writing data to the mini-segment, the actual
!                        subtype (which indicates the packet size) is
!                        is assumed to be 1. ***
!
!
!     DEGRES         is an array of interpolating polynomial degrees.
!                    The Ith element of DEGRES is the polynomial degree
!                    of the packets associated with the Ith
!                    interpolation interval/mini-segment.
!
!                    For subtypes 0 and 2, interpolation degrees must be
!                    equivalent to 3 mod 4, that is, they must be in
!                    the set
!
!                       { 3, 7, 11, ..., MAXDEG }
!
!                    For subtypes 1 and 3, interpolation degrees must
!                    be odd and must be in the range 1:MAXDEG.
!
!                    DEGRES has dimension NINTVL.
!
!
!     PACKTS         is an array of data packets representing the
!                    orientation of INST relative to the frame REF. The
!                    packets for a given mini-segment are stored
!                    contiguously in increasing time order. The order
!                    of the sets of packets for different mini-segments
!                    is the same as the order of their corresponding
!                    interpolation intervals.
!
!                    Each packet contains a SPICE-style quaternion and
!                    optionally, depending on the segment subtype,
!                    attitude derivative data, from which a C-matrix
!                    and an angular velocity vector may be derived.
!
!                    See the discussion of quaternion styles in
!                    Particulars below.
!
!                    The C-matrix CMAT represented by the Ith data
!                    packet is a rotation matrix that transforms the
!                    components of a vector expressed in the base frame
!                    specified by REF to components expressed in the
!                    instrument fixed frame at the time SCLKDP(I).
!
!                    Thus, if a vector V has components x, y, z in the
!                    base frame, then V has components x', y', z'
!                    in the instrument fixed frame where:
!
!                       [ x' ]     [          ] [ x ]
!                       | y' |  =  |   CMAT   | | y |
!                       [ z' ]     [          ] [ z ]
!
!                    Attitude derivative information either explicitly
!                    contained in, or else derived from, PACKTS(I)
!                    gives the angular velocity of the instrument fixed
!                    frame at time SCLKDP(I) with respect to the
!                    reference frame specified by REF.
!
!                    The direction of an angular velocity vector gives
!                    the right-handed axis about which the instrument
!                    fixed reference frame is rotating. The magnitude
!                    of the vector is the magnitude of the
!                    instantaneous velocity of the rotation, in radians
!                    per second.
!
!                    Packet contents and the corresponding
!                    interpolation methods depend on the segment
!                    subtype, and are as follows:
!
!                       Subtype 0:  Hermite interpolation, 8-element
!                                   packets. Quaternion and quaternion
!                                   derivatives only, no angular
!                                   velocity vector provided.
!                                   Quaternion elements are listed
!                                   first, followed by derivatives.
!                                   Angular velocity is derived from
!                                   the quaternions and quaternion
!                                   derivatives.
!
!                       Subtype 1:  Lagrange interpolation, 4-element
!                                   packets. Quaternion only. Angular
!                                   velocity is derived by
!                                   differentiating the interpolating
!                                   polynomials.
!
!                       Subtype 2:  Hermite interpolation, 14-element
!                                   packets.  Quaternion and angular
!                                   angular velocity vector, as well as
!                                   derivatives of each, are provided.
!                                   The quaternion comes first, then
!                                   quaternion derivatives, then
!                                   angular velocity and its
!                                   derivatives.
!
!                       Subtype 3:  Lagrange interpolation, 7-element
!                                   packets. Quaternion and angular
!                                   velocity vector provided.  The
!                                   quaternion comes first.
!
!                    Angular velocity is always specified relative to
!                    the base frame.
!
!                    Units of angular velocity, and of quaternion
!                    derivatives, are radians/second.
!
!                    For the Hermite subtypes (0 and 2), quaternion
!                    representations must be selected so that, for
!                    consecutive quaternions Q(I) and Q(I+1) in a
!                    mini-segment, the distance between Q and Q(I+1) is
!                    less than the distance between Q and -Q(I+1). The
!                    Lagrange subtypes do not have this restriction.
!
!
!     RATES          is an array of nominal rates of the spacecraft
!                    clock associated with INST. The Ith element of
!                    rates is the clock rate for the packets associated
!                    with the Ith interpolation interval/mini-segment.
!                    Units are seconds per tick. Spacecraft clock rates
!                    are used to scale angular velocity to
!                    radians/second.
!
!
!     SCLKDP         is an array containing epochs for all input
!                    mini-segments. The epochs have a one-to-one
!                    relationship with the packets in the input
!                    packet array. The epochs are encoded SCLK times.
!
!                    The epochs for a given mini-segment are stored
!                    contiguously in increasing order. The order of the
!                    sets of epochs for different mini-segments is the
!                    same as the order of their corresponding
!                    interpolation intervals.
!
!                    For each mini-segment, "padding" is allowed: the
!                    sequence of epochs for that mini-segment may start
!                    before the corresponding interpolation interval
!                    start time and end after the corresponding
!                    interpolation interval stop time. Padding is used
!                    to control behavior of interpolating polynomials
!                    near interpolation interval boundaries.
!
!                    Due to possible use of padding, the elements of
!                    SCLKDP, taken as a whole, may not be in increasing
!                    order.
!
!
!     IVLBDS         is an array of interpolation interval boundary
!                    times. This array is a strictly increasing list of
!                    the interpolation interval start times, to which
!                    the end time for the last interval is appended.
!                    The interval bounds are encoded SCLK times.
!
!                    The Ith interpolation interval is the time
!                    coverage interval of the Ith mini-segment (see the
!                    description of NPKTS above).
!
!                    For each mini-segment, the corresponding
!                    interpolation interval's start time is greater
!                    than or equal to the mini-segment's first epoch.
!                    The interval's stop time may exceed the
!                    mini-segment's last epoch, allowing a single
!                    coverage gap to exist between a mini-segment's
!                    last epoch and its interval stop time.
!
!                    For each interpolation interval other than the
!                    last, the interval's coverage stop time coincides
!                    with the coverage start time of the next interval.
!
!                    IVLBDS has dimension NINTVL+1.
!
!
!     SELLST         is a logical flag indicating to the CK type 6
!                    segment reader CKR06 how to select the
!                    interpolation interval when a request time
!                    coincides with a time boundary shared by two
!                    interpolation intervals. When SELLST ("select
!                    last") is .TRUE., the later interval is selected;
!                    otherwise the earlier interval is selected.
!
! $ Detailed_Output
!
!     None.  See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     MAXDEG         is the maximum allowed degree of the interpolating
!                    polynomial.
!
!                    See the INCLUDE file ck06.inc for the value of
!                    MAXDEG.
!
! $ Exceptions
!
!     This routine does not check its inputs.
!
! $ Files
!
!     A new type 6 CK segment is written to the CK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This is a TSPICE test routine.
!
!     This routine writes a CK type 6 data segment to the open CK
!     file according to the format described in the type 6 section of
!     the CK Required Reading. The CK file must have been opened with
!     write access.
!
!
!     Quaternion Styles
!     -----------------
!
!     There are different "styles" of quaternions used in
!     science and engineering applications. Quaternion styles
!     are characterized by
!
!        - The order of quaternion elements
!
!        - The quaternion multiplication formula
!
!        - The convention for associating quaternions
!          with rotation matrices
!
!     Two of the commonly used styles are
!
!        - "SPICE"
!
!           > Invented by Sir William Rowan Hamilton
!           > Frequently used in mathematics and physics textbooks
!
!        - "Engineering"
!
!           > Widely used in aerospace engineering applications
!
!
!     SPICELIB subroutine interfaces ALWAYS use SPICE quaternions.
!     Quaternions of any other style must be converted to SPICE
!     quaternions before they are passed to SPICELIB routines.
!
!
!     Relationship between SPICE and Engineering Quaternions
!     ------------------------------------------------------
!
!     Let M be a rotation matrix such that for any vector V,
!
!        M*V
!
!     is the result of rotating V by theta radians in the
!     counterclockwise direction about unit rotation axis vector A.
!     Then the SPICE quaternions representing M are
!
!        (+/-) (  cos(theta/2),
!                 sin(theta/2) A(1),
!                 sin(theta/2) A(2),
!                 sin(theta/2) A(3)  )
!
!     while the engineering quaternions representing M are
!
!        (+/-) ( -sin(theta/2) A(1),
!                -sin(theta/2) A(2),
!                -sin(theta/2) A(3),
!                 cos(theta/2)       )
!
!     For both styles of quaternions, if a quaternion q represents
!     a rotation matrix M, then -q represents M as well.
!
!     Given an engineering quaternion
!
!        QENG   = ( q0,  q1,  q2,  q3 )
!
!     the equivalent SPICE quaternion is
!
!        QSPICE = ( q3, -q0, -q1, -q2 )
!
!
!     Associating SPICE Quaternions with Rotation Matrices
!     ----------------------------------------------------
!
!     Let FROM and TO be two right-handed reference frames, for
!     example, an inertial frame and a spacecraft-fixed frame. Let the
!     symbols
!
!        V    ,   V
!         FROM     TO
!
!     denote, respectively, an arbitrary vector expressed relative to
!     the FROM and TO frames. Let M denote the transformation matrix
!     that transforms vectors from frame FROM to frame TO; then
!
!        V   =  M * V
!         TO         FROM
!
!     where the expression on the right hand side represents left
!     multiplication of the vector by the matrix.
!
!     Then if the unit-length SPICE quaternion q represents M, where
!
!        q = (q0, q1, q2, q3)
!
!     the elements of M are derived from the elements of q as follows:
!
!          +-                                                         -+
!          |           2    2                                          |
!          | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) |
!          |                                                           |
!          |                                                           |
!          |                               2    2                      |
!      M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) |
!          |                                                           |
!          |                                                           |
!          |                                                   2    2  |
!          | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) |
!          |                                                           |
!          +-                                                         -+
!
!     Note that substituting the elements of -q for those of q in the
!     right hand side leaves each element of M unchanged; this shows
!     that if a quaternion q represents a matrix M, then so does the
!     quaternion -q.
!
!     To map the rotation matrix M to a unit quaternion, we start by
!     decomposing the rotation matrix as a sum of symmetric
!     and skew-symmetric parts:
!
!                                        2
!        M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ]
!
!                     symmetric                   skew-symmetric
!
!
!     OMEGA is a skew-symmetric matrix of the form
!
!                   +-             -+
!                   |  0   -n3   n2 |
!                   |               |
!         OMEGA  =  |  n3   0   -n1 |
!                   |               |
!                   | -n2   n1   0  |
!                   +-             -+
!
!     The vector N of matrix entries (n1, n2, n3) is the rotation axis
!     of M and theta is M's rotation angle.  Note that N and theta
!     are not unique.
!
!     Let
!
!        C = cos(theta/2)
!        S = sin(theta/2)
!
!     Then the unit quaternions Q corresponding to M are
!
!        Q = +/- ( C, S*n1, S*n2, S*n3 )
!
!     The mappings between quaternions and the corresponding rotations
!     are carried out by the SPICELIB routines
!
!        Q2M {quaternion to matrix}
!        M2Q {matrix to quaternion}
!
!     M2Q always returns a quaternion with scalar part greater than
!     or equal to zero.
!
!
!     SPICE Quaternion Multiplication Formula
!     ---------------------------------------
!
!     Given a SPICE quaternion
!
!        Q = ( q0, q1, q2, q3 )
!
!     corresponding to rotation axis A and angle theta as above, we can
!     represent Q using "scalar + vector" notation as follows:
!
!        s =   q0           = cos(theta/2)
!
!        v = ( q1, q2, q3 ) = sin(theta/2) * A
!
!        Q = s + v
!
!     Let Q1 and Q2 be SPICE quaternions with respective scalar
!     and vector parts s1, s2 and v1, v2:
!
!        Q1 = s1 + v1
!        Q2 = s2 + v2
!
!     We represent the dot product of v1 and v2 by
!
!        <v1, v2>
!
!     and the cross product of v1 and v2 by
!
!        v1 x v2
!
!     Then the SPICE quaternion product is
!
!        Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2)
!
!     If Q1 and Q2 represent the rotation matrices M1 and M2
!     respectively, then the quaternion product
!
!        Q1*Q2
!
!     represents the matrix product
!
!        M1*M2
!
!
! $ Examples
!
!     Suppose that you have states and are prepared to produce
!     a segment of type 6 in a CK file.
!
!     The following code fragment could be used to add the new segment
!     to a previously opened CK file attached to HANDLE. The file must
!     have been opened with write access.
!
!        C
!        C     Create a segment identifier.
!        C
!              SEGID = 'MY_SAMPLE_CK_TYPE_6_SEGMENT'
!
!        C
!        C     Write the segment.
!        C
!              CALL T_CKW06 ( HANDLE,  INST,    REF,     AVFLAG,
!             .             FIRST,   LAST,    SEGID,   NINTVL,
!             .             NPKTS,   SUBTPS,  DEGRES,  PACKTS,
!             .             RATES,   SCLKDP,  IVLBDS,  SELLST  )
!
!
! $ Restrictions
!
!     This routine should not be used outside of TSPICE.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman   (JPL)
!     B.V. Semenov   (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS)
!
!-&

! $ Index_Entries
!
!     write spk type_6 ephemeris data segment
!
!-&



!
!     SPICELIB functions
!
      INTEGER               BRCKTI

      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               ND
      PARAMETER           ( ND     =   2 )

      INTEGER               NI
      PARAMETER           ( NI     =   6 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ =   5 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE  =  6 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ = 100 )

!
!     Packet structure parameters
!

!
!     Local variables
!
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( DSCSIZ )

      INTEGER               BEPIX
      INTEGER               EEPIX
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               ISEL
      INTEGER               J
      INTEGER               K
      INTEGER               MINISZ
      INTEGER               NDIR
      INTEGER               PKTBEG
      INTEGER               PKTDSZ
      INTEGER               PKTEND
      INTEGER               PKTSIZ
      INTEGER               PKTSZS ( 0 : C06NST-1 )
      INTEGER               REFCOD
      INTEGER               SEGBEG
      INTEGER               SEGEND
      INTEGER               SUBTYP
      INTEGER               WINSIZ

!
!     Saved values
!
      SAVE                  PKTSZS

!
!     Initial values
!
      DATA                  PKTSZS  /  C06PS0, C06PS1, C06PS2, C06PS3 /


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_CKW06' )

!
!     Check the input data before writing to the file.
!
!     This order of operations entails some redundant
!     calculations, but it allows for rapid error
!     detection.
!
!     Initialize the mini-segment packet array indices,
!     and those of the mini-segment epoch array as well.
!
      PKTBEG = 0
      PKTEND = 0

      BEPIX  = 0
      EEPIX  = 0


!
!     If we made it this far, we're ready to start writing the segment.
!
!     The type 6 segment structure is eloquently described by this
!     diagram from the CK Required Reading:
!
!        +--------------------------------+
!        | Interval 1 mini-segment        |
!        +--------------------------------+
!              .
!              .
!              .
!        +--------------------------------+
!        | Interval N mini-segment        |
!        +--------------------------------+
!        | Interval 1 start time          |
!        +--------------------------------+
!              .
!              .
!              .
!        +--------------------------------+
!        | Interval N start time          |
!        +--------------------------------+
!        | Interval N stop time           |
!        +--------------------------------+
!        | Interval start 100             | (First interval directory)
!        +--------------------------------+
!              .
!              .
!              .
!        +--------------------------------+
!        | Interval start (N/100)*100     | (Last interval directory)
!        +--------------------------------+
!        | Interval 1 start pointer       |
!        +--------------------------------+
!              .
!              .
!              .
!        +--------------------------------+
!        | Interval N start pointer       |
!        +--------------------------------+
!        | Interval N stop pointer + 1    |
!        +--------------------------------+
!        | Boundary choice flag           |
!        +--------------------------------+
!        | Number of intervals            |
!        +--------------------------------+
!
!     CK type 6 mini-segments have the following structure:
!
!        +-----------------------+
!        | Packet 1              |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Packet M              |
!        +-----------------------+
!        | Epoch 1               |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Epoch M               |
!        +-----------------------+
!        | Epoch 100             | (First time tag directory)
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Epoch ((M-1)/100)*100 | (Last time tag directory)
!        +-----------------------+
!        | Clock rate (sec/tick) |
!        +-----------------------+
!        | Subtype code          |
!        +-----------------------+
!        | Window size           |
!        +-----------------------+
!        | Number of packets     |
!        +-----------------------+
!
!     Note that the set of parameters at the end of a mini-segment does
!     not contain an interpolation interval count. This is because,
!     unlike a CK type 5 segment, a CK type 6 segment can contain at
!     most one gap. If present, the gap is located at the end of
!     mini-segment's interpolation interval.
!
!
!     Get the NAIF integer code for the reference frame.
!
      CALL NAMFRM ( REF, REFCOD )


!     Create the segment descriptor. We don't use CKPDS because
!     that routine doesn't allow creation of a singleton segment.
!
      IC(1) = INST
      IC(2) = REFCOD
      IC(3) = DTYPE

      IF ( AVFLAG ) THEN
         IC(4) = 1
      ELSE
         IC(4) = 0
      END IF

      DC(1) = FIRST
      DC(2) = LAST

      CALL DAFPS  ( ND, NI, DC, IC, DESCR )

!
!     Begin a new segment.
!
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_CKW06' )
         RETURN
      END IF

!
!     Re-initialize the mini-segment packet array indices,
!     and those of the mini-segment epoch array as well.
!
      PKTBEG = 0
      PKTEND = 0

      BEPIX  = 0
      EEPIX  = 0

!
!     Write data for each mini-segment to the file.
!
      DO I = 1, NINTVL
!
!        Set the packet size, which is a function of the subtype.
!
         SUBTYP = BRCKTI( SUBTPS( I ),  C06TP0, C06TP3 )

         IF ( SUBTYP .NE. SUBTPS(I) ) THEN
            SUBTYP = 1
         END IF

         PKTSIZ = PKTSZS ( SUBTYP )

         IF ( ODD(SUBTYP) ) THEN

            WINSIZ =   DEGRES(I) + 1
         ELSE
            WINSIZ = ( DEGRES(I) + 1 ) / 2
         END IF

!
!        Now that we have the packet size, we can compute
!        mini-segment packet index range. We'll let PKTDSZ
!        be the total count of packet data entries for this
!        mini-segment.
!
         PKTDSZ = NPKTS(I) * PKTSIZ

         PKTBEG = PKTEND  +  1
         PKTEND = PKTBEG  -  1  +  PKTDSZ

!
!        At this point, we're read to start writing the
!        current mini-segment to the file. Start with the
!        packet data.
!
         CALL DAFADA ( PACKTS(PKTBEG), PKTDSZ )

!
!        Write the epochs for this mini-segment.
!
         BEPIX = EEPIX + 1
         EEPIX = BEPIX - 1 + NPKTS(I)

         CALL DAFADA ( SCLKDP(BEPIX), NPKTS(I) )

!
!        Compute the number of epoch directories for the
!        current mini-segment.
!
         NDIR = ( NPKTS(I) - 1 ) / DIRSIZ

!
!        Write the epoch directories to the segment.
!
         DO J = 1, NDIR

            K = BEPIX  -  1  +  ( J * DIRSIZ )

            CALL DAFADA ( SCLKDP(K), 1 )

         END DO

!
!        Write the mini-segment's SCLK rate, subtype, window size, and
!        packet count to the segment.
!
         CALL DAFADA ( RATES(I),           1 )
         CALL DAFADA ( DBLE( SUBTPS(I) ),  1 )
         CALL DAFADA ( DBLE( WINSIZ    ),  1 )
         CALL DAFADA ( DBLE( NPKTS(I)  ),  1 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_CKW06' )
            RETURN
         END IF

      END DO

!
!     We've finished writing the mini-segments.
!
!     Next write the interpolation interval bounds.
!
      CALL DAFADA ( IVLBDS, NINTVL+1 )

!
!     Create and write directories for the interval
!     bounds.
!
!     The directory count is the interval bound count
!     (N+1), minus 1, divided by the directory size.
!
      NDIR = NINTVL / DIRSIZ

      DO I = 1, NDIR
         CALL DAFADA (  IVLBDS( DIRSIZ*I ),  1  )
      END DO

!
!     Now we compute and write the start/stop pointers
!     for each mini-segment.
!
!     The pointers are relative to the DAF address
!     preceding the segment. For example, a pointer
!     to the first DAF address in the segment has
!     value 1.
!
      SEGEND = 0

      DO I = 1, NINTVL
!
!        Set the packet size, which is a function of the subtype. Also
!        set the window size. First check the subtype, which will be
!        used as an array index.
!
         SUBTYP = BRCKTI( SUBTPS( I ),  C06TP0, C06TP3 )

         IF ( SUBTYP .NE. SUBTPS(I) ) THEN
            SUBTYP = 1
         END IF

         PKTSIZ = PKTSZS ( SUBTYP )

!
!        In order to compute the end pointer of the current
!        mini-segment, we must compute the size, in terms
!        of DAF addresses, of this mini-segment. The formula
!        for the size is
!
!            size =     n_packets * packet_size
!                    +  n_epochs
!                    +  n_epoch_directories
!                    +  4
!
!                 =     n_packets * ( packet_size + 1 )
!                    +  ( n_packets - 1 ) / DIRSIZ
!                    +  4
!
         MINISZ =       NPKTS(I) * ( PKTSIZ + 1 ) &
                    + ( NPKTS(I) - 1 ) / DIRSIZ &
                    +   4


         SEGBEG = SEGEND + 1
         SEGEND = SEGBEG + MINISZ - 1

!
!        Write the mini-segment begin pointer.
!
!        After the loop terminates, the final end pointer, incremented
!        by 1, will be written.
!
         CALL DAFADA ( DBLE(SEGBEG), 1 )

      END DO

!
!     Write the last mini-segment end pointer, incremented by one.
!     SEGEND was computed on the last iteration of the above loop.
!
      CALL DAFADA ( DBLE(SEGEND+1), 1 )

!
!     Write out the interval selection flag. The input
!     boolean value is represented by a numeric constant.
!
      IF ( SELLST ) THEN

         ISEL = ITRUE
      ELSE
         ISEL = IFALSE
      END IF

      CALL DAFADA ( DBLE( ISEL ),  1 )

!
!     Write the mini-segment/interpolation interval count.
!
      CALL DAFADA ( DBLE(NINTVL), 1 )

!
!     End the segment.
!
      CALL DAFENA

      CALL CHKOUT ( 'T_CKW06' )
      RETURN
      END
