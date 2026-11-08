C$Procedure      T_CKW06 ( CK, Write segment, type 6 )
 
      SUBROUTINE T_CKW06 ( HANDLE,  INST,    REF,     AVFLAG,
     .                     FIRST,   LAST,    SEGID,   NINTVL,
     .                     NPKTS,   SUBTPS,  DEGRES,  PACKTS, 
     .                     RATES,   SCLKDP,  IVLBDS,  SELLST )

C$ Abstract
C
C     This is a TSPICE test routine.
C
C     Write a type 6 segment to a CK file, without performing
C     the usual error checks on the input data.
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
C     DAF
C     NAIF_IDS
C     SPC
C     CK
C     TIME
C
C$ Keywords
C
C     ATTITUDE
C     FILES
C     POINTING
C
C$ Declarations
 
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
      
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     HANDLE     I   Handle of a CK file open for writing.
C     INST       I   NAIF instrument ID code.
C     REF        I   Reference frame name.
C     AVFLAG     I   True if the segment will contain angular velocity.
C     FIRST      I   Start time of interval covered by segment.
C     LAST       I   End time of interval covered by segment.
C     SEGID      I   Segment identifier.
C     NINTVL     I   Number of mini-segments and interpolation 
C                    intervals.
C     NPKTS      I   Array of packet counts of mini-segments.
C     SUBTPS     I   Array of segment subtypes of mini-segments.
C     DEGRES     I   Array of polynomial degrees of mini-segments.
C     PACKTS     I   Array of data packets of mini-segments.
C     RATES      I   Nominal SCLK rates in seconds per tick.
C     SCLKDP     I   Array of epochs of mini-segments.
C     IVLBDS     I   Interpolation interval bounds.
C     SELLST     I   Interval selection flag.
C     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
C
C$ Detailed_Input
C
C     HANDLE         is the handle of a CK file that has been opened
C                    for writing.
C
C
C     INST           is a NAIF integer code associated with an
C                    instrument or spacecraft structure whose
C                    orientation is described by the segment to be
C                    created. INST is treated by the SPICE frame
C                    subsystem as a CK frame class ID (see the
C                    Frames Required Reading for details).
C
C
C     AVFLAG         is a logical flag which indicates whether or not
C                    the segment will contain angular velocity.
C
C
C     REF            is the NAIF name for a reference frame relative to
C                    which the pointing (attitude) information for INST
C                    is specified.
C
C     FIRST,
C     LAST           are, respectively, the bounds of the time interval
C                    over which the segment defines the attitude of
C                    INST. FIRST and LAST are encoded SCLK times.
C
C                    FIRST must be greater than or equal to the first
C                    interpolation interval start time; LAST must be
C                    less than or equal to the last interpolation 
C                    interval stop time. See the description of IVLBDS
C                    below.
C
C
C     SEGID          is the segment identifier. A CK segment
C                    identifier may contain up to 40 characters.
C
C
C     NINTVL         is the number of interpolation intervals
C                    associated with the input data. The interpolation
C                    intervals are associated with data sets referred
C                    to as "mini-segments."
C
C                    The input data comprising each mini-segment are:
C            
C                       - a packet count
C                       - a type 6 subtype
C                       - an interpolating polynomial degree
C                       - a sequence of type 6 data packets
C                       - a sequence of SCLK rates
C                       - a sequence of packet epochs
C                    
C                    These inputs are described below.
C
C
C     NPKTS          is an array of packet counts. The Ith element of
C                    NPKTS is the packet count of the Ith interpolation
C                    interval/mini-segment.
C
C                    NPKTS has dimension NINTVL.
C
C
C     SUBTPS         is an array of type 6 subtypes. The Ith element
C                    of SUBTPS is the subtype of the packets associated
C                    with the Ith interpolation interval/mini-segment.
C
C                    SUBTPS has dimension NINTVL.
C
C                    *** If an element of SUBTPS is out of range,
C                        that element is written to the corresponding
C                        mini-segment control area. For the purpose
C                        of writing data to the mini-segment, the actual
C                        subtype (which indicates the packet size) is 
C                        is assumed to be 1. ***
C
C
C     DEGRES         is an array of interpolating polynomial degrees.
C                    The Ith element of DEGRES is the polynomial degree
C                    of the packets associated with the Ith
C                    interpolation interval/mini-segment.
C
C                    For subtypes 0 and 2, interpolation degrees must be
C                    equivalent to 3 mod 4, that is, they must be in
C                    the set
C
C                       { 3, 7, 11, ..., MAXDEG }
C
C                    For subtypes 1 and 3, interpolation degrees must
C                    be odd and must be in the range 1:MAXDEG.
C
C                    DEGRES has dimension NINTVL.
C
C
C     PACKTS         is an array of data packets representing the
C                    orientation of INST relative to the frame REF. The
C                    packets for a given mini-segment are stored
C                    contiguously in increasing time order. The order
C                    of the sets of packets for different mini-segments
C                    is the same as the order of their corresponding
C                    interpolation intervals.
C
C                    Each packet contains a SPICE-style quaternion and
C                    optionally, depending on the segment subtype,
C                    attitude derivative data, from which a C-matrix
C                    and an angular velocity vector may be derived.
C
C                    See the discussion of quaternion styles in
C                    Particulars below.
C
C                    The C-matrix CMAT represented by the Ith data
C                    packet is a rotation matrix that transforms the
C                    components of a vector expressed in the base frame
C                    specified by REF to components expressed in the
C                    instrument fixed frame at the time SCLKDP(I).
C
C                    Thus, if a vector V has components x, y, z in the
C                    base frame, then V has components x', y', z'
C                    in the instrument fixed frame where:
C
C                       [ x' ]     [          ] [ x ]
C                       | y' |  =  |   CMAT   | | y |
C                       [ z' ]     [          ] [ z ]
C                    
C                    Attitude derivative information either explicitly
C                    contained in, or else derived from, PACKTS(I)
C                    gives the angular velocity of the instrument fixed
C                    frame at time SCLKDP(I) with respect to the
C                    reference frame specified by REF.
C
C                    The direction of an angular velocity vector gives
C                    the right-handed axis about which the instrument
C                    fixed reference frame is rotating. The magnitude
C                    of the vector is the magnitude of the
C                    instantaneous velocity of the rotation, in radians
C                    per second.
C
C                    Packet contents and the corresponding
C                    interpolation methods depend on the segment
C                    subtype, and are as follows:
C
C                       Subtype 0:  Hermite interpolation, 8-element
C                                   packets. Quaternion and quaternion
C                                   derivatives only, no angular
C                                   velocity vector provided.
C                                   Quaternion elements are listed
C                                   first, followed by derivatives.
C                                   Angular velocity is derived from
C                                   the quaternions and quaternion
C                                   derivatives. 
C
C                       Subtype 1:  Lagrange interpolation, 4-element
C                                   packets. Quaternion only. Angular
C                                   velocity is derived by
C                                   differentiating the interpolating
C                                   polynomials.
C
C                       Subtype 2:  Hermite interpolation, 14-element
C                                   packets.  Quaternion and angular
C                                   angular velocity vector, as well as
C                                   derivatives of each, are provided.
C                                   The quaternion comes first, then
C                                   quaternion derivatives, then
C                                   angular velocity and its
C                                   derivatives.
C
C                       Subtype 3:  Lagrange interpolation, 7-element
C                                   packets. Quaternion and angular
C                                   velocity vector provided.  The 
C                                   quaternion comes first.
C
C                    Angular velocity is always specified relative to
C                    the base frame. 
C
C                    Units of angular velocity, and of quaternion
C                    derivatives, are radians/second.
C
C                    For the Hermite subtypes (0 and 2), quaternion
C                    representations must be selected so that, for
C                    consecutive quaternions Q(I) and Q(I+1) in a
C                    mini-segment, the distance between Q and Q(I+1) is
C                    less than the distance between Q and -Q(I+1). The
C                    Lagrange subtypes do not have this restriction.
C                    
C
C     RATES          is an array of nominal rates of the spacecraft
C                    clock associated with INST. The Ith element of
C                    rates is the clock rate for the packets associated
C                    with the Ith interpolation interval/mini-segment.
C                    Units are seconds per tick. Spacecraft clock rates
C                    are used to scale angular velocity to
C                    radians/second.
C
C
C     SCLKDP         is an array containing epochs for all input
C                    mini-segments. The epochs have a one-to-one
C                    relationship with the packets in the input 
C                    packet array. The epochs are encoded SCLK times.
C
C                    The epochs for a given mini-segment are stored
C                    contiguously in increasing order. The order of the
C                    sets of epochs for different mini-segments is the
C                    same as the order of their corresponding
C                    interpolation intervals.
C
C                    For each mini-segment, "padding" is allowed: the
C                    sequence of epochs for that mini-segment may start
C                    before the corresponding interpolation interval
C                    start time and end after the corresponding
C                    interpolation interval stop time. Padding is used
C                    to control behavior of interpolating polynomials
C                    near interpolation interval boundaries.
C
C                    Due to possible use of padding, the elements of
C                    SCLKDP, taken as a whole, may not be in increasing
C                    order.
C
C
C     IVLBDS         is an array of interpolation interval boundary
C                    times. This array is a strictly increasing list of
C                    the interpolation interval start times, to which
C                    the end time for the last interval is appended.
C                    The interval bounds are encoded SCLK times.
C
C                    The Ith interpolation interval is the time
C                    coverage interval of the Ith mini-segment (see the
C                    description of NPKTS above).
C
C                    For each mini-segment, the corresponding
C                    interpolation interval's start time is greater
C                    than or equal to the mini-segment's first epoch.
C                    The interval's stop time may exceed the
C                    mini-segment's last epoch, allowing a single
C                    coverage gap to exist between a mini-segment's
C                    last epoch and its interval stop time.
C
C                    For each interpolation interval other than the
C                    last, the interval's coverage stop time coincides
C                    with the coverage start time of the next interval.
C                 
C                    IVLBDS has dimension NINTVL+1.
C
C
C     SELLST         is a logical flag indicating to the CK type 6
C                    segment reader CKR06 how to select the
C                    interpolation interval when a request time
C                    coincides with a time boundary shared by two
C                    interpolation intervals. When SELLST ("select
C                    last") is .TRUE., the later interval is selected;
C                    otherwise the earlier interval is selected.
C   
C$ Detailed_Output
C
C     None.  See $Particulars for a description of the effect of this
C     routine.
C
C$ Parameters
C
C     MAXDEG         is the maximum allowed degree of the interpolating
C                    polynomial. 
C
C                    See the INCLUDE file ck06.inc for the value of
C                    MAXDEG.
C
C$ Exceptions
C
C     This routine does not check its inputs.
C
C$ Files
C
C     A new type 6 CK segment is written to the CK file attached
C     to HANDLE.
C
C$ Particulars
C
C     This is a TSPICE test routine.
C
C     This routine writes a CK type 6 data segment to the open CK
C     file according to the format described in the type 6 section of
C     the CK Required Reading. The CK file must have been opened with
C     write access.
C
C
C     Quaternion Styles
C     -----------------
C
C     There are different "styles" of quaternions used in 
C     science and engineering applications. Quaternion styles
C     are characterized by 
C
C        - The order of quaternion elements
C
C        - The quaternion multiplication formula
C
C        - The convention for associating quaternions
C          with rotation matrices
C
C     Two of the commonly used styles are
C
C        - "SPICE"
C
C           > Invented by Sir William Rowan Hamilton
C           > Frequently used in mathematics and physics textbooks
C
C        - "Engineering"
C
C           > Widely used in aerospace engineering applications
C
C
C     SPICELIB subroutine interfaces ALWAYS use SPICE quaternions.
C     Quaternions of any other style must be converted to SPICE
C     quaternions before they are passed to SPICELIB routines.
C     
C
C     Relationship between SPICE and Engineering Quaternions
C     ------------------------------------------------------
C
C     Let M be a rotation matrix such that for any vector V, 
C
C        M*V
C
C     is the result of rotating V by theta radians in the 
C     counterclockwise direction about unit rotation axis vector A.
C     Then the SPICE quaternions representing M are
C
C        (+/-) (  cos(theta/2), 
C                 sin(theta/2) A(1),  
C                 sin(theta/2) A(2),  
C                 sin(theta/2) A(3)  ) 
C
C     while the engineering quaternions representing M are 
C
C        (+/-) ( -sin(theta/2) A(1),  
C                -sin(theta/2) A(2),  
C                -sin(theta/2) A(3),
C                 cos(theta/2)       )
C
C     For both styles of quaternions, if a quaternion q represents
C     a rotation matrix M, then -q represents M as well.
C
C     Given an engineering quaternion
C
C        QENG   = ( q0,  q1,  q2,  q3 )
C
C     the equivalent SPICE quaternion is
C
C        QSPICE = ( q3, -q0, -q1, -q2 )
C
C
C     Associating SPICE Quaternions with Rotation Matrices
C     ----------------------------------------------------
C
C     Let FROM and TO be two right-handed reference frames, for
C     example, an inertial frame and a spacecraft-fixed frame. Let the
C     symbols
C
C        V    ,   V
C         FROM     TO
C
C     denote, respectively, an arbitrary vector expressed relative to
C     the FROM and TO frames. Let M denote the transformation matrix
C     that transforms vectors from frame FROM to frame TO; then
C
C        V   =  M * V
C         TO         FROM
C
C     where the expression on the right hand side represents left
C     multiplication of the vector by the matrix.
C
C     Then if the unit-length SPICE quaternion q represents M, where
C
C        q = (q0, q1, q2, q3)
C
C     the elements of M are derived from the elements of q as follows:
C
C          +-                                                         -+
C          |           2    2                                          |
C          | 1 - 2*( q2 + q3 )   2*(q1*q2 - q0*q3)   2*(q1*q3 + q0*q2) |
C          |                                                           |
C          |                                                           |
C          |                               2    2                      |
C      M = | 2*(q1*q2 + q0*q3)   1 - 2*( q1 + q3 )   2*(q2*q3 - q0*q1) |
C          |                                                           |
C          |                                                           |
C          |                                                   2    2  |
C          | 2*(q1*q3 - q0*q2)   2*(q2*q3 + q0*q1)   1 - 2*( q1 + q2 ) |
C          |                                                           |
C          +-                                                         -+
C
C     Note that substituting the elements of -q for those of q in the
C     right hand side leaves each element of M unchanged; this shows
C     that if a quaternion q represents a matrix M, then so does the
C     quaternion -q.
C
C     To map the rotation matrix M to a unit quaternion, we start by
C     decomposing the rotation matrix as a sum of symmetric
C     and skew-symmetric parts:
C
C                                        2
C        M = [ I  +  (1-cos(theta)) OMEGA  ] + [ sin(theta) OMEGA ]
C
C                     symmetric                   skew-symmetric
C
C
C     OMEGA is a skew-symmetric matrix of the form
C
C                   +-             -+
C                   |  0   -n3   n2 |
C                   |               |
C         OMEGA  =  |  n3   0   -n1 |
C                   |               |
C                   | -n2   n1   0  |
C                   +-             -+
C
C     The vector N of matrix entries (n1, n2, n3) is the rotation axis
C     of M and theta is M's rotation angle.  Note that N and theta
C     are not unique.
C
C     Let
C
C        C = cos(theta/2)
C        S = sin(theta/2)
C
C     Then the unit quaternions Q corresponding to M are
C
C        Q = +/- ( C, S*n1, S*n2, S*n3 )
C
C     The mappings between quaternions and the corresponding rotations
C     are carried out by the SPICELIB routines
C
C        Q2M {quaternion to matrix}
C        M2Q {matrix to quaternion}
C
C     M2Q always returns a quaternion with scalar part greater than
C     or equal to zero.
C
C
C     SPICE Quaternion Multiplication Formula
C     ---------------------------------------
C
C     Given a SPICE quaternion 
C
C        Q = ( q0, q1, q2, q3 )
C
C     corresponding to rotation axis A and angle theta as above, we can
C     represent Q using "scalar + vector" notation as follows:
C
C        s =   q0           = cos(theta/2)
C
C        v = ( q1, q2, q3 ) = sin(theta/2) * A
C
C        Q = s + v
C
C     Let Q1 and Q2 be SPICE quaternions with respective scalar
C     and vector parts s1, s2 and v1, v2:
C 
C        Q1 = s1 + v1
C        Q2 = s2 + v2
C
C     We represent the dot product of v1 and v2 by
C
C        <v1, v2>
C
C     and the cross product of v1 and v2 by
C
C        v1 x v2
C
C     Then the SPICE quaternion product is
C
C        Q1*Q2 = s1*s2 - <v1,v2>  + s1*v2 + s2*v1 + (v1 x v2)       
C
C     If Q1 and Q2 represent the rotation matrices M1 and M2 
C     respectively, then the quaternion product
C
C        Q1*Q2
C
C     represents the matrix product
C
C        M1*M2
C
C
C$ Examples
C
C     Suppose that you have states and are prepared to produce
C     a segment of type 6 in a CK file.
C
C     The following code fragment could be used to add the new segment
C     to a previously opened CK file attached to HANDLE. The file must
C     have been opened with write access.
C
C        C
C        C     Create a segment identifier.
C        C
C              SEGID = 'MY_SAMPLE_CK_TYPE_6_SEGMENT'
C
C        C
C        C     Write the segment.
C        C
C              CALL T_CKW06 ( HANDLE,  INST,    REF,     AVFLAG,
C             .             FIRST,   LAST,    SEGID,   NINTVL,  
C             .             NPKTS,   SUBTPS,  DEGRES,  PACKTS,
C             .             RATES,   SCLKDP,  IVLBDS,  SELLST  )
C
C
C$ Restrictions
C
C     This routine should not be used outside of TSPICE.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman   (JPL)
C     B.V. Semenov   (JPL)
C
C$ Version
C
C-    SPICELIB Version 1.0.0, 05-FEB-2014 (NJB) (BVS)
C
C-&
 
C$ Index_Entries
C
C     write spk type_6 ephemeris data segment
C
C-&
 
 
 
C
C     SPICELIB functions
C      
      INTEGER               BRCKTI

      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN
 
C
C     Local parameters
C 
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

C
C     Packet structure parameters
C
         
C
C     Local variables
C
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

C
C     Saved values
C
      SAVE                  PKTSZS

C
C     Initial values
C
      DATA                  PKTSZS  /  C06PS0, C06PS1, C06PS2, C06PS3 /

     
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'T_CKW06' )
          
C
C     Check the input data before writing to the file. 
C
C     This order of operations entails some redundant 
C     calculations, but it allows for rapid error 
C     detection.
C     
C     Initialize the mini-segment packet array indices,
C     and those of the mini-segment epoch array as well.
C
      PKTBEG = 0
      PKTEND = 0

      BEPIX  = 0
      EEPIX  = 0
 

C     
C     If we made it this far, we're ready to start writing the segment.
C
C     The type 6 segment structure is eloquently described by this
C     diagram from the CK Required Reading:
C
C        +--------------------------------+
C        | Interval 1 mini-segment        |
C        +--------------------------------+
C              .
C              .
C              .
C        +--------------------------------+
C        | Interval N mini-segment        |
C        +--------------------------------+
C        | Interval 1 start time          |
C        +--------------------------------+
C              .
C              .
C              .
C        +--------------------------------+
C        | Interval N start time          |
C        +--------------------------------+
C        | Interval N stop time           |
C        +--------------------------------+
C        | Interval start 100             | (First interval directory)
C        +--------------------------------+
C              .
C              .
C              .
C        +--------------------------------+
C        | Interval start (N/100)*100     | (Last interval directory)
C        +--------------------------------+
C        | Interval 1 start pointer       |
C        +--------------------------------+
C              .
C              .
C              .
C        +--------------------------------+
C        | Interval N start pointer       |
C        +--------------------------------+
C        | Interval N stop pointer + 1    |
C        +--------------------------------+
C        | Boundary choice flag           |
C        +--------------------------------+
C        | Number of intervals            |
C        +--------------------------------+
C
C     CK type 6 mini-segments have the following structure:
C
C        +-----------------------+
C        | Packet 1              |
C        +-----------------------+
C                    .
C                    .
C                    .
C        +-----------------------+
C        | Packet M              |
C        +-----------------------+
C        | Epoch 1               |
C        +-----------------------+
C                    .
C                    .
C                    .
C        +-----------------------+
C        | Epoch M               |
C        +-----------------------+
C        | Epoch 100             | (First time tag directory)
C        +-----------------------+
C                    .
C                    .
C                    .
C        +-----------------------+
C        | Epoch ((M-1)/100)*100 | (Last time tag directory)
C        +-----------------------+
C        | Clock rate (sec/tick) |
C        +-----------------------+
C        | Subtype code          |
C        +-----------------------+
C        | Window size           |
C        +-----------------------+
C        | Number of packets     |
C        +-----------------------+
C
C     Note that the set of parameters at the end of a mini-segment does
C     not contain an interpolation interval count. This is because,
C     unlike a CK type 5 segment, a CK type 6 segment can contain at
C     most one gap. If present, the gap is located at the end of
C     mini-segment's interpolation interval.
C
C
C     Get the NAIF integer code for the reference frame.
C
      CALL NAMFRM ( REF, REFCOD )


C     Create the segment descriptor. We don't use CKPDS because
C     that routine doesn't allow creation of a singleton segment.
C
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
 
C
C     Begin a new segment.
C
      CALL DAFBNA ( HANDLE, DESCR, SEGID )
 
      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'T_CKW06' )
         RETURN
      END IF

C
C     Re-initialize the mini-segment packet array indices,
C     and those of the mini-segment epoch array as well.
C
      PKTBEG = 0
      PKTEND = 0

      BEPIX  = 0
      EEPIX  = 0

C
C     Write data for each mini-segment to the file.
C
      DO I = 1, NINTVL
C
C        Set the packet size, which is a function of the subtype.
C         
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

C
C        Now that we have the packet size, we can compute
C        mini-segment packet index range. We'll let PKTDSZ
C        be the total count of packet data entries for this
C        mini-segment.
C
         PKTDSZ = NPKTS(I) * PKTSIZ

         PKTBEG = PKTEND  +  1
         PKTEND = PKTBEG  -  1  +  PKTDSZ

C
C        At this point, we're read to start writing the 
C        current mini-segment to the file. Start with the
C        packet data.
C
         CALL DAFADA ( PACKTS(PKTBEG), PKTDSZ )

C
C        Write the epochs for this mini-segment.
C
         BEPIX = EEPIX + 1
         EEPIX = BEPIX - 1 + NPKTS(I)

         CALL DAFADA ( SCLKDP(BEPIX), NPKTS(I) )

C
C        Compute the number of epoch directories for the 
C        current mini-segment.
C
         NDIR = ( NPKTS(I) - 1 ) / DIRSIZ

C        
C        Write the epoch directories to the segment.
C
         DO J = 1, NDIR

            K = BEPIX  -  1  +  ( J * DIRSIZ )

            CALL DAFADA ( SCLKDP(K), 1 )

         END DO

C
C        Write the mini-segment's SCLK rate, subtype, window size, and
C        packet count to the segment.
C
         CALL DAFADA ( RATES(I),           1 )
         CALL DAFADA ( DBLE( SUBTPS(I) ),  1 )
         CALL DAFADA ( DBLE( WINSIZ    ),  1 )
         CALL DAFADA ( DBLE( NPKTS(I)  ),  1 )

         IF ( FAILED() ) THEN
            CALL CHKOUT ( 'T_CKW06' )
            RETURN
         END IF

      END DO
 
C
C     We've finished writing the mini-segments.
C
C     Next write the interpolation interval bounds.
C
      CALL DAFADA ( IVLBDS, NINTVL+1 )
 
C
C     Create and write directories for the interval
C     bounds.
C
C     The directory count is the interval bound count
C     (N+1), minus 1, divided by the directory size.
C
      NDIR = NINTVL / DIRSIZ

      DO I = 1, NDIR
         CALL DAFADA (  IVLBDS( DIRSIZ*I ),  1  )
      END DO
 
C
C     Now we compute and write the start/stop pointers
C     for each mini-segment.
C
C     The pointers are relative to the DAF address 
C     preceding the segment. For example, a pointer
C     to the first DAF address in the segment has
C     value 1.
C
      SEGEND = 0

      DO I = 1, NINTVL
C
C        Set the packet size, which is a function of the subtype. Also
C        set the window size. First check the subtype, which will be
C        used as an array index.
C
         SUBTYP = BRCKTI( SUBTPS( I ),  C06TP0, C06TP3 )

         IF ( SUBTYP .NE. SUBTPS(I) ) THEN
            SUBTYP = 1
         END IF

         PKTSIZ = PKTSZS ( SUBTYP )

C
C        In order to compute the end pointer of the current
C        mini-segment, we must compute the size, in terms
C        of DAF addresses, of this mini-segment. The formula
C        for the size is
C
C            size =     n_packets * packet_size 
C                    +  n_epochs
C                    +  n_epoch_directories
C                    +  4
C
C                 =     n_packets * ( packet_size + 1 )
C                    +  ( n_packets - 1 ) / DIRSIZ 
C                    +  4
C
         MINISZ =       NPKTS(I) * ( PKTSIZ + 1 )
     .              + ( NPKTS(I) - 1 ) / DIRSIZ
     .              +   4


         SEGBEG = SEGEND + 1
         SEGEND = SEGBEG + MINISZ - 1

C
C        Write the mini-segment begin pointer. 
C
C        After the loop terminates, the final end pointer, incremented
C        by 1, will be written.
C        
         CALL DAFADA ( DBLE(SEGBEG), 1 )

      END DO

C
C     Write the last mini-segment end pointer, incremented by one.
C     SEGEND was computed on the last iteration of the above loop.
C        
      CALL DAFADA ( DBLE(SEGEND+1), 1 )

C
C     Write out the interval selection flag. The input
C     boolean value is represented by a numeric constant.
C
      IF ( SELLST ) THEN
         
         ISEL = ITRUE
      ELSE
         ISEL = IFALSE
      END IF

      CALL DAFADA ( DBLE( ISEL ),  1 )
 
C
C     Write the mini-segment/interpolation interval count.
C
      CALL DAFADA ( DBLE(NINTVL), 1 )

C
C     End the segment.
C
      CALL DAFENA
 
      CALL CHKOUT ( 'T_CKW06' )
      RETURN
      END
