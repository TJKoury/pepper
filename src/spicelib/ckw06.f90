!$Procedure CKW06 ( CK, Write segment, type 6 )

      subroutine ckw06 ( handle,  inst,    ref,     avflag, &
                         first,   last,    segid,   nmini, &
                         npkts,   subtps,  degres,  packts, &
                         rates,   sclkdp,  ivlbds,  sellst )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Write a type 6 segment to a CK file.
!
! $ Required_Reading
!
!     CK
!     DAF
!     NAIF_IDS
!     SCLK
!     SPC
!     TIME
!
! $ Keywords
!
!     ATTITUDE
!     FILES
!     POINTING
!
! $ Declarations

      implicit none

      include 'ck06.inc'
      include 'ckparam.inc'


      integer(ip)           handle
      integer(ip)           inst
      character(*)          ref
      logical               avflag
      real(wp)              first
      real(wp)              last
      character(*)          segid
      integer(ip)           nmini
      integer(ip)           npkts  ( * )
      integer(ip)           subtps ( * )
      integer(ip)           degres ( * )
      real(wp)              packts ( * )
      real(wp)              rates  ( * )
      real(wp)              sclkdp ( * )
      real(wp)              ivlbds ( * )
      logical               sellst

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of a CK file open for writing.
!     INST       I   NAIF instrument ID code.
!     REF        I   Reference frame name.
!     AVFLAG     I   Flag indicating if the segment will contain angular
!                    velocity.
!     FIRST      I   Start time of interval covered by segment.
!     LAST       I   End time of interval covered by segment.
!     SEGID      I   Segment identifier.
!     NMINI      I   Number of mini-segments.
!     NPKTS      I   Array of packet counts of mini-segments.
!     SUBTPS     I   Array of segment subtypes of mini-segments.
!     DEGRES     I   Array of polynomial degrees of mini-segments.
!     PACKTS     I   Array of data packets of mini-segments.
!     RATES      I   Nominal SCLK rates in seconds per tick.
!     SCLKDP     I   Array of epochs of mini-segments.
!     IVLBDS     I   Mini-segment interval bounds.
!     SELLST     I   Interval selection flag.
!     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of a CK file that has been opened
!              for writing.
!
!
!     INST     is a NAIF integer code associated with an
!              instrument or spacecraft structure whose
!              orientation is described by the segment to be
!              created. INST is treated by the SPICE frame
!              subsystem as a CK frame class ID (see the
!              Frames Required Reading for details).
!
!
!     AVFLAG   is a logical flag which indicates whether or not
!              the segment will contain angular velocity.
!
!
!     REF      is the NAIF name for a reference frame relative to
!              which the pointing (attitude) information for INST
!              is specified.
!
!     FIRST,
!     LAST     are, respectively, the bounds of the time interval
!              over which the segment defines the attitude of
!              INST. FIRST and LAST are encoded SCLK times.
!
!              FIRST must be greater than or equal to the first
!              mini-segment interval start time; LAST must be
!              less than or equal to the last mini-segment
!              interval stop time. See the description of IVLBDS
!              below.
!
!
!     SEGID    is the segment identifier. A CK segment
!              identifier may contain up to 40 characters.
!
!
!     NMINI    is the number of mini-segments comprised by
!              the input data. Each mini-segment contains data
!              that could be stored in a type 5 segment.
!              The parameters and data of a mini-segment are:
!
!                 - a packet count
!                 - a type 6 subtype
!                 - an interpolating polynomial degree
!                 - a nominal SCLK rate in seconds/tick
!                 - a sequence of type 6 data packets
!                 - a sequence of packet epochs
!
!              These inputs are described below.
!
!
!     NPKTS    is an array of packet counts. The Ith element of
!              NPKTS is the packet count of the Ith mini-segment.
!
!              NPKTS has dimension NMINI.
!
!
!     SUBTPS   is an array of type 6 subtypes. The Ith element of
!              SUBTPS is the subtype of the packets associated
!              with the Ith mini-segment.
!
!              SUBTPS has dimension NMINI.
!
!
!     DEGRES   is an array of interpolating polynomial degrees.
!              The Ith element of DEGRES is the polynomial degree
!              of the packets associated with the Ith
!              mini-segment.
!
!              For subtypes 0 and 2, interpolation degrees must be
!              equivalent to 3 mod 4, that is, they must be in
!              the set
!
!                 { 3, 7, 11, ..., MAXDEG }
!
!              For subtypes 1 and 3, interpolation degrees must
!              be odd and must be in the range 1:MAXDEG.
!
!              DEGRES has dimension NMINI.
!
!
!     PACKTS   is an array of data packets representing the
!              orientation of INST relative to the frame REF. The
!              packets for a given mini-segment are stored
!              contiguously in increasing time order. The order
!              of the sets of packets for different mini-segments
!              is the same as the order of their corresponding
!              mini-segment intervals.
!
!              Each packet contains a SPICE-style quaternion and
!              optionally, depending on the segment subtype,
!              attitude derivative data, from which a C-matrix
!              and an angular velocity vector may be derived.
!
!              See the discussion of quaternion styles in
!              $Particulars below.
!
!              The C-matrix CMAT represented by the Ith data
!              packet is a rotation matrix that transforms the
!              components of a vector expressed in the base frame
!              specified by REF to components expressed in the
!              instrument fixed frame at the time SCLKDP(I).
!
!              Thus, if a vector V has components x, y, z in the
!              base frame, then V has components x', y', z'
!              in the instrument fixed frame where:
!
!                 [ x' ]     [          ] [ x ]
!                 | y' |  =  |   CMAT   | | y |
!                 [ z' ]     [          ] [ z ]
!
!              Attitude derivative information either explicitly
!              contained in, or else derived from, PACKTS(I)
!              gives the angular velocity of the instrument fixed
!              frame at time SCLKDP(I) with respect to the
!              reference frame specified by REF.
!
!              The direction of an angular velocity vector gives
!              the right-handed axis about which the instrument
!              fixed reference frame is rotating. The magnitude
!              of the vector is the magnitude of the
!              instantaneous velocity of the rotation, in radians
!              per second.
!
!              Packet contents and the corresponding
!              interpolation methods depend on the segment
!              subtype, and are as follows:
!
!                 Subtype 0:  Hermite interpolation, 8-element
!                             packets. Quaternion and quaternion
!                             derivatives only, no angular
!                             velocity vector provided.
!                             Quaternion elements are listed
!                             first, followed by derivatives.
!                             Angular velocity is derived from
!                             the quaternions and quaternion
!                             derivatives.
!
!                 Subtype 1:  Lagrange interpolation, 4-element
!                             packets. Quaternion only. Angular
!                             velocity is derived by
!                             differentiating the interpolating
!                             polynomials.
!
!                 Subtype 2:  Hermite interpolation, 14-element
!                             packets. Quaternion and angular
!                             velocity vector, as well as
!                             derivatives of each, are provided.
!                             The quaternion comes first, then
!                             quaternion derivatives, then
!                             angular velocity and its
!                             derivatives.
!
!                 Subtype 3:  Lagrange interpolation, 7-element
!                             packets. Quaternion and angular
!                             velocity vector provided. The
!                             quaternion comes first.
!
!              Angular velocity is always specified relative to
!              the base frame.
!
!              Units of the input data are:
!
!                 Quaternions                unitless
!                 Quaternion derivatives     1/TDB second
!                 Angular velocity           radians/TDB second
!                 Angular acceleration       radians/TDB second**2
!
!              For the Hermite subtypes (0 and 2), quaternion
!              representations must be selected so that, for
!              consecutive quaternions Q(I) and Q(I+1) in a
!              mini-segment, the distance between Q and Q(I+1) is
!              less than the distance between Q and -Q(I+1). The
!              Lagrange subtypes do not have this restriction.
!
!
!     RATES    is an array of nominal rates of the spacecraft
!              clock associated with INST. The Ith element of
!              rates is the clock rate for the packets associated
!              with the Ith mini-segment. Units are seconds per
!              tick. Spacecraft clock rates are used to scale
!              angular velocity to radians/second.
!
!
!     SCLKDP   is an array containing epochs for all input
!              mini-segments. The epochs have a one-to-one
!              relationship with the packets in the input
!              packet array. The epochs are encoded SCLK times.
!
!              The epochs for a given mini-segment are stored
!              contiguously in increasing order. The order of the
!              sets of epochs for different mini-segments is the
!              same as the order of their corresponding
!              mini-segment intervals.
!
!              For each mini-segment, "padding" is allowed: the
!              sequence of epochs for that mini-segment may start
!              before the corresponding mini-segment interval
!              start time and end after the corresponding
!              mini-segment interval stop time. Padding is used
!              to control behavior of interpolating polynomials
!              near mini-segment interval boundaries.
!
!              Due to possible use of padding, the elements of
!              SCLKDP, taken as a whole, might not be in
!              increasing order.
!
!
!     IVLBDS   is an array of mini-segment interval boundary
!              times. This array is a strictly increasing list of
!              the mini-segment interval start times, to which
!              the end time for the last interval is appended.
!              The interval bounds are encoded SCLK times.
!
!              The Ith mini-segment interval is the time
!              coverage interval of the Ith mini-segment (see the
!              description of NPKTS above).
!
!              For each mini-segment, the corresponding
!              mini-segment interval's start time is greater
!              than or equal to the mini-segment's first epoch.
!              The interval's stop time may exceed the
!              mini-segment's last epoch, allowing a single
!              coverage gap to exist between a mini-segment's
!              last epoch and its interval stop time.
!
!              The "interpolation interval" of the ith
!              mini-segment is contained in the ith mini-segment
!              interval: the interpolation interval extends from
!              IVLBDS(I) to the minimum of IVLBDS(I+1) and the
!              last epoch of the mini-segment.
!
!              For each mini-segment interval other than the
!              last, the interval's coverage stop time coincides
!              with the coverage start time of the next interval.
!
!              IVLBDS has dimension NMINI+1.
!
!
!     SELLST   is a logical flag indicating to the CK type 6
!              segment reader CKR06 how to select the
!              mini-segment interval when a request time
!              coincides with a time boundary shared by two
!              mini-segment intervals. When SELLST ("select
!              last") is .TRUE., the later interval is selected;
!              otherwise the earlier interval is selected.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     MAXDEG   is the maximum allowed degree of the interpolating
!              polynomial.
!
!              See the INCLUDE file ck06.inc for the value of
!              MAXDEG.
!
! $ Exceptions
!
!     If any of the following exceptions occur, this routine will
!     return without creating a new segment.
!
!
!     1)  If FIRST is greater than LAST, the error
!         SPICE(BADDESCRTIMES) is signaled.
!
!     2)  If REF is not a recognized name, the error
!         SPICE(INVALIDREFFRAME) is signaled.
!
!     3)  If the last non-blank character of SEGID occurs past index
!         40, the error SPICE(SEGIDTOOLONG) is signaled.
!
!     4)  If SEGID contains any nonprintable characters, the error
!         SPICE(NONPRINTABLECHARS) is signaled.
!
!     5)  If NMINI is not at least 1, the error SPICE(INVALIDCOUNT)
!         is signaled.
!
!     6)  If the elements of the array IVLBDS are not in strictly
!         increasing order, the error SPICE(BOUNDSOUTOFORDER) is
!         signaled.
!
!     7)  If the first interval start time IVLBDS(1) is greater than
!         FIRST, or if the last interval end time IVLBDS(NMINI+1) is
!         less than LAST, the error SPICE(COVERAGEGAP) is signaled.
!
!     8)  If any packet count in the array NPKTS is not at least 2, the
!         error SPICE(TOOFEWPACKETS) is signaled.
!
!     9)  If any subtype code in the array SUBTPS is not recognized,
!         the error SPICE(INVALIDSUBTYPE) is signaled.
!
!     10) If any interpolation degree in the array DEGRES is not at
!         least 1 or is greater than MAXDEG, the error
!         SPICE(INVALIDDEGREE) is signaled.
!
!     11) If the window size implied by any element of the array DEGRES
!         is odd, the error SPICE(BADWINDOWSIZE) is signaled.
!
!     12) If the elements of the array SCLKDP corresponding to a given
!         mini-segment are not in strictly increasing order, the error
!         SPICE(TIMESOUTOFORDER) is signaled.
!
!     13) If the first epoch of a mini-segment exceeds the start time
!         of the associated mini-segment interval, or if the last
!         epoch of a mini-segment is less than the interval start
!         time, the error SPICE(BOUNDSDISAGREE) is signaled. However,
!         the last epoch of a mini-segment may be less than the end
!         time of the corresponding mini-segment interval.
!
!     14) If any quaternion has magnitude zero, the error
!         SPICE(ZEROQUATERNION) is signaled.
!
!     15) If an error occurs while writing the output segment, the error
!         is signaled by a routine in the call tree of this routine.
!
!     16) This routine assumes that the rotation between adjacent
!         quaternions that are stored in the same interval has a
!         rotation angle of THETA radians, where
!
!            0  <  THETA  <  pi.
!               _
!
!         The routines that evaluate the data in the segment produced
!         by this routine cannot distinguish between rotations of THETA
!         radians, where THETA is in the interval [0, pi), and
!         rotations of
!
!            THETA   +   2 * k * pi
!
!         radians, where k is any integer. These "large" rotations will
!         yield invalid results when interpolated. The segment creator
!         must ensure that the data stored in the segment will not be
!         subject to this sort of ambiguity.
!
!     17) For the Hermite subtypes (0 and 2), quaternion
!         representations must be selected so that, for consecutive
!         quaternions Q(I) and Q(I+1) in a mini-segment, the distance
!         between Q and Q(I+1) is less than the distance between Q and
!         -Q(I+1).
!
!         If a pair of quaternions violating this condition is found in
!         the input array PACKTS, the error SPICE(BADQUATSIGN) is
!         signaled.
!
!     18) If any element of the input RATES array is non-positive, the
!         error SPICE(INVALIDSCLKRATE) is signaled.
!
! $ Files
!
!     A new type 6 CK segment is written to the CK file attached
!     to HANDLE.
!
! $ Particulars
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
!     -  The order of quaternion elements
!
!     -  The quaternion multiplication formula
!
!     -  The convention for associating quaternions
!        with rotation matrices
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
!     of M and theta is M's rotation angle. Note that N and theta
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
!              CALL CKW06 ( HANDLE,  INST,    REF,     AVFLAG,
!             .             FIRST,   LAST,    SEGID,   NMINI,
!             .             NPKTS,   SUBTPS,  DEGRES,  PACKTS,
!             .             RATES,   SCLKDP,  IVLBDS,  SELLST  )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.0.1, 02-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 11-AUG-2015 (NJB)
!
!        Added check for invalid SCLK rates.
!
!        Corrected error in header $Exceptions section: changed
!        subscript N+1 to NMINI+1. Corrected typo in description
!        of subtype 2 data. Added mention of angular acceleration
!        units.
!
!-    SPICELIB Version 1.0.0, 14-MAR-2014 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     write SPK type_6 ephemeris data segment
!
!-&


!
!     SPICELIB functions
!
      real(wp)              vdistg
      integer(ip)           lastnb

      logical               even
      logical               failed
      logical               odd
      logical               return
      logical               vzerog

!
!     Local parameters
!
      integer(ip)           sidlen
      parameter           ( sidlen =  40 )

      integer(ip)           fprint
      parameter           ( fprint =  32 )

      integer(ip)           lprint
      parameter           ( lprint = 126 )

      integer(ip)           nd
      parameter           ( nd     =   2 )

      integer(ip)           ni
      parameter           ( ni     =   6 )

      integer(ip)           dscsiz
      parameter           ( dscsiz =   5 )

      integer(ip)           dtype
      parameter           ( dtype  =  6 )

      integer(ip)           dirsiz
      parameter           ( dirsiz = 100 )

!
!     Packet structure parameters
!
      integer(ip)           qidx
      parameter           ( qidx   = 1 )

!
!     Local variables
!
      real(wp)              dc     ( nd )
      real(wp)              descr  ( dscsiz )
      real(wp)              prevq  ( 0 : 3 )
      real(wp)              q      ( 0 : 3 )
      real(wp)              qneg   ( 0 : 3 )

      integer(ip)           addr
      integer(ip)           bepix
      integer(ip)           chrcod
      integer(ip)           eepix
      integer(ip)           i
      integer(ip)           ic     ( ni )
      integer(ip)           isel
      integer(ip)           j
      integer(ip)           k
      integer(ip)           minisz
      integer(ip)           ndir
      integer(ip)           pktbeg
      integer(ip)           pktdsz
      integer(ip)           pktend
      integer(ip)           pktsiz
      integer(ip)           pktszs ( 0 : c06nst-1 )
      integer(ip)           refcod
      integer(ip)           segbeg
      integer(ip)           segend
      integer(ip)           subtyp
      integer(ip)           winsiz

!
!     Saved values
!
      save                  pktszs

!
!     Initial values
!
      data                  pktszs  /  c06ps0, c06ps1, c06ps2, c06ps3 /


!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Start with a parameter compatibility check.
!
      if ( ckmrsz < maxrsz ) then

         call setmsg ( 'CK type 6 record size is #, but ' &
         //            'CKPFS record size is #.' &
         //            'is #.'                            )
         call errint ( '#',  maxrsz                       )
         call errint ( '#',  ckmrsz                       )
         call sigerr ( 'SPICE(BUG)'                       )
         return

      end if

!
!     Make sure the segment descriptor bounds are
!     correctly ordered.
!
      if ( last < first ) then

         call setmsg ( 'Segment start time is #; stop time is #; ' &
         //            'bounds must be in nondecreasing order.'    )
         call errdp  ( '#', first                                  )
         call errdp  ( '#', last                                   )
         call sigerr ( 'SPICE(BADDESCRTIMES)'                      )
         return

      end if

!
!     Get the NAIF integer code for the reference frame.
!
      call namfrm ( ref, refcod )

      if ( refcod == 0 ) then

         call setmsg ( 'The reference frame # is not supported.' )
         call errch  ( '#', ref                                  )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                  )
         return

      end if

!
!     Check to see if the segment identifier is too long.
!
      if ( lastnb(segid) > sidlen ) then

         call setmsg ( 'Segment identifier contains more than ' &
         //            '40 characters.'                        )
         call sigerr ( 'SPICE(SEGIDTOOLONG)'                   )
         return

      end if

!
!     Now check that all the characters in the segment identifier
!     can be printed.
!
      do i = 1, lastnb(segid)

         chrcod = ichar( segid(i:i) )

         if ( ( chrcod < fprint ) .or. ( chrcod > lprint ) ) then

            call setmsg ( 'The segment identifier contains ' &
            //            'nonprintable characters'         )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'        )
            return

         end if

      end do

!
!     The mini-segment count must be positive.
!
      if ( nmini < 1 ) then

         call setmsg ( 'Mini-segment count was #; ' &
         //            'this count must be positive.' )
         call errint ( '#', nmini                     )
         call sigerr ( 'SPICE(INVALIDCOUNT)'          )
         return

      end if

!
!     Make sure the interval bounds form a strictly
!     increasing sequence.
!
!     Note that there are NMINI+1 bounds.
!
      do i = 1, nmini

         if ( ivlbds(i) >= ivlbds(i+1) ) then

            call setmsg ( 'Mini-segment interval ' &
            //            'bounds at indices # and # ' &
            //            'are # and # respectively. The ' &
            //            'difference is #. The bounds are ' &
            //            'required to be strictly increasing.' )
            call errint ( '#', i                                )
            call errint ( '#', i+1                              )
            call errdp  ( '#', ivlbds(i)                        )
            call errdp  ( '#', ivlbds(i+1)                      )
            call errdp  ( '#', ivlbds(i+1) - ivlbds(i)          )
            call sigerr ( 'SPICE(BOUNDSOUTOFORDER)'             )
            return

         end if

      end do

!
!     Make sure the time span of the descriptor doesn't extend
!     beyond the span of the interval bounds.
!
      if (      ( first < ivlbds(1        ) ) &
           .or. ( last  > ivlbds(nmini+1) ) ) then

         call setmsg ( 'First mini-segment interval ' &
         //            'start time is #; segment start time ' &
         //            'is #; segment stop time is #; ' &
         //            'last mini-segment interval stop ' &
         //            'time is #. This sequence of times ' &
         //            'is required to be non-decreasing: ' &
         //            'segment coverage must be contained ' &
         //            'within the union of the mini-segment ' &
         //            'intervals.'                            )
         call errdp  ( '#', ivlbds(1)                          )
         call errdp  ( '#', first                              )
         call errdp  ( '#', last                               )
         call errdp  ( '#', ivlbds(nmini+1)                    )
         call sigerr ( 'SPICE(COVERAGEGAP)'                    )
         return

      end if

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
      pktbeg = 0
      pktend = 0

      bepix  = 0
      eepix  = 0

      do i = 1, nmini
!
!        First, just make sure the packet count for the current
!        mini-segment is at least two. This check reduces our chances
!        of a subscript range violation.
!
!        Check the number of packets.
!
         if ( npkts(i) < 2 ) then

            call setmsg ( 'At least 2 packets are required ' &
            //            'for CK type 6. Number of packets ' &
            //            'supplied was # in mini-segment at index #.' )
            call errint ( '#', npkts(i)                                )
            call errint ( '#', i                                       )
            call sigerr ( 'SPICE(TOOFEWPACKETS)'                       )
            return

         end if

!
!        Set the packet size, which is a function of the subtype. Also
!        set the window size. First check the subtype, which will be
!        used as an array index.
!
         subtyp = subtps(i)

         if (  ( subtyp < 0 ) .or. ( subtyp > c06nst-1 ) ) then

            call setmsg ( 'Unexpected CK type 6 subtype # ' &
            //            'found in mini-segment #.'       )
            call errint ( '#',  subtyp                     )
            call errint ( '#',  i                          )
            call sigerr ( 'SPICE(INVALIDSUBTYPE)'          )
            return

         end if

         pktsiz = pktszs ( subtyp )

         if ( odd(subtyp) ) then

            winsiz =   degres(i) + 1
         else
            winsiz = ( degres(i) + 1 ) / 2
         end if

!
!        Make sure the SCLK rates in this mini-segment are positive.
!
         if ( rates(i) <= 0.0_wp ) then

            call setmsg ( 'SCLK rate at index # was #; rate must ' &
            //            'be positive.'                           )
            call errint ( '#',  i                                  )
            call errdp  ( '#',  rates(i)                           )
            call sigerr ( 'SPICE(INVALIDSCLKRATE)'                 )
            return

         end if

!
!        Update the packet range pointers for this mini-segment.
!
         pktbeg = pktend + 1
         pktend = pktbeg - 1  +  npkts(i)*pktsiz

!
!        Make sure that the degree of the interpolating polynomials is
!        in range.
!
         if (      ( degres(i) < 1      ) &
              .or. ( degres(i) > maxdeg )  )  then

            call setmsg ( 'The interpolating polynomials of ' &
            //            'mini-segment # have degree #; the ' &
            //            'valid degree range is [1, #]'      )
            call errint ( '#', i                              )
            call errint ( '#', degres(i)                      )
            call errint ( '#', maxdeg                         )
            call sigerr ( 'SPICE(INVALIDDEGREE)'              )
            return

         end if

!
!        Make sure that the window size is even.
!
         if (  odd( winsiz )  )  then

            call setmsg ( 'The interpolating polynomials of ' &
            //            'mini-segment # have window size # ' &
            //            'and degree # for CK type 6. ' &
            //            'The mini-segment subtype is #. ' &
            //            'The degree must be equivalent to 3 ' &
            //            'mod 4 for subtypes 0 or 2 (Hermite ' &
            //            'interpolation) and odd for subtypes ' &
            //            '1 or 3 (Lagrange interpolation).'     )
            call errint ( '#', i                                 )
            call errint ( '#', winsiz                            )
            call errint ( '#', degres(i)                         )
            call errint ( '#', subtps(i)                         )
            call sigerr ( 'SPICE(BADWINDOWSIZE)'                 )
            return

         end if

!
!        Make sure the epochs of the Ith mini-segment form a
!        strictly increasing sequence.
!
!        To start out, determine the indices of the epoch sequence
!        of the Ith mini-segment. We'll call the begin and end
!        epoch indices BEPIX and EEPIX respectively.
!
         bepix  =  eepix + 1
         eepix  =  bepix - 1 +  npkts(i)


         do j = 1, npkts(i) - 1

            k = bepix + j - 1

            if ( sclkdp(k) >= sclkdp(k+1) ) then

               call setmsg ( 'In mini-segment #, epoch # having ' &
               //            'mini-segment-relative index # and ' &
               //            'array-relative index # is greater ' &
               //            'than or equal to its successor #.' )
               call errint ( '#',  i                             )
               call errdp  ( '#',  sclkdp(k  )                   )
               call errint ( '#',  j                             )
               call errint ( '#',  k                             )
               call errdp  ( '#',  sclkdp(k+1)                   )
               call sigerr ( 'SPICE(TIMESOUTOFORDER)'            )
               return

            end if

         end do

!
!        Make sure that the span of the input epochs of the Ith
!        mini-segment includes the start of the Ith mini-segment
!        interval. Note that the stop time need not be covered, since
!        gaps are allowed at the right ends of mini-segment intervals.
!
         if (  sclkdp(bepix) > ivlbds(i) ) then

            call setmsg ( 'Mini-segment interval # start time # ' &
            //            'precedes mini-segment''s first epoch #.' )
            call errint ( '#',  i                                   )
            call errdp  ( '#',  ivlbds(i)                           )
            call errdp  ( '#',  sclkdp(bepix)                       )
            call sigerr ( 'SPICE(BOUNDSDISAGREE)'                   )
            return

         else if (  sclkdp(eepix) < ivlbds(i) ) then

            call setmsg ( 'Mini-segment interval # start time # ' &
            //            'follows mini-segment''s last epoch #.'   )
            call errint ( '#',  i                                   )
            call errdp  ( '#',  ivlbds(i)                           )
            call errdp  ( '#',  sclkdp(eepix)                       )
            call sigerr ( 'SPICE(BOUNDSDISAGREE)'                   )
            return

         end if

!
!        Make sure that the quaternions are non-zero. This is just a
!        check for uninitialized data.
!
!        For the Hermite subtypes, make sure quaternions are suitable
!        for interpolation.
!
         do j = 1, npkts(i)
!
!           We have to address the quaternion explicitly, since the
!           shape of the packet array is not known at compile time.
!
            addr = pktbeg  +  ( pktsiz*(j-1) )  -  1  +  qidx

            if (  vzerog( packts(addr), 4 )  ) then

               call setmsg ( 'The quaternion in packet # within ' &
               //            'mini-segment # has magnitude zero.' )
               call errint ( '#', j                               )
               call errint ( '#', i                               )
               call sigerr ( 'SPICE(ZEROQUATERNION)'              )
               return

            end if

!
!           For the Hermite subtypes, each quaternion must be closer
!           than its negative to its predecessor in the quaternion
!           sequence.
!

            if (  ( j > 1 )  .and.  even(subtyp)  ) then
!
!              Compare the distance between the current quaternion
!              and its predecessor vs the distance between the
!              negative of the current quaternion and its predecessor.
!
               call moved  ( packts(addr),        4, q     )
               call moved  ( packts(addr-pktsiz), 4, prevq )
               call vminug ( q, 4, qneg )

               if (      vdistg(prevq, qneg, 4 ) &
                    < vdistg(prevq, q,    4 )  ) then

                  call setmsg ( 'The quaternion in packet # within ' &
                  //            'mini-segment # is farther than its ' &
                  //            'negative from its predecessor at ' &
                  //            'index #. This makes the quaternion ' &
                  //            'sequence unsuitable for Hermite ' &
                  //            'interpolation. The quaternions, and ' &
                  //            'if applicable, their derivatives, ' &
                  //            'must be adjusted before they are ' &
                  //            'passed to this routine.'              )
                  call errint ( '#', j                                 )
                  call errint ( '#', i                                 )
                  call errint ( '#', j-1                               )
                  call sigerr ( 'SPICE(BADQUATSIGN)'                   )
                  return

               end if

            end if

         end do

      end do

!
!     If we made it this far, we're ready to start writing the segment.
!
!     The type 6 segment structure is eloquently described by this
!     diagram from the CK Required Reading:
!
!        +---------------------------------------+
!        | Mini-segment 1                        |
!        +---------------------------------------+
!              .
!              .
!              .
!        +---------------------------------------+
!        | Mini-segment N                        |
!        +---------------------------------------+
!        | Mini-segment interval 1 start time    |
!        +---------------------------------------+
!              .
!              .
!              .
!        +---------------------------------------+
!        | Mini-segment interval N start time    |
!        +---------------------------------------+
!        | Mini-segment interval N stop time     |
!        +---------------------------------------+
!        | Mini-seg. interval start time 100     | (First interval
!        +---------------------------------------+  directory)
!              .
!              .
!              .
!        +---------------------------------------+
!        | Mini-seg. ival. start time (N/100)*100| (Last interval
!        +---------------------------------------+  directory)
!        | Mini-segment 1 start pointer          |
!        +---------------------------------------+
!              .
!              .
!              .
!        +---------------------------------------+
!        | Mini-segment N start pointer          |
!        +---------------------------------------+
!        | Mini-segment N stop pointer + 1       |
!        +---------------------------------------+
!        | Boundary choice flag                  |
!        +---------------------------------------+
!        | Number of intervals                   |
!        +---------------------------------------+
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
!     not contain an mini-segment interval count. This is because,
!     unlike a CK type 5 segment, a CK type 6 segment can contain at
!     most one gap. If present, the gap is located at the end of
!     mini-segment's mini-segment interval.
!
!     Create the segment descriptor. We don't use CKPDS because
!     that routine doesn't allow creation of a singleton segment.
!
      ic(1) = inst
      ic(2) = refcod
      ic(3) = dtype

      if ( avflag ) then
         ic(4) = 1
      else
         ic(4) = 0
      end if

      dc(1) = first
      dc(2) = last

      call dafps  ( nd, ni, dc, ic, descr )

!
!     Begin a new segment.
!
      call dafbna ( handle, descr, segid )

      if ( failed() ) return

!
!     Re-initialize the mini-segment packet array indices,
!     and those of the mini-segment epoch array as well.
!
      pktbeg = 0
      pktend = 0

      bepix  = 0
      eepix  = 0

!
!     Write data for each mini-segment to the file.
!
      do i = 1, nmini
!
!        Set the packet size, which is a function of the subtype.
!
         subtyp = subtps ( i )

         pktsiz = pktszs ( subtyp )

         if ( odd(subtyp) ) then

            winsiz =   degres(i) + 1
         else
            winsiz = ( degres(i) + 1 ) / 2
         end if

!
!        Now that we have the packet size, we can compute
!        mini-segment packet index range. We'll let PKTDSZ
!        be the total count of packet data entries for this
!        mini-segment.
!
         pktdsz = npkts(i) * pktsiz

         pktbeg = pktend  +  1
         pktend = pktbeg  -  1  +  pktdsz

!
!        At this point, we're read to start writing the
!        current mini-segment to the file. Start with the
!        packet data.
!
         call dafada ( packts(pktbeg), pktdsz )

!
!        Write the epochs for this mini-segment.
!
         bepix = eepix + 1
         eepix = bepix - 1 + npkts(i)

         call dafada ( sclkdp(bepix), npkts(i) )

!
!        Compute the number of epoch directories for the
!        current mini-segment.
!
         ndir = ( npkts(i) - 1 ) / dirsiz

!
!        Write the epoch directories to the segment.
!
         do j = 1, ndir

            k = bepix  -  1  +  ( j * dirsiz )

            call dafada ( sclkdp(k), 1 )

         end do

!
!        Write the mini-segment's SCLK rate, subtype, window size, and
!        packet count to the segment.
!
         call dafada ( rates(i),           1 )
         call dafada ( real( subtps(i), wp ),  1 )
         call dafada ( real( winsiz, wp    ),  1 )
         call dafada ( real( npkts(i), wp  ),  1 )

         if ( failed() ) return

      end do

!
!     We've finished writing the mini-segments.
!
!     Next write the mini-segment interval bounds.
!
      call dafada ( ivlbds, nmini+1 )

!
!     Create and write directories for the interval
!     bounds.
!
!     The directory count is the interval bound count
!     (N+1), minus 1, divided by the directory size.
!
      ndir = nmini / dirsiz

      do i = 1, ndir
         call dafada (  ivlbds( dirsiz*i ),  1  )
      end do

!
!     Now we compute and write the start/stop pointers
!     for each mini-segment.
!
!     The pointers are relative to the DAF address
!     preceding the segment. For example, a pointer
!     to the first DAF address in the segment has
!     value 1.
!
      segend = 0

      do i = 1, nmini
!
!        Set the packet size, which is a function of the subtype. Also
!        set the window size. First check the subtype, which will be
!        used as an array index.
!
         pktsiz = pktszs ( subtps(i) )

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
         minisz =       npkts(i) * ( pktsiz + 1 ) &
                    + ( npkts(i) - 1 ) / dirsiz &
                    +   4


         segbeg = segend + 1
         segend = segbeg + minisz - 1

!
!        Write the mini-segment begin pointer.
!
!        After the loop terminates, the final end pointer, incremented
!        by 1, will be written.
!
         call dafada ( real(segbeg, wp), 1 )

      end do

!
!     Write the last mini-segment end pointer, incremented by one.
!     SEGEND was computed on the last iteration of the above loop.
!
      call dafada ( real(segend+1, wp), 1 )

!
!     Write out the interval selection flag. The input
!     boolean value is represented by a numeric constant.
!
      if ( sellst ) then

         isel = itrue
      else
         isel = ifalse
      end if

      call dafada ( real( isel, wp ),  1 )

!
!     Write the mini-segment/mini-segment interval count.
!
      call dafada ( real(nmini, wp), 1 )

!
!     End the segment.
!
      call dafena

      return
      end
