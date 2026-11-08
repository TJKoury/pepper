!$Procedure CKW03 ( C-Kernel, write segment to C-kernel, data type 3 )

      subroutine ckw03 ( handle, begtim, endtim, inst,  ref,  avflag, &
                         segid,  nrec,   sclkdp, quats, avvs, nints, &
                         starts                                      )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Add a type 3 segment to a C-kernel.
!
! $ Required_Reading
!
!     CK
!     DAF
!     ROTATION
!     SCLK
!
! $ Keywords
!
!     POINTING
!     UTILITY
!
! $ Declarations

      implicit none

      integer(ip)           handle
      real(wp)              begtim
      real(wp)              endtim
      integer(ip)           inst
      character(*)          ref
      logical               avflag
      character(*)          segid
      integer(ip)           nrec
      real(wp)              sclkdp (      * )
      real(wp)              quats  ( 0:3, * )
      real(wp)              avvs   (   3, * )
      integer(ip)           nints
      real(wp)              starts (      * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an open CK file.
!     BEGTIM     I   Beginning encoded SCLK of the segment.
!     ENDTIM     I   Ending encoded SCLK of the segment.
!     INST       I   NAIF instrument ID code.
!     REF        I   Reference frame of the segment.
!     AVFLAG     I   .TRUE. if the segment will contain angular
!                    velocity.
!     SEGID      I   Segment identifier.
!     NREC       I   Number of pointing records.
!     SCLKDP     I   Encoded SCLK times.
!     QUATS      I   SPICE quaternions representing instrument pointing.
!     AVVS       I   Angular velocity vectors.
!     NINTS      I   Number of intervals.
!     STARTS     I   Encoded SCLK interval start times.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of the CK file to which the segment will
!              be written. The file must have been opened with write
!              access.
!
!     BEGTIM,
!     ENDTIM   are the beginning and ending encoded SCLK times for
!              which the segment provides pointing information.
!              BEGTIM must be less than or equal to the SCLK time
!              associated with the first pointing instance in the
!              segment, and ENDTIM must be greater than or equal to
!              the time associated with the last pointing instance
!              in the segment.
!
!     INST     is the NAIF integer ID code for the instrument that
!              this segment will contain pointing information for.
!
!     REF      is a character string which specifies the inertial
!              reference frame of the segment.
!
!              The rotation matrices represented by the quaternions
!              that are to be written to the segment transform the
!              components of vectors from the inertial reference frame
!              specified by REF to components in the instrument fixed
!              frame. Also, the components of the angular velocity
!              vectors to be written to the segment should be given
!              with respect to REF.
!
!              REF should be the name of one of the frames supported
!              by the SPICELIB routine FRAMEX.
!
!     AVFLAG   is a logical flag which indicates whether or not the
!              segment will contain angular velocity.
!
!     SEGID    is the segment identifier. A CK segment identifier may
!              contain up to 40 printable characters and spaces.
!
!     NREC     is the number of pointing instances in the segment.
!
!     SCLKDP   are the encoded spacecraft clock times associated with
!              each pointing instance. These times must be strictly
!              increasing.
!
!     QUATS    is an array of SPICE-style quaternions representing
!              a sequence of C-matrices. See the discussion of
!              quaternion styles in $Particulars below.
!
!              The C-matrix represented by the Ith quaternion in
!              QUATS is a rotation matrix that transforms the
!              components of a vector expressed in the inertial
!              frame specified by REF to components expressed in
!              the instrument fixed frame at the time SCLKDP(I).
!
!              Thus, if a vector V has components x, y, z in the
!              inertial frame, then V has components x', y', z' in
!              the instrument fixed frame where:
!
!                   [ x' ]     [          ] [ x ]
!                   | y' |  =  |   CMAT   | | y |
!                   [ z' ]     [          ] [ z ]
!
!     AVVS     are the angular velocity vectors ( optional ).
!
!              The Ith vector in AVVS gives the angular velocity of
!              the instrument fixed frame at time SCLKDP(I). The
!              components of the angular velocity vectors should
!              be given with respect to the inertial reference frame
!              specified by REF.
!
!              The direction of an angular velocity vector gives
!              the right-handed axis about which the instrument fixed
!              reference frame is rotating. The magnitude of the
!              vector is the magnitude of the instantaneous velocity
!              of the rotation, in radians per second.
!
!              If AVFLAG is .FALSE. then this array is ignored by the
!              routine; however it still must be supplied as part of
!              the calling sequence.
!
!     NINTS    is the number of intervals that the pointing instances
!              are partitioned into.
!
!     STARTS   are the start times of each of the interpolation
!              intervals. These times must be strictly increasing
!              and must coincide with times for which the segment
!              contains pointing.
!
! $ Detailed_Output
!
!     None. See $Files section.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If HANDLE is not the handle of a C-kernel opened for writing,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If SEGID is more than 40 characters long, the error
!         SPICE(SEGIDTOOLONG) is signaled.
!
!     3)  If SEGID contains any non-printable characters, the error
!         SPICE(NONPRINTABLECHARS) is signaled.
!
!     4)  If the first encoded SCLK time is negative, the error
!         SPICE(INVALIDSCLKTIME) is signaled.
!
!     5)  If the second encoded SCLK or any subsequent times, or if the
!         encoded SCLK times are not strictly increasing, the error
!         SPICE(TIMESOUTOFORDER) is signaled.
!
!     6)  If BEGTIM is greater than SCLKDP(1) or ENDTIM is less than
!         SCLKDP(NREC), the error SPICE(INVALIDDESCRTIME) is signaled.
!
!     7)  If the name of the reference frame is not one of those
!         supported by the SPICELIB routine NAMFRM, the error
!         SPICE(INVALIDREFFRAME) is signaled.
!
!     8)  If NREC, the number of pointing records, is less than or equal
!         to 0, the error SPICE(INVALIDNUMREC) is signaled.
!
!     9)  If NINTS, the number of interpolation intervals, is less than
!         or equal to 0, the error SPICE(INVALIDNUMINT) is signaled.
!
!     10) If the encoded SCLK interval start times are not strictly
!         increasing, the error SPICE(TIMESOUTOFORDER) is signaled.
!
!     11) If an interval start time does not coincide with a time for
!         which there is an actual pointing instance in the segment, the
!         error SPICE(INVALIDSTARTTIME) is signaled.
!
!     12) This routine assumes that the rotation between adjacent
!         quaternions that are stored in the same interval has a
!         rotation angle of THETA radians, where
!
!            0  <=  THETA  <  pi.
!
!         The routines that evaluate the data in the segment produced
!         by this routine cannot distinguish between rotations of THETA
!         radians, where THETA is in the interval [0, pi), and
!         rotations of
!
!            THETA   +   2 * k * pi
!
!         radians, where k is any integer. These `large' rotations
!         will yield invalid results when interpolated. You must
!         ensure that the data stored in the segment will not be
!         subject to this sort of ambiguity.
!
!     13) If any quaternion has magnitude zero, the error
!         SPICE(ZEROQUATERNION) is signaled.
!
!     14) If the start time of the first interval and the time of the
!         first pointing instance are not the same, the error
!         SPICE(TIMESDONTMATCH) is signaled.
!
! $ Files
!
!     This routine adds a type 3 segment to a C-kernel. The C-kernel
!     may be either a new one or an existing one opened for writing.
!
! $ Particulars
!
!     For a detailed description of a type 3 CK segment please see the
!     CK Required Reading.
!
!     This routine relieves the user from performing the repetitive
!     calls to the DAF routines necessary to construct a CK segment.
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
!          .-                                                         -.
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
!          `-                                                         -'
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
!                   .-             -.
!                   |  0   -n3   n2 |
!                   |               |
!         OMEGA  =  |  n3   0   -n1 |
!                   |               |
!                   | -n2   n1   0  |
!                   `-             -'
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
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Create a CK type 3 segment; fill with data for a simple time
!        dependent rotation and angular velocity.
!
!        Example code begins here.
!
!
!              PROGRAM CKW03_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         CK3
!              PARAMETER           ( CK3 = 'ckw03_ex1.bc' )
!
!              DOUBLE PRECISION      SPTICK
!              PARAMETER           ( SPTICK = 0.001D0 )
!
!              INTEGER               INST
!              PARAMETER           ( INST = -77703 )
!
!              INTEGER               MAXREC
!              PARAMETER           ( MAXREC = 201 )
!
!              INTEGER               NAMLEN
!              PARAMETER           ( NAMLEN = 42 )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(NAMLEN)    REF
!              CHARACTER*(NAMLEN)    IFNAME
!              CHARACTER*(NAMLEN)    SEGID
!
!              DOUBLE PRECISION      AVVS   (   3,MAXREC )
!              DOUBLE PRECISION      BEGTIM
!              DOUBLE PRECISION      ENDTIM
!              DOUBLE PRECISION      QUATS  ( 0:3,MAXREC )
!              DOUBLE PRECISION      RATE
!              DOUBLE PRECISION      RWMAT  ( 3, 3 )
!              DOUBLE PRECISION      SPACES
!              DOUBLE PRECISION      SCLKDP (     MAXREC )
!              DOUBLE PRECISION      STARTS (    MAXREC/2)
!              DOUBLE PRECISION      STICKS
!              DOUBLE PRECISION      THETA
!              DOUBLE PRECISION      WMAT   ( 3, 3 )
!              DOUBLE PRECISION      WQUAT  ( 0:3 )
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               NCOMCH
!              INTEGER               NINTS
!
!              LOGICAL               AVFLAG
!
!        C
!        C     NCOMCH is the number of characters to reserve for the
!        C     kernel's comment area. This example doesn't write
!        C     comments, but it reserves room for 5000 characters.
!        C
!              NCOMCH = 5000
!
!        C
!        C     The base reference from for the rotation data.
!        C
!              REF = 'J2000'
!
!        C
!        C     Time spacing in encoded ticks and in seconds
!        C
!              STICKS = 10.D0
!              SPACES = STICKS * SPTICK
!
!        C
!        C     Declare an angular rate in radians per sec.
!        C
!              RATE = 1.D-2
!
!        C
!        C     Internal file name and segment ID.
!        C
!              SEGID  = 'Test type 3 CK segment'
!              IFNAME = 'Test CK type 3 segment created by CKW03'
!
!
!        C
!        C     Open a new kernel.
!        C
!              CALL CKOPN ( CK3, IFNAME, NCOMCH, HANDLE )
!
!        C
!        C     Create a 3x3 double precision identity matrix.
!        C
!              CALL IDENT ( WMAT )
!
!        C
!        C     Convert the matrix to quaternion.
!        C
!              CALL M2Q ( WMAT, WQUAT )
!
!        C
!        C     Copy the work quaternion to the first row of
!        C     QUATS.
!        C
!              CALL MOVED ( WQUAT, 4, QUATS(0,1) )
!
!        C
!        C     Create an angular velocity vector. This vector is in the
!        C     REF reference frame and indicates a constant rotation
!        C     about the Z axis.
!        C
!              CALL VPACK ( 0.D0, 0.D0, RATE, AVVS(1,1) )
!
!        C
!        C     Set the initial value of the encoded ticks.
!        C
!              SCLKDP(1) = 1000.D0
!
!        C
!        C     Fill the rest of the AVVS and QUATS matrices
!        C     with simple data.
!        C
!              DO I = 2, MAXREC
!
!        C
!        C        Create the corresponding encoded tick value in
!        C        increments of STICKS with an initial value of
!        C        1000.0 ticks.
!        C
!                 SCLKDP(I) = 1000.D0 + (I-1) * STICKS
!
!        C
!        C        Create the transformation matrix for a rotation of
!        C        THETA about the Z axis. Calculate THETA from the
!        C        constant angular rate RATE at increments of SPACES.
!        C
!                 THETA = (I-1) * RATE * SPACES
!                 CALL ROTMAT ( WMAT, THETA, 3, RWMAT )
!
!        C
!        C        Convert the RWMAT matrix to SPICE type quaternion.
!        C
!                 CALL M2Q ( RWMAT, WQUAT )
!
!        C
!        C        Store the quaternion in the QUATS matrix.
!        C        Store angular velocity in AVVS.
!        C
!                 CALL MOVED ( WQUAT, 4, QUATS(0,I) )
!                 CALL VPACK ( 0.D0, 0.D0, RATE, AVVS(1,I) )
!
!              END DO
!
!        C
!        C     Create an array start times for the interpolation
!        C     intervals. The end time for a particular interval is
!        C     determined as the time of the final data value prior in
!        C      time to the next start time.
!        C
!              NINTS = MAXREC/2
!              DO I = 1, NINTS
!
!                 STARTS(I) = SCLKDP(I*2 - 1)
!
!              END DO
!
!        C
!        C     Set the segment boundaries equal to the first and last
!        C     time for the data arrays.
!        C
!              BEGTIM = SCLKDP(1)
!              ENDTIM = SCLKDP(MAXREC)
!
!        C
!        C     This segment contains angular velocity.
!        C
!              AVFLAG = .TRUE.
!
!        C
!        C     All information ready to write. Write to a CK type 3
!        C     segment to the file indicated by HANDLE.
!        C
!              CALL CKW03 ( HANDLE, BEGTIM, ENDTIM, INST,   REF,
!             .             AVFLAG, SEGID,  MAXREC, SCLKDP, QUATS,
!             .             AVVS,   NINTS,  STARTS                )
!
!        C
!        C     SAFELY close the file.
!        C
!              CALL CKCLS ( HANDLE )
!
!              END
!
!
!        When this program is executed, no output is presented on
!        screen. After run completion, a new CK file exists in the
!        output directory.
!
! $ Restrictions
!
!     1)  The creator of the segment is given the responsibility for
!         determining whether it is reasonable to interpolate between
!         two given pointing values.
!
!     2)  This routine assumes that the rotation between adjacent
!         quaternions that are stored in the same interval has a
!         rotation angle of THETA radians, where
!
!             0  <=  THETA  <  pi.
!
!         The routines that evaluate the data in the segment produced
!         by this routine cannot distinguish between rotations of THETA
!         radians, where THETA is in the interval [0, pi), and
!         rotations of
!
!             THETA   +   2 * k * pi
!
!         radians, where k is any integer. These "large" rotations will
!         yield invalid results when interpolated. You must ensure that
!         the data stored in the segment will not be subject to this
!         sort of ambiguity.
!
!     3)  All pointing instances in the segment must belong to one and
!         only one of the intervals.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     J.M. Lynch         (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.1, 08-JUL-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Changed, in $Exceptions
!        entry #7, reference to FRAMEX by NAMFRM.
!
!        Created complete code example from existing fragment.
!
!-    SPICELIB Version 3.0.0, 01-JUN-2010 (NJB)
!
!        The check for non-unit quaternions has been replaced
!        with a check for zero-length quaternions.
!
!-    SPICELIB Version 2.3.0, 26-FEB-2008 (NJB)
!
!        Updated header; added information about SPICE
!        quaternion conventions.
!
!        Minor typo in a long error message was corrected.
!
!-    SPICELIB Version 2.2.0, 26-SEP-2005 (BVS)
!
!        Added check to ensure that the start time of the first
!        interval is the same as the time of the first pointing
!        instance.
!
!-    SPICELIB Version 2.1.0, 22-FEB-1999 (WLT)
!
!        Added check to make sure that all quaternions are unit
!        length to single precision.
!
!-    SPICELIB Version 2.0.0, 28-DEC-1993 (WLT)
!
!        The routine was upgraded to support non-inertial reference
!        frames.
!
!-    SPICELIB Version 1.1.1, 05-SEP-1993 (KRG)
!
!        Removed all references to a specific method of opening the CK
!        file in the $Brief_I/O, $Detailed_Input, $Exceptions,
!        $Files, and $Examples sections of the header. It is assumed
!        that a person using this routine has some knowledge of the DAF
!        system and the methods for obtaining file handles.
!
!-    SPICELIB Version 1.0.0, 25-NOV-1992 (JML)
!
!-&


! $ Index_Entries
!
!     write CK type_3 pointing data segment
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           lastnb

      logical               return
      logical               failed
      logical               vzerog

!
!     Local parameters
!
!     SIDLEN   is the maximum number of characters allowed in a CK
!              segment identifier.
!
!     NDC      is the size of a packed CK segment descriptor.
!
!     ND       is the number of double precision components in a CK
!              segment descriptor.
!
!     NI       is the number of integer components in a CK segment
!              descriptor.
!
!     DTYPE    is the data type of the segment that this routine
!              operates on.
!
!     FPRINT   is the integer value of the first printable ASCII
!              character.
!
!     LPRINT   is the integer value of the last printable ASCII
!              character.
!
!
      integer(ip)           sidlen
      parameter           ( sidlen  =  40 )

      integer(ip)           ndc
      parameter           ( ndc     =   5 )

      integer(ip)           nd
      parameter           ( nd      =   2 )

      integer(ip)           ni
      parameter           ( ni      =   6 )

      integer(ip)           dtype
      parameter           ( dtype   =   3 )

      integer(ip)           fprint
      parameter           ( fprint  =  32 )

      integer(ip)           lprint
      parameter           ( lprint  = 126 )


!
!     Local variables
!
      real(wp)              dcd    ( nd    )
      real(wp)              descr  ( ndc   )


      integer(ip)           i
      integer(ip)           icd    ( ni    )
      integer(ip)           index
      integer(ip)           nrdir
      integer(ip)           nidir
      integer(ip)           refcod
      integer(ip)           value

      logical               match


!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     The first thing that we will do is create the segment descriptor.
!
!     The structure of the segment descriptor is as follows.
!
!           DCD( 1 ) and DCD( 2 ) -- SCLK limits of the segment.
!           ICD( 1 )              -- Instrument code.
!           ICD( 2 )              -- Reference frame ID.
!           ICD( 3 )              -- Data type of the segment.
!           ICD( 4 )              -- Angular rates flag.
!           ICD( 5 )              -- Beginning address of segment.
!           ICD( 6 )              -- Ending address of segment.
!


!
!     Make sure that there is a positive number of pointing records.
!
      if ( nrec <= 0 ) then

         call setmsg ( '# is an invalid number of pointing ' // &
                       'instances for type 3.'                     )
         call errint ( '#', nrec                                   )
         call sigerr ( 'SPICE(INVALIDNUMREC)'                      )
         return

      end if

!
!     Make sure that there is a positive number of interpolation
!     intervals.
!
      if ( nints <= 0 ) then

         call setmsg ( '# is an invalid number of interpolation ' // &
                       'intervals for type 3.'                     )
         call errint ( '#', nints                                  )
         call sigerr ( 'SPICE(INVALIDNUMINT)'                      )
         return

      end if

!
!     Check that the SCLK bounds on the segment are reasonable.
!
      if ( begtim > sclkdp(1) ) then

         call setmsg ( 'The segment begin time is greater than the ' // &
                       'time associated with the first pointing '    // &
                       'instance in the segment. DCD(1) = # and '    // &
                       'SCLKDP(1) = # '                               )

         call errdp  ( '#', begtim                                    )
         call errdp  ( '#', sclkdp(1)                                 )
         call sigerr ( 'SPICE(INVALIDDESCRTIME)'                      )
         return

      end if

      if ( endtim < sclkdp(nrec) ) then

         call setmsg ( 'The segment end time is less than the time '  // &
                       'associated with the last pointing instance '  // &
                       'in the segment. DCD(2) = # and SCLKDP(#) = #'  )

         call errdp  ( '#', endtim               )
         call errint ( '#', nrec                 )
         call errdp  ( '#', sclkdp(nrec)         )
         call sigerr ( 'SPICE(INVALIDDESCRTIME)' )
         return

      end if

      dcd( 1 ) = begtim
      dcd( 2 ) = endtim


!
!     Get the NAIF integer code for the reference frame.
!
      call namfrm ( ref, refcod )

      if ( refcod == 0 ) then

         call setmsg ( 'The reference frame # is not supported.'   )
         call errch  ( '#', ref                                    )
         call sigerr ( 'SPICE(INVALIDREFFRAME)'                    )
         return

      end if

!
!     Assign values to the integer components of the segment descriptor.
!
      icd ( 1 ) = inst
      icd ( 2 ) = refcod
      icd ( 3 ) = dtype

      if ( avflag ) then
         icd ( 4 ) = 1
      else
         icd ( 4 ) = 0
      end if

!
!     Now pack the segment descriptor.
!
      call dafps ( nd, ni, dcd, icd, descr )

!
!     Check that all the characters in the segid can be printed.
!
      do i = 1, lastnb(segid)

         value = ichar( segid(i:i) )

         if ( ( value < fprint ) .or. ( value > lprint ) ) then

            call setmsg ( 'The segment identifier contains '  // &
                          'nonprintable characters'               )
            call sigerr ( 'SPICE(NONPRINTABLECHARS)'              )
            return

         end if

      end do

!
!     Also check to see if the segment identifier is too long.
!
      if ( lastnb(segid) > sidlen ) then

         call setmsg ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         call sigerr ( 'SPICE(SEGIDTOOLONG)'                     )
         return

      end if


!
!     Now check that the encoded SCLK times are positive and strictly
!     increasing.
!
!     Check that the first time is nonnegative.
!
      if ( sclkdp(1) < 0.0_wp ) then

         call setmsg ( 'The first SCLKDP time: # is negative.' )
         call errdp  ( '#', sclkdp(1)                          )
         call sigerr ( 'SPICE(INVALIDSCLKTIME)'                )
         return

      end if

!
!     Now check that the times are ordered properly.
!
      do i = 2, nrec

         if ( sclkdp(i) <= sclkdp(i-1) )  then

            call setmsg ( 'The SCLKDP times are not strictly '      // &
                          'increasing. SCLKDP(#) = # and SCLKDP(#) '// &
                          '= #.'                                )
            call errint ( '#', i                                )
            call errdp  ( '#', sclkdp(i)                        )
            call errint ( '#', i-1                              )
            call errdp  ( '#', sclkdp(i-1)                      )
            call sigerr ( 'SPICE(TIMESOUTOFORDER)'              )
            return

         end if

      end do

!
!     Now check that the start time of the first interval is the
!     same as the time of the first pointing instance.
!
      if ( sclkdp(1) /= starts(1) ) then

         call setmsg ( 'The start time of the first interval # ' // &
                       'and the time of the first pointing '     // &
                       'instance # are not the same.'          )
         call errdp  ( '#', starts(1)                          )
         call errdp  ( '#', sclkdp(1)                          )
         call sigerr ( 'SPICE(TIMESDONTMATCH)'                 )
         return

      end if


!
!     Now check that the interval start times are ordered properly.
!
      do i = 2, nints

         if ( starts(i) <= starts(i-1) )  then

            call setmsg ( 'The interval start times are not strictly '// &
                          'increasing. STARTS(#) = # and STARTS(#) '  // &
                          '= #.'                                )
            call errint ( '#', i                                )
            call errdp  ( '#', starts(i)                        )
            call errint ( '#', i-1                              )
            call errdp  ( '#', starts(i-1)                      )
            call sigerr ( 'SPICE(TIMESOUTOFORDER)'              )
            return

         end if

      end do


!
!     Now make sure that all of the interval start times coincide with
!     one of the times associated with the actual pointing.
!

      index = 0

      do i = 1, nints

         match = .false.

         do while ( ( .not. match ) .and. ( index < nrec ) )

            index = index + 1

            match = ( starts(i) == sclkdp(index) )

         end do

         if ( .not. match ) then

            call setmsg ( 'Interval start time number # is invalid. '// &
                          'STARTS(#) = *'                             )
            call errint ( '#', i                                      )
            call errint ( '#', i                                      )
            call errdp  ( '*', starts(i)                              )
            call sigerr ( 'SPICE(INVALIDSTARTTIME)'                   )
            return

         end if

      end do

!
!     Make sure that the quaternions are non-zero. This is just
!     a check for uninitialized data.
!
      do i = 1, nrec

         if (  vzerog( quats(0,i), 4 )  ) then

            call setmsg ( 'The quaternion at index # has magnitude ' &
            //            'zero.'                                    )
            call errint ( '#', i                                     )
            call sigerr ( 'SPICE(ZEROQUATERNION)'                    )
            return

         end if

      end do

!
!     No more checks, begin writing the segment.
!

      call dafbna ( handle, descr, segid )

      if ( failed() ) return


!
!     Now add the quaternions and optionally, the angular velocity
!     vectors.
!
      if ( avflag )  then

         do i = 1, nrec
            call dafada ( quats(0,i), 4 )
            call dafada ( avvs (1,i), 3 )
         end do

      else

         call dafada ( quats, 4*nrec )

      end if


!
!     Add the SCLK times.
!
      call dafada ( sclkdp, nrec )

!
!     The time tag directory.  The Ith element is defined to be the
!     (I*100)th SCLK time.
!
      nrdir  =  ( nrec - 1 ) / 100

      index = 100

      do i = 1, nrdir

         call dafada ( sclkdp(index), 1 )

         index    =    index + 100

      end do



!
!     Now add the interval start times.
!
      call dafada ( starts, nints )

!
!     And the directory of interval start times.  The directory of
!     start times will simply be every 100th start time.
!
      nidir  =   ( nints - 1 )  / 100

      index = 100

      do i = 1, nidir

         call dafada ( starts(index), 1 )

         index    =    index + 100

      end do


!
!     Finally, the number of intervals and records.
!
      call dafada ( real(nints, wp), 1 )
      call dafada ( real(nrec, wp),  1 )

!
!     End the segment.
!
      call dafena

      return
      end
