!$Procedure CKW02 ( C-Kernel, write segment to C-kernel, data type 2 )

      subroutine ckw02 ( handle, begtim, endtim, inst,  ref,  segid, &
                         nrec,   start,  stop,   quats, avvs, rates )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Write a type 2 segment to a C-kernel.
!
! $ Required_Reading
!
!     CK
!     DAF
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
      character(*)          segid
      integer(ip)           nrec
      real(wp)              start  (      * )
      real(wp)              stop   (      * )
      real(wp)              quats  ( 0:3, * )
      real(wp)              avvs   (   3, * )
      real(wp)              rates  (      * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an open CK file.
!     BEGTIM     I   The beginning encoded SCLK of the segment.
!     ENDTIM     I   The ending encoded SCLK of the segment.
!     INST       I   The NAIF instrument ID code.
!     REF        I   The reference frame of the segment.
!     SEGID      I   Segment identifier.
!     NREC       I   Number of pointing records.
!     START      I   Encoded SCLK interval start times.
!     STOP       I   Encoded SCLK interval stop times.
!     QUATS      I   SPICE quaternions representing instrument pointing.
!     AVVS       I   Angular velocity vectors.
!     RATES      I   Number of seconds per tick for each interval.
!
! $ Detailed_Input
!
!     HANDLE   is the handle of the CK file to which the segment will
!              be written. The file must have been opened with write
!              access.
!
!     BEGTIM   is the beginning encoded SCLK time of the segment. This
!              value should be less than or equal to the first START
!              time in the segment.
!
!     ENDTIM   is the encoded SCLK time at which the segment ends.
!              This value should be greater than or equal to the last
!              STOP time in the segment.
!
!     INST     is the NAIF integer ID code for the instrument.
!
!     REF      is a character string that specifies the
!              reference frame of the segment. This should be one of
!              the frames supported by the SPICELIB routine NAMFRM
!              which is an entry point to FRAMEX.
!
!     SEGID    is the segment identifier. A CK segment identifier may
!              contain up to 40 characters.
!
!     NREC     is the number of pointing intervals that will be
!              written to the segment.
!
!     START    are the start times of each interval in encoded
!              spacecraft clock. These times must be strictly
!              increasing.
!
!     STOP     are the stop times of each interval in encoded
!              spacecraft clock. These times must be greater than
!              the START times that they correspond to but less
!              than or equal to the START time of the next interval.
!
!     QUATS    is an array of SPICE-style quaternions representing
!              the C-matrices associated with the start times of each
!              interval. See the discussion of quaternion styles in
!              $Particulars below.
!
!     AVVS     are the angular velocity vectors for each interval.
!
!     RATES    are the number of seconds per encoded spacecraft clock
!              tick for each interval.
!
!              In most applications this value will be the same for
!              each interval within a segment. For example, when
!              constructing a predict C-kernel for Mars Observer, the
!              rate would be 1/256 for each interval since this is
!              the smallest time unit expressible by the MO clock. The
!              nominal seconds per tick rates for Galileo and Voyager
!              are 1/120 and 0.06 respectively.
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
!     3)  If SEGID contains any nonprintable characters, the error
!         SPICE(NONPRINTABLECHARS) is signaled.
!
!     4)  If the first START time is negative, the error
!         SPICE(INVALIDSCLKTIME) is signaled.
!
!     5)  If the second or any subsequent START times are negative, the
!         error SPICE(TIMESOUTOFORDER) is signaled.
!
!     6)  If any of the STOP times are negative, the error
!         SPICE(DEGENERATEINTERVAL) is signaled.
!
!     7)  If the STOP time of any of the intervals is less than or equal
!         to the START time, the error SPICE(DEGENERATEINTERVAL) is
!         signaled.
!
!     8)  If the START times are not strictly increasing, the
!         error SPICE(TIMESOUTOFORDER) is signaled.
!
!     9)  If the STOP time of one interval is greater than the START
!         time of the next interval, the error SPICE(BADSTOPTIME)
!         is signaled.
!
!     10) If BEGTIM is greater than START(1) or ENDTIM is less than
!         STOP(NREC), the error SPICE(INVALIDDESCRTIME) is
!         signaled.
!
!     11) If the name of the reference frame is not one of those
!         supported by the routine NAMFRM, the error
!         SPICE(INVALIDREFFRAME) is signaled.
!
!     12) If NREC, the number of pointing records, is less than or
!         equal to 0, the error SPICE(INVALIDNUMRECS) is signaled.
!
!     13) If any quaternion has magnitude zero, the error
!         SPICE(ZEROQUATERNION) is signaled.
!
! $ Files
!
!     This routine adds a type 2 segment to a C-kernel. The C-kernel
!     may be either a new one or an existing one opened for writing.
!
! $ Particulars
!
!     For a detailed description of a type 2 CK segment please see the
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
!     1) The following example creates a CK file with a type-2 segment,
!        with data for a simple time dependent rotation and angular
!        velocity.
!
!        Example code begins here.
!
!
!              PROGRAM CKW02_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              CHARACTER*(*)         CK2
!              PARAMETER           ( CK2 = 'ckw02_ex1.bc' )
!
!              DOUBLE PRECISION      SPTICK
!              PARAMETER           ( SPTICK = 0.001D0 )
!
!              INTEGER               INST
!              PARAMETER           ( INST = -77702 )
!
!              INTEGER               MAXREC
!              PARAMETER           ( MAXREC = 21 )
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
!              DOUBLE PRECISION      RATES  (     MAXREC )
!              DOUBLE PRECISION      RWMAT  ( 3, 3 )
!              DOUBLE PRECISION      SPACES
!              DOUBLE PRECISION      STARTS (     MAXREC )
!              DOUBLE PRECISION      STOPS  (     MAXREC )
!              DOUBLE PRECISION      STICKS
!              DOUBLE PRECISION      THETA
!              DOUBLE PRECISION      WMAT   ( 3, 3 )
!              DOUBLE PRECISION      WQUAT  ( 0:3 )
!
!              INTEGER               HANDLE
!              INTEGER               I
!              INTEGER               NCOMCH
!
!        C
!        C     NCOMCH is the number of characters to reserve for the
!        C     kernel's comment area. This example doesn't write
!        C     comments, so set to zero.
!        C
!              NCOMCH = 0
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
!              SEGID  = 'Test type 2 CK segment'
!              IFNAME = 'Test CK type 2 segment created by CKW02'
!
!
!        C
!        C     Open a new kernel.
!        C
!              CALL CKOPN ( CK2, IFNAME, NCOMCH, HANDLE )
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
!        C     Set the initial value of the encoded ticks. The interval
!        C     associated with each quaternion will start at the epoch
!        C     of the quaternion and will extend 0.8 * STICKS forward in
!        C     time, leaving small gaps between the intervals.
!        C
!        C     The clock rates array will have a constant SPTICK value.
!        C
!              STARTS(1) = 1000.D0
!              STOPS(1)  = STARTS(1) + ( 0.8D0 * STICKS )
!              RATES(1)  = SPTICK
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
!                 STARTS(I) = 1000.D0 + (I-1) * STICKS
!                 STOPS(I)  = STARTS(I) + ( 0.8D0 * STICKS )
!                 RATES(I)  = SPTICK
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
!        C     Set the segment boundaries equal to the first and last
!        C     time for the data arrays.
!        C
!              BEGTIM = STARTS(1)
!              ENDTIM = STOPS(MAXREC)
!
!        C
!        C     All information ready to write. Write to a CK type 2
!        C     segment to the file indicated by HANDLE.
!        C
!              CALL CKW02 ( HANDLE, BEGTIM, ENDTIM, INST,  REF,
!             .             SEGID,  MAXREC, STARTS, STOPS, QUATS,
!             .             AVVS,   RATES                       )
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
!     K.R. Gehringer     (JPL)
!     J.M. Lynch         (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.1, 26-MAY-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Created
!        complete code example from existing fragment.
!
!        Updated Exception #12 to describe the actual check and error
!        produced by this routine.
!
!-    SPICELIB Version 3.0.0, 01-JUN-2010 (NJB)
!
!        The check for non-unit quaternions has been replaced
!        with a check for zero-length quaternions.
!
!-    SPICELIB Version 2.2.0, 26-FEB-2008 (NJB)
!
!        Updated header; added information about SPICE
!        quaternion conventions.
!
!        Minor typo in a long error message was corrected.
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
!-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
!
!        1) If the number of pointing records is not positive an error
!           is now signaled.
!
!        2) FAILED is checked after the call to DAFBNA.
!
!        3) The variables HLDBEG and HLDEND were removed from the loop
!           where the interval start and stop times are tested.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 30-AUG-1991 (JML)
!
!-&


! $ Index_Entries
!
!     write CK type_2 pointing data segment
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.1.0, 22-FEB-1999 (WLT)
!
!        Added check to make sure that all quaternions are unit
!        length to single precision.
!
!-    SPICELIB Version 1.1.1, 05-SEP-1993 (KRG)
!
!        Removed all references to a specific method of opening the CK
!        file in the $Brief_I/O, $Detailed_Input, $Exceptions,
!        $Files, and $Examples sections of the header. It is assumed
!        that a person using this routine has some knowledge of the DAF
!        system and the methods for obtaining file handles.
!
!-    SPICELIB Version 1.1.0, 25-NOV-1992 (JML)
!
!        1) If the number of pointing records is not positive an error
!           is now signaled.
!
!        2) FAILED is checked after the call to DAFBNA.
!
!        3) The variables HLDBEG and HLDEND were removed from the loop
!           where the interval start and stop times are tested.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 30-AUG-1991 (JML)
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
      integer(ip), parameter :: sidlen = 40

      integer(ip), parameter :: ndc = 5

      integer(ip), parameter :: nd = 2

      integer(ip), parameter :: ni = 6

      integer(ip), parameter :: dtype = 2

      integer(ip), parameter :: fprint = 32

      integer(ip), parameter :: lprint = 126


!
!     Local variables
!
      real(wp)              dcd    ( nd    )
      real(wp)              dirent
      real(wp)              descr  ( ndc   )

      integer(ip)           i
      integer(ip)           icd    ( ni    )
      integer(ip)           index
      integer(ip)           ndir
      integer(ip)           refcod
      integer(ip)           value


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
!           ICD( 5 )              -- Beginning address of the segment.
!           ICD( 6 )              -- Ending address of the segment.
!

!
!     Make sure that there is a positive number of pointing records.
!
      if ( nrec <= 0 ) then

         call setmsg ( '# is an invalid number of pointing ' // &
                       'instances for type 2.'                     )
         call errint ( '#', nrec                                   )
         call sigerr ( 'SPICE(INVALIDNUMREC)'                      )
         return

      end if


!
!     Check that the SCLK bounds on the segment are reasonable.
!
      if ( begtim > start(1) ) then

         call setmsg ( 'The first d.p. component of the descriptor ' // &
                       'is invalid.  DCD(1) = # and START(1) = # '    )
         call errdp  ( '#', begtim               )
         call errdp  ( '#', start(1)             )
         call sigerr ( 'SPICE(INVALIDDESCRTIME)' )
         return

      end if

      if ( endtim < stop(nrec) ) then

         call setmsg ( 'The second d.p. component of the descriptor ' // &
                       'is invalid.  DCD(2) = # and STOP(NREC) = # '   )
         call errdp  ( '#', endtim               )
         call errdp  ( '#', stop(nrec)           )
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
!     By definition data type two must have angular velocity.
!
      icd ( 1 ) = inst
      icd ( 2 ) = refcod
      icd ( 3 ) = dtype
      icd ( 4 ) = 1


!
!     Now pack the segment descriptor.
!
      call dafps ( nd, ni, dcd, icd, descr )

!
!     Now check that all the characters in the segid can be printed.
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
!     Now check that the START and STOP times on the intervals
!     make sense. Three checks will be performed on each interval:
!
!        1)  Check that the STOP time is greater than the START time.
!
!        2)  Check that the START times are strictly increasing.
!
!        3)  Check that the START time is greater than or equal to the
!            STOP time from the previous interval.
!
!     For the first interval also make sure that the START time is
!     nonnegative.
!
      if ( start(1) < 0.0_wp ) then

         call setmsg ( 'The first START time: # is negative.'  )
         call errdp  ( '#', start(1)                           )
         call sigerr ( 'SPICE(INVALIDSCLKTIME)'                )
         return

      end if

      if ( stop(1) <= start(1) ) then

         call setmsg ( 'The STOP time is less than or equal to the '  // &
                       'START time for interval number 1. START time '// &
                       'is # and STOP time is #.'                      )
         call errdp  ( '#', start(1)               )
         call errdp  ( '#', stop (1)               )
         call sigerr ( 'SPICE(DEGENERATEINTERVAL)' )
         return

      end if


      do i = 2, nrec

         if ( stop(i) <= start(i) ) then

            call setmsg ( 'The STOP time is less than or equal to '  // &
                          'the START time for interval number #. '   // &
                          'START time is # and STOP time is #.'       )
            call errint ( '#', i                                      )
            call errdp  ( '#', start(i)                               )
            call errdp  ( '#', stop (i)                               )
            call sigerr ( 'SPICE(DEGENERATEINTERVAL)'                 )
            return

         end if

         if ( start(i) <= start(i-1) )  then

            call setmsg ( 'The START times are not strictly '      // &
                          'increasing.  START(#) = # and START(#) '// &
                          '= #.'                                )
            call errint ( '#', i                                )
            call errdp  ( '#', start(i)                         )
            call errint ( '#', i-1                              )
            call errdp  ( '#', start(i-1)                       )
            call sigerr ( 'SPICE(TIMESOUTOFORDER)'              )
            return

         end if

         if ( stop(i-1) > start(i) )  then

            call setmsg ( 'The STOP time for interval # is greater ' // &
                          'than the following START time. STOP(#) = '// &
                          '# and START(#) = #.'                       )
            call errint ( '#', i-1                                    )
            call errint ( '#', i-1                                    )
            call errdp  ( '#', stop(i-1)                              )
            call errint ( '#', i                                      )
            call errdp  ( '#', start(i)                               )
            call sigerr ( 'SPICE(BADSTOPTIME)'                        )
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
!     Now add the quaternions, angular velocity vectors, and time
!     conversion factors for each interval.
!
      do i = 1, nrec
         call dafada ( quats(0,i), 4 )
         call dafada ( avvs (1,i), 3 )
         call dafada ( rates(i),   1 )
      end do

!
!     The SCLK start times.
!
      call dafada ( start, nrec )
!
!     The SCLK stop times.
!
      call dafada ( stop, nrec )

!
!     The time tag directory.  The Ith element is defined to be the
!     average of the (I*100)th STOP time and the (I*100+1)th START time.
!
      ndir  =  ( nrec - 1 ) / 100

      index = 100

      do i = 1, ndir
         dirent   =  ( stop(index) + start(index+1) ) / 2.0_wp
         call dafada ( dirent, 1 )
         index    =    index + 100
      end do

!
!     End the segment.
!
      call dafena

      return
      end
