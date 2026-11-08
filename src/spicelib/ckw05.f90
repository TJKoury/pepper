!$Procedure CKW05 ( Write CK segment, type 5 )

      SUBROUTINE CKW05 ( HANDLE,  SUBTYP,  DEGREE,  BEGTIM,  ENDTIM, &
                         INST,    REF,     AVFLAG,  SEGID,   N, &
                         SCLKDP,  PACKTS,  RATE,    NINTS,   STARTS  )

! $ Abstract
!
!     Write a type 5 segment to a CK file.
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
!     CK
!     NAIF_IDS
!     ROTATION
!     TIME
!
! $ Keywords
!
!     FILES
!     POINTING
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ck05.inc'

      INTEGER               HANDLE
      INTEGER               SUBTYP
      INTEGER               DEGREE
      DOUBLE PRECISION      BEGTIM
      DOUBLE PRECISION      ENDTIM
      INTEGER               INST
      CHARACTER*(*)         REF
      LOGICAL               AVFLAG
      CHARACTER*(*)         SEGID
      INTEGER               N
      DOUBLE PRECISION      SCLKDP ( * )
      DOUBLE PRECISION      PACKTS ( * )
      DOUBLE PRECISION      RATE
      INTEGER               NINTS
      DOUBLE PRECISION      STARTS ( * )

      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 23 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     HANDLE     I   Handle of an CK file open for writing.
!     SUBTYP     I   CK type 5 subtype code.
!     DEGREE     I   Degree of interpolating polynomials.
!     BEGTIM     I   Start time of interval covered by segment.
!     ENDTIM     I   End time of interval covered by segment.
!     INST       I   NAIF code for a s/c instrument or structure.
!     REF        I   Reference frame name.
!     AVFLAG     I   Flag indicating if the segment will contain angular
!                    velocity.
!     SEGID      I   Segment identifier.
!     N          I   Number of packets.
!     SCLKDP     I   Encoded SCLK times.
!     PACKTS     I   Array of packets.
!     RATE       I   Nominal SCLK rate in seconds per tick.
!     NINTS      I   Number of intervals.
!     STARTS     I   Encoded SCLK interval start times.
!     MAXDEG     P   Maximum allowed degree of interpolating polynomial.
!
! $ Detailed_Input
!
!     HANDLE   is the file handle of a CK file that has been
!              opened for writing.
!
!     SUBTYP   is an integer code indicating the subtype of the
!              the segment to be created.
!
!     DEGREE   is the degree of the polynomials used to
!              interpolate the quaternions contained in the input
!              packets. All components of the quaternions are
!              interpolated by polynomials of fixed degree.
!
!     BEGTIM,
!     ENDTIM   are the beginning and ending encoded SCLK times
!              for which the segment provides pointing
!              information. BEGTIM must be less than or equal to
!              ENDTIM, and at least one data packet must have a
!              time tag T such that
!
!                 BEGTIM  <  T  <  ENDTIM
!                         -     -
!
!     INST     is the NAIF integer code for the instrument or
!              structure for which a segment is to be created.
!
!     REF      is the NAIF name for a reference frame relative to
!              which the pointing information for INST is
!              specified.
!
!     AVFLAG   is a logical flag which indicates whether or not
!              the segment will contain angular velocity.
!
!     SEGID    is the segment identifier.  A CK segment
!              identifier may contain up to 40 characters.
!
!     N        is the number of packets in the input packet
!              array.
!
!     SCLKDP   are the encoded spacecraft clock times associated
!              with each pointing instance. These times must be
!              strictly increasing.
!
!     PACKTS   contains a time-ordered array of data packets
!              representing the orientation of INST relative to
!              the frame REF. Each packet contains a SPICE-style
!              quaternion and optionally, depending on the
!              segment subtype, attitude derivative data, from
!              which a C-matrix and an angular velocity vector
!              may be derived.
!
!              See the discussion of quaternion styles in
!              $Particulars below.
!
!              The C-matrix represented by the Ith data packet is
!              a rotation matrix that transforms the components
!              of a vector expressed in the base frame specified
!              by REF to components expressed in the instrument
!              fixed frame at the time SCLKDP(I).
!
!              Thus, if a vector V has components x, y, z in the
!              base frame, then V has components x', y', z'
!              in the instrument fixed frame where:
!
!                 [ x' ]     [          ] [ x ]
!                 | y' |  =  |   CMAT   | | y |
!                 [ z' ]     [          ] [ z ]
!
!
!              The attitude derivative information in PACKTS(I)
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
!                             angular velocity vector, as well as
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
!     RATE     is the nominal rate of the spacecraft clock
!              associated with INST. Units are seconds per
!              tick. RATE is used to scale angular velocity
!              to radians/second.
!
!     NINTS    is the number of intervals that the pointing
!              instances are partitioned into.
!
!     STARTS   are the start times of each of the interpolation
!              intervals. These times must be strictly increasing
!              and must coincide with times for which the segment
!              contains pointing.
!
! $ Detailed_Output
!
!     None. See $Particulars for a description of the effect of this
!     routine.
!
! $ Parameters
!
!     MAXDEG   is the maximum allowed degree of the interpolating
!              polynomial. If the value of MAXDEG is increased,
!              the SPICELIB routine CKPFS must be changed
!              accordingly. In particular, the size of the
!              record passed to CKRnn and CKEnn must be
!              increased, and comments describing the record size
!              must be changed.
!
! $ Exceptions
!
!     If any of the following exceptions occur, this routine will
!     return without creating a new segment.
!
!     1)  If HANDLE is not the handle of a C-kernel opened for writing,
!         an error is signaled by a routine in the call tree of this
!         routine.
!
!     2)  If the last non-blank character of SEGID occurs past index
!         40, the error SPICE(SEGIDTOOLONG) is signaled.
!
!     3)  If SEGID contains any nonprintable characters, the error
!         SPICE(NONPRINTABLECHARS) is signaled.
!
!     4)  If the first encoded SCLK time is negative, the error
!         SPICE(INVALIDSCLKTIME) is signaled. If any subsequent times
!         are negative the error will be detected in exception (5).
!
!     5)  If the encoded SCLK times are not strictly increasing,
!         the error SPICE(TIMESOUTOFORDER) is signaled.
!
!     6)  If the name of the reference frame is not one of those
!         supported by the routine FRAMEX, the error
!         SPICE(INVALIDREFFRAME) is signaled.
!
!     7)  If the number of packets N is not at least 1, the error
!         SPICE(TOOFEWPACKETS) is signaled.
!
!     8)  If NINTS, the number of interpolation intervals, is less than
!         or equal to 0, the error SPICE(INVALIDNUMINTS) is signaled.
!
!     9)  If the encoded SCLK interval start times are not strictly
!         increasing, the error SPICE(TIMESOUTOFORDER) is signaled.
!
!     10) If an interval start time does not coincide with a time for
!         which there is an actual pointing instance in the segment, the
!         error SPICE(INVALIDSTARTTIME) is signaled.
!
!     11) This routine assumes that the rotation between adjacent
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
!         radians, where k is any integer. These "large" rotations will
!         yield invalid results when interpolated. You must ensure that
!         the data stored in the segment will not be subject to this
!         sort of ambiguity.
!
!     12) If any quaternion has magnitude zero, the error
!         SPICE(ZEROQUATERNION) is signaled.
!
!     13) If the interpolation window size implied by DEGREE is not
!         even, the error SPICE(INVALIDDEGREE) is signaled. The window
!         size is DEGREE+1 for Lagrange subtypes and is (DEGREE+1)/2
!         for Hermite subtypes.
!
!     14) If an unrecognized subtype code is supplied, the error
!         SPICE(NOTSUPPORTED) is signaled.
!
!     15) If DEGREE is not at least 1 or is greater than MAXDEG, the
!         error SPICE(INVALIDDEGREE) is signaled.
!
!     16) If the segment descriptor bounds are out of order, the
!         error SPICE(BADDESCRTIMES) is signaled.
!
!     17) If there is no element of SCLKDP that lies between BEGTIM and
!         ENDTIM inclusive, the error SPICE(EMPTYSEGMENT) is signaled.
!
!     18) If RATE is zero, the error SPICE(INVALIDVALUE) is signaled.
!
! $ Files
!
!     A new type 5 CK segment is written to the CK file attached
!     to HANDLE.
!
! $ Particulars
!
!     This routine writes a CK type 5 data segment to the open CK
!     file according to the format described in the type 5 section of
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
!     Suppose that you have data packets and are prepared to produce
!     a segment of type 5 in a CK file.
!
!     The following code fragment could be used to add the new segment
!     to a previously opened CK file attached to HANDLE. The file must
!     have been opened with write access.
!
!        C
!        C     Create a segment identifier.
!        C
!              SEGID = 'MY_SAMPLE_CK_TYPE_5_SEGMENT'
!
!        C
!        C     Write the segment.
!        C
!              CALL CKW05 ( HANDLE, SUBTYP, DEGREE, BEGTIM, ENDTIM,
!             .             INST,   REF,    AVFLAG, SEGID,  N,
!             .             SCLKDP, PACKTS, RATE,   NINTS,  STARTS )
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
!-    SPICELIB Version 3.0.1, 08-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.0, 27-JAN-2014 (NJB)
!
!        Increased MAXDEG to 23 for compatibility with CK type 6.
!
!-    SPICELIB Version 2.0.0, 08-FEB-2010 (NJB)
!
!        The check for non-unit quaternions has been replaced
!        with a check for zero-length quaternions.
!
!-    SPICELIB Version 1.1.0, 26-FEB-2008 (NJB)
!
!        Updated header; added information about SPICE
!        quaternion conventions.
!
!        Minor typo in a long error message was corrected.
!
!-    SPICELIB Version 1.0.1, 07-JAN-2005 (NJB)
!
!        Description in $Detailed_Input header section of
!        constraints on BEGTIM and ENDTIM was corrected.
!
!-    SPICELIB Version 1.0.0, 30-AUG-2002 (NJB) (KRG) (JML) (WLT)
!
!-&


! $ Index_Entries
!
!     write CK type_5 data segment
!
!-&


! $ Revisions
!
!-    SPICELIB Version 2.0.0, 08-FEB-2010 (NJB)
!
!        The check for non-unit quaternions has been replaced
!        with a check for zero-length quaternions.
!
!        This change was made to accommodate CK generation,
!        via the non-SPICE utility MEX2KER, for European missions.
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               VZEROG

      INTEGER               BSRCHD
      INTEGER               LASTNB
      INTEGER               LSTLTD

      LOGICAL               FAILED
      LOGICAL               ODD
      LOGICAL               RETURN

!
!     Local parameters
!
      INTEGER               SIDLEN
      PARAMETER           ( SIDLEN  =  40 )

      INTEGER               FPRINT
      PARAMETER           ( FPRINT  =  32 )

      INTEGER               LPRINT
      PARAMETER           ( LPRINT  = 126 )

      INTEGER               ND
      PARAMETER           ( ND      =   2 )

      INTEGER               NI
      PARAMETER           ( NI      =   6 )

      INTEGER               DSCSIZ
      PARAMETER           ( DSCSIZ  =   5 )

      INTEGER               TYPIDX
      PARAMETER           ( TYPIDX  =   4 )

      INTEGER               DTYPE
      PARAMETER           ( DTYPE   =   5 )

      INTEGER               DIRSIZ
      PARAMETER           ( DIRSIZ  = 100 )

      INTEGER               STATSZ
      PARAMETER           ( STATSZ  =   6 )

!
!     Packet structure parameters
!
      INTEGER               QIDX
      PARAMETER           ( QIDX   = 1 )

!
!     Local variables
!
      DOUBLE PRECISION      DC     ( ND )
      DOUBLE PRECISION      DESCR  ( DSCSIZ )

      INTEGER               ADDR
      INTEGER               CHRCOD
      INTEGER               I
      INTEGER               IC     ( NI )
      INTEGER               PACKSZ
      INTEGER               REFCOD
      INTEGER               WINSIZ

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'CKW05' )
      END IF

!
!     Make sure that the number of packets is positive.
!
      IF ( N .LT. 1 ) THEN

         CALL SETMSG ( 'At least 1 packet is required for CK type 5. '// &
                       'Number of packets supplied:  #'                )
         CALL ERRINT ( '#', N                                          )
         CALL SIGERR ( 'SPICE(TOOFEWPACKETS)'                          )
         CALL CHKOUT ( 'CKW05'                                         )
         RETURN

      END IF

!
!     Make sure that there is a positive number of interpolation
!     intervals.
!
      IF ( NINTS .LE. 0 ) THEN

         CALL SETMSG ( '# is an invalid number of interpolation ' // &
                       'intervals for type 5.'                     )
         CALL ERRINT ( '#', NINTS                                  )
         CALL SIGERR ( 'SPICE(INVALIDNUMINTS)'                     )
         CALL CHKOUT ( 'CKW05'                                     )
         RETURN

      END IF


!
!     Get the NAIF integer code for the reference frame.
!
      CALL NAMFRM ( REF, REFCOD )

      IF ( REFCOD .EQ. 0 ) THEN

         CALL SETMSG ( 'The reference frame # is not supported.'   )
         CALL ERRCH  ( '#', REF                                    )
         CALL SIGERR ( 'SPICE(INVALIDREFFRAME)'                    )
         CALL CHKOUT ( 'CKW05'                                     )
         RETURN

      END IF

!
!     Check to see if the segment identifier is too long.
!
      IF ( LASTNB(SEGID) .GT. SIDLEN ) THEN

         CALL SETMSG ( 'Segment identifier contains more than ' // &
                       '40 characters.'                          )
         CALL SIGERR ( 'SPICE(SEGIDTOOLONG)'                     )
         CALL CHKOUT ( 'CKW05'                                   )
         RETURN

      END IF

!
!     Now check that all the characters in the segment identifier
!     can be printed.
!
      DO I = 1, LASTNB(SEGID)

         CHRCOD = ICHAR( SEGID(I:I) )

         IF ( ( CHRCOD .LT. FPRINT ) .OR. ( CHRCOD .GT. LPRINT ) ) THEN

            CALL SETMSG ( 'The segment identifier contains '  // &
                          'nonprintable characters'               )
            CALL SIGERR ( 'SPICE(NONPRINTABLECHARS)'              )
            CALL CHKOUT ( 'CKW05'                                 )
            RETURN

         END IF

      END DO

!
!     Now check that the encoded SCLK times are positive and strictly
!     increasing.
!
!     Check that the first time is nonnegative.
!
      IF ( SCLKDP(1) .LT. 0.D0 ) THEN

         CALL SETMSG ( 'The first SCLKDP time: # is negative.' )
         CALL ERRDP  ( '#', SCLKDP(1)                          )
         CALL SIGERR ( 'SPICE(INVALIDSCLKTIME)'                )
         CALL CHKOUT ( 'CKW05'                                 )
         RETURN

      END IF

!
!     Now check that the times are ordered properly.
!
      DO I = 2, N

         IF ( SCLKDP(I) .LE. SCLKDP(I-1) )  THEN

            CALL SETMSG ( 'The SCLKDP times are not strictly '      // &
                          'increasing. SCLKDP(#) = # and SCLKDP(#) '// &
                          '= #.'                                )
            CALL ERRINT ( '#', I                                )
            CALL ERRDP  ( '#', SCLKDP(I)                        )
            CALL ERRINT ( '#', I-1                              )
            CALL ERRDP  ( '#', SCLKDP(I-1)                      )
            CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'              )
            CALL CHKOUT ( 'CKW05'                               )
            RETURN

         END IF

      END DO

!
!     Now check that the interval start times are ordered properly.
!
      DO I = 2, NINTS

         IF ( STARTS(I) .LE. STARTS(I-1) )  THEN

            CALL SETMSG ( 'The interval start times are not strictly '// &
                          'increasing. STARTS(#) = # and STARTS(#) '  // &
                          '= #.'                                )
            CALL ERRINT ( '#', I                                )
            CALL ERRDP  ( '#', STARTS(I)                        )
            CALL ERRINT ( '#', I-1                              )
            CALL ERRDP  ( '#', STARTS(I-1)                      )
            CALL SIGERR ( 'SPICE(TIMESOUTOFORDER)'              )
            CALL CHKOUT ( 'CKW05'                               )
            RETURN

         END IF

      END DO


!
!     Now make sure that all of the interval start times coincide with
!     one of the times associated with the actual pointing.
!
      DO I = 1, NINTS
!
!        We know the SCLKDP array is ordered, so a binary search is
!        ok.
!
         IF (  BSRCHD ( STARTS(I), N, SCLKDP )  .EQ.  0  ) THEN

            CALL SETMSG ( 'Interval start time number # is invalid. '// &
                          'STARTS(#) = *'                             )
            CALL ERRINT ( '#', I                                      )
            CALL ERRINT ( '#', I                                      )
            CALL ERRDP  ( '*', STARTS(I)                              )
            CALL SIGERR ( 'SPICE(INVALIDSTARTTIME)'                   )
            CALL CHKOUT ( 'CKW05'                                     )
            RETURN

         END IF

      END DO



!
!     Set the window, packet size and angular velocity flag, all of
!     which are functions of the subtype.
!
      IF (  SUBTYP .EQ. C05TP0 ) THEN

         WINSIZ = ( DEGREE + 1 ) / 2
         PACKSZ = C05PS0

      ELSE IF ( SUBTYP .EQ. C05TP1  ) THEN

         WINSIZ = DEGREE + 1
         PACKSZ = C05PS1

      ELSE IF (  SUBTYP .EQ. C05TP2 ) THEN

         WINSIZ = ( DEGREE + 1 ) / 2
         PACKSZ = C05PS2

      ELSE IF ( SUBTYP .EQ. C05TP3  ) THEN

         WINSIZ = DEGREE + 1
         PACKSZ = C05PS3

      ELSE

         CALL SETMSG ( 'CK type 5 subtype <#> is not supported.' )
         CALL ERRINT ( '#', SUBTYP                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                     )
         CALL CHKOUT ( 'CKW05'                                   )
         RETURN

      END IF

!
!     Make sure that the quaternions are non-zero. This is just
!     a check for uninitialized data.
!
      DO I = 1, N
!
!        We have to address the quaternion explicitly, since the shape
!        of the packet array is not known at compile time.
!
         ADDR = PACKSZ * (I-1)  +  QIDX

         IF (  VZEROG( PACKTS(ADDR), 4 )  ) THEN

            CALL SETMSG ( 'The quaternion at index # has magnitude ' &
            //            'zero.'                                    )
            CALL ERRINT ( '#', I                                     )
            CALL SIGERR ( 'SPICE(ZEROQUATERNION)'                    )
            CALL CHKOUT ( 'CKW05'                                    )
            RETURN

         END IF

      END DO

!
!     Make sure that the degree of the interpolating polynomials is
!     in range.
!
      IF (  ( DEGREE .LT. 1 ) .OR. ( DEGREE .GT. MAXDEG )  )  THEN

         CALL SETMSG ( 'The interpolating polynomials have degree #; '// &
                       'the valid degree range is [1, #]'              )
         CALL ERRINT ( '#', DEGREE                                     )
         CALL ERRINT ( '#', MAXDEG                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'CKW05'                                         )
         RETURN

      END IF

!
!     Make sure that the window size is even.  If not, the input
!     DEGREE is incompatible with the subtype.
!
      IF (  ODD(WINSIZ)  )  THEN

         CALL SETMSG ( 'The interpolating polynomials have degree #; '// &
                       'for CK type 5, the degree must be '           // &
                       'equivalent to 3 mod 4 for Hermite '           // &
                       'interpolation and odd for for Lagrange '      // &
                       'interpolation.'                                )
         CALL ERRINT ( '#', DEGREE                                     )
         CALL SIGERR ( 'SPICE(INVALIDDEGREE)'                          )
         CALL CHKOUT ( 'CKW05'                                         )
         RETURN

      END IF


!
!     If we made it this far, we're ready to start writing the segment.
!
!     Create the segment descriptor.
!
!     Assign values to the integer components of the segment descriptor.
!
      IC ( 1 ) = INST
      IC ( 2 ) = REFCOD
      IC ( 3 ) = DTYPE

      IF ( AVFLAG ) THEN
         IC ( 4 ) = 1
      ELSE
         IC ( 4 ) = 0
      END IF

      DC( 1 ) = BEGTIM
      DC( 2 ) = ENDTIM

!
!     Make sure the descriptor times are in increasing order.
!
      IF ( ENDTIM .LT. BEGTIM ) THEN

         CALL SETMSG ( 'Descriptor bounds are non-increasing: #:#' )
         CALL ERRDP  ( '#', BEGTIM                                 )
         CALL ERRDP  ( '#', ENDTIM                                 )
         CALL SIGERR ( 'SPICE(BADDESCRTIMES)'                      )
         CALL CHKOUT ( 'CKW05'                                     )
         RETURN

      END IF

!
!     Make sure that at least one time tag lies between BEGTIM and
!     ENDTIM.  The first time tag not less than BEGTIM must exist
!     and must be less than or equal to ENDTIM.
!
      I = LSTLTD ( BEGTIM, N, SCLKDP )


      IF ( I .EQ. N ) THEN

         CALL SETMSG ( 'All time tags are less than segment start ' // &
                       'time #.'                                  )
         CALL ERRDP  ( '#',  BEGTIM                               )
         CALL SIGERR ( 'SPICE(EMPTYSEGMENT)'                      )
         CALL CHKOUT ( 'CKW05'                                    )
         RETURN

      ELSE IF (  SCLKDP(I+1)  .GT.  ENDTIM  ) THEN

         CALL SETMSG ( 'No time tags lie between the segment start ' // &
                       'time # and segment end time #'              )
         CALL ERRDP  ( '#',  BEGTIM                                 )
         CALL ERRDP  ( '#',  ENDTIM                                 )
         CALL SIGERR ( 'SPICE(EMPTYSEGMENT)'                        )
         CALL CHKOUT ( 'CKW05'                                      )
         RETURN

      END IF

!
!     The clock rate must be non-zero.
!
      IF ( RATE .EQ. 0.D0 ) THEN

         CALL SETMSG ( 'The SCLK rate RATE was zero.'  )
         CALL SIGERR ( 'SPICE(INVALIDVALUE)'           )
         CALL CHKOUT ( 'CKW05'                         )
         RETURN

      END IF

!
!     Now pack the segment descriptor.
!
      CALL DAFPS ( ND, NI, DC, IC, DESCR )

!
!     Begin a new segment.
!
      CALL DAFBNA ( HANDLE, DESCR, SEGID )

      IF ( FAILED() ) THEN
         CALL CHKOUT ( 'CKW05' )
         RETURN
      END IF


!
!     The type 5 segment structure is eloquently described by this
!     diagram from the CK Required Reading:
!
!        +-----------------------+
!        | Packet 1              |
!        +-----------------------+
!        | Packet 2              |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +-----------------------+
!        | Packet N              |
!        +-----------------------+
!        | Epoch 1               |
!        +-----------------------+
!        | Epoch 2               |
!        +-----------------------+
!                    .
!                    .
!                    .
!        +----------------------------+
!        | Epoch N                    |
!        +----------------------------+
!        | Epoch 100                  | (First directory)
!        +----------------------------+
!                    .
!                    .
!                    .
!        +----------------------------+
!        | Epoch ((N-1)/100)*100      | (Last directory)
!        +----------------------------+
!        | Start time 1               |
!        +----------------------------+
!        | Start time 2               |
!        +----------------------------+
!                    .
!                    .
!                    .
!        +----------------------------+
!        | Start time M               |
!        +----------------------------+
!        | Start time 100             | (First interval start
!        +----------------------------+  time directory)
!                    .
!                    .
!                    .
!        +----------------------------+
!        | Start time ((M-1)/100)*100 | (Last interval start
!        +----------------------------+  time directory)
!        | Seconds per tick           |
!        +----------------------------+
!        | Subtype code               |
!        +----------------------------+
!        | Window size                |
!        +----------------------------+
!        | Number of interp intervals |
!        +----------------------------+
!        | Number of packets          |
!        +----------------------------+
!
!
      CALL DAFADA ( PACKTS,  N*PACKSZ )
      CALL DAFADA ( SCLKDP,  N        )

      DO I = 1,   (N-1) / DIRSIZ

         CALL DAFADA (  SCLKDP( DIRSIZ*I ),  1  )

      END DO

!
!     Now add the interval start times.
!
      CALL DAFADA ( STARTS, NINTS )

!
!     And the directory of interval start times.  The directory of
!     start times will simply be every (DIRSIZ)th start time.
!
      DO I = 1, (NINTS -1) / DIRSIZ

         CALL DAFADA ( STARTS( DIRSIZ*I ), 1 )

      END DO

!
!     Add the SCLK rate, segment subtype, window size, interval
!     count, and packet count.
!
      CALL DAFADA ( RATE,            1 )
      CALL DAFADA ( DBLE( SUBTYP ),  1 )
      CALL DAFADA ( DBLE( WINSIZ ),  1 )
      CALL DAFADA ( DBLE( NINTS  ),  1 )
      CALL DAFADA ( DBLE( N      ),  1 )

!
!     As long as nothing went wrong, end the segment.
!
      IF ( .NOT. FAILED() ) THEN
         CALL DAFENA
      END IF


      CALL CHKOUT ( 'CKW05' )
      RETURN
      END
