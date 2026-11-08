!$Procedure CKE05 ( C-Kernel, evaluate, type 5 )

      SUBROUTINE CKE05 ( NEEDAV, RECORD, CMAT, AV, CLKOUT )

! $ Abstract
!
!     Evaluate a single data record from a type 5 CK segment.
!
! $ Required_Reading
!
!     CK
!
! $ Keywords
!
!     POINTING
!
! $ Declarations

      IMPLICIT NONE

      INCLUDE 'ck05.inc'
      INCLUDE 'ckparam.inc'


      LOGICAL               NEEDAV
      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      CMAT     ( 3, 3 )
      DOUBLE PRECISION      AV       ( 3 )
      DOUBLE PRECISION      CLKOUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NEEDAV     I   .TRUE. if angular velocity is requested.
!     RECORD    I-O  Data type 5 record.
!     CMAT       O   C-matrix.
!     AV         O   Angular velocity vector.
!     CLKOUT     O   SCLK associated with C-matrix.
!
! $ Detailed_Input
!
!     NEEDAV   is .TRUE. if angular velocity is requested.
!
!     RECORD   is a record from a type 5 CK segment which, when
!              evaluated at the epoch contained in its first
!              element, will give the attitude and angular velocity
!              of a spacecraft structure or instrument relative to a
!              base reference frame.
!
!              The structure of the record is as follows:
!
!                 +----------------------+
!                 | evaluation epoch     |
!                 +----------------------+
!                 | subtype code         |
!                 +----------------------+
!                 | number of packets (n)|
!                 +----------------------+
!                 | nominal SCLK rate    |
!                 +----------------------+
!                 | packet 1             |
!                 +----------------------+
!                 | packet 2             |
!                 +----------------------+
!                          .
!                          .
!                          .
!                 +----------------------+
!                 | packet n             |
!                 +----------------------+
!                 | epochs 1--n          |
!                 +----------------------+
!
!                See the CK Required Reading or the include file
!                ck05.inc for details on CK type 5 packet contents.
!
! $ Detailed_Output
!
!     RECORD   has been modified due to its use as a workspace array.
!              The contents are undefined.
!
!
!     CMAT     is a rotation matrix that transforms the components
!              of a vector expressed in the base frame given in
!              the segment to components expressed in the instrument
!              fixed frame at the returned time.
!
!              Thus, if a vector v has components x, y, z in the
!              base frame, then v has components x', y', z' in the
!              instrument fixed frame where:
!
!                   [ x' ]     [          ] [ x ]
!                   | y' |  =  |   CMAT   | | y |
!                   [ z' ]     [          ] [ z ]
!
!              If the x', y', z' components are known, use the
!              transpose of the C-matrix to determine x, y, z as
!              follows.
!
!                   [ x ]      [          ]T    [ x' ]
!                   | y |  =   |   CMAT   |     | y' |
!                   [ z ]      [          ]     [ z' ]
!                            (Transpose of CMAT)
!
!
!     AV       is the angular velocity vector of the instrument fixed
!              frame defined by CMAT. The angular velocity is
!              returned only if NEEDAV is .TRUE.
!
!              The direction of the angular velocity vector gives
!              the right-handed axis about which the instrument fixed
!              reference frame is rotating. The magnitude of AV is
!              the magnitude of the instantaneous velocity of the
!              rotation, in radians per second.
!
!              The angular velocity vector is returned in component
!              form
!
!                       AV = [ AV1  , AV2  , AV3  ]
!
!              which is in terms of the base coordinate frame
!              specified in the segment descriptor.
!
!     CLKOUT   is the encoded SCLK associated with the returned
!              C-matrix and angular velocity vector.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the input record contains an unrecognized subtype code,
!         the error SPICE(NOTSUPPORTED) is signaled.
!
!     2)  If the record subtype is one for which quaternion derivatives
!         are stored (subtypes 0 and 2), and if the Ith quaternion in
!         the input record is farther than its negative from the (I-1)st
!         quaternion in the record, the error SPICE(BADQUATSIGN) is
!         signaled.
!
!         For subtypes 1 and 3, this condition is not considered an
!         error: the closer to the preceding quaternion of the two
!         quaternion representations is used for interpolation.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The exact format and structure of CK type 5 (MEX/Rosetta Attitude
!     file interpolation) CK segments is described in the CK Required
!     Reading.
!
! $ Examples
!
!     The CKEnn routines are almost always used in conjunction with
!     the corresponding CKRnn routines, which read the records from
!     CK files.
!
!     The following code fragment searches through all of the segments
!     in a file applicable to the Mars Express spacecraft bus that
!     are of data type 5, for a particular spacecraft clock time.
!     It then evaluates the pointing for that epoch and prints the
!     result.
!
!           CHARACTER*(20)        SCLKCH
!           CHARACTER*(20)        SCTIME
!           CHARACTER*(40)        IDENT
!
!           INTEGER               I
!           INTEGER               SC
!           INTEGER               INST
!           INTEGER               HANDLE
!           INTEGER               DTYPE
!           INTEGER               ICD      (    6 )
!
!           DOUBLE PRECISION      SCLKDP
!           DOUBLE PRECISION      TOL
!           DOUBLE PRECISION      CLKOUT
!           DOUBLE PRECISION      DESCR    (    5 )
!           DOUBLE PRECISION      DCD      (    2 )
!           DOUBLE PRECISION      RECORD   (   17 )
!           DOUBLE PRECISION      CMAT     ( 3, 3 )
!           DOUBLE PRECISION      AV       (    3 )
!
!           LOGICAL               NEEDAV
!           LOGICAL               FND
!           LOGICAL               SFND
!
!
!           SC     = -41
!           INST   = -41000
!           DTYPE  =  5
!           NEEDAV = .FALSE.
!
!     C
!     C     Load the MEX SCLK kernel and the C-kernel.
!     C
!           CALL FURNSH ( 'MEX_SCLK.TSC'       )
!           CALL DAFOPR ( 'MEX_CK.BC',  HANDLE )
!     C
!     C     Get the spacecraft clock time. Then encode it for use
!     C     in the C-kernel.
!     C
!           WRITE (*,*) 'Enter spacecraft clock time string:'
!           READ (*,FMT='(A)') SCLKCH
!
!           CALL SCENCD ( SC, SCLKCH, SCLKDP )
!     C
!     C     Use a tolerance of 2 seconds ( half of the nominal
!     C     separation between MEX pointing instances ).
!     C
!           CALL SCTIKS ( SC, '0000000002:000', TOL )
!
!     C
!     C     Search from the beginning of the CK file through all
!     C     of the segments.
!     C
!           CALL DAFBFS ( HANDLE )
!           CALL DAFFNA ( SFND   )
!
!           FND    = .FALSE.
!
!           DO WHILE ( ( SFND ) .AND. ( .NOT. FND ) )
!
!     C
!     C        Get the segment identifier and descriptor.
!     C
!              CALL DAFGN ( IDENT )
!              CALL DAFGS ( DESCR )
!     C
!     C        Unpack the segment descriptor into its integer and
!     C        double precision components.
!     C
!              CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!
!     C
!     C        Determine if this segment should be processed.
!     C
!              IF ( ( INST          .EQ. ICD( 1 ) ) .AND.
!          .        ( SCLKDP + TOL  .GE. DCD( 1 ) ) .AND.
!          .        ( SCLKDP - TOL  .LE. DCD( 2 ) ) .AND.
!          .        ( DTYPE         .EQ. ICD( 3 ) )      ) THEN
!
!
!                 CALL CKR05 ( HANDLE, DESCR, SCLKDP, TOL, NEEDAV,
!          .                   RECORD, FND )
!
!                 IF ( FND ) THEN
!
!                    CALL CKE05 (NEEDAV,RECORD,CMAT,AV,CLKOUT)
!
!                    CALL SCDECD ( SC, CLKOUT, SCTIME )
!
!                    WRITE (*,*)
!                    WRITE (*,*) 'Segment identifier: ', IDENT
!                    WRITE (*,*)
!                    WRITE (*,*) 'Pointing returned for time: ',
!          .                      SCTIME
!                    WRITE (*,*)
!                    WRITE (*,*) 'C-matrix:'
!                    WRITE (*,*)
!                    WRITE (*,*) ( CMAT(1,I), I = 1, 3 )
!                    WRITE (*,*) ( CMAT(2,I), I = 1, 3 )
!                    WRITE (*,*) ( CMAT(3,I), I = 1, 3 )
!                    WRITE (*,*)
!
!                 END IF
!
!              END IF
!
!              CALL DAFFNA ( SFND )
!
!           END DO
!
! $ Restrictions
!
!     1)  This routine assumes that the input record is valid. Any
!         checking of the input data is assumed to have been performed
!         when the source CK file was created.
!
!     2)  This routine assumes that the input data are suitable for the
!         interpolation method indicated by the subtype code in the
!         input record. Since the mapping of rotations to quaternions
!         is multiple-valued, this routine assumes that whichever sign
!         minimizes the Euclidean distance between one quaternion and
!         the next is the correct sign.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!
! $ Version
!
!-    SPICELIB Version 3.1.1, 12-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.1.0, 11-AUG-2015 (NJB)
!
!        Bug fix: PRVPTR is now updated at the end of the quaternion
!        sequence check for Hermite subtypes.
!
!-    SPICELIB Version 3.0.0, 06-FEB-2014 (NJB)
!
!        Bug fix and functional change: quaternion sign adjustment
!        is no longer performed for the Hermite subtypes (0 and 2).
!        If a sign adjustment is needed for quaternions belonging to
!        a record of Hermite subtype, an error is signaled. Sign
!        adjustment is still performed for the Lagrange subtypes.
!
!        Corrected in-line comments concerning change of AV units.
!
!-    SPICELIB Version 2.0.0, 20-NOV-2006 (NJB)
!
!        Bug fix: this routine now assumes that angular velocity
!        and quaternion derivative values stored in the input
!        record have units of radians/second.
!
!        Bug fix: this routine no longer attempts to determine
!        the correct sign of quaternion derivatives. The caller
!        must supply quaternion derivatives that are suitable
!        for interpolation.
!
!-    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments in
!        XPOSEG and VSCL calls. Replaced header reference to LDPOOL
!        with reference to FURNSH.
!
!-    SPICELIB Version 1.2.0, 14-FEB-2003 (NJB)
!
!        Bug fix: angular velocity computation was modified to
!        match that used in the corresponding algorithm employed
!        by the MEX/Rosetta attitude file reader. The quaternion
!        derivative used to derive angular velocity now is the
!        derivative of the *unit* quaternion.
!
!-    SPICELIB Version 1.1.0, 06-SEP-2002 (NJB)
!
!-&


! $ Index_Entries
!
!     evaluate type_5 CK segment
!
!-&


! $ Revisions
!
!-    SPICELIB Version 1.3.0, 23-OCT-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments in
!        XPOSEG and VSCL calls. Replaced header reference to LDPOOL
!        with reference to FURNSH.
!
!-    SPICELIB Version 1.2.0, 14-FEB-2003 (NJB)
!
!        Bug fix: angular velocity computation was modified to
!        match that used in the corresponding algorithm employed
!        by the MEX/Rosetta attitude file reader. The quaternion
!        derivative used to derive angular velocity now is the
!        derivative of the *unit* quaternion.
!
!        Letting Q(t) be the quaternion derived by polynomial
!        interpolation, and letting UQ(t) be Q(t)/||Q(t)||,
!        the quaternion derivative d(UQ)/dt is now used.
!
!-&


!
!     SPICELIB functions
!
      DOUBLE PRECISION      LGRINT
      DOUBLE PRECISION      VDISTG
      DOUBLE PRECISION      VDOTG
      DOUBLE PRECISION      VNORMG

      LOGICAL               RETURN

!
!     Local parameters
!

!
!     Index of evaluation epoch in record:
!
      INTEGER               EPCIDX
      PARAMETER           ( EPCIDX = 1 )

!
!     Index of subtype code in record:
!
      INTEGER               SBTIDX
      PARAMETER           ( SBTIDX = 2 )

!
!     Index of packet count in record:
!
      INTEGER               CNTIDX
      PARAMETER           ( CNTIDX = 3 )

!
!     Index at which packets start; packet base:
!
      INTEGER               PKTIDX
      PARAMETER           ( PKTIDX = 5 )

      INTEGER               PKTBAS
      PARAMETER           ( PKTBAS = PKTIDX - 1 )

!
!     Local variables
!
      DOUBLE PRECISION      DQ     ( 0 : 3 )
      DOUBLE PRECISION      DS     ( 0 : 3 )
      DOUBLE PRECISION      LOCREC ( CKMRSZ )
      DOUBLE PRECISION      MAGS
      DOUBLE PRECISION      Q      ( 0 : 3 )
      DOUBLE PRECISION      QNEG   ( 0 : 3 )
      DOUBLE PRECISION      RADTRM ( 0 : 3 )
      DOUBLE PRECISION      RATE
      DOUBLE PRECISION      SCLDDQ ( 0 : 3 )
      DOUBLE PRECISION      SCLKDP
      DOUBLE PRECISION      STATE  ( 8 )
      DOUBLE PRECISION      VBUFF  ( 6 )
      DOUBLE PRECISION      WORK   ( CKMRSZ * 2,  2 )

      INTEGER               FROM
      INTEGER               I
      INTEGER               J
      INTEGER               N
      INTEGER               NEWPTR
      INTEGER               PACKSZ
      INTEGER               PRVPTR
      INTEGER               SUBTYP
      INTEGER               TO
      INTEGER               UB
      INTEGER               XSTART
      INTEGER               YSTART


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'CKE05' )

!
!     Capture the subtype from the record and set the packet size
!     accordingly.
!
      SUBTYP =  NINT( RECORD(SBTIDX) )


      IF ( SUBTYP .EQ. C05TP0 ) THEN

         PACKSZ = C05PS0

      ELSE IF ( SUBTYP .EQ. C05TP1 ) THEN

         PACKSZ = C05PS1

      ELSE IF ( SUBTYP .EQ. C05TP2 ) THEN

         PACKSZ = C05PS2

      ELSE IF ( SUBTYP .EQ. C05TP3 ) THEN

         PACKSZ = C05PS3

      ELSE

         CALL SETMSG ( 'Unexpected CK type 5 subtype # found in ' // &
                       'type 5 segment.'                          )
         CALL ERRINT ( '#',  SUBTYP                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'CKE05'                                    )
         RETURN

      END IF

!
!     Get the packet count and epoch.
!
      N       =  NINT( RECORD(CNTIDX) )
      SCLKDP  =        RECORD(EPCIDX)

!
!     Get the nominal clock rate.
!
      RATE = RECORD(4)

!
!     Adjust quaternion "signs" as necessary to minimize distance
!     between successive quaternions. This adjustment is performed
!     only for subtypes that don't store quaternion derivatives
!     (these are the Lagrange subtypes).
!
      IF (  ( SUBTYP .EQ. C05TP1 ) .OR. ( SUBTYP .EQ. C05TP3 )  ) THEN
!
!        For these subtypes, only the quaternions themselves need be
!        adjusted.
!
!        PRVPTR is the index of the "previous" quaternion---the one to
!        which the successor and its negative will be compared.
!
         PRVPTR = PKTIDX

         DO I = 2, N
!
!           NEWPTR points to the quaternion ahead of the one
!           pointed to by PRVPTR.
!
            NEWPTR = PKTIDX + PACKSZ*(I-1)

            CALL VMINUG ( RECORD(NEWPTR), 4,  QNEG )

!
!           Replace the Ith quaternion with QNEG if QNEG is closer
!           than the current quaternion to the previous quaternion.
!
            IF (     VDISTG( RECORD(PRVPTR), QNEG,           4 ) &
                .LT. VDISTG( RECORD(PRVPTR), RECORD(NEWPTR), 4 ) ) THEN

               CALL MOVED ( QNEG, 4, RECORD( NEWPTR ) )

            END IF

            PRVPTR = NEWPTR

         END DO


      ELSE
!
!        For the Hermite types, if the quaternions need to be adjusted,
!        we have an error condition.
!
!        PRVPTR is the index of the "previous" quaternion---the one to
!        which the successor and its negative will be compared.
!
         PRVPTR = PKTIDX

         DO I = 2, N
!
!           NEWPTR points to the quaternion ahead of the one
!           pointed to by PRVPTR.
!
            NEWPTR = PKTIDX + PACKSZ*(I-1)

            CALL VMINUG ( RECORD(NEWPTR), 4,  QNEG )
!
!           Replace the Ith quaternion with QNEG if QNEG is closer
!           than the current quaternion to the previous quaternion.
!
            IF (     VDISTG( RECORD(PRVPTR), QNEG,           4 ) &
                .LT. VDISTG( RECORD(PRVPTR), RECORD(NEWPTR), 4 ) ) THEN

               CALL SETMSG ( 'Quaternion sign error: quaternion at ' &
               //            'index # in the input record is farther ' &
               //            'than its negative from the preceding ' &
               //            'quaternion in the record. Quaternion ' &
               //            'is (#, #, #, #); predecessor is ' &
               //            '(#, #, #, #). This makes the quaternion ' &
               //            'sequence unsuitable for Hermite ' &
               //            'interpolation. The quaternions, and ' &
               //            'if applicable, their derivatives, ' &
               //            'must be adjusted before they are ' &
               //            'passed to this routine.'                 )
               CALL ERRINT ( '#', I                                    )
               CALL ERRDP  ( '#', RECORD(NEWPTR  )                     )
               CALL ERRDP  ( '#', RECORD(NEWPTR+1)                     )
               CALL ERRDP  ( '#', RECORD(NEWPTR+2)                     )
               CALL ERRDP  ( '#', RECORD(NEWPTR+3)                     )
               CALL ERRDP  ( '#', RECORD(PRVPTR  )                     )
               CALL ERRDP  ( '#', RECORD(PRVPTR+1)                     )
               CALL ERRDP  ( '#', RECORD(PRVPTR+2)                     )
               CALL ERRDP  ( '#', RECORD(PRVPTR+3)                     )
               CALL SIGERR ( 'SPICE(BADQUATSIGN)'                      )
               CALL CHKOUT ( 'CKE05'                                   )
               RETURN

            END IF

            PRVPTR = NEWPTR

         END DO

      END IF



      IF ( SUBTYP .EQ. C05TP1 ) THEN
!
!        We perform Lagrange interpolation on each quaternion
!        component, and obtain quaternion derivatives from the
!        interpolating polynomials.  The quaternion and derivative
!        gives us angular velocity.
!
!        We'll transpose the pointing information in the input record so
!        that contiguous pieces of it can be shoved directly into the
!        interpolation routine LGRINT.  We allow LGRINT to overwrite
!        the state values in the input record, since this saves local
!        storage and does no harm.  (See the header of LGRINT for a
!        description of its work space usage.)
!
         N   =  NINT( RECORD(CNTIDX) )

         CALL XPSGIP ( PACKSZ, N, RECORD(PKTIDX) )

!
!        We interpolate each state component in turn.
!
         XSTART   =   PKTIDX   +  N * PACKSZ

         DO I = 1, PACKSZ

            YSTART    =   PKTIDX  +  N * (I-1)


            CALL LGRIND ( N, &
                          RECORD(XSTART), &
                          RECORD(YSTART), &
                          WORK, &
                          SCLKDP, &
                          STATE(I), &
                          STATE(I+4)  )

         END DO

!
!        The output quaternion is a unitized version of the
!        interpolated state.
!
         MAGS = VNORMG( STATE, 4 )


         IF ( MAGS .EQ. 0.D0 ) THEN

            CALL SETMSG ( 'Quaternion magnitude at SCLK # was zero.' )
            CALL ERRDP  ( '#',  SCLKDP                               )
            CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                      )
            CALL CHKOUT ( 'CKE05'                                    )
            RETURN

         END IF

         CALL VSCLG ( 1.D0/MAGS, STATE, 4, Q )


         IF ( NEEDAV ) THEN
!
!           Find the time derivative of the unit quaternion:
!           Letting S represent the quaternion portion of STATE, we
!           have
!
!              Q = S/||S||
!
!
!           Then letting < , > denote the 4-dimensional inner product
!           operator, we have
!
!
!                         d(S)/dt      < Q, d(S)/dt >
!              d(Q)/dt =  -------  -   -------------- * Q
!                          ||S||            ||S||
!
!
            CALL MOVED ( STATE(5), 4, DS )

            CALL VSCLG ( 1.D0          / MAGS,  DS, 4,  SCLDDQ )
            CALL VSCLG ( VDOTG(Q,DS,4) / MAGS,  Q,  4,  RADTRM )

            CALL VSUBG ( SCLDDQ, RADTRM, 4, DQ )
!
!           Derive angular velocity from Q and dQ/dt:
!
            CALL QDQ2AV ( Q, DQ, AV )

!
!           Scale the AV from radians/tick to radians/second.
!
            CALL VSCLIP ( 1.D0/RATE, AV )

         END IF

!
!        Q and if required AV have been assigned.
!


      ELSE IF ( SUBTYP .EQ. C05TP3 ) THEN
!
!        This is the easiest case:  we perform Lagrange interpolation
!        on each quaternion or angular velocity component.
!
!        We'll transpose the pointing information in the input record so
!        that contiguous pieces of it can be shoved directly into the
!        interpolation routine LGRINT.  We allow LGRINT to overwrite
!        the state values in the input record, since this saves local
!        storage and does no harm.  (See the header of LGRINT for a
!        description of its work space usage.)
!
         N   =  NINT( RECORD(CNTIDX) )

         CALL XPSGIP ( PACKSZ, N, RECORD(PKTIDX) )

!
!        We interpolate each state component in turn.
!
         XSTART  =  PKTIDX   +  N * PACKSZ

         IF ( NEEDAV ) THEN
            UB  =  PACKSZ
         ELSE
            UB  =  4
         END IF

         DO I = 1, UB

            YSTART    =   PKTIDX  +  N * (I-1)

            STATE(I)  =   LGRINT ( N, &
                                   RECORD(XSTART), &
                                   RECORD(YSTART), &
                                   LOCREC, &
                                   SCLKDP          )
         END DO

!
!        The output quaternion is a unitized version of the
!        interpolated state.
!
         CALL VHATG ( STATE, 4, Q )


         IF ( NEEDAV ) THEN
!
!           The angular velocity already is in units of radians/second.
!
            CALL VEQU ( STATE(5), AV )

         END IF

!
!        Q and if required AV have been assigned.
!

      ELSE
!
!        We have a Hermite-style subtype.  Whether it's subtype 0
!        or 2, we perform Hermite interpolation on the quaternions.
!
!        We interpolate each quaternion component in turn.  Attitude and
!        angular velocity are interpolated separately.
!
         XSTART   =   PKTIDX  +  PACKSZ * N

         DO I = 1, 4

            DO J = 1, N
!
!              For the Jth input packet, copy the Ith position and
!              velocity components into the local record buffer RECORD.
!
!              In order to perform Hermite interpolation, the
!              quaternions and quaternion derivatives must have a
!              common time scale. So prior to interpolation, we scale
!              the units of the quaternion derivatives from radians/sec
!              to radians/tick.
!
               FROM         = PKTBAS + PACKSZ*(J-1) + I
               TO           =              2 * J    - 1

               LOCREC(TO  ) = RECORD ( FROM     )
               LOCREC(TO+1) = RECORD ( FROM + 4 ) * RATE

            END DO

!
!           Interpolate the Ith quaternion and quaternion derivative
!           components.
!
            CALL HRMINT ( N, &
                          RECORD(XSTART), &
                          LOCREC, &
                          SCLKDP, &
                          WORK, &
                          STATE(I  ), &
                          STATE(I+4)      )

         END DO

!
!        The output quaternion is a unitized version of the
!        interpolated state.
!
         MAGS = VNORMG( STATE, 4 )

         IF ( MAGS .EQ. 0.D0 ) THEN

            CALL SETMSG ( 'Quaternion magnitude at SCLK # was zero.' )
            CALL ERRDP  ( '#',  SCLKDP                               )
            CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                      )
            CALL CHKOUT ( 'CKE05'                                    )
            RETURN

         END IF

         CALL VSCLG ( 1.D0/MAGS, STATE, 4, Q )


         IF ( NEEDAV ) THEN

            IF ( SUBTYP .EQ. C05TP0 ) THEN
!
!              Find the time derivative of the unit quaternion:
!              Letting S represent the quaternion portion of STATE, we
!              have
!
!                 Q = S/||S||
!
!
!              Then letting < , > denote the 4-dimensional inner product
!              operator, we have
!
!
!                            d(S)/dt      < Q, d(S)/dt >
!                 d(Q)/dt =  -------  -   -------------- * Q
!                             ||S||            ||S||
!
!
               CALL MOVED ( STATE(5), 4, DS )

               CALL VSCLG ( 1.D0          / MAGS,  DS, 4,  SCLDDQ )
               CALL VSCLG ( VDOTG(Q,DS,4) / MAGS,  Q,  4,  RADTRM )

               CALL VSUBG ( SCLDDQ, RADTRM, 4, DQ )
!
!              Derive angular velocity from Q and dQ/dt:
!
               CALL QDQ2AV ( Q, DQ, AV )

!
!              Scale the AV from radians/tick to radians/second.
!
               CALL VSCLIP ( 1.D0/RATE, AV )

            ELSE
!
!              This is subtype 2; we perform Hermite interpolation on
!              the angular velocity and its derivative.
!
!              Now interpolate angular velocity, using separate angular
!              velocity data and angular acceleration.
!
               DO I = 1, 3

                  DO J = 1, N
!
!                    For the Jth input packet, copy the Ith position
!                    and velocity components into the local record
!                    buffer LOCREC.  Note that, as with quaternion
!                    derivatives, we must scale angular acceleration
!                    from radians/sec**2 to radians/(sec*tick) before
!                    interpolating.
!
                     FROM         = PKTBAS + PACKSZ*(J-1) +  8  +  I
                     TO           =              2 * J    -  1

                     LOCREC(TO  ) = RECORD ( FROM     )
                     LOCREC(TO+1) = RECORD ( FROM + 3 ) * RATE

                  END DO

!
!                 Interpolate the Ith angular velocity and angular
!                 acceleration components of the attitude. We'll
!                 capture the result in a temporary buffer, then
!                 transfer the velocity to the output argument AV.
!
                  CALL HRMINT ( N, &
                                RECORD(XSTART), &
                                LOCREC, &
                                SCLKDP, &
                                WORK, &
                                VBUFF(I  ), &
                                VBUFF(I+3)     )
               END DO

!
!              Fill in the angular velocity in the output angular
!              velocity vector using the results of interpolating
!              velocity and acceleration.
!
!              The angular velocity is already in units of
!              radians/second.
!
               CALL VEQU ( VBUFF, AV )

            END IF
!
!           We've handled the type 0 and type 2 cases.
!
         END IF
!
!        We've computed the angular velocity AV for the Hermite
!        subtypes, if a.v. was requested.
!
      END IF
!
!     We've handled all four subtypes.
!


!
!     Produce a C-matrix from the interpolated quaternion. Set CLKOUT.
!
      CALL Q2M ( Q, CMAT )

      CLKOUT = RECORD(EPCIDX)


      CALL CHKOUT ( 'CKE05' )
      RETURN
      END
