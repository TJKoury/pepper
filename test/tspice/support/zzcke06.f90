!$Procedure      ZZCKE06 ( C-Kernel, evaluate, type 6 )

      SUBROUTINE ZZCKE06 ( RECORD, QSTATE, CLKOUT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Evaluate a single data record from a type 6 CK segment. The
!     output is expressed as an interpolated unit quaternion and
!     quaternion derivative rather than as a C-matrix and angular
!     velocity vector.
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

      INCLUDE 'ck06.inc'
      INCLUDE 'ckparam.inc'


      DOUBLE PRECISION      RECORD   ( * )
      DOUBLE PRECISION      QSTATE   ( * )
      DOUBLE PRECISION      CLKOUT

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     RECORD    I-O  Data type 6 record.
!     QSTATE     O   Interpolated output record.
!     CLKOUT     O   SCLK associated with input record.
!
! $ Detailed_Input
!
!     RECORD      is a record from a type 6 CK segment which, when
!                 evaluated at the epoch contained in its first
!                 element, will give the attitude and angular velocity
!                 of a spacecraft structure or instrument relative to a
!                 base reference frame.
!
!                 The structure of the record is as follows:
!
!                    +----------------------+
!                    | evaluation epoch     |
!                    +----------------------+
!                    | subtype code         |
!                    +----------------------+
!                    | number of packets (n)|
!                    +----------------------+
!                    | nominal SCLK rate    |
!                    +----------------------+
!                    | packet 1             |
!                    +----------------------+
!                    | packet 2             |
!                    +----------------------+
!                             .
!                             .
!                             .
!                    +----------------------+
!                    | packet n             |
!                    +----------------------+
!                    | epochs 1--n          |
!                    +----------------------+
!
!                See the CK Required Reading or the include file
!                ck06.inc for details on CK type 6 packet contents.
!
!
! $ Detailed_Output
!
!     RECORD     has been modified due to its use as a workspace array.
!                The contents are undefined.
!
!     QSTATE     is an interpolated output record, represented as a unit
!                quaternion and its derivative with respect to time.
!
!     CLKOUT     is the encoded SCLK associated with the returned
!                C-matrix and angular velocity vector.
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
!         quaternion in the record, the error SPICE(BADQUATSIGN)
!         is signaled.
!
!         For subtypes 1 and 3, this condition is not considered an
!         error: the closer to the preceding quaternion of the two
!         quaternion representations is used for interpolation.
!
!     3)  If a zero-magnitude quaternion is produced as a result
!         of interpolating quaternions, the error SPICE(DIVIDEBYZERO)
!         is signaled.
!
!     4)  If the input record contains a non-positive SCLK rate value,
!         the error SPICE(INVALIDSCLKRATE) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine returns quaternion states that, presuming validity
!     of the input record, are suitable for interpolation, unlike
!     quaternion states obtained by calling CKE06 and then M2Q. The
!     latter method of obtaining quaternions is subject to branch
!     singularities.
!
!     The exact format and structure of CK type 6 (MEX/Rosetta Attitude
!     file interpolation) CK segments is described in the CK Required
!     Reading.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     1)  This routine performs minimal error checking. The input data
!         are assumed to have been checked when the source CK file was
!         created.
!
!     2)  With the exception of the check described in item 2 of
!         the Exceptions section above, the input data are assumed to
!         be suitable for the interpolation method specified by the
!         input record's subtype and packet count (which implies an
!         interpolating polynomial degree).
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
!-    SPICELIB Version 1.0.0, 10-AUG-2015 (NJB) (BVS)
!
!-&


! $ Index_Entries
!
!     evaluate type_6 ck_segment
!
!-&

! $ Revisions
!
!     None.
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
      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      DQ     ( 0 : 3 )
      DOUBLE PRECISION      DS     ( 0 : 3 )
      DOUBLE PRECISION      LOCREC ( CKMRSZ )
      DOUBLE PRECISION      MAGS
      DOUBLE PRECISION      Q      ( 0 : 3 )
      DOUBLE PRECISION      QAV    ( 0 : 3 )
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
      INTEGER               PKTSZS ( 0 : C06NST-1 )
      INTEGER               PRVPTR
      INTEGER               SUBTYP
      INTEGER               TO
      INTEGER               XSTART
      INTEGER               YSTART

!
!     Saved variables
!
      SAVE                  PKTSZS

!
!     Initial values
!
      DATA                  PKTSZS / C06PS0, C06PS1, C06PS2, C06PS3 /

!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      END IF

      CALL CHKIN ( 'ZZCKE06' )

!
!     Transfer the input record's epoch to the output epoch.
!
      CLKOUT = RECORD(1)

!
!     Capture the subtype from the record and set the packet size
!     accordingly.
!
      SUBTYP =  NINT( RECORD(SBTIDX) )


      IF (  ( SUBTYP .LT. 0 ) .OR. ( SUBTYP .GE. C06NST)  )  THEN

         CALL SETMSG ( 'Unexpected CK type 6 subtype # found in ' &
         //            'type 6 segment.'                          )
         CALL ERRINT ( '#',  SUBTYP                               )
         CALL SIGERR ( 'SPICE(NOTSUPPORTED)'                      )
         CALL CHKOUT ( 'ZZCKE06'                                  )
         RETURN

      ELSE

         PACKSZ = PKTSZS( SUBTYP )

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

      IF ( RATE .LE. 0.D0 ) THEN

         CALL SETMSG ( 'SCLK rate is #; rate must be positive.' )
         CALL ERRDP  ( '#', RATE                                )
         CALL SIGERR ( 'SPICE(INVALIDSCLKRATE)'                 )
         CALL CHKOUT ( 'ZZCKE06'                                )
         RETURN

      END IF

!
!     Adjust quaternion "signs" as necessary to minimize distance
!     between successive quaternions. This adjustment is performed
!     only for subtypes that don't store quaternion derivatives
!     (these are the Lagrange subtypes).
!
      IF (  ( SUBTYP .EQ. C06TP1 ) .OR. ( SUBTYP .EQ. C06TP3 )  ) THEN
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
!           For the Hermite subtypes, it's an error for the current
!           quaternion to be closer to QNEG than to the previous
!           quaternion.
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
               CALL CHKOUT ( 'ZZCKE06'                                 )
               RETURN

            END IF

            PRVPTR = NEWPTR

         END DO

      END IF



      IF ( SUBTYP .EQ. C06TP1 ) THEN
!
!        We perform Lagrange interpolation on each quaternion
!        component, and obtain quaternion derivatives from the
!        interpolating polynomials.
!
!        We'll transpose the pointing information in the input record so
!        that contiguous pieces of it can be shoved directly into the
!        interpolation routine LGRIND.
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
            CALL CHKOUT ( 'ZZCKE06'                                  )
            RETURN

         END IF

         CALL VSCLG ( 1.D0/MAGS, STATE, 4, Q )

!
!        Find the time derivative of the unit quaternion:
!        Letting S represent the quaternion portion of STATE, we
!        have
!
!           Q = S/||S||
!
!
!        Then letting < , > denote the 4-dimensional inner product
!        operator, we have
!
!
!                      d(S)/dt      < Q, d(S)/dt >
!           d(Q)/dt =  -------  -   -------------- * Q
!                       ||S||            ||S||
!
!
         CALL MOVED ( STATE(5), 4, DS )

         CALL VSCLG ( 1.D0          / MAGS,  DS, 4,  SCLDDQ )
         CALL VSCLG ( VDOTG(Q,DS,4) / MAGS,  Q,  4,  RADTRM )

         CALL VSUBG ( SCLDDQ, RADTRM, 4, DQ )

!
!        Scale the derivative from 1/tick to 1/second.
!
         CALL VSCLG ( 1.D0/RATE,  DQ,  4,  SCLDDQ )

         CALL MOVED ( Q,      4, QSTATE(1) )
         CALL MOVED ( SCLDDQ, 4, QSTATE(5) )



      ELSE IF ( SUBTYP .EQ. C06TP3 ) THEN
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

         DO I = 1, PACKSZ

            YSTART    =   PKTIDX  +  N * (I-1)

            STATE(I)  =   LGRINT ( N, &
                                   RECORD(XSTART), &
                                   RECORD(YSTART), &
                                   LOCREC, &
                                   SCLKDP          )
         END DO


         MAGS = VNORMG( STATE, 4 )

         IF ( MAGS .EQ. 0.D0 ) THEN

            CALL SETMSG ( 'Quaternion magnitude at SCLK # was zero.' )
            CALL ERRDP  ( '#',  SCLKDP                               )
            CALL SIGERR ( 'SPICE(DIVIDEBYZERO)'                      )
            CALL CHKOUT ( 'ZZCKE06'                                  )
            RETURN

         END IF

         CALL VSCLG ( 1.D0/MAGS, STATE, 4, Q )

!
!        The angular velocity already is in units of radians/second.
!
         CALL VEQU ( STATE(5), AV )

!
!        Convert AV to a quaternion derivative. We have from
!        the header of QDQ2AV
!
!                       *
!           AV =  -2 * Q  * DQ
!
!        so
!
!           DQ =  -1/2 * Q * AV
!
!
         CALL VSCLIP ( -0.5D0, AV )

         QAV(0) = 0.D0
         CALL VEQU ( AV, QAV(1) )

         CALL QXQ ( Q, QAV, DQ )


         CALL MOVED ( Q,  4, QSTATE    )
         CALL MOVED ( DQ, 4, QSTATE(5) )

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
            CALL CHKOUT ( 'ZZCKE06'                                  )
            RETURN

         END IF

         CALL VSCLG ( 1.D0/MAGS, STATE, 4, Q )



         IF ( SUBTYP .EQ. C06TP0 ) THEN
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
!           Scale the derivative from radians/tick to
!           radians/second.
!
            CALL VSCLG ( 1.D0/RATE, DQ, 4, SCLDDQ )

!
!           Store Q and DQ in QSTATE. In the process,
!
            CALL MOVED ( Q,      4, QSTATE    )
            CALL MOVED ( SCLDDQ, 4, QSTATE(5) )


         ELSE
!
!           This is subtype 2; we perform Hermite interpolation on
!           the angular velocity and its derivative.
!
!           Now interpolate angular velocity, using separate angular
!           velocity data and angular acceleration.
!
            DO I = 1, 3

               DO J = 1, N
!
!                 For the Jth input packet, copy the Ith angular
!                 velocity and angular acceleration components into the
!                 local record buffer LOCREC.  Note that, as with
!                 quaternion derivatives, we must scale angular
!                 acceleration from radians/sec**2 to
!                 radians/(sec*tick) before interpolating. We would
!                 need to scale the angular acceleration to
!                 radians/sec**2 for output, if we were returning this
!                 quantity. However, we're returning only angular
!                 velocity, which is already in the correct units of
!                 radians/second.
!
                  FROM         = PKTBAS + PACKSZ*(J-1) +  8  +  I
                  TO           =              2 * J    -  1

                  LOCREC(TO  ) = RECORD ( FROM     )
                  LOCREC(TO+1) = RECORD ( FROM + 3 ) * RATE

               END DO

!
!              Interpolate the Ith angular velocity and angular
!              acceleration components of the attitude. We'll
!              capture the result in a temporary buffer, then
!              transfer the velocity to the output argument AV.
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
!           Fill in the angular velocity in the output angular
!           velocity vector using the results of interpolating
!           velocity and acceleration.
!
!           The angular velocity is already in units of
!           radians/second.
!
            CALL VEQU ( VBUFF, AV )
!
!           Convert AV to a quaternion derivative. We have from
!           the header of QDQ2AV
!
!                          *
!              AV =  -2 * Q  * DQ
!
!           so
!
!              DQ =  -1/2 * Q * AV
!
!
            CALL VSCLIP ( -0.5D0, AV )

            QAV(0) = 0.D0
            CALL VEQU ( AV, QAV(1) )

            CALL QXQ ( Q, QAV, DQ )

            CALL MOVED ( Q,  4, QSTATE    )
            CALL MOVED ( DQ, 4, QSTATE(5) )

         END IF
!
!        We've handled the type 0 and type 2 cases.
!
!
!        We've computed the angular velocity AV for the Hermite
!        subtypes, if a.v. was requested.
!
      END IF
!
!     We've handled all four subtypes.
!

      CALL CHKOUT ( 'ZZCKE06' )
      RETURN
      END

