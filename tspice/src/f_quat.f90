!$Procedure F_QUAT ( Quaternion routine tests )

      SUBROUTINE F_QUAT ( OK )

! $ Abstract
!
!     This routine tests a subset of the SPICELIB quaternion routines.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TEST FAMILY
!
! $ Declarations

      IMPLICIT NONE

      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     OK         O   logical indicating test status.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     OK       is a logical that indicates the test status to the
!              caller.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  This routine does not generate any errors. Routines in its
!         call tree may generate errors that are either intentional and
!         trapped or unintentional and need reporting. The test family
!         utilities manage this.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine tests the following routines:
!
!        QDQ2AV
!        QXQ
!
!     Note that Q2M and M2Q are tested elsewhere.
!
! $ Examples
!
!     None.
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
!-    TSPICE Version 1.2.0, 30-MAY-2021 (BVS) (JDR)
!
!        Added missing 'D' to literal DP constants.
!
!        Edited the header to comply with NAIF standard.
!
!-    TSPICE Version 1.1.1, 21-JAN-2016 (NJB)
!
!        Filled out header. Corrected spelling errors in comments.
!
!-    TSPICE Version 1.1.0, 27-SEP-2005 (NJB)
!
!        Updated to remove non-standard use of duplicate arguments
!        in VSCLG call.
!
!-    TSPICE Version 1.0.0, 25-AUG-2002 (NJB)
!
!-&


! $ Index_Entries
!
!     None.
!
!-&


!
!     Test Utility Functions
!


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      RPD

!
!     Local Parameters
!
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-12 )

!
!     Local Variables
!
      DOUBLE PRECISION      ANGLE  ( 3 )
      DOUBLE PRECISION      AV     ( 3 )
      DOUBLE PRECISION      AVX    ( 3 )
      DOUBLE PRECISION      DM     ( 3,  3 )
      DOUBLE PRECISION      DQ     ( 0 : 3 )
      DOUBLE PRECISION      EXPAV  ( 3 )
      DOUBLE PRECISION      M1     ( 3,  3 )
      DOUBLE PRECISION      M2     ( 3,  3 )
      DOUBLE PRECISION      MEXP   ( 3,  3 )
      DOUBLE PRECISION      MOUT   ( 3,  3 )
      DOUBLE PRECISION      Q      ( 0 : 3 )
      DOUBLE PRECISION      Q1     ( 0 : 3 )
      DOUBLE PRECISION      Q2     ( 0 : 3 )
      DOUBLE PRECISION      QAV    ( 0 : 3 )
      DOUBLE PRECISION      QEXP   ( 0 : 3 )
      DOUBLE PRECISION      QID    ( 0 : 3 )
      DOUBLE PRECISION      QIDNEG ( 0 : 3 )
      DOUBLE PRECISION      QI     ( 0 : 3 )
      DOUBLE PRECISION      QJ     ( 0 : 3 )
      DOUBLE PRECISION      QK     ( 0 : 3 )
      DOUBLE PRECISION      QOUT   ( 0 : 3 )
      DOUBLE PRECISION      QTMP   ( 0 : 3 )
      DOUBLE PRECISION      XTRANS ( 6,  6 )

      INTEGER               I
      INTEGER               J
!
!     Saved variables
!
      SAVE                  QIDNEG
      SAVE                  QID
      SAVE                  QI
      SAVE                  QJ
      SAVE                  QK

!
!     Initial values
!
      DATA                  QIDNEG / -1.D0, 0.D0, 0.D0, 0.D0 /
      DATA                  QID    /  1.D0, 0.D0, 0.D0, 0.D0 /
      DATA                  QI     /  0.D0, 1.D0, 0.D0, 0.D0 /
      DATA                  QJ     /  0.D0, 0.D0, 1.D0, 0.D0 /
      DATA                  QK     /  0.D0, 0.D0, 0.D0, 1.D0 /

!
!     Begin every test family with an open call.
!
      CALL TOPEN ( 'F_QUAT' )

!
!     QXQ tests follow:
!
      CALL TCASE ( 'QXQ test:  Check compliance with  ' // &
                   'Hamilton''s rules.  Test QI x QJ.'     )


      CALL QXQ ( QI, QJ, QOUT )
      CALL CHCKAD ( 'i*j', QOUT, '~', QK, 4, TIGHT, OK  )


      CALL TCASE ( 'Check compliance with Hamilton''s rules.  ' // &
                   'Test QJ x QK.'                                )

      CALL QXQ ( QJ, QK, QOUT )
      CALL CHCKAD ( 'j*k', QOUT, '~', QI, 4, TIGHT, OK  )


      CALL TCASE ( 'Check compliance with Hamilton''s rules.  ' // &
                   'Test QK x QI.'                                )

      CALL QXQ ( QK, QI, QOUT )
      CALL CHCKAD ( 'k*i', QOUT, '~', QJ, 4, TIGHT, OK  )


      CALL TCASE ( 'Check compliance with Hamilton''s rules.  ' // &
                   'Test QI x QI.'                                )
      CALL QXQ ( QI, QI, QOUT )
      CALL CHCKAD ( 'i*i', QOUT, '~', QIDNEG, 4, TIGHT, OK  )


      CALL TCASE ( 'Check compliance with Hamilton''s rules.  ' // &
                   'Test QJ x QJ.'                                )

      CALL QXQ ( QJ, QJ, QOUT )
      CALL CHCKAD ( 'j*j', QOUT, '~', QIDNEG, 4, TIGHT, OK  )


      CALL TCASE ( 'Check compliance with Hamilton''s rules.  ' // &
                   'Test QK x QK.'                                )

      CALL QXQ ( QK, QK, QOUT )
      CALL CHCKAD ( 'k*k', QOUT, '~', QIDNEG, 4, TIGHT, OK  )

      QEXP(0) = 1.D0
      QEXP(1) = 2.D0
      QEXP(2) = 3.D0
      QEXP(3) = 4.D0


      CALL TCASE ( 'Check right-multiplication by the identity.' )
      CALL QXQ ( QEXP, QID, QOUT )
      CALL CHCKAD ( 'QEXP * 1', QOUT, '~', QEXP, 4, TIGHT, OK  )


      CALL TCASE ( 'Check left-multiplication by the identity.' )
      CALL QXQ ( QID, QEXP, QOUT )
      CALL CHCKAD ( '1 * QEXP', QOUT, '~', QEXP, 4, TIGHT, OK  )

!
!     Try a more complex case:  multiply two rotation matrices
!     via quaternion multiplication.
!
      CALL TCASE ( 'Multiply two rotations via quaternion ' // &
                   'multiplication.'                           )


      CALL EUL2M ( RPD() * 20.D0,  RPD() * 10.D0,  RPD() * 70.D0, &
                   3,             1,             3, &
                   M1                                          )


      CALL EUL2M ( RPD() * (-20.D0),  RPD() * 30.D0,  RPD() * (-10.D0), &
                   3,                1,             3, &
                   M2                                               )

      CALL M2Q ( M1, Q1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL M2Q ( M2, Q2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL QXQ ( Q1, Q2, QOUT )
      CALL Q2M ( QOUT,   MOUT )

      CALL MXM ( M1, M2, MEXP )

      CALL CHCKAD ( 'MOUT', MOUT, '~', MEXP, 9, 1.D-12, OK )


!
!     QDQ2AV tests follow:
!
      CALL TCASE ( 'Produce quaternion and derivative from Euler ' // &
                   'angles and a.v.  Recover a.v. from QDQ2AV; '   // &
                   'compare to original a.v.'                       )

!
!     Start with a known rotation and angular velocity.  Find
!     the quaternion and quaternion derivative.  The latter is
!     computed from
!
!                        *
!         AV  =   -2  * Q  * DQ
!
!         DQ  =  -1/2 * Q  * AV
!

      ANGLE(1) = -20.D0 * RPD()
      ANGLE(2) =  50.D0 * RPD()
      ANGLE(3) = -60.D0 * RPD()

      CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 3, 1, 3, M1 )

      CALL M2Q   ( M1, Q )

      EXPAV(1) =  1.0D0
      EXPAV(2) =  2.0D0
      EXPAV(3) =  3.0D0

!
!     Form the quaternion derivative.
!
      QAV(0)    =  0.D0
      CALL VEQU ( EXPAV, QAV(1) )

      CALL QXQ ( Q, QAV, DQ )

      CALL VSCLG ( -0.5D0, DQ, 4, QTMP )
      CALL MOVED ( QTMP,       4, DQ   )

!
!     Recover angular velocity from Q and DQ.
!
      CALL QDQ2AV ( Q, DQ, AV )

!
!     Do a consistency check against the original a.v.  This is
!     an intermediate check; it demonstrates invertibility but
!     not correctness of our formulas.
!
      CALL CHCKAD ( 'AV from Q and DQ', AV, '~', EXPAV, 3, 1.D-12, OK )

!
!     Convert Q back to a rotation matrix.
!
      CALL Q2M ( Q, M1 )


      CALL TCASE ( 'Map a quaternion and derivative to angular ' // &
                   'velocity via a transformation matrix and '   // &
                   'xf2rav.  Compare to result from QDQ2AV.'     )
!
!     Convert Q and DQ to a rotation derivative matrix.  This
!     somewhat messy procedure is based on differentiating the
!     formula for deriving a rotation from a quaternion, then
!     substituting components of Q and DQ into the derivative
!     formula.
!
      DM(1,1)  =  -4.D0 * (   Q(2)*DQ(2)  +  Q(3)*DQ(3)  )

      DM(1,2)  =   2.D0 * (   Q(1)*DQ(2)  +  Q(2)*DQ(1) &
                            - Q(0)*DQ(3)  -  Q(3)*DQ(0)  )

      DM(1,3)  =   2.D0 * (   Q(1)*DQ(3)  +  Q(3)*DQ(1) &
                            + Q(0)*DQ(2)  +  Q(2)*DQ(0)  )

      DM(2,1)  =   2.D0 * (   Q(1)*DQ(2)  +  Q(2)*DQ(1) &
                            + Q(0)*DQ(3)  +  Q(3)*DQ(0)  )


      DM(2,2)  =  -4.D0 * (   Q(1)*DQ(1)  +  Q(3)*DQ(3)  )


      DM(2,3)  =   2.D0 * (   Q(2)*DQ(3)  +  Q(3)*DQ(2) &
                            - Q(0)*DQ(1)  -  Q(1)*DQ(0)  )


      DM(3,1)  =   2.D0 * (   Q(3)*DQ(1)  +  Q(1)*DQ(3) &
                            - Q(0)*DQ(2)  -  Q(2)*DQ(0)  )


      DM(3,2)  =   2.D0 * (   Q(2)*DQ(3)  +  Q(3)*DQ(2) &
                            + Q(0)*DQ(1)  +  Q(1)*DQ(0)  )


      DM(3,3)  =  -4.D0 * (   Q(1)*DQ(1)  +  Q(2)*DQ(2)  )


!
!     Form the state transformation matrix corresponding to M1
!     and DM.
!
      CALL CLEARD ( 36, XTRANS )

!
!     Upper left block:
!
      DO I = 1, 3

         DO J = 1, 3
            XTRANS(I,J) = M1(I,J)
         END DO

      END DO

!
!     Lower right block:
!
      DO I = 1, 3

         DO J = 1, 3
            XTRANS(3+I,3+J) = M1(I,J)
         END DO

      END DO

!
!     Lower left block:
!
      DO I = 1, 3

         DO J = 1, 3
            XTRANS(3+I,J) = DM(I,J)
         END DO

      END DO


      CALL XF2RAV ( XTRANS, MOUT, AVX )

!
!     Compare the angular velocity obtained from the state
!     transformation with that from QDQ2AV.
!
      CALL CHCKAD ( 'AV from Q and DQ', AV, '~', AVX, 3, 1.D-12, OK )



      CALL T_SUCCESS ( OK )
      RETURN
      END
