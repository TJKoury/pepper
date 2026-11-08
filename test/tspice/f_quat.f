C$Procedure F_QUAT ( Quaternion routine tests )

      SUBROUTINE F_QUAT ( OK )

C$ Abstract
C
C     This routine tests a subset of the SPICELIB quaternion routines.
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
C     None.
C
C$ Keywords
C
C     TEST FAMILY
C
C$ Declarations

      IMPLICIT NONE

      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     OK         O   logical indicating test status.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     OK       is a logical that indicates the test status to the
C              caller.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     1)  This routine does not generate any errors. Routines in its
C         call tree may generate errors that are either intentional and
C         trapped or unintentional and need reporting. The test family
C         utilities manage this.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine tests the following routines:
C
C        QDQ2AV
C        QXQ
C
C     Note that Q2M and M2Q are tested elsewhere.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     N.J. Bachman       (JPL)
C     J. Diaz del Rio    (ODC Space)
C     B.V. Semenov       (JPL)
C
C$ Version
C
C-    TSPICE Version 1.2.0, 30-MAY-2021 (BVS) (JDR)
C
C        Added missing 'D' to literal DP constants.
C
C        Edited the header to comply with NAIF standard.
C
C-    TSPICE Version 1.1.1, 21-JAN-2016 (NJB)
C
C        Filled out header. Corrected spelling errors in comments.
C
C-    TSPICE Version 1.1.0, 27-SEP-2005 (NJB)
C
C        Updated to remove non-standard use of duplicate arguments
C        in VSCLG call.
C
C-    TSPICE Version 1.0.0, 25-AUG-2002 (NJB)
C
C-&


C$ Index_Entries
C
C     None.
C
C-&


C
C     Test Utility Functions
C


C
C     SPICELIB Functions
C
      DOUBLE PRECISION      RPD

C
C     Local Parameters
C
      INTEGER               MAXDEG
      PARAMETER           ( MAXDEG = 15 )

      DOUBLE PRECISION      TIGHT
      PARAMETER           ( TIGHT  = 1.D-14 )

      DOUBLE PRECISION      MEDIUM
      PARAMETER           ( MEDIUM = 1.D-12 )

C
C     Local Variables
C
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
C
C     Saved variables
C
      SAVE                  QIDNEG
      SAVE                  QID
      SAVE                  QI
      SAVE                  QJ
      SAVE                  QK

C
C     Initial values
C
      DATA                  QIDNEG / -1.D0, 0.D0, 0.D0, 0.D0 /
      DATA                  QID    /  1.D0, 0.D0, 0.D0, 0.D0 /
      DATA                  QI     /  0.D0, 1.D0, 0.D0, 0.D0 /
      DATA                  QJ     /  0.D0, 0.D0, 1.D0, 0.D0 /
      DATA                  QK     /  0.D0, 0.D0, 0.D0, 1.D0 /

C
C     Begin every test family with an open call.
C
      CALL TOPEN ( 'F_QUAT' )

C
C     QXQ tests follow:
C
      CALL TCASE ( 'QXQ test:  Check compliance with  ' //
     .             'Hamilton''s rules.  Test QI x QJ.'     )


      CALL QXQ ( QI, QJ, QOUT )
      CALL CHCKAD ( 'i*j', QOUT, '~', QK, 4, TIGHT, OK  )


      CALL TCASE ( 'Check compliance with Hamilton''s rules.  ' //
     .             'Test QJ x QK.'                                )

      CALL QXQ ( QJ, QK, QOUT )
      CALL CHCKAD ( 'j*k', QOUT, '~', QI, 4, TIGHT, OK  )


      CALL TCASE ( 'Check compliance with Hamilton''s rules.  ' //
     .             'Test QK x QI.'                                )

      CALL QXQ ( QK, QI, QOUT )
      CALL CHCKAD ( 'k*i', QOUT, '~', QJ, 4, TIGHT, OK  )


      CALL TCASE ( 'Check compliance with Hamilton''s rules.  ' //
     .             'Test QI x QI.'                                )
      CALL QXQ ( QI, QI, QOUT )
      CALL CHCKAD ( 'i*i', QOUT, '~', QIDNEG, 4, TIGHT, OK  )


      CALL TCASE ( 'Check compliance with Hamilton''s rules.  ' //
     .             'Test QJ x QJ.'                                )

      CALL QXQ ( QJ, QJ, QOUT )
      CALL CHCKAD ( 'j*j', QOUT, '~', QIDNEG, 4, TIGHT, OK  )


      CALL TCASE ( 'Check compliance with Hamilton''s rules.  ' //
     .             'Test QK x QK.'                                )

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

C
C     Try a more complex case:  multiply two rotation matrices
C     via quaternion multiplication.
C
      CALL TCASE ( 'Multiply two rotations via quaternion ' //
     .             'multiplication.'                           )


      CALL EUL2M ( RPD() * 20.D0,  RPD() * 10.D0,  RPD() * 70.D0,
     .             3,             1,             3,
     .             M1                                          )


      CALL EUL2M ( RPD() * (-20.D0),  RPD() * 30.D0,  RPD() * (-10.D0),
     .             3,                1,             3,
     .             M2                                               )

      CALL M2Q ( M1, Q1 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL M2Q ( M2, Q2 )
      CALL CHCKXC ( .FALSE., ' ', OK )

      CALL QXQ ( Q1, Q2, QOUT )
      CALL Q2M ( QOUT,   MOUT )

      CALL MXM ( M1, M2, MEXP )

      CALL CHCKAD ( 'MOUT', MOUT, '~', MEXP, 9, 1.D-12, OK )


C
C     QDQ2AV tests follow:
C
      CALL TCASE ( 'Produce quaternion and derivative from Euler ' //
     .             'angles and a.v.  Recover a.v. from QDQ2AV; '   //
     .             'compare to original a.v.'                       )

C
C     Start with a known rotation and angular velocity.  Find
C     the quaternion and quaternion derivative.  The latter is
C     computed from
C
C                        *
C         AV  =   -2  * Q  * DQ
C
C         DQ  =  -1/2 * Q  * AV
C

      ANGLE(1) = -20.D0 * RPD()
      ANGLE(2) =  50.D0 * RPD()
      ANGLE(3) = -60.D0 * RPD()

      CALL EUL2M ( ANGLE(3), ANGLE(2), ANGLE(1), 3, 1, 3, M1 )

      CALL M2Q   ( M1, Q )

      EXPAV(1) =  1.0D0
      EXPAV(2) =  2.0D0
      EXPAV(3) =  3.0D0

C
C     Form the quaternion derivative.
C
      QAV(0)    =  0.D0
      CALL VEQU ( EXPAV, QAV(1) )

      CALL QXQ ( Q, QAV, DQ )

      CALL VSCLG ( -0.5D0, DQ, 4, QTMP )
      CALL MOVED ( QTMP,       4, DQ   )

C
C     Recover angular velocity from Q and DQ.
C
      CALL QDQ2AV ( Q, DQ, AV )

C
C     Do a consistency check against the original a.v.  This is
C     an intermediate check; it demonstrates invertibility but
C     not correctness of our formulas.
C
      CALL CHCKAD ( 'AV from Q and DQ', AV, '~', EXPAV, 3, 1.D-12, OK )

C
C     Convert Q back to a rotation matrix.
C
      CALL Q2M ( Q, M1 )


      CALL TCASE ( 'Map a quaternion and derivative to angular ' //
     .             'velocity via a transformation matrix and '   //
     .             'xf2rav.  Compare to result from QDQ2AV.'     )
C
C     Convert Q and DQ to a rotation derivative matrix.  This
C     somewhat messy procedure is based on differentiating the
C     formula for deriving a rotation from a quaternion, then
C     substituting components of Q and DQ into the derivative
C     formula.
C
      DM(1,1)  =  -4.D0 * (   Q(2)*DQ(2)  +  Q(3)*DQ(3)  )

      DM(1,2)  =   2.D0 * (   Q(1)*DQ(2)  +  Q(2)*DQ(1)
     .                      - Q(0)*DQ(3)  -  Q(3)*DQ(0)  )

      DM(1,3)  =   2.D0 * (   Q(1)*DQ(3)  +  Q(3)*DQ(1)
     .                      + Q(0)*DQ(2)  +  Q(2)*DQ(0)  )

      DM(2,1)  =   2.D0 * (   Q(1)*DQ(2)  +  Q(2)*DQ(1)
     .                      + Q(0)*DQ(3)  +  Q(3)*DQ(0)  )


      DM(2,2)  =  -4.D0 * (   Q(1)*DQ(1)  +  Q(3)*DQ(3)  )


      DM(2,3)  =   2.D0 * (   Q(2)*DQ(3)  +  Q(3)*DQ(2)
     .                      - Q(0)*DQ(1)  -  Q(1)*DQ(0)  )


      DM(3,1)  =   2.D0 * (   Q(3)*DQ(1)  +  Q(1)*DQ(3)
     .                      - Q(0)*DQ(2)  -  Q(2)*DQ(0)  )


      DM(3,2)  =   2.D0 * (   Q(2)*DQ(3)  +  Q(3)*DQ(2)
     .                      + Q(0)*DQ(1)  +  Q(1)*DQ(0)  )


      DM(3,3)  =  -4.D0 * (   Q(1)*DQ(1)  +  Q(2)*DQ(2)  )


C
C     Form the state transformation matrix corresponding to M1
C     and DM.
C
      CALL CLEARD ( 36, XTRANS )

C
C     Upper left block:
C
      DO I = 1, 3

         DO J = 1, 3
            XTRANS(I,J) = M1(I,J)
         END DO

      END DO

C
C     Lower right block:
C
      DO I = 1, 3

         DO J = 1, 3
            XTRANS(3+I,3+J) = M1(I,J)
         END DO

      END DO

C
C     Lower left block:
C
      DO I = 1, 3

         DO J = 1, 3
            XTRANS(3+I,J) = DM(I,J)
         END DO

      END DO


      CALL XF2RAV ( XTRANS, MOUT, AVX )

C
C     Compare the angular velocity obtained from the state
C     transformation with that from QDQ2AV.
C
      CALL CHCKAD ( 'AV from Q and DQ', AV, '~', AVX, 3, 1.D-12, OK )



      CALL T_SUCCESS ( OK )
      RETURN
      END
