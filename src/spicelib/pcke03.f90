!$Procedure PCKE03 ( PCK, evaluate data record from type 3 segment )

      SUBROUTINE PCKE03 ( ET, RECORD, ROTMAT )

! $ Abstract
!
!     Evaluate a single PCK data record from a segment of type 03
!     (Variable width Chebyshev Polynomials for RA, DEC, and W) to
!     obtain a state transformation matrix.
!
! $ Required_Reading
!
!     PCK
!
! $ Keywords
!
!     PCK
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION      ET
      DOUBLE PRECISION      RECORD   ( *    )
      DOUBLE PRECISION      ROTMAT   ( 6, 6 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ET         I   Target epoch state transformation.
!     RECORD     I   Data record valid for epoch ET.
!     ROTMAT     O   State transformation matrix at epoch ET.
!
! $ Detailed_Input
!
!     ET       is a target epoch, at which a state transformation
!              matrix is to be calculated.
!
!     RECORD   is a data record which, when evaluated at epoch ET,
!              will give RA, DEC, and W and angular velocity
!              for a body. The RA, DEC and W are relative to
!              some inertial frame. The angular velocity is
!              expressed relative to the body fixed coordinate frame.
!
! $ Detailed_Output
!
!     ROTMAT   is the state transformation matrix at epoch ET.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The exact format and structure of type 03 PCK segments are
!     described in the PCK Required Reading file.
!
!     A type 03 segment contains six sets of Chebyshev coefficients,
!     one set each for RA, DEC, and W and one set each for the
!     components of the angular velocity of the body. The coefficients
!     for RA, DEC, and W are relative to some inertial reference
!     frame. The coefficients for the components of angular velocity
!     are relative to the body fixed frame and must be transformed
!     via the position transformation corresponding to RA, DEC and W.
!
!     PCKE03 calls the routine CHBVAL to evaluate each polynomial,
!     to obtain a complete set of values. These values are then
!     used to determine a state transformation matrix that will
!     rotate an inertially referenced state into the bodyfixed
!     coordinate system.
!
! $ Examples
!
!     The PCKEnn routines are almost always used in conjunction with
!     the corresponding PCKRnn routines, which read the records from
!     binary PCK files.
!
!     The data returned by the PCKRnn routine is in its rawest form,
!     taken directly from the segment. As such, it will be meaningless
!     to a user unless he/she understands the structure of the data type
!     completely. Given that understanding, however, the PCKRnn
!     routines might be used to examine raw segment data before
!     evaluating it with the PCKEnn routines.
!
!
!     C
!     C     Get a segment applicable to a specified body and epoch.
!     C
!           CALL PCKSFS ( BODY, ET, HANDLE, DESCR, IDENT, FOUND )
!
!     C
!     C     Look at parts of the descriptor.
!     C
!           CALL DAFUS ( DESCR, 2, 6, DCD, ICD )
!           TYPE   = ICD( 3 )
!
!           IF ( TYPE .EQ. 03 ) THEN
!
!              CALL PCKR03 ( HANDLE, DESCR, ET, RECORD )
!                  .
!                  .  Look at the RECORD data.
!                  .
!              CALL PCKE03 ( ET, RECORD, ROTMAT )
!                  .
!                  .  Apply the rotation and check out the state.
!                  .
!           END IF
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
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 3.0.2, 20-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 3.0.1, 03-JAN-2014 (EDW)
!
!        Minor edits to $Procedure; clean trailing whitespace.
!        Removed unneeded $Revisions section.
!
!-    SPICELIB Version 3.0.0, 06-OCT-1995 (WLT)
!
!        Brian Carcich at Cornell discovered that the Euler
!        angles were being re-arranged unnecessarily. As a
!        result the state transformation matrix computed was
!        not the one we expected. (The re-arrangement was
!        a left-over from  implementation 1.0.0. This problem
!        has now been corrected.
!
!-    SPICELIB Version 2.0.0, 28-JUL-1995 (WLT)
!
!        Version 1.0.0 was written under the assumption that
!        RA, DEC, W and dRA/dt, dDEC/dt and dW/dt were supplied
!        in the input RECORD. This version repairs the
!        previous misinterpretation.
!
!-    SPICELIB Version 1.0.0, 14-MAR-1995 (KRG)
!
!-&


! $ Index_Entries
!
!     evaluate type_03 PCK segment
!
!-&


!
!     SPICELIB Functions
!
      DOUBLE PRECISION      HALFPI
      DOUBLE PRECISION      RPD

      LOGICAL               RETURN
!
!     Local variables
!
      DOUBLE PRECISION      EULANG ( 6    )
      DOUBLE PRECISION      MAV    ( 3    )
      DOUBLE PRECISION      ROT    ( 3, 3 )
      DOUBLE PRECISION      DROTDT ( 3, 3 )

      INTEGER               NCOEFF
      INTEGER               DEGREE
      INTEGER               COFLOC
      INTEGER               I
      INTEGER               J
!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'PCKE03' )
      END IF

!
!     The first number in the record is the number of Chebyshev
!     Polynomial coefficients used to represent each component of the
!     state vector.  Following it are two numbers that will be used
!     later, then the six sets of coefficients.
!
      NCOEFF = INT( RECORD( 1 ) )
!
!     The degree of each polynomial is one less than the number of
!     coefficients.
!
      DEGREE = NCOEFF - 1
!
!     Call CHBVAL once for each quantity to obtain RA, DEC, and W values
!     as well as values for the angular velocity.
!
!     Note that we stick the angular velocity in the components 4 thru 6
!     of the array EULANG even though they are not derivatives of
!     components 1 thru 3.  It's just simpler to do it this way.
!
!     Editorial Comment:
!
!        Unlike every other SPICE routine, the units for the type 03
!        PCK segment are degrees.  This inconsistency exists solely
!        to support the NEAR project and the intransigence of one of the
!        participants of that project.
!
!        It's a bad design and we know it.
!
!        ---W.L. Taber
!
!
      DO I = 1, 6
!
!        The coefficients for each variable are located contiguously,
!        following the first three words in the record.
!
         COFLOC = NCOEFF * ( I - 1 ) + 4
!
!        CHBVAL needs as input the coefficients, the degree of the
!        polynomial, the epoch, and also two variable transformation
!        parameters, which are located, in our case, in the second and
!        third slots of the record.
!
         CALL CHBVAL ( RECORD( COFLOC ), DEGREE, RECORD( 2 ), ET, &
                       EULANG(I)                                  )
!
!        Convert to radians.
!
         EULANG(I) = RPD() * EULANG(I)

      END DO
!
!     EULANG(1) is RA make it PHI
!     EULANG(2) is DEC make it DELTA
!     EULANG(3) is W
!
      EULANG(1) = HALFPI() + EULANG(1)
      EULANG(2) = HALFPI() - EULANG(2)

!
!     Before we obtain the state transformation matrix, we need to
!     compute the rotation components of the transformation..
!     The rotation we want to perform is:
!
!        [W]  [DELTA]  [PHI]
!           3        1      3
!
!     The array of Euler angles is now:
!
!        EULANG(1) = PHI
!        EULANG(2) = DELTA
!        EULANG(3) = W
!        EULANG(4) = AV_1 (bodyfixed)
!        EULANG(5) = AV_2 (bodyfixed)
!        EULANG(6) = AV_3 (bodyfixed)
!
!
!     Compute the rotation associated with the Euler angles.
!
      CALL EUL2M ( EULANG(3), EULANG(2), EULANG(1), 3, 1, 3, ROT  )

!
!     This rotation transforms positions relative to the inertial
!     frame to positions relative to the bodyfixed frame.
!
!     We next need to get dROT/dt.
!
!     For this discussion let P be the bodyfixed coordinates of
!     a point that is fixed with respect to the bodyfixed frame.
!
!     The velocity of P with respect to the inertial frame is
!     given by
!                 t             t
!        V   = ROT ( AV ) x  ROT ( P )
!
!                  t
!              dROT
!            = ----  ( P )
!               dt
!
!     But
!            t            t            t
!         ROT ( AV ) x ROT ( P ) = ROT  ( AV x P )
!
!     Let OMEGA be the cross product matrix corresponding to AV.
!     Then
!           t                   t
!        ROT  ( AV x P )  =  ROT * OMEGA * P
!
!     where * denotes matrix multiplication.
!
!     From these observations it follows that
!
!                                  t
!           t                  dROT
!        ROT  * OMEGA * P   =  ---- * P
!                                dt
!
!     Consequently, it follows that
!
!        dROT         t
!        ----  = OMEGA  * ROT
!         dt
!
!              = -OMEGA * ROT
!
!     We compute dROT/dt now.  Note that we can get the columns
!     of  -OMEGA*ROT by computing the cross products -AV x COL
!     for each column COL of ROT.
!
      MAV(1) = -EULANG(4)
      MAV(2) = -EULANG(5)
      MAV(3) = -EULANG(6)

      CALL VCRSS ( MAV, ROT(1,1), DROTDT(1,1) )
      CALL VCRSS ( MAV, ROT(1,2), DROTDT(1,2) )
      CALL VCRSS ( MAV, ROT(1,3), DROTDT(1,3) )

!
!     Now we simply fill in the blanks.
!
      DO I = 1, 3
         DO J = 1, 3
            ROTMAT(I,  J  ) = ROT   (I,J)
            ROTMAT(I+3,J  ) = DROTDT(I,J)
            ROTMAT(I,  J+3) = 0.0D0
            ROTMAT(I+3,J+3) = ROT   (I,J)
         END DO
      END DO

      CALL CHKOUT ( 'PCKE03' )
      RETURN
      END
