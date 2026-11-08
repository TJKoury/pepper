!$Procedure MXM ( Matrix times matrix, 3x3 )

      SUBROUTINE MXM ( M1, M2, MOUT )

! $ Abstract
!
!     Multiply two 3x3 matrices.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATRIX
!
! $ Declarations

      IMPLICIT NONE

      DOUBLE PRECISION   M1   ( 3,3 )
      DOUBLE PRECISION   M2   ( 3,3 )
      DOUBLE PRECISION   MOUT ( 3,3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M1         I   3x3 double precision matrix.
!     M2         I   3x3 double precision matrix.
!     MOUT       O   The 3x3 double precision matrix product M1*M2.
!
! $ Detailed_Input
!
!     M1       is an arbitrary 3x3 double precision matrix.
!
!     M2       is an arbitrary 3x3 double precision matrix.
!
! $ Detailed_Output
!
!     MOUT     is a 3x3 double precision matrix. MOUT is the product
!              M1*M2.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The code reflects precisely the following mathematical expression
!
!        For each value of the subscripts I and J from 1 to 3:
!
!                          3
!                       .-----
!                        \
!           MOUT(I,J) =   )  M1(I,K) * M2(K,J)
!                        /
!                       '-----
!                         K=1
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given two 3x3 double precision matrices, compute their
!        product.
!
!
!        Example code begins here.
!
!
!              PROGRAM MXM_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      M1   ( 3, 3 )
!              DOUBLE PRECISION      M2   ( 3, 3 )
!              DOUBLE PRECISION      MOUT ( 3, 3 )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define M1 and M2.
!        C
!              DATA                  M1   /  1.0D0, -1.0D0,  0.0D0,
!             .                              1.0D0,  1.0D0,  0.0D0,
!             .                              0.0D0,  0.0D0,  1.0D0  /
!
!              DATA                  M2   /  1.0D0,  0.0D0,  0.0D0,
!             .                              0.0D0,  1.0D0, -1.0D0,
!             .                              0.0D0,  1.0D0,  1.0D0  /
!
!        C
!        C     Compute M1 times M2.
!        C
!              CALL MXM ( M1, M2, MOUT )
!
!              WRITE(*,'(A)') 'M1:'
!              DO I=1, 3
!
!                 WRITE(*,'(3F16.7)') ( M1(I,J), J=1,3 )
!
!              END DO
!
!              WRITE(*,*)
!              WRITE(*,'(A)') 'M2:'
!              DO I=1, 3
!
!                 WRITE(*,'(3F16.7)') ( M2(I,J), J=1,3 )
!
!              END DO
!
!              WRITE(*,*)
!              WRITE(*,'(A)') 'M1 times M2:'
!              DO I=1, 3
!
!                 WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 )
!
!              END DO
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        M1:
!               1.0000000       1.0000000       0.0000000
!              -1.0000000       1.0000000       0.0000000
!               0.0000000       0.0000000       1.0000000
!
!        M2:
!               1.0000000       0.0000000       0.0000000
!               0.0000000       1.0000000       1.0000000
!               0.0000000      -1.0000000       1.0000000
!
!        M1 times M2:
!               1.0000000       1.0000000       1.0000000
!              -1.0000000       1.0000000       1.0000000
!               0.0000000      -1.0000000       1.0000000
!
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
!     W.M. Owen          (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing example.
!
!-    SPICELIB Version 1.0.2, 22-APR-2010 (NJB)
!
!        Header correction: assertions that the output
!        can overwrite the input have been removed.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
!
!-&


! $ Index_Entries
!
!     matrix times matrix 3x3_case
!
!-&


!
!     Local variables
!
      DOUBLE PRECISION      PRODM( 3, 3 )

      INTEGER               I
      INTEGER               J

!
!  Perform the matrix multiplication
!
      DO I=1,3
         DO J=1,3
            PRODM(I,J) = M1(I,1)*M2(1,J) &
                       + M1(I,2)*M2(2,J) &
                       + M1(I,3)*M2(3,J)
         END DO
      END DO
!
!  Move the result into MOUT
!
      CALL MOVED (PRODM,9,MOUT)

      RETURN
      END
