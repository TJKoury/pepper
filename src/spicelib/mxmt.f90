!$Procedure MXMT ( Matrix times matrix transpose, 3x3 )

      subroutine mxmt ( m1, m2, mout )

! $ Abstract
!
!     Multiply a 3x3 matrix and the transpose of another 3x3 matrix.
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

      implicit none

      double precision   m1   ( 3,3 )
      double precision   m2   ( 3,3 )
      double precision   mout ( 3,3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M1         I   3x3 double precision matrix.
!     M2         I   3x3 double precision matrix.
!     MOUT       O   The product M1 times transpose of M2.
!
! $ Detailed_Input
!
!     M1       is an arbitrary 3x3 double precision matrix.
!
!     M2       is an arbitrary 3x3 double precision matrix.
!              Typically, M2 will be a rotation matrix since
!              then its transpose is its inverse (but this is
!              NOT a requirement).
!
! $ Detailed_Output
!
!     MOUT     is a 3x3 double precision matrix. MOUT is the product
!
!                               T
!                 MOUT = M1 x M2
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
!           MOUT(I,J) =   )  M1(I,K) * M2(J,K)
!                        /
!                       '-----
!                         K=1
!
!     Note that the reversal of the K and J subscripts in the right-
!     hand matrix M2 is what makes MOUT the product of the TRANSPOSE of
!     M2 and not simply of M2 itself.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given two 3x3 double precision matrices, multiply the first
!        matrix by the transpose of the second one.
!
!
!        Example code begins here.
!
!
!              PROGRAM MXMT_EX1
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
!        C     Define M1.
!        C
!              DATA                  M1   /  0.0D0, -1.0D0,  0.0D0,
!             .                              1.0D0,  0.0D0,  0.0D0,
!             .                              0.0D0,  0.0D0,  1.0D0  /
!
!        C
!        C     Make M2 equal to M1.
!        C
!              CALL MEQU ( M1, M2 )
!
!        C
!        C     Multiply M1 by the transpose of M2.
!        C
!              CALL MXMT ( M1, M2, MOUT )
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
!              WRITE(*,'(A)') 'M1 times transpose of M2:'
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
!               0.0000000       1.0000000       0.0000000
!              -1.0000000       0.0000000       0.0000000
!               0.0000000       0.0000000       1.0000000
!
!        M2:
!               0.0000000       1.0000000       0.0000000
!              -1.0000000       0.0000000       0.0000000
!               0.0000000       0.0000000       1.0000000
!
!        M1 times transpose of M2:
!               1.0000000       0.0000000       0.0000000
!               0.0000000       1.0000000       0.0000000
!               0.0000000       0.0000000       1.0000000
!
!
! $ Restrictions
!
!     1)  The user is responsible for checking the magnitudes of the
!         elements of M1 and M2 so that a floating point overflow does
!         not occur. (In the typical use where M1 and M2 are rotation
!         matrices, this not a risk at all.)
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
!        code examples based on existing code fragments.
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
!     matrix times matrix_transpose 3x3_case
!
!-&


!
!     Local variables
!
      double precision      prodm ( 3, 3 )

      integer               i
      integer               j

!
!  Perform the matrix multiplication
!
      do i=1,3
         do j=1,3
            prodm(i,j) = m1(i,1)*m2(j,1) &
                       + m1(i,2)*m2(j,2) &
                       + m1(i,3)*m2(j,3)
         end do
      end do
!
!  Move the result into MOUT
!
      call moved (prodm,9,mout)

      return
      end
