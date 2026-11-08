!$Procedure VTMV ( Vector transpose times matrix times vector, 3 dim )

      double precision function vtmv ( v1, matrix, v2 )

! $ Abstract
!
!     Multiply the transpose of a 3-dimensional column vector,
!     a 3x3 matrix, and a 3-dimensional column vector.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATRIX
!     VECTOR
!
! $ Declarations

      implicit none

      double precision   v1     (   3 )
      double precision   matrix ( 3,3 )
      double precision   v2     (   3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     V1         I   3-dimensional double precision column vector.
!     MATRIX     I   3x3 double precision matrix.
!     V2         I   3-dimensional double precision column vector.
!
!     The function returns the result of multiplying the transpose of
!     V1 by MATRIX by V2.
!
! $ Detailed_Input
!
!     V1       is any double precision 3-dimensional column vector.
!
!     MATRIX   is any double precision 3x3 matrix.
!
!     V2       is any double precision 3-dimensional column vector.
!
! $ Detailed_Output
!
!     The function returns the double precision value of the equation
!
!          T
!        V1  *  MATRIX * V2
!
!     Notice that VTMV is actually the dot product of the vector
!     resulting from multiplying the transpose of V1 and MATRIX and the
!     vector V2.
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
!     This routine implements the following vector/matrix/vector
!     multiplication:
!
!                 T
!        VTMV = V1  * MATRIX * V2
!
!     V1 is a column vector which becomes a row vector when transposed.
!     V2 is a column vector.
!
!     No checking is performed to determine whether floating point
!     overflow has occurred.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Compute the multiplication of the transpose of a 3-dimensional
!        column vector, a 3x3 matrix, and a second 3-dimensional column
!        vector.
!
!
!        Example code begins here.
!
!
!              PROGRAM VTMV_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              DOUBLE PRECISION      VTMV
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      MATRIX ( 3, 3 )
!              DOUBLE PRECISION      V1     (    3 )
!              DOUBLE PRECISION      V2     (    3 )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define V1, MATRIX and V2.
!        C
!              DATA                  V1      /  2.D0,  4.D0, 6.D0  /
!              DATA                  MATRIX  /  0.D0, -1.D0, 0.D0,
!             .                                 1.D0,  0.D0, 0.D0,
!             .                                 0.D0,  0.D0, 1.D0  /
!              DATA                  V2      /  1.D0,  1.D0, 1.D0  /
!
!
!              WRITE(*,'(A)') 'V1:'
!              DO I = 1, 3
!
!                 WRITE(*,'(F6.1)') V1(I)
!
!              END DO
!
!              WRITE(*,*)
!              WRITE(*,'(A)') 'MATRIX:'
!              DO I = 1, 3
!
!                 WRITE(*,'(3F6.1)') ( MATRIX(I,J), J=1,3 )
!
!              END DO
!
!              WRITE(*,*)
!              WRITE(*,'(A)') 'V2:'
!              DO I = 1, 3
!
!                 WRITE(*,'(F6.1)') V2(I)
!
!              END DO
!
!        C
!        C     Compute the transpose of V1 times MATRIX times V2.
!        C
!              WRITE(*,*)
!              WRITE(*,'(A,F6.1)') 'Transpose of V1 times MATRIX '
!             .                 // 'times V2:', VTMV ( V1, MATRIX, V2 )
!
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        V1:
!           2.0
!           4.0
!           6.0
!
!        MATRIX:
!           0.0   1.0   0.0
!          -1.0   0.0   0.0
!           0.0   0.0   1.0
!
!        V2:
!           1.0
!           1.0
!           1.0
!
!        Transpose of V1 times MATRIX times V2:   4.0
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
!        Edited the header and code to comply with NAIF standard. Added
!        complete code example based on existing example.
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
!     3-dimensional vector_transpose times matrix times vector
!
!-&


!
!     Local variables
!
      integer               k
      integer               l

      vtmv = 0.d0
      do k=1,3
         do l=1,3
            vtmv = vtmv + v1(k) * matrix(k,l) * v2(l)
         end do
      end do

      return
      end
