!$Procedure MTXM  ( Matrix transpose times matrix, 3x3 )

      subroutine mtxm ( m1, m2, mout )

! $ Abstract
!
!     Multiply the transpose of a 3x3 matrix and a 3x3 matrix.
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
!     MOUT       O   3x3 double precision matrix which is the product
!                    (M1**T) * M2.
!
! $ Detailed_Input
!
!     M1       is any 3x3 double precision matrix. Typically,
!              M1 will be a rotation matrix since then its
!              transpose is its inverse (but this is NOT a
!              requirement).
!
!     M2       is any 3x3 double precision matrix.
!
! $ Detailed_Output
!
!     MOUT     is s 3x3 double precision matrix. MOUT is the
!              product MOUT = (M1**T) x M2.
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
!        MOUT(I,J) = Summation from K=1 to 3 of  ( M1(K,I) * M2(K,J) )
!
!     Note that the reversal of the K and I subscripts in the left-hand
!     matrix M1 is what makes MOUT the product of the TRANSPOSE of M1
!     and not simply of M1 itself.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given two 3x3 matrices, multiply the transpose of the first
!        matrix by the second one.
!
!
!        Example code begins here.
!
!
!              PROGRAM MTXM_EX1
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
!              DATA                  M1 /  1.0D0,  4.0D0,  7.0D0,
!             .                            2.0D0,  5.0D0,  8.0D0,
!             .                            3.0D0,  6.0D0,  9.0D0  /
!
!              DATA                  M2 /  1.0D0, -1.0D0,  0.0D0,
!             .                            1.0D0,  1.0D0,  0.0D0,
!             .                            0.0D0,  0.0D0,  1.0D0  /
!
!        C
!        C     Multiply the transpose of M1 by M2.
!        C
!              CALL MTXM ( M1, M2, MOUT )
!
!              WRITE(*,'(A)') 'Transpose of M1 times M2:'
!              DO I = 1, 3
!
!                 WRITE(*,'(3F10.3)') ( MOUT(I,J), J=1,3)
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
!        Transpose of M1 times M2:
!            -3.000     5.000     7.000
!            -3.000     7.000     8.000
!            -3.000     9.000     9.000
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
!-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example based on the existing example.
!
!-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
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
!     matrix_transpose times matrix 3x3_case
!
!-&


!
!     Local variables
!
      double precision      prodm(3,3)

      integer               i
      integer               j

!
!  Perform the matrix multiplication
!
      do   i=1,3
         do   j=1,3
            prodm(i,j) = m1(1,i)*m2(1,j) &
                       + m1(2,i)*m2(2,j) &
                       + m1(3,i)*m2(3,j)
         end do
      end do

!
!  Move the result into MOUT
!
      call moved ( prodm, 9, mout )

      return
      end
