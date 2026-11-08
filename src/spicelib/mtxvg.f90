!$Procedure MTXVG ( Matrix transpose times vector, general dimension )

      subroutine mtxvg ( m1, v2, nc1, nr1r2, vout )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Multiply the transpose of a matrix and a vector of
!     arbitrary size.
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

      integer(ip)        nc1
      integer(ip)        nr1r2
      real(wp)           m1   ( nr1r2,nc1 )
      real(wp)           v2   ( nr1r2     )
      real(wp)           vout (       nc1 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M1         I   Left-hand matrix whose transpose is to be
!                    multiplied.
!     V2         I   Right-hand vector to be multiplied.
!     NC1        I   Column dimension of M1 and length of VOUT.
!     NR1R2      I   Row dimension of M1 and length of V2.
!     VOUT       O   Product vector M1**T * V2.
!
! $ Detailed_Input
!
!     M1       is a double precision matrix of arbitrary size whose
!              transpose forms the left-hand matrix of the
!              multiplication.
!
!     V2       is a double precision vector on the right of the
!              multiplication.
!
!     NC1      is the column dimension of M1 and length of VOUT.
!
!     NR1R2    is the row dimension of M1 and length of V2.
!
! $ Detailed_Output
!
!     VOUT     is the double precision vector which results from
!              the expression
!
!                            T
!                 VOUT = (M1)  x V2
!
!              where the T denotes the transpose of M1.
!
!              VOUT must NOT overwrite either M1 or V2.
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
!     For each value of the subscript I from 1 to NC1,
!
!     VOUT(I) = Summation from K=1 to NR1R2 of  ( M1(K,I) * V2(K) )
!
!     Note that the reversal of the K and I subscripts in the left-hand
!     matrix M1 is what makes VOUT the product of the TRANSPOSE of M1
!     and not simply of M1 itself.
!
!     Since this subroutine operates on matrices of arbitrary size, it
!     is not feasible to buffer intermediate results. Thus, VOUT
!     should NOT overwrite either M1 or V2.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a 3x2 matrix and a 3-vector, multiply the transpose of
!        the matrix by the vector.
!
!
!        Example code begins here.
!
!
!              PROGRAM MTXVG_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      M    ( 3, 2 )
!              DOUBLE PRECISION      VIN  ( 3    )
!              DOUBLE PRECISION      VOUT ( 2    )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define M and VIN.
!        C
!              DATA                  M    /  1.0D0,  1.0D0,  1.0D0,
!             .                              2.0D0,  3.0D0,  4.0D0  /
!
!              DATA                  VIN  /  1.0D0,  2.0D0,  3.0D0  /
!
!        C
!        C     Multiply the transpose of M by VIN.
!        C
!              CALL MTXVG ( M, VIN, 2, 3, VOUT )
!
!              WRITE(*,'(A)') 'Transpose of M times VIN:'
!              WRITE(*,'(2F10.3)') VOUT
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Transpose of M times VIN:
!             6.000    20.000
!
!
! $ Restrictions
!
!     1)  The user is responsible for checking the magnitudes of the
!         elements of M1 and V2 so that a floating point overflow does
!         not occur.
!
!     2)  VOUT not overwrite M1 or V2 or else the intermediate
!         will affect the final result.
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
!-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example based on the existing example.
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
!     matrix_transpose times n-dimensional vector
!
!-&


!
!     Local variables
!
      real(wp) sum
      integer(ip)      i,k


!
!  Perform the matrix-vector multiplication
!
      do i = 1, nc1

         sum = 0.0_wp

         do k = 1, nr1r2
            sum = sum + m1(k,i)*v2(k)
         end do

         vout(i) = sum

      end do

      return
      end
