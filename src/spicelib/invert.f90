!$Procedure INVERT ( Invert a 3x3 matrix )

      subroutine invert ( m, mout )

! $ Abstract
!
!     Generate the inverse of a 3x3 matrix.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     MATH
!     MATRIX
!
! $ Declarations

      implicit none

      double precision   m    ( 3,3 )
      double precision   mout ( 3,3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M          I   Matrix to be inverted.
!     MOUT       O   Inverted matrix (M)**-1. If M is singular, then
!                    MOUT will be the zero matrix.
!
! $ Detailed_Input
!
!     M        is an arbitrary 3x3 matrix. The limits on the size of
!              elements of M are determined by the process of
!              calculating the cofactors of each element of the matrix.
!              For a 3x3 matrix this amounts to the differencing of two
!              terms, each of which consists of the multiplication of
!              two matrix elements. This multiplication must not exceed
!              the range of double precision numbers or else an overflow
!              error will occur.
!
! $ Detailed_Output
!
!     MOUT     is the inverse of M and is calculated explicitly using
!              the matrix of cofactors. MOUT is set to be the zero
!              matrix if M is singular.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If M is singular, MOUT is set to be the zero matrix.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     First the determinant is explicitly calculated using the
!     fundamental definition of the determinant. If this value is less
!     that 10**-16 then the matrix is deemed to be singular and the
!     output value is filled with zeros. Otherwise, the output matrix
!     is calculated an element at a time by generating the cofactor of
!     each element. Finally, each element in the matrix of cofactors
!     is multiplied by the reciprocal of the determinant and the result
!     is the inverse of the original matrix.
!
!     NO INTERNAL CHECKING ON THE INPUT MATRIX M IS PERFORMED EXCEPT
!     ON THE SIZE OF ITS DETERMINANT.  THUS IT IS POSSIBLE TO GENERATE
!     A FLOATING POINT OVERFLOW OR UNDERFLOW IN THE PROCESS OF
!     CALCULATING THE MATRIX OF COFACTORS.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a double precision 3x3 matrix, compute its inverse. Check
!        that the original matrix times the computed inverse produces
!        the identity matrix.
!
!        Example code begins here.
!
!
!              PROGRAM INVERT_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      IMAT ( 3, 3 )
!              DOUBLE PRECISION      M    ( 3, 3 )
!              DOUBLE PRECISION      MOUT ( 3, 3 )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define a matrix to invert.
!        C
!              DATA                  M  /  0.D0,  0.5D0, 0.D0,
!             .                           -1.D0,  0.D0,  0.D0,
!             .                            0.D0,  0.D0,  1.D0 /
!
!              WRITE(*,*) 'Original Matrix:'
!              DO I=1, 3
!
!                 WRITE(*,'(3F16.7)') ( M(I,J), J=1,3 )
!
!              END DO
!        C
!        C     Invert the matrix, then output.
!        C
!              CALL INVERT ( M, MOUT )
!
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Inverse Matrix:'
!              DO I=1, 3
!
!                 WRITE(*,'(3F16.7)') ( MOUT(I,J), J=1,3 )
!
!              END DO
!
!        C
!        C     Check the M times MOUT produces the identity matrix.
!        C
!              CALL MXM ( M, MOUT, IMAT )
!
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Original times inverse:'
!              DO I=1, 3
!
!                 WRITE(*,'(3F16.7)') ( IMAT(I,J), J=1,3 )
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
!         Original Matrix:
!               0.0000000      -1.0000000       0.0000000
!               0.5000000       0.0000000       0.0000000
!               0.0000000       0.0000000       1.0000000
!
!         Inverse Matrix:
!               0.0000000       2.0000000      -0.0000000
!              -1.0000000       0.0000000      -0.0000000
!               0.0000000      -0.0000000       1.0000000
!
!         Original times inverse:
!               1.0000000       0.0000000       0.0000000
!               0.0000000       1.0000000       0.0000000
!               0.0000000       0.0000000       1.0000000
!
!
! $ Restrictions
!
!     1)  The input matrix must be such that generating the cofactors
!         will not cause a floating point overflow or underflow. The
!         strictness of this condition depends, of course, on the
!         computer installation and the resultant maximum and minimum
!         values of double precision numbers.
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
!        Changed input argument name M1 to M for consistency with other
!        routines.
!
!        Added IMPLICIT NONE statement.
!
!        Updated the header to comply with NAIF standard. Added
!        complete code example to $Examples section.
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
!     invert a 3x3_matrix
!
!-&


!
!     Local variables
!
      double precision   det
      double precision   mtemp(3,3)
      double precision   mdet
      double precision   invdet

!
!  Find the determinant of M and check for singularity
!
      mdet = det(m)
      if ( dabs(mdet) .lt. 1.d-16 ) then
         call filld ( 0.d0, 9, mout )
         return
      end if

!
!  Get the cofactors of each element of M
!
      mtemp(1,1) =  (m(2,2)*m(3,3) - m(3,2)*m(2,3))
      mtemp(1,2) = -(m(1,2)*m(3,3) - m(3,2)*m(1,3))
      mtemp(1,3) =  (m(1,2)*m(2,3) - m(2,2)*m(1,3))
      mtemp(2,1) = -(m(2,1)*m(3,3) - m(3,1)*m(2,3))
      mtemp(2,2) =  (m(1,1)*m(3,3) - m(3,1)*m(1,3))
      mtemp(2,3) = -(m(1,1)*m(2,3) - m(2,1)*m(1,3))
      mtemp(3,1) =  (m(2,1)*m(3,2) - m(3,1)*m(2,2))
      mtemp(3,2) = -(m(1,1)*m(3,2) - m(3,1)*m(1,2))
      mtemp(3,3) =  (m(1,1)*m(2,2) - m(2,1)*m(1,2))

!
!  Multiply the cofactor matrix by 1/MDET to obtain the inverse
!
      invdet = 1.d0 / mdet
      call vsclg (invdet, mtemp, 9, mout)
!
      return
      end
