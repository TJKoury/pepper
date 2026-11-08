!$Procedure MTXV ( Matrix transpose times vector, 3x3 )

      subroutine mtxv ( m, vin, vout )

! $ Abstract
!
!     Multiply the transpose of a 3x3 matrix on the left with a vector
!     on the right.
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

      double precision  m      ( 3,3 )
      double precision  vin    (   3 )
      double precision  vout   (   3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M          I   3X3 double precision matrix.
!     VIN        I   3-dimensional double precision vector.
!     VOUT       O   3-dimensional double precision vector. VOUT is
!                    the product M**T * VIN.
!
! $ Detailed_Input
!
!     M        is an arbitrary 3x3 double precision matrix.
!              Typically, M will be a rotation matrix since
!              then its transpose is its inverse (but this is NOT
!              a requirement).
!
!     VIN      is an arbitrary 3-dimensional double precision
!              vector.
!
! $ Detailed_Output
!
!     VOUT     is a 3-dimensional double precision vector. VOUT is
!              the product VOUT = (M**T)  x (VIN).
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
!        For each value of the subscript I from 1 to 3:
!
!                        3
!                     .-----
!                      \
!           VOUT(I) =   )  M(K,I) * VIN(K)
!                      /
!                     '-----
!                       K=1
!
!     Note that the reversal of the K and I subscripts in the left-hand
!     matrix M is what makes VOUT the product of the TRANSPOSE of
!     and not simply of M itself.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a 3x3 matrix and a 3-vector, multiply the transpose of
!        the matrix by the vector.
!
!
!        Example code begins here.
!
!
!              PROGRAM MTXV_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      M    ( 3, 3 )
!              DOUBLE PRECISION      VIN  ( 3    )
!              DOUBLE PRECISION      VOUT ( 3    )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define M and VIN.
!        C
!              DATA                  M    /  1.0D0, -1.0D0,  0.0D0,
!             .                              1.0D0,  1.0D0,  0.0D0,
!             .                              0.0D0,  0.0D0,  1.0D0  /
!
!              DATA                  VIN  /  5.0D0, 10.0D0, 15.0D0  /
!
!        C
!        C     Multiply the transpose of M by VIN.
!        C
!              CALL MTXV ( M, VIN, VOUT )
!
!              WRITE(*,'(A)') 'Transpose of M times VIN:'
!              WRITE(*,'(3F10.3)') VOUT
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Transpose of M times VIN:
!            -5.000    15.000    15.000
!
!
!        Note that typically the matrix M will be a rotation matrix.
!        Because the transpose of an orthogonal matrix is equivalent to
!        its inverse, applying the rotation to the vector is
!        accomplished by multiplying the vector by the transpose of the
!        matrix.
!
!        Let
!
!               -1
!              M   * VIN = VOUT
!
!        If M is an orthogonal matrix, then (M**T) * VIN = VOUT.
!
! $ Restrictions
!
!     1)  The user is responsible for checking the magnitudes of the
!         elements of M and VIN so that a floating point overflow does
!         not occur.
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
!-    SPICELIB Version 1.1.0, 25-AUG-2021 (JDR)
!
!        Changed input argument name MATRIX to M for consistency with
!        other routines.
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
!     matrix_transpose times 3-dimensional vector
!
!-&


!
!     Local variables
!
      double precision  prodv  ( 3 )
      integer           i


!
!  Perform the matrix-vector multiplication
!
      do i = 1, 3
        prodv(i) = m(1,i)*vin(1) + &
                   m(2,i)*vin(2) + &
                   m(3,i)*vin(3)
      end do

!
!  Move the result into VOUT
!
      vout(1) = prodv(1)
      vout(2) = prodv(2)
      vout(3) = prodv(3)


      return
      end
