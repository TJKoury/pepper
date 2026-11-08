!$Procedure MXV ( Matrix times vector, 3x3 )

      subroutine mxv ( m, vin, vout )

! $ Abstract
!
!     Multiply a 3x3 double precision matrix with a 3-dimensional
!     double precision vector.
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

      double precision   m      ( 3, 3 )
      double precision   vin    (    3 )
      double precision   vout   (    3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M          I   3x3 double precision matrix.
!     VIN        I   3-dimensional double precision vector.
!     VOUT       O   3-dimensional double precision vector. VOUT is
!                    the product M*VIN.
!
! $ Detailed_Input
!
!     M        is an arbitrary 3x3 double precision matrix.
!
!     VIN      is an arbitrary 3-dimensional double precision vector.
!
! $ Detailed_Output
!
!     VOUT     is a 3-dimensional double precision vector. VOUT is
!              the product M * V.
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
!           VOUT(I) =   )  M(I,K) * VIN(K)
!                      /
!                     '-----
!                       K=1
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a 3x3 matrix and a 3-vector, multiply the matrix by
!        the vector.
!
!
!        Example code begins here.
!
!
!              PROGRAM MXV_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      M    ( 3, 3 )
!              DOUBLE PRECISION      VIN  ( 3    )
!              DOUBLE PRECISION      VOUT ( 3    )
!
!        C
!        C     Define M and VIN.
!        C
!              DATA                  M    /  0.0D0, -1.0D0,  0.0D0,
!             .                              1.0D0,  0.0D0,  0.0D0,
!             .                              0.0D0,  0.0D0,  1.0D0  /
!
!              DATA                  VIN  /  1.0D0,  2.0D0,  3.0D0  /
!
!        C
!        C     Multiply M by VIN.
!        C
!              CALL MXV ( M, VIN, VOUT )
!
!              WRITE(*,'(A)') 'M times VIN:'
!              WRITE(*,'(3F10.3)') VOUT
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        M times VIN:
!             2.000    -1.000     3.000
!
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
!-    SPICELIB Version 1.1.0, 13-AUG-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on the existing example.
!
!        Changed input argument name MATRIX to M for consistency with
!        other routines.
!
!        Added IMPLICIT NONE statement.
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
!     matrix times 3-dimensional vector
!
!-&


!
!     Local variables
!
      double precision     prodv  ( 3 )
      integer              i

!
!  Perform the matrix-vector multiplication
!
      do i = 1, 3
         prodv(i) = m(i,1)*vin(1) + &
                    m(i,2)*vin(2) + &
                    m(i,3)*vin(3)
      end do
!
!  Move the buffered vector into the output vector VOUT.
!
      vout(1) = prodv(1)
      vout(2) = prodv(2)
      vout(3) = prodv(3)

      return
      end
