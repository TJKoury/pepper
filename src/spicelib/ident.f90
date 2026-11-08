!$Procedure IDENT ( Return the 3x3 identity matrix )

      subroutine ident ( matrix )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Return the 3x3 identity matrix.
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

      real(wp)              matrix ( 3, 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MATRIX     O   The 3x3 identity matrix.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     MATRIX   is the 3x3 Identity matrix. That MATRIX is
!              the following
!
!                 .-                       -.
!                 |  1.0D0   0.0D0   0.0D0  |
!                 |  0.0D0   1.0D0   0.0D0  |
!                 |  0.0D0   0.0D0   1.0D0  |
!                 `-                       -'
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
!     This is a utility routine for obtaining the 3x3 identity matrix
!     so that you may avoid having to write the loop or assignments
!     needed to get the matrix.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Define a 3x3 matrix and compute its inverse using the SPICELIB
!        routine INVERT. Verify the accuracy of the computed inverse
!        using the mathematical identity
!
!                -1
!           M x M   - I = 0
!
!        where I is the 3x3 identity matrix.
!
!
!        Example code begins here.
!
!
!              PROGRAM IDENT_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      IDMAT  ( 3, 3 )
!              DOUBLE PRECISION      IMAT   ( 3, 3 )
!              DOUBLE PRECISION      M      ( 3, 3 )
!              DOUBLE PRECISION      MOUT   ( 3, 3 )
!              DOUBLE PRECISION      MZERO  ( 3, 3 )
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
!              CALL IDENT ( IDMAT )
!              CALL MXM   ( M, MOUT, IMAT )
!
!              CALL VSUBG ( IMAT, IDMAT, 9, MZERO )
!
!              WRITE(*,*) ' '
!              WRITE(*,*) 'Original times inverse minus identity:'
!              DO I=1, 3
!
!                 WRITE(*,'(3F16.7)') ( MZERO(I,J), J=1,3 )
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
!         Original times inverse minus identity:
!               0.0000000       0.0000000       0.0000000
!               0.0000000       0.0000000       0.0000000
!               0.0000000       0.0000000       0.0000000
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
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.1, 03-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!-    SPICELIB Version 1.0.0, 05-FEB-1996 (WLT)
!
!-&


! $ Index_Entries
!
!     Get the 3x3 identity matrix
!
!-&


      matrix ( 1, 1 ) = 1.0_wp
      matrix ( 2, 1 ) = 0.0_wp
      matrix ( 3, 1 ) = 0.0_wp

      matrix ( 1, 2 ) = 0.0_wp
      matrix ( 2, 2 ) = 1.0_wp
      matrix ( 3, 2 ) = 0.0_wp

      matrix ( 1, 3 ) = 0.0_wp
      matrix ( 2, 3 ) = 0.0_wp
      matrix ( 3, 3 ) = 1.0_wp

      return
      end
