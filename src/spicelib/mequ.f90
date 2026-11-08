!$Procedure MEQU ( Matrix equal to another, 3x3 )

      subroutine mequ ( m1, mout )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Set one double precision 3x3 matrix equal to another.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASSIGNMENT
!     MATRIX
!
! $ Declarations

      implicit none

      real(wp)           m1   ( 3, 3 )
      real(wp)           mout ( 3, 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M1         I   Input matrix.
!     MOUT       O   Output matrix equal to M1.
!
! $ Detailed_Input
!
!     M1       is an arbitrary input 3x3 matrix. There are no
!              restrictions on what it may contain.
!
! $ Detailed_Output
!
!     MOUT     is a 3x3 matrix set to be equal to M1.
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
!     None.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) This trivial example demonstrates how to use MEQU to assign
!        one matrix to another.
!
!
!        Example code begins here.
!
!
!              PROGRAM MEQU_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      M1   ( 3, 3 )
!              DOUBLE PRECISION      MOUT ( 3, 3 )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define M1.
!        C
!              DATA                  M1   /  0.0D0,  1.0D0,  0.0D0,
!             .                             -1.0D0,  0.0D0,  0.0D0,
!             .                              0.0D0,  0.0D0,  1.0D0  /
!
!        C
!        C     Assign M1 to MOUT and print MOUT.
!        C
!              CALL MEQU ( M1, MOUT )
!
!              WRITE(*,'(A)') 'MOUT:'
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
!        MOUT:
!               0.0000000      -1.0000000       0.0000000
!               1.0000000       0.0000000       0.0000000
!               0.0000000       0.0000000       1.0000000
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
!-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example based on existing example.
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
!     equal to another 3x3_matrix
!
!-&


      call moved (m1, 9, mout)

      return
      end
