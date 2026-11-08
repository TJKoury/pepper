!$Procedure MXVG ( Matrix time vector, general dimension )

      SUBROUTINE MXVG ( M1, V2, NR1, NC1R2, VOUT )

! $ Abstract
!
!     Multiply a matrix and a vector of arbitrary size.
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

      IMPLICIT NONE

      INTEGER            NR1
      INTEGER            NC1R2
      DOUBLE PRECISION   M1   ( NR1,NC1R2 )
      DOUBLE PRECISION   V2   (     NC1R2 )
      DOUBLE PRECISION   VOUT ( NR1       )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     M1         I   Left-hand matrix to be multiplied.
!     V2         I   Right-hand vector to be multiplied.
!     NR1        I   Row dimension of M1 and length of VOUT.
!     NC1R2      I   Column dimension of M1 and length of V2.
!     VOUT       O   Product vector M1*V2.
!
! $ Detailed_Input
!
!     M1       is a double precision matrix of arbitrary size which
!              forms the left-hand matrix of the multiplication.
!
!     V2       is a double precision vector on the right of the
!              multiplication.
!
!     NR1      is the row dimension of M1 and length of VOUT.
!
!     NC1R2    is the column dimension of M1 and length of V2.
!
! $ Detailed_Output
!
!     VOUT     is the double precision vector which results from
!              the expression VOUT = (M1) x V2.
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
!        For each value of the subscript I from 1 to NR1,
!
!        VOUT(I) = Summation from K=1 to NC1R2 of  ( M1(I,K) * V2(K) )
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a 2x3 matrix and a 3-vector, multiply the matrix by
!        the vector.
!
!
!        Example code begins here.
!
!
!              PROGRAM MXVG_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      M    ( 2, 3 )
!              DOUBLE PRECISION      VIN  ( 3    )
!              DOUBLE PRECISION      VOUT ( 2    )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define M and VIN.
!        C
!              DATA                  M    /  1.0D0,  2.0D0,
!             .                              1.0D0,  3.0D0,
!             .                              1.0D0,  4.0D0  /
!
!              DATA                  VIN  /  1.0D0,  2.0D0,  3.0D0  /
!
!        C
!        C     Multiply M by VIN.
!        C
!              CALL MXVG ( M, VIN, 2, 3, VOUT )
!
!              WRITE(*,'(A)') 'M times VIN:'
!              WRITE(*,'(2F10.3)') VOUT
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        M times VIN:
!             6.000    20.000
!
!
! $ Restrictions
!
!     1)  The user is responsible for checking the magnitudes of the
!         elements of M1 and V2 so that a floating point overflow does
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
!-    SPICELIB Version 1.1.0, 04-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!        Added complete code example based on the existing example.
!
!-    SPICELIB Version 1.0.2, 23-APR-2010 (NJB)
!
!        Re-ordered header sections and made minor formatting
!        changes.
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
!     matrix times n-dimensional vector
!
!-&


!
!     Local variables
!
      DOUBLE PRECISION      SUM

      INTEGER               I
      INTEGER               K

!
!  Perform the matrix-vector multiplication
!
      DO I = 1, NR1
         SUM = 0.D0
         DO K = 1, NC1R2
            SUM = SUM + M1(I,K)*V2(K)
         END DO
         VOUT(I) = SUM
      END DO

      RETURN
      END
