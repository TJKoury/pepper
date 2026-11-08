!$Procedure TRACEG ( Trace of a matrix, general dimension )

      real(wp) function  traceg ( matrix, ndim )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Return the trace of a square matrix of arbitrary dimension.
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

      integer(ip)        ndim
      real(wp)           matrix ( ndim,ndim )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     MATRIX     I     NDIM x NDIM matrix of double precision numbers.
!     NDIM       I     Dimension of the matrix.
!
!     The function returns the trace of the square matrix of arbitrary
!     dimension MATRIX.
!
! $ Detailed_Input
!
!     MATRIX   is a double precision square matrix of arbitrary
!              dimension. The input matrix must be square or else the
!              concept is meaningless.
!
!     NDIM     is the dimension of MATRIX.
!
! $ Detailed_Output
!
!     The function returns the trace of MATRIX, i.e. it is the sum of
!     the diagonal elements of MATRIX.
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
!     The code reflects precisely the following mathematical
!     expression:
!
!                   NDIM
!                 .------
!                  \
!        TRACEG =   ) MATRIX(I,I)
!                  /
!                 '------
!                    I=1
!
!     No error detection or correction is implemented within this
!     function.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Given a 4x4 double precision matrix, compute its trace.
!
!
!        Example code begins here.
!
!
!              PROGRAM TRACEG_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              DOUBLE PRECISION      TRACEG
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               NDIM
!              PARAMETER           ( NDIM   = 4 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      MATRIX ( NDIM, NDIM )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define MATRIX.
!        C
!              DATA                  MATRIX  /
!             .                          3.D0,  0.D0,  4.D0,  0.D0,
!             .                          5.D0, -2.D0,  0.D0,  0.D0,
!             .                          7.D0,  8.D0, -1.D0,  1.D0,
!             .                          3.D0,  1.D0,  0.D0,  0.D0  /
!
!
!              WRITE(*,'(A)') 'Matrix:'
!              DO I=1, NDIM
!
!                 WRITE(*,'(4F6.1)') ( MATRIX(I,J), J=1,NDIM )
!
!              END DO
!
!        C
!        C     Compute the trace of MATRIX and display the result.
!        C
!              WRITE(*,*)
!              WRITE(*,'(A,F4.1)') 'Trace: ', TRACEG ( MATRIX, NDIM )
!
!              END
!
!
!        When this program was executed on a Mac/Intel/gfortran/64-bit
!        platform, the output was:
!
!
!        Matrix:
!           3.0   5.0   7.0   3.0
!           0.0  -2.0   8.0   1.0
!           4.0   0.0  -1.0   0.0
!           0.0   0.0   1.0   0.0
!
!        Trace:  0.0
!
!
! $ Restrictions
!
!     1)  No checking is performed to guard against floating point
!         overflow or underflow. This routine should probably not be
!         used if the input matrix is expected to have large double
!         precision numbers along the diagonal.
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
!-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Created
!        complete code example based on existing fragment. Updated
!        $Particulars to provide mathematical representation of the
!        implemented algorithm.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     trace of a nxn_matrix
!
!-&


      integer(ip)     i

      traceg = 0.0_wp
      do i = 1,ndim
         traceg = traceg + matrix(i,i)
      end do

      return
      end
