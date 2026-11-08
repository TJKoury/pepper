!$Procedure VNORM ( Vector norm, 3 dimensions )

      real(wp) function vnorm ( v1 )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Compute the magnitude of a double precision 3-dimensional
!     vector.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     VECTOR
!
! $ Declarations

      implicit none

      real(wp)          v1 ( 3 )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     V1         I   Vector whose magnitude is to be found.
!
!     The function returns the magnitude of V1.
!
! $ Detailed_Input
!
!     V1       is any double precision 3-dimensional vector.
!
! $ Detailed_Output
!
!     The function returns the magnitude of V1 calculated in a
!     numerically stable way.
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
!     VNORM takes care to avoid overflow while computing the norm of the
!     input vector V1. VNORM finds the component of V1 whose magnitude
!     is the largest. Calling this magnitude V1MAX, the norm is computed
!     using the formula:
!
!                        ||    1         ||
!        VNORM = V1MAX * || ------- * V1 ||
!                        ||  V1MAX       ||
!
!     where the notation ||X|| indicates the norm of the vector X.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Define a set of 3-dimensional vectors and compute the
!        magnitude of each vector within.
!
!
!        Example code begins here.
!
!
!              PROGRAM VNORM_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              DOUBLE PRECISION      VNORM
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               SETSIZ
!              PARAMETER           ( SETSIZ = 3 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      V1   ( 3, SETSIZ )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define a set of 3-dimensional vectors.
!        C
!              DATA                  V1  /  1.D0,   2.D0,   2.D0,
!             .                             5.D0,  12.D0,   0.D0,
!             .                            -5.D-17, 0.0D0, 12.D-17  /
!
!        C
!        C     Calculate the magnitude of each vector
!        C
!              DO I=1, SETSIZ
!
!                 WRITE(*,'(A,3E10.2)') 'Input vector: ',
!             .                         ( V1(J,I), J=1,3 )
!                 WRITE(*,'(A,F24.20)') 'Magnitude   : ',
!             .                         VNORM ( V1(1,I) )
!                 WRITE(*,*)
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
!        Input vector:   0.10E+01  0.20E+01  0.20E+01
!        Magnitude   :   3.00000000000000000000
!
!        Input vector:   0.50E+01  0.12E+02  0.00E+00
!        Magnitude   :  13.00000000000000000000
!
!        Input vector:  -0.50E-16  0.00E+00  0.12E-15
!        Magnitude   :   0.00000000000000013000
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
!-    SPICELIB Version 1.0.2, 06-JUL-2020 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
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
!     norm of 3-dimensional vector
!
!-&


      real(wp) v1max

!
!  Determine the maximum component of the vector.
!
      v1max = max (dabs(v1(1)), dabs(v1(2)), dabs(v1(3)))

!
!  If the vector is zero, return zero; otherwise normalize first.
!  Normalizing helps in the cases where squaring would cause overflow
!  or underflow.  In the cases where such is not a problem it not worth
!  it to optimize further.
!
      if (v1max.eq.0.0_wp) then
         vnorm = 0.0_wp
      else
         vnorm = v1max * dsqrt (  (v1(1)/v1max)**2 &
                                + (v1(2)/v1max)**2 &
                                + (v1(3)/v1max)**2)
      end if
!
      return
      end
