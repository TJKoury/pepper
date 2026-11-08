!$Procedure UNORM ( Unit vector and norm, 3 dimensional )

      subroutine unorm ( v1, vout, vmag )
      use pepper_kinds, only: wp

! $ Abstract
!
!     Normalize a double precision 3-vector and return its magnitude.
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

      real(wp)          v1   ( 3 )
      real(wp)          vout ( 3 )
      real(wp)          vmag

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     V1         I   Vector to be normalized.
!     VOUT       O   Unit vector V1 / |V1|.
!     VMAG       O   Magnitude of V1, i.e. |V1|.
!
! $ Detailed_Input
!
!     V1       is an arbitrary 3-vector, including the
!              zero vector.
!
! $ Detailed_Output
!
!     VOUT     is the unit vector in the direction of V1. If V1 is
!              the zero vector, then VOUT will also be the zero
!              vector.
!
!     VMAG     is the magnitude of V1.
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
!     UNORM references a function called VNORM (which itself is
!     numerically stable) to calculate the norm of the input vector V1.
!     If the norm is equal to zero, then each component of the output
!     vector VOUT is set to zero. Otherwise, VOUT is calculated by
!     dividing V1 by the norm.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Define a set of vectors and compute their corresponding unit
!        vector and magnitudes.
!
!
!        Example code begins here.
!
!
!              PROGRAM UNORM_EX1
!              IMPLICIT NONE
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               SETSIZ
!              PARAMETER           ( SETSIZ = 2 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      VMAG
!              DOUBLE PRECISION      SETA ( 3, SETSIZ )
!              DOUBLE PRECISION      VOUT ( 3 )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define the vector set.
!        C
!              DATA                  SETA / 5.D0,  12.D0,  0.D0,
!             .                             1.D-7,  2.D-7, 2.D-7 /
!
!        C
!        C     Calculate the unit vectors and magnitudes.
!        C
!              DO I=1, SETSIZ
!
!                 CALL UNORM ( SETA(1,I), VOUT, VMAG )
!
!                 WRITE(*,'(A,3F13.8)') 'Vector     : ',
!             .                                   ( SETA(J,I), J=1, 3 )
!                 WRITE(*,'(A,3F13.8)') 'Unit vector: ', VOUT
!                 WRITE(*,'(A,F13.8)')  'Magnitude:   ', VMAG
!                 WRITE(*,*) ' '
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
!        Vector     :    5.00000000  12.00000000   0.00000000
!        Unit vector:    0.38461538   0.92307692   0.00000000
!        Magnitude:     13.00000000
!
!        Vector     :    0.00000010   0.00000020   0.00000020
!        Unit vector:    0.33333333   0.66666667   0.66666667
!        Magnitude:      0.00000030
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     W.M. Owen          (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 05-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Added complete example code.
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO) (WLT)
!
!-&


! $ Index_Entries
!
!     3-dimensional unit vector and norm
!
!-&


!
!     SPICELIB functions
!
      real(wp)           vnorm

!
!  Obtain the magnitude of V1
!
      vmag = vnorm(v1)

!
!   If VMAG is nonzero, then normalize.  Note that this process is
!   numerically stable: overflow could only happen if VMAG were small,
!   but this could only happen if each component of V1 were small.
!   In fact, the magnitude of any vector is never less than the
!   magnitude of any component.
!
      if ( vmag .gt. 0.0_wp ) then
         vout(1) = v1(1) / vmag
         vout(2) = v1(2) / vmag
         vout(3) = v1(3) / vmag
      else
         vout(1) = 0.0_wp
         vout(2) = 0.0_wp
         vout(3) = 0.0_wp
      end if

      return
      end
