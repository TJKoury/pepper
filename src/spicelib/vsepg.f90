!$Procedure VSEPG ( Angular separation of vectors, general dimension )

      double precision function vsepg ( v1, v2, ndim )

! $ Abstract
!
!     Find the separation angle in radians between two double precision
!     vectors of arbitrary dimension. This angle is defined as zero if
!     either vector is zero.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ANGLE
!     VECTOR
!
! $ Declarations

      implicit none

      double precision   v1 ( * )
      double precision   v2 ( * )
      integer            ndim

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     V1         I   First vector.
!     V2         I   Second vector.
!     NDIM       I   The number of elements in V1 and V2.
!
!     The function returns the angle between V1 and V2 expressed in
!     radians.
!
! $ Detailed_Input
!
!     V1,
!     V2       are two double precision vectors of arbitrary dimension.
!              Either V1 or V2, or both, may be the zero vector.
!
!              An implicit assumption exists that V1 and V2 are
!              specified in the same reference space. If this is not
!              the case, the numerical result of this routine has no
!              meaning.
!
!     NDIM     is the dimension of both V1 and V2.
!
! $ Detailed_Output
!
!     The function returns the angle between V1 and V2 expressed in
!     radians.
!
!     VSEPG is strictly non-negative. For input vectors of four or more
!     dimensions, the angle is defined as the generalization of the
!     definition for three dimensions. If either V1 or V2 is the zero
!     vector, then VSEPG is defined to be 0 radians.
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
!     In four or more dimensions this angle does not have a physically
!     realizable interpretation. However, the angle is defined as
!     the generalization of the following definition which is valid in
!     three or two dimensions:
!
!        In the plane, it is a simple matter to calculate the angle
!        between two vectors once the two vectors have been made to be
!        unit length. Then, since the two vectors form the two equal
!        sides of an isosceles triangle, the length of the third side
!        is given by the expression
!
!           LENGTH = 2.0 * SIN ( VSEPG/2.0 )
!
!        The length is given by the magnitude of the difference of the
!        two unit vectors
!
!           LENGTH = NORM ( U1 - U2 )
!
!        Once the length is found, the value of VSEPG may be calculated
!        by inverting the first expression given above as
!
!           VSEPG = 2.0 * ARCSIN ( LENGTH/2.0 )
!
!        This expression becomes increasingly unstable when VSEPG gets
!        larger than PI/2 radians or 90 degrees. In this situation
!        (which is easily detected by determining the sign of the dot
!        product of V1 and V2) the supplementary angle is calculated
!        first and then VSEPG is given by
!
!           VSEPG = PI - SUPPLEMENTARY_ANGLE
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Define two sets of n-dimensional vectors and compute the
!        angular separation between each vector in first set and the
!        corresponding vector in the second set.
!
!
!        Example code begins here.
!
!
!              PROGRAM VSEPG_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              DOUBLE PRECISION      VSEPG
!
!        C
!        C     Local parameters.
!        C
!              INTEGER               NDIM
!              PARAMETER           ( NDIM   = 4 )
!
!              INTEGER               SETSIZ
!              PARAMETER           ( SETSIZ = 3 )
!
!        C
!        C     Local variables.
!        C
!              DOUBLE PRECISION      V1   ( NDIM, SETSIZ )
!              DOUBLE PRECISION      V2   ( NDIM, SETSIZ )
!
!              INTEGER               I
!              INTEGER               J
!
!        C
!        C     Define the two vector sets.
!        C
!              DATA                  V1 /
!             .                      1.D0,  0.D0,  0.D0,  0.D0,
!             .                      1.D0,  0.D0,  0.D0,  0.D0,
!             .                      3.D0,  0.D0,  0.D0,  0.D0   /
!
!              DATA                  V2 /
!             .                      1.D0,  0.D0,  0.D0,  0.D0,
!             .                      0.D0,  1.D0,  0.D0,  0.D0,
!             .                     -5.D0,  0.D0,  0.D0,  0.D0  /
!
!        C
!        C     Calculate the angular separation between each pair
!        C     of vectors.
!        C
!              DO I=1, SETSIZ
!
!                 WRITE(*,'(A,4F6.1)')  'First vector            : ',
!             .                        ( V1(J,I), J=1,NDIM )
!                 WRITE(*,'(A,4F6.1)')  'Second vector           : ',
!             .                        ( V2(J,I), J=1,NDIM )
!                 WRITE(*,'(A,F15.10)') 'Angular separation (rad): ',
!             .                VSEPG ( V1(1,I), V2(1,I), NDIM )
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
!        First vector            :    1.0   0.0   0.0   0.0
!        Second vector           :    1.0   0.0   0.0   0.0
!        Angular separation (rad):    0.0000000000
!
!        First vector            :    1.0   0.0   0.0   0.0
!        Second vector           :    0.0   1.0   0.0   0.0
!        Angular separation (rad):    1.5707963268
!
!        First vector            :    3.0   0.0   0.0   0.0
!        Second vector           :   -5.0   0.0   0.0   0.0
!        Angular separation (rad):    3.1415926536
!
!
! $ Restrictions
!
!     1)  The user is required to insure that the input vectors will not
!         cause floating point overflow upon calculation of the vector
!         dot product since no error detection or correction code is
!         implemented. In practice, this is not a significant
!         restriction.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     C.A. Curzon        (JPL)
!     J. Diaz del Rio    (ODC Space)
!     K.R. Gehringer     (JPL)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 06-JUL-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!        Added complete code example based on existing example.
!
!-    SPICELIB Version 1.1.0, 29-FEB-1996 (KRG)
!
!        The declaration for the SPICELIB function PI is now
!        preceded by an EXTERNAL statement declaring PI to be an
!        external function. This removes a conflict with any
!        compilers that have a PI intrinsic function.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (CAC) (HAN)
!
!-&


! $ Index_Entries
!
!     angular separation of n-dimensional vectors
!
!-&


!
!     SPICELIB functions
!
      external              pi
      double precision      pi
      double precision      vnormg
      double precision      vdotg

!
!     Local Variables
!
!     The following declarations represent, respectively:
!        Magnitudes of V1, V2
!        Reciprocals of the magnitudes of V1, V2
!        Magnitude of either of the difference vectors: V1-V2 or
!           V1-(-V2)
!
      double precision      dmag1
      double precision      dmag2
      double precision      r1
      double precision      r2
      double precision      magdif

      integer               i

!
!     Calculate the magnitudes of V1 and V2; if either is 0, VSEPG = 0
!
      dmag1 = vnormg ( v1, ndim )
      if ( dmag1 .eq. 0.d0 ) then
         vsepg = 0.d0
         return
      end if

      dmag2 = vnormg ( v2, ndim )
      if ( dmag2 .eq. 0.d0 ) then
         vsepg = 0.d0
         return
      end if


      if      ( vdotg (v1, v2, ndim) .gt. 0 ) then

         r1 = 1.0d0/dmag1
         r2 = 1.0d0/dmag2

         magdif = 0.0d0
         do i = 1, ndim
            magdif = magdif + ( v1(i)*r1 - v2(i)*r2 )**2
         end do
         magdif = dsqrt(magdif)

         vsepg = 2.0d0 * asin (0.5d0 * magdif)

      else if ( vdotg (v1, v2, ndim) .lt. 0 ) then

         r1 = 1.0d0/dmag1
         r2 = 1.0d0/dmag2

         magdif = 0.0d0
         do i = 1, ndim
            magdif = magdif + ( v1(i)*r1 + v2(i)*r2 )**2
         end do
         magdif = dsqrt(magdif)

         vsepg = pi() - 2.0d0 * asin (0.5d0 * magdif)

      else
         vsepg = pi() / 2.0d0
      end if

      return
      end
