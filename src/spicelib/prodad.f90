!$Procedure PRODAD ( Product of a double precision array )

      double precision function  prodad ( array, n )

! $ Abstract
!
!     Return the product of the elements of a double precision array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     MATH
!     UTILITY
!
! $ Declarations

      implicit none

      double precision   array ( * )
      integer            n

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ARRAY      I   Input array.
!     N          I   Number of elements in ARRAY.
!
!     The function returns the product of the elements of ARRAY.
!
! $ Detailed_Input
!
!     ARRAY    is the input array.
!
!     N        is the number of elements in the array.
!
! $ Detailed_Output
!
!     The function returns the product of the elements of the input
!     array. That is,
!
!        PRODAD = ARRAY(1) * ARRAY(2) * ... * ARRAY(N)
!
!     If N is zero or negative, PRODAD is one.
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
!     The value of the function is initially set to one. The elements
!     of the array are then multiplied. If the number of elements is
!     zero or negative, PRODAD is one.
!
! $ Examples
!
!     Let ARRAY contain the following elements.
!
!           ARRAY(1) = 12.D0
!           ARRAY(2) =  2.D0
!           ARRAY(3) =  4.D0
!           ARRAY(4) = 75.D0
!           ARRAY(5) = 18.D0
!
!     Then
!
!           PRODAD ( ARRAY,   -3 )       =      1.D0
!           PRODAD ( ARRAY,    0 )       =      1.D0
!           PRODAD ( ARRAY,    1 )       =     12.D0
!           PRODAD ( ARRAY,    2 )       =     24.D0
!           PRODAD ( ARRAY,    5 )       = 129600.D0
!           PRODAD ( ARRAY(3), 3 )       =   5400.D0
!
! $ Restrictions
!
!     1)  PRODAD does not check for overflow.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 07-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     product of a d.p. array
!
!-&


!
!     Local variables
!
      double precision prod
      integer          i


!
!     Begin at one.
!
      prod = 1.d0

!
!     Multiply the elements. If N is zero or negative, nothing happens.
!
      do i = 1, n
         prod = prod * array(i)
      end do

!
!     Return the product.
!
      prodad = prod

      return
      end
