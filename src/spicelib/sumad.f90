!$Procedure SUMAD ( Sum of a double precision array )

      double precision function sumad ( array, n )

! $ Abstract
!
!     Return the sum of the elements of a double precision array.
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

      double precision   array  ( * )
      integer            n

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ARRAY      I   Input array.
!     N          I   Number of elements in ARRAY.
!
!     The function returns the sum of the elements of ARRAY.
!
! $ Detailed_Input
!
!     ARRAY    is the input double precision array.
!
!     N        is the number of elements in the array.
!
! $ Detailed_Output
!
!     The function returns the sum of the elements of the input array.
!     That is,
!
!        SUMAD( ARRAY, N ) = ARRAY(1) + ARRAY(2) + ... + ARRAY(N)
!
!     If N is zero or negative, SUMAD is zero.
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
!     The value of the function is initially set to zero. The elements
!     of the array are then added. If the number of elements is zero or
!     negative, SUMAD is zero.
!
! $ Examples
!
!     Let ARRAY contain the following elements.
!
!           ARRAY(1) = 12.D0
!           ARRAY(2) =  1.D0
!           ARRAY(3) =  4.D0
!           ARRAY(4) = 75.D0
!           ARRAY(5) = 18.D0
!
!     Then
!
!           SUMAD ( ARRAY,   -3 )       =   0.D0
!           SUMAD ( ARRAY,    0 )       =   0.D0
!           SUMAD ( ARRAY,    1 )       =  12.D0
!           SUMAD ( ARRAY,    2 )       =  13.D0
!           SUMAD ( ARRAY,    5 )       = 110.D0
!           SUMAD ( ARRAY(3), 3 )       =  97.D0
!
! $ Restrictions
!
!     1)  SUMAD does not check for overflow.
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
!-    SPICELIB Version 1.1.0, 09-APR-2021 (JDR)
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
!     sum of a d.p. array
!
!-&


!
!     Local variables
!
      double precision sum
      integer          i


!
!     Begin at zero.
!
      sum = 0.d0

!
!     Sum the elements. If N is zero or negative, nothing happens.
!
      do i = 1, n
         sum = sum + array(i)
      end do

!
!     Return the sum.
!
      sumad = sum

      return
      end
