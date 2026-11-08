!$Procedure PRODAI ( Product of an integer array )

      INTEGER FUNCTION  PRODAI ( ARRAY, N )

! $ Abstract
!
!     Return the product of the elements of an integer array.
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

      IMPLICIT NONE

      INTEGER          ARRAY  ( * )
      INTEGER          N

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
!        PRODAI = ARRAY(1) * ARRAY(2) * ... * ARRAY(N)
!
!     If N is zero or negative, PRODAI is one.
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
!     zero or negative, PRODAI is one.
!
! $ Examples
!
!     Let ARRAY contain the following elements.
!
!           ARRAY(1) = 12
!           ARRAY(2) =  2
!           ARRAY(3) =  4
!           ARRAY(4) = 75
!           ARRAY(5) = 18
!
!     Then
!
!           PRODAI ( ARRAY,   -3 )       =      1
!           PRODAI ( ARRAY,    0 )       =      1
!           PRODAI ( ARRAY,    1 )       =     12
!           PRODAI ( ARRAY,    2 )       =     24
!           PRODAI ( ARRAY,    5 )       = 129600
!           PRODAI ( ARRAY(3), 3 )       =   5400
!
! $ Restrictions
!
!     1)  PRODAI does not check for overflow. (For integers, this can
!         occur relatively quickly.)
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
!-    SPICELIB Version 1.1.0, 08-APR-2021 (JDR)
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
!     product of an integer array
!
!-&


!
!     Local variables
!
      INTEGER          PROD
      INTEGER          I


!
!     Begin at one.
!
      PROD = 1

!
!     Multiply the elements. If N is zero or negative, nothing happens.
!
      DO I = 1, N
         PROD = PROD * ARRAY(I)
      END DO

!
!     Return the product.
!
      PRODAI = PROD

      RETURN
      END
