!$Procedure BSCHOI ( Binary search with order vector, integer )

      INTEGER FUNCTION BSCHOI ( VALUE, NDIM, ARRAY, ORDER )

! $ Abstract
!
!     Do a binary search for a given value within an integer array,
!     accompanied by an order vector. Return the index of the
!     matching array entry, or zero if the key value is not found.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     SEARCH
!
! $ Declarations

      IMPLICIT NONE

      INTEGER          VALUE
      INTEGER          NDIM
      INTEGER          ARRAY ( * )
      INTEGER          ORDER ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VALUE      I   Value to find in ARRAY.
!     NDIM       I   Dimension of ARRAY.
!     ARRAY      I   Array to be searched.
!     ORDER      I   Order vector.
!
!     The function returns the index of the first matching array element
!     or zero if the value is not found.
!
! $ Detailed_Input
!
!     VALUE    is the value to be found in the input array.
!
!     NDIM     is the number of elements in the input array.
!
!     ARRAY    is the array to be searched.
!
!     ORDER    is an order vector which can be used to access the
!              elements of ARRAY in order. The contents of order are a
!              permutation of the sequence of integers ranging from 1 to
!              NDIM.
!
! $ Detailed_Output
!
!     The function returns the index of the specified value in the input
!     array. Indices range from 1 to NDIM.
!
!     If the input array does not contain the specified value, the
!     function returns zero.
!
!     If the input array contains more than one occurrence of the
!     specified value, the returned index may point to any of the
!     occurrences.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If NDIM < 1, the value of the function is zero.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     A binary search is performed on the input array, whose order is
!     given by an associated order vector. If an element of the array is
!     found to match the input value, the index of that element is
!     returned. If no matching element is found, zero is returned.
!
! $ Examples
!
!     Let ARRAY and ORDER contain the following elements:
!
!           ARRAY         ORDER
!           -----------   -----
!             100             2
!               1             3
!              10             1
!           10000             5
!            1000             4
!
!     Then
!
!           BSCHOI (  1000, 5, ARRAY, ORDER )  = 5
!           BSCHOI (     1, 5, ARRAY, ORDER )  = 2
!           BSCHOI ( 10000, 5, ARRAY, ORDER )  = 4
!           BSCHOI (    -1, 5, ARRAY, ORDER )  = 0
!           BSCHOI (    17, 5, ARRAY, ORDER )  = 0
!
!     That is,
!
!           ARRAY(5) =  1000
!           ARRAY(2) =     1
!           ARRAY(4) = 10000
!
! $ Restrictions
!
!     1)  ORDER is assumed to give the order of the elements of ARRAY
!         in increasing order. If this condition is not met, the results
!         of BSCHOI are unpredictable.
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
!-    SPICELIB Version 1.0.1, 09-APR-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 18-SEP-1995 (IMU) (WLT)
!
!-&


! $ Index_Entries
!
!     binary search for an integer using an order vector
!
!-&


!
!     Local variables
!
      INTEGER          LEFT
      INTEGER          RIGHT
      INTEGER          I

!
!     Set the initial bounds for the search area.
!
      LEFT  = 1
      RIGHT = NDIM

      DO WHILE ( LEFT .LE. RIGHT )

!
!        Check the middle element.
!
         I = (LEFT+RIGHT)/2

!
!        If the middle element matches, return its location.
!
         IF ( VALUE .EQ. ARRAY(ORDER(I)) ) THEN
            BSCHOI = ORDER(I)
            RETURN

!
!        Otherwise narrow the search area.
!
         ELSE IF ( VALUE .LT. ARRAY(ORDER(I)) ) THEN
            RIGHT = I-1
         ELSE
            LEFT  = I+1
         END IF

      END DO

!
!     If the search area is empty, return zero.
!
      BSCHOI = 0

      RETURN
      END
