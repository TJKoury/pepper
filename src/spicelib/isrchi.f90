!$Procedure ISRCHI  ( Search in an integer array )

      integer function  isrchi ( value, ndim, array )

! $ Abstract
!
!     Search for a given value within a integer array. Return
!     the index of the first matching array entry, or zero if
!     the key value was not found.
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

      implicit none

      integer    value
      integer    ndim
      integer    array ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VALUE      I   Key value to be found in ARRAY.
!     NDIM       I   Dimension of ARRAY.
!     ARRAY      I   Integer array to search.
!
!     The function returns the index of the first matching array
!     element or zero if the value is not found.
!
! $ Detailed_Input
!
!     VALUE    is the key value to be found in the array.
!
!     NDIM     is the dimension of the array.
!
!     ARRAY    is the integer array to be searched.
!
! $ Detailed_Output
!
!     The function returns the index of the first matching array
!     element in ARRAY. If VALUE is not found, ISRCHI is zero.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If NDIM < 1, the function value is zero.
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
!     The following table shows the value of ISRCHI given the contents
!     of ARRAY and VALUE:
!
!       ARRAY        VALUE   ISRCHI
!     ----------     -----   ------
!     1, 0, 4, 2       4        3
!     1, 0, 4, 2       2        4
!     1, 0, 4, 2       3        0
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
!-    SPICELIB Version 1.1.0, 03-JUL-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
!
!-&


! $ Index_Entries
!
!     search in an integer array
!
!-&


!
!     Local variables
!
      integer i


      isrchi = 0

      do i = 1, ndim

         if ( array(i) .eq. value ) then
            isrchi = i
            return
         end if

      end do

      return
      end
