!$Procedure ISRCHD  ( Search in a double precision array )

      integer(ip) function  isrchd ( value, ndim, array )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Search for a given value within a double precision array. Return
!     the index of the first matching array entry, or zero if the key
!     value was not found.
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

      real(wp)           value
      integer(ip)        ndim
      real(wp)           array ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VALUE      I   Key value to be found in ARRAY.
!     NDIM       I   Dimension of ARRAY.
!     ARRAY      I   Double precision array to search.
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
!     ARRAY    is the double precision array to be searched.
!
! $ Detailed_Output
!
!     The function returns the index of the first matching array
!     element in ARRAY. If VALUE is not found, ISRCHD is zero.
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
!     The following table shows the value of ISRCHD given the contents
!     of ARRAY and VALUE:
!
!       ARRAY                         VALUE   ISRCHD
!     ---------------------------     -----   ------
!     1.0D0, 0.0D0, 4.0D0, 2.0D0      4.0D0     3
!     1.0D0, 0.0D0, 4.0D0, 2.0D0      2.OD0     4
!     1.0D0, 0.0D0, 4.0D0, 2.0D0      3.0D0     0
!
! $ Restrictions
!
!     1)  CAUTION must be exercised when comparing floating point
!         numbers for equality. If the numbers in ARRAY or the number in
!         VALUE are the result of computations, then it is likely that
!         strict equality between VALUE and some element of ARRAY will
!         NOT hold (even if the two numbers are very close) unless the
!         numbers are the result of exactly the same computations.
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
!     search in a d.p. array
!
!-&


!
!     Local variables
!
      integer(ip) i


      isrchd = 0

      do i = 1, ndim

         if ( array(i) .eq. value ) then
            isrchd = i
            return
         end if

      end do

      return
      end
