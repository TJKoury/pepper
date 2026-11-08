!$Procedure BSRCHD ( Binary search for double precision value )

      integer(ip) function bsrchd ( value, ndim, array )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Do a binary search for a given value within a double precision
!     array, assumed to be in nondecreasing order. Return the index of
!     the matching array entry, or zero if the key value is not found.
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
!     VALUE      I   Value to find in ARRAY.
!     NDIM       I   Dimension of ARRAY.
!     ARRAY      I   Array to be searched.
!
!     The function returns the index of VALUE in ARRAY, or zero if not
!     found.
!
! $ Detailed_Input
!
!     VALUE    is the double precision value to be found in the input
!              array.
!
!     NDIM     is the number of elements in the input array.
!
!     ARRAY    is the double precision array to be searched. The
!              elements in ARRAY are assumed to sorted in increasing
!              order.
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
!     A binary search is performed on the input array. If an element of
!     the array is found to match the input value, the index of that
!     element is returned. If no matching element is found, zero is
!     returned.
!
! $ Examples
!
!     Let ARRAY contain the following elements:
!
!             -11.D0
!               0.D0
!              22.491D0
!             750.0D0
!
!     Then
!
!           BSRCHD ( -11.D0,    4, ARRAY )    = 1
!           BSRCHD (  22.491D0, 4, ARRAY )    = 3
!           BSRCHD ( 751.D0,    4, ARRAY )    = 0
!
! $ Restrictions
!
!     1)  ARRAY is assumed to be sorted in increasing order. If this
!         condition is not met, the results of BSRCHD are unpredictable.
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
!-    SPICELIB Version 1.1.0, 26-OCT-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section. Improved $Detailed_Output
!        section.
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
!     binary search for d.p. value
!
!-&


!
!     Local variables
!
      integer(ip)      left
      integer(ip)      right
      integer(ip)      i



!
!     Set the initial bounds for the search area.
!
      left  = 1
      right = ndim

      do while ( left .le. right )

!
!        Check the middle element.
!
         i = (left+right)/2

!
!        If the middle element matches, return its location.
!
         if ( value .eq. array(i) ) then
            bsrchd = i
            return

!
!        Otherwise narrow the search area.
!
         else if ( value .lt. array(i) ) then
            right = i-1
         else
            left  = i+1
         end if

      end do

!
!     If the search area is empty, return zero.
!
      bsrchd = 0

      return
      end
