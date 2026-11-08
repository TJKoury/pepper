!$Procedure BSRCHC ( Binary search for a character string )

      integer(ip) function bsrchc ( value, ndim, array )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Do a binary search for a given value within a character array,
!     assumed to be in nondecreasing order. Return the index of the
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

      implicit none

      character*(*)    value
      integer(ip)      ndim
      character*(*)    array ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VALUE      I   Key value to be found in ARRAY.
!     NDIM       I   Dimension of ARRAY.
!     ARRAY      I   Character string array to search.
!
!     The function returns the index of the first matching array element
!     or zero if the value is not found.
!
! $ Detailed_Input
!
!     VALUE    is the key value to be found in the array. Trailing
!              blanks in this key are not significant: string matches
!              found by this routine do not require trailing blanks in
!              value to match those in the corresponding element of
!              array.
!
!     NDIM     is the number of elements in the input array.
!
!     ARRAY    is the array of character strings to be searched.
!              Trailing blanks in the strings in this array are not
!              significant. The elements in ARRAY are assumed to
!              sorted according to the ASCII collating sequence.
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
!     1)  If NDIM < 1, the value of the function is zero. This is not
!         considered an error.
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
!           'BOHR'
!           'EINSTEIN'
!           'FEYNMAN'
!           'GALILEO'
!           'NEWTON'
!
!     Then
!
!           BSRCHC ( 'NEWTON',   5, ARRAY )    = 5
!           BSRCHC ( 'EINSTEIN', 5, ARRAY )    = 2
!           BSRCHC ( 'GALILEO',  5, ARRAY )    = 4
!           BSRCHC ( 'Galileo',  5, ARRAY )    = 0
!           BSRCHC ( 'BETHE',    5, ARRAY )    = 0
!
! $ Restrictions
!
!     1)  ARRAY is assumed to be sorted in increasing order according to
!         the ASCII collating sequence. If this condition is not met,
!         the results of BSRCHC are unpredictable.
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
!        unnecessary $Revisions section. Improved $Detailed_Input and
!        $Detailed_Output section.
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
!     binary search for a character_string
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
            bsrchc = i
            return

!
!        Otherwise narrow the search area.
!
         else if ( llt (value, array(i)) ) then
            right = i-1
         else
            left  = i+1
         end if

      end do

!
!     If the search area is empty, return zero.
!
      bsrchc = 0

      return
      end
