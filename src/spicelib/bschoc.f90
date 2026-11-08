!$Procedure BSCHOC ( Binary search with order vector, character )

      integer(ip) function bschoc ( value, ndim, array, order )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Do a binary search for a given value within an array of character
!     strings, accompanied by an order vector. Return the index of the
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
      integer(ip)      order ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VALUE      I   Key value to be found in ARRAY.
!     NDIM       I   Dimension of ARRAY.
!     ARRAY      I   Character string array to search.
!     ORDER      I   Order vector.
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
!              significant.
!
!     ORDER    is an order array that can be used to access the elements
!              of ARRAY in order (according to the ASCII collating
!              sequence). The contents of ORDER are a permutation of
!              the sequence of integers ranging from 1 to NDIM.
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
!     A binary search is performed on the input array, whose order is
!     given by an associated order vector. If an element of the array is
!     found to match the input value, the index of that element is
!     returned. If no matching element is found, zero is returned.
!
! $ Examples
!
!     The numerical results shown for this example may differ across
!     platforms. The results depend on the SPICE kernels used as
!     input, the compiler and supporting libraries, and the machine
!     specific arithmetic implementation.
!
!     1) Search for different character strings in an array that
!        is sorted following a given criteria, not necessarily
!        alphabetically.
!
!        Example code begins here.
!
!
!              PROGRAM BSCHOC_EX1
!              IMPLICIT NONE
!
!        C
!        C     SPICELIB functions.
!        C
!              INTEGER                 BSCHOC
!
!        C
!        C     Local constants.
!        C
!              INTEGER                 NDIM
!              PARAMETER             ( NDIM   = 5  )
!
!              INTEGER                 STRLEN
!              PARAMETER             ( STRLEN = 8  )
!
!        C
!        C     Local variables.
!        C
!              CHARACTER*(STRLEN)      ARRAY  ( NDIM )
!              CHARACTER*(STRLEN)      NAMES  ( NDIM )
!
!              INTEGER                 I
!              INTEGER                 IDX
!              INTEGER                 ORDER  ( NDIM )
!
!
!        C
!        C     Let ARRAY and ORDER contain the following elements:
!        C
!              DATA                    ARRAY  / 'FEYNMAN', 'BOHR',
!             .                     'EINSTEIN', 'NEWTON',  'GALILEO' /
!
!              DATA                    ORDER  / 2, 3, 1, 5, 4 /
!
!        C
!        C     Set the list of NAMES to be searched.
!        C
!              DATA                    NAMES /  'NEWTON',  'EINSTEIN',
!             .                      'GALILEO', 'Galileo', 'BETHE'    /
!
!        C
!        C     Search for the NAMES.
!        C
!              DO I = 1, NDIM
!
!                 IDX = BSCHOC ( NAMES(I), NDIM, ARRAY, ORDER )
!
!                 IF ( IDX .EQ. 0 ) THEN
!
!                    WRITE(*,*) 'Name ', NAMES(I),
!             .                 ' not found in ARRAY.'
!
!                 ELSE
!
!                    WRITE(*,*) 'Name ', NAMES(I),
!             .                 ' found in position', IDX
!
!                 END IF
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
!         Name NEWTON   found in position           4
!         Name EINSTEIN found in position           3
!         Name GALILEO  found in position           5
!         Name Galileo  not found in ARRAY.
!         Name BETHE    not found in ARRAY.
!
!
!        Note that these results indicate that:
!
!            ARRAY(4) = 'NEWTON'
!            ARRAY(3) = 'EINSTEIN'
!            ARRAY(5) = 'GALILEO'
!
! $ Restrictions
!
!     1)  ORDER is assumed to give the order of the elements of ARRAY in
!         increasing order according to the ASCII collating sequence. If
!         this condition is not met, the results of BSCHOC are
!         unpredictable.
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
!-    SPICELIB Version 1.0.1, 17-JUN-2021 (JDR)
!
!        Edited the header to comply with NAIF standard. Added complete
!        code example.
!
!        Updated $Brief_I/O, $Detailed_Input and $Detailed_Output
!        sections to improve the description of the arguments and
!        returned values of the function.
!
!-    SPICELIB Version 1.0.0, 18-SEP-1995 (IMU) (WLT)
!
!-&


! $ Index_Entries
!
!     binary search for a string using an order vector
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
         if ( value .eq. array(order(i)) ) then
            bschoc = order(i)
            return

!
!        Otherwise narrow the search area.
!
         else if ( llt (value, array(order(i))) ) then
            right = i-1
         else
            left  = i+1
         end if

      end do

!
!     If the search area is empty, return zero.
!
      bschoc = 0

      return
      end
