!$Procedure ESRCHC ( Equivalence search, character )

      integer function esrchc ( value, ndim, array )

! $ Abstract
!
!     Search for a given value within a character string array.
!     Return the index of the first equivalent array entry, or zero
!     if no equivalent element is found.
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
      integer          ndim
      character*(*)    array ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     VALUE      I   Key value to be found in ARRAY.
!     NDIM       I   Dimension of ARRAY.
!     ARRAY      I   Character string array to search.
!
!     The function returns the index of the first array entry equivalent
!     to VALUE, or zero if none is found.
!
! $ Detailed_Input
!
!     VALUE    is an arbitrary character string.
!
!     NDIM     is the dimension of (number of elements in) an array of
!              character strings.
!
!     ARRAY    is the array.
!
! $ Detailed_Output
!
!     The function returns the index of the first element of the input
!     array equivalent to the input value, or zero if the array contains
!     no such elements.
!
!     Two strings are equivalent if they contain the same characters in
!     the same order, when blanks are ignored and uppercase and
!     lowercase characters are considered equal.
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
!     ESRCHC is identical to ISRCHC, except that it looks for the first
!     equivalent string (as defined by EQSTR) instead of the first
!     identical one.
!
! $ Examples
!
!     Let ARRAY contain the following elements:
!
!        ARRAY(1) = 'This'
!        ARRAY(2) = 'little'
!        ARRAY(3) = 'piggy'
!        ARRAY(4) = 'went'
!        ARRAY(5) = 'to'
!        ARRAY(6) = 'market'
!
!     Then
!
!        ESRCHC ( 'PIGGY',      6, ARRAY )  =  3
!        ESRCHC ( ' LiTtLe  ',  6, ARRAY )  =  2
!        ESRCHC ( 'W e n t',    6, ARRAY )  =  4
!        ESRCHC ( 'mall',       6, ARRAY )  =  0
!
! $ Restrictions
!
!     1)  ESRCHC assumes that the function EQSTR does not participate
!         in normal SPICELIB error handling.
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
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
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
!     search array for equivalent character_string
!
!-&


!
!     SPICELIB functions
!
      logical               eqstr

!
!     Local variables
!
      integer               i


!
!     Just like ISRCHC.
!
      esrchc = 0

      do i = 1, ndim

         if ( eqstr ( array(i), value ) ) then
            esrchc = i
            return
         end if

      end do

      return
      end
