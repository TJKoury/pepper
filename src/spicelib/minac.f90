!$Procedure MINAC  ( Minimum element of array, character )

      subroutine minac ( array, ndim, minval, loc )

! $ Abstract
!
!     Locate the minimum element of a character array.
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

      character*(*)   array ( * )
      integer         ndim
      character*(*)   minval
      integer         loc

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     ARRAY      I   Array.
!     NDIM       I   Number of elements in ARRAY.
!     MINVAL     O   Minimum value in ARRAY.
!     LOC        O   Location of MINVAL in ARRAY.
!
! $ Detailed_Input
!
!     ARRAY    is an arbitrary array.
!
!     NDIM     is the number of elements in ARRAY.
!
! $ Detailed_Output
!
!     MINVAL   is the value in array that is less than or equal
!              to all other values in the array. If the array
!              contains more than one element with this value,
!              the first one is returned.
!
!              Elements in character arrays are compared according
!              to the ASCII collating sequence.
!
!     LOC      is the location of the minimum element. That is,
!              MINVAL contains element ARRAY(LOC).
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the array is empty (NDIM is less than one), LOC is zero,
!         and MINVAL is not changed.
!
!     2)  If the declared length of MINVAL is too short to contain the
!         entire element, the element is truncated. (The original value
!         can be accessed via LOC.)
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
!     Let array A contain the following elements.
!
!        A(1) = 'Einstein'
!        A(2) = 'Bohr'
!        A(3) = 'Feynman'
!        A(4) = 'Pauli'
!        A(5) = 'Bardeen'
!        A(6) = 'Dirac'
!
!     Then following the call
!
!        CALL MINAC ( A, 6, MINVAL, LOC )
!
!     the values of MINVAL and LOC are 'Bardeen' and 5 respectively.
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
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-APR-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
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
!     minimum element of character array
!
!-&


!
!     Local variables
!
      integer i


      if (ndim .le. 0) then
         loc = 0
         return
      end if

      minval = array(1)
      loc = 1

      do i = 2, ndim
         if (llt (array(i), minval)) then
            minval = array(i)
            loc = i
         end if
      end do

      return
      end
