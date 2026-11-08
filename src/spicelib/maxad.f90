!$Procedure MAXAD  ( Maximum element of array, DP )

      subroutine maxad ( array, ndim, maxval, loc )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Locate the maximum element of a DP array.
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

      real(wp)              array ( * )
      integer(ip)           ndim
      real(wp)              maxval
      integer(ip)           loc

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  -------------------------------------------------
!     ARRAY      I   Array.
!     NDIM       I   Number of elements in ARRAY.
!     MAXVAL     O   Maximum value in ARRAY.
!     LOC        O   Location of MAXVAL in ARRAY.
!
! $ Detailed_Input
!
!     ARRAY    is an arbitrary array.
!
!     NDIM     is the number of elements in ARRAY.
!
! $ Detailed_Output
!
!     MAXVAL   is the value in array that is greater than or equal
!              to all other values in the array. If the array
!              contains more than one element with this value,
!              the first one is returned.
!
!              Elements in character arrays are compared according
!              to the ASCII collating sequence.
!
!     LOC      is the location of the maximum element. That is,
!              MAXVAL contains element ARRAY(LOC).
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
!         and MAXVAL is not changed.
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
!        A(1) = 16.D0
!        A(2) =  4.D0
!        A(3) = 32.D0
!        A(4) = 64.D0
!        A(5) =  2.D0
!        A(6) =  8.D0
!
!     Then following the call
!
!        CALL MAXAD ( A, 6, MAXVAL, LOC )
!
!     the values of MAXVAL and LOC are 64.D0 and 4 respectively.
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
!     maximum element of d.p. array
!
!-&


!
!     Local variables
!
      integer(ip) i


      if (ndim .le. 0) then
         loc = 0
         return
      end if

      maxval = array(1)
      loc = 1

      do i = 2, ndim
         if (array(i) .gt. maxval) then
            maxval = array(i)
            loc = i
         end if
      end do


      return
      end
