!$Procedure REMLAC ( Remove elements from a character array )

      subroutine remlac ( ne, loc, array, na )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Remove one or more elements from a character array at the
!     indicated location.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!     ASSIGNMENT
!
! $ Declarations

      implicit none

      integer(ip)      ne
      integer(ip)      loc
      character(*)     array ( * )
      integer(ip)      na

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NE         I   Number of elements to be removed.
!     LOC        I   Location of the first removed element.
!     ARRAY     I-O  Input/output array.
!     NA        I-O  Number of elements in the input/output array.
!
! $ Detailed_Input
!
!     NE       is the number of elements to be removed.
!
!     LOC      is the location in the array at which the first
!              element is to be removed.
!
!     ARRAY    on input, is the original array.
!
!     NA       on input, is the number of elements in ARRAY.
!
! $ Detailed_Output
!
!     ARRAY    on output, is the original array with elements
!              LOC through LOC+NE-1 removed. Succeeding elements
!              are moved forward to fill the vacated spaces.
!
!     NA       on output, is the number of elements in ARRAY.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If LOC is not in the interval [1, NA], the error
!         SPICE(INVALIDINDEX) is signaled.
!
!     2)  If the number of elements to be removed is greater than the
!         number of elements that can be removed, the error
!         SPICE(NONEXISTELEMENTS) is signaled.
!
!     3)  If NE is less than one, the array is not modified.
!
!     4)  If NA is less than one, any location is invalid, and, the
!         error SPICE(INVALIDINDEX) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The elements in positions LOC through LOC+NE-1 are overwritten
!     as the elements beginning at LOC+NE are moved back.
!
! $ Examples
!
!     Let
!
!           NA = 7      ARRAY(1) = 'The'
!                       ARRAY(2) = 'boy'
!                       ARRAY(3) = 'in'
!                       ARRAY(4) = 'the'
!                       ARRAY(5) = 'park'
!                       ARRAY(6) = 'fell'
!                       ARRAY(7) = 'down'
!
!     Then the call
!
!           CALL REMLAC ( 3, 3, ARRAY, NA )
!
!     yields the following result:
!
!           NA = 4      ARRAY(1) = 'The'
!                       ARRAY(2) = 'boy'
!                       ARRAY(3) = 'fell'
!                       ARRAY(4) = 'down'
!
!
!     The following calls would signal errors:
!
!     CALL REMLAC ( 3,  1, ARRAY, -1 )
!     CALL REMLAC ( 3, -1, ARRAY,  7 )
!     CALL REMLAC ( 3,  6, ARRAY,  7 )
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
!-    SPICELIB Version 1.1.0, 05-JUN-2021 (JDR)
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
!     remove elements from a character array
!
!-&


! $ Revisions
!
!-     Beta Version 2.0.0, 1-JAN-1989 (HAN)
!
!         Code was added to handle the following exceptional
!         inputs.
!
!         If the dimension of the array is less than one, any
!         value of LOC is invalid. The old version did not check
!         the dimension of the array, and as a result, its output
!         was unpredictable.
!
!         If the location at which the elements are to be removed is
!         not in the interval [1, NA], an error is signaled.
!         Locations not within that interval refer to non-existent
!         array elements. The old routine did not signal an error.
!         It just returned the original array.
!
!         If the number of elements to be removed is greater than the
!         number of elements can be removed, an error is signaled.
!         In the old version, only those elements that could be
!         removed were removed, and no error was signaled.
!
!-&


!
!     SPICELIB functions
!
      logical          return


!
!     Local variables
!
      integer(ip)      i



!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     If LOC does not point to an actual element, signal an error and
!     check out. If the dimension of the array is less than one, any
!     value of LOC is invalid, and an error is signaled.
!

      if ( ( loc < 1 ) .or. ( loc > na ) ) then

         call setmsg ( 'Location was *.'     )
         call errint ( '*', loc              )
         call sigerr ( 'SPICE(INVALIDINDEX)' )
         return

!
!     Don't try to remove non-existent elements.
!

      else if ( ne > na-loc+1 ) then

         call setmsg ( 'Trying to remove non-existent elements.' )
         call sigerr ( 'SPICE(NONEXISTELEMENTS)' )
         return


!
!     If there are elements to be removed, remove them. Otherwise,
!     do not modify the array.
!

      else if ( ne > 0 ) then

!
!        Move the elements forward.
!
         do i = loc, na - ne
            array(i) = array(i+ne)
         end do


!
!        Update the number of elements in the array.
!
         na = na - ne


      end if



      return
      end
