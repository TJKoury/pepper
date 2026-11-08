!$Procedure MOVEI  ( Move a integer array to another )

      subroutine movei ( arrfrm, ndim, arrto )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Copy the elements of one integer array into another
!     array.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ARRAY
!
! $ Declarations

      implicit none

      integer(ip)     arrfrm ( * )
      integer(ip)     ndim
      integer(ip)     arrto  ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ARRFRM     I   Integer array to be moved.
!     NDIM       I   Number of elements to copy, i.e. the dimension
!                    of ARRFRM and ARRTO.
!     ARRTO      O   Destination array.
!
! $ Detailed_Input
!
!     ARRFRM   is an array from which to copy items.
!
!     NDIM     is the number of items to copy.
!
! $ Detailed_Output
!
!     ARRTO    is the array to which items should be copied.
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
!     This routine is simply shorthand for the following 3 lines of
!     code.
!
!             DO I = 1, NDIM
!                ARRTO(I) = ARRFRM(I)
!             END DO
!
! $ Examples
!
!     Often one needs to make a temporary copy of an array so that
!     it can be manipulated without altering the original array.
!     As pointed out in particulars, you could just do this within
!     the code that needs the copy. However, if you have several
!     arrays to copy, you can cut the number of lines of code that
!     are needed by a third.
!
!     For example:
!
!          DO I = 1, 19
!             TEMPA(I) = A(I)
!          END DO
!
!          DO I = 1, 38
!             TEMPB(I) = B(I)
!          END DO
!
!     Can be rewritten as
!
!          CALL MOVEI ( A, 19, TEMPA )
!          CALL MOVEI ( B, 38, TEMPB )
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WMO)
!
!-&


! $ Index_Entries
!
!     move a integer array to another integer array
!
!-&


! $ Revisions
!
!-     Beta Version 1.0.1, 4-FEB-1989 (WLT)
!
!      Header fully filled out.
!
!-&


!
!     Local variables
!
      integer(ip) i


      do i = 1, ndim
        arrto(i) = arrfrm(i)
      end do


      return
      end
