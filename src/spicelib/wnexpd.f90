!$Procedure WNEXPD ( Expand the intervals of a DP window )

      subroutine wnexpd ( left, right, window )
      use pepper_kinds, only: ip, wp

! $ Abstract
!
!     Expand each of the intervals of a double precision window.
!
! $ Required_Reading
!
!     WINDOWS
!
! $ Keywords
!
!     WINDOWS
!
! $ Declarations

      implicit none

      integer(ip)           lbcell
      parameter           ( lbcell = -5 )

      real(wp)              left
      real(wp)              right
      real(wp)              window  ( lbcell:* )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     LEFT       I   Amount subtracted from each left endpoint.
!     RIGHT      I   Amount added to each right endpoint.
!     WINDOW    I-O  Window to be expanded.
!
! $ Detailed_Input
!
!     LEFT     is the amount to be subtracted from the left endpoint of
!              each interval in the input window. The amount LEFT is
!              signed.
!
!     RIGHT    is the amount to be added to the right endpoint of each
!              interval in the window. The amount RIGHT is signed.
!
!     WINDOW   on input, is a window containing zero or more
!              intervals.
!
! $ Detailed_Output
!
!     WINDOW   on output, is the original window with each of its
!              intervals expanded by LEFT units on the left and
!              RIGHT units on the right.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  The cardinality of the input WINDOW must be even. Left
!         endpoints of stored intervals must be strictly greater than
!         preceding right endpoints. Right endpoints must be greater
!         than or equal to corresponding left endpoints. Invalid window
!         data are not diagnosed by this routine and may lead to
!         unpredictable results.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine expands (lengthens) each of the intervals in
!     the input window. The adjustments are not necessarily symmetric.
!     That is, LEFT units are subtracted from the left endpoint of
!     each interval, and RIGHT units are added to the right endpoint
!     of each interval, where LEFT and RIGHT may be different.
!
!     Intervals are merged when expansion causes them to overlap.
!
! $ Examples
!
!     Let WINDOW contain the intervals
!
!           [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]  [ 29, 29 ]
!
!     Then the following series of calls
!
!           CALL WNEXPD (  2,  1, WINDOW )              (1)
!           CALL WNEXPD ( -2,  2, WINDOW )              (2)
!           CALL WNEXPD ( -2, -1, WINDOW )              (3)
!
!     produces the following series of windows
!
!           [ -1, 4 ]  [ 5, 12 ]  [ 21, 30 ]            (1)
!           [  1, 6 ]  [ 7, 14 ]  [ 23, 32 ]            (2)
!           [  3, 5 ]  [ 9, 13 ]  [ 25, 31 ]            (3)
!
!     Note that intervals may be "expanded" by negative amounts.
!     In the example above, the second call shifts each interval to
!     the right, while the third call undoes the effect of the first
!     call (without restoring the merged intervals).
!
!     Note also that the third call is exactly equivalent to the
!     call
!
!           CALL WNCOND ( 2, 1, WINDOW )
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
!     N.J. Bachman       (JPL)
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 06-JUL-2021 (JDR) (NJB)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!        Added entry #1 in $Exceptions section. Extended LEFT and RIGHT
!        description in $Detailed_Input.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU) (HAN)
!
!-&


! $ Index_Entries
!
!     expand the intervals of a d.p. window
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           cardd
      logical               return

!
!     Local variables
!
      integer(ip)           card
      integer(ip)           gone
      integer(ip)           i
      integer(ip)           j


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Get the cardinality of the window. (The size is not important;
!     this routine can't create any new intervals.)
!
      card = cardd ( window )

!
!     Expand the intervals individually. We'll take care of
!     overlaps later on. Negative expansion may cause some
!     intervals to disappear.
!
      gone = 0

      do i = 1, card, 2
         window(i-gone  ) = window(i)   - left
         window(i-gone+1) = window(i+1) + right

         if ( window(i-gone) > window(i-gone+1) ) then
            gone = gone + 2
         end if
      end do

!
!     Proceed only if at least one interval remains. (If there were
!     no intervals to begin with, we skip the previous loop and come
!     here without delay. Do not pass GO, do not collect $200.)
!
      card = card - gone

      if ( card == 0 ) then
         call scardd ( 0, window )
         return
      end if

!
!     None of the intervals can have extended to completely contain
!     any of the other intervals. (They were all expanded by the
!     same amount. Convince yourself that this is true.) So the first
!     endpoint is still the first endpoint (so to speak).
!
!     Step through the window, looking for the next right endpoint
!     less than the following left endpoint. This marks the end of
!     the new first interval, and the beginning of the new second
!     interval. Keep this up until the last right endpoint has been
!     reached. This remains the last right endpoint.
!
      i = 2
      j = 2

      do while ( j < card )

         if ( window(j) < window(j+1) ) then
            window(i  ) = window(j  )
            window(i+1) = window(j+1)
            i           = i + 2
         end if

         j = j + 2

      end do

      window(i) = window(j)
      call scardd ( i, window )

      return
      end
