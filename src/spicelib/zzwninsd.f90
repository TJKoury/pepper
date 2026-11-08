!$Procedure ZZWNINSD ( Insert an interval into a DP window )

      subroutine zzwninsd ( left, right, context, window )

! $ Abstract
!
!      Insert an interval into a double precision window.
!
! $ Required_Reading
!
!      WINDOWS
!
! $ Keywords
!
!      WINDOWS
!
! $ Declarations

      implicit none

      include                'errhnd.inc'

      integer                 lbcell
      parameter             ( lbcell = -5 )

      double precision        left
      double precision        right
      character*(*)           context
      double precision        window    ( lbcell:* )

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      LEFT,
!      RIGHT      I   Left, right endpoints of new interval.
!      CONTEXT    I   A call explanation string.
!      WINDOW    I,O  Input, output window.
!
! $ Detailed_Input
!
!      LEFT,
!      RIGHT       are the left and right endpoints of the interval
!                  to be inserted.
!
!      CONTEXT     a context/explanation string to append to the
!                  long error message if an error signals. The caller
!                  need not include a message. A single blank, ' ',
!                  represents no message.
!
!      WINDOW      on input, is a window containing zero or more
!                  intervals.
!
! $ Detailed_Output
!
!      WINDOW      on output, is the original window following the
!                  insertion of the interval from LEFT to RIGHT.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If LEFT is greater than RIGHT, the error SPICE(BADENDPOINTS) is
!        signaled.
!
!     2) If the insertion of the interval causes an excess of elements,
!        the error SPICE(WINDOWEXCESS) is signaled.
!
! $ Files
!
!      None.
!
! $ Particulars
!
!      This routine inserts the interval from LEFT to RIGHT into the
!      input window. If the new interval overlaps any of the intervals
!      in the window, the intervals are merged. Thus, the cardinality
!      of the input window can actually decrease as the result of an
!      insertion. However, because inserting an interval that is
!      disjoint from the other intervals in the window can increase the
!      cardinality of the window, the routine signals an error.
!
!      This is the only unary routine to signal an error. No
!      other unary routine can increase the number of intervals in
!      the input window.
!
!      If a non-blank CONTEXT string passes from the caller, any error
!      signal will return the long error message with the CONTEXT
!      string appended to that message.
!
! $ Examples
!
!      Let WINDOW contain the intervals
!
!            [ 1, 3 ]  [ 7, 11 ]  [ 23, 27 ]
!
!      Then the following series of calls
!
!            CALL ZZWNINSD ( 5,  5, CONTEXT, WINDOW)       (1)
!            CALL ZZWNINSD ( 4,  8, CONTEXT, WINDOW)       (2)
!            CALL ZZWNINSD ( 0, 30, CONTEXT, WINDOW)       (3)
!
!      produces the following series of windows
!
!            [ 1,  3 ]  [ 5,  5 ]  [  7, 11 ]  [ 23, 27 ]   (1)
!            [ 1,  3 ]  [ 4, 11 ]  [ 23, 27 ]               (2)
!            [ 0, 30 ]                                      (3)
!
! $ Restrictions
!
!      None.
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!      K.R. Gehringer  (JPL)
!      N.J. Bachman    (JPL)
!      H.A. Neilan     (JPL)
!      W.L. Taber      (JPL)
!      I.M. Underwood  (JPL)
!
! $ Version
!
!-     SPICELIB Version 1.1.1, 05-OCT-2021 (NJB)
!
!         Corrected typos in long error messages and in comments.
!
!-     SPICELIB Version 1.0.0, 03-MAR-2009 (EDW)
!
!         This routine is a copy of the SPICELIB WNINSD routine
!         changed only by the addition of the CONTEXT string.
!
!-&

! $ Index_Entries
!
!     insert interval into d.p. window, with context string
!
!-&


!
!     SPICELIB functions
!
      integer               cardd
      integer               sized
      integer               lastnb
      logical               return

!
!     Local Variables
!
      integer               size
      integer               card

      integer               i
      integer               j

!
!     Local parameters
!
      character*(lmsgln)    msg

!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'ZZWNINSD' )
      end if

!
!     Get the size and cardinality of the window.
!
      size = sized ( window )
      card = cardd ( window )

!
!     Let's try the easy cases first. No input interval? No change.
!     Signal that an error has occurred and set the error message.
!
      if ( left .gt. right ) then

         msg = 'Left endpoint greater-than right. Left endpoint ' &
            // 'was #1. Right endpoint was #2.'

         msg = msg(1: lastnb(msg) ) //' '// context(1: lastnb(context) )

         call setmsg ( msg)
         call errdp  ( '#1', left  )
         call errdp  ( '#2', right )
         call sigerr ( 'SPICE(BADENDPOINTS)' )

         call chkout ( 'ZZWNINSD' )
         return

      else if ( card .eq. 0  .or.  left .gt. window(card) ) then

!
!        Empty window? Input interval later than the end of the window?
!        Just insert the interval, if there's room.
!
         if ( size .ge. card+2 ) then

            call scardd ( card+2, window )
            window(card+1) = left
            window(card+2) = right

         else

            msg = 'Window has size, #1, cardinality #2. Cannot ' &
               // 'insert an additional interval into the ' &
               // 'window.'

            msg = msg(1: lastnb(msg) ) //' '// &
                  context(1: lastnb(context) )

            call setmsg ( msg)
            call errint ( '#1', size  )
            call errint ( '#2', card )
            call sigerr ( 'SPICE(WINDOWEXCESS)' )

         end if

         call chkout ( 'ZZWNINSD' )
         return

      end if


!
!     Now on to the tougher cases.
!
!     Skip intervals which lie completely to the left of the input
!     interval. (The index I will always point to the right endpoint
!     of an interval).
!
      i = 2

      do while ( i .le. card  .and.  window(i) .lt. left )
         i = i + 2
      end do

!
!     There are three ways this can go. The new interval can:
!
!        1) lie entirely between the previous interval and the next.
!
!        2) overlap the next interval, but no others.
!
!        3) overlap more than one interval.
!
!     Only the first case can possibly cause an overflow, since the
!     other two cases require existing intervals to be merged.
!

!
!     Case (1). If there's room, move succeeding intervals back and
!     insert the new one. If there isn't room, signal an error.
!
      if ( right .lt. window(i-1) ) then

         if ( size .ge. card+2 ) then

            do j = card, i-1, -1
               window(j+2) = window(j)
            end do

            call scardd ( card+2, window )
            window(i-1) = left
            window(i  ) = right

         else

            msg = 'Window has size, #1, cardinality #2. Cannot ' &
               // 'insert an additional interval into the ' &
               // 'window. The new interval lies entirely ' &
               // 'between the previous interval and the' &
               // 'next.'

            msg = msg(1: lastnb(msg) ) //' '// &
                  context(1: lastnb(context) )

            call setmsg ( msg)
            call errint ( '#1', size  )
            call errint ( '#2', card )
            call sigerr ( 'SPICE(WINDOWEXCESS)' )

            call chkout ( 'ZZWNINSD' )
            return

         end if

!
!     Cases (2) and (3).
!
      else

!
!        The left and right endpoints of the new interval may or
!        may not replace the left and right endpoints of the existing
!        interval.
!
         window(i-1) = min ( left,  window(i-1) )
         window(i  ) = max ( right, window(i  ) )

!
!        Skip any intervals contained in the one we modified.
!        (Like I, J always points to the right endpoint of an
!        interval.)
!
         j = i + 2

         do while ( j .le. card  .and.  window(j) .le. window(i) )
            j = j + 2
         end do

!
!        If the modified interval extends into the next interval,
!        merge the two. (The modified interval grows to the right.)
!
         if ( j .le. card  .and.  window(i) .ge. window(j-1) ) then
            window(i) = window(j)
            j         = j + 2
         end if

!
!        Move the rest of the intervals forward to take up the
!        spaces left by the absorbed intervals.
!
         do while ( j .le. card )
            i           = i + 2
            window(i-1) = window(j-1)
            window(i  ) = window(j  )
            j           = j + 2
         end do

         call scardd ( i, window )

      end if

      call chkout ( 'ZZWNINSD' )

      return
      end
