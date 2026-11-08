!$Procedure CYCLEC ( Cycle a character string )

      subroutine cyclec ( instr, dir, ncycle, outstr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Cycle the contents of a character string to the left or right.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     UTILITY
!
! $ Declarations

      implicit none

      character(*)       instr
      character(1)       dir
      integer(ip)        ncycle
      character(*)       outstr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INSTR      I   String to be cycled.
!     DIR        I   Direction to cycle.
!     NCYCLE     I   Number of times to cycle.
!     OUTSTR     O   Cycled string.
!
! $ Detailed_Input
!
!     DIR      is the direction in which the characters in the
!              string are to be cycled.
!
!                    'L' or 'l'  to cycle left.
!                    'R' or 'r'  to cycle right.
!
!     NCYCLE   is the number of times the characters in the string
!              are to be cycled.
!
!     INSTR    is the string to be cycled.
!
! $ Detailed_Output
!
!     OUTSTR   is the input string after it has been cycled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the direction flag is not one of the acceptable values
!         'r', 'R', 'l', 'L',  the error SPICE(INVALIDDIRECTION) is
!         signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     A string is cycled when its contents are shifted to the left
!     or right by one place. A character pushed off one end of the
!     string is brought around to the other end of the string instead
!     of disappearing.
!
!     Leading and trailing blanks are treated just like any other
!     characters.
!
!     If the output string is not large enough to contain the input
!     string, the cycled string is truncated on the right.
!
! $ Examples
!
!     'abcde'   cycled left twice becomes               'cdeab'
!     'abcde '  cycled left twice becomes               'cde ab'
!     'abcde'   cycled right once becomes               'eabcd'
!     'Apple '  cycled left six times becomes           'Apple '
!     'Apple '  cycled right twenty-four times becomes  'Apple '
!
! $ Restrictions
!
!     1)  The memory used for the output string must be identical to
!         that used for the input string or be disjoint from the input
!         string memory.
!
!         That is:
!
!            CALL CYCLEN ( STRING, DIR, NCYCLE, STRING )
!
!         will produce correct results with output overwriting input.
!
!            CALL CYCLEN ( STRING(4:20), DIR, NCYCLE, STRING(2:18) )
!
!         will produce garbage results.
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
!-    SPICELIB Version 1.2.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 18-JUN-1999 (WLT)
!
!        Fixed problem with unbalanced CHKIN/CHKOUT calls.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
!
!-&


! $ Index_Entries
!
!     cycle a character_string
!
!-&


! $ Revisions
!
!-     Beta Version 1.1.0, 6-FEB-1989 (WLT)
!
!      Error handling for bad direction flag added.
!
!-&


!
!     SPICELIB functions
!
      logical          return
      integer(ip)      gcd

!
!     Local variables
!
      character(1)     last
      character(1)     temp

      integer(ip)      g
      integer(ip)      i
      integer(ip)      j
      integer(ip)      k
      integer(ip)      l
      integer(ip)      m
      integer(ip)      n
      integer(ip)      limit

      if ( return() ) then
         return
      else
         call chkin ( 'CYCLEC' )
      end if
!
!     Get the length of the input string.
!
      n     = len ( instr  )
      limit = len ( outstr )

!
!     A left cycle is the same as a right cycle by the opposite of
!     NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum positive right cycle that is equivalent to the inputs.
!
      if (      (dir == 'L') &
           .or. (dir == 'l') ) then

         k = mod ( - ncycle, n )

      else if (      ( dir == 'R') &
                .or. ( dir == 'r') ) then

         k = mod (   ncycle, n )

      else

         call setmsg ( 'The direction flag should be one of the '     // &
                       'following: ''r'', ''R'', ''l'', ''L''.  It'   // &
                       ' was #.'   )

         call errch  ( '#', dir )
         call sigerr ( 'SPICE(INVALIDDIRECTION)' )
         call chkout ( 'CYCLEC' )
         return

      end if

      if      ( k < 0 ) then
         k = k + n
      else if ( k == 0 ) then
         call chkout ( 'CYCLEC' )
         return
      end if


!
!     As to the method for performing the cycle in place, we need a
!     few preliminaries.
!
!        1.  Since we are performing a cycle on the input string we
!            can regard the letters of the string as being attached
!            to a circle at N equally spaced points.  Thus a cycle
!            by K has the effect of moving the position of each letter
!            to the K'th point from its current position along the
!            circle.  (The first point from its position is the
!            adjacent point.)
!
!        2.  If we start at some point on the circle and begin moves to
!            other points of the circle by always moving K points
!            at a time, how long will it take until we get back to
!            the starting point?  Answer: N/gcd(K,N)
!
!               Justification of the above answer.
!
!               a.  If we count all of the points that we move past or
!                   onto in such a trip (counting second, third, ...
!                   passes), we will find that we have
!                   moved past or onto i*K points after i steps.
!
!               b.  In order to get back to the starting point we will
!                   have to move past or onto a multiple of N points.
!
!               c.  The first time we will get back to the starting
!                   point is the smallest value of i such that i*K
!                   is a multiple of N.  That value is N/g.c.d.(K,N)
!                   where g.c.d stands for the greatest common divisor
!                   of K and N. Lets call this number M.
!
!                      i.  To see that this is the smallest number we
!                          first show that K*M is in fact a multiple of
!                          N.  The product K*M = K * ( N / gcd(K,N) )
!                                              = N * ( K / gcd(K,N) )
!
!                          Since gcd(K,N) evenly divides K, K/gcd(K,N)
!                          is an integer.  Thus K*M = N*I for some
!                          integer I ( = K / gcd(K,N) ).
!
!                      ii. The least common multiple of K and N is:
!                          K*N / gcd(K,N)  thus the first multiple
!                          of K that is also a multiple of N is the
!                          N/ gcd(K,N) 'th multiple of K.
!
!        3.  The closest stopping point on the circle will be gcd(K,N)
!            points away from our starting point.  To see this recall
!            that we make N/gcd(K,N) moves of size K inorder to get
!            back to the starting point.  The stopping points must
!            be equally spaced around the circle since the set of
!            points must look the same from any one of the points
!            visited --- after all we could get the same set by just
!            starting at one of those visited and making N/gcd(K,N)
!            moves.  But the set of N/gcd(K,N) equally space points
!            out of the original N must be gcd(K,N) points apart.
!
!        4.  To visit every point on the circle we could
!
!            a.  Pick a starting point
!            b.  Take N/gcd(K,N) steps of size K (bringing us back
!                to our starting point.
!            c.  move forward 1 point
!            d.  repeat steps a. b. and c. gcd(K,N) times.
!
!        5.  If in addition to moving around the circle by the
!            prescription of 4. above we:
!               a. pick up the letter at a position when we stop there
!                  (starting being the same as stopping)
!               b. put down the letter we had picked up at a previous
!                  point.
!            then we will cycle every letter by the prescribed value
!            of K.
!
!     In this case the code is much shorter than its explanation.
!

      g = gcd ( k, n )
      m = n / g

      do i = 1, g

         l    = i
         last = instr (l:l)

         do j = 1, m

            l = l + k

!
!           Compute L mod N.
!
            if ( l > n ) then
               l = l - n
            end if

            temp        = instr (l:l)

!
!           Make sure there is someplace to put the letter picked up
!           in the previous pass through the loop.
!
            if ( l <= limit ) then
               outstr(l:l) = last
            end if

            last        = temp

         end do

      end do

      call chkout ( 'CYCLEC' )
      return
      end
