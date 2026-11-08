!$Procedure CYAIIP ( Cycle the elements of an integer array, in place )

      subroutine cyaiip ( nelt, dir, ncycle, array )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Cycle the elements of an integer array forward or backward
!     in place.
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

      integer(ip)           nelt
      character(1)          dir
      integer(ip)           ncycle
      integer(ip)           array    ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NELT       I   Number of elements.
!     DIR        I   Direction to cycle: 'F' or 'B'.
!     NCYCLE     I   Number of times to cycle.
!     ARRAY     I-O  Array to be cycled/cycled array.
!
! $ Detailed_Input
!
!     NELT     is the number of elements in the input array.
!
!     DIR      is the direction in which the elements in the
!              array are to be cycled.
!
!                 'F' or 'f'  to cycle forward.
!                 'B' or 'b'  to cycle backward.
!
!     NCYCLE   is the number of times the elements in the array
!              are to be cycled.
!
!     ARRAY    is the array to be cycled.
!
! $ Detailed_Output
!
!     ARRAY    is the input array after it has been cycled.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If the value of DIR is not recognized, the error
!         SPICE(INVALIDDIRECTION) is signaled.
!
!     2)  If NELT is less than 1, the output array is not modified.
!
!     3)  If NCYCLE is negative, the array is cycled NCYCLE times in
!         the opposite direction of DIR.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine cycles an integer array in place. To cycle
!     an array and store the result in a new array, use CYCLAI.
!
!     An array is cycled when its contents are shifted forward or
!     backward by one place. An element pushed off one end of the
!     array is brought around to the other end of the array instead
!     of disappearing.
!
! $ Examples
!
!     Let the integer array A contain the following elements.
!
!        A(1) = 1
!        A(2) = 2
!        A(3) = 3
!        A(4) = 4
!
!     Cycling A forward once yields the array
!
!        A(1) = 4
!        A(2) = 1
!        A(3) = 2
!        A(4) = 3
!
!     Cycling A backward once yields the array
!
!        A(1) = 2
!        A(2) = 3
!        A(3) = 4
!        A(4) = 1
!
!     Cycling by any multiple of the number of elements in the array
!     yields the same array.
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
!-    SPICELIB Version 1.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.0, 09-SEP-2005 (NJB) (HAN) (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     cycle the elements of an integer array in place
!
!-&


!
!     SPICELIB functions
!
      integer(ip)      gcd
      logical          return

!
!     Local variables
!
      integer(ip)      last
      integer(ip)      temp

      integer(ip)      g
      integer(ip)      i
      integer(ip)      j
      integer(ip)      k
      integer(ip)      l
      integer(ip)      m


!
!     Standard SPICE error handling.
!
      if ( return () ) return

!
!     Don't even screw around if there are no elements in the array.
!
      if ( nelt < 1 ) return

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!
      if (      ( dir == 'B' ) &
           .or. ( dir == 'b' ) ) then

         k = mod ( - ncycle, nelt )

      else if (      ( dir == 'F' ) &
                .or. ( dir == 'F' ) ) then

         k = mod (   ncycle, nelt )

      else

         call setmsg ( 'Cycling direction was *.' )
         call errch  ( '*', dir                   )
         call sigerr ( 'SPICE(INVALIDDIRECTION)'  )
         return

      end if


      if ( k < 0 ) then

         k = k + nelt

      else if ( k == 0 ) then

         return

      end if


!
!     The algorithm used to cycle arrays is identical to the one used
!     to cycle character strings in CYCLEC. We won't repeat the (rather
!     lengthy) description here.
!
      g = gcd ( k, nelt )
      m = nelt / g

      do i = 1, g

         l    = i
         last = array(l)

         do j = 1, m

            l = l + k

            if ( l > nelt ) then
               l = l - nelt
            end if

            temp     = array(l)
            array(l) = last
            last     = temp

         end do

      end do


      return
      end
