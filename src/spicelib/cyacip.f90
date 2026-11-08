!$Procedure CYACIP ( Cycle the elements of a character array )

      subroutine cyacip ( nelt, dir, ncycle, array )

! $ Abstract
!
!     Cycle the elements of a character array forward or backward
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

      integer               nelt
      character*1           dir
      integer               ncycle
      character*(*)         array    ( * )

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
!     This routine cycles a character array in place. To cycle
!     an array and store the result in a new array, use CYCLAC.
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
!        A(1) = 'apple'
!        A(2) = 'bear'
!        A(3) = 'cake'
!        A(4) = 'dragon'
!
!     Cycling A forward once yields the array
!
!        A(1) = 'dragon'
!        A(2) = 'apple'
!        A(3) = 'bear'
!        A(4) = 'cake'
!
!     Cycling A backward once yields the array
!
!        A(1) = 'bear'
!        A(2) = 'cake'
!        A(3) = 'dragon'
!        A(4) = 'apple'
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
!     cycle the elements of a character array in place
!
!-&


!
!     SPICELIB functions
!
      logical          return
      integer          gcd
      integer          nbwid

!
!     Local variables
!
      integer          outlen
      integer          widest

      character*1      last
      character*1      temp

      integer          c
      integer          g
      integer          i
      integer          j
      integer          k
      integer          l
      integer          m



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'CYACIP' )
      end if

!
!     Don't even screw around if there are no elements in the array.
!
      if ( nelt .lt. 1 ) then
         call chkout ( 'CYACIP' )
         return
      end if

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!     If the cycling direction is not recognized, signal an error.
!
      if (          ( dir .eq. 'B' ) &
               .or. ( dir .eq. 'b' ) ) then

         k = mod ( - ncycle, nelt )

      else if (      ( dir .eq. 'F' ) &
                .or. ( dir .eq. 'f' ) ) then

         k = mod (   ncycle, nelt )

      else

         call setmsg ( 'Cycling direction was *.' )
         call errch  ( '*', dir )
         call sigerr ( 'SPICE(INVALIDDIRECTION)'  )
         call chkout ( 'CYACIP' )
         return

      end if



      if ( k .lt. 0 ) then

         k = k + nelt

      else if ( k .eq. 0 ) then

         call chkout ( 'CYACIP' )
         return

      end if


!
!     The algorithm used to cycle arrays is identical to the one used
!     to cycle character strings in CYCLEC. We won't repeat the (rather
!     lengthy) description here.
!
!     The character version of CYCLAx differs from the other
!     versions in that a single character is cycled at a time. That
!     is, the first trip through the outermost loop cycles the first
!     characters of the array elements; the second trip cycles the
!     second characters; and so on. This allows the same algorithm to
!     be used for all the routines. The local storage required is just
!     a couple of characters.
!
!     Don't swap the ends of strings if they're just blank padded.
!     And don't overwrite the elements of the output array, if they
!     happen to be shorter than those in the input array.
!
      outlen = len   ( array(1)       )
      widest = nbwid ( array,  nelt   )

!
!     The greatest common divisor need only be computed once.
!
      g = gcd ( k, nelt )
      m = nelt / g

!
!     To make this a non-character routine, remove all references to C.
!
      do c = 1, widest

         do i = 1, g

            l    = i
            last = array(l)(c:c)

            do j = 1, m

               l = l + k

               if ( l .gt. nelt ) then
                  l = l - nelt
               end if

               temp          = array(l)(c:c)
               array(l)(c:c) = last
               last          = temp

            end do

         end do

      end do

!
!     If needed, pad the output array with blanks.
!
      if ( outlen .gt. widest ) then

         do i = 1, nelt
            array(i)(widest+1: ) = ' '
         end do

      end if


      call chkout ( 'CYACIP' )
      return
      end
