!$Procedure CYCLAC ( Cycle the elements of a character array )

      subroutine cyclac ( array, nelt, dir, ncycle, out )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Cycle the elements of a character array forward or backward.
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

      character(*)          array    ( * )
      integer(ip)           nelt
      character(1)          dir
      integer(ip)           ncycle
      character(*)          out      ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     ARRAY      I   Input array.
!     NELT       I   Number of elements.
!     DIR        I   Direction to cycle: 'F' or 'B'.
!     NCYCLE     I   Number of times to cycle.
!     OUT        O   Cycled array.
!
! $ Detailed_Input
!
!     ARRAY    is the array to be cycled.
!
!     NELT     is the number of elements in the input array.
!
!     DIR      is the direction in which the elements in the
!              array are to be cycled.
!
!                    'F' or 'f'  to cycle forward.
!                    'B' or 'b'  to cycle backward.
!
!     NCYCLE   is the number of times the elements in the array
!              are to be cycled.
!
! $ Detailed_Output
!
!     OUT      is the input array after it has been cycled.
!              OUT may overwrite ARRAY.
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
!     An array is cycled when its contents are shifted forward or
!     backward by one place. An element pushed off one end of the
!     array is brought around to the other end of the array instead
!     of disappearing.
!
! $ Examples
!
!     Let the integer array A contain the following elements.
!
!           A(1) = 'apple'
!           A(2) = 'bear'
!           A(3) = 'cake'
!           A(4) = 'dragon'
!
!     Cycling A forward once yields the array
!
!           A(1) = 'dragon'
!           A(2) = 'apple'
!           A(3) = 'bear'
!           A(4) = 'cake'
!
!     Cycling A backward once yields the array
!
!           A(1) = 'bear'
!           A(2) = 'cake'
!           A(3) = 'dragon'
!           A(4) = 'apple'
!
!     Cycling by any multiple of the number of elements in the array
!     yields the same array.
!
! $ Restrictions
!
!     1)  The memory used for the output array must be identical to or
!         disjoint from the memory used for the input array.
!
!         That is:
!
!            CALL CYCLAC ( ARRAY, NELT, DIR, NCYCLE, ARRAY )
!
!         will produce correct results, while
!
!            CALL CYCLAC ( ARRAY, NELT-3, DIR, NCYCLE, ARRAY(4) )
!
!         will produce garbage.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     H.A. Neilan        (JPL)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 18-MAY-2010 (BVS)
!
!        Removed "C$" marker from text in the header.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!         Comment section for permuted index source lines was added
!         following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN)
!
!-&


! $ Index_Entries
!
!     cycle the elements of a character array
!
!-&


! $ Revisions
!
!-     Beta Version 2.0.0, 16-JAN-1989 (HAN)
!
!         Error handling was added to detect an invalid value for
!         the cycling direction. If the direction is not recognized
!         the error SPICE(INVALIDDIRECTION) is signaled and the
!         output array is not modified. (The routine used to copy the
!         input array into the output array if the direction was not
!         recognized.)
!
!         The "Exceptions" section was filled out in more detail.
!
!-&


!
!     SPICELIB functions
!
      logical          return
      integer(ip)      gcd
      integer(ip)      nbwid

!
!     Local variables
!
      integer(ip)      outlen
      integer(ip)      widest
      integer(ip)      limit

      character(1)     last
      character(1)     temp

      integer(ip)      c
      integer(ip)      g
      integer(ip)      i
      integer(ip)      j
      integer(ip)      k
      integer(ip)      l
      integer(ip)      m



!
!     Standard SPICE error handling.
!
      if ( return () ) then
         return
      else
         call chkin ( 'CYCLAC' )
      end if

!
!     Don't even screw around if there are no elements in the array.
!
      if ( nelt < 1 ) then
         call chkout ( 'CYCLAC' )
         return
      end if

!
!     A backward cycle is the same as a forward cycle by the opposite
!     of NCYCLE.  Moreover a cycle by K is the same as a cycle by
!     K + m*N for any integer m.  Thus we compute the value of the
!     minimum forward right cycle that is equivalent to the inputs.
!     If the cycling direction is not recognized, signal an error.
!
      if (          ( dir == 'B' ) &
               .or. ( dir == 'b' ) ) then

         k = mod ( - ncycle, nelt )

      else if (      ( dir == 'F' ) &
                .or. ( dir == 'f' ) ) then

         k = mod (   ncycle, nelt )

      else

         call setmsg ( 'Cycling direction was *.' )
         call errch  ( '*', dir )
         call sigerr ( 'SPICE(INVALIDDIRECTION)'  )
         call chkout ( 'CYCLAC' )
         return

      end if



      if ( k < 0 ) then

         k = k + nelt

      else if ( k == 0 ) then

         call movec  ( array, nelt, out )
         call chkout ( 'CYCLAC' )
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

!
!     Don't swap the ends of strings if they're just blank padded.
!     And don't overwrite the elements of the output array, if they
!     happen to be shorter thAn those in the input array.
!
      outlen = len   ( out(1)         )
      widest = nbwid ( array,  nelt   )
      limit  = min   ( outlen, widest )

!
!     The greatest common divisor need only be computed once.
!
      g = gcd ( k, nelt )
      m = nelt / g

!
!     To make this a non-character routine, remove all references to C.
!
      do c = 1, limit

         do i = 1, g

            l    = i
            last = array(l)(c:c)

            do j = 1, m

               l = l + k

               if ( l > nelt ) then
                  l = l - nelt
               end if

               temp        = array(l)(c:c)
               out(l)(c:c) = last
               last        = temp

            end do

         end do

      end do

!
!     If needed, pad the output array with blanks.
!
      if ( outlen > limit ) then

         do i = 1, nelt
            out(i)(limit+1: ) = ' '
         end do

      end if


      call chkout ( 'CYCLAC' )
      return
      end
