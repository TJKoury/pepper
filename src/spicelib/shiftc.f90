!$Procedure SHIFTC ( Shift a character string )

      subroutine shiftc ( in, dir, nshift, fillc, out )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Shift the contents of a character string to the left or right.
!     Characters moved past the beginning or end of the string are
!     lost. Vacant spaces are filled with a specified character.
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

      character(*)       in
      character(1)       dir
      integer(ip)        nshift
      character(1)       fillc
      character(*)       out

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     DIR        I   Direction to shift.
!     NSHIFT     I   Number of times to shift.
!     FILLC      I   Character to fill spaces left vacant.
!     OUT        O   Shifted string.
!
! $ Detailed_Input
!
!     IN       is the input character string.
!
!     DIR      is the direction in which the characters in the
!              string are to be shifted.
!
!                    'L' or 'l'  to shift left.
!                    'R' or 'r'  to shift right.
!
!     NSHIFT   is the number of times the string is to be
!              shifted.
!
!     FILLC    is the character with which spaces left vacant by
!              the shift are to be filled.
!
! $ Detailed_Output
!
!     OUT      is the output string. This is the input string,
!              shifted N times, filled with FILLC.
!
!              OUT may overwrite IN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  A negative shift in one direction is equal to a positive
!         shift in the other.
!
!     2)  If a legal direction ('L', 'l', 'R', 'r') is not supplied,
!         the error SPICE(ILLEGSHIFTDIR) is signaled.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The first NSHIFT characters of the output string are filled
!     with the fill character, and the input string is appended.
!
! $ Examples
!
!     If FILLC = ' '
!
!            'abcde'   shifted left twice becomes     'cde  '
!            'abcde'   shifted right once becomes     ' abcd'
!
!     If FILLC = '.'
!
!            '12345 '  shifted right once becomes     '.12345'
!            'Apple '  shifted left ten times becomes '......'
!
! $ Restrictions
!
!     1)  SHIFTC is being maintained for historical reasons only.
!         To avoid the overhead imposed by the error handling in this
!         routine, use the equivalent routines SHIFTL and SHIFTR.
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
!
!-&


! $ Index_Entries
!
!     shift a character_string
!
!-&


! $ Revisions
!
!-    Beta Version 1.1.0, 17-OCT-1988 (IMU)
!
!        Dick Simpson reported that the statement
!
!           OUT(N+1: ) = IN
!
!        which began the right-shift section failed on his Data
!        General, presumably because it requires temporary buffering
!        of characters. The new version seems to work for all cases.
!        It has been tested on the VAX and on the Sun (f77 compiler).
!
!-&


!
!     SPICELIB functions
!
      logical               return



!
!     Standard SPICE error handling.
!
      if ( return () ) return


!
!     Hand off to one of the other routines.
!
      if ( dir == 'L' .or. dir == 'l' ) then

         if ( nshift >= 0 ) then
            call shiftl ( in, nshift, fillc, out )

         else
            call shiftr ( in, -nshift, fillc, out )
         end if

      else if ( dir == 'R' .or. dir == 'r' ) then

         if ( nshift >= 0 ) then
            call shiftr ( in, nshift, fillc, out )

         else
            call shiftl ( in, -nshift, fillc, out )
         end if

      else
         call setmsg ( 'Shift direction (#) must be L, l, R, or r.' )
         call errch  ( '#', dir )

         call sigerr ( 'SPICE(ILLEGSHIFTDIR)' )
      end if

      return
      end
