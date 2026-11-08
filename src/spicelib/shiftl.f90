!$Procedure SHIFTL ( Shift left )

      subroutine shiftl ( in, nshift, fillc, out )

! $ Abstract
!
!     Shift the contents of a character string to the left.
!     Characters moved past the beginning of the input string are
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

      character*(*)      in
      integer            nshift
      character*1        fillc
      character*(*)      out

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     NSHIFT     I   Number of times to shift.
!     FILLC      I   Character to fill spaces left vacant.
!     OUT        O   Shifted string.
!
! $ Detailed_Input
!
!     IN       is the input character string.
!
!     NSHIFT   is the number of times the string is to be
!              shifted. If NSHIFT is negative, OUT will be
!              identical to IN.
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
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     As a string is shifted left or right, the leftmost or
!     rightmost characters of the string disappear (as if pushed
!     off the end of the string). This is .TRUE. regardless of
!     the length of the output string.
!
!     The remaining characters are shifted simultaneously, and
!     the spaces vacated by those characters are filled with a
!     replacement character.
!
! $ Examples
!
!     If FILLC = ' '
!
!        'abcde'   shifted left twice becomes     'cde  '
!        'abcde'   shifted right once becomes     ' abcd'
!
!     If FILLC = '.'
!
!        '12345 '  shifted right once becomes     '.12345'
!        'Apple '  shifted left ten times becomes '......'
!
!     Given the declarations
!
!        CHARACTER*3         SHORT
!        CHARACTER*10        LONG
!
!     The calls
!
!        CALL SHIFTR ( 'abcde ', 2, '-', SHORT )
!        CALL SHIFTR ( 'abcde ', 2, '-', LONG  )
!
!     yield the strings
!
!        SHORT = '--a'
!        LONG  = '--abcd    '
!
!     while the calls
!
!        CALL SHIFTL ( 'abcde ', 2, '-', SHORT )
!        CALL SHIFTL ( 'abcde ', 2, '-', LONG  )
!
!     yield the strings
!
!        SHORT = 'cde'
!        LONG  = 'cde ..     '
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
!     M.J. Spencer       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.1, 22-AUG-2001 (EDW)
!
!        Corrected ENDDO to END DO.
!
!-    SPICELIB Version 2.0.0, 01-SEP-1994 (MJS)
!
!        This version correctly handles negative shifts.
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
!     shift left
!
!-&


!
!     Local variables
!
      integer          inlen
      integer          outlen

      integer          n
      integer          nsave
      integer          nfill
      integer          i
      integer          s

!
!     Get the length of the input, output strings.
!
      inlen  = len ( in  )
      outlen = len ( out )

!
!     If the shift is zero or negative, the string is not changed.
!     If longer than the input string, the entire string is shifted.
!
      s = max ( nshift, 0 )
      n = min ( inlen,  s )

!
!     Figure out how many characters in the input string will
!     be saved (will not be shifted off the end of the string,
!     and will fit in the output string), and how many fill
!     characters will be needed (no more than NSHIFT, no fewer
!     than zero).
!
      nsave =     min ( inlen - n, outlen         )
      nfill = n - max ( 0,         inlen - outlen )

!
!     Move the saved characters to output.
!
      do i = 1, nsave
         out(i:i) = in(i+s:i+s)
      end do

!
!     Add as many fill characters as appropriate.
!
      do i = nsave+1, nsave+nfill
         out(i:i) = fillc
      end do

!
!     Pad the output string with blanks (to cover any previous
!     ugliness there).
!
      if ( outlen .gt. inlen ) then
         out(inlen+1: ) = ' '
      end if

      return
      end
