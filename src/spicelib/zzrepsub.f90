!$Procedure      ZZREPSUB ( Replace one substring with another )

      subroutine zzrepsub ( in, left, right, string, out )
      use pepper_kinds, only: ip

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Replace the substring (LEFT:RIGHT) with a string of any length.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASSIGNMENT
!     CHARACTER
!     STRING
!
! $ Declarations

      implicit none
      character*(*)         in
      integer(ip)           left
      integer(ip)           right
      character*(*)         string
      character*(*)         out

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     LEFT,
!     RIGHT      I   Ends of substring to be replaced.
!     STRING     I   Replacement string.
!     OUT        O   Resulting string.
!
! $ Detailed_Input
!
!     IN         is an arbitrary character string.
!
!     LEFT,
!     RIGHT      are the ends of the substring to be replaced.
!                Legitimate substrings satisfy the following
!                conditions
!
!                    RIGHT > LEFT - 2
!                    LEFT  > 1
!                    RIGHT < LEN(STRING) + 1
!
!                This allows users to refer to zero-length substrings
!                (null substrings) of IN.
!
!     STRING     is the replacement string. Essentially, the
!                substring (LEFT:RIGHT) is removed from the
!                input string, and STRING is inserted at the
!                point of removal.
!
! $ Detailed_Output
!
!     OUT        is the resulting string. OUT may overwrite IN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If RIGHT is one less than LEFT, the substring to
!        replace will be the null substring.  In this case,
!        STRING will be inserted between IN(:RIGHT) and IN(LEFT:).
!
!     2) If LEFT is smaller than one, it's treated as 1.
!
!     3) If RIGHT is greater than the length of the input string,
!        it is treated as being the length of the string.
!
!     4) If RIGHT is less than LEFT-1, no substitution is made.
!
!     5) Whenever the output string is too small to hold the result,
!        the result is truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Ideally, replacement could be done with simple concatenation,
!
!        OUT = IN(1:LEFT-1) // STRING // IN(RIGHT+1: )
!
!     but the Fortran 77 standard makes this illegal for strings of
!     unknown length.
!
!     This private routine is basically just a copy of the SPICE
!     routine REPSUB with all error handling removed and "reasonable"
!     interpretations used for exceptional cases.
!
! $ Examples
!
!     A typical use for this routine might be to replace all
!     occurrences of one word in a string with another word.
!     For example, the following code fragment replaces every
!     occurrence of the word 'AND' with the word 'OR' in the
!     character string LINE.
!
!        LEFT = WDINDX ( LINE, 'AND' )
!
!        DO WHILE ( LEFT .NE. 0 )
!           CALL REPSUB ( LINE, LEFT, LEFT+2, 'OR', LINE )
!           LEFT = WDINDX ( LINE, 'AND' )
!        END DO
!
!     This routine can also be used to insert substring between
!     two characters.  Consider the string:
!
!         IN   = 'The defendant,, was found innocent.'
!
!     to insert ' Emelda Marcos' between the first and second commas
!     determine the location of the pair ',,'
!
!        RIGHT = POS ( IN, ',,', 1 )
!        LEFT  = RIGHT + 1
!
!     then
!
!        CALL REPSUB ( IN, LEFT, RIGHT, ' Emelda Marcos', OUT )
!
!     The output (OUT) will have the value:
!
!        'The defendant, Emelda Marcos, was found innocent.'
!
! $ Restrictions
!
!     The memory used by STRING and OUT must be disjoint. The memory
!     used by IN and OUT must be identical or disjoint.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 27-APR-1996 (WLT)
!
!
!-&





!
!     SPICELIB functions
!
      integer(ip)           sumai

!
!     Local variables
!
      integer(ip)           inlen
      integer(ip)           strlen
      integer(ip)           outlen

      integer(ip)           remain
      integer(ip)           use         ( 3 )

      integer(ip)           end
      integer(ip)           next
      integer(ip)           i
      integer(ip)           myleft
      integer(ip)           myrght

!
!     Get the lengths of all the strings involved in this transaction.
!
      inlen  = len ( in     )
      strlen = len ( string )
      outlen = len ( out    )
      myleft = min( inlen+1, max(1, left ) )
      myrght = min( inlen,   max(0, right) )
!
!     Reject bad inputs.
!
      if ( myleft .lt. 1 ) then

         myleft = 1

      else if ( myrght .gt. inlen ) then

         myrght = inlen

      else if ( right .lt. left-1 ) then

         return

      end if

!
!     Consider three separate sections:
!
!        1) The front of the original string.
!
!        2) The replacement string.
!
!        3) The end of the original string.
!
!     Determine how much of each section to use in the output string.
!     REMAIN is the number of characters that will fit in the output
!     string.
!
      remain = outlen
      use(1) = min ( remain, myleft - 1 )

      remain = remain - use(1)
      use(2) = min ( remain, strlen )

      remain = remain - use(2)
      use(3) = min ( remain, inlen - right )

!
!     Move the third section first. It gets moved back to front
!     or front to back, depending on whether the replacement string
!     is longer than the original substring. The main thing is to
!     avoid overwriting characters that have yet to be moved.
!
      end = sumai ( use, 3 )

      if ( myleft + strlen .gt. right ) then

         next = end

         do i = use(3), 1, -1
            out(next:next) = in(right+i:right+i)
            next           = next - 1
         end do

      else

         next = myleft + strlen

         do i = 1, use(3)
            out(next:next) = in(right+i:right+i)
            next           = next + 1
         end do

      end if

!
!     The first two sections can be moved directly to the front of
!     the output string.
!
      next = 1

      do i = 1, use(1)
         out(next:next) = in(i:i)
         next           = next + 1
      end do

      do i = 1, use(2)
         out(next:next) = string(i:i)
         next           = next + 1
      end do

!
!     Pad with blanks, if the output string was not filled.
!
      if ( end .lt. outlen ) then
         out(end+1: ) = ' '
      end if


      return
      end
