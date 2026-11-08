!$Procedure RJUST ( Right justify a character string )

      subroutine rjust ( input, output )

! $ Abstract
!
!     Right justify a character string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!     STRING
!
! $ Declarations

      implicit none

      character*(*)    input
      character*(*)    output

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INPUT      I   Input character string.
!     OUTPUT     O   Output character string, right justified.
!
! $ Detailed_Input
!
!     INPUT    is the input character string.
!
! $ Detailed_Output
!
!     OUTPUT   is the output character string, right justified.
!              If INPUT is too large to fit into OUTPUT, it is
!              truncated on the left.
!
!              OUTPUT may overwrite INPUT.
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
!     Any trailing blanks in the input string are removed, and
!     the remaining string is copied to the output string.
!
! $ Examples
!
!     The following examples should illustrate the use of RJUST.
!
!        'ABCDE          '   becomes  '          ABCDE'
!        'AN EXAMPLE     '            '     AN EXAMPLE'
!        '   AN EXAMPLE  '            '     AN EXAMPLE'
!        '               '            '               '
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
!     right justify a character_string
!
!-&


! $ Revisions
!
!-     Beta Version 1.1.0, 11-DEC-1989 (IMU)
!
!         Did not work on Sun when INPUT and OUTPUT were
!         the same string, and where the initial and final
!         locations of the non-blank part of the string
!         overlapped.
!
!         The solution is to move the characters one by one,
!         starting from the right side of the input string.
!         That way, nothing gets clobbered.
!
!-&


!
!     SPICELIB functions
!
      integer               frstnb
      integer               lastnb

!
!     Local variables
!
      integer               first
      integer               i
      integer               last
      integer               loc
      integer               start



!
!     Blank string? It's all the same.
!
      if ( input .eq. ' ' ) then
         output = input

!
!     Get the first non-blank character. Start OUTPUT at that point.
!
      else
         first  = frstnb ( input )
         last   = lastnb ( input )
         start  = len ( output ) - (last-first)

!
!        If the input string is too long (START < 1), move FIRST
!        up a little to truncate on the left.
!
         if ( start .lt. 1 ) then
            first = first + (1-start)
            start = 1
         end if

!
!        Move the characters in reverse order, to keep from stomping
!        anything if the operation is being done in place.
!
         loc = len ( output )

         do i = last, first, -1
            output(loc:loc) = input(i:i)
            loc = loc - 1
         end do

!
!        Clear the first part of OUTPUT, if necessary.
!
         if ( start .gt. 1 ) then
            output(1:start-1) = ' '
         end if

      end if

      return
      end
