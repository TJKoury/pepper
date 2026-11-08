!$Procedure LJUST ( Left justify a character string )

      subroutine ljust ( input, output )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Left-justify a character string.
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
!     OUTPUT     O   Output character string, left justified.
!
! $ Detailed_Input
!
!     INPUT    is the input character string.
!
! $ Detailed_Output
!
!     OUTPUT   is the output character string, left justified.
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
!     Leading blanks are removed from the input character string.
!     If the output string is not large enough to hold the left
!     justified string, it is truncated on the right.
!
! $ Examples
!
!     The following examples illustrate the use of LJUST.
!
!            'ABCDE'             becomes   'ABCDE'
!            'AN EXAMPLE'                  'AN EXAMPLE'
!            '   AN EXAMPLE  '             'AN EXAMPLE'
!            '               '             ' '
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
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 27-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.0, 29-JUL-2013 (BVS)
!
!        Added the quick return branch for input strings that are
!        already left-justified. Removed the initial check for blank
!        input and changed logic to return an empty string after
!        scanning the input. Re-ordered header sections.
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
!     left justify a character_string
!
!-&


!
!     Local variables
!
      integer(ip)           li
      integer(ip)           lo
      integer(ip)           i
      integer(ip)           j
      integer(ip)           pos


!
!     Is the first character of the input string non-blank? If yes, the
!     input string is already left-justified. There is nothing to do
!     but to assign the input string to the output string.
!
      if ( input(1:1) .ne. ' ' ) then

         output = input

      else

!
!        Get the first non-blank character. Start OUTPUT at that point.
!
         li = len    ( input )
         lo = len    ( output)

         j  = 1

!
!        Set I equal to position of first non-blank character of INPUT.
!
         i   = 0
         pos = 1

         do while ( i .eq. 0 .and. pos .le. li )

            if ( input(pos:pos) .ne. ' ' )  then
               i    =  pos
            else
               pos  =  pos + 1
            end if

         end do

!
!        Did we find a non-blank character? If not, the input string is
!        blank. Set output to blank.
!
         if ( i .eq. 0 ) then

            output = ' '

         else

!
!           I is now the index of the first non-blank character of
!           INPUT.
!
            do while  ( ( i .le. li ) .and. ( j .le. lo ) )
               output(j:j) = input(i:i)
               j           = j + 1
               i           = i + 1
            end do

            if ( j .le. lo ) then
               output(j:) = ' '
            end if

         end if

      end if

      return
      end
