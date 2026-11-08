!$Procedure QUOTE ( Enclose in quotes )

      subroutine quote ( in, left, right, out )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Enclose (quote) the non-blank part of a character string
!     between delimiting symbols.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     CHARACTER
!     PARSING
!
! $ Declarations

      implicit none

      character*(*)    in
      character*1      left
      character*1      right
      character*(*)    out

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     LEFT       I   Left delimiter.
!     RIGHT      I   Right delimiter.
!     OUT        O   Output (quoted) string.
!
! $ Detailed_Input
!
!     IN       is the input string to be quoted.
!
!     LEFT,
!     RIGHT    are the left and right delimiters to be used in
!              quoting the input string. These may be the same
!              character (apostrophe, vertical bar), complementary
!              characters (left and right parentheses, brackets,
!              or braces), or two totally unrelated characters.
!
! $ Detailed_Output
!
!     OUT      is the output string. This is the non-blank part
!              of the input string delimited by LEFT and RIGHT.
!              If the output string is not large enough to contain
!              the quoted string, it is truncated on the right.
!              (The right delimiter would be lost in this case.)
!
!              If the input string is blank, the output string is
!              a single quoted blank.
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
!     The first character of the output string is the left delimiter,
!     LEFT. This is followed immediately by the non-blank part of the
!     input string, which is in turn followed by the right delimiter,
!     RIGHT.
!
!     If the input string is blank (has no non-blank characters),
!     a single quoted blank is returned.
!
! $ Examples
!
!     Let
!           IN    = '    This string has leading and trailing blanks  '
!           LEFT  = '('
!           RIGHT = ')'
!
!     Then
!           OUT   = '(This string has leading and trailing blanks)    '
!
!     Or, let IN = '         '. Then OUT = '( )'.
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
!     enclose in quotes
!
!-&


!
!     SPICELIB functions
!
      integer(ip)      frstnb
      integer(ip)      lastnb



!
!     Check for blank string first.
!
      if ( in .eq. ' ' ) then
         out = left
         call suffix ( right, 1, out )

      else
         out = in(frstnb(in) : lastnb(in))
         call prefix ( left,  0, out )
         call suffix ( right, 0, out )
      end if

      return
      end
