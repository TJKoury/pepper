!$Procedure REPLCH ( Replace characters in a string )

      subroutine replch ( instr, old, new, outstr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Replace all occurrences of a single character with a second
!     character.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASCII
!     CHARACTER
!
! $ Declarations

      implicit none

      character*(*)    instr
      character*1      old
      character*1      new
      character*(*)    outstr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INSTR      I   Input string.
!     OLD        I   Character to be replaced.
!     NEW        I   Replacement character.
!     OUTSTR     O   Output string.
!
! $ Detailed_Input
!
!     INSTR    is the input character string, possibly containing
!              one or more occurrences of the character OLD.
!
!     OLD      is the character to be replaced wherever it occurs in
!              the input string.
!
!     NEW      is the character which is to replace each occurrence
!              of the character OLD in the output string.
!
! $ Detailed_Output
!
!     OUTSTR   is the output string. This is the input string
!              with every occurrence of the character OLD replaced
!              by the character NEW.
!
!              OUTSTR may overwrite INSTR.
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
!     Copy the contents of the input string to the output string
!     a character at a time, replacing each occurrence of OLD with NEW.
!     If the output string is not long enough to contain the input
!     string, it is truncated on the right.
!
! $ Examples
!
!     Let
!           INSTR  = 'Woodsy is the Anti-Pollution Owl.'
!           OLD    = 'O'
!           NEW    = 'E'
!     then
!           OUTSTR = 'Woodsy is the Anti-Pollution Ewl.'
!
!     Note the case-sensitivity of REPLCH. The lowercase o's are
!     not affected.
!
!     REPLCH may similarly be used to replace control characters
!     (such as tab stops, line feeds, and nulls) with regular ASCII
!     characters (such as blanks).
!
! $ Restrictions
!
!     1)  REPLCH is sensitive to case, as shown in the examples above.
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     replace characters in a string
!
!-&


!
!   Local Variables
!
      integer(ip)      i



!
!     Move the input string to the output string. If it's too long,
!     this will truncate it.
!
      outstr = instr

!
!     Check each character of OUTSTR and replace as necessary.
!
      do i = 1, len ( outstr )

         if ( outstr(i:i) .eq. old ) then
            outstr(i:i) = new
         end if

      end do

      return
      end
