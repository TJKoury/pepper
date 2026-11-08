!$Procedure REPLWD ( Replace a word )

      subroutine replwd ( instr, nth, new, outstr )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Replace the Nth word in a string with a new word.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASSIGNMENT
!     WORD
!
! $ Declarations

      implicit none

      character*(*)    instr
      integer(ip)      nth
      character*(*)    new
      character*(*)    outstr

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     INSTR      I   Input string.
!     NTH        I   Number of the word to be replaced.
!     NEW        I   Replacement word.
!     OUTSTR     O   Output string.
!
! $ Detailed_Input
!
!     INSTR    is the input character string, possibly containing
!              one or more words, where a word is any string of
!              consecutive non-blank characters delimited by a
!              blank or by either end of the string.
!
!     NTH      is the number of the word to be replaced. Words
!              are numbered from one. If NTH is less than one,
!              or greater than the number of words in the string,
!              no replacement is made.
!
!     NEW      is the word which is to replace the specified word
!              in the input string. Leading and trailing blanks
!              are ignored. If the replacement word is blank,
!              the original word is simply removed.
!
! $ Detailed_Output
!
!     OUTSTR   is the output string. This is the input string
!              with the N'th word replaced by the word NEW.
!              Any blanks originally surrounding the replaced
!              word are retained.
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
!     1)  If NEW is blank, then the Nth word is replaced by a single
!         space.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     The effect of this routine is to remove the old word with
!     REMSUB, and insert the replacement word with INSSUB.
!
! $ Examples
!
!     Let
!           INSTR  = '  Woodsy is the Anti-Pollution  Owl.'
!
!     and
!           NEW    = '   an   '
!
!     then the following values of NTH yield the following strings.
!
!           NTH      OUTSTR
!           ---      ------------------------------------------
!            -1      '  Woodsy is the Anti-Pollution  Owl.'
!             0      '  Woodsy is the Anti-Pollution  Owl.'
!             1      '  an is the Anti-Pollution  Owl.'
!             3      '  Woodsy is an Anti-Pollution  Owl.'
!             4      '  Woodsy is the an  Owl.'
!             5      '  Woodsy is the Anti-Pollution  an'
!             6      '  Woodsy is the Anti-Pollution  Owl.'
!
!     Note that in the first, second, and last cases, the string
!     was not changed. Note also that in the next to last case,
!     the final period was treated as part of the fifth word in the
!     string.
!
!     If NEW is ' ', and NTH is 3, then
!
!           OUTSTR = '  Woodsy is Anti-Pollution  Owl.'
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
!        Edited the header to comply with NAIF standard. Removed
!        unnecessary $Revisions section.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT) (HAN) (NJB)
!
!-&


! $ Index_Entries
!
!     replace a word
!
!-&


!
!     SPICELIB functions
!
      integer(ip)           frstnb
      integer(ip)           lastnb

!
!   Local Variables
!
      character*2           short
      integer(ip)           begin
      integer(ip)           i
      integer(ip)           j
      integer(ip)           f
      integer(ip)           l
      integer(ip)           k
      integer(ip)           n
      integer(ip)           shift



!
!     First just shift the input string into the output string,
!     then do everything in place (for the case when the new
!     word is longer than the old one.  When its shorter we'll
!     need to change this scheme slightly.)
!
      outstr = instr
!
!     Where does the word to be replaced begin? If there is none,
!     just return the original string.
!
      call nthwd  ( outstr, nth, short, begin )

      if ( begin .eq. 0 ) then

         return

      end if
!
!     Otherwise, find out where it ends as well.
!
      call fndnwd ( instr, begin, i, j )

!
!     Now insert only the non-blank part of the replacement string.
!     If the replacement string is blank, don't insert anything.
!
      if ( new .ne. ' ' ) then

         f = frstnb ( new )
         l = lastnb ( new )

!
!        Except in the lucky case that the word to insert is the
!        same length as the word it's replacing, we will have
!        to shift right or left by some amount.  Compute the
!        appropriate amount to shift right.
!
         shift = ( l - f ) - ( j - i )

      else

         f     = 1
         l     = 1
         shift = i - j

      end if

      if      ( shift .gt. 0 ) then

!
!        To shift right in place start at the right most character
!        of the string and copy the character SHIFT spaces to the
!        left.
!
         k = len(outstr)
         n = k - shift

         do while ( n .gt. j )
            outstr(k:k) = outstr(n:n)
            k           = k - 1
            n           = n - 1
         end do

!
!        Once the appropriate characters have been shifted out
!        of the way, replace the opened space with the new
!        word.
!
         do while (       ( f .le. l           ) &
                    .and. ( i .le. len(outstr) ) )
            outstr(i:i) = new(f:f)
            f           = f + 1
            i           = i + 1
         end do

      else

!
!        We have a left shift. Fill in the first part of the word
!        we are replacing with the new one.
!
         do while (       ( f .le. l           ) &
                    .and. ( i .le. len(outstr) ) )

            outstr(i:i) = new(f:f)
            f           = f + 1
            i           = i + 1

         end do

!
!        Now starting just past the end of the word we are replacing
!        shift the remainder of string left one character at a time.
!
         if ( shift .lt. 0 ) then

            j = j + 1

            do while (       ( i .le. len(outstr)  ) &
                       .and. ( j .le. len(instr)   ) )

               outstr(i:i) = instr(j:j)
               i           = i + 1
               j           = j + 1
            end do

!
!           Finally pad the string with blanks.
!
            if ( i .le. len(outstr) ) then
               outstr(i:) = ' '
            end if

         end if

      end if

      return
      end
