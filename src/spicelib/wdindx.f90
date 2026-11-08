!$Procedure WDINDX ( Index of a Word Within a String )

      integer function wdindx ( string, word )

! $ Abstract
!
!     Find the index of a word within a string. If the word does not
!     exist as a word within the string, the value zero is returned.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PARSING
!     SEARCH
!     WORD
!
! $ Declarations

      implicit none

      character*(*)    string
      character*(*)    word

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   String of characters, potentially containing words
!     WORD       I   A string of consecutive printable letters.
!
!     The function returns the location of WORD within STRING.
!
! $ Detailed_Input
!
!     STRING   is a string of characters, potentially containing the
!              word.
!
!              Leading and trailing blanks are not significant in
!              STRING.
!
!     WORD     is a string of consecutive printable characters.
!
!              Leading and trailing blanks are not significant in
!              WORD.
!
! $ Detailed_Output
!
!     The function returns the location of WORD within STRING, providing
!     the index of the first letter of WORD within the input STRING. If
!     WORD does not exist or WORD is blank, the function returns zero.
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
!     A word within a string is a substring beginning and ending with a
!     non-blank characters that is delimited by blanks on each end. (A
!     blank is assumed to precede and follow the first and last
!     characters of a string.)
!
!     Given a word, this routine returns the index of the first letter
!     of the first word of STRING that matches the word.
!
! $ Examples
!
!     STRING:
!                      1         2         3         4
!     WORD    1234567890123456789012345678901234567890123456    WDINDX
!     ------  ----------------------------------------------    ------
!     'POT'   'PUT THE POTATOES IN THE POT'                     25
!     'TOES'                                                    0
!     'PUT'                                                     1
!     'THE'                                                     5
!     'IN THE'                                                  18
!     'THE PO'                                                  0
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
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 03-OCT-2021 (JDR)
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
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT)
!
!-&


! $ Index_Entries
!
!     index of a word within a string
!
!-&


!
!     SPICELIB functions
!
      integer          frstnb
      integer          lastnb

!
!     Local variables
!
      integer          i
      integer          j

      integer          begwd
      integer          endwd

      integer          begstr
      integer          endstr

      integer          strlen
      integer          wdlen

      integer          bgtond

!
!     Find the ends of the word and input string.
!
      endstr = lastnb ( string )
      begstr = frstnb ( string )

      endwd  = lastnb ( word   )
      begwd  = frstnb ( word   )

!
!     Get the offset from the beginning of the word to the end of the
!     word, the word length and the string length.
!
      bgtond =     endwd  - begwd
      wdlen  = 1 + bgtond
      strlen = 1 + endstr - begstr

!
!     We deal with all of the pathologies first...
!
      if (      ( endwd  .lt. 1      ) &
           .or. ( strlen .lt. wdlen  ) ) then

!
!        ... If we got a blank word or a string that is too short, then
!        the index of the word is zero.
!
         wdindx =  0
         return

      else if ( strlen .eq. wdlen ) then

!
!        ... the word and string have the same non-blank length.
!        Either they match up or they don't.  Find out and return.
!
         if ( string(begstr:endstr) .eq. word(begwd:endwd) ) then
            wdindx = begstr
         else
            wdindx = 0
         end if

         return
      end if

!
!     Ok.  Now we've got a realistic case to deal with.  The string
!     length is longer than the word length.  Check to see if we have a
!     match at the beginning of the string.
!
      i      = begstr
      j      = i + bgtond

      if (       ( string(i:j)     .eq. word(begwd:endwd) ) &
           .and. ( string(j+1:j+1) .eq. ' '               )  ) then

         wdindx = i
         return

      end if

!
!     No luck yet?  Search the string until we find a word match or
!     we run out of string to check.
!
      i      = begstr + 1
      j      = i      + bgtond

      do while (       ( j                    .lt. endstr            ) &
                 .and. &
                 .not. (     (string(i  :j  ) .eq. word(begwd:endwd) ) &
                        .and.(string(i-1:i-1) .eq. ' '               ) &
                        .and.(string(j+1:j+1) .eq. ' '               ) &
                       ) &
               )
         i   = i + 1
         j   = j + 1
      end do

!
!     If J equals ENDSTR then no match was found in the interior of the
!     string.  We make a last check at the end.
!
      if ( j .eq. endstr ) then

         if (       ( string(i-1:i-1) .eq. ' '               ) &
              .and. ( string(i  :j  ) .eq. word(begwd:endwd) ) ) then
            wdindx = i
         else
            wdindx = 0
         end if

      else
!
!        The only way to get here is if we exited the above loop before
!        running out of room --- that is we had a word match.  Set
!        the index to the value of "I" that got us out of the loop.
!
         wdindx    = i

      end if

      return

      end
