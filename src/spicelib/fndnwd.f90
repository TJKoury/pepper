!$Procedure FNDNWD ( Find the next word after an index )

      subroutine fndnwd ( string, start, b, e )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Find the beginning and end of the first word starting at
!     or after a specified character.
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
      integer(ip)      start
      integer(ip)      b
      integer(ip)      e

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   A string to examine for words.
!     START      I   Position in the string to start looking for words.
!     B          O   String position of first character of the word.
!     E          O   String position of last character of the word.
!
! $ Detailed_Input
!
!     STRING   is a character string that potentially consists of
!              words of text.
!
!     START    is the index of a letter within the string from which
!              to start looking for the next word.
!
! $ Detailed_Output
!
!     B        is the index of the first letter of the word substring
!              of STRING that begins at or after position START. If
!              there are no such substrings I is returned as 0.
!
!     E        is the index of the last letter of the word substring
!              of STRING that begins at or after position START. If
!              there are no such substrings J is returned as 0.
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
!     Given a character string and location of a character within that
!     string, this routine finds the first full word of the string
!     that starts on or after the specified location.
!
! $ Examples
!
!     1         2         3         4         5
!              12345678901234567890123456789012345678901234567890
!     STRING: 'Now is the time for all good men to go home to bed'
!
!     START    I      J
!     -----   ---    ---
!     1        1      3
!     2        5      6
!     3        5      6
!     4        5      6
!     5        5      6
!     6        8      10
!     7        8      10
!     8        8      10
!     9        12     15
!
!     48       48     50
!     49       0      0
!     111      0      0
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
!-    SPICELIB Version 2.1.0, 12-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 2.0.0, 15-OCT-1993 (WLT)
!
!        The routine was completely rewritten with a resulting
!        increase in execution speed of between 2000% and 6000%.
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
!     find the next word after an index
!
!-&


! $ Revisions
!
!-     SPICELIB Version 2.0.0, 15-OCT-1993 (WLT)
!
!         The routine was completely rewritten with a resulting
!         increase in execution speed of between 2000% and 6000%.
!         It was tested against the old version of the routine to
!         ensure that the functionality was exactly duplicated.
!
!-&


!
!     Local Variables
!
      integer(ip)           blank
      integer(ip)           l
      integer(ip)           i
      integer(ip)           n
      integer(ip)           size

      logical               lastn
      logical               thisb

!
!     Set up neede parameters and check obvious out-of-bound cases.
!
      blank = ichar(' ')
      size  = len(string)

      if ( start .gt. size ) then
         b = 0
         e = 0
         return
      end if

      n = max(1,start)
      l = n - 1

      if ( l .le. 0 ) then
         lastn = .false.
      else
         lastn = ichar( string(l:l) ) .ne. blank
      end if

      thisb = ichar( string(n:n) ) .eq. blank

!
!     Search for the beginning of a word (the last character
!     blank and the current non-blank).
!
      do while ( thisb .or. lastn )

         n = n + 1

         if ( n .gt. size ) then
            b = 0
            e = 0
            return
         end if

         lastn = .not.   thisb
         thisb =  ichar( string(n:n) ) .eq. blank

      end do

!
!     If we get this far, we found the beginning of the
!     string.  To find the end look for the next blank and
!     back up one.
!
      b     =  n

      do i = n+1, size

         if ( ichar(string(i:i)) .eq. blank ) then
            e = i - 1
            return
         end if

      end do

!
!     If we get this far, the word ends at the end of the
!     string.
!
      e = size
      return

      end
