!$Procedure DXTRCT (Extract Double Precision Values From A String)

      subroutine dxtrct ( keywd,   maxwds ,  string, &
                          nfound,  parsed,   values   )

! $ Abstract
!
!     Locate a keyword and succeeding numeric words within a string.
!     Parse and store the numeric words. Remove the keyword and
!     numeric words from the input string.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PARSING
!     WORD
!
! $ Declarations

      implicit none

      character*(*)      keywd
      integer            maxwds
      character*(*)      string
      integer            nfound
      integer            parsed
      double precision   values ( * )

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     KEYWD      I   Keyword used to mark start of a set of numbers.
!     MAXWDS     I   Maximum number of numeric words that can be parsed
!     STRING    I-O  String potentially containing KEYWD and numbers.
!     NFOUND     O   Number of numeric words found following the KEYWD.
!     PARSED     O   Number of numeric words translated and returned.
!     VALUES     O   The double precision values for the numbers.
!
! $ Detailed_Input
!
!     KEYWD    is a word used to mark the start of a set of numeric
!              words of interest.
!
!     MAXWDS   is the maximum number of numeric words that can be
!              parsed and returned.
!
!     STRING   is a string potentially containing KEYWD and numbers.
!
! $ Detailed_Output
!
!     STRING   is the input string stripped of all parsed
!              numeric words. If there was room available to parse
!              all of the numeric words associated with KEYWD, the
!              keyword that marked the beginning of the parsed
!              numbers in the original string will also be removed.
!
!     NFOUND   is the number of numeric words that were found
!              following KEYWD but preceding the next non-numeric
!              word of the string. If the KEYWD is not present in
!              the string, NFOUND is returned as -1. If the keyword
!              is located but the next word in the string is
!              non-numeric NFOUND will be returned as 0.
!
!     PARSED   is the number of numeric words that were actually
!              parsed and stored in the output array VALUES. If no
!              values are parsed PARSED is returned as 0.
!
!     VALUES   are the double precision values for the parsed
!              numeric words that follow the first occurrence of the
!              keyword but precede the next non-numeric word.
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
!     Definitions:
!
!        WORD           is a set of consecutive non-blank characters
!                       delimited by blanks or the end of the string
!                       that contains them.
!
!        NUMERIC WORD   a word that can be parsed by the
!                       SPICELIB routine NPARSD without error. All
!                       FORTRAN numeric representations are numeric
!                       words. In addition 'PI', 'Pi', 'pI', and 'pi'
!                       are all recognized as having the value:
!
!                          3.1415926535897932384626D0
!
!                       See NPARSD FOR A a full description of
!                       legitimate numeric words.
!
!     Given a string and a keyword this routine locates the first
!     occurrence of the keyword in the string and returns the double
!     precision representations of up to MAXWDS succeeding numeric
!     words. All parsed numeric words are removed from the string.
!     If every numeric word following KEYWD but preceding the next
!     non-numeric word is parsed,  KEYWD will also be removed from
!     the string.
!
!     If the keyword cannot be located in the string, the variable
!     NFOUND will be returned as -1 and the string will be unchanged.
!
!     In all other cases, some part of the string (possibly all of it)
!     will be removed.
!
! $ Examples
!
!     Input   STRING  'LONGITUDE 39.2829  LATITUDE 24.27682'
!             KEYWD   'LONGITUDE'
!             MAXWDS   4
!
!     Output: STRING  '  LATITUDE 24.27682'
!             NFOUND  1
!             PARSED  1
!             VALUES  3.92829D+01
!
!
!
!     Input   STRING  'THIS IS A BAD STRING FOR NUMBERS'
!             KEYWD   'RADIUS'
!             MAXWDS  2
!
!     Output: STRING  'THIS IS A BAD STRING FOR NUMBERS'
!             NFOUND  -1
!             PARSED   0
!             VALUES   (unchanged)
!
!
!
!     Input   STRING  'PRIMES 11  13 17 19 23 NON-PRIMES 12 14 15'
!             KEYWD   'PRIMES'
!             MAXWDS  3
!
!     Output: STRING  'PRIMES  19 23 NON-PRIMES 12 14 15'
!             NFOUND  5
!             PARSED  3
!             VALUES  1.1D+01
!                     1.3D+01
!                     1.7D+01
!
!     Input   STRING  'PRIMES 11  13 17 19 23 NON-PRIMES 12 14 15'
!             KEYWD   'PRIMES'
!             MAXWDS  5
!
!     Output: STRING  ' NON-PRIMES 12 14 15'
!             NFOUND  5
!             PARSED  5
!             VALUES  1.1D+01
!                     1.3D+01
!                     1.7D+01
!                     1.9D+01
!                     2.3D+01
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
!     H.A. Neilan        (JPL)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.2.0, 05-JUN-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.1.0, 23-MAY-1990 (HAN)
!
!        The variable FOUND was changed to NFOUND.
!
!-    SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     extract d.p. values from a string
!
!-&


! $ Revisions
!
!-     SPICELIB Version 1.1.0, 23-MAY-1990 (HAN)
!
!         The variable FOUND was changed to NFOUND. Other SPICELIB
!         routines that use the variable FOUND declare it as a logical.
!         In order to conform to this convention, FOUND was changed to
!         NFOUND to indicate that it has an integer value, not a logical
!         value.
!
!-&


!
!     SPICELIB functions
!
      integer          lastnb
      integer          wdindx
      integer          nblen

!
!     Local variables
!
      integer          length
      integer          positn

      integer          berase
      integer          eerase
      integer          fallbk

      integer          start
      integer          i
      integer          j


      double precision x
      character*80     error
      integer          pntr

!
!     No keywords or numbers have been located yet.
!
      nfound  =  0
      parsed  =  0

!
!     Locate the keyword within the string and get the length of the
!     string.
!
      positn  =  wdindx ( string, keywd )
      length  =  lastnb ( string      )

      if ( positn .eq. 0 ) then
         nfound = -1
         parsed =  0
         return
      end if

!
!     Set the begin erase marker to the start of the current word
!     Set the end   erase marker to the end   of the current word
!
      berase  = positn
      eerase  = positn + nblen(keywd) - 1

      start   = eerase + 1

      if ( start .lt. length ) then

!
!        Locate the next word and try to parse it ...
!
         call fndnwd ( string,      start, i,     j    )
         call nparsd ( string(i:j), x,     error, pntr )

         if ( error .eq. ' ' ) then
!
!           ...  mark its starting position as a possible starting
!           point for deletion if we run out of room for parsed numbers.
!
            fallbk         = i
            eerase         = j
            start          = j      + 1
            nfound         = nfound + 1
            parsed         = parsed + 1
            values(parsed) = x
         end if

      else

         string(berase: )  = ' '
         return

      end if


!
!     Now find all of the succeeding numeric words until we run out of
!     numeric words or string to look at.
!
      do while (       ( start .lt. length ) &
                 .and. ( error .eq. ' '    ) )

!
!        Find the next word and try to parse it as a number.
!
         call fndnwd ( string,      start, i,     j       )
         call nparsd ( string(i:j), x,     error, pntr )

         if ( error .eq. ' ' ) then

!
!           It's a number! Congratulations!
!
            nfound = nfound + 1

!
!           If there is room ...
!
            if ( nfound .le. maxwds ) then

!
!              1.  Increment the counter PARSED.
!              2.  Load the DP value into the output array.
!              3.  Set the pointer for the end of the erase
!                   region to be the end of this word.
!
               parsed         = parsed + 1
               values(parsed) = x
               eerase         = j

            else

!
!              Set the pointer of the begin erase region to be the
!              the pointer set up just for this occasion.
!
               berase = fallbk

            end if

!
!           Set the place to begin looking for the next word to be
!           at the first character following the end of the current
!           word.
!
            start = j + 1

         end if

      end do

!
!     Remove the parsed words from the string.
!
      i = berase
      j = eerase + 1

      do while ( j .le. length )
         string(i:i) = string(j:j)
         i           = i + 1
         j           = j + 1
      end do

      string(i:) = ' '

      return

      end
