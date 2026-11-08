!$Procedure ANA ( AN or A ? )

      character(*) function ana ( word, case )
      use pepper_kinds, only: ip

! $ Abstract
!
!     Return the correct article "a" or "an" used to modify a word
!     and return it capitalized, lower case, or upper case.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!     WORD
!
! $ Declarations

      implicit none

      character(*)          word
      character(*)          case

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WORD       I   is a word that should be modified by "a" or "an".
!     CASE       I   'U', 'L', or 'C' to specify capitalization of ANA.
!
!     The function returns the correct article, 'A' or 'AN', needed to
!     modify a word WORD, appropriately capitalized.
!
! $ Detailed_Input
!
!     WORD     is any English word for which you want to write the
!              correct phrase "a(an) response(answer)".  The case of the
!              letters of word do not matter.
!
!              Leading white space in word is ignored. The characters
!              " and ' are ignored.  Thus ''' apple '' ' and
!              '"apple"' and ' apple' and 'apple' are all treated as
!              the same word.
!
!     CASE     is a character that describes how the value returned in
!              ANA should be capitalized. The rules are:
!
!                 'U'  ---  ANA is returned in all caps ( A, AN )
!                 'C'  ---  ANA is returned capitalized ( A, An )
!                 'L'  ---  ANA is returned lower case  ( a, an )
!
!              The case of CASE does not matter. Any value other than
!              those specified result in ANA being returned in all lower
!              case.
!
! $ Detailed_Output
!
!     The function returns the correct indefinite article needed to
!     modify the word contained in WORD.
!
!     ANA should be declared to be
!
!        CHARACTER*(2)
!
!     (or CHARACTER*(N) where N > 1) in the calling program.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)  If the uppercase value of CASE is not 'U', 'C' or 'L', it
!         shall be treated as 'L'.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine allows you to construct grammatically correct phrases
!     when you need to modify a word by an indefinite article. Using
!     the pronunciations contained in the Webster's Ninth Collegiate
!     Dictionary, the phrase
!
!        ANA(WORD, CASE) // ' ' // WORD
!
!     will be grammatically correct.
!
! $ Examples
!
!     Suppose you wished to construct one of the messages
!
!        'a new file'
!        'an existing file'
!
!     and that the NEW/EXISTING word was in the variable WORD. Then
!     you could write
!
!        MESSAGE = ANA( WORD, 'L' ) // ' ' // WORD // ' file '
!        CALL CMPRSS ( ' ', 1, MESSAGE, MESSAGE )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     [1]  Merriam-Webster (Ed.), "Webster's Ninth New Collegiate
!          Dictionary," 10th edition, 1990.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     B.V. Semenov       (JPL)
!     W.L. Taber         (JPL)
!     E.D. Wright        (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.3, 24-NOV-2021 (JDR)
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.1.2, 28-FEB-2008 (BVS)
!
!        Corrected the contents of the $Required_Reading section.
!
!-    SPICELIB Version 1.1.1, 22-SEP-2004 (EDW)
!
!        Added Copyright section.
!
!-    SPICELIB Version 1.1.0, 18-JAN-2001 (WLT)
!
!        Made SCLK an "an" word.
!
!-    SPICELIB Version 1.0.0, 29-NOV-1995 (WLT)
!
!-&


! $ Index_Entries
!
!     get the correct indefinite article
!
!-&


      integer(ip)           isrchc

      integer(ip), parameter :: anwrds = 22

      integer(ip), parameter :: awrds = 33

      integer(ip), parameter :: wdsize = 32

      character(8)          anword ( anwrds )
      character(8)          aword  ( awrds  )

      character(wdsize)     myword
      character(1)          mycase
      character(1)          begin


      integer(ip)           i
      integer(ip)           caps

      character(wdsize)     start ( 7 )

      character(2)          a  ( 3 )
      character(2)          an ( 3 )

      save

      data                  a  / 'A',  'A',  'a'  /
      data                  an / 'AN', 'An', 'an' /


      data  anword / 'HEIR', 'HONEST', 'HONOR', 'H',  'HOUR', &
                     'HORS ','HOMBRE', 'F',     'L',  'M', &
                     'N',    'R',      'S',     'X', &
                     'UNIN', 'UNIM',   'ONEI',  'ONER', &
                     'SPK',  'EK',     'IK',     'SCLK' /

      data  aword  / 'HORSE',   'ONE',   'ONE-',   'ONCE', &
                     'ONENESS', 'UIG',   'UIN',    'UKA', &
                     'UKE',     'UKO',   'UKI',    'UKU', &
                     'ULOT',    'UNANI', 'UNI',    'UNINU', &
                     'UPA',     'URA',   'URE',    'URO', &
                     'USA',     'USE',   'USU',    'UTE', &
                     'UTI',     'UTO',   'UVA',    'UVE', &
                     'UVU',     'EU',    'EWE',    'UTRI', 'U' /

      call ucase ( word,              myword )
      call replch( myword, '''', ' ', myword )
      call replch( myword, '"',  ' ', myword )
      call ljust ( myword,            myword )
      call ucase ( case,              mycase )


      ana = ' '

      if ( mycase == 'U' ) then
         caps = 1
      else if ( mycase == 'C' ) then
         caps = 2
      else
         caps = 3
      end if

!
!     Handle the obvious things first.
!
      begin = myword(1:1)

      if      ( index( 'AI', begin ) > 0 ) then

         ana = an(caps)
         return

      else if ( index( 'BCDGJKPQTVWYZ', begin ) > 0 ) then

         ana = a(caps)
         return

      end if

!
!     If we are still here, we need to be a bit more careful
!     in our determination of ANA.
!
!     Get the beginnings of the input word.
!
      do i = 1, 7
         start(i) = myword(1:i)
      end do

!
!     Now see if the start of the input word belongs to
!     one of the special collections.
!
      do i = 7, 2, -1

         if ( isrchc ( start(i), awrds,  aword ) /= 0 ) then

            ana = a (caps)
            return

         end if

         if ( isrchc ( start(i), anwrds, anword ) /= 0 ) then

            ana = an(caps)
            return

         end if

      end do

!
!     If we got this far we can determine the ANA by
!     just looking at the beginning of the string.
!
      if ( index( 'AEIOU', myword(1:1) ) > 0 ) then

         ana = an(caps)

      else

         ana = a (caps)

      end if

      return

      end
