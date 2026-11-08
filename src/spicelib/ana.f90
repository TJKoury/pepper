!$Procedure ANA ( AN or A ? )

      CHARACTER*(*) FUNCTION ANA ( WORD, CASE )

! $ Abstract
!
!     Return the correct article "a" or "an" used to modify a word
!     and return it capitalized, lower case, or upper case.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE

      CHARACTER*(*)         WORD
      CHARACTER*(*)         CASE

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


      INTEGER               ISRCHC

      INTEGER               ANWRDS
      PARAMETER           ( ANWRDS = 22 )

      INTEGER               AWRDS
      PARAMETER           ( AWRDS  = 33 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(8)         ANWORD ( ANWRDS )
      CHARACTER*(8)         AWORD  ( AWRDS  )

      CHARACTER*(WDSIZE)    MYWORD
      CHARACTER*(1)         MYCASE
      CHARACTER*(1)         BEGIN


      INTEGER               I
      INTEGER               CAPS

      CHARACTER*(WDSIZE)    START ( 7 )

      CHARACTER*(2)         A  ( 3 )
      CHARACTER*(2)         AN ( 3 )

      SAVE

      DATA                  A  / 'A',  'A',  'a'  /
      DATA                  AN / 'AN', 'An', 'an' /


      DATA  ANWORD / 'HEIR', 'HONEST', 'HONOR', 'H',  'HOUR', &
                     'HORS ','HOMBRE', 'F',     'L',  'M', &
                     'N',    'R',      'S',     'X', &
                     'UNIN', 'UNIM',   'ONEI',  'ONER', &
                     'SPK',  'EK',     'IK',     'SCLK' /

      DATA  AWORD  / 'HORSE',   'ONE',   'ONE-',   'ONCE', &
                     'ONENESS', 'UIG',   'UIN',    'UKA', &
                     'UKE',     'UKO',   'UKI',    'UKU', &
                     'ULOT',    'UNANI', 'UNI',    'UNINU', &
                     'UPA',     'URA',   'URE',    'URO', &
                     'USA',     'USE',   'USU',    'UTE', &
                     'UTI',     'UTO',   'UVA',    'UVE', &
                     'UVU',     'EU',    'EWE',    'UTRI', 'U' /

      CALL UCASE ( WORD,              MYWORD )
      CALL REPLCH( MYWORD, '''', ' ', MYWORD )
      CALL REPLCH( MYWORD, '"',  ' ', MYWORD )
      CALL LJUST ( MYWORD,            MYWORD )
      CALL UCASE ( CASE,              MYCASE )


      ANA = ' '

      IF ( MYCASE .EQ. 'U' ) THEN
         CAPS = 1
      ELSE IF ( MYCASE .EQ. 'C' ) THEN
         CAPS = 2
      ELSE
         CAPS = 3
      END IF

!
!     Handle the obvious things first.
!
      BEGIN = MYWORD(1:1)

      IF      ( INDEX( 'AI', BEGIN ) .GT. 0 ) THEN

         ANA = AN(CAPS)
         RETURN

      ELSE IF ( INDEX( 'BCDGJKPQTVWYZ', BEGIN ) .GT. 0 ) THEN

         ANA = A(CAPS)
         RETURN

      END IF

!
!     If we are still here, we need to be a bit more careful
!     in our determination of ANA.
!
!     Get the beginnings of the input word.
!
      DO I = 1, 7
         START(I) = MYWORD(1:I)
      END DO

!
!     Now see if the start of the input word belongs to
!     one of the special collections.
!
      DO I = 7, 2, -1

         IF ( ISRCHC ( START(I), AWRDS,  AWORD ) .NE. 0 ) THEN

            ANA = A (CAPS)
            RETURN

         END IF

         IF ( ISRCHC ( START(I), ANWRDS, ANWORD ) .NE. 0 ) THEN

            ANA = AN(CAPS)
            RETURN

         END IF

      END DO

!
!     If we got this far we can determine the ANA by
!     just looking at the beginning of the string.
!
      IF ( INDEX( 'AEIOU', MYWORD(1:1) ) .GT. 0 ) THEN

         ANA = AN(CAPS)

      ELSE

         ANA = A (CAPS)

      END IF

      RETURN

      END
