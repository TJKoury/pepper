!$Procedure      M2TRAN ( See if a word has a restriction template )

      SUBROUTINE M2TRAN ( STRING, BEG, END, BASE, KEY, TEMP )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Determine a META-WORD class and whether or not a word ends
!     with a substring of the (%*).  If it ends with such a substring
!     return pointers to the left and right parentheses.
!
! $ Required_Reading
!
!     META/2 a language specification language.
!
! $ Keywords
!
!     PARSING
!     UTILITY
!     WORD
!
! $ Declarations

      CHARACTER*(*)         STRING
      INTEGER               BEG
      INTEGER               END
      CHARACTER*(*)         BASE
      LOGICAL               KEY
      LOGICAL               TEMP

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   A META/2 language statement specification.
!     BEG       I/O  The beginning of a word in STRING
!     END       I/O  The end of a word in STRING
!     BASE       O   Portion of the word preceeding the template.
!     KEY        O   .TRUE. if the the substring is a keyword.
!     TEMP       O   .TRUE. if a restriction template is present.
!
! $ Detailed_Input
!
!     STRING(BEG:END)  is a META/2 word that potentially ends with a
!                      substring of the form (%*) where '%' and '*'
!                      stand for the wildstring and wildcharacter
!                      symbols.
!
! $ Detailed_Output
!
!     BEG        is the index of the first character of the restriction
!                template ( the left parenthesis )
!                first parenthesis '(' if a restriction template
!                is present.  If no restriction template is present
!                it is returned as END + 1.
!
!     END        is the index of the last character in the string.
!
!     BASE       is the portion of the string that precedes the
!                restriction template. If no template is present
!                BASE is assigned the value of word (with truncation
!                if BASE has shorter than END - BEG + 1 .
!
!     KEY        is returned as true if STRING(BEG:END) is a keyword
!                in the language that is being specified.  Otherwise
!                it is false.
!
!     TEMP       is returned as true if STRING(BEG:END) is a META-KEY
!                and ends with a restriction template. Otherwise it is
!                false.
!
! $ Error_Handling
!
!     None.  A restriction template is present or it isn't.
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Particulars
!
!      The list of META/2 keywords is given below.  A word of a
!      statement template is viewed as a language keyword if it is
!      not on this list.
!
!        '@alpha',   '@alpha(%*)',  '@body',        '@day',
!        '@end'      '@english',    '@english(%*)', '@epoch',
!        '@int',     '@int(*:*)'    '@month',       '@month(%*)',
!        '@name',    '@name(%*)',   '@number'       '@number(*:*)',
!        '@then'     '@then(%*)',   '@time',        '@unit',
!        '@year',    '}'
!
!      If the word is not a keyword, then it is examined and any
!      restriction templates are returned.
!
!      The restriction template is part of the META/2 language and is
!      described in the required reading section.  Briefly it is
!      a string at the end of a word that has the form
!
!      (x)
!
!      where x is any string of length at least 1.  The interpretation
!      of this string is handled in META2.
!
!      This is purely a utility for META2 and is not a general purpose
!      routine.
!
! $ Examples
!
!      None.
!
! $ Restrictions
!
!      None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-     META/2 Version 4.0.0, 23-MAR-2000 (WLT)
!
!         Extended the routine to add the keyword @unit to the
!         list of Meta/2 keywords.
!
!-     META/2 Configured Version 3.0.0, 14-AUG-1995 (WLT)
!
!         The keyword @body was out of order in the quick
!         check list below.  Who knows what other terrible
!         bugs this was causing.
!
!-     META/2 Configured Version 2.0.0, 9-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 9, 1994
!
!
!-     META/2 Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of META/2
!         software as of May 3, 1994
!
!
!     Version B1.0.0, 23-MAR-1988 (WLT) (IMU)
!
!-&



!
!     SPICELIB functions
!


!
!     Local variables
!
      INTEGER               I
      INTEGER               J
      INTEGER               K

      CHARACTER*4           CWORD

      INTEGER               NQUICK
      PARAMETER           ( NQUICK = 19 )

      CHARACTER*4           QUICK  (NQUICK)

      INTEGER               CHECKS (NQUICK)

      INTEGER               PNTRS  (NQUICK)

      INTEGER               NFULL
      PARAMETER           ( NFULL = 26 )

      CHARACTER*16          FULL   (NFULL)

      LOGICAL               MATCH
      INTEGER               TEMPS  (NQUICK)


      SAVE
!
!     The array QUICK contains abbreviations of all of the know META-KEY
!     words in alphabetical order.
!
      DATA                  QUICK  / '@alp',  '@bod',  '@cal',  '@day', &
                                     '@end', &
                                     '@eng',  '@epo',  '@int',  '@mon', &
                                     '@nam',  '@num',  '@the',  '@tim', &
                                     '@uni', &
                                     '@wor',  '@yea',  '{',     '|', &
                                     '}'    /



!
!     The array TEMPS gives the character position within a word where
!     a template will be attached to a META-KEY word.
!     If the first portion of a word equals QUICK(I), TEMP(I) will be
!     the character immediately before the template (if one is present).
!
!     If a template is not allowed for a META-KEY word, TEMP will be 0.
!
      DATA                  TEMPS  /  6,       5,       0,       0, &
                                      0, &
                                      8,       0,       4,       6, &
                                      5,       7,       5,       0, &
                                      5, &
                                      5,       0,       0,       0, &
                                      0     /

!
!     The array CHECKS tells how many different ways a META-KEY word
!     can be represented.  For example @alpha or @alpha(template).
!     If a word matches up in the beginning with QUICK(I) then there
!     are at most CHECKS(I) checks that we must perform to see if it
!     is in fact a legitimate META-KEY word.
!
      DATA                  CHECKS /  2,       2,       1,       1, &
                                      1, &
                                      2,       1,       2,       2, &
                                      2,       2,       2,       1, &
                                      2, &
                                      2,       1,       0,       0, &
                                      0     /


!
!     PNTRS(I) points to the first position in the array FULL where
!     one should look to find the actual patterns that should be
!     checked to see if a word that matches the initial portion
!     in QUICK(I) is in fact a META-KEY
!
      DATA                  PNTRS  /  1,      3,        5,      6, &
                                      7, &
                                      8,     10,       11,      13, &
                                     15,     17,       19,      21, &
                                     22, &
                                     24,     26,       26,      26, &
                                     26      /

      DATA                  FULL   /  '@alpha',        '@alpha(%*)', &
                                      '@body', &
                                      '@body(%*)',     '@calendar', &
                                      '@day',          '@end', &
                                      '@english',      '@english(%*)', &
                                      '@epoch',        '@int', &
                                      '@int(*:*)',     '@month', &
                                      '@month(%*)',    '@name', &
                                      '@name(%*)',     '@number', &
                                      '@number(*:*)',  '@then', &
                                      '@then(%*)',     '@time', &
                                      '@unit',         '@unit(%*)', &
                                      '@word',         '@word(%*)', &
                                      '@year'                       /



!
!     First do a binary search on the abreviations of the META-KEYS
!     to see if this is a key word.
!
      CWORD = STRING(BEG:END)
      I     = BSRCHC ( CWORD, NQUICK, QUICK )

      IF ( I .EQ. 0 ) THEN

!
!        We didn't even match up with one of the abbreviations,  this
!        can't be a META-KEY and so must be a language specification
!        keyword.
!
         KEY    = .TRUE.
         TEMP   = .FALSE.
         BASE   = STRING(BEG:END)
         BEG    = END + 1
         RETURN

      ELSE

!
!        We at least match an abbreviation.  See if we match the
!        full expansion of the abbreviation.
!
         KEY    = .FALSE.
         K      = PNTRS(I)
         J      = 1
         MATCH  = .FALSE.

         DO WHILE ( (J .LE. CHECKS(I)) .AND. &
                    .NOT. MATCH              )


            MATCH  = MATCHW ( STRING(BEG:END), FULL(K), '*', '%' )
            KEY    = .NOT. MATCH

            K      = K + 1
            J      = J + 1

         END DO

         IF ( KEY ) THEN


            TEMP = .FALSE.
            BASE = STRING(BEG:END)
            BEG  = END + 1
            RETURN

         END IF

!
!        If we get this far we must have a META-KEY.  See if there
!        is a restriction template.
!

         IF ( STRING(BEG:END) .EQ. FULL(PNTRS(I)) ) THEN

!
!           There is no restriction template.
!
            BASE  = STRING(BEG:END)
            BEG   = END + 1
            TEMP  = .FALSE.

         ELSE

!
!           We have a restriction template.
!

            BASE  = FULL ( PNTRS(I) )
            BEG   = BEG + TEMPS(I)
            TEMP  = .TRUE.

         END IF
      END IF


      RETURN
      END
