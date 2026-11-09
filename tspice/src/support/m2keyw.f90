!$Procedure      M2KEYW ( Determine whether or not a word is a keyword )

      LOGICAL FUNCTION M2KEYW ( WORD )
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string is a keyword in the
!     sense of META/2.
!
! $ Required_Reading
!
!     META/2 a language specification language.
!
! $ Keywords
!
!     ALPHANUMERIC
!     ASCII
!     PARSING
!     UTILITY
!     WORD
!
! $ Declarations

      CHARACTER*(*)         WORD

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     WORD       I   A character string word
!
!     The function is returned as .TRUE. if word is an META/2 keyword.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2KEYW    returns as .TRUE. if WORD is not one of the following:
!
!             '@alpha',   '@alpha(%*)',  '@calendar',    '@body',
!             '@day',
!             '@end'      '@english',    '@english(%*)', '@epoch',
!             '@int',     '@int(*:*)'    '@month',       '@month(%*)',
!             '@name',    '@name(%*)',   '@number'       '@number(*:*)',
!             '@then'     '@then(%*)',   '@time',        '@year',
!             '{',        '|',           '}'             '@unit'
!
! $ Error_Handling
!
!     None.
!C
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
!     This is a utility routine for the subroutine META2.  It
!     determines whether or not a word is a keyword in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2KEYW
!     -------                               ------
!     @english(A*)                          .FALSE.
!     SPAM                                  .TRUE.
!     |                                     .FALSE.
!     19                                    .TRUE.
!     @bug                                  .TRUE.
!     @number                               .FALSE.
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
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-     META/2 Version 3.0.0, 23-MAR-2000 (WLT)
!
!         Extended the routine to handle the new meta-keyword @unit
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
!     Version B1.0.0, 22-MAR-1988 (WLT) (IMU)
!
!-&



!
!     SPICELIB functions
!

      INTEGER               BSRCHC
      LOGICAL               MATCHW


!
!     Local variables
!
      INTEGER               END
      INTEGER               I
      INTEGER               J
      INTEGER               K
      INTEGER               L
      INTEGER               LBRACE
      INTEGER               RBRACE
      INTEGER               BLANK

      CHARACTER*4           CWORD

      INTEGER               NQUICK
      PARAMETER           ( NQUICK = 20 )

      CHARACTER*4           QUICK  (NQUICK)

      INTEGER               CHECKS (NQUICK)

      INTEGER               PNTRS  (NQUICK)

      INTEGER               NSLOW
      PARAMETER           ( NSLOW = 25 )

      CHARACTER*16          SLOW   (NSLOW)

      LOGICAL               MATCH

      SAVE
!
!     We are going to look at the first four characters of the input
!     word.  If it doesn't match one of the following, then it isn't
!     a meta-2 specification word, it's a keyword.  The data in
!     this array should always be in increasing order.
!


      DATA                  QUICK  / ')',     '@alp',  '@bod', &
                                     '@cal',  '@day', &
                                     '@end',  '@eng',  '@epo',  '@int', &
                                     '@mon',  '@nam',  '@num',  '@the', &
                                     '@tim',  '@uni',  '@wor',  '@yea', &
                                     '{',     '|',     '}'    /
!
!     If after checking against the previous list we have a match,
!     then we need to do further checks to see if we have a
!     legitimate meta-2 specification word.  If we have a bracket or
!     vertical bar, we are done ( zero more checks are required ).
!     In other cases 1 or two more checks may be required.  The
!     data below tells how many further checks may be required.
!



      DATA                  CHECKS /  0,      2,        1, &
                                      1,      1, &
                                      1,      2,        1,       2, &
                                      2,      2,        2,       2, &
                                      1,      2,        2,       1, &
                                      0,      0,        0     /
!
!     The PNTRS array points to the slot in the SLOW check array
!     where our matching pattern templates reside for checking
!     the current input word.
!


      DATA                  PNTRS  /  0,      1,        3, &
                                      4,      5, &
                                      6,      7,        9,      10, &
                                     12,     14,       16,      18, &
                                     20,     21,       23,      25, &
                                     25,     25,       25    /

      DATA                  SLOW   /  '@alpha',        '@alpha(%*)', &
                                      '@body',         '@calendar', &
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
                                      '@year'                     /



      CWORD = WORD
      I     = BSRCHC ( CWORD, NQUICK, QUICK )

      IF ( I .EQ. 0 ) THEN

         M2KEYW = .TRUE.
         RETURN

      END IF

!
!     We only want to examine the portion of the word that preceeds
!     a parsing qualifier.  First locate the last non-blank character
!     of the word.
!
      LBRACE = ICHAR ( '['  )
      RBRACE = ICHAR ( ']'  )
      BLANK  = ICHAR ( ' '  )
      END    = LEN   ( WORD )

      DO WHILE ( END .GT. 1 .AND. ICHAR(WORD(END:END)) .EQ. BLANK )
         END = END - 1
      END DO

!
!     If the length is not at least 4 or the last character is not
!     a right brace, there is no name associated with this word.
!
      IF (      ( ICHAR(WORD(END:END)) .EQ. RBRACE ) &
          .AND. ( END                   .GE. 4      ) ) THEN
!
!        Ok. We have a chance at getting a name.  Look for
!        a left brace and if found set the name and class end.
!
         L = 2

         DO WHILE (L .LT. END - 1)
            IF ( ICHAR(WORD(L:L)) .EQ. LBRACE ) THEN
!
!              We've found the beginning of the name portion
!              of the word.  Record the end of the meta-2
!              word and then reset L so that we exit this loop.
!
               END    = L - 1
               L      = END
            END IF
            L = L + 1
         END DO
      END IF


      M2KEYW = .FALSE.
      K      = PNTRS(I)
      J      = 1
      MATCH  = .FALSE.

      DO WHILE ( (J .LE. CHECKS(I)) .AND. &
                  .NOT. MATCH              )

         MATCH  =  MATCHW ( WORD(1:END), SLOW(K), '*', '%' )
         M2KEYW = .NOT. MATCH

         K      = K + 1
         J      = J + 1

      END DO


      RETURN
      END
