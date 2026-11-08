!$Procedure      M2BEGR ( See if a word begins with a range template )

      SUBROUTINE M2BEGR ( STRING, BEG, END, A, B )
      IMPLICIT NONE

! $ Abstract
!
!     Determine whether or not the substring STRING(BEG:END) begins
!     with a substring of the form (A:B) where A and B are integers.
!     If it does, et BEG is set to the index of the first character
!     following this substring and the integer values of A and B are
!     returned.  Otherwise no action is taken.
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
      INTEGER               A
      INTEGER               B

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     STRING     I   A META/2 language statement specification.
!     BEG       I/0  The beginning of the substring on input and output
!     END       I/0  The end of the substring on input and output
!     A          O   Lower value of the range template
!     B          O   Upper value of the range template
!
! $ Detailed_Input
!
!     STRING(BEG:END)  is a word in the META/2 language.  It potentially
!                      begins with a substring of the form (A:B) where
!                      A and B are both chracter strings representing
!                      integers.
!
!
! $ Detailed_Output
!
!     BEG        On ouput BEG points to the beginning of the portion of
!                the input word that follows the range template (if
!                one was present)  Otherwise it remains unchanged.
!
!     END        points to the end of the input META/2 word.
!
!     A          is the value represented by the first numeric string
!                of the range template.  If a range template is not
!                present, A is not assigned a value.
!
!     B          is the value represented by the second numeric string
!                of the range template (if there is a second numeric
!                string)  If a range template is present, but no numeric
!                string is present B is assigned the value INTMAX().
!
!
! $ Error_Handling
!
!     None.  A range template is present or it isn't.
!
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
!      The range template is part of the META/2 language and is
!      described in the required reading section.  Briefly it is
!      a string at the beginning of a word that has the form
!
!      (A:B)
!
!      where A is a string representing a positive integer, and
!      B the null string or a string representing a positive integer
!      greater than A.
!
!      This routine determines if a range template is present and if so
!      what the values of A and B are.  If B is the null string it
!      is assumed to represent the largest positive integer.
!
! $ Examples
!
!      Consider the following
!
!      inputs                              outputs
!
!      STRING(BEG:END) BEG    END          BEG   END   A      B
!      --------------- ---    --- ---      ---   ---  ---    ---
!      (1:2)@number     5     16            10    16   1      2
!      1:2@number       7     16             7    16   x      x
!      (-1:23)@word     3     14             3    14   x      x
!      @frank           6     11             6    11   x      x
!      (4:)@spam(1:2)  54     67            58    67   4     INTMAX()
!      @spud(1:12)     10     20            10    20   x      x
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
      INTEGER               INTMAX

!
!     Local variables
!
      INTEGER               START
      INTEGER               I
      INTEGER               LPAREN
      INTEGER               RPAREN
      INTEGER               COLON

      LOGICAL               DIGIT ( 0 : 255 )

      CHARACTER*80          ERROR
      INTEGER               POINTR


      LOGICAL               FIRST
      SAVE

      DATA                  FIRST / .TRUE. /




      IF ( FIRST ) THEN
         FIRST = .FALSE.

         DO I = 0, 255
            DIGIT(I) = .FALSE.
         END DO

         DIGIT( ICHAR('0') ) = .TRUE.
         DIGIT( ICHAR('1') ) = .TRUE.
         DIGIT( ICHAR('2') ) = .TRUE.
         DIGIT( ICHAR('3') ) = .TRUE.
         DIGIT( ICHAR('4') ) = .TRUE.
         DIGIT( ICHAR('5') ) = .TRUE.
         DIGIT( ICHAR('6') ) = .TRUE.
         DIGIT( ICHAR('7') ) = .TRUE.
         DIGIT( ICHAR('8') ) = .TRUE.
         DIGIT( ICHAR('9') ) = .TRUE.

         LPAREN = ICHAR( '(' )
         RPAREN = ICHAR( ')' )
         COLON  = ICHAR( ':' )

      END IF



!
!     We need at least (x:) in order to have a range template,  that
!     means at least 4 characters.
!
      IF ( END - BEG .LT. 3 ) THEN
         RETURN
      END IF

      I = BEG
!
!     Range templates must begin with '('
!
      IF ( ICHAR(STRING(I:I)) .NE. LPAREN ) THEN

         RETURN

      END IF

      I     = I + 1

!
!     We must have at least 1 digit
!
      IF ( .NOT. DIGIT( ICHAR(STRING(I:I)) ) ) THEN

         RETURN

      ELSE

         I      = I + 1

      END IF


!
!     Now examin characters until we reach a non-digit
!     or run out of characters in the string.
!
      DO WHILE ( ( I .LE. END ) .AND. DIGIT(ICHAR(STRING(I:I))) )

         I     = I + 1

      END DO


!
!     If the last character encountered was a number or if it was
!     not a colon, we don't have a range template.
!
      IF (      DIGIT(ICHAR(STRING(I:I))) ) THEN

         RETURN

      ELSE IF ( ICHAR(STRING(I:I)) .NE. COLON ) THEN

         RETURN

      END IF

!
!     Ok. we've got an integer. Parse it and put the result
!     into A.
!
      CALL NPARSI ( STRING(BEG+1:I-1), A, ERROR, POINTR )

!
!     Just in case, make sure the number didn't cause an NPARSI error
!     (the only thing can go wrong is the number is too big)
!
      IF ( POINTR .NE. 0 ) THEN
         RETURN
      END IF


!
!     Look at the next letter ( if there is one ) and see if it
!     is a digit.
!
      I     = I + 1
      START = I

      IF ( I .GT. END  ) THEN
         RETURN
      END IF


!
!     Examine letters until we reach a non-digit or run out of
!     characters to examine.
!
      DO WHILE ( ( I .LT. END )  .AND.  DIGIT(ICHAR(STRING(I:I)))  )
         I     = I + 1
      END DO

!
!     If the last character is a digit (we ran out of letters)
!     or was not
!
      IF ( DIGIT(ICHAR(STRING(I:I)))            ) THEN

         RETURN

      ELSE IF (  ICHAR(STRING(I:I)) .NE. RPAREN ) THEN

         RETURN

      END IF

!
!     If the last character read is beyond the first character
!     after the ':', then we've got an integer.
!
      IF ( I .GT. START ) THEN

         CALL NPARSI ( STRING(START:I-1), B, ERROR, POINTR )

!
!        Make sure everythin parsed ok.
!
         IF ( POINTR .NE. 0 ) THEN

            RETURN
         ELSE IF ( B .LT. A   ) THEN

            RETURN

         ELSE

            BEG    = I + 1
            RETURN

         END IF

!
!     If the first character after the colon was the right parenthesis
!     put INTMAX into B
!
      ELSE

         B      = INTMAX()
         BEG    = I + 1
         RETURN

      END IF

      END
