!$Procedure      M2INT ( Determine whether or not a word is an integer )

      LOGICAL FUNCTION M2INT ( WORD )
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string is an integer in the
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
!     The function is returned as .TRUE. if word is a META/2 integer.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2INT     returns as .TRUE. if WORD is a META/2 integer.
!               Otherwise it is returned .FALSE.
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
!     determines whether or not a word is an integer in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2INT
!     -------                               ------
!     SPAM                                  .FALSE.
!     1                                     .TRUE.
!     0.289E19                              .FALSE.
!     0.2728D12                             .FALSE.
!     -12.1892e-5                           .FALSE.
!     12.E29                                .FALSE.
!     12.E291                               .FALSE.
!     1.2E10                                .TRUE.
!     .E12                                  .FALSE.
!     1.2E.12                               .FALSE.
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
      INTEGER               INTMAX
      INTEGER               INTMIN
      INTEGER               QRTRIM
      INTEGER               LTRIM
!
!     Local variables
!
      INTEGER               END
      INTEGER               FACTOR
      INTEGER               I
      INTEGER               LENGTH
      INTEGER               MINUS
      INTEGER               PLUS
      INTEGER               START
      INTEGER               SUBSEQ
      INTEGER               VALUE
      INTEGER               ZERO

      LOGICAL               BAD ( 0 : 255 )
      LOGICAL               FIRST
      LOGICAL               USEMIN
      SAVE

      DATA                  FIRST /.TRUE./

      IF ( FIRST ) THEN
         FIRST = .FALSE.

         DO I = 0, 255
            BAD(I) = .TRUE.
         END DO

         MINUS             = ICHAR( '-' )
         PLUS              = ICHAR( '+' )
         ZERO              = ICHAR( '0' )

         BAD( ICHAR('0') ) = .FALSE.
         BAD( ICHAR('1') ) = .FALSE.
         BAD( ICHAR('2') ) = .FALSE.
         BAD( ICHAR('3') ) = .FALSE.
         BAD( ICHAR('4') ) = .FALSE.
         BAD( ICHAR('5') ) = .FALSE.
         BAD( ICHAR('6') ) = .FALSE.
         BAD( ICHAR('7') ) = .FALSE.
         BAD( ICHAR('8') ) = .FALSE.
         BAD( ICHAR('9') ) = .FALSE.


      END IF

      START        = LTRIM (WORD)
      END          = QRTRIM(WORD)
      LENGTH       = END - START + 1
      SUBSEQ       = START + 1

      IF ( LENGTH .EQ. 1 ) THEN

         BAD ( MINUS ) = .TRUE.
         BAD ( PLUS  ) = .TRUE.

         M2INT = .NOT. BAD( ICHAR(WORD(START:START)) )
         RETURN

      ELSE IF ( LENGTH .GT. 10 ) THEN

         M2INT = .FALSE.

      ELSE

         BAD ( MINUS ) = .FALSE.
         BAD ( PLUS  ) = .FALSE.

      END IF

      IF ( BAD( ICHAR(WORD(START:START)) ) ) THEN
         M2INT = .FALSE.
         RETURN
      END IF

      BAD ( MINUS ) = .TRUE.
      BAD ( PLUS  ) = .TRUE.

      DO I = SUBSEQ, END
         IF ( BAD( ICHAR(WORD(I:I)) ) ) THEN
            M2INT = .FALSE.
            RETURN
         END IF
      END DO
!
!     We allow 10 digit numbers only if the first character
!     is a '+' or '-'  So if we have 10 digits the first must
!     now be a "bad" character.
!
      USEMIN = ICHAR(WORD(START:START)) .EQ. MINUS

      IF (   BAD( ICHAR(WORD(START:START)) )   ) THEN

         IF ( LENGTH .LT. 11 ) THEN
            M2INT = .TRUE.
            RETURN
         END IF

         START  = SUBSEQ

      ELSE IF ( LENGTH .EQ. 11 ) THEN

         M2INT = .FALSE.
         RETURN

      ELSE IF ( LENGTH .LT. 10 ) THEN

         M2INT = .TRUE.
         RETURN

      END IF

      IF ( USEMIN ) THEN

         VALUE  = INTMIN()
         FACTOR = 1

         DO I  = END, START+1, -1
            VALUE  = VALUE  + ( ICHAR(WORD(I:I)) - ZERO )*FACTOR
            FACTOR = FACTOR * 10
         END DO

         IF ( ICHAR(WORD(START:START)) .GT. ICHAR('2') ) THEN
            M2INT = .FALSE.
         ELSE
            I     = START
            VALUE = VALUE  + ( ICHAR(WORD(I:I)) - ZERO )*FACTOR
            M2INT = VALUE .LE. 0
         END IF
      ELSE
         VALUE  = INTMAX()
         FACTOR = 1
         DO I  = END, START+1, -1
            VALUE  = VALUE  - ( ICHAR(WORD(I:I)) - ZERO )*FACTOR
            FACTOR = FACTOR * 10
         END DO

         IF ( ICHAR(WORD(START:START)) .GT. ICHAR('2') ) THEN
            M2INT = .FALSE.
         ELSE
            I     = START
            VALUE = VALUE  - ( ICHAR(WORD(I:I)) - ZERO )*FACTOR
            M2INT = VALUE .GE. 0
         END IF
      END IF

      RETURN
      END
