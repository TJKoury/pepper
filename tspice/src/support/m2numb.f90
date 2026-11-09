!$Procedure      M2NUMB ( Determine whether or not a word is a number )

      LOGICAL FUNCTION M2NUMB ( WORD )
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string is a number in the
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
!     The function is returned as .TRUE. if word is an META/2 number.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2NUMB    returns as .TRUE. if WORD is a parsable number.
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
!     determines whether or not a word is a number in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2NUMB
!     -------                               ------
!     SPAM                                  .FALSE.
!     1                                     .TRUE.
!     0.289E19                              .TRUE.
!     0.2728D12                             .TRUE.
!     -12.1892e-5                           .TRUE.
!     12.E29                                .TRUE.
!     12.E291                               .FALSE.
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
      INTEGER               QRTRIM
      INTEGER               LTRIM

!
!     Local variables
!
      INTEGER               LENGTH
      INTEGER               START
      INTEGER               END

      DOUBLE PRECISION      X

      CHARACTER*80          ERROR
      INTEGER               POINTR

      SAVE


!
!     Make sure the string has the right length.
!
      START  = LTRIM  ( WORD )
      END    = QRTRIM  ( WORD )
      LENGTH = END - START + 1


!
!     Rule out the goofy cases that NPARSD will allow.
!
      IF      ( LENGTH .EQ. 1 ) THEN

         M2NUMB =       LLE ( '0', WORD ) &
                  .AND. LGE ( '9', WORD )
         RETURN

      END IF

      IF ( LENGTH .GE. 2 ) THEN
         IF (      ( WORD(START:START) .EQ. 'E' ) &
              .OR. ( WORD(START:START) .EQ. 'e' ) &
              .OR. ( WORD(START:START) .EQ. 'D' ) &
              .OR. ( WORD(START:START) .EQ. 'd' ) ) THEN

            M2NUMB = .FALSE.
            RETURN

         END IF

         IF (      ( WORD(START:START+1) .EQ. '+E' ) &
              .OR. ( WORD(START:START+1) .EQ. '-E' ) &
              .OR. ( WORD(START:START+1) .EQ. '+D' ) &
              .OR. ( WORD(START:START+1) .EQ. '-D' ) &
              .OR. ( WORD(START:START+1) .EQ. '-e' ) &
              .OR. ( WORD(START:START+1) .EQ. '+e' ) &
              .OR. ( WORD(START:START+1) .EQ. '-d' ) &
              .OR. ( WORD(START:START+1) .EQ. '+d' ) &
              .OR. ( WORD(START:START+1) .EQ. '.E' ) &
              .OR. ( WORD(START:START+1) .EQ. '.D' ) &
              .OR. ( WORD(START:START+1) .EQ. '.e' ) &
              .OR. ( WORD(START:START+1) .EQ. '.d' ) )THEN

            M2NUMB = .FALSE.
            RETURN
         END IF
      END IF

      IF ( LENGTH .GE. 3 ) THEN

         IF (      ( WORD(START:START+2) .EQ. '+.E' ) &
              .OR. ( WORD(START:START+2) .EQ. '-.E' ) &
              .OR. ( WORD(START:START+2) .EQ. '+.D' ) &
              .OR. ( WORD(START:START+2) .EQ. '-.D' ) ) THEN

            M2NUMB = .FALSE.
            RETURN

         END IF

      END IF

!
!     Ok.  Now just hit the word with NPARSD.
!
      ERROR = ' '

      CALL NPARSD ( WORD, X, ERROR, POINTR )

!
!     Any errors indicate we don't have a number.
!
      IF ( ERROR .NE. ' ' ) THEN

         M2NUMB = .FALSE.

      ELSE

         M2NUMB = .TRUE.

      END IF

      RETURN
      END
