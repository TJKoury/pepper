!$Procedure      M2MON ( Determine whether or not a word is a month )

      LOGICAL FUNCTION M2MON ( WORD )
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string is a month in the
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
!     The function is returned as .TRUE. if word is an META/2 month.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2MON    returns as .TRUE. if WORD is less than 32 characters
!               in length, starts with an alphabetic character and
!               contains only letters, digits, underscores and hyphens.
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
!     determines whether or not a word is a month in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2MON
!     -------                               ------
!     SPAM                                  .FALSE.
!     JAN                                   .TRUE.
!     FEBR                                  .TRUE.
!     OCTA                                  .FALSE.
!     AUGU                                  .TRUE.
!     JU                                    .FALSE.
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
      INTEGER               BSRCHC
      INTEGER               QRTRIM
      INTEGER               LTRIM

!
!     Local variables
!
      INTEGER               I
      INTEGER               LENGTH
      INTEGER               START
      INTEGER               END

      CHARACTER*3           SHORT  ( 12 )
      CHARACTER*9           MONTHS ( 12 )
      CHARACTER*9           COPY



      INTEGER               MONTH

      SAVE

      DATA                  SHORT  / 'APR', 'AUG', 'DEC', 'FEB', &
                                     'JAN', 'JUL', 'JUN', 'MAR', &
                                     'MAY', 'NOV', 'OCT', 'SEP'  /

      DATA                  MONTHS / 'APRIL',    'AUGUST',  'DECEMBER', &
                                     'FEBRUARY', 'JANUARY', 'JULY', &
                                     'JUNE',     'MARCH',   'MAY', &
                                     'NOVEMBER', 'OCTOBER', 'SEPTEMBER'/



!
!     Make sure the string has the right length.
!
      START  = LTRIM   ( WORD )
      END    = QRTRIM  ( WORD )
      LENGTH = END - START + 1

      IF ( LENGTH .LT. 3 ) THEN
         M2MON = .FALSE.
         RETURN
      END IF

      IF ( LENGTH .GT. 9 ) THEN
         M2MON = .FALSE.
         RETURN
      END IF

      CALL UCASE ( WORD, COPY )

!
!     See if the first three letters match anything we've got so far.
!
      MONTH = BSRCHC ( COPY(START:START+2), 12, SHORT )

      IF ( MONTH .EQ. 0 ) THEN
         M2MON = .FALSE.
         RETURN
      END IF

!
!     Now make sure that any remaining letters match up exactly.
!
      I     = START + 3
      M2MON = .TRUE.

      DO WHILE ( ( I .LE. END ) .AND. M2MON )

         M2MON = COPY(I:I) .EQ. MONTHS(MONTH)(I:I)
         I     = I + 1

      END DO

      RETURN
      END
