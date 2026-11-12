!$Procedure      M2DAY ( Determine whether or not a word is a day )

      LOGICAL FUNCTION M2DAY ( WORD )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string is a day in the
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
!     The function is returned as .TRUE. if word is a META/2 day.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2DAY     returns as .TRUE. if WORD is a META/2 day.
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
!     determines whether or not a word is a day in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2DAY
!     -------                               ------
!     SPAM                                  .FALSE.
!     1                                     .TRUE.
!     0.289E19                              .FALSE.
!     0.2728D12                             .FALSE.
!     -12.1892e-5                           .FALSE.
!     12                                    .TRUE.
!     12.E291                               .FALSE.
!     1.2E10                                .FALSE.
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
!
!     Local variables
!
      INTEGER               LENGTH
      INTEGER               I
      INTEGER               I1
      INTEGER               I2
      INTEGER               I3
      INTEGER               VALUE
      INTEGER               VALUES ( 0:255 )

      LOGICAL               FIRST
      SAVE

      DATA                  FIRST / .TRUE. /

      IF ( FIRST ) THEN

         FIRST = .FALSE.
!
!        We will construct a value for the string by taking
!        the non-blank portion and computing the value assuming
!        that the first non-blank is a digit with the appropriate
!        power of 10 attached.  Since all non-digit characters
!        will have values of 1000, we will get a value greater
!        than 1000 if any non-digit characters are present.
!
         DO  I =  0,255
            VALUES(I) = 1000
         END DO

         VALUES( ICHAR('0') ) = 0
         VALUES( ICHAR('1') ) = 1
         VALUES( ICHAR('2') ) = 2
         VALUES( ICHAR('3') ) = 3
         VALUES( ICHAR('4') ) = 4
         VALUES( ICHAR('5') ) = 5
         VALUES( ICHAR('6') ) = 6
         VALUES( ICHAR('7') ) = 7
         VALUES( ICHAR('8') ) = 8
         VALUES( ICHAR('9') ) = 9

      END IF


!
!     Make sure the string has the right length.
!
      I1     = LTRIM  ( WORD )
      I3     = QRTRIM ( WORD )
      LENGTH = I3 - I1 + 1


!
!     Rule out the goofy cases that NPARSD will allow.
!
      IF      ( LENGTH .GT. 3 ) THEN

         VALUE = 1000

      ELSE IF ( LENGTH .EQ. 3 ) THEN

         I2    =  I1 + 1
         VALUE =  100  * VALUES( ICHAR( WORD(I1:I1) ) ) &
               +   10  * VALUES( ICHAR( WORD(I2:I2) ) ) &
               +         VALUES( ICHAR( WORD(I3:I3) ) )


      ELSE IF ( LENGTH .EQ. 2 ) THEN

         VALUE =  10   * VALUES( ICHAR( WORD(I1:I1) ) ) &
               +         VALUES( ICHAR( WORD(I3:I3) ) )
      ELSE

         VALUE =         VALUES( ICHAR( WORD(I1:I1) ) )

      END IF
!
!     That's all just make sure that the value is within the
!     bound required of a day of month or year.
!
      M2DAY = (   VALUE .GE. 1 ) .AND. ( VALUE .LE. 366 )

      RETURN
      END
