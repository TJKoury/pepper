!$Procedure      M2ENGL ( Determine if a word contains all letters)

      LOGICAL FUNCTION M2ENGL ( WORD )
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string is an english word in
!     the sense of META/2.
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
!     The function is returned as .TRUE. if word is an META/2 english
!     word
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2ENGL    returns as .TRUE. if WORD is less than 32 characters
!               in length, and contains only letters. Otherwise it is
!               returned .FALSE.
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
!     determines whether or not a word is an english word  name
!     in the sense of the language META/2.
!
! $ Examples
!
!     WORD                                  M2ENGL
!     -------                               ------
!     SPAM                                  .TRUE.
!     _SPUD                                 .FALSE.
!     THE_QUICK_BROWN_FOX                   .FALSE.
!     THE_FIRST_TIME_EVERY_I_SAW_YOUR_FACE  .FALSE.
!     WHO?_ME?                              .FALSE.
!     D!#@!@#!                              .FALSE.
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
      INTEGER               I
      INTEGER               LENGTH
      INTEGER               START
      INTEGER               END

!
!     WRDLEN is the parameter that gives the maximum allowed length
!     of a name.
!
      INTEGER               WRDLEN
      PARAMETER           ( WRDLEN = 32 )



      SAVE


!
!     Make sure the string has the right length.
!
      START  = LTRIM  ( WORD )
      END    = QRTRIM ( WORD )
      LENGTH = END - START + 1

      M2ENGL = (      ( LENGTH .LE. WRDLEN ) &
                .AND. ( LENGTH .GE. 1      ) )

      I      = START

      DO WHILE ( M2ENGL .AND. ( I .LE. END ) )


         M2ENGL = (      ICHAR('A') .LE. ICHAR(WORD(I:I)) &
                   .AND. ICHAR('Z') .GE. ICHAR(WORD(I:I)) ) &
                   .OR. &
                  (      ICHAR('a') .LE. ICHAR(WORD(I:I)) &
                   .AND. ICHAR('z') .GE. ICHAR(WORD(I:I)) )

         I = I + 1

      END DO

      RETURN
      END
