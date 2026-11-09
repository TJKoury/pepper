!$Procedure      M2UNIT ( Determine whether a word is a unit spec )

      LOGICAL FUNCTION M2UNIT ( WORD )
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
!     The function is returned as .TRUE. if word is an META/2 unit.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2UNIT    returns as .TRUE. if WORD is a parsable unit.
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
!     determines whether or not a word is a unit spec in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2UNIT
!     -------                               ------
!     SPAM                                  .FALSE.
!     KM                                    .TRUE.
!     KM/SEC                                .TRUE.
!     100*AU/YEAR                           .TRUE.
!     12.E2                                 .FALSE.
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
!-     META/2 Version 1.0.0, 23-MAR-2000 (WLT)
!
!
!-&



!
!     SPICELIB functions
!
      LOGICAL               UNITP
      INTEGER               LTRIM
      INTEGER               RTRIM
!
!     Local variables
!
      INTEGER               START
      INTEGER               END

      SAVE


!
!     Make sure the string has the right length.
!
      START  = LTRIM  ( WORD )
      END    = RTRIM  ( WORD )

      M2UNIT = UNITP( WORD(START:END) )


      RETURN
      END
