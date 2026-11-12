!$Procedure      M2ALPH ( Determine if a word starts with a letter)

      LOGICAL FUNCTION M2ALPH ( WORD )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string begins with an
!     alphabetic character.
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
!     The function is returned as .TRUE. if word is an META/2 alpha
!     word.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2ALPH    returns as .TRUE. if WORD starts with an alphabetic
!               character.  Otherwise it is returned .FALSE.
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
!     determines whether or not a word is an alpha word in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2ALPH
!     -------                               ------
!     SPAM                                  .TRUE.
!     _SPUD                                 .FALSE.
!     THE_QUICK_BROWN_FOX                   .TRUE.
!     THE_FIRST_TIME_EVERY_I_SAW_YOUR_FACE  .TRUE.
!     WHO?_ME?                              .TRUE.
!     D!#@!@#!                              .TRUE.
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

!
!     Local variables
!
      INTEGER               I


      SAVE



!
!     Make sure the string has the right length.
!
      I      = LTRIM    ( WORD )
      M2ALPH =          (       LLE( 'A', WORD(I:I) ) &
                          .AND. LGE( 'Z', WORD(I:I) ) ) &
                  .OR. &
                        (       LLE( 'a', WORD(I:I) ) &
                          .AND. LGE( 'z', WORD(I:I) )  )





      RETURN
      END
