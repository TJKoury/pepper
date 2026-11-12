!$Procedure      M2BODY ( Determine whether or not a word is a body )

      LOGICAL FUNCTION M2BODY ( WORD )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string is a known body in the
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
!     The function is returned as .TRUE. if word is a META/2 body.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2INT     returns as .TRUE. if WORD is a META/2 body.
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
!     determines whether or not a word is a body in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2BODY
!     -------                               ------
!     JUPITER                               .TRUE.
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
!     Library functions
!
      LOGICAL               M2INT

!
!     Local Parameters
!

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
!
!     Local Variables
!

      CHARACTER*(WDSIZE)    COPY
      INTEGER               IDCODE
      LOGICAL               FOUND


      IF ( M2INT(WORD) ) THEN
         M2BODY = .TRUE.
         RETURN
      END IF

      CALL UCASE ( WORD, COPY )


      CALL M2BODN2C ( COPY, IDCODE, FOUND )

      M2BODY = FOUND

      RETURN
      END
