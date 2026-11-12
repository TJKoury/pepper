!$Procedure      M2EPOC ( Determine whether or not a word is an epoch )

      LOGICAL FUNCTION M2EPOC ( WORD )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     This function is true if the input string is an epoch in the
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
!     The function is returned as .TRUE. if word is a META/2 epoch.
!
! $ Detailed_Input
!
!     WORD      is a character string that is assumed to have no
!               spaces between the first and last non-blank characters.
!
! $ Detailed_Output
!
!     M2EPOC    returns as .TRUE. if WORD passes throught TPARSE without
!               error. Otherwise M2EPOC is returned .FALSE.
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
!     determines whether or not a word is an epoch in the sense
!     of the language META/2.
!
! $ Examples
!
!     WORD                                  M2EPOC
!     -------                               ------
!     SPAM                                  .FALSE.
!     _SPUD                                 .FALSE.
!     1:23:1927                             .TRUE.
!     jan/1/1988                            .TRUE.
!     4-1-1988/24:13:48.28                  .TRUE.
!     1988-MAR-8/23:59:60.281               .TRUE.
!     19:3:1                                .FALSE.
!     88-JAN-89                             .FALSE.
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
!     SPICE functions
!
      LOGICAL               M2TIME
      LOGICAL               M2YEAR
      LOGICAL               M2MON

!
!     Local variables
!
      INTEGER               TCODE
      CHARACTER*80          ERROR



      SAVE




      IF ( M2TIME(WORD) ) THEN

         M2EPOC = .FALSE.

      ELSE IF ( M2YEAR(WORD) ) THEN

         M2EPOC = .TRUE.

      ELSE IF ( M2MON(WORD) ) THEN

         M2EPOC = .FALSE.

      ELSE

         CALL M2CAL ( WORD, ERROR, TCODE )
         M2EPOC =   ( ERROR .EQ.  ' ' )

      END IF

      RETURN
      END
