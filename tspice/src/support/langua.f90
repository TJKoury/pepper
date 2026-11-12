      SUBROUTINE LANGUA ( STRING )
      use pepper_legacy
      IMPLICIT NONE
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!

!
!     This subroutine is used by CMLOOP to store the language that
!     is currently used by the user's program.  You may freely use
!     it throughout the rest of your program if you make your
!     program language sensitive.
!
      CHARACTER*(*)         STRING


      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
      CHARACTER*(WDSIZE)    LANG

      SAVE
      DATA                  LANG / 'ENGLISH' /

      STRING = ' '
      RETURN

!
!     The SETLAN entry point is used for setting the language.
!
      ENTRY SETLAN ( STRING )

         CALL LJUST ( STRING, LANG )
         CALL UCASE ( LANG,   LANG )
         RETURN

!
!     Use the GETLAN entry point to get the language.
!
      ENTRY GETLAN ( STRING )

         STRING = LANG

      RETURN

      END
