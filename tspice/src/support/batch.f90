!$Procedure      BATCH (Tell whether or not a program is in batch mode)

      LOGICAL FUNCTION      BATCH ()
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     This function returns information regarding the interactive
!     status of a program.  If BATCH is TRUE the function is considered
!     to be in background mode.  If BATCH is FALSE the function is
!     considered to be in interactive mode.
!
!     To set a program in batch mode call the entry point SETBAT.
!     To set a program in interactive mode call SETMOD
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 20-NOV-1995 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!     Entry points.
!
!
!     Local Variable
!
      LOGICAL               INBTCH

      SAVE                  INBTCH

      LOGICAL               SETBAT
      LOGICAL               SETMOD

      DATA                  INBTCH / .FALSE. /

      BATCH = INBTCH
      RETURN


!$Procedure SETMOD (Set the reader to interative mode.)

      ENTRY SETMOD ()

! $ Abstact

!     Set NXTCOM to interactive mode.  In puts that are expected to
!     come from the keyboard generate an result in a prompt for input

! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
      INBTCH = .FALSE.
      SETMOD = .TRUE.
      RETURN


!$Procedure SETBAT (Set the reader to interative mode.)

      ENTRY SETBAT ()

! $ Abstact

!     Set NXTCOM to interactive mode.  In puts that are expected to
!     come from the keyboard generate an result in a prompt for input

! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
      INBTCH = .TRUE.
      SETBAT = .TRUE.
      RETURN

      END
