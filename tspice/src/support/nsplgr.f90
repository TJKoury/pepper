      SUBROUTINE NSPLG  ( COMMND, HIDDEN, VSTYLE, HSTYLE, CDELIM )
!
! $ Version
!
!-     Command Loop Configured Version 1.1.0, 21-JUN-1999 (WLT)
!
!         Placed RETURN before first entry point.
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!

!
!     Save the contents of the command to a log file and any save
!     file that might be open and active.
!
      IMPLICIT NONE

      CHARACTER*(*)         COMMND
      LOGICAL               HIDDEN
      CHARACTER*(*)         VSTYLE
      CHARACTER*(*)         HSTYLE
      CHARACTER*(*)         CDELIM

      EXTERNAL              NSPWLN


      INCLUDE              'commdpar.inc'

      INTEGER               LNGSIZ
      PARAMETER           ( LNGSIZ = COMSIZ + 1 )

      CHARACTER*(LNGSIZ)    MYSTR
      CHARACTER*(STYSIZ)    SEEN
      CHARACTER*(STYSIZ)    HIDE
      CHARACTER*(1)         DELIM

      SAVE

      DATA   MYSTR / ' ' /
      DATA   SEEN  / 'LEFT 1 RIGHT 78'                       /
      DATA   HIDE  / 'LEADER ;^ LEFT 1 RIGHT 78 HARDSPACE ^' /
      DATA   DELIM / ';' /
      RETURN
!
!     This entry point handles the logging of commands.
!
      ENTRY NSPLOG ( COMMND, HIDDEN )
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!


      MYSTR = COMMND
!
!     Inhibit writing to the screen.
!
      CALL NSPIOH ( 'SCREEN' )
      CALL NSPIOA ( 'LOG'    )

      IF ( HIDDEN ) THEN
         CALL NICEPR_1   ( COMMND,  HIDE, NSPWLN )
      ELSE
         MYSTR = COMMND
         CALL    SUFFIX   ( DELIM,  0,    MYSTR  )
         CALL NICEPR_1    ( MYSTR,  SEEN, NSPWLN )
      END IF
!
!     Re-activate the screen for writing output.
!
      CALL NSPIOA ( 'SCREEN' )
      CALL NSPIOH ( 'LOG'    )

      RETURN

!
!     This entry point allows users to set the style used for
!     logging hidden and visible commands.
!
      ENTRY NSPLGS ( VSTYLE, HSTYLE, CDELIM )
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!


      SEEN  = VSTYLE
      HIDE  = HSTYLE
      DELIM = CDELIM

      RETURN

!
!     This entry point allows users to get the style used for
!     logging hidden and visible commands.
!
      ENTRY NSPGLS ( VSTYLE, HSTYLE, CDELIM )

      VSTYLE = SEEN
      HSTYLE = HIDE
      CDELIM = DELIM
      RETURN


      END
