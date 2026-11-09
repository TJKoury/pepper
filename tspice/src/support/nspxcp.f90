      SUBROUTINE NSPXCP ( STRING, ERROR, SCREEN, LOGFIL )

      IMPLICIT NONE
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!


      CHARACTER*(*)         STRING
      CHARACTER*(*)         ERROR ( 2 )
      CHARACTER*(*)         SCREEN
      CHARACTER*(*)         LOGFIL

      EXTERNAL               NSPWLN



      CHARACTER*(128)       LSTYLE
      CHARACTER*(128)       SSTYLE
      CHARACTER*(128)       MARGIN

      INTEGER               I
      LOGICAL               SCRSTT ( 3 )
      LOGICAL               SAVSTT ( 3 )

      SAVE

      DATA LSTYLE / 'LEFT 1 RIGHT 78' /
      DATA SSTYLE / 'LEFT 1 RIGHT 78' /



      RETURN


      ENTRY NSPERR ( STRING, ERROR )
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!


!
!     This entry point is intended to be called once for
!     brief error diagnostics and a second time for more detailed
!     diagnostics.  We can tell which is which by examining the
!     first entry of the error array.  If it is non-blank this
!     must be the first such call (because we set it to blank
!     after we get done doing something with it).  A second
!     call can only happen if the special command
!     was entered by the user ('?').  In this case the command
!     manager will not reset the error array and not pass the
!     command to any other routines.  Instead it returns immediately
!     so that this routine can process the second part of the
!     error message.
!

      IF ( ERROR(1) .NE. ' ' ) THEN
!
!        We automatically clear the procedure stack whenever
!        an error occurs.
!
         CALL PRCLR ()

!
!        First inhibit writing to the log file.
!
         CALL NSPIOH ( 'LOG' )

!
!        Now write out only the first component of the error message.
!
         CALL NSPMRG   (            MARGIN         )
         CALL SUFFIX   ( SSTYLE, 1, MARGIN         )
         CALL NICEPR_1 ( ERROR(1),  MARGIN, NSPWLN )

!
!        Now inhibit writing to the screen or the save file. But
!        fetch their current state so that we can reset them
!        to exactly their current states.
!
         CALL NSPGST ( 'SCREEN', SCRSTT )
         CALL NSPGST ( 'SAVE',   SAVSTT )

         CALL NSPIOH ( 'SCREEN' )
         CALL NSPIOH ( 'SAVE'   )

!
!        Reactivate the log file.
!
         CALL NSPIOA ( 'LOG'    )

         DO I = 1, 2
            CALL NICEPR_1 ( ERROR(I), LSTYLE, NSPWLN )
         END DO

         CALL NSPPST ( 'SCREEN', SCRSTT )
         CALL NSPPST ( 'SAVE',   SAVSTT )

         ERROR(1) = ' '
         RETURN

      END IF

!
!     The only way to get here is for the user to have processed
!     the first half of an error and typed a question mark or
!     blank command. (This relies on all kinds of side effects.
!     Better talk to Bill if you want to be able to figure this out).
!

      IF ( STRING   .EQ. '?' ) THEN

         IF (ERROR(2) .EQ. ' ' ) THEN
            CALL TRNLAT ( 'NOMOREDIAGNOSTICS', ERROR(2) )
         END IF
!
!        We've already written the second part of the error
!        message to the log file, so we shall inhibit writing
!        there now.
!
         CALL NSPIOH ( 'LOG' )

         CALL NSPMRG   (            MARGIN         )
         CALL SUFFIX   ( SSTYLE, 1, MARGIN         )
         CALL NICEPR_1 ( ERROR(2),  MARGIN, NSPWLN )
!
!        Now re-activate the log file.
!
         CALL NSPIOA ( 'LOG' )

         ERROR(2) = ' '

      END IF

      RETURN


!
!     Set the style string that shall be used for printing
!     errors.
!
      ENTRY NSPSTY ( SCREEN, LOGFIL  )
!
! $ Version
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!


      SSTYLE = SCREEN
      LSTYLE = LOGFIL

      RETURN

      END
