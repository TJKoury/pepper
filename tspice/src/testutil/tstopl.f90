      SUBROUTINE TSTOPL ( LOGNAM, VERSN )
      use pepper_legacy

!
! $ Version
!
!-     Test Utilities Version 1.1.0, 12-MAY-1998 (WLT)
!
!         Added output of the toolkit version to the logfile.
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!

      IMPLICIT NONE
      CHARACTER*(*)         LOGNAM
      CHARACTER*(*)         VERSN
!
!     This routine opens the log file that will be used for loging
!     commands.  It should only be called once.  If a log file
!     cannot be opened, the routine stop.
!

      INTEGER               FILEN
      PARAMETER           ( FILEN = 255 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE  = 80 )

      INTEGER               ROOM
      PARAMETER           ( ROOM   = 2 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )


      CHARACTER*(FILEN)     LOGFIL
      CHARACTER*(LNSIZE)    ENV  ( 2    )

      CHARACTER*(WDSIZE)    ATTR ( ROOM )
      CHARACTER*(WDSIZE)    TIME
      CHARACTER*(WDSIZE)    TKV

      INTEGER               N
      SAVE


!
!     Empty out the internal error buffers.
!

      CALL TSTFIL ( LOGNAM, 'LOG', LOGFIL )

      IF ( FAILED() ) THEN
         WRITE (*,*)
         WRITE (*,*) 'A log file cannot be opened.'
         WRITE (*,*)
         STOP
      END IF
!
!     Fetch the current time and the current platform information.
!
      CALL CURTIM ( TIME              )
      CALL PLTFRM ( ROOM,   N,  ATTR  )
      CALL TKVRSN ( 'TOOLKIT',  TKV   )

      ENV(1) = '--'
      ENV(2) = '--'
      CALL SUFFIX ( ATTR(1), 1, ENV(1)   )
      CALL SUFFIX ( '--',    1, ENV(1)   )
      CALL SUFFIX ( ATTR(2), 1, ENV(2)   )
      CALL SUFFIX ( '--',    1, ENV(2)   )

      CALL TSTLOG ( ENV(1),    .FALSE. )
      CALL TSTLOG ( ENV(2),    .FALSE. )
      CALL TSTLOG ( VERSN,     .FALSE. )
      CALL TSTLOG ( TKV,       .FALSE. )
      CALL TSTLOG ( TIME,      .FALSE. )
!
!     Save the Environment, Version and Time so that it may be used
!     in an error or pass log.
!
      CALL TSTSAV ( ENV, VERSN, TIME )

      RETURN


      END










