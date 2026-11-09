!$Procedure      LOGCHK ( Log file check )

      SUBROUTINE LOGCHK ( DEFALT, USENAM, DOLOG )

! $ Abstract
!
!     Determine whether to use a log file, and if so what name
!     pattern to use.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     UTILITY
!
! $ Declarations

      IMPLICIT NONE
      CHARACTER*(*)         DEFALT
      CHARACTER*(*)         USENAM
      LOGICAL               DOLOG

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     DEFALT     I   Default logfile name pattern
!     USENAM     O   Acutal logfile name pattern that will be used.
!     DOLOG      O   Flag indicating whether or not to use a log file.
!
! $ Detailed_Input
!
!     DEFALT     is a default pattern to use if nothing is specified
!                on the command line.
!
! $ Detailed_Output
!
!     USENAM     is the name to use for the log file or blank
!                if the -nolog flag is supplied on the command line.
!
!     DOLOG      is a logical flag that indicates whether or not
!                to create a log file.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This is a utility routine for use by the "Command Loop" routines
!     so that one can specify a custom name for a log file (or
!     specify that no log file be used at all.
!
!     The options examined from the command line are:
!
!       -nolog
!       -log <filename>
!
!      This routine does not judge the "fitness" of the name of
!      the logfile, if one is specified on the command line.  Checking
!      for suitability is left to other portions of the system.
!
! $ Examples
!
!     None.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 28-DEC-2001 (WLT)
!
!
!-&
!
!     Spicelib Functions.
!
      LOGICAL               EQSTR

      INTEGER               LONGSZ
      PARAMETER           ( LONGSZ = 900 )

      CHARACTER*(LONGSZ)    LINE

      INTEGER               B
      INTEGER               E
      INTEGER               START
!
!     Until we know otherwise, we set the logname to the default
!     value and set action to "use a log file".
!
      USENAM = DEFALT
      DOLOG  = .TRUE.
      START  =  1

      CALL GETCML ( LINE )
      CALL FNDNWD ( LINE, START, B, E )

      DO WHILE ( B .GT. 0 )

         START = E+1

         IF ( EQSTR( LINE(B:E), '-nolog' ) ) THEN

            USENAM = ' '
            DOLOG  = .FALSE.
            RETURN

         ELSE IF ( EQSTR( LINE(B:E), '-log' ) ) THEN


            CALL FNDNWD ( LINE, START, B, E )

            IF ( E .GT. B ) THEN
               USENAM = LINE(B:E)
            END IF
            RETURN

         END IF

         CALL FNDNWD ( LINE, START, B, E )

      END DO

      RETURN
      END
