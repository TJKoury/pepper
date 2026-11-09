!$Proceedure CMLOOP ( Command line loop )
!
      SUBROUTINE CMLOOP ( DELIM,  PROMPT, LOGNAM, VERSN, &
                          GREET,  PREPRC, ACTION )
      IMPLICIT NONE
!
! $ Abstract
!
!     This routine handles the main processing loop of a
!     command driven program.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     INTERFACE
!
! $ Declarations

      CHARACTER*(*)         DELIM
      CHARACTER*(*)         PROMPT
      CHARACTER*(*)         LOGNAM
      CHARACTER*(*)         VERSN

      EXTERNAL              GREET
      EXTERNAL              PREPRC
      EXTERNAL              ACTION

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     DELIM      I   Non-blank character used to delimit commands
!     PROMPT     I   Prompt to let the user know input is expected
!     LOGNAM     I   Name pattern of file where commands will be logged
!     VERSN      I   Program name and version
!     INTIZE     S   Subroutine that initializes I/O facilities
!     GREET      S   Displays a banner at program startup
!     ACTION     S   The command parser and processor.
!
! $ Detailed_Input
!
!     DELIM     is a character that will be used to tell the
!               program that a command has ended. Commands may
!               extend over as many lines as allowed by the
!               configuration include file.  They end on the
!               first line on which the delimiter character is
!               encountered. THIS CHARACTER MUST NOT BE "?"
!
!     PROMPT    is a string used to prompt the user for commands.
!               Typically, this is the name of the program that
!               calles CMLOOP.
!
!     LOGNAM    is a pattern to use when creating the name of
!               a file to which all commands will be written.
!               This can be hard coded in the calling
!               program, or may be determined by a file naming
!               convention such as is provided by Christen
!               and NOMEN.
!
!     VERSN     is a string that may contain anything you would
!               like to appear as descriptive text in the first
!               line of the log file (and possibly in the greeting
!               presented by the program)  Something like
!               '<program name> --- Version X.Y' would be appropriate.
!               For example if your programs name is KINDLE and you
!               are at version 4.2.3 of your program a good value for
!               VERSN would be
!
!               'KINDLE --- Version 4.2.3'
!
!               Your greeting routine can make use of this when
!               displaying your program's greeting.  In this way
!               you can centralize the name and version number of
!               your program at a high level or in a subroutine and
!               simply make the information available to CMLOOP so
!               that the automatic aspects of presenting this
!               information can be handled for you.
!
!
!     GREET     is a routine that displays a message at program
!               startup.  This should contain the version number
!               of the program, any general instructions such
!               as how to get help and who the author or organization
!               is that is responsible for this program. GREET
!               takes a single argument VERSN which you supply in
!               your call to CMLOOP.  It may also have
!               initializations that override various items set
!               up prior to the call to GREET such as the style
!               used for displaying error messages.  GREET
!               is the action taken by CMLOOP  before commencing the
!               loop of fetching and processing commands.
!
!     PREPRC    is a command preprocessor.  It might remove
!               non-printing characters such as TABS, resolve
!               symbols and convert units to expected ranges.
!
!     ACTION    is a routine responsible for action upon the commands
!               entered by a user at the keyboard. ACTION has two
!               arguments COMMAND a string input and ERROR a two
!               dimensional array for error and diagnostic output.
!               The first message should point to the the problem
!               assuming the user is aware of the context in which
!               the problem occurred.  The second message will
!               have more detailed information including trace
!               and other technical information.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     The parameters COMSIZ and ERRSIZ are given in the include
!     file commdpar.inc.
!
!     COMSIZ  is maximum number of characters that can be present
!             in a command.
!
!     ERRSIZ  is the maximum number of characters that can be used
!             when creating a diagnostic message.
!
! $ Exceptions
!
!     None.  This routine cannot detect any errors in its inputs
!     and all commands are regarded as legal input at this level.
!     Some can be acted on while others cannot.  Commands that
!     can not be exercised are expected to return diagnostics
!     in the array ERROR.  These are then reported by the
!     program to the user via his/her terminal.
!
! $ Files
!
!     The file specified by LOGFIL will be opened if possible
!     and all user commands and messages will be written to this
!     file.
!
!     Other files may be used a run time by "STARTing" a command
!     sequence file. Or by some result of the activity of the
!     user supplied routines ACTION, GREET, PREPRC.
!
! $ Particulars
!
!     This routine organizes the main loop of a command line
!     program so that the calling program can automatically
!     log files that a user enters, report errors in a uniform
!     manner and make use of sequences of commands stored in
!     files. The calling program supplies routines that handle
!     the chores of greeting the user and performing special
!     program initializations and performing actions based upon
!     the commands supplied by the user.  By making use of this
!     routine and its subordinates, the user inherits a flexible
!     I/O system and command interface freeing him/her to concentrate
!     on the actions of the program.
!
!     However, there is a minor price incurred by making use of
!     this routine.  Several commands have specific meanings that
!     the user cannot override.  They are commands that start with:
!
!        start
!        exit
!        stop
!        quit
!        echo
!        no echo
!        demo on
!        demo off
!        wait on
!        wait off
!        pause
!        ?
!     These commands are case insensitive with respect to the
!     words presented above.
!
!
! $ Examples
!
!     None.
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
!     W.L. Taber    (JPL)
!
! $ Version
!
!-     Command Loop Configured Version 6.0.0, 20-JUN-2000 (WLT)
!
!         Added the ability to run the loop without logging
!         of inputs.
!
!-     Command Loop Configured Version 5.0.0, 23-MAR-2000 (WLT)
!
!         Modified the routine to call NSPEND instead of FINISH
!         now that NSPIO has been redone.
!
!-     Command Loop Configured Version 4.0.0, 20-NOV-1995 (WLT)
!
!         Added ability to run programs in batch mode and to
!         start procedures at program startup.
!
!-     Command Loop Configured Version 3.0.0, 1-AUG-1995 (WLT)
!
!         The routine was modified to better support command
!         pre-processing.  In particular symbol definition
!         and resolution is now supported.
!
!-     Command Loop Configured Version 2.0.0, 19-JUL-1995 (WLT)
!
!         A slight change was made so that the command delimiter
!         is now stored in the routine GETDEL.  Also errors
!         are now checked after command pre-processing has
!         been performed.
!
!-     Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!
!-    Beta Version 1.0.0, 8-OCT-1993 (WLT)
!
!-&

      INTEGER               NONE
      PARAMETER           ( NONE   = 0 )

      INTEGER               COMBUF
      PARAMETER           ( COMBUF = NONE   + 1 )

      INTEGER               KEYBRD
      PARAMETER           ( KEYBRD = COMBUF + 1 )

      INTEGER               INPFIL
      PARAMETER           ( INPFIL = KEYBRD + 1 )

      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )

      CHARACTER*(FILSIZ)    USENAM
!
!     Language Sensitive Strings
!
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(WDSIZE)    ERRFLG






      INTEGER               ISRCHC
      INTEGER               LTRIM
      INTEGER               QRTRIM

      LOGICAL               HAVE
      LOGICAL               NO
      LOGICAL               CMMORE
      LOGICAL               DOLOG

      INCLUDE              'commdpar.inc'

      CHARACTER*(COMSIZ)    COMMND
      CHARACTER*(COMSIZ)    COM2DO
      CHARACTER*(ERRSIZ)    ERROR ( 2 )
      LOGICAL               PROBLM

      CHARACTER*(STYSIZ)    SSTYLE
      CHARACTER*(STYSIZ)    LSTYLE
      CHARACTER*(STYSIZ)    VSTYLE
      CHARACTER*(STYSIZ)    HSTYLE

      INTEGER               FROM
      INTEGER               L
      INTEGER               REST

      LOGICAL               LOG ( 0:3 )
      LOGICAL               TRAP
      LOGICAL               HIT
!
!     The following are for special commands that will not be
!     processed by ACTION.
!
      INTEGER               NSP
      PARAMETER           ( NSP = 2 )

      CHARACTER*(8)         SPCIAL ( NSP )
      SAVE

      DATA  SPCIAL / ' ', '?' /
!
!     Store the delimiter used by the program incase someone
!     else needs to know later on.
!
      CALL SETDEL ( DELIM )


!
!     First, set up the SPICELIB error handling.
!
      ERROR(1) = ' '
      ERROR(2) = ' '
      COMMND   = ' '

      LOG (NONE  )  = .FALSE.
      LOG (COMBUF)  = .FALSE.
      LOG (KEYBRD)  = .TRUE.
      LOG (INPFIL)  = .TRUE.

      CALL ERRACT ( 'SET', 'RETURN' )
      CALL ERRDEV ( 'SET', 'NULL'   )

!
!     Set the prompt for the program.
!
      CALL SETDAP ( DELIM, PROMPT )


!
!     The following styles are for reporting errors to the
!     screen and log file respectively.
!
      CALL TRNLAT ( 'ERRFLAG',  ERRFLG )

      SSTYLE = 'HARDSPACE ^ NEWLINE /cr ' &
      //       'VTAB /vt FLAG ' // ERRFLG

      LSTYLE = 'HARDSPACE ^ NEWLINE /cr ' &
      //       'VTAB /vt FLAG ' &
      //        DELIM(1:1) // ERRFLG(1:QRTRIM(ERRFLG)) &
      //       ' LEADER '  // DELIM(1:1) // '-- ' &
      //       'LEFT 1 RIGHT 72 '
!
!     The following styles will be used for logging of
!     commands and for commenting them out.
!
      VSTYLE = 'LEFT 1 RIGHT 78 '
      HSTYLE = 'LEFT 1 RIGHT 78 LEADER ' // DELIM(1:1) // '-- '

      CALL NSPSTY ( SSTYLE, LSTYLE        )
      CALL NSPLGS ( VSTYLE, HSTYLE, DELIM )
      CALL NSPSLR ( 1, 78 )

!
!     See whether or not a log file should be used and if so
!     what it's name should be.
!
      CALL LOGCHK ( LOGNAM, USENAM, DOLOG )

!
!     Open a log file.
!
      IF ( DOLOG ) THEN
         CALL NSPOPL ( USENAM, VERSN )
      END IF

      IF ( HAVE(ERROR) ) THEN
         CALL NSPERR ( COMMND, ERROR )
      END IF
!
!     Present a greeting to the user and perform any override
!     or special initializations that need to be local to this
!     routine.
!
      CALL GREET ( VERSN )
!
!     Get the input command line.  This may have
!     several useful bits of information to tell us how
!     to run the program.
!
!     -b      means run the program in batch mode.  In this case
!             we should never prompt the user for information.
!
!     -start  means we have a startup file to use and we want to
!             use the name of that file to determine how to
!             proceed.
!
      CALL CMSTUP

!
!     Fetch and log the first command.
!

      TRAP = .TRUE.
!
!     Get the next command and resolve any symbols or
!     queries that might show up in it,
!
      DO WHILE ( TRAP )

         CALL GETCOM (                COM2DO,  FROM )
         CALL EDTCOM ( DELIM, PROMPT, COM2DO,  FROM )

         IF ( NO(ERROR) .AND. LOG(FROM) ) THEN
            CALL NSPLOG ( COM2DO,  .FALSE. )
         END IF

         IF ( NO(ERROR) ) THEN
            CALL RESSYM ( COM2DO, COMMND )
            CALL ECHO   ( COM2DO, COMMND )
         END IF

         IF ( NO(ERROR) ) THEN
            CALL CMREDO ( COMMND, FROM, TRAP )
         END IF

         IF ( HAVE(ERROR) ) THEN
            TRAP = .FALSE.
         END IF

      END DO

!
!     Now apply the user's preprocessing software
!     to the comman.
!
      COM2DO = COMMND

      CALL PREPRC ( COM2DO, COMMND )

!
!     Now process commands until we get an EXIT command.
!
      DO WHILE ( CMMORE( COMMND ) )
!
!        Perform any preprocessing that can be performed easily
!        on this command.
!

         IF ( NO(ERROR) ) THEN
            CALL BUILTN ( COMMND, HIT, ERROR )
         END IF

         IF ( NO(ERROR) .AND. .NOT. HIT ) THEN

            L    = LTRIM  ( COMMND)
            REST = QRTRIM ( COMMND ) + 1

            IF ( ISRCHC( COMMND(L:REST), NSP, SPCIAL ) .EQ. 0 ) THEN
               CALL ACTION ( COMMND, ERROR  )
            END IF

         END IF

         PROBLM = HAVE(ERROR)
!
!        Process any errors that were diagnosed.
!
         CALL NSPERR ( COMMND, ERROR )
!
!        Fetch and log the next command.
!
         TRAP = .TRUE.

         DO WHILE ( TRAP )

            CALL GETCOM (                COM2DO,  FROM )
            CALL EDTCOM ( DELIM, PROMPT, COM2DO,  FROM )

            IF ( NO(ERROR) .AND. LOG(FROM) ) THEN
               CALL NSPLOG ( COM2DO,  .FALSE. )
            END IF

            IF ( NO(ERROR) ) THEN
               CALL RESSYM ( COM2DO, COMMND )
               CALL ECHO   ( COM2DO, COMMND )
            END IF

            IF ( NO(ERROR) ) THEN
               CALL CMREDO ( COMMND, FROM, TRAP )
            END IF

            IF ( HAVE(ERROR) ) THEN
               TRAP = .FALSE.
            END IF

         END DO
!
!        Now apply the user's preprocessing software
!        to the comman.
!
         COM2DO = COMMND
         CALL PREPRC ( COM2DO, COMMND )

      END DO
!
!     Take care of closing files and so on.
!

      IF ( LOG(FROM) ) THEN
         CALL NSPEND
      END IF

      RETURN
      END
