!$Procedure TSETUP ( Test utility setup )

      SUBROUTINE TSETUP ( CMLINE, LOGNAM, VERSN )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     This routine handles the initializations needed for making use
!     of the SPICE testing utilities.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!     INTERFACE
!
! $ Declarations

      CHARACTER*(*)         CMLINE
      CHARACTER*(*)         LOGNAM
      CHARACTER*(*)         VERSN

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     CMLINE     I   The command line string.
!     LOGNAM     I   Name pattern of file where commands will be logged.
!     VERSN      I   Program name and version.
!
! $ Detailed_Input
!
!     CMLINE    the command line line string containing any options
!               to modify the test run behavior.
!
!               Accepted option flags:
!
!                  -V   activate verbose logging. This logs every test
!                       comparison performed.  You should probably never
!                       use this option.
!
!                  -C   activate the logging of individual test cases.
!
!                  -D   activate debugging mode, files are not
!                       automatically deleted when a new test family is
!                       initiated.
!
!                  -Q   suppress output to standard out.
!
!                  CMLINE is case insensitive.
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
!               displaying your program's greeting. In this way
!               you can centralize the name and version number of
!               your program at a high level or in a subroutine and
!               simply make the information available to TSETUP so
!               that the automatic aspects of presenting this
!               information can be handled for you.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None. This routine cannot detect any errors in its inputs
!     and all commands are regarded as legal input at this level.
!
! $ Files
!
!     The file specified by LOGFIL will be opened if possible
!     and all test results will then be stored in that file.
!
!     Other files may be used a run time by "STARTing" a command
!     sequence file. Or by some result of the activity of the
!     user supplied routines ACTION, GREET, PREPRC.
!
! $ Particulars
!
!     This routine preforms the initializations needed for using
!     the NAIF test utilities. It should be called once in your
!     test program.
!
!     The routine process an input string, CMLINE, to determine any
!     run-time options.
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
!-     Test Utilities Version 2.0.0, 19-MAY-2010 (EDW)
!
!         Eliminated GETCML call from TSETUP. The command line
!         passes into TSETUP as an argument CMLINE.
!
!         The addition of the CMLINE argument nullifies the need to use
!         the "tspice.ker" kernel to pass command line arguments
!         in environments without shells, e.g IDL, Matlab. The code
!         implementing loading and processing of "tspice.ker" has
!         been removed.
!
!-     Test Utilities Version 1.3.0, 09-MAY-2002 (EDW)
!
!         Added capability to read command line options from
!         a text kernel. tspice checks for the existence of
!         the kernel "tspice.ker" if present, and if no arguments
!         supplied on the command line, tspice reads the
!         COMMAND_LINE string variable from the kernel then uses
!         that string as the command line.
!
!-     Test Utilities Version 1.2.0, 24-APR-2002 (EDW)
!
!         Added the quiet mode, activated from the command line
!         by the '-Q' argument. The mode prevents output to the
!         standard device, i.e. the screen. Modifications made to
!         tstio.f.
!
!         Moved command line parse code to a position so it executes
!         prior to logging functions.
!
!-     Test Utilities Version 1.1.0, 27-JUL-1999 (WLT)
!
!         Added initialization for automatic checks of SPICE
!         exception handling. Added "DEBUGGING" mode to Test Utilites.
!
!-     Test Utilities Version 1.0.0,  3-NOV-1994 (WLT)
!
!-&


!
!     Test Utility Functions
!
      LOGICAL               VERBON
      LOGICAL               SETOFF
      LOGICAL               SETON

!
!     Local Variables
!
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 128 )

      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )

      CHARACTER*(STYSIZ)    SSTYLE
      CHARACTER*(STYSIZ)    LSTYLE
      CHARACTER*(STYSIZ)    VSTYLE
      CHARACTER*(STYSIZ)    HSTYLE

      CHARACTER*(WDSIZE)    WORD

      LOGICAL               ON
      LOGICAL               DUMMY

      SAVE



!
!     The following styles are for reporting errors to the
!     screen and log file respectively.
!
      SSTYLE = 'HARDSPACE ^ NEWLINE /cr ' &
      //       'VTAB /vt FLAG Failure: '

      LSTYLE = 'HARDSPACE ^ NEWLINE /cr ' &
      //       'VTAB /vt FLAG Failure: ' &
      //       'LEADER -- ' &
      //       'LEFT 1 RIGHT 72 '


!
!     Set the exception handling status to OFF and set the debugging
!     status to OFF.
!
      DUMMY = SETOFF ( 'CHCKXC' )
      DUMMY = SETOFF ( 'DEBUGGING' )

!
!     The following styles will be used for logging of
!     commands and for commenting them out.
!
      VSTYLE = 'LEFT 1 RIGHT 78 '
      HSTYLE = 'LEFT 1 RIGHT 78 LEADER -- FLAG -- '

      CALL TSTLGS ( VSTYLE, HSTYLE )

      DO WHILE ( CMLINE .NE. ' ' )

         CALL NEXTWD ( CMLINE, WORD, CMLINE )
         CALL UCASE  ( WORD,   WORD         )

         IF ( WORD .EQ. '-V' ) THEN

            ON = VERBON()

         ELSE IF ( WORD .EQ. '-C' ) THEN

            CALL TSTLCY

         ELSE IF ( WORD .EQ. '-D' ) THEN
!
!           We want to run in debugging mode. This means
!           files are not automatically deleted when a new
!           test family is initiated.
!
            ON = SETON ( 'DEBUGGING' )

         END IF

!
!        Check for quiet mode; supress all output to the
!        standard IO device.
!
         IF ( WORD .EQ. '-Q' ) THEN

            CALL LCKOUT ( 'SCREEN' )

         END IF

      END DO

!
!     Open a log file.
!
      CALL TSTOPL ( LOGNAM, VERSN )

!
!     Now, set up the SPICELIB error handling.
!
      CALL ERRACT ( 'SET', 'RETURN' )
      CALL ERRDEV ( 'SET', 'NULL'   )


      RETURN
      END

