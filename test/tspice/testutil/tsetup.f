C$Procedure TSETUP ( Test utility setup )

      SUBROUTINE TSETUP ( CMLINE, LOGNAM, VERSN )
      IMPLICIT NONE

C$ Abstract
C
C     This routine handles the initializations needed for making use
C     of the SPICE testing utilities.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TESTING
C     INTERFACE
C
C$ Declarations

      CHARACTER*(*)         CMLINE
      CHARACTER*(*)         LOGNAM
      CHARACTER*(*)         VERSN

C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     CMLINE     I   The command line string.
C     LOGNAM     I   Name pattern of file where commands will be logged.
C     VERSN      I   Program name and version.
C
C$ Detailed_Input
C
C     CMLINE    the command line line string containing any options
C               to modify the test run behavior.
C
C               Accepted option flags:
C
C                  -V   activate verbose logging. This logs every test 
C                       comparison performed.  You should probably never
C                       use this option. 
C
C                  -C   activate the logging of individual test cases.
C
C                  -D   activate debugging mode, files are not
C                       automatically deleted when a new test family is
C                       initiated.
C
C                  -Q   suppress output to standard out.
C
C                  CMLINE is case insensitive.
C
C     LOGNAM    is a pattern to use when creating the name of
C               a file to which all commands will be written.
C               This can be hard coded in the calling
C               program, or may be determined by a file naming
C               convention such as is provided by Christen
C               and NOMEN.
C
C     VERSN     is a string that may contain anything you would
C               like to appear as descriptive text in the first
C               line of the log file (and possibly in the greeting
C               presented by the program)  Something like
C               '<program name> --- Version X.Y' would be appropriate.
C               For example if your programs name is KINDLE and you
C               are at version 4.2.3 of your program a good value for
C               VERSN would be
C
C               'KINDLE --- Version 4.2.3'
C
C               Your greeting routine can make use of this when
C               displaying your program's greeting. In this way
C               you can centralize the name and version number of
C               your program at a high level or in a subroutine and
C               simply make the information available to TSETUP so
C               that the automatic aspects of presenting this
C               information can be handled for you.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     None. This routine cannot detect any errors in its inputs
C     and all commands are regarded as legal input at this level.
C
C$ Files
C
C     The file specified by LOGFIL will be opened if possible
C     and all test results will then be stored in that file.
C
C     Other files may be used a run time by "STARTing" a command
C     sequence file. Or by some result of the activity of the
C     user supplied routines ACTION, GREET, PREPRC.
C
C$ Particulars
C
C     This routine preforms the initializations needed for using
C     the NAIF test utilities. It should be called once in your
C     test program.
C
C     The routine process an input string, CMLINE, to determine any 
C     run-time options.
C
C$ Examples
C
C     None.
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     W.L. Taber    (JPL)
C
C$ Version
C
C-     Test Utilities Version 2.0.0, 19-MAY-2010 (EDW)
C
C         Eliminated GETCML call from TSETUP. The command line
C         passes into TSETUP as an argument CMLINE.
C
C         The addition of the CMLINE argument nullifies the need to use
C         the "tspice.ker" kernel to pass command line arguments
C         in environments without shells, e.g IDL, Matlab. The code
C         implementing loading and processing of "tspice.ker" has
C         been removed.
C
C-     Test Utilities Version 1.3.0, 09-MAY-2002 (EDW)
C
C         Added capability to read command line options from
C         a text kernel. tspice checks for the existence of
C         the kernel "tspice.ker" if present, and if no arguments
C         supplied on the command line, tspice reads the
C         COMMAND_LINE string variable from the kernel then uses
C         that string as the command line.
C
C-     Test Utilities Version 1.2.0, 24-APR-2002 (EDW)
C
C         Added the quiet mode, activated from the command line
C         by the '-Q' argument. The mode prevents output to the
C         standard device, i.e. the screen. Modifications made to
C         tstio.f.
C
C         Moved command line parse code to a position so it executes
C         prior to logging functions.
C
C-     Test Utilities Version 1.1.0, 27-JUL-1999 (WLT)
C
C         Added initialization for automatic checks of SPICE
C         exception handling. Added "DEBUGGING" mode to Test Utilites.
C
C-     Test Utilities Version 1.0.0,  3-NOV-1994 (WLT)
C
C-&


C
C     Test Utility Functions
C
      LOGICAL               VERBON
      LOGICAL               SETOFF
      LOGICAL               SETON

C
C     Local Variables
C
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



C
C     The following styles are for reporting errors to the
C     screen and log file respectively.
C
      SSTYLE = 'HARDSPACE ^ NEWLINE /cr '
     .//       'VTAB /vt FLAG Failure: '

      LSTYLE = 'HARDSPACE ^ NEWLINE /cr '
     .//       'VTAB /vt FLAG Failure: '
     .//       'LEADER -- '
     .//       'LEFT 1 RIGHT 72 '


C
C     Set the exception handling status to OFF and set the debugging
C     status to OFF.
C
      DUMMY = SETOFF ( 'CHCKXC' )
      DUMMY = SETOFF ( 'DEBUGGING' )

C
C     The following styles will be used for logging of
C     commands and for commenting them out.
C
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
C
C           We want to run in debugging mode. This means
C           files are not automatically deleted when a new
C           test family is initiated.
C
            ON = SETON ( 'DEBUGGING' )

         END IF

C
C        Check for quiet mode; supress all output to the
C        standard IO device.
C
         IF ( WORD .EQ. '-Q' ) THEN

            CALL LCKOUT ( 'SCREEN' )

         END IF

      END DO

C
C     Open a log file.
C
      CALL TSTOPL ( LOGNAM, VERSN )

C
C     Now, set up the SPICELIB error handling.
C
      CALL ERRACT ( 'SET', 'RETURN' )
      CALL ERRDEV ( 'SET', 'NULL'   )


      RETURN
      END

