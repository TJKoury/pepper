C$Procedure      TCASE (Test Case)
 
      SUBROUTINE TCASE ( TITLE )
 
C$ Abstract
C
C     Set the title for the next test case and log the success of
C     the last test case if it passed and logging of individual
C     test case success is enabled.
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
C      None.
C
C$ Keywords
C
C       TESTING
C
C$ Declarations
 
      IMPLICIT NONE
      CHARACTER*(*)         TITLE
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      TITLE      I   The title of this test case.
C
C$ Detailed_Input
C
C     TITLE       is the title of a test case.  It should have no
C                 more than 32 characters.  If it does characters
C                 beyond the thirty second character are ignored.
C
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C     None.
C
C$ Files
C
C     If test case success logging is enabled, the previous test
C     case success is logged to SCREEN and the log file.
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This is the user interface routine for initializing test cases.
C
C$ Examples
C
C     Later.
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    Testing Utilities 3.3.0, 19-JUN-2001 (WLT)
C
C        Added a call to TSTMOF to turn off the test message from the
C        last test case.
C
C-    Testing Utilities 3.2.0, 11-NOV-1999 (WLT)
C
C        Declared SETOFF to be a function and it is now called
C        as a function instead of as a subroutine.
C
C-    Testing Utilities 3.0.0, 27-JUL-1999 (WLT)
C
C        Added code so that if Error Handling status was not
C        checked in the previous test case, it will be done
C        here.
C
C-    Testing Utilities 2.0.0, 20-JUN-1997 (WLT)
C
C        Added the tests to make sure that the current SPICE
C        trace is the same at the end of a test case as it was
C        at the beginning of a test case.  This identifies routines
C        that checkin but don't check out.
C
C-    Testing Utilities 1.0.0, 18-NOV-1994 (WLT)
C
C
C-&
 
C$ Index_Entries
C
C     Initializing a test case.
C
C-&
C
C     Take care of resetting the error handling and logging the
C     last test case if it passed.
C
C
C     Test Utility Functions
C
      LOGICAL               VERBOS
      LOGICAL               ISOFF
      LOGICAL               SETOFF
 
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 100 )
 
      INTEGER               LNGSTR
      PARAMETER           ( LNGSTR = 1200 )
 
      CHARACTER*(LNSIZE)    MESSGE
      CHARACTER*(LNGSTR)    CTRACE
      CHARACTER*(LNGSTR)    OTRACE
 
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )
 
      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD
      CHARACTER*(STYSIZ)    DOING
 
      LOGICAL               OK
      LOGICAL               DUMMY
      SAVE
 
C
C     See if exceptions were examined in the last test case.
C
      IF ( ISOFF ( 'CHCKXC' ) ) THEN
         CALL CHCKXC ( .FALSE., ' ',  OK )
         DUMMY = SETOFF ( 'CHCKXC' )
      END IF
C
C     Any message created with TSTMSG is no longer relevent.  Disable
C     any such message.
C
      CALL TSTMOF
 
 
C
C     Check the current traceback to make sure things that
C     checked in checked out in the last test case.
C
      CALL QCKTRC  ( CTRACE )
      CALL T_TRACE ( 'GET', OTRACE )
 
      CALL CHCKSC ( 'Current Trace', CTRACE, '=', OTRACE,  OK )
 
      CALL TSTLIP ()
      CALL T_CASE ( TITLE )
      CALL TSTRUL
      CALL TSTRUL
 
      IF ( VERBOS() ) THEN
 
C
C        Copy the current logging style.
C
         CALL TSTSTY ( GOOD, BAD )
 
         DOING  = 'LEFT 1 RIGHT 78 HARDSPACE $ FLAG Performing$:'
         MESSGE = '  '
 
         CALL TSTLGS   ( DOING,   DOING  )
         CALL T_CNAME  ( MESSGE          )
         CALL TSTLOG   ( MESSGE, .FALSE. )
 
C
C        Reset the logging style back to what it was.
C
         CALL TSTLGS   ( GOOD,    BAD    )
 
      END IF
 
      CALL T_TRACE ( 'SET', CTRACE )
      CALL RESET
      CALL ERRDEV  ( 'SET', 'NULL' )
 
      RETURN
      END
