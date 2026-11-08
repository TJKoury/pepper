C$Procedure      TCLOSE (Close testing.)
 
      SUBROUTINE TCLOSE
 
C$ Abstract
C
C     Close out all  testing.
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
 
C$ Brief_I/O
C
C      None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      None.
C
C$ Particulars
C
C     This routine takes care of the problems of finishing a test
C     program.  It displays a summary of all testing, closes the
C     test log and failure log.  If no failures have occurred the
C     file 'passage.tst' is created and filled with a brief message
C     to indicate that all tests passed.
C
C$ Examples
C
C     Test functions
C
C     LOGICAL               T_MYROUTINE
C     LOGICAL               T_ROUTINE2
C
C     LOGICAL               RESULT
C
C     Enable the testing software.
C
C     CALL TSETUP ('test{0-9}{0-9}{0-9}{0-9}.log', '1.0.0' )
C
C     Open the first test case.
C
C     CALL TOPEN  ('MYROUTINE' )
C     RESULT = T_MYROUTINE ()
C
C     CALL TOPEN  ('ROUTINE2'  )
C     RESULT = RESULT .AND. T_ROUTINE2()
C
C     CALL TCLOSE
C     END
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Version
C
C-    Testing Utilities Version 3.1.0, 04-SEP-2013 (BVS)
C
C        Added getting and printing the current time at the very end.
C
C-    Testing Utilities Version 3.0.0, 27-JUL-1999 (WLT)
C
C        Added code so that if Error Handling status was not
C        checked in the previous test case, it will be done
C        here.  Moreover, all test files that have not been
C        deleted are deleted (provided the test program is not
C        in DEBUGGING mode.
C
C-    Testing Utilities Version 2.0.0,  4-APR-1994 (WLT)
C
C        The wrong filename was being written into the pass file.
C        Now the logfile name is recovered and resaved after the
C        pass file is opened.
C
C-    Testing Utilities Version 1.0.0, 4-NOV-1994 (WLT)
C
C
C-&
C
C     Test Utility Functions
C
 
      LOGICAL               ISOFF
      LOGICAL               SETOFF
 
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )
 
      CHARACTER*(LNSIZE)    ENV
      CHARACTER*(LNSIZE)    VERSN
      CHARACTER*(LNSIZE)    TIME
      CHARACTER*(LNSIZE)    TIME2
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    NAME
 
      INTEGER               MSGSIZ
      PARAMETER           ( MSGSIZ = 256 )
 
      CHARACTER*(MSGSIZ)    MESSGE
 
      INTEGER               FILSIZ
      PARAMETER           ( FILSIZ = 255 )
 
      CHARACTER*(FILSIZ)    FILE
      CHARACTER*(FILSIZ)    LOGFIL
 
      INTEGER               NFAIL
      INTEGER               NCASE
 
      LOGICAL               FAILUR
      LOGICAL               DUMMY
      LOGICAL               OK
 
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )
 
      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD
 
 
 
C
C     See if exceptions were examined in the last test case.
C
      IF ( ISOFF ( 'CHCKXC' ) ) THEN
         CALL CHCKXC ( .FALSE., ' ',  OK )
         DUMMY = SETOFF ( 'CHCKXC' )
      END IF
 
      CALL RESET  ()
      CALL ERRDEV ( 'SET', 'NULL'   )
 
      CALL TSTSTY ( GOOD, BAD )
 
C
C     Log the last test case if it passed.
C
      CALL TSTLIP ()
C
C     Draw any rules that might be needed by verbose output.
C
      CALL TSTRUL
      CALL TSTRUL
C
C     Put a blank line between the cases and our output if we are in
C     case reporting or verbose mode.
C
      CALL TSTCBL ()
 
C
C     We need to close out the old case
C
      CALL T_NAME   ( NAME   )
      CALL T_FCOUNT ( NFAIL  )
      CALL T_CNUM   ( NCASE  )
      CALL T_SUCCESS( OK     )
 
 
 
      IF ( OK ) THEN
 
         MESSGE = 'Passed  --- Test Family: '
         CALL SUFFIX ( NAME, 1, MESSGE )
 
         CALL TSTLOG ( MESSGE, .FALSE.        )
 
         CALL TSTLGS ( 'LEFT 9 RIGHT 78 FLAG --- NEWLINE /cr',
     .                 'LEFT 9 RIGHT 78 FLAG --- NEWLINE /cr' )
 
 
         MESSGE = 'Successful tests for # of # test '
     .   //       'cases. '
         CALL REPMCT ( MESSGE, '#',   NCASE, 'L', MESSGE )
         CALL REPMCT ( MESSGE, '#',   NCASE, 'L', MESSGE )
         CALL TSTLOG ( MESSGE, .FALSE.              )
 
 
      ELSE
 
         MESSGE = 'FAILURE for Test Family: '
         CALL SUFFIX ( NAME, 1, MESSGE )
 
         CALL TSTLOG ( MESSGE, .FALSE.        )
 
         CALL TSTLGS ( 'LEFT 24 RIGHT 78 FLAG : NEWLINE /cr',
     .                 'LEFT 24 RIGHT 78 FLAG : NEWLINE /cr' )
         MESSGE = '# of # test cases failed. '
         CALL REPMCT ( MESSGE, '#',   NFAIL, 'C', MESSGE )
         CALL REPMCT ( MESSGE, '#',   NCASE, 'L', MESSGE )
         CALL TSTLOG ( MESSGE, .FALSE.              )
 
      END IF

C
C     Get current time. We will print it at the very end.
C
      CALL CURTIM ( TIME2 )


      CALL TSTLGS ( GOOD, BAD )
C
C     Now we check on the overall success of the testing.
C
      CALL T_ANYBAD ( FAILUR )
 
      IF ( .NOT. FAILUR ) THEN
 
         CALL TSTGLF ( LOGFIL )
         CALL TSTFIL ( 'pass{0-9}{0-9}{0-9}{0-9}.log', 'SAVE', FILE )
         CALL TSTSLF ( LOGFIL )
 
         CALL TSTGET (  ENV, VERSN, TIME )
C
C        We temporarily turn off writing to the log file and screen
C        so that we can put the test stamp in the pass file.
C
         CALL TSTIOA ( 'SAVE'   )
         CALL TSTIOH ( 'LOG'    )
         CALL TSTIOH ( 'SCREEN' )
 
         CALL TSTWLN ( ENV   )
         CALL TSTWLN ( VERSN )
         CALL TSTWLN ( TIME  )

C
C        Now reactivate the screen and log file.
C
         CALL TSTIOA ( 'LOG'    )
         CALL TSTIOA ( 'SCREEN' )
C
C        Write out the testing summary.
C
         CALL TSTWLN ( ' ' )
         CALL TSTWLN ( 'All tests passed.  ' )
 
         CALL TSTIOH ( 'SCREEN' )
         CALL TSTWLN ( 'Tests performed were:' )
         CALL TSTWLN ( ' ' )
 
         CALL TBEGF  (      )
         CALL TFNAME ( NAME )
 
         DO WHILE ( NAME .NE. ' ' )
            CALL TSTWLN ( NAME )
            CALL TFNAME ( NAME )
         END DO
C
C        Turn off writing to the log file,  the next message doesn't
C        belong in it.
C
         CALL TSTGLF ( MESSGE )
         CALL PREFIX ( 'For details, see the test log:', 1, MESSGE )
 
         CALL TSTIOH ( 'LOG' )
         CALL TSTWLN ( ' '   )
         CALL TSTLOG ( MESSGE, .FALSE.)
         CALL TSTWLN ( ' '   )

         CALL TSTLOG ( TIME2, .FALSE.)
         CALL TSTWLN ( ' '   )

C
C        Close the pass file.
C
         CALL TSTIOC ( 'SAVE' )
C
C        Reactivate writing to the log file.
C
         CALL TSTIOA ( 'LOG'    )
         CALL TSTIOA ( 'SCREEN' )
 
      END IF
 
C
C     Clean up any files that might be lying around from the
C     last test family.
C
      CALL CSTART

C
C     Finish up everything now.
      CALL TSTLOG ( ' ',   .FALSE. )
      CALL TSTLOG ( TIME2, .FALSE. )
      CALL TSTLOG ( ' ',   .FALSE. )

      CALL FINISH ()
 
      RETURN
      END
