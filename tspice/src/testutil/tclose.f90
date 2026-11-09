!$Procedure      TCLOSE (Close testing.)

      SUBROUTINE TCLOSE

! $ Abstract
!
!     Close out all  testing.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       TESTING
!
! $ Declarations

      IMPLICIT NONE

! $ Brief_I/O
!
!      None.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!      None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!      None.
!
! $ Particulars
!
!     This routine takes care of the problems of finishing a test
!     program.  It displays a summary of all testing, closes the
!     test log and failure log.  If no failures have occurred the
!     file 'passage.tst' is created and filled with a brief message
!     to indicate that all tests passed.
!
! $ Examples
!
!     Test functions
!
!     LOGICAL               T_MYROUTINE
!     LOGICAL               T_ROUTINE2
!
!     LOGICAL               RESULT
!
!     Enable the testing software.
!
!     CALL TSETUP ('test{0-9}{0-9}{0-9}{0-9}.log', '1.0.0' )
!
!     Open the first test case.
!
!     CALL TOPEN  ('MYROUTINE' )
!     RESULT = T_MYROUTINE ()
!
!     CALL TOPEN  ('ROUTINE2'  )
!     RESULT = RESULT .AND. T_ROUTINE2()
!
!     CALL TCLOSE
!     END
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!      None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Version
!
!-    Testing Utilities Version 3.1.0, 04-SEP-2013 (BVS)
!
!        Added getting and printing the current time at the very end.
!
!-    Testing Utilities Version 3.0.0, 27-JUL-1999 (WLT)
!
!        Added code so that if Error Handling status was not
!        checked in the previous test case, it will be done
!        here.  Moreover, all test files that have not been
!        deleted are deleted (provided the test program is not
!        in DEBUGGING mode.
!
!-    Testing Utilities Version 2.0.0,  4-APR-1994 (WLT)
!
!        The wrong filename was being written into the pass file.
!        Now the logfile name is recovered and resaved after the
!        pass file is opened.
!
!-    Testing Utilities Version 1.0.0, 4-NOV-1994 (WLT)
!
!
!-&
!
!     Test Utility Functions
!

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



!
!     See if exceptions were examined in the last test case.
!
      IF ( ISOFF ( 'CHCKXC' ) ) THEN
         CALL CHCKXC ( .FALSE., ' ',  OK )
         DUMMY = SETOFF ( 'CHCKXC' )
      END IF

      CALL RESET  ()
      CALL ERRDEV ( 'SET', 'NULL'   )

      CALL TSTSTY ( GOOD, BAD )

!
!     Log the last test case if it passed.
!
      CALL TSTLIP ()
!
!     Draw any rules that might be needed by verbose output.
!
      CALL TSTRUL
      CALL TSTRUL
!
!     Put a blank line between the cases and our output if we are in
!     case reporting or verbose mode.
!
      CALL TSTCBL ()

!
!     We need to close out the old case
!
      CALL T_NAME   ( NAME   )
      CALL T_FCOUNT ( NFAIL  )
      CALL T_CNUM   ( NCASE  )
      CALL T_SUCCESS( OK     )



      IF ( OK ) THEN

         MESSGE = 'Passed  --- Test Family: '
         CALL SUFFIX ( NAME, 1, MESSGE )

         CALL TSTLOG ( MESSGE, .FALSE.        )

         CALL TSTLGS ( 'LEFT 9 RIGHT 78 FLAG --- NEWLINE /cr', &
                       'LEFT 9 RIGHT 78 FLAG --- NEWLINE /cr' )


         MESSGE = 'Successful tests for # of # test ' &
         //       'cases. '
         CALL REPMCT ( MESSGE, '#',   NCASE, 'L', MESSGE )
         CALL REPMCT ( MESSGE, '#',   NCASE, 'L', MESSGE )
         CALL TSTLOG ( MESSGE, .FALSE.              )


      ELSE

         MESSGE = 'FAILURE for Test Family: '
         CALL SUFFIX ( NAME, 1, MESSGE )

         CALL TSTLOG ( MESSGE, .FALSE.        )

         CALL TSTLGS ( 'LEFT 24 RIGHT 78 FLAG : NEWLINE /cr', &
                       'LEFT 24 RIGHT 78 FLAG : NEWLINE /cr' )
         MESSGE = '# of # test cases failed. '
         CALL REPMCT ( MESSGE, '#',   NFAIL, 'C', MESSGE )
         CALL REPMCT ( MESSGE, '#',   NCASE, 'L', MESSGE )
         CALL TSTLOG ( MESSGE, .FALSE.              )

      END IF

!
!     Get current time. We will print it at the very end.
!
      CALL CURTIM ( TIME2 )


      CALL TSTLGS ( GOOD, BAD )
!
!     Now we check on the overall success of the testing.
!
      CALL T_ANYBAD ( FAILUR )

      IF ( .NOT. FAILUR ) THEN

         CALL TSTGLF ( LOGFIL )
         CALL TSTFIL ( 'pass{0-9}{0-9}{0-9}{0-9}.log', 'SAVE', FILE )
         CALL TSTSLF ( LOGFIL )

         CALL TSTGET (  ENV, VERSN, TIME )
!
!        We temporarily turn off writing to the log file and screen
!        so that we can put the test stamp in the pass file.
!
         CALL TSTIOA ( 'SAVE'   )
         CALL TSTIOH ( 'LOG'    )
         CALL TSTIOH ( 'SCREEN' )

         CALL TSTWLN ( ENV   )
         CALL TSTWLN ( VERSN )
         CALL TSTWLN ( TIME  )

!
!        Now reactivate the screen and log file.
!
         CALL TSTIOA ( 'LOG'    )
         CALL TSTIOA ( 'SCREEN' )
!
!        Write out the testing summary.
!
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
!
!        Turn off writing to the log file,  the next message doesn't
!        belong in it.
!
         CALL TSTGLF ( MESSGE )
         CALL PREFIX ( 'For details, see the test log:', 1, MESSGE )

         CALL TSTIOH ( 'LOG' )
         CALL TSTWLN ( ' '   )
         CALL TSTLOG ( MESSGE, .FALSE.)
         CALL TSTWLN ( ' '   )

         CALL TSTLOG ( TIME2, .FALSE.)
         CALL TSTWLN ( ' '   )

!
!        Close the pass file.
!
         CALL TSTIOC ( 'SAVE' )
!
!        Reactivate writing to the log file.
!
         CALL TSTIOA ( 'LOG'    )
         CALL TSTIOA ( 'SCREEN' )

      END IF

!
!     Clean up any files that might be lying around from the
!     last test family.
!
      CALL CSTART

!
!     Finish up everything now.
      CALL TSTLOG ( ' ',   .FALSE. )
      CALL TSTLOG ( TIME2, .FALSE. )
      CALL TSTLOG ( ' ',   .FALSE. )

      CALL FINISH ()

      RETURN
      END
