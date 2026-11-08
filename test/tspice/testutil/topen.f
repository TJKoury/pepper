C$Procedure      TOPEN (Open a family of tests)
 
      SUBROUTINE TOPEN ( NAME )
 
C$ Abstract
C
C     Open a collection of tests
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
      CHARACTER*(*)         NAME
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NAME       I   The name of a family of tests
C
C$ Detailed_Input
C
C     NAME        is the name of some collection of tests that are
C                 to be performed.  Often this is simply the name
C                 of a subroutine that is to be tested.
C
C                 NAME should be no more than 32 characters in length.
C
C                 Longer names will be truncacted to 32 characters by
C                 the testing utilities.
C
C$ Detailed_Output
C
C      None.
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
C     This routine establishes a new test family.   It acts by
C     side effect setting up the various test utilities that need
C     to be initialized before beginning a series of tests and logs
C     that this task has been accomplished.
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
C-    Testing Utilities 5.0.0, 30-MAY-2010 (NJB)
C
C        Increased length of test family names from 32 to
C        MAXFNM.
C
C-    Testing Utilities 4.0.0, 20-JUN-2001 (WLT)
C
C        Added code to remove any messages left lying around from a
C        previous test case or family that were created with TSTMSG.
C
C-    Testing Utilities 3.0.0, 27-JUL-1999 (WLT)
C
C        Added code so that if Error Handling status was not
C        checked in the previous test case, it will be done
C        here.  In addition the Test Program is reset to a
C        "clean" SPICE status.  All files are unloaded and
C        test files are deleted.
C
C-    Testing Utilities 2.0.0, 20-JUN-1997 (WLT)
C
C        Added the tests to make sure that the current SPICE
C        trace is the same at the end of a test case as it was
C        at the beginning of a test case.  This identifies routines
C        that checkin but don't check out.
C
C-    Testing Utilities Version 1.0.0, 3-NOV-1994 (WLT)
C
C
C-&
C
C     Spicelib Functions
C
      INTEGER               RTRIM
      LOGICAL               ISOFF
      LOGICAL               SETOFF
 
C
C     Local Variables and Parameters
C
      INTEGER               MAXFNM
      PARAMETER           ( MAXFNM = 80 )
 
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 160 )
 
      INTEGER               MAXTST
      PARAMETER           ( MAXTST = 1000 )
 
      INTEGER               LNGSTR
      PARAMETER           ( LNGSTR = 1200 )
 
      CHARACTER*(MAXFNM)    OLDNAM
      CHARACTER*(LNSIZE)    MESSGE
      CHARACTER*(MAXFNM)    TNAMES ( MAXTST )
      CHARACTER*(LNGSTR)    CTRACE
      CHARACTER*(LNGSTR)    OTRACE
      INTEGER               NFAIL
      INTEGER               NCASE
      INTEGER               NTEST
      INTEGER               R
      INTEGER               TFAMC
      INTEGER               I
 
 
      LOGICAL               FIRST
      LOGICAL               OK
      LOGICAL               DUMMY
 
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )
 
      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD
 
 
      SAVE
 
      DATA                  TFAMC  /  1     /
      DATA                  NTEST  /  0     /
      DATA                  OLDNAM / ' '    /
      DATA                  FIRST  / .TRUE. /
 
 
 
      IF ( FIRST ) THEN
 
         FIRST = .FALSE.
 
         DO I = 1, MAXTST
            TNAMES(I) = ' '
         END DO
 
      ELSE
C
C        See if exceptions were examined in the last test case.
C
         IF ( ISOFF ( 'CHCKXC' ) ) THEN
            CALL CHCKXC ( .FALSE., ' ',  OK )
            DUMMY = SETOFF ( 'CHCKXC' )
         END IF
 
      END IF
C
C     Turn off any messages that may have been left lying around from
C     the last call to TSTMSG.
C
      CALL TSTMOF
 
C
C     Append this name to the list of names we maintain for
C     summary purposes.
C
      IF ( NTEST .LT. MAXTST ) THEN
         NTEST           = NTEST + 1
         TNAMES( NTEST ) = NAME
      END IF
 
C
C     Get the current value of the trace.
C
      CTRACE = ' '
      CALL QCKTRC ( CTRACE )
C
C     Get the last stored value of the trace.
C
      CALL T_TRACE ( 'GET', OTRACE )
C
C     Reset the error handling
C
      CALL RESET  ()
      CALL ERRDEV ( 'SET', 'NULL'   )
C
C     Reset the kernel managers to a "clean" state.
C
      CALL CSTART
 
 
C
C     Log the last test case if it passed.
C
      IF ( NTEST .GT. 1 ) THEN
C
C        First we check the current trace and make sure it is the
C        same as the old trace.
C
         CALL CHCKSC ( 'Current Trace', CTRACE, '=', OTRACE,  OK )
 
         CALL TSTLIP ()
         CALL TSTRUL ()
         CALL TSTRUL ()
         CALL TSTCBL ()
      END IF
C
C     Set the "old" trace value.
C
      CALL T_TRACE ( 'SET', CTRACE )
 
      CALL T_NAME  ( OLDNAM )
 
      IF ( OLDNAM .NE. ' ' ) THEN
C
C        We need to close out the old case
C
         CALL T_FCOUNT ( NFAIL )
         CALL T_CNUM   ( NCASE )
         CALL T_SUCCESS( OK    )
 
         CALL TSTSTY ( GOOD, BAD )
 
         IF ( OK ) THEN
 
            MESSGE = 'Passed  --- Test Family: '
            CALL SUFFIX ( OLDNAM, 1, MESSGE )
 
            CALL TSTLOG ( MESSGE, .FALSE.        )
 
            CALL TSTLGS ( 'LEFT 9 RIGHT 78 FLAG --- NEWLINE /cr',
     .                    'LEFT 9 RIGHT 78 FLAG --- NEWLINE /cr' )
 
 
            MESSGE = 'Successful tests for # of # test '
     .      //       'cases. '
            CALL REPMCT ( MESSGE, '#',   NCASE, 'L', MESSGE )
            CALL REPMCT ( MESSGE, '#',   NCASE, 'L', MESSGE )
            CALL TSTLOG ( MESSGE, .FALSE.              )
 
 
         ELSE
 
            MESSGE = 'FAILURE for Test Family: '
            CALL SUFFIX ( OLDNAM, 1, MESSGE )
 
            CALL TSTLOG ( MESSGE, .FALSE.        )
 
            CALL TSTLGS ( 'LEFT 24 RIGHT 78 FLAG : NEWLINE /cr',
     .                    'LEFT 24 RIGHT 78 FLAG : NEWLINE /cr' )
            MESSGE = '# of # test cases failed. '
            CALL REPMCT ( MESSGE, '#',   NFAIL, 'C', MESSGE )
            CALL REPMCT ( MESSGE, '#',   NCASE, 'L', MESSGE )
            CALL TSTLOG ( MESSGE, .FALSE.              )
 
         END IF
 
         CALL TSTLGS ( GOOD, BAD )
         CALL TSTLOG ( ' ', .FALSE. )
 
      END IF
 
      CALL T_BEGIN ( NAME )
 
      MESSGE = 'Testing --- Test Family: ' // NAME
      R      =  RTRIM(MESSGE)
 
      CALL TSTLOG ( ' ',         .FALSE. )
      CALL TSTLOG ( MESSGE(1:R), .FALSE. )
      CALL TSTLOG ( ' ',         .FALSE. )
 
      RETURN
 
 
C
C     The following two entry points are provided so that a program
C     can fetch in order all tests that were begun with a call to TOPEN.
C
C     The entry TBEGF sets the test family counter to 1.  The entry
C     TFNAME fetches the next name from the list of test families and
C     increments the test family counter.  When the family counter
C     passes the number of calls to TOPEN, the name returned is set to
C     a blank.
C
 
      ENTRY TBEGF ()
 
         TFAMC = 1
         RETURN
 
      ENTRY TFNAME ( NAME )
 
         IF ( TFAMC .LE. NTEST ) THEN
 
            NAME  = TNAMES ( TFAMC )
            TFAMC = TFAMC + 1
         ELSE
 
            NAME = ' '
 
         END IF
         RETURN
 
 
      END
