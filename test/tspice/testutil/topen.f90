!$Procedure      TOPEN (Open a family of tests)

      SUBROUTINE TOPEN ( NAME )

! $ Abstract
!
!     Open a collection of tests
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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
      CHARACTER*(*)         NAME

! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      NAME       I   The name of a family of tests
!
! $ Detailed_Input
!
!     NAME        is the name of some collection of tests that are
!                 to be performed.  Often this is simply the name
!                 of a subroutine that is to be tested.
!
!                 NAME should be no more than 32 characters in length.
!
!                 Longer names will be truncacted to 32 characters by
!                 the testing utilities.
!
! $ Detailed_Output
!
!      None.
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
!     This routine establishes a new test family.   It acts by
!     side effect setting up the various test utilities that need
!     to be initialized before beginning a series of tests and logs
!     that this task has been accomplished.
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
!-    Testing Utilities 5.0.0, 30-MAY-2010 (NJB)
!
!        Increased length of test family names from 32 to
!        MAXFNM.
!
!-    Testing Utilities 4.0.0, 20-JUN-2001 (WLT)
!
!        Added code to remove any messages left lying around from a
!        previous test case or family that were created with TSTMSG.
!
!-    Testing Utilities 3.0.0, 27-JUL-1999 (WLT)
!
!        Added code so that if Error Handling status was not
!        checked in the previous test case, it will be done
!        here.  In addition the Test Program is reset to a
!        "clean" SPICE status.  All files are unloaded and
!        test files are deleted.
!
!-    Testing Utilities 2.0.0, 20-JUN-1997 (WLT)
!
!        Added the tests to make sure that the current SPICE
!        trace is the same at the end of a test case as it was
!        at the beginning of a test case.  This identifies routines
!        that checkin but don't check out.
!
!-    Testing Utilities Version 1.0.0, 3-NOV-1994 (WLT)
!
!
!-&
!
!     Spicelib Functions
!
      INTEGER               RTRIM
      LOGICAL               ISOFF
      LOGICAL               SETOFF

!
!     Local Variables and Parameters
!
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
!
!        See if exceptions were examined in the last test case.
!
         IF ( ISOFF ( 'CHCKXC' ) ) THEN
            CALL CHCKXC ( .FALSE., ' ',  OK )
            DUMMY = SETOFF ( 'CHCKXC' )
         END IF

      END IF
!
!     Turn off any messages that may have been left lying around from
!     the last call to TSTMSG.
!
      CALL TSTMOF

!
!     Append this name to the list of names we maintain for
!     summary purposes.
!
      IF ( NTEST .LT. MAXTST ) THEN
         NTEST           = NTEST + 1
         TNAMES( NTEST ) = NAME
      END IF

!
!     Get the current value of the trace.
!
      CTRACE = ' '
      CALL QCKTRC ( CTRACE )
!
!     Get the last stored value of the trace.
!
      CALL T_TRACE ( 'GET', OTRACE )
!
!     Reset the error handling
!
      CALL RESET  ()
      CALL ERRDEV ( 'SET', 'NULL'   )
!
!     Reset the kernel managers to a "clean" state.
!
      CALL CSTART


!
!     Log the last test case if it passed.
!
      IF ( NTEST .GT. 1 ) THEN
!
!        First we check the current trace and make sure it is the
!        same as the old trace.
!
         CALL CHCKSC ( 'Current Trace', CTRACE, '=', OTRACE,  OK )

         CALL TSTLIP ()
         CALL TSTRUL ()
         CALL TSTRUL ()
         CALL TSTCBL ()
      END IF
!
!     Set the "old" trace value.
!
      CALL T_TRACE ( 'SET', CTRACE )

      CALL T_NAME  ( OLDNAM )

      IF ( OLDNAM .NE. ' ' ) THEN
!
!        We need to close out the old case
!
         CALL T_FCOUNT ( NFAIL )
         CALL T_CNUM   ( NCASE )
         CALL T_SUCCESS( OK    )

         CALL TSTSTY ( GOOD, BAD )

         IF ( OK ) THEN

            MESSGE = 'Passed  --- Test Family: '
            CALL SUFFIX ( OLDNAM, 1, MESSGE )

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
            CALL SUFFIX ( OLDNAM, 1, MESSGE )

            CALL TSTLOG ( MESSGE, .FALSE.        )

            CALL TSTLGS ( 'LEFT 24 RIGHT 78 FLAG : NEWLINE /cr', &
                          'LEFT 24 RIGHT 78 FLAG : NEWLINE /cr' )
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


!
!     The following two entry points are provided so that a program
!     can fetch in order all tests that were begun with a call to TOPEN.
!
!     The entry TBEGF sets the test family counter to 1.  The entry
!     TFNAME fetches the next name from the list of test families and
!     increments the test family counter.  When the family counter
!     passes the number of calls to TOPEN, the name returned is set to
!     a blank.
!

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
