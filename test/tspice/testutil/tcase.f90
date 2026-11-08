!$Procedure      TCASE (Test Case)

      SUBROUTINE TCASE ( TITLE )

! $ Abstract
!
!     Set the title for the next test case and log the success of
!     the last test case if it passed and logging of individual
!     test case success is enabled.
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
      CHARACTER*(*)         TITLE


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      TITLE      I   The title of this test case.
!
! $ Detailed_Input
!
!     TITLE       is the title of a test case.  It should have no
!                 more than 32 characters.  If it does characters
!                 beyond the thirty second character are ignored.
!
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Files
!
!     If test case success logging is enabled, the previous test
!     case success is logged to SCREEN and the log file.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This is the user interface routine for initializing test cases.
!
! $ Examples
!
!     Later.
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-    Testing Utilities 3.3.0, 19-JUN-2001 (WLT)
!
!        Added a call to TSTMOF to turn off the test message from the
!        last test case.
!
!-    Testing Utilities 3.2.0, 11-NOV-1999 (WLT)
!
!        Declared SETOFF to be a function and it is now called
!        as a function instead of as a subroutine.
!
!-    Testing Utilities 3.0.0, 27-JUL-1999 (WLT)
!
!        Added code so that if Error Handling status was not
!        checked in the previous test case, it will be done
!        here.
!
!-    Testing Utilities 2.0.0, 20-JUN-1997 (WLT)
!
!        Added the tests to make sure that the current SPICE
!        trace is the same at the end of a test case as it was
!        at the beginning of a test case.  This identifies routines
!        that checkin but don't check out.
!
!-    Testing Utilities 1.0.0, 18-NOV-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Initializing a test case.
!
!-&
!
!     Take care of resetting the error handling and logging the
!     last test case if it passed.
!
!
!     Test Utility Functions
!
      LOGICAL               VERBOS
      LOGICAL               ISOFF
      LOGICAL               SETOFF

!
!     Local Variables
!
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

!
!     See if exceptions were examined in the last test case.
!
      IF ( ISOFF ( 'CHCKXC' ) ) THEN
         CALL CHCKXC ( .FALSE., ' ',  OK )
         DUMMY = SETOFF ( 'CHCKXC' )
      END IF
!
!     Any message created with TSTMSG is no longer relevent.  Disable
!     any such message.
!
      CALL TSTMOF


!
!     Check the current traceback to make sure things that
!     checked in checked out in the last test case.
!
      CALL QCKTRC  ( CTRACE )
      CALL T_TRACE ( 'GET', OTRACE )

      CALL CHCKSC ( 'Current Trace', CTRACE, '=', OTRACE,  OK )

      CALL TSTLIP ()
      CALL T_CASE ( TITLE )
      CALL TSTRUL
      CALL TSTRUL

      IF ( VERBOS() ) THEN

!
!        Copy the current logging style.
!
         CALL TSTSTY ( GOOD, BAD )

         DOING  = 'LEFT 1 RIGHT 78 HARDSPACE $ FLAG Performing$:'
         MESSGE = '  '

         CALL TSTLGS   ( DOING,   DOING  )
         CALL T_CNAME  ( MESSGE          )
         CALL TSTLOG   ( MESSGE, .FALSE. )

!
!        Reset the logging style back to what it was.
!
         CALL TSTLGS   ( GOOD,    BAD    )

      END IF

      CALL T_TRACE ( 'SET', CTRACE )
      CALL RESET
      CALL ERRDEV  ( 'SET', 'NULL' )

      RETURN
      END
