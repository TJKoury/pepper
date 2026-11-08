!$Procedure      TCASE (Test Case)

      SUBROUTINE TCASE ( TITLE )

! $ Abstract
!
!     Set the title for the next test case and log the success of
!     the last test case if it passed and logging of individual
!     test case success is enabled.
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
