!$Procedure      SHOERR ( Show Errors )

      SUBROUTINE SHOERR

! $ Abstract
!
!     This routine sets the error handling within a test case so
!     that error messages are sent to the screen.  When the
!     test case ends, messages are once again sent to 'NULL'
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     TESTING
!
! $ Declarations

      IMPLICIT NONE


! $ Brief_I/O
!
!     None.
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
!     None.
!
! $ Files
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This routine allows you to cause error messages issued through
!     the SPICE exception handling system to be sent to the screen
!     instead of being blocked from output by the Test Utilities
!     library.
!
!     However, this effect lasts only for the duration of a single
!     test case.  Once a new test case or family is specified
!     by a call to TCASE or TOPEN, error messages will once again
!     be prohibited from being displayed on the terminal window.
!
!     This routine is primarily useful in the debugging stages of
!     a test case or test family.
!
! $ Examples
!
!     Suppose that you are creating a test case within a family of
!     test cases and wish to have all text associated with an
!     exception that may arise within the scope of a test case
!     to be sent to standard output.  Simply set up your
!     test case in this way.
!
!        CALL TCASE ( 'Name of the test case.' )
!        CALL SHOERR
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!
! $ Literature_References
!
!     None.
!
! $ Version
!
!-    Test Utilities Version 1.0.0, 27-JUL-1999 (WLT)
!
!
!-&

      CALL ERRDEV ( 'SET', 'SCREEN' )
      RETURN

      END
