!$Procedure      TSTRUL (Test Rules)
!
      SUBROUTINE TSTRUL
!
! $ Abstract
!
!     This routine creates the rules that are used to delimit test
!     cases when testing in verbose mode.
!
! $ Required_Reading
!
!      None.
!
! $ Keywords
!
!       Testing
!
! $ Declarations
!
      IMPLICIT NONE

!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
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
! $ Files
!
!      This routine calls TSTLOG to write to write to the screen
!      and test log file.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This is a utility routine for writing the rules that are used
!     to mark the boundaries of test cases when a test program is
!     running in verbose mode.
!
!     The code is isolated here so that any part of the test
!     utiltities that may need to create the rules can do so.
!
!     Also this allows NAIF to easily change the appearence of these
!     rules if it is deemed necessary without having to search the
!     entire set of test utilities to find out where they might be
!     present.
!
! $ Examples
!
!     See TOPEN or TCLOSE.
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
!-    Testing Utilities 1.0.0, 5-DEC-1994 (WLT)
!
!
!-&
!
! $ Index_Entries
!
!     Draw the rules that delimit test cases in verbose mode.
!
!-&

      LOGICAL               VERBOS


      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 80 )

      CHARACTER*(LNSIZE)    MESSGE


      IF ( VERBOS() ) THEN

         MESSGE = '-------------------------------------------' &
         //       '---------------------------- '

         CALL TSTLOG   ( MESSGE,            .FALSE. )


      END IF

      RETURN
      END






