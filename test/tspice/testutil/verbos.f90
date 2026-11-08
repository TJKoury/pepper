!$Procedure      VERBOS ( Display detailed information to the screen)
!
      LOGICAL FUNCTION VERBOS ()
!
! $ Abstract
!
!     This routine controls whether or not detailed information is
!     written to the screen and log file.
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
!
      IMPLICIT NONE

!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!       The function returns TRUE if details are to be displayed.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     The function returns TRUE if detailed information should be
!     written to the screen and log file.  This behavior is controlled
!     through the two entry points VERBON and VERBOFF.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      None.
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This routines controls the writing of detailed messages to
!     the screen and log file.  The default behavior is to return
!     the value of FALSE.  You may turn on verbose output by calling
!     the entry point VERBON
!
!        ONOFF = VERBON
!
!     To turn verbose messages off call the entry point VERBOFF
!
!        ONOFF = VERBOFF
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
!-    Testing Utilities Version 1.0.0, 21-NOV-1994 (WLT)
!
!
!-&
!
! $ Index_Entries
!
!     Determine the status of verbos output.
!
!-&
!     Entry points
!
      LOGICAL               VERBON
      LOGICAL               VERBOFF
!
!     Local Variables.
!
      LOGICAL               DISPLY
      SAVE

      DATA                  DISPLY / .FALSE. /


      VERBOS = DISPLY
      RETURN

      ENTRY VERBON

      DISPLY = .TRUE.
      VERBON = .TRUE.
      RETURN

      ENTRY VERBOFF

      DISPLY  = .FALSE.
      VERBOFF = .FALSE.
      RETURN

      END


