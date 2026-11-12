!$Procedure      TSTLIP ( Test log if passed )
!
      SUBROUTINE TSTLIP ()
      use pepper_legacy
!
! $ Abstract
!
!     If the last registered test case passed and test case logging
!     is enabled, log the success of the last test case to SCREEN
!     and the log file.
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
!     If the last test case passed and individual test case logging
!     is enabled.  Log the success of that case.
!
! $ Examples
!
!     Later.
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
!-    Version 2.0.0, 29-MAY-2010 (NJB)
!
!        Increase line size to 2000.
!
!-    Testing Utilities 1.0.0, 18-NOV-1994 (WLT)
!
!
!-&

! $ Index_Entries
!
!     Log a succussful test case.
!
!-&
!
!     Test Utility Functions
!
      LOGICAL               VERBOS
!
!     Local Variables.
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 2000 )

      CHARACTER*(LNSIZE)    MESSGE

      INTEGER               NUMBER

      LOGICAL               LOGIFP
      LOGICAL               PASSED

      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )

      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD
      CHARACTER*(STYSIZ)    PASS
      CHARACTER*(STYSIZ)    FAIL
      SAVE

      DATA                  LOGIFP / .FALSE. /

      PASS = 'LEFT 1 RIGHT 78 HARDSPACE $ FLAG Case$Passed:'
      FAIL = 'LEFT 1 RIGHT 78 HARDSPACE $ FLAG CASE$FAILED!'


      IF ( LOGIFP .OR. VERBOS() ) THEN

         MESSGE = ' '
!
!        Copy the current logging style.
!
         CALL TSTSTY ( GOOD, BAD )
!
!        See whether or not this case passed and get its number.
!
         CALL T_CPASS ( PASSED )
         CALL T_CNUM  ( NUMBER )

         IF ( NUMBER .GT. 0 .AND. PASSED ) THEN
!
!           Print a blank line before the output message only
!           if we are in verbose mode.
!
            IF ( VERBOS() ) THEN
               CALL TSTLOG   ( ' ', .FALSE. )
            END IF
!
!           Set the logging style to PASS and print the case name.
!
            CALL TSTLGS   ( PASS,    PASS   )
            CALL T_CNAME  ( MESSGE          )
            CALL TSTLOG   ( MESSGE, .FALSE. )

         ELSE IF ( NUMBER .GT. 0 ) THEN
!
!           Always print a blank line before a case that failed.
!
            CALL TSTLOG   ( ' ',    .FALSE. )
!
!           Set the logging style to FAIL and print the case name.
!
            CALL TSTLGS   ( FAIL,    FAIL   )
            CALL T_CNAME  ( MESSGE          )
            CALL TSTLOG   ( MESSGE, .FALSE. )

         END IF

         CALL TSTLGS ( GOOD, BAD )

      END IF
      RETURN


!
!     Write a blank line if we are in case logging mode or verbose
!     mode.
!
      ENTRY TSTCBL ()

         IF ( LOGIFP .OR. VERBOS() ) THEN
            CALL TSTLOG ( ' ', .FALSE. )
         END IF
         RETURN


!
!     To turn on the logging of individual test cases call TSTLCY
!
      ENTRY TSTLCY ()

         LOGIFP = .TRUE.
         RETURN

!
!     To turn on the logging of individual test cases call TSTLCY
!
      ENTRY TSTLCN ()

         LOGIFP = .FALSE.
         RETURN

      END






