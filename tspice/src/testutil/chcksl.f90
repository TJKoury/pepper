!$Procedure      CHCKSL ( Check Scalar logical )
!
      SUBROUTINE CHCKSL ( NAME, VAL, EXP,  OK )
      use pepper_legacy
!
! $ Abstract
!
!     Check a logical value against an expected value.
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
      CHARACTER*(*)         NAME
      LOGICAL               VAL
      LOGICAL               EXP
      LOGICAL               OK

!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      NAME       I   the name of the variable to be examined.
!      VAL        I   the actual variable
!      EXP        I   the comparison value for the variable
!      OK         O   TRUE if the test passes, FALSE otherwise..
!
! $ Detailed_Input
!
!     NAME        is the string used to give the name of a variable
!
!     VAL         is the actual logical variable to be examined
!
!     EXP         an expected value or bound on the value VAL.
!
!
! $ Detailed_Output
!
!     OK         if the check of the input variable is successful then
!                OK is given the value TRUE.  Otherwise OK is given the
!                value FALSE and a diagnostic message is sent to the
!                test logger.
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
!      The result of a failure is automatically logged in the testing
!      log file and failure file.  Success is logged only if verbose
!      testing has been enabled.
!
! $ Particulars
!
!     This routine handles a wide variety of comparisons between
!     scalar double precision values.
!
! $ Examples
!
!     Suppose that you have just made a call to a subroutine that
!     you wish to test (call the routine SPUD) and you would like
!     to test an output logical against an expected value and
!     verify that they are the same. Using
!     this routine you can automatically have the test result logged
!     in via the testing utilities.
!
!        CALL SPUD   (  INPUT,   OUTPUT )
!        CALL CHCKSD ( 'OUTPUT', OUTPUT, EXPECT, OK )
!
! $ Restrictions
!
!     None.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber      (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    Testing Utility Version 1.1.0, 05-AUG-2013 (BVS)
!
!        Tweaked logic to do only necessary output message construction
!        operations to speed-up execution.
!
!-    Testing Utility Version 1.0.0, 7-NOV-1994 (WLT)
!
!
!-&
!
!     Test Utility functions
!
      LOGICAL               VERBOS


!
!     SPICELIB functions
!

!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      CHARACTER*(LNSIZE)    MESSGE
      LOGICAL               FAIL

      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )

      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD

      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ', &
                    'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )

!
!     Set placeholder message.
!
      MESSGE = 'This is a placeholder message that should have never '// &
               'been displayed. It indicates a CHCKSL bug.'

!
!     Just make sure that the values of VAL and EXP are the same.
!
      IF      ( EXP ) THEN

         OK = VAL

         IF ( OK ) THEN
            IF ( VERBOS() ) THEN
               MESSGE = 'The value of # was TRUE as expected. '
            END IF
         ELSE
            MESSGE = 'The expected value of # was TRUE.  The ' &
            //       'actual value was FALSE'
         END IF

      ELSE

         OK = .NOT. VAL

         IF ( OK ) THEN
            IF ( VERBOS() ) THEN
               MESSGE = 'The value of # was FALSE as expected. '
            END IF
         ELSE
            MESSGE = 'The expected value of # was FALSE.  The ' &
            //       'actual value was TRUE'
         END IF

      END IF

      FAIL = .NOT. OK

      IF ( FAIL .OR. VERBOS() ) THEN
         CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
      END IF

      IF ( OK ) THEN

         IF ( VERBOS() ) THEN

            CALL TSTLOG ( ' ',    FAIL )
            CALL TSTLOG ( MESSGE, FAIL )

         END IF

      ELSE

         CALL TSTLOG ( ' ',    FAIL )
         CALL TSTLOG ( MESSGE, FAIL )

      END IF

      CALL TSTLGS ( GOOD, BAD )

      RETURN
      END



