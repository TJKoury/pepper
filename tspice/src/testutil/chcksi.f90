!$Procedure      CHCKSI ( Check Scalar Integer )
!
      SUBROUTINE CHCKSI ( NAME, VAL, COMP, EXP, TOL, OK )
      use pepper_legacy
!
! $ Abstract
!
!     Check an integer scalar value against some expected value.
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
      INTEGER               VAL
      CHARACTER*(*)         COMP
      INTEGER               EXP
      INTEGER               TOL
      LOGICAL               OK

!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      NAME       I   the name of the variable to be examined.
!      VAL        I   the actual variable
!      EXP        I   the comparison value for the variable
!      COMP       I   the kind of comparison to perform.
!      TOL        I   the tolerance allowed in comparing.
!      OK         O   TRUE if the test passes, FALSE otherwise..
!
! $ Detailed_Input
!
!     NAME        is the string used to give the name of a variable
!
!     VAL         is the actual integer variable to be examined
!
!     EXP         an expected value or bound on the value VAL.
!
!                    =    ---   check for strict equality
!                    >    ---   check for VAL >  EXP
!                    <    ---   check for VAL <  EXP
!                    >=   ---   check for VAL >= EXP ( VAL .GE. EXP )
!                    <=   ---   check for VAL <= EXP ( VAL .LE. EXP )
!                    !=   ---   check for VAL != EXP ( VAL .NE. EXP )
!                    ~    ---   check for VAL ~  EXP ( VAL within TOL
!                               of EXP)
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
!
! $ Particulars
!
!     This routine handles a wide variety of comparisons between
!     scalar integer values.
!
! $ Examples
!
!     Suppose that you have just made a call to a subroutine that
!     you wish to test (call the routine SPUD) and you would like
!     to test an output integer against an expected value.  Using
!     this routine you can automatically have the test result logged
!     in via the testing utilities.
!
!        CALL SPUD   (  INPUT,   OUTPUT )
!        CALL CHCKSI ( 'OUTPUT', OUTPUT, '=', EXPECT, 0, OK )
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
!-    Testing Utilities Version 1.0.0, 8-Nov-1994 (WLT)
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
               'been displayed. It indicates a CHCKSI bug.'

      FAIL = .FALSE.

      IF      ( COMP .EQ. '='  ) THEN

         FAIL = VAL .NE. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not the value expected. ' &
         //         'The value was: # the expected value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '<'  ) THEN

         FAIL   =  VAL .GE. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not less than # as was ' &
         //          'expected.  The value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '>'  ) THEN

         FAIL   =  VAL .LE. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not greater than # as was ' &
         //          'expected.  The value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '>=' ) THEN

         FAIL   =  VAL .LT. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not greater than # as was ' &
         //          'expected.  The value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '<=' ) THEN

         FAIL   =  VAL .GT. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not less than # as was ' &
         //          'expected.  The value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '!=' ) THEN

         FAIL   = VAL .EQ. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # should have been different ' &
         //          'from #. It wasn''t. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '~'  ) THEN

         FAIL   = TOL .LT. ABS( VAL - EXP )

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not within # of #.  The ' &
         //          'value was # . The difference between the actual ' &
         //          'and expected value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', TOL,                 MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
            CALL REPMI ( MESSGE, '#', VAL - EXP,           MESSGE )
         END IF

      ELSE

         FAIL = .TRUE.

         MESSGE = 'The comparison # is not recognized. '
         CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )

      END IF

      IF ( .NOT. FAIL ) THEN

         IF ( VERBOS() ) THEN

            MESSGE = 'The comparison ''# # #'' was satisfied. ' &
         //          'The value of # was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )

            CALL TSTLOG ( ' ',    FAIL )
            CALL TSTLOG ( MESSGE, FAIL )

         END IF

      ELSE

         CALL TSTLOG ( ' ',    FAIL )
         CALL TSTLOG ( MESSGE, FAIL )

      END IF

      CALL TSTLGS ( GOOD, BAD )
      OK = .NOT. FAIL

      RETURN
      END


