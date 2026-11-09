!$Procedure      CHCKSD ( Check Scalar d.p. )
!
      SUBROUTINE CHCKSD ( NAME, VAL, COMP, EXP, TOL, OK )
!
! $ Abstract
!
!     Check an d.p. scalar value against some expected value.
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
      DOUBLE PRECISION      VAL
      CHARACTER*(*)         COMP
      DOUBLE PRECISION      EXP
      DOUBLE PRECISION      TOL
      LOGICAL               OK

!
! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      NAME       I   the name of the variable to be examined.
!      VAL        I   the actual variable
!      COMP       I   the kind of comparison to perform.
!      EXP        I   the comparison value for the variable
!      TOL        I   the tolerance allowed in comparing.
!      OK         O   TRUE if the test passes, FALSE otherwise..
!
! $ Detailed_Input
!
!     NAME        is the string used to give the name of a variable
!
!     VAL         is the actual d.p. variable to be examined
!
!     EXP         an expected value or bound on the value VAL.
!
!     COMP        a string giving the kind of comparison to perform:
!
!                    =    ---   check for strict equality
!                    >    ---   check for VAL >  EXP
!                    <    ---   check for VAL <  EXP
!                    >=   ---   check for VAL >= EXP ( VAL .GE. EXP )
!                    <=   ---   check for VAL <= EXP ( VAL .LE. EXP )
!                    !=   ---   check for VAL != EXP ( VAL .NE. EXP )
!                    ~    ---   check for VAL ~  EXP ( VAL within TOL
!                               of EXP)
!                    ~/   ---   check for VAL ~/ EXP ( Relative
!                               difference between VAL and EXP .LE. TOL)
!
!     TOL        is a "tolerance" to use when checking for VAL to
!                be nearly the same as EXP.
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
!     to test an output d.p. against an expected value and verify that
!     the relative difference is less than some value.  Using
!     this routine you can automatically have the test result logged
!     in via the testing utilities.
!
!        CALL SPUD   (  INPUT,   OUTPUT )
!        CALL CHCKSD ( 'OUTPUT', OUTPUT, '~/', EXPECT, 1.0D-12, OK )
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
!     N.J. Bachman    (JPL)
!     W.L. Taber      (JPL)
!     B.V. Semenov    (JPL)
!
! $ Version
!
!-    Testing Utility Version 2.0.0, 22-APR-2014 (NJB) (BVS)
!
!        Added checks for expected or actual values equal to NaN.
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
      INTEGER               RTRIM

!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      CHARACTER*(LNSIZE)    MESSGE
      DOUBLE PRECISION      REL
      LOGICAL               FAIL

      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )

      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD

      LOGICAL               VALNAN
      LOGICAL               EXPNAN




      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ', &
                    'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )

!
!     Set placeholder message.
!
      MESSGE = 'This is a placeholder message that should have never '// &
               'been displayed. It indicates a CHCKSD bug.'


      FAIL = .FALSE.

!
!     Test both actual and expected values to see whether they are NaN.
!
      VALNAN =        ( .NOT. ( VAL .GT. 1.D0 ) ) &
                .AND. ( .NOT. ( VAL .LT. 1.D0 ) ) &
                .AND. ( .NOT. ( VAL .EQ. 1.D0 ) )

      EXPNAN =        ( .NOT. ( EXP .GT. 1.D0 ) ) &
                .AND. ( .NOT. ( EXP .LT. 1.D0 ) ) &
                .AND. ( .NOT. ( EXP .EQ. 1.D0 ) )


      IF ( VALNAN .OR. EXPNAN ) THEN
!
!        At least one of the input values is NaN; the normal
!        comparisons won't tell us anything. We need to
!        construct a custom error message.
!
         FAIL = .TRUE.

         IF ( .NOT. VALNAN ) THEN
!
!           Only EXP is NaN.
!
            MESSGE = 'The value of # was #. The expected value was NaN.'
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )

         ELSE IF ( .NOT. EXPNAN ) THEN
!
!           Only VAL is NaN.
!
            MESSGE = 'The value of # was NaN. The expected value was #.'
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )

         ELSE

            MESSGE = 'The value of # and the expected value were NaN.'
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )

         END IF



      ELSE IF ( COMP .EQ. '=' ) THEN

         FAIL = VAL .NE. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not the value expected./cr ' &
         //          'The value was:         # /cr ' &
         //          'the expected value was #. /cr' &
         //          'The difference between these is: # .'
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL - EXP, 14,       MESSGE )
         END IF


      ELSE IF ( COMP .EQ. '<'  ) THEN

         FAIL   =  VAL .GE. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not less than # as was ' &
         //          'expected. /cr The value was #. /cr ' &
         //          'This is greater ' &
         //          'than # by #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL - EXP, 14,       MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '>'  ) THEN

         FAIL   =  VAL .LE. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not greater than # as was ' &
         //          'expected. /cr The value was #. /crThis is less ' &
         //          'than # by #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP - VAL, 14,       MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '>=' ) THEN

         FAIL   =  VAL .LT. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not greater than # as was ' &
         //          'expected. /cr The value was #./cr This is less ' &
         //          'than # by #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP - VAL, 14,       MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '<=' ) THEN

         FAIL   =  VAL .GT. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not less than # as was ' &
         //          'expected. /cr The value was #./cr This is ' &
         //          'greater ' &
         //          'than # by #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL - EXP, 14,       MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '!=' ) THEN

         FAIL   = VAL .EQ. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # should have been different ' &
         //          'from #./cr It wasn''t. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '~'  ) THEN

         FAIL   = TOL .LT. DABS( VAL - EXP )

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not within # of #. /cr The ' &
         //          'value was #./cr The difference between the ' &
         //          'actual ' &
         //          'and expected value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', TOL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL - EXP, 14,       MESSGE )
         END IF

      ELSE IF ( COMP .EQ. '~/' ) THEN

         IF ( VAL .EQ. EXP ) THEN
            REL = 0.0D0
         ELSE
            REL = DABS( VAL - EXP ) / MAX( DABS(VAL), DABS(EXP) )
         END IF

         FAIL = TOL .LT. REL

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was #.  The expected value was ' &
         //          '#. /cr The relative difference between # and' &
         //          'its ' &
         //          'expected value was #./cr The maximum relative ' &
         //          'difference allowed for a successful test is ' &
         //          '#. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', REL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', TOL, 14,             MESSGE )
         END IF

      ELSE

         FAIL = .TRUE.

         MESSGE = 'The comparison # is not recognized. '
         CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )

      END IF

      IF ( .NOT. FAIL ) THEN

         IF ( VERBOS() ) THEN

            MESSGE = 'The comparison/cr/cr(3:) ''# # #'' /cr/cr(-3:) ' &
         //          'was satisfied to the ' &
         //          'specified tolerance: #./crThe value of # was ' &
         //          '#. '

            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', TOL, 14,             MESSGE )
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )

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



