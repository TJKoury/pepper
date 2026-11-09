!$Procedure      CHCKSC ( Check scalar character string )
!
      SUBROUTINE CHCKSC ( NAME, VAL, COMP, EXP,  OK )
!
! $ Abstract
!
!     Check an string scalar value against some expected value.
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
      CHARACTER*(*)         VAL
      CHARACTER*(*)         EXP
      CHARACTER*(*)         COMP
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
!      OK         O   TRUE if the test passes, FALSE otherwise..
!
! $ Detailed_Input
!
!     NAME        is the string used to give the name of a variable
!
!     VAL         is the actual character variable to be examined
!
!     EXP         an expected value or bound on the value VAL.
!
!                    =    ---   check for strict equality
!                    >    ---   check for VAL >  EXP
!                    <    ---   check for VAL <  EXP
!                    >=   ---   check for VAL >= EXP ( LGE (VAL, EXP) )
!                    <=   ---   check for VAL <= EXP ( LLE (VAL, EXP) )
!                    !=   ---   check for VAL != EXP ( VAL .NE. EXP   )
!
!                    The following are the case insensitive versions
!                    of the previous checks.
!
!                    ~=    ---   check for strict equality
!                    ~>    ---   check for VAL >  EXP
!                    ~<    ---   check for VAL <  EXP
!                    ~>=   ---   check for VAL >= EXP
!                    ~<=   ---   check for VAL <= EXP
!                    ~!=   ---   check for VAL != EXP
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
!     scalar string values.
!
! $ Examples
!
!     Suppose that you have just made a call to a subroutine that
!     you wish to test (call the routine SPUD) and you would like
!     to test an output string against an expected value.  Using
!     this routine you can automatically have the test result logged
!     in via the testing utilities.
!
!        CALL SPUD   (  INPUT,   OUTPUT )
!        CALL CHCKSC ( 'OUTPUT', OUTPUT, '=', EXPECT, OK )
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
!-    Testing Utilities Version 1.0.1, 10-JAN-2006 (BVS)
!
!        Removed erroneous cut-n-paste lines from Brief_I/O section of
!        the header.
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
      INTEGER               RTRIM

!
!     Local Variables
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      CHARACTER*(LNSIZE)    MESSGE
      LOGICAL               FAIL

      INTEGER               LVAL
      INTEGER               LEXP

      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 4000 )

      CHARACTER*(MAXLEN)    MYVAL
      CHARACTER*(MAXLEN)    MYEXP
      CHARACTER*(4)         MYCOMP

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
               'been displayed. It indicates a CHCKSC bug.'


      LVAL  = LEN ( VAL )
      LEXP  = LEN ( EXP )
      MYVAL = VAL
      MYEXP = EXP


      IF ( COMP(1:1) .EQ. '~' ) THEN
         MYCOMP = COMP(2:)
         CALL UCASE( MYVAL, MYVAL )
         CALL UCASE( MYEXP, MYEXP )
      ELSE
         MYCOMP = COMP
      END IF


      FAIL = .FALSE.

      IF      ( MYCOMP .EQ. '='  ) THEN

         FAIL = MYVAL(1:LVAL) .NE. MYEXP(1:LEXP)

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not the value expected./cr' &
         //          'The value was:/cr(3:) ''#'' /cr(-3:) ' &
         //          'the expected value was:/cr(3:) ''#''. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', MYVAL(1:LVAL),       MESSGE )
            CALL REPMC ( MESSGE, '#', MYEXP(1:LEXP),       MESSGE )
         END IF


      ELSE IF ( MYCOMP .EQ. '<'  ) THEN

         FAIL   =  LGE( MYVAL(1:LVAL), MYEXP(1:LEXP) )

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not less than ''#'' as was ' &
         //          'expected. /cr The value was: ''#''. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', MYEXP(1:LEXP),       MESSGE )
            CALL REPMC ( MESSGE, '#', MYVAL(1:LVAL),       MESSGE )
         END IF

      ELSE IF ( MYCOMP .EQ. '>'  ) THEN

         FAIL   =  LLE( MYVAL(1:LVAL), MYEXP(1:LEXP) )

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not greater than ''#'' as ' &
         //          'was expected. /cr The value was: ''#''. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', MYEXP(1:LEXP),       MESSGE )
            CALL REPMC ( MESSGE, '#', MYVAL(1:LVAL),       MESSGE )
         END IF

      ELSE IF ( MYCOMP .EQ. '>=' ) THEN

         FAIL   =  LLT( MYVAL(1:LVAL), MYEXP(1:LEXP) )

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not greater than ''#'' as ' &
         //          'was expected. /cr The value was: ''#''. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', MYEXP(1:LEXP),       MESSGE )
            CALL REPMC ( MESSGE, '#', MYVAL(1:LVAL),       MESSGE )
         END IF

      ELSE IF ( MYCOMP .EQ. '<=' ) THEN

         FAIL   =  LGT( MYVAL(1:LVAL), MYEXP(1:LEXP) )

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not less than ''#'' as was ' &
         //          'expected. /cr The value was: ''#''. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', MYEXP(1:LEXP),       MESSGE )
            CALL REPMC ( MESSGE, '#', MYVAL(1:LVAL),       MESSGE )
         END IF

      ELSE IF ( MYCOMP .EQ. '!=' ) THEN

         FAIL   = MYVAL(1:LVAL) .EQ. MYEXP(1:LEXP)

         IF ( FAIL ) THEN
            MESSGE = 'The value of # should have been different ' &
         //          'from ''#''./cr It wasn''t. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', MYEXP(1:LEXP),       MESSGE )
         END IF

      ELSE

         FAIL = .TRUE.

         MESSGE = 'The comparison # is not recognized. '
         CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )

      END IF

      IF ( .NOT. FAIL ) THEN

         IF ( VERBOS() ) THEN

            MESSGE = 'The comparison/cr/cr(3:) # # ''#'' /cr/cr(-3:)' &
         //          'was satisfied./cr' &
         //          'The value of # was ''#''. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )
            CALL REPMC ( MESSGE, '#', MYEXP(1:LEXP),       MESSGE )
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', MYVAL(1:LVAL),       MESSGE )

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






