!$Procedure CHCKAC ( Check an array of characters )

      SUBROUTINE CHCKAC ( NAME, ARRAY, COMP, EXP, SIZE, OK )
      use pepper_legacy

! $ Abstract
!
!     Check the values in two character arrays.
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

      CHARACTER*(*)         NAME
      CHARACTER*(*)         ARRAY ( * )
      CHARACTER*(*)         COMP
      CHARACTER*(*)         EXP   ( * )
      INTEGER               SIZE
      LOGICAL               OK

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     NAME       I   the name of the array to be examined.
!     ARRAY      I   the actual array.
!     COMP       I   the kind of comparison to perform.
!     EXP        I   the comparison values for the array.
!     SIZE       I   the size of the input array
!     OK         O   .TRUE. if the test passes, .FALSE. otherwise.
!
! $ Detailed_Input
!
!     NAME     is the string used to give the name of an array.
!
!     ARRAY    is the actual array of strings to be examined.
!
!     COMP     is a string giving the kind of comparison to perform:
!
!                 =     --   check for strict equality
!                 >     --   check for VAL >  EXP
!                 <     --   check for VAL <  EXP
!                 >=    --   check for VAL >= EXP ( LGE (VAL, EXP) )
!                 <=    --   check for VAL <= EXP ( LLE (VAL, EXP) )
!                 !=    --   check for VAL != EXP ( VAL .NE. EXP   )
!
!              The following are the case insensitive versions
!              of the previous checks.
!
!                 ~=    --   check for strict equality
!                 ~>    --   check for VAL >  EXP
!                 ~<    --   check for VAL <  EXP
!                 ~>=   --   check for VAL >= EXP
!                 ~<=   --   check for VAL <= EXP
!                 ~!=   --   check for VAL != EXP
!
!     EXP      the array of expected values or bounds on values
!              in ARRAY.
!
! $ Detailed_Output
!
!     OK       if the check of the input array is successful then
!              OK is given the value .TRUE. Otherwise OK is given the
!              value .FALSE. and a diagnostic message is sent to the
!              test logger.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     This routine handles a wide variety of comparisons between
!     character string arrays.
!
! $ Examples
!
!     Suppose that you have just made a call to a subroutine that
!     you wish to test (call the routine SPUD) and you would like
!     to test an array of output strings matches your expectations.
!     Using this routine you can automatically have the test
!     result logged in via the testing utilities.
!
!        CALL SPUD   (  INPUT,   OUTPUT )
!        CALL CHCKAC ( 'OUTPUT', OUTPUT, '=', EXPECT, OK )
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
!     J. Diaz del Rio    (ODC Space)
!     F.S. Turner        (JPL)
!
! $ Version
!
!-    Test Utilities Version 1.0.1, 31-MAY-2021 (JDR)
!
!        Updated the header to comply with NAIF standard. Reordered
!        sections and added missing $Index_Entries.
!
!-    Test Utilities Version 1.0.0, 24-DEC-2001 (FST)
!
!-&


! $ Index_Entries
!
!     Check a two-dimensional array of characters
!
!-&


!
!     SPICELIB Functions
!

!
!     Testing Utility Functions
!
      LOGICAL               VERBOS

!
!     Local Parameters
!
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 4000 )

      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )

!
!     Local Variables
!
      CHARACTER*(LNSIZE)    MESSGE
      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD
      CHARACTER*(2)         MYCMP
      CHARACTER*(MAXLEN)    MYVAL
      CHARACTER*(MAXLEN)    MYEXP
      LOGICAL               FAIL
      LOGICAL               SN2CAS
      INTEGER               I
      INTEGER               LVAL
      INTEGER               LEXP

!
!     Setup style strings?
!
      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ', &
                    'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )

!
!     Set FAIL to .FALSE.
!
      FAIL = .FALSE.

!
!     Are we doing case-insensitive checks?
!
      IF ( COMP(1:1) .EQ. '~' ) THEN
         SN2CAS = .FALSE.
         MYCMP = COMP(2:)
      ELSE
         SN2CAS = .TRUE.
         MYCMP = COMP
      END IF

!
!     Set the length parameters.
!
      LVAL = LEN ( ARRAY(1) )
      LEXP = LEN ( EXP  (1) )

!
!     Do the comparisons.
!
      IF ( MYCMP .EQ. '=' ) THEN

         DO I = 1, SIZE

            IF ( .NOT. SN2CAS ) THEN
               CALL UCASE ( ARRAY(I), MYVAL )
               CALL UCASE ( EXP  (I), MYEXP )
            ELSE
               MYVAL = ARRAY(I)
               MYEXP = EXP  (I)
            END IF

            FAIL = MYVAL(1:LVAL) .NE. MYEXP(1:LEXP)

            IF ( FAIL ) THEN

               MESSGE = 'Value # of array # was not the value ' &
               //       'expected. /cr(3:)/cr ' &
               //       'The value was:         # /cr' &
               //       'the expected value was #./cr'

               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMC ( MESSGE, '#', ARRAY(I),            MESSGE )
               CALL REPMC ( MESSGE, '#', EXP(I),              MESSGE )

               CALL TSTLOG( ' ',    FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )

               OK = .NOT. FAIL

               RETURN

            END IF

         END DO

      ELSE IF ( MYCMP .EQ. '>' ) THEN

         DO I = 1, SIZE

            IF ( .NOT. SN2CAS ) THEN
               CALL UCASE ( ARRAY(I), MYVAL )
               CALL UCASE ( EXP  (I), MYEXP )
            ELSE
               MYVAL = ARRAY(I)
               MYEXP = EXP  (I)
            END IF

            FAIL = LLE ( MYVAL(1:LVAL), MYEXP(1:LEXP) )

            IF ( FAIL ) THEN

               MESSGE = 'Value # of array # was not greater than ' &
               //       'the value expected. /cr(3:)/cr ' &
               //       'The value was:         # /cr' &
               //       'the expected value was #./cr'

               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMC ( MESSGE, '#', ARRAY(I),            MESSGE )
               CALL REPMC ( MESSGE, '#', EXP(I),              MESSGE )

               CALL TSTLOG( ' ',    FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )

               OK = .NOT. FAIL

               RETURN

            END IF

         END DO

      ELSE IF ( MYCMP .EQ. '<' ) THEN

         DO I = 1, SIZE

            IF ( .NOT. SN2CAS ) THEN
               CALL UCASE ( ARRAY(I), MYVAL )
               CALL UCASE ( EXP  (I), MYEXP )
            ELSE
               MYVAL = ARRAY(I)
               MYEXP = EXP  (I)
            END IF

            FAIL = LGE ( MYVAL(1:LVAL), MYEXP(1:LEXP) )

            IF ( FAIL ) THEN

               MESSGE = 'Value # of array # was not less than the ' &
               //       'value expected. /cr(3:)/cr ' &
               //       'The value was:         # /cr' &
               //       'the expected value was #./cr'

               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMC ( MESSGE, '#', ARRAY(I),            MESSGE )
               CALL REPMC ( MESSGE, '#', EXP(I),              MESSGE )

               CALL TSTLOG( ' ',    FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )

               OK = .NOT. FAIL

               RETURN

            END IF

         END DO

      ELSE IF ( MYCMP .EQ. '>=' ) THEN

         DO I = 1, SIZE

            IF ( .NOT. SN2CAS ) THEN
               CALL UCASE ( ARRAY(I), MYVAL )
               CALL UCASE ( EXP  (I), MYEXP )
            ELSE
               MYVAL = ARRAY(I)
               MYEXP = EXP  (I)
            END IF

            FAIL = LLT ( MYVAL(1:LVAL), MYEXP(1:LEXP) )

            IF ( FAIL ) THEN

               MESSGE = 'Value # of array # was not greater than or ' &
               //       'equal to the value expected. /cr(3:)/cr ' &
               //       'The value was:         # /cr' &
               //       'the expected value was #./cr'

               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMC ( MESSGE, '#', ARRAY(I),            MESSGE )
               CALL REPMC ( MESSGE, '#', EXP(I),              MESSGE )

               CALL TSTLOG( ' ',    FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )

               OK = .NOT. FAIL

               RETURN

            END IF

         END DO

      ELSE IF ( MYCMP .EQ. '<=' ) THEN

         DO I = 1, SIZE

            IF ( .NOT. SN2CAS ) THEN
               CALL UCASE ( ARRAY(I), MYVAL )
               CALL UCASE ( EXP  (I), MYEXP )
            ELSE
               MYVAL = ARRAY(I)
               MYEXP = EXP  (I)
            END IF

            FAIL = LGT ( MYVAL(1:LVAL), MYEXP(1:LEXP) )

            IF ( FAIL ) THEN

               MESSGE = 'Value # of array # was not less than or ' &
               //       'equal to the value expected. /cr(3:)/cr ' &
               //       'The value was:         # /cr' &
               //       'the expected value was #./cr'

               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMC ( MESSGE, '#', ARRAY(I),            MESSGE )
               CALL REPMC ( MESSGE, '#', EXP(I),              MESSGE )

               CALL TSTLOG( ' ',    FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )

               OK = .NOT. FAIL

               RETURN

            END IF

         END DO

      ELSE IF ( MYCMP .EQ. '!=' ) THEN

         DO I = 1, SIZE

            IF ( .NOT. SN2CAS ) THEN
               CALL UCASE ( ARRAY(I), MYVAL )
               CALL UCASE ( EXP  (I), MYEXP )
            ELSE
               MYVAL = ARRAY(I)
               MYEXP = EXP  (I)
            END IF

            FAIL = MYVAL(1:LVAL) .EQ. MYEXP(1:LEXP)

            IF ( FAIL ) THEN

               MESSGE = 'Value # of array # should have been different ' &
               //       'from the expected value. /cr(3:)/cr ' &
               //       'The value was:         # /cr' &
               //       'the expected value was #./cr'

               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMC ( MESSGE, '#', ARRAY(I),            MESSGE )
               CALL REPMC ( MESSGE, '#', EXP(I),              MESSGE )

               CALL TSTLOG( ' ',    FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )

               OK = .NOT. FAIL

               RETURN

            END IF

         END DO

      ELSE

         FAIL   = .TRUE.

         MESSGE = 'The comparison "#" is not recognized. '
         CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )

      END IF

      IF ( .NOT. FAIL ) THEN

         MESSGE = 'The comparison /cr/cr(3:)''# # EXPECTED'' ' &
         //       '/cr/cr(-3:) ' &
         //       'was satisfied.'

         CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
         CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )

         IF ( VERBOS() ) THEN

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
