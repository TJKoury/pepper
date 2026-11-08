C$Procedure CHCKAC ( Check an array of characters )

      SUBROUTINE CHCKAC ( NAME, ARRAY, COMP, EXP, SIZE, OK )

C$ Abstract
C
C     Check the values in two character arrays.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     TESTING
C
C$ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         NAME
      CHARACTER*(*)         ARRAY ( * )
      CHARACTER*(*)         COMP
      CHARACTER*(*)         EXP   ( * )
      INTEGER               SIZE
      LOGICAL               OK

C$ Brief_I/O
C
C     VARIABLE  I/O  DESCRIPTION
C     --------  ---  --------------------------------------------------
C     NAME       I   the name of the array to be examined.
C     ARRAY      I   the actual array.
C     COMP       I   the kind of comparison to perform.
C     EXP        I   the comparison values for the array.
C     SIZE       I   the size of the input array
C     OK         O   .TRUE. if the test passes, .FALSE. otherwise.
C
C$ Detailed_Input
C
C     NAME     is the string used to give the name of an array.
C
C     ARRAY    is the actual array of strings to be examined.
C
C     COMP     is a string giving the kind of comparison to perform:
C
C                 =     --   check for strict equality
C                 >     --   check for VAL >  EXP
C                 <     --   check for VAL <  EXP
C                 >=    --   check for VAL >= EXP ( LGE (VAL, EXP) )
C                 <=    --   check for VAL <= EXP ( LLE (VAL, EXP) )
C                 !=    --   check for VAL != EXP ( VAL .NE. EXP   )
C
C              The following are the case insensitive versions
C              of the previous checks.
C
C                 ~=    --   check for strict equality
C                 ~>    --   check for VAL >  EXP
C                 ~<    --   check for VAL <  EXP
C                 ~>=   --   check for VAL >= EXP
C                 ~<=   --   check for VAL <= EXP
C                 ~!=   --   check for VAL != EXP
C
C     EXP      the array of expected values or bounds on values
C              in ARRAY.
C
C$ Detailed_Output
C
C     OK       if the check of the input array is successful then
C              OK is given the value .TRUE. Otherwise OK is given the
C              value .FALSE. and a diagnostic message is sent to the
C              test logger.
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     This routine handles a wide variety of comparisons between
C     character string arrays.
C
C$ Examples
C
C     Suppose that you have just made a call to a subroutine that
C     you wish to test (call the routine SPUD) and you would like
C     to test an array of output strings matches your expectations.
C     Using this routine you can automatically have the test
C     result logged in via the testing utilities.
C
C        CALL SPUD   (  INPUT,   OUTPUT )
C        CALL CHCKAC ( 'OUTPUT', OUTPUT, '=', EXPECT, OK )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     None.
C
C$ Author_and_Institution
C
C     J. Diaz del Rio    (ODC Space)
C     F.S. Turner        (JPL)
C
C$ Version
C
C-    Test Utilities Version 1.0.1, 31-MAY-2021 (JDR)
C
C        Updated the header to comply with NAIF standard. Reordered
C        sections and added missing $Index_Entries.
C
C-    Test Utilities Version 1.0.0, 24-DEC-2001 (FST)
C
C-&


C$ Index_Entries
C
C     Check a two-dimensional array of characters
C
C-&


C
C     SPICELIB Functions
C
      INTEGER               RTRIM

C
C     Testing Utility Functions
C
      LOGICAL               VERBOS

C
C     Local Parameters
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )

      INTEGER               MAXLEN
      PARAMETER           ( MAXLEN = 4000 )

      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )

C
C     Local Variables
C
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

C
C     Setup style strings?
C
      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ',
     .              'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )

C
C     Set FAIL to .FALSE.
C
      FAIL = .FALSE.

C
C     Are we doing case-insensitive checks?
C
      IF ( COMP(1:1) .EQ. '~' ) THEN
         SN2CAS = .FALSE.
         MYCMP = COMP(2:)
      ELSE
         SN2CAS = .TRUE.
         MYCMP = COMP
      END IF

C
C     Set the length parameters.
C
      LVAL = LEN ( ARRAY(1) )
      LEXP = LEN ( EXP  (1) )

C
C     Do the comparisons.
C
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

               MESSGE = 'Value # of array # was not the value '
     .         //       'expected. /cr(3:)/cr '
     .         //       'The value was:         # /cr'
     .         //       'the expected value was #./cr'

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

               MESSGE = 'Value # of array # was not greater than '
     .         //       'the value expected. /cr(3:)/cr '
     .         //       'The value was:         # /cr'
     .         //       'the expected value was #./cr'

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

               MESSGE = 'Value # of array # was not less than the '
     .         //       'value expected. /cr(3:)/cr '
     .         //       'The value was:         # /cr'
     .         //       'the expected value was #./cr'

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

               MESSGE = 'Value # of array # was not greater than or '
     .         //       'equal to the value expected. /cr(3:)/cr '
     .         //       'The value was:         # /cr'
     .         //       'the expected value was #./cr'

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

               MESSGE = 'Value # of array # was not less than or '
     .         //       'equal to the value expected. /cr(3:)/cr '
     .         //       'The value was:         # /cr'
     .         //       'the expected value was #./cr'

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

               MESSGE = 'Value # of array # should have been different '
     .         //       'from the expected value. /cr(3:)/cr '
     .         //       'The value was:         # /cr'
     .         //       'the expected value was #./cr'

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

         MESSGE = 'The comparison /cr/cr(3:)''# # EXPECTED'' '
     .   //       '/cr/cr(-3:) '
     .   //       'was satisfied.'

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
