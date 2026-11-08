C$Procedure      CHCKSI ( Check Scalar Integer )
C
      SUBROUTINE CHCKSI ( NAME, VAL, COMP, EXP, TOL, OK )
C
C$ Abstract
C
C     Check an integer scalar value against some expected value.
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
C      None.
C
C$ Keywords
C
C       TESTING
C
C$ Declarations
C
      IMPLICIT NONE
      CHARACTER*(*)         NAME
      INTEGER               VAL
      CHARACTER*(*)         COMP
      INTEGER               EXP
      INTEGER               TOL
      LOGICAL               OK
 
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NAME       I   the name of the variable to be examined.
C      VAL        I   the actual variable
C      EXP        I   the comparison value for the variable
C      COMP       I   the kind of comparison to perform.
C      TOL        I   the tolerance allowed in comparing.
C      OK         O   TRUE if the test passes, FALSE otherwise..
C
C$ Detailed_Input
C
C     NAME        is the string used to give the name of a variable
C
C     VAL         is the actual integer variable to be examined
C
C     EXP         an expected value or bound on the value VAL.
C
C                    =    ---   check for strict equality
C                    >    ---   check for VAL >  EXP
C                    <    ---   check for VAL <  EXP
C                    >=   ---   check for VAL >= EXP ( VAL .GE. EXP )
C                    <=   ---   check for VAL <= EXP ( VAL .LE. EXP )
C                    !=   ---   check for VAL != EXP ( VAL .NE. EXP )
C                    ~    ---   check for VAL ~  EXP ( VAL within TOL
C                               of EXP)
C
C$ Detailed_Output
C
C     OK         if the check of the input variable is successful then
C                OK is given the value TRUE.  Otherwise OK is given the
C                value FALSE and a diagnostic message is sent to the
C                test logger.
C
C$ Parameters
C
C      None.
C
C$ Exceptions
C
C     Error free.
C
C$ Files
C
C      The result of a failure is automatically logged in the testing
C      log file and failure file.  Success is logged only if verbose
C      testing has been enabled.
C
C
C$ Particulars
C
C     This routine handles a wide variety of comparisons between
C     scalar integer values.
C
C$ Examples
C
C     Suppose that you have just made a call to a subroutine that
C     you wish to test (call the routine SPUD) and you would like
C     to test an output integer against an expected value.  Using
C     this routine you can automatically have the test result logged
C     in via the testing utilities.
C
C        CALL SPUD   (  INPUT,   OUTPUT )
C        CALL CHCKSI ( 'OUTPUT', OUTPUT, '=', EXPECT, 0, OK )
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
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    Testing Utility Version 1.1.0, 05-AUG-2013 (BVS)
C
C        Tweaked logic to do only necessary output message construction
C        operations to speed-up execution.
C
C-    Testing Utilities Version 1.0.0, 8-Nov-1994 (WLT)
C
C
C-&
 
C
C     Test Utility functions
C
      LOGICAL               VERBOS
 
 
C
C     SPICELIB functions
C
      INTEGER               RTRIM
 
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 320 )
 
      CHARACTER*(LNSIZE)    MESSGE
      LOGICAL               FAIL
 
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )
 
      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD
 
      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ',
     .              'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )
 
C
C     Set placeholder message.
C 
      MESSGE = 'This is a placeholder message that should have never '//
     .         'been displayed. It indicates a CHCKSI bug.' 

      FAIL = .FALSE.
 
      IF      ( COMP .EQ. '='  ) THEN
 
         FAIL = VAL .NE. EXP
 
         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not the value expected. '
     .   //         'The value was: # the expected value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
         END IF
 
      ELSE IF ( COMP .EQ. '<'  ) THEN
 
         FAIL   =  VAL .GE. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not less than # as was '
     .   //          'expected.  The value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
         END IF
 
      ELSE IF ( COMP .EQ. '>'  ) THEN
 
         FAIL   =  VAL .LE. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not greater than # as was '
     .   //          'expected.  The value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
         END IF
 
      ELSE IF ( COMP .EQ. '>=' ) THEN
 
         FAIL   =  VAL .LT. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not greater than # as was '
     .   //          'expected.  The value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
         END IF
 
      ELSE IF ( COMP .EQ. '<=' ) THEN
 
         FAIL   =  VAL .GT. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not less than # as was '
     .   //          'expected.  The value was #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
            CALL REPMI ( MESSGE, '#', VAL,                 MESSGE )
         END IF
 
      ELSE IF ( COMP .EQ. '!=' ) THEN
 
         FAIL   = VAL .EQ. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # should have been different '
     .   //          'from #. It wasn''t. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMI ( MESSGE, '#', EXP,                 MESSGE )
         END IF
            
      ELSE IF ( COMP .EQ. '~'  ) THEN
 
         FAIL   = TOL .LT. ABS( VAL - EXP )
 
         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not within # of #.  The '
     .   //          'value was # . The difference between the actual '
     .   //          'and expected value was #. '
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
 
            MESSGE = 'The comparison ''# # #'' was satisfied. '
     .   //          'The value of # was #. '
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
 
 
