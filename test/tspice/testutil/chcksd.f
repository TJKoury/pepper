C$Procedure      CHCKSD ( Check Scalar d.p. )
C
      SUBROUTINE CHCKSD ( NAME, VAL, COMP, EXP, TOL, OK )
C
C$ Abstract
C
C     Check an d.p. scalar value against some expected value.
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
      DOUBLE PRECISION      VAL
      CHARACTER*(*)         COMP
      DOUBLE PRECISION      EXP
      DOUBLE PRECISION      TOL
      LOGICAL               OK
 
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NAME       I   the name of the variable to be examined.
C      VAL        I   the actual variable
C      COMP       I   the kind of comparison to perform.
C      EXP        I   the comparison value for the variable
C      TOL        I   the tolerance allowed in comparing.
C      OK         O   TRUE if the test passes, FALSE otherwise..
C
C$ Detailed_Input
C
C     NAME        is the string used to give the name of a variable
C
C     VAL         is the actual d.p. variable to be examined
C
C     EXP         an expected value or bound on the value VAL.
C
C     COMP        a string giving the kind of comparison to perform:
C
C                    =    ---   check for strict equality
C                    >    ---   check for VAL >  EXP
C                    <    ---   check for VAL <  EXP
C                    >=   ---   check for VAL >= EXP ( VAL .GE. EXP )
C                    <=   ---   check for VAL <= EXP ( VAL .LE. EXP )
C                    !=   ---   check for VAL != EXP ( VAL .NE. EXP )
C                    ~    ---   check for VAL ~  EXP ( VAL within TOL
C                               of EXP)
C                    ~/   ---   check for VAL ~/ EXP ( Relative
C                               difference between VAL and EXP .LE. TOL)
C
C     TOL        is a "tolerance" to use when checking for VAL to
C                be nearly the same as EXP.
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
C$ Particulars
C
C     This routine handles a wide variety of comparisons between
C     scalar double precision values.
C
C$ Examples
C
C     Suppose that you have just made a call to a subroutine that
C     you wish to test (call the routine SPUD) and you would like
C     to test an output d.p. against an expected value and verify that
C     the relative difference is less than some value.  Using
C     this routine you can automatically have the test result logged
C     in via the testing utilities.
C
C        CALL SPUD   (  INPUT,   OUTPUT )
C        CALL CHCKSD ( 'OUTPUT', OUTPUT, '~/', EXPECT, 1.0D-12, OK )
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
C     N.J. Bachman    (JPL)
C     W.L. Taber      (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    Testing Utility Version 2.0.0, 22-APR-2014 (NJB) (BVS)
C
C        Added checks for expected or actual values equal to NaN.
C
C        Tweaked logic to do only necessary output message construction
C        operations to speed-up execution.
C
C-    Testing Utility Version 1.0.0, 7-NOV-1994 (WLT)
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
      DOUBLE PRECISION      REL
      LOGICAL               FAIL
 
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )
 
      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD

      LOGICAL               VALNAN
      LOGICAL               EXPNAN



 
      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ',
     .              'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )

C
C     Set placeholder message.
C 
      MESSGE = 'This is a placeholder message that should have never '//
     .         'been displayed. It indicates a CHCKSD bug.' 
 
 
      FAIL = .FALSE.
 
C
C     Test both actual and expected values to see whether they are NaN.
C 
      VALNAN =        ( .NOT. ( VAL .GT. 1.D0 ) )
     .          .AND. ( .NOT. ( VAL .LT. 1.D0 ) )
     .          .AND. ( .NOT. ( VAL .EQ. 1.D0 ) )

      EXPNAN =        ( .NOT. ( EXP .GT. 1.D0 ) )
     .          .AND. ( .NOT. ( EXP .LT. 1.D0 ) )
     .          .AND. ( .NOT. ( EXP .EQ. 1.D0 ) )


      IF ( VALNAN .OR. EXPNAN ) THEN
C
C        At least one of the input values is NaN; the normal 
C        comparisons won't tell us anything. We need to
C        construct a custom error message.
C
         FAIL = .TRUE.

         IF ( .NOT. VALNAN ) THEN
C
C           Only EXP is NaN.
C
            MESSGE = 'The value of # was #. The expected value was NaN.'
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )

         ELSE IF ( .NOT. EXPNAN ) THEN
C
C           Only VAL is NaN.
C
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
            MESSGE = 'The value of # was not the value expected./cr '
     .   //          'The value was:         # /cr '
     .   //          'the expected value was #. /cr'
     .   //          'The difference between these is: # .'
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL - EXP, 14,       MESSGE )
         END IF
 
 
      ELSE IF ( COMP .EQ. '<'  ) THEN
 
         FAIL   =  VAL .GE. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not less than # as was '
     .   //          'expected. /cr The value was #. /cr '
     .   //          'This is greater '
     .   //          'than # by #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL - EXP, 14,       MESSGE )
         END IF
 
      ELSE IF ( COMP .EQ. '>'  ) THEN
 
         FAIL   =  VAL .LE. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not greater than # as was '
     .   //          'expected. /cr The value was #. /crThis is less '
     .   //          'than # by #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP - VAL, 14,       MESSGE )
         END IF
 
      ELSE IF ( COMP .EQ. '>=' ) THEN
 
         FAIL   =  VAL .LT. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not greater than # as was '
     .   //          'expected. /cr The value was #./cr This is less '
     .   //          'than # by #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP - VAL, 14,       MESSGE )
         END IF
 
      ELSE IF ( COMP .EQ. '<=' ) THEN
 
         FAIL   =  VAL .GT. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not less than # as was '
     .   //          'expected. /cr The value was #./cr This is '
     .   //          'greater '
     .   //          'than # by #. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
            CALL REPMD ( MESSGE, '#', VAL - EXP, 14,       MESSGE )
         END IF
 
      ELSE IF ( COMP .EQ. '!=' ) THEN
 
         FAIL   = VAL .EQ. EXP

         IF ( FAIL ) THEN
            MESSGE = 'The value of # should have been different '
     .   //          'from #./cr It wasn''t. '
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMD ( MESSGE, '#', EXP, 14,             MESSGE )
         END IF
            
      ELSE IF ( COMP .EQ. '~'  ) THEN
 
         FAIL   = TOL .LT. DABS( VAL - EXP )
 
         IF ( FAIL ) THEN
            MESSGE = 'The value of # was not within # of #. /cr The '
     .   //          'value was #./cr The difference between the '
     .   //          'actual '
     .   //          'and expected value was #. '
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
            MESSGE = 'The value of # was #.  The expected value was '
     .   //          '#. /cr The relative difference between # and'
     .   //          'its '
     .   //          'expected value was #./cr The maximum relative '
     .   //          'difference allowed for a successful test is '
     .   //          '#. '
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
 
            MESSGE = 'The comparison/cr/cr(3:) ''# # #'' /cr/cr(-3:) '
     .   //          'was satisfied to the '
     .   //          'specified tolerance: #./crThe value of # was '
     .   //          '#. '
 
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
 
 
 
