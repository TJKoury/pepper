C$Procedure      CHCKSL ( Check Scalar logical )
C
      SUBROUTINE CHCKSL ( NAME, VAL, EXP,  OK )
C
C$ Abstract
C
C     Check a logical value against an expected value.
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
      LOGICAL               VAL
      LOGICAL               EXP
      LOGICAL               OK
 
C
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NAME       I   the name of the variable to be examined.
C      VAL        I   the actual variable
C      EXP        I   the comparison value for the variable
C      OK         O   TRUE if the test passes, FALSE otherwise..
C
C$ Detailed_Input
C
C     NAME        is the string used to give the name of a variable
C
C     VAL         is the actual logical variable to be examined
C
C     EXP         an expected value or bound on the value VAL.
C
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
C     to test an output logical against an expected value and
C     verify that they are the same. Using
C     this routine you can automatically have the test result logged
C     in via the testing utilities.
C
C        CALL SPUD   (  INPUT,   OUTPUT )
C        CALL CHCKSD ( 'OUTPUT', OUTPUT, EXPECT, OK )
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
     .         'been displayed. It indicates a CHCKSL bug.' 
 
C
C     Just make sure that the values of VAL and EXP are the same.
C
      IF      ( EXP ) THEN
 
         OK = VAL
 
         IF ( OK ) THEN
            IF ( VERBOS() ) THEN
               MESSGE = 'The value of # was TRUE as expected. '
            END IF
         ELSE
            MESSGE = 'The expected value of # was TRUE.  The '
     .      //       'actual value was FALSE'
         END IF
 
      ELSE
 
         OK = .NOT. VAL
 
         IF ( OK ) THEN
            IF ( VERBOS() ) THEN
               MESSGE = 'The value of # was FALSE as expected. '
            END IF
         ELSE
            MESSGE = 'The expected value of # was FALSE.  The '
     .      //       'actual value was TRUE'
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
 
 
 
