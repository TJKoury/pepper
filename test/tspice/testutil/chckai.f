C$Procedure      CHCKAI ( Check an array of integers )
 
      SUBROUTINE CHCKAI ( NAME, ARRAY, COMP, EXP, SIZE,  OK )
 
C$ Abstract
C
C     Check the  values in two integer arrays..
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
 
      IMPLICIT NONE
      CHARACTER*(*)         NAME
      INTEGER               ARRAY ( * )
      CHARACTER*(*)         COMP
      INTEGER               EXP   ( * )
      INTEGER               SIZE
      LOGICAL               OK
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      NAME       I   the name of the array to be examined.
C      ARRAY      I   the actual array
C      COMP       I   the kind of comparison to perform.
C      EXP        I   the comparison values for the array
C      SIZE       I   the size of the input array
C      OK         O   TRUE if the test passes, FALSE otherwise..
C
C$ Detailed_Input
C
C     NAME        is the string used to give the name of an array.
C
C     ARRAY       is the actual d.p. array to be examined
C
C     COMP        a string giving the kind of comparison to perform:
C
C                    =    ---   check for strict equality
C
C     EXP         an expected values or bounds on the values in ARRAY.
C
C
C$ Detailed_Output
C
C     OK         if the check of the input array is successful then
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
C      None.
C
C$ Particulars
C
C     This routine handles a wide variety of comparisons between
C     double precision arrays.
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
C        CALL CHCKAI ( 'OUTPUT', OUTPUT, '=', EXPECT, OK )
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C      None.
C
C$ Author_and_Institution
C
C     W.L. Taber      (JPL)
C     F.S. Turner     (JPL)
C     B.V. Semenov    (JPL)
C
C$ Version
C
C-    Testing Utility Version 1.2.0, 05-AUG-2013 (BVS)
C
C        Tweaked logic to do only necessary output message construction
C        operations to speed-up execution.
C
C-    Testing Utility Version 1.1.0, 10-MAY-2001 (FST)
C
C        OK is now properly set before returning control to the
C        caller in all cases.
C
C-    Testing Utility Version 1.0.0, 7-NOV-1994 (WLT)
C
C
C-&
C
C     SPICELIB functions
C
      INTEGER               RTRIM
C
C     Testing Utility Functions
C
      LOGICAL               VERBOS
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 800 )
 
 
      CHARACTER*(LNSIZE)    MESSGE
 
      INTEGER               I
 
 
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
     .         'been displayed. It indicates a CHCKAI bug.' 
 
 
      FAIL = .FALSE.
 
      IF      ( COMP .EQ. '='  ) THEN
 
         DO I = 1, SIZE
 
            FAIL = ARRAY(I) .NE. EXP(I)
 
            IF ( FAIL ) THEN
               MESSGE = 'Value # of array # was not the value '
     .         //       'expected. /cr(3:)/cr '
     .         //       'The value was:         # /cr'
     .         //       'the expected value was #./cr'
     .         //       'The difference between these is: # . '
               CALL REPMI ( MESSGE, '#', I,                   MESSGE )
               CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
               CALL REPMI ( MESSGE, '#', ARRAY(I),            MESSGE )
               CALL REPMI ( MESSGE, '#', EXP(I),              MESSGE )
               CALL REPMI ( MESSGE, '#', ARRAY(I)-EXP(I),     MESSGE )
               CALL TSTLOG( ' ',    FAIL )
               CALL TSTLOG( MESSGE, FAIL )
               CALL TSTLGS ( GOOD, BAD )
               OK = .NOT. FAIL
               RETURN
            END IF
         END DO
 
      ELSE
 
         FAIL = .TRUE.
 
         MESSGE = 'The comparison "#" is not recognized. '
         CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )
 
      END IF
 
      IF ( .NOT. FAIL ) THEN
 
         IF ( VERBOS() ) THEN
 
            MESSGE = 'The comparison /cr/cr(3:)''# # EXPECTED'' '
     .   //          '/cr/cr(-3:) '
     .   //          'was satisfied.'
 
            CALL REPMC ( MESSGE, '#', NAME(1:RTRIM(NAME)), MESSGE )
            CALL REPMC ( MESSGE, '#', COMP(1:RTRIM(COMP)), MESSGE )
 
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
