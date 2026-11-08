C$Procedure      CHCKXC ( Check exceptions )
 
      SUBROUTINE CHCKXC ( EXCEPT, SHORT, OK )
 
C$ Abstract
C
C     Check an string scalar value against some expected value.
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
      LOGICAL               EXCEPT
      CHARACTER*(*)         SHORT
      LOGICAL               OK
 
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      EXCEPT     I   Logical indicating if an exception should exist
C      SHORT      I   The short error string associated with exception
C
C$ Detailed_Input
C
C     EXCEPT      is a logical that indicates whether or not an
C                 exception should have occurred.  If EXCEPT is
C                 TRUE an exception is expected. Otherwise no
C                 exception is expected.
C
C     SHORT       is the short error message that is associated
C                 with an exception.  SHORT will be used only if
C                 EXCEPT is TRUE.  Otherwise it is ignored as no
C                 exception is expected.
C
C
C
C$ Detailed_Output
C
C     OK         if the check exception condition is successful then
C                OK is given the value TRUE.  Otherwise OK is given the
C                value FALSE and a diagnostic message is sent to the
C                test logger.
C
C$ Parameters
C
C      None.
C
C$ Files
C
C      The result of a failure is automatically logged in the testing
C      log file and failure file.  Success is logged only if verbose
C      testing has been enabled.
C
C
C$ Exceptions
C
C     Error free.
C
C$ Particulars
C
C     This routine checks that exceptions handling has the expected
C     status.
C
C$ Examples
C
C     Suppose that you have just made a call to a subroutine that
C     you wish to test (call the routine SPUD) and you would like
C     to test the handling of some exception.  Using
C     this routine you can automatically have the test result logged
C     in via the testing utitities.
C
C        CALL SPUD   (  INPUT,   OUTPUT )
C        CALL CHCKXC (  EXPECT, 'SPICE(ERRORMESSAGE)', OK )
C
C$ Restrictions
C
C     None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-    Testing Utilities 2.0.0, 27-JUL-1999 (WLT)
C
C        Added code so that the fact that a check of the
C        status of the error handling system will be communicated
C        to the rest of the test utility software.
C
C-    Testing Utilities Version 1.0.0, 10-Nov-1994 (WLT)
C
C
C-&
C
C     Test Utility functions
C
      LOGICAL               VERBOS
 
 
C
C     Spicelib functions
C
      LOGICAL               FAILED
      LOGICAL               SETON
      INTEGER               RTRIM
 
 
 
C
C     Local Variables
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 640 )
 
      CHARACTER*(LNSIZE)    LMSG
      CHARACTER*(LNSIZE)    MESSGE
      CHARACTER*(LNSIZE)    CTRACE
 
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )
 
      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD
 
      INTEGER               WDSIZE
      PARAMETER           ( WDSIZE = 32 )
 
      CHARACTER*(WDSIZE)    SMSG
 
 
      LOGICAL               FAIL
      LOGICAL               DUMMY
 
      CALL TSTSTY ( GOOD, BAD )
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ',
     .              'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )
 
 
      DUMMY = SETON('CHCKXC')
 
      IF ( .NOT. EXCEPT ) THEN
 
         FAIL = FAILED()
 
         IF ( FAIL ) THEN
 
            CALL GETMSG  ( 'SHORT',  SMSG )
            CALL GETMSG  ( 'LONG',   LMSG )
            CALL QCKTRC  ( CTRACE )
            
            CALL RESET
 
            MESSGE = 'No exception was expected however one '
     .      //       'exists.  The short error message was: '
     .      //       '''#''. '
 
 
            CALL REPMC  ( MESSGE, '#', SMSG(1:RTRIM(SMSG)), MESSGE )
            CALL TSTLOG ( MESSGE, FAIL )
 
            MESSGE = 'The long error message follows:'
            CALL TSTLOG ( ' ',    FAIL )
            CALL TSTLOG ( MESSGE, FAIL )
            CALL TSTLOG ( ' ',    FAIL )
            CALL TSTLOG ( LMSG,   FAIL )
            
            MESSGE = 'The current trace is: '
            CALL SUFFIX ( CTRACE, 1, MESSGE )
            CALL TSTLOG ( ' ',    FAIL )
            CALL TSTLOG ( MESSGE, FAIL )
            
            
            
 
         ELSE IF ( VERBOS() ) THEN
 
            MESSGE = 'No exception was detected.  This is the '
     .      //       'expected behaviour. '
 
            CALL TSTLOG ( ' ',    FAIL )
            CALL TSTLOG ( MESSGE, FAIL )
 
         END IF
 
      ELSE
 
         IF ( FAILED() ) THEN
 
            CALL GETMSG  ( 'SHORT',  SMSG )
            CALL GETMSG  ( 'LONG',   LMSG )
            CALL QCKTRC  ( CTRACE )
            CALL RESET
 
            FAIL = SMSG .NE. SHORT
 
            IF ( FAIL ) THEN
 
               MESSGE = 'The expected short error message, ''#'' '
     .         //       'was not found. Instead the short error '
     .         //       'message was ''#''. '
 
               CALL REPMC  ( MESSGE, '#', SHORT(1:RTRIM(SHORT)),
     .                       MESSGE )
               CALL REPMC  ( MESSGE, '#', SMSG(1:RTRIM(SMSG)),
     .                       MESSGE )
               CALL TSTLOG ( MESSGE, FAIL )
 
               MESSGE = 'The long error message follows:'
               CALL TSTLOG ( ' ',    FAIL )
               CALL TSTLOG ( MESSGE, FAIL )
               CALL TSTLOG ( ' ',    FAIL )
               CALL TSTLOG ( LMSG,   FAIL )

               MESSGE = 'The current trace is: '
               CALL SUFFIX ( CTRACE, 1, MESSGE )
               CALL TSTLOG ( ' ',    FAIL )
               CALL TSTLOG ( MESSGE, FAIL )
            

 
            ELSE IF ( VERBOS() ) THEN
 
               MESSGE = 'The short error message was ''#'' as '
     .         //       'expected. '
 
 
               CALL REPMC  ( MESSGE, '#', SMSG(1:RTRIM(SMSG)),
     .                       MESSGE )
               CALL TSTLOG ( MESSGE, FAIL )
 
               MESSGE = 'The long error message follows:'
               CALL TSTLOG ( ' ',    FAIL )
               CALL TSTLOG ( MESSGE, FAIL )
               CALL TSTLOG ( ' ',    FAIL )
               CALL TSTLOG ( LMSG,   FAIL )
 
            END IF
 
         ELSE
 
            FAIL = .TRUE.
 
            MESSGE = 'No exception was detected.  The expected '
     .      //       'short error message was: ''#''. '
 
            CALL REPMC  ( MESSGE, '#', SHORT(1:RTRIM(SHORT)), MESSGE )
            CALL TSTLOG ( MESSGE, FAIL )
 
         END IF
 
      END IF
 
      CALL TSTLGS ( GOOD, BAD )
 
      OK = .NOT. FAIL
 
      RETURN
      END
