C$Procedure      TSTLIP ( Test log if passed )
C
      SUBROUTINE TSTLIP ()
C
C$ Abstract
C
C     If the last registered test case passed and test case logging
C     is enabled, log the success of the last test case to SCREEN
C     and the log file.
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
 
C
C$ Brief_I/O
C
C      None.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     None.
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
C     If the last test case passed and individual test case logging
C     is enabled.  Log the success of that case.
C
C$ Examples
C
C     Later.
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
C      W.L. Taber      (JPL)
C     
C$ Version
C
C-    Version 2.0.0, 29-MAY-2010 (NJB)
C
C        Increase line size to 2000.
C
C-    Testing Utilities 1.0.0, 18-NOV-1994 (WLT)
C
C
C-&

C$ Index_Entries
C
C     Log a succussful test case.
C
C-&
C
C     Test Utility Functions
C
      LOGICAL               VERBOS
C
C     Local Variables.
C
      INTEGER               LNSIZE
      PARAMETER           ( LNSIZE = 2000 )
 
      CHARACTER*(LNSIZE)    MESSGE
 
      INTEGER               NUMBER
 
      LOGICAL               LOGIFP
      LOGICAL               PASSED
 
      INTEGER               STYSIZ
      PARAMETER           ( STYSIZ = 120 )
 
      CHARACTER*(STYSIZ)    GOOD
      CHARACTER*(STYSIZ)    BAD
      CHARACTER*(STYSIZ)    PASS
      CHARACTER*(STYSIZ)    FAIL
      SAVE
 
      DATA                  LOGIFP / .FALSE. /
 
      PASS = 'LEFT 1 RIGHT 78 HARDSPACE $ FLAG Case$Passed:'
      FAIL = 'LEFT 1 RIGHT 78 HARDSPACE $ FLAG CASE$FAILED!'
 
 
      IF ( LOGIFP .OR. VERBOS() ) THEN
 
         MESSGE = ' '
C
C        Copy the current logging style.
C
         CALL TSTSTY ( GOOD, BAD )
C
C        See whether or not this case passed and get its number.
C
         CALL T_CPASS ( PASSED )
         CALL T_CNUM  ( NUMBER )
 
         IF ( NUMBER .GT. 0 .AND. PASSED ) THEN
C
C           Print a blank line before the output message only
C           if we are in verbose mode.
C
            IF ( VERBOS() ) THEN
               CALL TSTLOG   ( ' ', .FALSE. )
            END IF
C
C           Set the logging style to PASS and print the case name.
C
            CALL TSTLGS   ( PASS,    PASS   )
            CALL T_CNAME  ( MESSGE          )
            CALL TSTLOG   ( MESSGE, .FALSE. )
 
         ELSE IF ( NUMBER .GT. 0 ) THEN
C
C           Always print a blank line before a case that failed.
C
            CALL TSTLOG   ( ' ',    .FALSE. )
C
C           Set the logging style to FAIL and print the case name.
C
            CALL TSTLGS   ( FAIL,    FAIL   )
            CALL T_CNAME  ( MESSGE          )
            CALL TSTLOG   ( MESSGE, .FALSE. )
 
         END IF
 
         CALL TSTLGS ( GOOD, BAD )
 
      END IF
      RETURN
 
 
C
C     Write a blank line if we are in case logging mode or verbose
C     mode.
C
      ENTRY TSTCBL ()
 
         IF ( LOGIFP .OR. VERBOS() ) THEN
            CALL TSTLOG ( ' ', .FALSE. )
         END IF
         RETURN
 
 
C
C     To turn on the logging of individual test cases call TSTLCY
C
      ENTRY TSTLCY ()
 
         LOGIFP = .TRUE.
         RETURN
 
C
C     To turn on the logging of individual test cases call TSTLCY
C
      ENTRY TSTLCN ()
 
         LOGIFP = .FALSE.
         RETURN
 
      END
 
 
 
 
 
 
