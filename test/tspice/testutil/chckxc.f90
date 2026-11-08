!$Procedure      CHCKXC ( Check exceptions )

      SUBROUTINE CHCKXC ( EXCEPT, SHORT, OK )

! $ Abstract
!
!     Check an string scalar value against some expected value.
!
! $ Disclaimer
!
!     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
!     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
!     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
!     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
!     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
!     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
!     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
!     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
!     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
!     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
!
!     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
!     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
!     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
!     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
!     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
!     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
!
!     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
!     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
!     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
!     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
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

      IMPLICIT NONE
      LOGICAL               EXCEPT
      CHARACTER*(*)         SHORT
      LOGICAL               OK


! $ Brief_I/O
!
!      VARIABLE  I/O  DESCRIPTION
!      --------  ---  --------------------------------------------------
!      EXCEPT     I   Logical indicating if an exception should exist
!      SHORT      I   The short error string associated with exception
!
! $ Detailed_Input
!
!     EXCEPT      is a logical that indicates whether or not an
!                 exception should have occurred.  If EXCEPT is
!                 TRUE an exception is expected. Otherwise no
!                 exception is expected.
!
!     SHORT       is the short error message that is associated
!                 with an exception.  SHORT will be used only if
!                 EXCEPT is TRUE.  Otherwise it is ignored as no
!                 exception is expected.
!
!
!
! $ Detailed_Output
!
!     OK         if the check exception condition is successful then
!                OK is given the value TRUE.  Otherwise OK is given the
!                value FALSE and a diagnostic message is sent to the
!                test logger.
!
! $ Parameters
!
!      None.
!
! $ Files
!
!      The result of a failure is automatically logged in the testing
!      log file and failure file.  Success is logged only if verbose
!      testing has been enabled.
!
!
! $ Exceptions
!
!     Error free.
!
! $ Particulars
!
!     This routine checks that exceptions handling has the expected
!     status.
!
! $ Examples
!
!     Suppose that you have just made a call to a subroutine that
!     you wish to test (call the routine SPUD) and you would like
!     to test the handling of some exception.  Using
!     this routine you can automatically have the test result logged
!     in via the testing utitities.
!
!        CALL SPUD   (  INPUT,   OUTPUT )
!        CALL CHCKXC (  EXPECT, 'SPICE(ERRORMESSAGE)', OK )
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!      W.L. Taber      (JPL)
!
! $ Literature_References
!
!      None.
!
! $ Version
!
!-    Testing Utilities 2.0.0, 27-JUL-1999 (WLT)
!
!        Added code so that the fact that a check of the
!        status of the error handling system will be communicated
!        to the rest of the test utility software.
!
!-    Testing Utilities Version 1.0.0, 10-Nov-1994 (WLT)
!
!
!-&
!
!     Test Utility functions
!
      LOGICAL               VERBOS


!
!     Spicelib functions
!
      LOGICAL               FAILED
      LOGICAL               SETON
      INTEGER               RTRIM



!
!     Local Variables
!
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
      CALL TSTLGS ( 'LEFT 3 RIGHT 75 NEWLINE /cr ', &
                    'LEFT 3 RIGHT 75 NEWLINE /cr FLAG --- LEADER ---' )


      DUMMY = SETON('CHCKXC')

      IF ( .NOT. EXCEPT ) THEN

         FAIL = FAILED()

         IF ( FAIL ) THEN

            CALL GETMSG  ( 'SHORT',  SMSG )
            CALL GETMSG  ( 'LONG',   LMSG )
            CALL QCKTRC  ( CTRACE )

            CALL RESET

            MESSGE = 'No exception was expected however one ' &
            //       'exists.  The short error message was: ' &
            //       '''#''. '


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

            MESSGE = 'No exception was detected.  This is the ' &
            //       'expected behaviour. '

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

               MESSGE = 'The expected short error message, ''#'' ' &
               //       'was not found. Instead the short error ' &
               //       'message was ''#''. '

               CALL REPMC  ( MESSGE, '#', SHORT(1:RTRIM(SHORT)), &
                             MESSGE )
               CALL REPMC  ( MESSGE, '#', SMSG(1:RTRIM(SMSG)), &
                             MESSGE )
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

               MESSGE = 'The short error message was ''#'' as ' &
               //       'expected. '


               CALL REPMC  ( MESSGE, '#', SMSG(1:RTRIM(SMSG)), &
                             MESSGE )
               CALL TSTLOG ( MESSGE, FAIL )

               MESSGE = 'The long error message follows:'
               CALL TSTLOG ( ' ',    FAIL )
               CALL TSTLOG ( MESSGE, FAIL )
               CALL TSTLOG ( ' ',    FAIL )
               CALL TSTLOG ( LMSG,   FAIL )

            END IF

         ELSE

            FAIL = .TRUE.

            MESSGE = 'No exception was detected.  The expected ' &
            //       'short error message was: ''#''. '

            CALL REPMC  ( MESSGE, '#', SHORT(1:RTRIM(SHORT)), MESSGE )
            CALL TSTLOG ( MESSGE, FAIL )

         END IF

      END IF

      CALL TSTLGS ( GOOD, BAD )

      OK = .NOT. FAIL

      RETURN
      END
