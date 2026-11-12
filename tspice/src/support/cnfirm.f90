!$Procedure CNFIRM ( Return status of a yes/no query )

      SUBROUTINE CNFIRM ( PRMPT, TORF )
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     Return the .TRUE./.FALSE. status of a query which has a yes/no
!     response.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     PARSING
!     UTILITY
!
! $ Declarations


      CHARACTER*(*)         PRMPT
      LOGICAL               TORF

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     PRMPT      I   The prompt used to elicit a yes/no response.
!     TORF       O   The truth value of a yes/no response.
!
! $ Detailed_Input
!
!     PRMPT    The prompt which is used to elicit a yes/no response.
!
! $ Detailed_Output
!
!     TORF     A logical flag which indicates the truth value of a
!              yes/no response to a continue/try again prompt. If the
!              response was equivalent to yes, TORF = .TRUE.. If the
!              response was equivalent to no, TORF = .FALSE..
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     Error free.
!
!     1)   Any input value that is not equivalent to 'Y', 'YES', 'N'
!          or 'NO' (or lower case equivalents), will cause the routine
!          to redisplay the prompt. A yes/no response MUST be given,
!          there are no implicit values for any other response.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Often a program needs to ask whether or not a user wishes
!     to exercise some option. This routine simplifies the task
!     of converting the answer to a logical value.
!
!     If the response to a yes/no question is logically equivalent
!     to 'YES' the variable TORF will be set to a value of .TRUE.
!     If the response to a yes/no question is logically equivalent
!     to 'NO' the variable TORF will be set to a value of .FALSE.
!     Any other response will cause the routine to redisplay the
!     prompt.
!
! $ Examples
!
!     Suppose you need to ask a user whether or not diagnostic
!     information about the behaviour of a program should be
!     written to a file.  Using this routine, you can easily
!     take the action desired and avoid the details of parsing
!     the user's answer.
!
!        PRMPT = 'Log information to a file? (Yes/No) '
!        OK = .FALSE.
!        CALL CONFRM( PRMPT, OK )
!
!        IF ( OK ) THEN
!
!        ...enable recording diagnostics in the log file.
!
!        ELSE
!
!        ...disable recording of diagnostics.
!
!        END IF
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
!     K.R. Gehringer (JPL)
!     W.L. Taber     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-DEC-2010 (EDW)
!
!         Declared PROMPT as EXTERNAL.
!
!-    Beta Version 1.0.0, 09-SEP-1992 (KRG)
!
!-&

! $ Index_Entries
!
!      prompt with a yes/no query and return logical response
!
!-&


!
!     SPICELIB functions
!
!     None.
!

!
!     Local Parameters
!
      INTEGER               LINLEN
      PARAMETER           ( LINLEN = 256 )
!
!     Local Variables
!
      CHARACTER*(LINLEN)    RESPNS
      LOGICAL               YESNO


!
!     Do while we have not gotten a yes/no response
!
      YESNO = .FALSE.
      DO WHILE ( .NOT. YESNO )
!
!        Prompt for a response
!
         CALL PROMPT( PRMPT, RESPNS )
!
!        Left justify the response string, RESPNS, and convert it to
!        uppercase.
!
         CALL LJUST ( RESPNS, RESPNS )
         CALL UCASE ( RESPNS, RESPNS )

         IF ( ( RESPNS .EQ. 'Y' ) .OR. ( RESPNS .EQ. 'YES' ) ) THEN

            TORF  = .TRUE.
            YESNO = .TRUE.

         ELSE IF( ( RESPNS .EQ. 'N' ) .OR. ( RESPNS .EQ. 'NO' ) ) THEN

            TORF  = .FALSE.
            YESNO = .TRUE.

         END IF

      END DO

      RETURN
      END
