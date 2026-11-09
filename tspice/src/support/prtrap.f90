!$Procedure     PRTRAP

      SUBROUTINE PRTRAP ( COMMAND, TRAN )
      IMPLICIT NONE

! $ Abstract
!
!     Determine whether the given command should be trapped (left
!     untranslated).
!
! $ Keywords
!
!     PERCY
!
! $ Declarations

      CHARACTER*(*)    COMMAND
      LOGICAL          TRAN

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     COMMND     I   PERCY command to be evaluated.
!     TRAN       I   True if further translation is needed.
!
! $ Detailed_Input
!
!     COMMAND    is the input PERCY command. The following commands
!                should not be translated fully. (A moment's thought
!                will show why.)
!
!                        - SHOW SYMBOL <symbol>
!
!                        - INQUIRE <symbol>
!
!                If translation has proceeded far enough for either
!                of these statements to be recognized, then it has
!                gone far enough.
!
! $ Detailed_Output
!
!     TRAN       is true if further translation of COMMAND is okay.
!                If any of the statements mentioned above is recognized,
!                TRAN is false. (This will prevent PERCY from trying
!                to resolve any more symbols.)
!
! $ Input_Files
!
!     None.
!
! $ Output_Files
!
!     None.
!
! $ Input_Output_Common
!
!     See 'SYMBOLS.INC'.
!
! $ Detailed_Description
!
!     Get the first three words of COMMAND.
!
!         - If the first two words are SHOW SYMBOL, and the
!           third word is not blank and does not end with '?',
!           then this should be trapped.
!
!         - If the first word is INQUIRE and the second word
!           is not blank and does not end with '?', then this
!           should be trapped.
!
!     If the statement should be trapped, set TRAN to false and return.
!
! $ Examples
!
!     Command                                 Trap?
!     ------------------------------------    -----
!     'SHOW SYMBOL CARROT        '              Y
!     'SHOW SYMBOL               '              N
!     'SHOW SYMBOL SYMBOL_NAME?  '              N
!
!     'INQUIRE PRIMARY_PLANET    '              N
!     'INQUIRE                   '              Y
!     'INQUIRE QUERY_NAME?       '              Y
!
! $ Restrictions
!
!     None.
!
! $ Author_and_Institution
!
!     W. L. Taber     (JPL)
!     I. M. Underwood (JPL)
!
! $ Version_and_Date
!
!     Version 1, 17-SEP-1986
!
!-&


!
!     Spicelib Functions
!
      INTEGER               RTRIM
!
!     Local variables
!
      CHARACTER*33          WORD (3)
      INTEGER               LOC
      INTEGER               I



!
!     Get the first three words of COMMAND.
!
      DO I = 1, 3
         CALL NTHWD ( COMMAND, I, WORD(I), LOC )
         CALL UCASE ( WORD(I),    WORD(I)      )
      END DO


!
!     Is this a SHOW SYMBOL command?
!
      IF ( WORD(1) .EQ. 'SHOW' .AND.  WORD(2) .EQ. 'SYMBOL' ) THEN

!
!        The third word must not be blank, and must not end with '?'.
!        (WORD is longer than any allowable symbol or query, so there
!        should always be a blank at the end.)
!
         IF ( WORD(3) .NE. ' ' ) THEN

            LOC = RTRIM(WORD(3))

            IF ( WORD(3)(LOC:LOC) .NE. '?' ) THEN

               TRAN = .FALSE.
               RETURN

            END IF

         END IF

!
!     Is this an INQUIRE command?
!
      ELSE IF ( WORD(1) .EQ. 'INQUIRE' ) THEN

!
!        The second word must not be blank, and must not end with '?'.
!
         IF ( WORD(2) .NE. ' ' ) THEN

            LOC = RTRIM( WORD(2) )

            IF ( WORD(2)(LOC:LOC) .EQ. '?' ) THEN

               TRAN = .FALSE.

               CALL CHKIN ( 'PRTRAP' )
               CALL SETMSG ( 'INQUIRE commands must be of the ' &
               //            'form INQUIRE <symbol_name>,  You ' &
               //            'have INQUIRE # which is inquiring ' &
               //            'for the value of a query. This ' &
               //            'kind of INQUIRE is not supported. ' )
               CALL ERRCH  ( '#', WORD(2) )
               CALL SIGERR ('INVALID_INQUIRE')
               CALL CHKOUT ( 'PRTRAP' )
               RETURN

            END IF

         END IF

      END IF

!
!     No reason to trap this.
!
      TRAN = .TRUE.

      RETURN
      END
