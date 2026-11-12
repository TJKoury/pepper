!$Procedure RDSTMN ( Read a statement entered on one or more lines)

      SUBROUTINE RDSTMN (PRMPT, DELIM, STMT)
      use pepper_legacy
      IMPLICIT NONE

! $ Abstract
!
!     None.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     None.
!
! $ Declarations


      CHARACTER*(*)      PRMPT
      CHARACTER*(1)      DELIM
      CHARACTER*(*)      STMT

! $ Brief_I/O
!
!     Variable  I/O  Description
!     --------  ---  --------------------------------------------------
!     PRMPT      I   Prompt for input. If PRMPT is not blank,
!                    the cursor is positioned one space after the
!                    last non-blank character. Successive lines
!                    are indented by the length of PRMPT.
!     DELIM      I   Statement delimiter. RDSTMN will continue
!                    to read until the either the delimiter or
!                    a blank line is entered.
!     STMT       O   The statement entered, up to but not
!                    including the delimiter. If RDSTMN is
!                    terminated by the entry of a blank line,
!                    STMT is blank.
!
! $ Detailed_Input
!
!     None.
!
! $ Detailed_Output
!
!     None.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     None.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     None.
!
! $ Examples
!
!     None.
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
!     None.
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 14-DEC-2010 (EDW)
!
!         Declared PROMPT as EXTERNAL.
!
!-    Command Loop Configured Version 1.0.0, 3-MAY-1994 (WLT)
!
!         This is the configured version of the Command Loop
!         software as of May 4, 1994
!
!-     7 February 1986, I.M. Underwood
!
!-&

! $ Index_Entries
!
!   support
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               BATCH

!
!     Local variables
!
      CHARACTER*(1)         SPACE
      CHARACTER*(1)         TAB
      CHARACTER*(132)       BLANK
      SAVE                  BLANK

      INTEGER               PRLEN
      CHARACTER*(132)       LINE
      CHARACTER*(132)       MYPRM
      INTEGER               END

      DATA                  BLANK / ' ' /


!
!     Read the first statement. Use the prompt. Return immediately
!     if a blank line or an error is encountered.
!
      IF ( BATCH() ) THEN
         STMT = ' '
         RETURN
      END IF

      PRLEN  = RTRIM (PRMPT) + 1
      MYPRM  = PRMPT
      LINE   = ' '
      SPACE  = ' '
      TAB    = CHAR(9)

      CALL PROMPT ( MYPRM(1:PRLEN), LINE )

      IF ( LINE .EQ. ' ' ) THEN
         STMT = ' '
         RETURN
      ELSE
         STMT = LINE
      END IF

!
!     Get rid of any of those nasty old tabs.
!
      CALL REPLCH ( LINE, TAB, SPACE, LINE )


!
!     Read succeeding lines. Indent to the length of the original
!     prompt. Add the input line to the current statement. Quit when:
!
!            - A delimiter is encountered. (Return the statement
!              up to the delimiter.)
!
!            - A blank line or an error is encountered. (Return
!              a blank statement.)
!
      DO WHILE ( INDEX ( STMT, DELIM ) .EQ. 0 )

         CALL PROMPT ( BLANK(1:PRLEN),   LINE )
         CALL REPLCH ( LINE, TAB, SPACE, LINE )

         IF ( LINE .EQ. ' ' ) THEN
            STMT = ' '
            RETURN
         ELSE
            CALL SUFFIX ( LINE, 1, STMT )
         END IF

      END DO

!
!     If we made it to here, we encountered a delimiter. Take the
!     entire statement up to the character before the delimiter.
!
      END        = INDEX ( STMT, DELIM )
      STMT(END:) = ' '

      RETURN
      END
