!$Procedure REPSUB ( Replace one substring with another )

      SUBROUTINE REPSUB ( IN, LEFT, RIGHT, STRING, OUT )

! $ Abstract
!
!     Replace the substring (LEFT:RIGHT) with a string of any length.
!
! $ Required_Reading
!
!     None.
!
! $ Keywords
!
!     ASSIGNMENT
!     CHARACTER
!     STRING
!
! $ Declarations

      IMPLICIT NONE

      CHARACTER*(*)         IN
      INTEGER               LEFT
      INTEGER               RIGHT
      CHARACTER*(*)         STRING
      CHARACTER*(*)         OUT

! $ Brief_I/O
!
!     VARIABLE  I/O  DESCRIPTION
!     --------  ---  --------------------------------------------------
!     IN         I   Input string.
!     LEFT,
!     RIGHT      I   Ends of substring to be replaced.
!     STRING     I   Replacement string.
!     OUT        O   Resulting string.
!
! $ Detailed_Input
!
!     IN       is an arbitrary character string.
!
!     LEFT,
!     RIGHT    are the ends of the substring to be replaced.
!              Legitimate substrings satisfy the following
!              conditions
!
!                  RIGHT > LEFT - 2
!                  LEFT  > 1
!                  RIGHT < LEN(STRING) + 1
!
!              This allows users to refer to zero-length substrings
!              (null substrings) of IN.
!
!     STRING   is the replacement string. Essentially, the
!              substring (LEFT:RIGHT) is removed from the
!              input string, and STRING is inserted at the
!              point of removal.
!
! $ Detailed_Output
!
!     OUT      is the resulting string. OUT may overwrite IN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1)  If RIGHT is one less than LEFT, the substring to
!         replace will be the null substring. In this case,
!         STRING will be inserted between IN(:RIGHT) and IN(LEFT:).
!
!     2)  If LEFT is smaller than one, the error SPICE(BEFOREBEGSTR)
!         is signaled.
!
!     3)  If RIGHT is greater than the length of the input string,
!         the error SPICE(PASTENDSTR) is signaled.
!
!     4)  If RIGHT is less than LEFT-1, the error SPICE(BADSUBSTR)
!         is signaled.
!
!     5)  Whenever the output string is too small to hold the result,
!         the result is truncated on the right.
!
! $ Files
!
!     None.
!
! $ Particulars
!
!     Ideally, replacement could be done with simple concatenation,
!
!        OUT = IN(1:LEFT-1) // STRING // IN(RIGHT+1: )
!
!     but the Fortran 77 standard makes this illegal for strings of
!     unknown length.
!
! $ Examples
!
!     A typical use for this routine might be to replace all
!     occurrences of one word in a string with another word.
!     For example, the following code fragment replaces every
!     occurrence of the word 'AND' with the word 'OR' in the
!     character string LINE.
!
!        LEFT = WDINDX ( LINE, 'AND' )
!
!        DO WHILE ( LEFT .NE. 0 )
!           CALL   REPSUB ( LINE, LEFT, LEFT+2, 'OR', LINE )
!           LEFT = WDINDX ( LINE, 'AND' )
!        END DO
!
!     This routine can also be used to insert substring between
!     two characters. Consider the string:
!
!         IN   = 'The defendant,, was found innocent.'
!
!     to insert ' Imelda Marcos' between the first and second commas
!     determine the location of the pair ',,'
!
!        RIGHT = POS ( IN, ',,', 1 )
!        LEFT  = RIGHT + 1
!
!     then
!
!        CALL REPSUB ( IN, LEFT, RIGHT, ' Imelda Marcos', OUT )
!
!     The output (OUT) will have the value:
!
!        'The defendant, Imelda Marcos, was found innocent.'
!
! $ Restrictions
!
!     1)  The memory used by STRING and OUT must be disjoint. The memory
!         used by IN and OUT must be identical or disjoint.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     J. Diaz del Rio    (ODC Space)
!     W.L. Taber         (JPL)
!     I.M. Underwood     (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.1.0, 20-AUG-2021 (JDR)
!
!        Added IMPLICIT NONE statement.
!
!        Edited the header to comply with NAIF standard.
!
!-    SPICELIB Version 1.0.2, 17-JUN-1999 (WLT)
!
!        Fixed example code fragment.
!
!-    SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
!
!        Comment section for permuted index source lines was added
!        following the header.
!
!-    SPICELIB Version 1.0.0, 24-AUG-1990 (WLT) (IMU)
!
!-&


! $ Index_Entries
!
!     replace one substring with another substring
!
!-&


!
!     SPICELIB functions
!
      LOGICAL               RETURN
      INTEGER               SUMAI

!
!     Local variables
!
      INTEGER               INLEN
      INTEGER               STRLEN
      INTEGER               OUTLEN

      INTEGER               REMAIN
      INTEGER               USE         ( 3 )

      INTEGER               END
      INTEGER               NEXT
      INTEGER               I


!
!     Standard SPICE error handling.
!
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'REPSUB' )
      END IF

!
!     Get the lengths of all the strings involved in this transaction.
!
      INLEN  = LEN ( IN     )
      STRLEN = LEN ( STRING )
      OUTLEN = LEN ( OUT    )

!
!     Reject bad inputs.
!
      IF ( LEFT .LT. 1 ) THEN
         CALL SETMSG ( 'REPSUB error: LEFT (#) must not be less '     // &
                       'than 1.' )

         CALL ERRINT ( '#', LEFT )
         CALL SIGERR ( 'SPICE(BEFOREBEGSTR)' )
         CALL CHKOUT ( 'REPSUB' )
         RETURN

      ELSE IF ( RIGHT .GT. INLEN ) THEN
         CALL SETMSG ( 'REPSUB error: RIGHT (#) must not exceed '     // &
                       'length of IN (#).' )
         CALL ERRINT ( '#', RIGHT )
         CALL ERRINT ( '#', INLEN )
         CALL SIGERR ( 'SPICE(PASTENDSTR)' )
         CALL CHKOUT ( 'REPSUB' )
         RETURN

      ELSE IF ( RIGHT .LT. LEFT-1 ) THEN
         CALL SETMSG ( 'REPSUB error: LEFT (#) must not exceed '      // &
                       'RIGHT+1 (# + 1). ' )
         CALL ERRINT ( '#', LEFT  )
         CALL ERRINT ( '#', RIGHT )
         CALL SIGERR ( 'SPICE(BADSUBSTR)' )
         CALL CHKOUT ( 'REPSUB' )
         RETURN

      END IF

!
!     Consider three separate sections:
!
!        1) The front of the original string.
!
!        2) The replacement string.
!
!        3) The end of the original string.
!
!     Determine how much of each section to use in the output string.
!     REMAIN is the number of characters that will fit in the output
!     string.
!
      REMAIN = OUTLEN
      USE(1) = MIN ( REMAIN, LEFT - 1 )

      REMAIN = REMAIN - USE(1)
      USE(2) = MIN ( REMAIN, STRLEN )

      REMAIN = REMAIN - USE(2)
      USE(3) = MIN ( REMAIN, INLEN - RIGHT )

!
!     Move the third section first. It gets moved back to front
!     or front to back, depending on whether the replacement string
!     is longer than the original substring. The main thing is to
!     avoid overwriting characters that have yet to be moved.
!
      END = SUMAI ( USE, 3 )

      IF ( LEFT + STRLEN .GT. RIGHT ) THEN

         NEXT = END

         DO I = USE(3), 1, -1
            OUT(NEXT:NEXT) = IN(RIGHT+I:RIGHT+I)
            NEXT           = NEXT - 1
         END DO

      ELSE

         NEXT = LEFT + STRLEN

         DO I = 1, USE(3)
            OUT(NEXT:NEXT) = IN(RIGHT+I:RIGHT+I)
            NEXT           = NEXT + 1
         END DO

      END IF

!
!     The first two sections can be moved directly to the front of
!     the output string.
!
      NEXT = 1

      DO I = 1, USE(1)
         OUT(NEXT:NEXT) = IN(I:I)
         NEXT           = NEXT + 1
      END DO

      DO I = 1, USE(2)
         OUT(NEXT:NEXT) = STRING(I:I)
         NEXT           = NEXT + 1
      END DO

!
!     Pad with blanks, if the output string was not filled.
!
      IF ( END .LT. OUTLEN ) THEN
         OUT(END+1: ) = ' '
      END IF


      CALL CHKOUT ( 'REPSUB' )
      RETURN
      END
