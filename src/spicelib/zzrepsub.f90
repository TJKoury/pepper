!$Procedure      ZZREPSUB ( Replace one substring with another )

      SUBROUTINE ZZREPSUB ( IN, LEFT, RIGHT, STRING, OUT )

! $ Abstract
!
!     SPICE Private routine intended solely for the support of SPICE
!     routines.  Users should not call this routine directly due
!     to the volatile nature of this routine.
!
!     Replace the substring (LEFT:RIGHT) with a string of any length.
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
!     IN         is an arbitrary character string.
!
!     LEFT,
!     RIGHT      are the ends of the substring to be replaced.
!                Legitimate substrings satisfy the following
!                conditions
!
!                    RIGHT > LEFT - 2
!                    LEFT  > 1
!                    RIGHT < LEN(STRING) + 1
!
!                This allows users to refer to zero-length substrings
!                (null substrings) of IN.
!
!     STRING     is the replacement string. Essentially, the
!                substring (LEFT:RIGHT) is removed from the
!                input string, and STRING is inserted at the
!                point of removal.
!
! $ Detailed_Output
!
!     OUT        is the resulting string. OUT may overwrite IN.
!
! $ Parameters
!
!     None.
!
! $ Exceptions
!
!     1) If RIGHT is one less than LEFT, the substring to
!        replace will be the null substring.  In this case,
!        STRING will be inserted between IN(:RIGHT) and IN(LEFT:).
!
!     2) If LEFT is smaller than one, it's treated as 1.
!
!     3) If RIGHT is greater than the length of the input string,
!        it is treated as being the length of the string.
!
!     4) If RIGHT is less than LEFT-1, no substitution is made.
!
!     5) Whenever the output string is too small to hold the result,
!        the result is truncated on the right.
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
!     This private routine is basically just a copy of the SPICE
!     routine REPSUB with all error handling removed and "reasonable"
!     interpretations used for exceptional cases.
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
!           CALL REPSUB ( LINE, LEFT, LEFT+2, 'OR', LINE )
!           LEFT = WDINDX ( LINE, 'AND' )
!        END DO
!
!     This routine can also be used to insert substring between
!     two characters.  Consider the string:
!
!         IN   = 'The defendant,, was found innocent.'
!
!     to insert ' Emelda Marcos' between the first and second commas
!     determine the location of the pair ',,'
!
!        RIGHT = POS ( IN, ',,', 1 )
!        LEFT  = RIGHT + 1
!
!     then
!
!        CALL REPSUB ( IN, LEFT, RIGHT, ' Emelda Marcos', OUT )
!
!     The output (OUT) will have the value:
!
!        'The defendant, Emelda Marcos, was found innocent.'
!
! $ Restrictions
!
!     The memory used by STRING and OUT must be disjoint. The memory
!     used by IN and OUT must be identical or disjoint.
!
! $ Literature_References
!
!     None.
!
! $ Author_and_Institution
!
!     W.L. Taber     (JPL)
!     I.M. Underwood (JPL)
!
! $ Version
!
!-    SPICELIB Version 1.0.0, 27-APR-1996 (WLT)
!
!
!-&





!
!     SPICELIB functions
!
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
      INTEGER               MYLEFT
      INTEGER               MYRGHT

!
!     Get the lengths of all the strings involved in this transaction.
!
      INLEN  = LEN ( IN     )
      STRLEN = LEN ( STRING )
      OUTLEN = LEN ( OUT    )
      MYLEFT = MIN( INLEN+1, MAX(1, LEFT ) )
      MYRGHT = MIN( INLEN,   MAX(0, RIGHT) )
!
!     Reject bad inputs.
!
      IF ( MYLEFT .LT. 1 ) THEN

         MYLEFT = 1

      ELSE IF ( MYRGHT .GT. INLEN ) THEN

         MYRGHT = INLEN

      ELSE IF ( RIGHT .LT. LEFT-1 ) THEN

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
      USE(1) = MIN ( REMAIN, MYLEFT - 1 )

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

      IF ( MYLEFT + STRLEN .GT. RIGHT ) THEN

         NEXT = END

         DO I = USE(3), 1, -1
            OUT(NEXT:NEXT) = IN(RIGHT+I:RIGHT+I)
            NEXT           = NEXT - 1
         END DO

      ELSE

         NEXT = MYLEFT + STRLEN

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


      RETURN
      END
